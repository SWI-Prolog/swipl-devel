/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>
#include <h/text.h>

static Any	getSelectionTextItem(TextItem);
static Type	getSelectionTypeTextItem(TextItem);
static Bool	getModifiedTextItem(TextItem);
static Any	getDefaultTextItem(TextItem);
static void	compute_label_text_item(TextItem, int *, int *);
static status	applyTextItem(TextItem, Bool);
static status	WantsKeyboardFocusTextItem(TextItem);
static status	restoreTextItem(TextItem ti);
static status	selectionTextItem(TextItem ti, Any selection);
static status	resetTextItem(TextItem ti);
static status	displayedValueTextItem(TextItem ti, CharArray txt);


static status
initialiseTextItem(TextItem ti, Name name, Any val, Code msg)
{ CharArray str;

  if ( isDefault(name) )
    name = NAME_textItem;
  if ( isDefault(val) )
    val = CtoName("");

  createDialogItem(ti, name);

  assign(ti, message, 	    msg);
  assign(ti, value_set,	    DEFAULT);
  assign(ti, print_name,    CtoString(""));

  assign(ti, label_font,    DEFAULT); /* resources */
  assign(ti, value_font,    DEFAULT);
  assign(ti, length,	    DEFAULT);
  assign(ti, pen,	    DEFAULT);
  assign(ti, value_width,   DEFAULT);
  assign(ti, advance,       NAME_next);
  assign(ti, show_label,    ON);
  assign(ti, value_text,    newObject(ClassText, 0));
  assign(ti, label_width,   DEFAULT);
  assign(ti, editable,	    ON);

  assign(ti, default_value, val);
  assign(ti, selection,     getDefaultTextItem(ti));
  assign(ti, type,	    getSelectionTypeTextItem(ti));
  assign(ti, auto_value_align,	 OFF);

  if ( (str = get(ti, NAME_printNameOfValue, val, 0)) )
    valueString(ti->print_name, str);
  resetTextItem(ti);

  return requestComputeGraphical(ti, DEFAULT);
}


#define InBox(ti, z) ((ti->look == NAME_motif || ti->look == NAME_win) && \
		      z && notNil(z))

static status
RedrawAreaTextItem(TextItem ti, Area a)
{ int x, y, w, h;
  int al, av, am;
  int lw, lh;
  String lb = &ti->label->data;
  int fw = valInt(getExFont(ti->value_text->font));
  Elevation z = getResourceValueObject(ti, NAME_elevation);
  int tx, ty, tw, th;
  TextObj vt = ti->value_text;

  initialiseDeviceGraphical(ti, &x, &y, &w, &h);

  al = valInt(getAscentFont(ti->label_font));
  av = valInt(getAscentFont(vt->font)) + valInt(vt->border);
  am = max(al, av);

  compute_label_text_item(ti, &lw, &lh);
  if ( ti->show_label == ON )
    str_string(lb, ti->label_font,
	       x, y+am-al, lw-fw, h,
	       ti->label_format, NAME_top);
      
  tx = x+lw;
  ty = y+am-av;
  tw = valInt(vt->area->w);
  th = valInt(vt->area->h);

  if ( InBox(ti, z) )
    r_3d_box(tx, ty, tw, th, 0, z, FALSE);
  else if ( ti->look != NAME_motif && ti->look != NAME_win )
  { if ( z && notNil(z) )
    { int zh = abs(valInt(z->height));
      int ly = y+am+zh+valInt(getDescentFont(vt->font));

      r_3d_line(x+lw, ly, x+w-1, ly, z, TRUE);
    } else if ( ti->pen != ZERO )
    { int pen = valInt(ti->pen);
      int ly = y+am+1+pen/2;
    
      r_dash(ti->texture);
      r_thickness(valInt(ti->pen));

      r_line(x+lw, ly, x+w-pen, ly);
    }
  }

  repaintText(vt, tx, ty, tw, th);

  return RedrawAreaGraphical(ti, a);
}


static status
updateShowCaretTextItem(TextItem ti)
{ if ( ti->status != NAME_inactive )
  { PceWindow sw = getWindowGraphical((Graphical)ti);
    int active = (sw && sw->input_focus == ON);

    caretText(ti->value_text, DEFAULT);
    showCaretText(ti->value_text, active ? (Any)ON : (Any)NAME_passive);  
  } else
    showCaretText(ti->value_text, OFF);

  return requestComputeGraphical(ti, DEFAULT);

  succeed;
}


static status
statusTextItem(TextItem ti, Name stat)
{ if ( ti->status != stat )
  { assign(ti, status, stat);

    return updateShowCaretTextItem(ti);
  }

  succeed;
}


static void
compute_label_text_item(TextItem ti, int *lw, int *lh)
{ if ( ti->show_label == ON )
  { str_size(&ti->label->data, ti->label_font, lw, lh);
    *lw += valInt(getExFont(ti->label_font));
    if ( notDefault(ti->label_width) )
      *lw = max(valInt(ti->label_width), *lw);
  } else
  { *lw = *lh = 0;
  }
}


static status
computeTextItem(TextItem ti)
{ if ( notNil(ti->request_compute) )
  { int lw, lh, w, h;
    Int b = getResourceValueObject(ti, NAME_border);

    obtainResourcesObject(ti);
    fontText(ti->value_text, ti->value_font);
    borderText(ti->value_text, b);
    if ( notDefault(ti->value_width) )
    { Int vw = toInt(valInt(ti->value_width) - 2 * valInt(b));
      marginText(ti->value_text, vw, NAME_clip);
    } else
      lengthText(ti->value_text, ti->length);
    ComputeGraphical(ti->value_text);

    compute_label_text_item(ti, &lw, &lh);
    h = max(lh, valInt(ti->value_text->area->h));
    w = lw + valInt(ti->value_text->area->w);

    if ( ti->pen != ZERO )
    { int al, av, am;

      al = valInt(getAscentFont(ti->label_font));
      av = valInt(getAscentFont(ti->value_text->font));
      am = max(al, av);

      h = max(h, am+1+valInt(ti->pen));
    }

    CHANGING_GRAPHICAL(ti,
	  assign(ti->area, w, toInt(w));
	  assign(ti->area, h, toInt(h));
	  changedDialogItem(ti));

    assign(ti, request_compute, NIL);
  }

  succeed;
}


static Point
getReferenceTextItem(TextItem ti)
{ Point ref;

  if ( !(ref = getReferenceDialogItem(ti)) )
    ref = answerObject(ClassPoint, ZERO, getAscentFont(ti->label_font));
  
  answer(ref);
}


static status
pasteTextItem(TextItem ti, Int buffer)
{ TRY( pasteText(ti->value_text, buffer) );
  requestComputeGraphical(ti, DEFAULT);

  succeed;
}

		/********************************
		*          COMPLETION		*
		********************************/

static Browser Completer = NULL;

static Any
completer(void)
{ if ( !Completer )
  { KeyBinding kb;
    Any client;
    Any quit;

    Completer = globalObject(NAME_completer, ClassBrowser, 0);
    client    = newObject(ClassObtain, Completer, NAME_client, 0);
    quit      = newObject(ClassMessage, client, NAME_keyboardQuit, 0);

    protectObject(Completer);
    protectObject(Completer->frame);
    attributeObject(Completer, NAME_client, NIL);
    attributeObject(Completer, NAME_prefix, CtoName(""));
    send(Completer, NAME_selectMessage,
	 newObject(ClassMessage, client, NAME_selectedCompletion, Arg(1), 0),
	 0);
    send(get(Completer, NAME_tile, 0), NAME_border, ZERO, 0);
    send(Completer, NAME_kind, NAME_popup, 0);
    send(Completer, NAME_create, 0);
    send(Completer->frame, NAME_border, ZERO, 0);

    kb = get(Completer, NAME_keyBinding, 0);
    functionKeyBinding(kb, CtoName("\\C-g"), quit);
    functionKeyBinding(kb, CtoName("\\e"),   quit);
    functionKeyBinding(kb, CtoName("SPC"),   NAME_extendPrefix);
  }

  return Completer;
}
				       

static status
quitCompleterTextItem(TextItem ti)
{ if ( Completer && notNil(getAttributeObject(Completer, NAME_client)) )
  { send(Completer, NAME_clear, 0);
    send(Completer, NAME_client, NIL, 0);
    send(Completer, NAME_show, OFF, 0);
    send(Completer, NAME_transientFor, NIL, 0);
  }

  succeed;
}


static status
enterCompleterTextItem(TextItem ti)
{ if ( Completer && 
       getAttributeObject(Completer, NAME_client) == ti )
  { send(Completer, NAME_enter, 0);

    quitCompleterTextItem(ti);
  }

  succeed;
}


static status
selectedCompletionTextItem(TextItem ti, DictItem di)
{ Any c = completer();
  Any prefix = get(c, NAME_prefix, 0);

  displayedValueTextItem(ti, getAppendCharArray(prefix, (CharArray) di->key));
  quitCompleterTextItem(ti);

  succeed;
}


static status
selectCompletionTextItem(TextItem ti, Chain matches, CharArray prefix)
{ Any c = completer();
  Any val;
  Point pos;

  send(c, NAME_client, ti, 0);
  send(c, NAME_prefix, isDefault(prefix) ? (CharArray)CtoName("") : prefix, 0);
  send(c, NAME_clear, 0);
  for_chain(matches, val,
	    send(c, NAME_append, get(val, NAME_printName, 0), 0));
  send((pos = get(ti, NAME_displayPosition, 0)), NAME_offset,
       toInt(40), toInt(25), 0);
  send(c, NAME_transientFor, getFrameGraphical((Graphical) ti), 0);
  send(c, NAME_open, pos, ON, 0);	/* pos, normalise */
  send(c, NAME_cancelSearch, 0);
  send(c, NAME_extendPrefix, 0);

  succeed;
}


static status
completeTextItem(TextItem ti, EventId id)
{ Any c;

  if ( getAttributeObject(c = completer(), NAME_client) == ti )
  { send(c, NAME_extendPrefix, 0);
  } else
  { Any split;
    CharArray dir, file;
    Chain files;

    if ( (split = get(ti, NAME_splitCompletion, ti->value_text->string, 0)) &&
	 (files = get(ti, NAME_completions, split, 0)) &&
	 (files = checkType(files, TypeChain, NIL)) )
    { Tuple t;
      int dirmode;
      Bool ignore_case = getResourceValueObject(ti, NAME_searchIgnoreCase);
      
      if ( (dirmode = instanceOfObject(split, ClassTuple)) )
      { dir = ((Tuple)split)->first;
	file = ((Tuple)split)->second;
      } else
      { dir = (CharArray)CtoName("");
	file = split;
      }

      if ( (t=getCompleteNameChain(files, file, DEFAULT, ignore_case)) )
      { int unique = (getSizeChain(t->first) == ONE);
	StringObj path;

	path = answerObject(ClassString, CtoName("%s%s"), dir, t->second, 0);
	if ( unique && dirmode )
	  send(ti, NAME_indicateDirectory, path, 0);
	if ( equalCharArray((CharArray) path,
			    (CharArray) ti->value_text->string) )
	{ if ( unique )
	    errorPce(path, NAME_soleCompletion);
	  else
	    send(ti, NAME_selectCompletion, t->first, dirmode ? dir : 0, 0);
	} else
	  displayedValueTextItem(ti, (CharArray) path);
	doneObject(path);
	doneObject(t);
      } else
	errorPce(file, NAME_completeNoMatch);
    } else if ( isInteger(id) )
      send(ti, NAME_insertSelf, ONE, id, 0);
  }

  succeed;
}


static CharArray
getSplitCompletionTextItem(TextItem ti, CharArray base)
{ answer(base);
}


static Chain
getCompletionsTextItem(TextItem ti, Any base)
{ Any rval;

  if ( isNil(ti->value_set) )
    fail;
  if ( isDefault(ti->value_set) )
    return getValueSetType(ti->type, NIL);
  if ( instanceOfObject(ti->value_set, ClassChain) )
    answer(ti->value_set);
  if ( isFunction(ti->value_set) &&
       (rval = getForwardReceiverFunction(ti->value_set, ti,
					  ti->value_text->string, 0)) )
    answer(rval);

  fail;
}


static status
indicateDirectoryTextItem(TextItem ti, StringObj dir)
{ succeed;
}


		/********************************
		*        EVENT HANDLING		*
		********************************/


static status
eventTextItem(TextItem ti, EventObj ev)
{ if ( eventDialogItem(ti, ev) )
    succeed;

  if ( ti->active == OFF )
    fail;

  if ( isAEvent(ev, NAME_msLeftDown) )
  { if ( WantsKeyboardFocusTextItem(ti) )
      return send(ti, NAME_keyboardFocus, 0);
    else
      return alertGraphical((Graphical) ti);
  } else if ( isAEvent(ev, NAME_focus) )
  { if ( isAEvent(ev, NAME_obtainKeyboardFocus) )
    { send(ti, NAME_status, NAME_active, 0);
    } else if ( isAEvent(ev, NAME_releaseKeyboardFocus) )
    { send(ti, NAME_status, NAME_inactive, 0);
    }

    return updateShowCaretTextItem(ti);
  }

  if ( ti->status != NAME_inactive )
  { if ( isAEvent(ev, NAME_keyboard) )
    { Any f;
      EventId id = getIdEvent(ev);
      KeyBinding kb = (ti->editable == ON ? KeyBindingTextItem()
			 		  : KeyBindingTextItemView());

      if ( get(completer(), NAME_client, 0) == ti &&
	   (f = getFunctionKeyBinding(kb, id)) != NAME_complete &&
	   f != NAME_keyboardQuit )
	return postEvent(ev, completer(), DEFAULT);
      else
	return send(ti, NAME_typed, id, 0);
    } else
      if ( isAEvent(ev, NAME_msMiddleUp) )
	return pasteTextItem(ti, DEFAULT);
  }

  fail;
}


static status
keyboardQuitTextItem(TextItem ti)
{ Any c = completer();

  send(c, NAME_show, OFF, 0);
  send(c, NAME_transientFor, NIL, 0);
  send(c, NAME_client, NIL, 0);
  send(ti, NAME_alert, 0);

  succeed;
}


static status
executeTextItem(TextItem ti)
{ return applyTextItem(ti, ON);
}


static status
typedTextItem(TextItem ti, EventId id)
{ return typedKeyBinding(ti->editable == ON ? KeyBindingTextItem()
			 		    : KeyBindingTextItemView(),
			 id, (Graphical) ti);
}


static status
nextTextItem(TextItem ti)
{ return send(ti->device, NAME_advance, ti, 0);
}


static status
enterTextItem(TextItem ti, EventId id)
{ Device dev = ti->device;

  if ( isDefault(id) )
  { Any ev = EVENT->value;

    id = (instanceOfObject(ev, ClassEvent) ? getIdEvent(ev) : toInt('\r'));
  }

  if ( !(notNil(dev) && send(dev, NAME_typed, id, ON, 0)) && !isFreedObj(ti) )
  { int modified = (getModifiedTextItem(ti) == ON);

    if ( applyTextItem(ti, OFF) && !isFreedObj(ti) )
    { if ( ti->advance == NAME_clear && modified )
	selectionTextItem(ti, CtoName(""));
      else
	nextTextItem(ti);
    }
  }

  succeed;
}


static Bool
getModifiedTextItem(TextItem ti)
{ answer(equalCharArray((CharArray) ti->print_name,
			(CharArray) ti->value_text->string) ? OFF : ON);
}


static status
modifiedTextItem(TextItem ti, Bool val)
{ if ( val == OFF )
    getSelectionTextItem(ti);
  else
    deleteString(ti->print_name, ZERO, DEFAULT); /* is ->clear */

  succeed;
}


		/********************************
		*         SELECTION, ETC.	*
		********************************/

static status
defaultTextItem(TextItem ti, Any def)
{ if ( ti->default_value != def )
  { assign(ti, default_value, def);

    return restoreTextItem(ti);
  }

  succeed;
}


static Any
getDefaultTextItem(TextItem ti)
{ answer(checkType(ti->default_value, TypeAny, ti));
}


static status
restoreTextItem(TextItem ti)
{ Any val;

  TRY(val = getDefaultTextItem(ti));
  return selectionTextItem(ti, val);
}


static status
applyTextItem(TextItem ti, Bool always)
{ Any val;

  if ( instanceOfObject(ti->message, ClassCode) &&
       (always == ON || getModifiedTextItem(ti) == ON) &&
       (val = getv(ti, NAME_selection, 0, NULL)) )
    return forwardReceiverCode(ti->message, ti, val, 0);

  fail;
}


static Type
getSelectionTypeTextItem(TextItem ti)
{ Any val = ti->selection;

  if ( isObject(val) )
    return nameToType(getClassNameObject(val));
  if ( isInteger(val) )
    return TypeInt;
  if ( isBoolean(val) )
    return TypeBool;

  return TypeAny;
}


static CharArray
getPrintNameOfValueTextItem(TextItem ti, Any val)
{ CharArray rval;

  if ( isObject(val) && (rval = getv(val, NAME_printName, 0, NULL)) )
    answer(rval);
  if ( (rval = checkType(val, TypeCharArray, NIL)) )
    answer(rval);

  answer(CtoCharArray(pp(val)));
}


static Any
getSelectionTextItem(TextItem ti)
{ enterCompleterTextItem(ti);

  if ( getModifiedTextItem(ti) == ON ||
       !send(ti->type, NAME_validate, ti->selection, 0) )
  { Any value;
    Chain set;
    int ok = 0;

    if ( (value = get(ti->type, NAME_check, ti->value_text->string, 0)) )
    { valueString(ti->print_name, ti->value_text->string);
      assign(ti, selection, value);
      ok++;
    } else if ( (set = getCompletionsTextItem(ti, ti->value_text->string)) )
    { Cell cell;

      for_cell(cell, set)
      { CharArray pn;

	if ( (pn = getv(ti, NAME_printNameOfValue, 1, &cell->value)) &&
	     equalCharArray(ti->value_text->string, pn) )
	{ valueString(ti->print_name, ti->value_text->string);
	  assign(ti, selection, cell->value);
	  ok++;
	  break;
	}
      }
    }

    if ( !ok &&				/* better '' ---> @default?? */
	 includesType(ti->type, TypeDefault) &&
	 ti->value_text->string->data.size == 0 )

    { assign(ti, selection, DEFAULT);
      ok++;
    }

    if ( !ok )
    { errorPce(ti, NAME_cannotConvertText, ti->value_text->string, ti->type);
      fail;
    }
  }

  answer(ti->selection);
}


static status
clearTextItem(TextItem ti)
{ int modified = (getSizeCharArray(ti->value_text->string) != ZERO);
  
  deleteString(ti->print_name, ZERO, DEFAULT); /* clear */
  assign(ti, selection, NIL);
  if ( modified && hasSendMethodObject(ti->device, NAME_modifiedItem) )
    send(ti->device, NAME_modifiedItem, ti, ON, 0);
  
  return resetTextItem(ti);
}


static status
selectionTextItem(TextItem ti, Any selection)
{ if ( ti->selection != selection )
  { Any sel;

    TRY(sel = getv(ti->type, NAME_check, 1, (Any *) &selection));

    if ( ti->selection != sel )
    { CharArray pn;

      TRY( pn = getv(ti, NAME_printNameOfValue, 1, (Any *) &sel) );
      assign(ti, selection, sel);
      valueString(ti->print_name, pn);
      doneObject(pn);
    }
  }

  return resetTextItem(ti);
}


static status
resetTextItem(TextItem ti)
{ quitCompleterTextItem(ti);
    
  if ( !equalCharArray((CharArray)ti->value_text->string,
		       (CharArray)ti->print_name) )
  { stringText(ti->value_text, (CharArray) ti->print_name);
    requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


static status
displayedValueTextItem(TextItem ti, CharArray txt)
{ if ( !equalCharArray(ti->value_text->string, txt) )
  { TRY(stringText(ti->value_text, txt));
    requestComputeGraphical(ti, DEFAULT);
    if ( hasSendMethodObject(ti->device, NAME_modifiedItem) )
      send(ti->device, NAME_modifiedItem, ti, ON, 0);
  }

  succeed;
}


static CharArray
getDisplayedValueTextItem(TextItem ti)
{ answer(ti->value_text->string);
}


		/********************************
		*          ATTRIBUTES		*
		********************************/

static status
editableTextItem(TextItem ti, Bool val)
{ if ( ti->editable != val )
  { assign(ti, editable, val);
    if ( val == OFF && notNil(ti->device) )
      advanceDevice((Device) ti->device, (Graphical) ti);
  }

  succeed;
}


static status
lengthTextItem(TextItem ti, Int w)
{ if ( ti->length != w )
  { assign(ti, length, w);
    assign(ti, value_width, DEFAULT);

    requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


static status
labelFontTextItem(TextItem ti, FontObj font)
{ if ( ti->label_font != font )
  { assign(ti, label_font, font);
    requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


static status
showLabelTextItem(TextItem ti, Bool val)
{ if ( ti->show_label != val )
  { assign(ti, show_label, val);
    requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


static status
valueFontTextItem(TextItem ti, FontObj font)
{ if ( ti->value_text->font != font )
  { assign(ti, value_font, font);
    requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


static Int
getLabelWidthTextItem(TextItem ti)
{ int lw, lh;

  obtainResourcesObject(ti);
  compute_label_text_item(ti, &lw, &lh);
  answer(toInt(lw));
}


static status
labelWidthTextItem(TextItem ti, Int w)
{ if ( ti->show_label == ON && ti->label_width != w )
  { assign(ti, label_width, w);
    requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


static status
valueWidthTextItem(TextItem ti, Int val)
{ assign(ti, value_width, val);
  if ( notDefault(val) && instanceOfObject(ti->value_font, ClassFont) )
  { Int ex = getExFont(ti->value_font);
    int chars = valInt(val) / valInt(ex);

    if ( chars < 2 )
      chars = 2;
    
    assign(ti, length, toInt(chars));
  }
  requestComputeGraphical(ti, DEFAULT);

  succeed;
}


static status
geometryTextItem(TextItem ti, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) )
  { int lw, lh;

    compute_label_text_item(ti, &lw, &lh);
    valueWidthTextItem(ti, toInt(valInt(w) - lw));
  }

  return geometryGraphical(ti, x, y, w, DEFAULT);
}

		/********************************
		*       DELEGATE TO TEXT	*
		********************************/

static status
delegateTextItem(TextItem ti, Any impl, Any part, int argc, Any *argv)
{ Bool old = getModifiedTextItem(ti);

  if ( sendImplementation(impl, part, argc, argv) )
  { Bool new;

    requestComputeGraphical(ti, DEFAULT);
    if ( (new = getModifiedTextItem(ti)) != old &&
	 hasSendMethodObject(ti->device, NAME_modifiedItem) )
      send(ti->device, NAME_modifiedItem, ti, new, 0);

    succeed;
  }

  fail;
}


		/********************************
		*         MISCELLANEOUS		*
		********************************/

static status
WantsKeyboardFocusTextItem(TextItem ti)
{ if ( ti->displayed == ON &&
       ti->active == ON &&
       ti->editable == ON &&
       notNil(ti->device) )

    succeed;

  fail;
}


status
makeClassTextItem(Class class)
{ sourceClass(class, makeClassTextItem, __FILE__, "$Revision$");

  localClass(class, NAME_selection, NAME_selection, "any", NAME_none,
	     "Current value");
  localClass(class, NAME_default, NAME_apply, "any|function", NAME_none,
	     "The default value");
  localClass(class, NAME_printName, NAME_textual, "char_array", NAME_none,
	     "Text-representation of <->selection");
  localClass(class, NAME_type, NAME_type, "type", NAME_both,
	     "Value-type of the selection");
  localClass(class, NAME_valueSet, NAME_complete, "[chain|function]*",
	     NAME_both,
	     "Set of possible values");
  localClass(class, NAME_advance, NAME_action, "{next,clear,none}", NAME_both,
	     "If `next', proceed to next in window");
  localClass(class, NAME_length, NAME_area, "int", NAME_get,
	     "Length of entry field in characters");
  localClass(class, NAME_valueFont, NAME_appearance, "font", NAME_get,
	     "Font for entry field");
  localClass(class, NAME_labelFont, NAME_appearance, "font", NAME_get,
	     "Font for label");
  localClass(class, NAME_showLabel, NAME_appearance, "bool", NAME_get,
	     "Whether label is visible");
  superClass(class, NAME_valueText, NAME_visualisation, "text",
	     NAME_get, NAME_delegate,
	     "Graphical text object for selection");
  localClass(class, NAME_editable, NAME_event, "bool", NAME_get,
	     "TextItem may be edited");
  localClass(class, NAME_valueWidth, NAME_layout, "[int]", NAME_none,
	     "Width of the value-part in pixels");

  termClass(class, "text_item", 3, NAME_label, NAME_selection, NAME_message);
  setRedrawFunctionClass(class, RedrawAreaTextItem);

  storeMethod(class, NAME_selection, selectionTextItem);
  storeMethod(class, NAME_labelFont, labelFontTextItem);
  storeMethod(class, NAME_showLabel, showLabelTextItem);
  storeMethod(class, NAME_valueFont, valueFontTextItem);
  storeMethod(class, NAME_status,    statusTextItem);
  storeMethod(class, NAME_length,    lengthTextItem);
  storeMethod(class, NAME_editable,  editableTextItem);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "name=[name]", "default=[any|function]", "message=[code]*",
	     "Create from label, selection and message",
	     initialiseTextItem);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Compute desired size",
	     computeTextItem);
  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Resize the image",
	     geometryTextItem);
  sendMethod(class, NAME_clear, NAME_selection, 0,
	     "Clear entry field",
	     clearTextItem);
  sendMethod(class, NAME_restore, NAME_apply, 0,
	     "Set ->selection to <-default",
	     restoreTextItem);
  sendMethod(class, NAME_default, NAME_apply, 1, "any|function",
	     "Set variable -default and ->selection",
	     defaultTextItem);
  sendMethod(class, NAME_apply, NAME_apply, 1, "[bool]",
	     "->execute if <-modified or @on",
	     applyTextItem);
  sendMethod(class, NAME_enter, NAME_action, 1, "[event_id]",
	     "Default handling for RETURN",
	     enterTextItem);
  sendMethod(class, NAME_displayedValue, NAME_textual, 1, "char_array",
	     "Visible (typed) textual value",
	     displayedValueTextItem);

  sendMethod(class, NAME_keyboardQuit, NAME_complete, 0,
	     "Remove completer and ->alert",
	     keyboardQuitTextItem);
  sendMethod(class, NAME_selectCompletion, NAME_complete, 2,
	     "value_set=chain", "prefix=[char_array]",
	     "Select candidate expansion using browser",
	     selectCompletionTextItem);
  sendMethod(class, NAME_selectedCompletion, NAME_complete, 1, "dict_item",
	     "Handle selection from browser",
	     selectedCompletionTextItem);
  sendMethod(class, NAME_complete, NAME_complete, 1, "[event_id]",
	     "Complete current value",
	     completeTextItem);
  sendMethod(class, NAME_indicateDirectory, NAME_complete, 1, "text=string",
	     "Indicate current value is a `directory'",
	     indicateDirectoryTextItem);
  sendMethod(class, NAME_reset, NAME_abort, 0,
	     "Remove completer",
	     quitCompleterTextItem);
  sendMethod(class, NAME_quitCompleter, NAME_abort, 0,
	     "Remove completer",
	     quitCompleterTextItem);

  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Process user event",
	     eventTextItem);
  sendMethod(class, NAME_typed, NAME_event, 1, "event_id",
	     "Process event with given id",
	     typedTextItem);
  sendMethod(class, NAME_execute, NAME_action, 0,
	     "Execute related message",
	     executeTextItem);
  sendMethod(class, NAME_width, NAME_area, 1, "characters=int",
	     "Equivalent to ->length",
	     lengthTextItem);
  sendMethod(class, NAME_WantsKeyboardFocus, NAME_event, 0,
	     "Test if ready to accept input",
	     WantsKeyboardFocusTextItem);
  sendMethod(class, NAME_next, NAME_caret, 0,
	     "Advance to next item in same <-device",
	     nextTextItem);
  sendMethod(class, NAME_labelWidth, NAME_layout, 1, "[int]",
	     "Width of label in pixels",
	     labelWidthTextItem);
  sendMethod(class, NAME_paste, NAME_selection, 1, "[int]",
	     "Paste value of cut-buffer",
	     pasteTextItem);
  sendMethod(class, NAME_modified, NAME_apply, 1, "bool",
	     "Reset modified flag",
	     modifiedTextItem);
  sendMethod(class, NAME_delegate, NAME_delegate,
	     3, "program_object", "text", "unchecked ...",
	     "Delegate to <-value_text and update",
	     delegateTextItem);
  sendMethod(class, NAME_valueWidth, NAME_area, 1, "int",
	     "Set width of value-part in pixels",
	     valueWidthTextItem);

  getMethod(class, NAME_labelWidth, NAME_layout, "int", 0,
	    "Width required to display label",
	    getLabelWidthTextItem);
  getMethod(class, NAME_selection, NAME_selection, "any", 0,
	    "Current value of the selection",
	    getSelectionTextItem);
  getMethod(class, NAME_modified, NAME_apply, "bool", 0,
	    "Test if item has been modified",
	    getModifiedTextItem);
  getMethod(class, NAME_printNameOfValue, NAME_textual, "char_array", 1, "any",
	    "Determine printable representation",
	    getPrintNameOfValueTextItem);
  getMethod(class, NAME_default, NAME_apply, "any", 0,
	    "Current default value",
	    getDefaultTextItem);
  getMethod(class, NAME_displayedValue, NAME_textual, "char_array", 0,
	    "Visible (typed) textual value",
	    getDisplayedValueTextItem);

  getMethod(class, NAME_splitCompletion, NAME_complete, "char_array|tuple", 1,
	    "value=char_array",
	    "Split value in `directory'- and `file' part",
	    getSplitCompletionTextItem);
  getMethod(class, NAME_completions, NAME_complete, "chain", 1,
	    "char_array|tuple",
	    "Chain of `files' for dir/file",
	    getCompletionsTextItem);
  getMethod(class, NAME_reference, DEFAULT, "point", 0,
	    "Baseline of label",
	    getReferenceTextItem);


  attach_resource(class, "length",     "int", "25",
		  "Width of area for selection (chars)");
  attach_resource(class, "pen",        "int", "1",
		  "Thickness of line below selection");
  attach_resource(class, "search_ignore_case", "bool", "@on",
		  "@on: ignore case for completion");
  attach_resource(class, "border", "0..", "0",
		  "Border around <-value_text");

  succeed;
}

