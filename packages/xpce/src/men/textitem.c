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
static status	valueWidthTextItem(TextItem ti, Int val);
static int	combo_width(TextItem ti);
static int	combo_flags(TextItem ti);


static status
initialiseTextItem(TextItem ti, Name name, Any val, Code msg)
{ CharArray str;

  if ( isDefault(name) )
    name = NAME_textItem;
  if ( isDefault(val) )
    val = NAME_;

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
  assign(ti, hor_stretch,   toInt(100));
  assign(ti, style,	    NAME_normal);

  if ( (str = get(ti, NAME_printNameOfValue, val, 0)) )
    valueString(ti->print_name, str);
  resetTextItem(ti);

  return requestComputeGraphical(ti, DEFAULT);
}


static status
RedrawAreaTextItem(TextItem ti, Area a)
{ int x, y, w, h;
  int al, av, am;
  int lw, lh;
  int fw = valInt(getExFont(ti->value_text->font));
  Elevation z = getResourceValueObject(ti, NAME_elevation);
  int tx, ty, tw, th;
  TextObj vt = ti->value_text;
  int flags = 0;

  initialiseDeviceGraphical(ti, &x, &y, &w, &h);

  al = valInt(getAscentFont(ti->label_font));
  av = valInt(getAscentFont(vt->font)) + valInt(vt->border);
  am = max(al, av);

  compute_label_text_item(ti, &lw, &lh);
  if ( ti->show_label == ON )
  { RedrawLabelDialogItem(ti,
			  accelerator_code(ti->accelerator),
			  x, y+am-al, lw-fw, h,
			  ti->label_format, NAME_top,
			  ti->active == ON ? 0 : LABEL_INACTIVE);
  }
      
  tx = x+lw;
  ty = y+am-av;
  tw = valInt(vt->area->w);
  th = valInt(vt->area->h);

  if ( ti->editable == ON && ti->active == ON )
    flags |= TEXTFIELD_EDITABLE;
  flags |= combo_flags(ti);

  if ( !ws_entry_field(tx, ty, tw+combo_width(ti), th, flags) )
  { if ( ti->editable == ON )
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
  { if ( isDefault(ti->label_font) )
      obtainResourcesObject(ti);

    dia_label_size(ti, lw, lh, NULL);
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
    int cwb = combo_width(ti);

    obtainResourcesObject(ti);
    fontText(ti->value_text, ti->value_font);
    borderText(ti->value_text, b);
    if ( notDefault(ti->value_width) )
    { Int vw = toInt(valInt(ti->value_width) - cwb);
      marginText(ti->value_text, vw, NAME_clip);
    } else
      lengthText(ti->value_text, ti->length);
    ComputeGraphical(ti->value_text);

    compute_label_text_item(ti, &lw, &lh);
    h = max(lh, valInt(ti->value_text->area->h));
    w = lw + valInt(ti->value_text->area->w) + cwb;

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
  { TextObj vt = ti->value_text;
    int ry;

    ComputeGraphical(vt);
    ry = valInt(getAscentFont(vt->font)) + valInt(vt->border);

    if ( ti->show_label == ON )
      ry = max(valInt(getAscentFont(ti->label_font)), ry);

    ref = answerObject(ClassPoint, ZERO, toInt(ry), 0);
  }
  
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

Browser
CompletionBrowser()
{ if ( !Completer )
  { KeyBinding kb;
    Any client, lb;
    Any quit;

    Completer = globalObject(NAME_completer, ClassBrowser, 0);
    client    = newObject(ClassObtain, Completer, NAME_client, 0);
    quit      = newObject(ClassMessage, client, NAME_keyboardQuit, 0);

    protectObject(Completer);
    protectObject(Completer->frame);
    attributeObject(Completer, NAME_client, NIL);
    attributeObject(Completer, NAME_prefix, NAME_);
    attributeObject(Completer, NAME_autoHide, ZERO);
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

    if ( (lb = get(Completer, NAME_listBrowser, 0)) )
    { send(lb, NAME_recogniser,
	   newObject(ClassHandler, NAME_locMove,
		     newObject(ClassMessage, lb, NAME_selection,
			       newObject(ClassObtain, lb, NAME_dictItem,
					 Arg(1), 0), 0), 0), 0);
      send(Completer, NAME_recogniser,
	   newObject(ClassHandler, NAME_msLeftDown,
		     newObject(ClassIf,
			       newObject(ClassObtain, lb, NAME_dictItem,
					 EVENT, 0),
			       newObject(ClassMessage, Arg(1), NAME_post, lb, 0),
			       quit, 0), 0), 0);
    }
  }

  return Completer;
}
				       

status
completerShownDialogItem(Any di)
{ if ( Completer && getAttributeObject(Completer, NAME_client) == di )
    succeed;

  fail;
}


status
quitCompleterDialogItem(Any di)
{ if ( completerShownDialogItem(di) )
  { PceWindow sw;
    Browser c = CompletionBrowser();

    if ( (sw = getWindowGraphical(di)) )
    { grabPointerWindow(sw, OFF);
      focusWindow(sw, NIL, NIL, NIL, NIL);
    }

    send(c, NAME_clear, 0);
    send(c, NAME_client, NIL, 0);
    send(c, NAME_show, OFF, 0);
    send(c, NAME_transientFor, NIL, 0);
    if ( combo_width(di) )
      changedDialogItem(di);		/* indicator will change */
  }

  succeed;
}


status
selectCompletionDialogItem(Any item, Chain matches,
			   CharArray searchstring,
			   Int autohide)
{ Browser c = CompletionBrowser();
  DialogItem di = item;
  Any val;
  Point pos;
  int lw;
  int fw;
  PceWindow sw;
  int lines;				/* lines of the browser */
  int bh;				/* height of the browser */

  ComputeGraphical(di);

  if ( isDefault(di->label_width) )
    lw = valInt(get(di, NAME_labelWidth, 0));
  else
    lw = valInt(di->label_width);
  fw = valInt(di->area->w) - lw;
  fw = max(fw, 50);

  if ( isDefault(searchstring) )
    searchstring = NIL;
  if ( isDefault(autohide) )
    autohide = (notNil(searchstring) ? getSizeCharArray(searchstring) : ZERO);

  send(c, NAME_client, di, 0);
  send(c, NAME_autoHide, autohide, 0);
  if ( notNil(matches) )
  { send(c, NAME_clear, 0);

    for_chain(matches, val,
	      send(c, NAME_append, get(val, NAME_printName, 0), 0));
  }

  lines = valInt(getSizeChain(c->list_browser->dict->members));
  if ( lines > 6 )
    lines = 6;				/* resource? */
  bh = lines * valInt(getHeightFont(c->list_browser->font));
  bh += 2 * TXT_X_MARGIN + 2;

  send((pos = get(di, NAME_displayPosition, 0)), NAME_offset,
       toInt(lw), di->area->h, 0);
  send(c, NAME_transientFor, getFrameGraphical((Graphical) di), 0);
  send(c->frame, NAME_set, pos->x, pos->y, toInt(fw), toInt(bh), 0);
  send(c, NAME_open, DEFAULT, ON, 0);	/* pos, normalise */
  if ( (sw = getWindowGraphical((Graphical)di)) )
  { grabPointerWindow(sw, ON);
    focusWindow(sw, (Graphical)di, DEFAULT, DEFAULT, NIL);
  }
  send(c, NAME_cancelSearch, 0);
  if ( isDefault(searchstring) )
  { send(c, NAME_extendPrefix, 0);
  } else if ( notNil(searchstring) )
  { assign(c->list_browser, search_string,
	   newObject(ClassString, name_procent_s, searchstring, 0));
    if ( !executeSearchListBrowser(c->list_browser) )
      send(c, NAME_cancelSearch, 0);
  }

  succeed;
}


static status
selectCompletionTextItem(TextItem ti, Chain matches,
			 CharArray prefix, CharArray searchstring,
			 Int autohide)
{ Browser c = CompletionBrowser();

  if ( isDefault(prefix) || isNil(prefix) )
    prefix = (CharArray)NAME_;

  send(c, NAME_prefix, prefix, 0);

  if ( combo_width(ti) )
    changedDialogItem(ti);

  return selectCompletionDialogItem((DialogItem)ti, matches,
				    searchstring, autohide);
}


static status
enterCompleterTextItem(TextItem ti)
{ if ( completerShownDialogItem(ti) )
  { send(CompletionBrowser(), NAME_enter, 0);

    quitCompleterDialogItem(ti);
  }

  succeed;
}


static status
selectedCompletionTextItem(TextItem ti, DictItem di)
{ Any c = CompletionBrowser();
  Any prefix = get(c, NAME_prefix, 0);

  displayedValueTextItem(ti, getAppendCharArray(prefix, (CharArray) di->key));
  quitCompleterDialogItem(ti);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the completions.  `Prefix' is the text entered sofar.  Dirp will
be filled with the `common prefix' that is not to be shown in the browser.
Filep is the start of the text typed.  Filesp is a chain of possible vaues.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
completions(TextItem ti, CharArray prefix, Bool all,
	    CharArray *dirp, CharArray *filep, Chain *filesp)
{ Any split;

  if ( (split = get(ti, NAME_splitCompletion, prefix, 0)) )
  { Chain files;

    if ( all == ON )
    { if ( instanceOfObject(split, ClassTuple) )
	assign(((Tuple)split), second, NAME_);
      else
	split = NAME_;
    }

    if ( (files = get(ti, NAME_completions, split, 0)) &&
	 (files = checkType(files, TypeChain, NIL)) )
    { if ( instanceOfObject(split, ClassTuple) )
      { *dirp  = ((Tuple)split)->first;
	*filep = ((Tuple)split)->second;
      } else
      { *dirp  = NIL;
	*filep = split;
      }
  
      *filesp = files;
  
      succeed;
    }
  }
    
  fail;
}


static status
completeTextItem(TextItem ti, EventId id)
{ Any c = CompletionBrowser();

  if ( completerShownDialogItem(ti) )
  { send(c, NAME_extendPrefix, 0);
  } else
  { CharArray dir, file;
    Chain files;

    if ( completions(ti, ti->value_text->string, OFF, &dir, &file, &files) )
    { Tuple t;
      int dirmode;
      Bool ignore_case = getResourceValueObject(ti, NAME_searchIgnoreCase);
      
      if ( !(dirmode = notNil(dir)) )
	dir = (CharArray)NAME_;

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
	    send(ti, NAME_selectCompletion,
		 t->first,
		 dirmode ? dir : DEFAULT,
		 file,
		 0);
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
       (rval = getForwardReceiverFunction(ti->value_set, ti, base, 0)) )
    answer(rval);

  fail;
}


static status
indicateDirectoryTextItem(TextItem ti, StringObj dir)
{ succeed;
}


		 /*******************************
		 *      COMBO-BOX HANDLING      *
		 *******************************/

static Bool
getHasCompletionsTextItem(TextItem ti)
{ Chain vset;

#if 0					/* if you can program it anyway ... */
  Any rec;
  GetMethod m;

					/* <-split_completions is redefined */
  if ( (m = resolveGetMethodObject(ti, NULL, NAME_splitCompletion, &rec)) &&
       (!instanceOfObject(m, ClassGetMethod) ||
	m->function != getSplitCompletionTextItem) )
    answer(ON);
					/* <-completions is redefined */
  if ( (m = resolveGetMethodObject(ti, NULL, NAME_completions, &rec)) &&
       (!instanceOfObject(m, ClassGetMethod) ||
	m->function != getCompletionsTextItem) )
    answer(ON);
#endif

  if ( isNil(ti->value_set) )
    answer(OFF);
  if ( isDefault(ti->value_set) )
  { if ( (vset=getValueSetType(ti->type, NIL)) )
    { doneObject(vset);
      answer(ON);
    }
    answer(OFF);
  }

  answer(ON);
}


static status
styleTextItem(TextItem ti, Name style)
{ if ( isDefault(style) )
  { if ( get(ti, NAME_hasCompletions, 0) == ON )
      style = NAME_comboBox;
    else
      style = NAME_normal;
  }

  return assignGraphical(ti, NAME_style, style);
}


static status
typeTextItem(TextItem ti, Type type)
{ assign(ti, type, type);
  send(ti, NAME_style, DEFAULT, 0);
  succeed;
}


static status
valueSetTextItem(TextItem ti, Chain set)
{ assign(ti, value_set, set);
  send(ti, NAME_style, DEFAULT, 0);
  succeed;
}


static int
combo_flags(TextItem ti)
{ int flags = 0;

  if ( ti->style == NAME_comboBox )
  { flags |= TEXTFIELD_COMBO;

    if ( completerShownDialogItem(ti) )
      flags |= TEXTFIELD_COMBO_DOWN;
  }

  return flags;
}


static int
combo_width(TextItem ti)
{ if ( ti->style == NAME_comboBox )
    return ws_combo_box_width();

  return 0;
}


static status
showComboBoxTextItem(TextItem ti, Bool val)
{ if ( val == OFF )
    return quitCompleterDialogItem(ti);
  else
  { CharArray file;
    CharArray dir;
    Chain files;

    if ( completions(ti, ti->value_text->string, ON, &dir, &file, &files) &&
	 !emptyChain(files) )
    { return send(ti, NAME_selectCompletion,
		  files, dir, ti->value_text->string, ZERO, 0);
    }

    fail;
  }
}


		/********************************
		*        EVENT HANDLING		*
		********************************/


static status
eventTextItem(TextItem ti, EventObj ev)
{ if ( completerShownDialogItem(ti) )
  { Browser c = CompletionBrowser();

    if ( isAEvent(ev, NAME_keyboard) )
    { KeyBinding kb = (ti->editable == ON ? KeyBindingTextItem()
			 		  : KeyBindingTextItemView());
      Any id = ev->id;
      Name f = getFunctionKeyBinding(kb, id);

      if ( f != NAME_complete && f != NAME_keyboardQuit )
      { postEvent(ev, (Graphical)c, DEFAULT);

	f = getFunctionKeyBinding(c->list_browser->key_binding, id);
	if ( f == NAME_backwardDeleteChar )
	{ Int autohide = getAttributeObject(c, NAME_autoHide);
	
	  if ( autohide != ZERO )
	  { StringObj ss = c->list_browser->search_string;
	    if ( isNil(ss) || valInt(autohide) > valInt(getSizeCharArray(ss)) )
	    quitCompleterDialogItem(ti);
	  }
	}

	succeed;
      }

      return send(ti, NAME_typed, id, 0);
    }	 
    
    if ( isAEvent(ev, NAME_msLeftDown) &&
	 !insideEvent(ev, (Graphical)c) )
      return quitCompleterDialogItem(ti);

    return postEvent(ev, (Graphical)c, DEFAULT);
  }

  if ( eventDialogItem(ti, ev) )
    succeed;

  if ( ti->active == OFF )
    fail;

  if ( isAEvent(ev, NAME_msLeftDown) )
  { if ( WantsKeyboardFocusTextItem(ti) )
    { int cbw;

      send(ti, NAME_keyboardFocus, 0);
      
      if ( (cbw = combo_width(ti)) > 0 )
      { Int X, Y;
	int x, y;

	get_xy_event(ev, ti, ON, &X, &Y);
	x = valInt(X); y = valInt(Y);
	if ( y >= 0 &&
	     y <= valInt(ti->area->h) &&
	     x <= valInt(ti->area->w) &&
	     x >= valInt(ti->area->w) - cbw )
	{ Bool val = (completerShownDialogItem(ti) ? OFF : ON);

	  send(ti, NAME_showComboBox, val, 0);
	}

	succeed;
      }
    } else
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
    { return send(ti, NAME_typed, ev->id, 0);
    } else
    { if ( isAEvent(ev, NAME_msMiddleUp) )
	return pasteTextItem(ti, DEFAULT);
    }
  }

  fail;
}


static status
keyboardQuitTextItem(TextItem ti)
{ quitCompleterDialogItem(ti);
  send(ti, NAME_alert, 0);

  succeed;
}


static status
executeTextItem(TextItem ti)
{ return applyTextItem(ti, ON);
}


static status
keyTextItem(TextItem ti, Name key)
{ if ( ti->accelerator == key && WantsKeyboardFocusTextItem(ti) )
    return send(ti, NAME_keyboardFocus, 0);

  fail;
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

  if ( !(notNil(dev) &&
	 !instanceOfObject(dev, ClassEditor) && /* HACK */
	 send(dev, NAME_typed, id, ON, 0)) &&
       !isFreedObj(ti) )
  { int modified = (getModifiedTextItem(ti) == ON);

    if ( applyTextItem(ti, OFF) && !isFreedObj(ti) )
    { if ( ti->advance == NAME_clear )
      { if ( modified )
	  selectionTextItem(ti, NAME_);
      } else if ( ti->advance == NAME_next )
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
  return send(ti, NAME_selection, val, 0);
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
  
  stringText(ti->value_text, (CharArray) NAME_);
  if ( modified && hasSendMethodObject(ti->device, NAME_modifiedItem) )
    send(ti->device, NAME_modifiedItem, ti, ON, 0);
  
  quitCompleterDialogItem(ti);
  return requestComputeGraphical(ti, DEFAULT);
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
{ quitCompleterDialogItem(ti);
    
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
      send(ti->device, NAME_advance, ti, 0);
    changedDialogItem(ti);
  }

  succeed;
}


static status
lengthTextItem(TextItem ti, Int w)
{ assign(ti, hor_stretch, ZERO);

  if ( ti->length != w )
  { assign(ti, length, w);
    valueWidthTextItem(ti, DEFAULT);
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
    int chars = (valInt(val) - combo_width(ti)) / valInt(ex);

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
    int vw;

    compute_label_text_item(ti, &lw, &lh);
    vw = valInt(w) - lw;
    if ( vw < 15 )
      vw = 15;
    valueWidthTextItem(ti, toInt(vw));
  }

  return geometryGraphical(ti, x, y, DEFAULT, DEFAULT);
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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=[name]", "default=[any|function]", "message=[code]*" };
static char *T_delegate[] =
        { "program_object", "text", "unchecked ..." };
static char *T_selectCompletion[] =
        { "value_set=chain", "prefix=[char_array]*",
	  "search=[char_array]*", "auto_hide=[0..]" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_textItem[] =
{ SV(NAME_selection, "any", IV_NONE|IV_STORE, selectionTextItem,
     NAME_selection, "Current value"),
  IV(NAME_default, "any|function", IV_NONE,
     NAME_apply, "The default value"),
  IV(NAME_printName, "char_array", IV_NONE,
     NAME_textual, "Text-representation of <->selection"),
  SV(NAME_type, "type", IV_BOTH|IV_STORE, typeTextItem,
     NAME_type, "Value-type of the selection"),
  SV(NAME_valueSet, "[chain|function]*", IV_GET|IV_STORE, valueSetTextItem,
     NAME_complete, "Set of possible values"),
  IV(NAME_advance, "{next,clear,none}", IV_BOTH,
     NAME_action, "If `next', proceed to next in window"),
  SV(NAME_length, "int", IV_GET|IV_STORE, lengthTextItem,
     NAME_area, "Length of entry field in characters"),
  SV(NAME_valueFont, "font", IV_GET|IV_STORE, valueFontTextItem,
     NAME_appearance, "Font for entry field"),
  SV(NAME_showLabel, "bool", IV_GET|IV_STORE, showLabelTextItem,
     NAME_appearance, "Whether label is visible"),
  SV(NAME_valueText, "text", IV_GET|IV_SUPER, NAME_delegate,
     NAME_visualisation, "Graphical text object for selection"),
  SV(NAME_editable, "bool", IV_GET|IV_STORE, editableTextItem,
     NAME_event, "TextItem may be edited"),
  IV(NAME_valueWidth, "[int]", IV_NONE,
     NAME_layout, "Width of the value-part in pixels"),
  IV(NAME_horStretch, "0..100", IV_BOTH,
     NAME_layout, "Horizontal stretchability"),
  IV(NAME_style, "{normal,combo_box}", IV_GET,
     NAME_appearance, "Show combo-box item for completions")
};

/* Send Methods */

static senddecl send_textItem[] =
{ SM(NAME_compute, 0, NULL, computeTextItem,
     DEFAULT, "Compute desired size"),
  SM(NAME_geometry, 4, T_geometry, geometryTextItem,
     DEFAULT, "Resize the image"),
  SM(NAME_initialise, 3, T_initialise, initialiseTextItem,
     DEFAULT, "Create from label, selection and message"),
  SM(NAME_status, 1, "{inactive,active,preview,execute}", statusTextItem,
     DEFAULT, "Status for event-processing"),
  SM(NAME_quitCompleter, 0, NULL, quitCompleterDialogItem,
     NAME_abort, "Remove completer"),
  SM(NAME_reset, 0, NULL, quitCompleterDialogItem,
     NAME_abort, "Remove completer"),
  SM(NAME_enter, 1, "[event_id]", enterTextItem,
     NAME_action, "Default handling for RETURN"),
  SM(NAME_execute, 0, NULL, executeTextItem,
     NAME_action, "Execute related message"),
  SM(NAME_apply, 1, "[bool]", applyTextItem,
     NAME_apply, "->execute if <-modified or @on"),
  SM(NAME_default, 1, "any|function", defaultTextItem,
     NAME_apply, "Set variable -default and ->selection"),
  SM(NAME_modified, 1, "bool", modifiedTextItem,
     NAME_apply, "Reset modified flag"),
  SM(NAME_restore, 0, NULL, restoreTextItem,
     NAME_apply, "Set ->selection to <-default"),
  SM(NAME_valueWidth, 1, "int", valueWidthTextItem,
     NAME_area, "Set width of value-part in pixels"),
  SM(NAME_width, 1, "characters=int", lengthTextItem,
     NAME_area, "Equivalent to ->length"),
  SM(NAME_next, 0, NULL, nextTextItem,
     NAME_caret, "Advance to next item in same <-device"),
  SM(NAME_style, 1, "[{normal,combo_box}]", styleTextItem,
     DEFAULT, "Set style or termine default style"),
  SM(NAME_complete, 1, "[event_id]", completeTextItem,
     NAME_complete, "Complete current value"),
  SM(NAME_indicateDirectory, 1, "text=string", indicateDirectoryTextItem,
     NAME_complete, "Indicate current value is a `directory'"),
  SM(NAME_keyboardQuit, 0, NULL, keyboardQuitTextItem,
     NAME_complete, "Remove completer and ->alert"),
  SM(NAME_selectCompletion, 4, T_selectCompletion, selectCompletionTextItem,
     NAME_complete, "Select candidate expansion using browser"),
  SM(NAME_selectedCompletion, 1, "dict_item", selectedCompletionTextItem,
     NAME_complete, "Handle selection from browser"),
  SM(NAME_showComboBox, 1, "bool", showComboBoxTextItem,
     NAME_complete, "Show the combo-box browser"),
  SM(NAME_delegate, 3, T_delegate, delegateTextItem,
     NAME_delegate, "Delegate to <-value_text and update"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, WantsKeyboardFocusTextItem,
     NAME_event, "Test if ready to accept input"),
  SM(NAME_event, 1, "event", eventTextItem,
     NAME_event, "Process user event"),
  SM(NAME_typed, 1, "event_id", typedTextItem,
     NAME_event, "Process event with given id"),
  SM(NAME_key, 1, "key=name", keyTextItem,
     NAME_accelerator, "Request keyboard if accelerator is typed"),
  SM(NAME_labelWidth, 1, "[int]", labelWidthTextItem,
     NAME_layout, "Width of label in pixels"),
  SM(NAME_clear, 0, NULL, clearTextItem,
     NAME_selection, "Clear entry field"),
  SM(NAME_paste, 1, "[int]", pasteTextItem,
     NAME_selection, "Paste value of cut-buffer"),
  SM(NAME_displayedValue, 1, "char_array", displayedValueTextItem,
     NAME_textual, "Visible (typed) textual value")
};

/* Get Methods */

static getdecl get_textItem[] =
{ GM(NAME_reference, 0, "point", NULL, getReferenceTextItem,
     DEFAULT, "Baseline of label"),
  GM(NAME_default, 0, "any", NULL, getDefaultTextItem,
     NAME_apply, "Current default value"),
  GM(NAME_modified, 0, "bool", NULL, getModifiedTextItem,
     NAME_apply, "Test if item has been modified"),
  GM(NAME_completions, 1, "chain", "char_array|tuple", getCompletionsTextItem,
     NAME_complete, "Chain of `files' for dir/file"),
  GM(NAME_splitCompletion, 1, "char_array|tuple", "value=char_array",
     getSplitCompletionTextItem,
     NAME_complete, "Split value in `directory'- and `file' part"),
  GM(NAME_hasCompletions, 0, "bool", NULL, getHasCompletionsTextItem,
     NAME_complete, "Return @on if item can generate completions"),
  GM(NAME_labelWidth, 0, "int", NULL, getLabelWidthTextItem,
     NAME_layout, "Width required to display label"),
  GM(NAME_selection, 0, "any", NULL, getSelectionTextItem,
     NAME_selection, "Current value of the selection"),
  GM(NAME_displayedValue, 0, "char_array", NULL, getDisplayedValueTextItem,
     NAME_textual, "Visible (typed) textual value"),
  GM(NAME_printNameOfValue, 1, "char_array", "any", getPrintNameOfValueTextItem,
     NAME_textual, "Determine printable representation")
};

/* Resources */

static resourcedecl rc_textItem[] =
{ RC(NAME_border, "0..", "0",
     "Border around <-value_text"),
  RC(NAME_length, "int", "25",
     "Width of area for selection (chars)"),
  RC(NAME_pen, "int", "1",
     "Thickness of line below selection"),
  RC(NAME_searchIgnoreCase, "bool", "@on",
     "@on: ignore case for completion")
};

/* Class Declaration */

static Name textItem_termnames[] = { NAME_label, NAME_selection, NAME_message };

ClassDecl(textItem_decls,
          var_textItem, send_textItem, get_textItem, rc_textItem,
          3, textItem_termnames,
          "$Rev$");

status
makeClassTextItem(Class class)
{ declareClass(class, &textItem_decls);
  setRedrawFunctionClass(class, RedrawAreaTextItem);

  succeed;
}

