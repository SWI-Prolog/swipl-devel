/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/dialog.h>
#include <h/text.h>

static Any	getSelectionTextItem(TextItem);
static Type	getSelectionTypeTextItem(TextItem);
static Bool	getModifiedTextItem(TextItem);
static Any	getDefaultTextItem(TextItem);
static void	compute_label_text_item(TextItem, int *, int *);
static status	WantsKeyboardFocusTextItem(TextItem);
static status	restoreTextItem(TextItem ti);
static status	selectionTextItem(TextItem ti, Any selection);
static status	resetTextItem(TextItem ti);
static int	combo_flags(TextItem ti);
static status	detachTimerTextItem(TextItem ti);

#define STEPPER_BOX_W   14
#define STEPPER_BOX_GAP 5

status
initialiseTextItem(TextItem ti, Name name, Any val, Code msg)
{ CharArray str;

  if ( isDefault(name) )
    name = getClassNameObject(ti);
  if ( isDefault(val) )
    val = NAME_;

  createDialogItem(ti, name);

  assign(ti, message, 	       msg);
  assign(ti, value_set,	       DEFAULT);
  assign(ti, value_width,      DEFAULT);
  assign(ti, print_name,       CtoString(""));

  assign(ti, advance,          NAME_next);
  assign(ti, show_label,       ON);
  assign(ti, value_text,       newObject(ClassText, EAV));
  assign(ti, editable,	       ON);

  assign(ti, default_value,    val);
  assign(ti, selection,        getDefaultTextItem(ti));
  assign(ti, type,	       getSelectionTypeTextItem(ti));
  assign(ti, auto_value_align, OFF);
  assign(ti, hor_stretch,      toInt(100));
  assign(ti, style,	       NAME_normal);

  if ( (str = get(ti, NAME_printNameOfValue, val, EAV)) )
    valueString(ti->print_name, str);
  resetTextItem(ti);

  return requestComputeGraphical(ti, DEFAULT);
}


static status
unlinkTextItem(TextItem ti)
{ detachTimerTextItem(ti);

  return unlinkDialogItem((DialogItem) ti);
}


static status
RedrawAreaTextItem(TextItem ti, Area a)
{ int x, y, w, h;
  int al, av, am;
  int lw, lh;
  int fw = valInt(getExFont(ti->value_text->font));
  Elevation z = getClassVariableValueObject(ti, NAME_elevation);
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

  if ( !ws_entry_field(tx, ty, tw+text_item_combo_width(ti), th, flags) )
  { if ( flags & TEXTFIELD_EDITABLE )
    { if ( z && notNil(z) )
      { int zh = abs(valInt(z->height));
	int ly = y+am+zh+valInt(getDescentFont(vt->font));
  
	r_3d_line(x+lw, ly, x+lw+tw, ly, z, TRUE);
      } else if ( ti->pen != ZERO )
      { int pen = valInt(ti->pen);
	int ly = y+am+1+pen/2;
      
	r_dash(ti->texture);
	r_thickness(valInt(ti->pen));
  
	r_line(x+lw, ly, x+lw+tw, ly);
      }
    }
    if ( flags & (TEXTFIELD_COMBO|TEXTFIELD_COMBO_DOWN) )
    { int trh = 8;
      int trw = 9;
      int trx, try;
      int up = (flags & TEXTFIELD_COMBO) ? TRUE : FALSE;
  
      trx = tx+tw+5;
      try = y + (h-trh)/2;
  
      r_3d_triangle(trx+trw/2, try+trh, trx, try, trx+trw, try, z, up, 0x3);
    }
    if ( flags & TEXTFIELD_STEPPER )
    { int sw = STEPPER_BOX_W;
      int bx = x+w-sw;
      int bh = (h+1)/2;
      int iw, ih, ix, dy;
      Elevation e = getClassVariableValueClass(ClassButton, NAME_elevation);

      r_3d_box(bx, y,    sw, bh,   0, e, !(flags & TEXTFIELD_INCREMENT));
      r_3d_box(bx, y+bh, sw, h-bh, 0, e, !(flags & TEXTFIELD_DECREMENT));

      iw = valInt(INT_ITEM_IMAGE->size->w)/2;
      ih = valInt(INT_ITEM_IMAGE->size->h);
      ix = x + w - (sw+iw+1)/2;
      dy = (bh-ih+1)/2;

      r_image(INT_ITEM_IMAGE, 0,  0, ix, y+dy,      iw, ih, ON);
      r_image(INT_ITEM_IMAGE, iw, 0, ix, y+h-dy-ih, iw, ih, ON);
    }
  }

  if ( notDefault(vt->colour) )
  { Any old = r_colour(vt->colour);
    repaintText(vt, tx, ty, tw, th);
    r_colour(old);
  } else
    repaintText(vt, tx, ty, tw, th);

  return RedrawAreaGraphical(ti, a);
}


static status
updateShowCaretTextItem(TextItem ti)
{ Any old = ti->value_text->show_caret;

  if ( ti->status != NAME_inactive )
  { PceWindow sw = getWindowGraphical((Graphical)ti);
    int active = (sw && sw->input_focus == ON);

    showCaretText(ti->value_text, active ? (Any)ON : (Any)NAME_passive);  
  } else
    showCaretText(ti->value_text, OFF);

  if ( old != ti->value_text->show_caret )
  { send(ti, NAME_activate, ti->value_text->show_caret == ON ? ON : OFF, EAV);

    return requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


static status
activateTextItem(TextItem ti, Bool val)
{ if ( getClassVariableValueObject(ti, NAME_autoSelect) == ON )
  { if ( val == ON )
    { send(ti->value_text, NAME_selection,
	   ZERO, getSizeCharArray(ti->value_text->string), EAV);
      send(ti, NAME_caret, DEFAULT, EAV);
    } else
      send(ti->value_text, NAME_selection, NIL, EAV);
  }

  succeed;
}


static status
selectAllTextItem(TextItem ti)
{ send(ti->value_text, NAME_selection,
       ZERO, getSizeCharArray(ti->value_text->string), EAV);

  return requestComputeGraphical(ti, DEFAULT);
}


static status
statusTextItem(TextItem ti, Name stat)
{ if ( ti->status != stat )
  { int incdec = (ti->status == NAME_increment ||
		  ti->status == NAME_decrement ||
		  stat == NAME_increment ||
		  stat == NAME_decrement );
    assign(ti, status, stat);

    updateShowCaretTextItem(ti);

    if ( incdec )
      changedDialogItem(ti);		/* ensure redraw */
  }

  succeed;
}


static void
compute_label_text_item(TextItem ti, int *lw, int *lh)
{ if ( ti->show_label == ON )
  { if ( isDefault(ti->label_font) )
      obtainClassVariablesObject(ti);

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
    int al, av, am;
    Int b = getClassVariableValueObject(ti, NAME_border);
    int cwb = text_item_combo_width(ti);
    TextObj vt = ti->value_text;

    obtainClassVariablesObject(ti);
    fontText(vt, ti->value_font);
    borderText(vt, b);
    if ( notDefault(ti->value_width) )
    { Int vw = toInt(valInt(ti->value_width) - cwb);
      marginText(vt, vw, NAME_clip);
    } else
      lengthText(vt, ti->length);
    ComputeGraphical(vt);

    compute_label_text_item(ti, &lw, &lh);
    al = valInt(getAscentFont(ti->label_font));
    av = valInt(getAscentFont(vt->font)) + valInt(vt->border);
    am = max(al, av);
    assign(vt->area, x, toInt(lw));
    assign(vt->area, y, toInt(am-av));
    
    h = max(lh, valInt(vt->area->h));
    w = lw + valInt(vt->area->w) + cwb;

    if ( ti->pen != ZERO )
      h = max(h, am+1+valInt(ti->pen));

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

    ref = answerObject(ClassPoint, ZERO, toInt(ry), EAV);
  }
  
  answer(ref);
}


static status
pasteTextItem(TextItem ti, Int buffer)
{ Bool oldm, newm;

  oldm = getModifiedTextItem(ti);
  TRY( pasteText(ti->value_text, buffer) );
  newm = getModifiedTextItem(ti);
  requestComputeGraphical(ti, DEFAULT);
  if ( oldm != newm && hasSendMethodObject(ti->device, NAME_modifiedItem) )
    send(ti->device, NAME_modifiedItem, ti, newm, EAV);

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
    Any client;
    Any quit;

    Completer = globalObject(NAME_completer, ClassBrowser, EAV);
    client    = newObject(ClassObtain, Completer, NAME_client, EAV);
    quit      = newObject(ClassMessage, client, NAME_keyboardQuit, EAV);

    protectObject(Completer);
    protectObject(Completer->frame);
    attributeObject(Completer, NAME_client, NIL);
    attributeObject(Completer, NAME_prefix, NAME_);
    attributeObject(Completer, NAME_autoHide, ZERO);
    send(Completer, NAME_selectMessage,
	 newObject(ClassMessage, client, NAME_selectedCompletion,
		   newObject(ClassObtain, Arg(1), NAME_key,
			     EAV),
		   EAV),
	 EAV);
    send(Completer, NAME_cancelMessage, quit, EAV);
    send(get(Completer, NAME_tile, EAV), NAME_border, ZERO, EAV);
    send(Completer, NAME_kind, NAME_popup, EAV);
    send(Completer, NAME_create, EAV);
    send(Completer->frame, NAME_border, ZERO, EAV);

    kb = get(Completer, NAME_keyBinding, EAV);
    functionKeyBinding(kb, CtoName("\\C-g"), quit);
    functionKeyBinding(kb, CtoName("\\e"),   quit);
    functionKeyBinding(kb, CtoName("SPC"),   NAME_extendPrefix);
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

    send(c, NAME_clear, EAV);
    send(c, NAME_client, NIL, EAV);
    send(c, NAME_show, OFF, EAV);
    send(c, NAME_transientFor, NIL, EAV);
    if ( text_item_combo_width(di) )
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
  Int ml;
  int lines, maxlines;			/* lines of the browser */
  int bh;				/* height of the browser */

  ComputeGraphical(di);

  if ( isDefault(di->label_width) )
    lw = valInt(get(di, NAME_labelWidth, EAV));
  else
    lw = valInt(di->label_width);
  fw = valInt(di->area->w) - lw;
  fw = max(fw, 50);

  if ( isDefault(searchstring) )
    searchstring = NIL;
  if ( isDefault(autohide) )
    autohide = (notNil(searchstring) ? getSizeCharArray(searchstring) : ZERO);

  send(c, NAME_client, di, EAV);
  send(c, NAME_autoHide, autohide, EAV);
  if ( notNil(matches) )
  { send(c, NAME_clear, EAV);

    for_chain(matches, val,
	      send(c, NAME_append, get(val, NAME_printName, EAV), EAV));
  }

  lines = valInt(getSizeChain(c->list_browser->dict->members));
  if ( (ml = getClassVariableValueObject(item, NAME_comboBoxHeigth)) &&
       isInteger(ml) )
    maxlines = max(1, valInt(ml));
  else
    maxlines = 6;

  if ( lines > maxlines )
    lines = maxlines;			/* class-variable? */
  bh = lines * valInt(getHeightFont(c->list_browser->font));
  bh += 2 * TXT_X_MARGIN + 2;

  send((pos = get(di, NAME_displayPosition, EAV)), NAME_offset,
       toInt(lw), di->area->h, EAV);
  send(c, NAME_transientFor, getFrameGraphical((Graphical) di), EAV);
  send(c->frame, NAME_set, pos->x, pos->y, toInt(fw), toInt(bh), EAV);
  ws_topmost_frame(c->frame, ON);
  send(c, NAME_open, pos, ON, EAV);	/* pos, normalise */
  if ( (sw = getWindowGraphical((Graphical)di)) )
  { grabPointerWindow(sw, ON);
    focusWindow(sw, (Graphical)di, DEFAULT, DEFAULT, NIL);
  }
  send(c, NAME_cancelSearch, EAV);
  if ( isDefault(searchstring) )
  { send(c, NAME_extendPrefix, EAV);
  } else if ( notNil(searchstring) )
  { assign(c->list_browser, search_string,
	   newObject(ClassString, name_procent_s, searchstring, EAV));
    if ( !executeSearchListBrowser(c->list_browser) )
      send(c, NAME_cancelSearch, EAV);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Forward an event to the completer  to   initiate  preview  of the object
below the pointer  as  well  as   `drag-scrolling'.  After  sending  the
left-down, the completer will obtain focus and process further events.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
forwardCompletionEvent(EventObj ev)
{ if ( Completer )
  { ListBrowser lb = Completer->list_browser;
    ScrollBar   sb = lb->scroll_bar;

    if ( sb->status != NAME_inactive )
    { postEvent(ev, (Graphical)lb->scroll_bar , DEFAULT);
      succeed;
    }

    if ( insideEvent(ev, (Graphical)lb->image) &&
	 !insideEvent(ev, (Graphical)sb) ) /* HACK: they can overlap a bit */
    { if ( isAEvent(ev, NAME_msLeftDrag) ||
	   isAEvent(ev, NAME_locMove) )
      { EventObj ev2 = answerObject(ClassEvent, NAME_msLeftDown, EAV);
	PceWindow sw = ev2->window;

	DEBUG(NAME_comboBox,
	      Cprintf("Sending artificial ms_left_down to %s\n", pp(lb)));
	postEvent(ev2, (Graphical) lb, DEFAULT);
	if ( notNil(sw) )
	  assign(sw, focus_button, NIL); /* Hack to keep the focus */
	succeed;
      }
    } else if ( insideEvent(ev, (Graphical)sb) && isDownEvent(ev) )
    { PceWindow sw = ev->window;

      DEBUG(NAME_comboBox, Cprintf("Initiating scrollbar\n"));
      postEvent(ev, (Graphical)lb->scroll_bar , DEFAULT);
      if ( notNil(sw) )
	assign(sw, focus_button, NIL); /* Hack to keep the focus */
      succeed;
    }
  }

  fail;
}


static status
selectCompletionTextItem(TextItem ti, Chain matches,
			 CharArray prefix, CharArray searchstring,
			 Int autohide)
{ Browser c = CompletionBrowser();

  if ( isDefault(prefix) || isNil(prefix) )
    prefix = (CharArray)NAME_;

  send(c, NAME_prefix, prefix, EAV);

  if ( text_item_combo_width(ti) )
    changedDialogItem(ti);

  return selectCompletionDialogItem((DialogItem)ti, matches,
				    searchstring, autohide);
}


static status
enterCompleterTextItem(TextItem ti)
{ if ( completerShownDialogItem(ti) )
  { send(CompletionBrowser(), NAME_enter, EAV);

    quitCompleterDialogItem(ti);
  }

  succeed;
}


static status
selectedCompletionTextItem(TextItem ti, CharArray value, Bool apply)
{ Any c = CompletionBrowser();
  Any prefix = get(c, NAME_prefix, EAV);

  displayedValueTextItem(ti, getAppendCharArray(prefix, value));
  quitCompleterDialogItem(ti);

  if ( apply != OFF )
    send(ti, NAME_apply, ON, EAV);

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

  if ( (split = get(ti, NAME_splitCompletion, prefix, EAV)) )
  { Chain files;

    if ( all == ON )
    { if ( instanceOfObject(split, ClassTuple) )
	assign(((Tuple)split), second, NAME_);
      else
	split = NAME_;
    }

    if ( (files = get(ti, NAME_completions, split, EAV)) &&
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
  { send(c, NAME_extendPrefix, EAV);
  } else
  { CharArray dir, file;
    Chain files;

    if ( completions(ti, ti->value_text->string, OFF, &dir, &file, &files) )
    { Tuple t;
      int dirmode;
      Bool ignore_case = getClassVariableValueObject(ti, NAME_searchIgnoreCase);
      
      if ( !(dirmode = notNil(dir)) )
	dir = (CharArray)NAME_;

      if ( (t=getCompleteNameChain(files, file, DEFAULT, ignore_case)) )
      { int unique = (getSizeChain(t->first) == ONE);
	StringObj path;

	path = answerObject(ClassString, CtoName("%s%s"), dir, t->second, EAV);
	if ( unique && dirmode )
	  send(ti, NAME_indicateDirectory, path, EAV);
	if ( equalCharArray((CharArray) path,
			    (CharArray) ti->value_text->string,
			    ignore_case) )
	{ if ( unique )
	    errorPce(path, NAME_soleCompletion);
	  else
	    send(ti, NAME_selectCompletion,
		 t->first,
		 dirmode ? dir : DEFAULT,
		 file,
		 EAV);
	} else
	  displayedValueTextItem(ti, (CharArray) path);
	doneObject(path);
	doneObject(t);
      } else
	errorPce(file, NAME_completeNoMatch);
    } else if ( isInteger(id) )
      send(ti, NAME_insertSelf, ONE, id, EAV);
  }

  succeed;
}


static status
completeOrNextTextItem(TextItem ti, EventId id)
{ if ( ti->style == NAME_comboBox )
    return send(ti, NAME_complete, id, EAV);
  else
    return send(ti, NAME_next, EAV);
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
       (rval = getForwardReceiverFunction(ti->value_set, ti, base, EAV)) )
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

  if ( isNil(ti->value_set) )
    answer(OFF);
  if ( isDefault(ti->value_set) )
  { if ( (vset=getValueSetType(ti->type, NIL)) )
    { Bool rval = ON;

      if ( vset->size == ONE && getHeadChain(vset) == DEFAULT )
	rval = OFF;

      doneObject(vset);

      answer(rval);
    }
    answer(OFF);
  }

  answer(ON);
}


status
styleTextItem(TextItem ti, Name style)
{ if ( isDefault(style) )
  { if ( get(ti, NAME_hasCompletions, EAV) == ON )
      style = NAME_comboBox;
    else
      style = NAME_normal;
  }

  return assignGraphical(ti, NAME_style, style);
}


static status
typeTextItem(TextItem ti, Type type)
{ assign(ti, type, type);
  send(ti, NAME_style, DEFAULT, EAV);
  succeed;
}


static status
valueSetTextItem(TextItem ti, Chain set)
{ assign(ti, value_set, set);
  send(ti, NAME_style, DEFAULT, EAV);
  succeed;
}


static int
combo_flags(TextItem ti)
{ int flags = 0;

  if ( ti->style == NAME_comboBox )
  { flags |= TEXTFIELD_COMBO;

    if ( completerShownDialogItem(ti) )
      flags |= TEXTFIELD_COMBO_DOWN;
  } else if ( ti->style == NAME_stepper )
  { flags |= TEXTFIELD_STEPPER;
    if ( ti->status == NAME_increment )
      flags |= TEXTFIELD_INCREMENT;
    else if ( ti->status == NAME_decrement )
      flags |= TEXTFIELD_DECREMENT;
  }

  return flags;
}


int
text_item_combo_width(TextItem ti)
{ if ( ti->style == NAME_comboBox )
  { int w = ws_combo_box_width();

    return w >= 0 ? w : 14;
  } else if ( ti->style == NAME_stepper )
  { int w = ws_stepper_width();

    return w >= 0 ? w : (STEPPER_BOX_W+STEPPER_BOX_GAP);
  }

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
		  files, dir, ti->value_text->string, ZERO, EAV);
    }

    fail;
  }
}


		/********************************
		*        EVENT HANDLING		*
		********************************/


static Name
getIncDecTextItem(TextItem ti, EventObj ev)
{ if ( ti->style == NAME_stepper )
  { Int X, Y;
    int x, y;
    int r = valInt(ti->area->w);
    
    get_xy_event(ev, ti, OFF, &X, &Y);
    x = valInt(X);
    y = valInt(Y);
      
    if ( x >= r - text_item_combo_width(ti) && x < r &&
	 y >= 0 && y <= valInt(ti->area->h) )
    { if ( y < valInt(ti->area->h)/2 )
	answer(NAME_increment);
      else
	answer(NAME_decrement);
    }
  }
    
  fail;
}


static status
attachTimerTextItem(TextItem ti)
{ Real delay = getClassVariableValueObject(ti, NAME_repeatDelay);

  if ( delay )
  { Timer t = newObject(ClassTimer, delay,
			newObject(ClassMessage, ti, NAME_repeat, EAV), EAV);
    attributeObject(ti, NAME_Timer, t);
    startTimer(t, NAME_once);
  }

  succeed;
}


static status
detachTimerTextItem(TextItem ti)
{ Timer t;

  if ( (t = getAttributeObject(ti, NAME_Timer)) )
  { freeObject(t);
    deleteAttributeObject(ti, NAME_Timer);
  }
  
  succeed;
}


static status
repeatTextItem(TextItem ti)
{ Timer t;
  Real i = getClassVariableValueObject(ti, NAME_repeatInterval);

  if ( ti->status == NAME_increment ||
       ti->status == NAME_decrement )
    send(ti, ti->status, EAV);

  if ( (t = getAttributeObject(ti, NAME_Timer)) )
  { intervalTimer(t, i);
    statusTimer(t, NAME_once);
  }

  succeed;
}



static Int
getPointedTextItem(TextItem ti, Point pos)
{ return get_pointed_text(ti->value_text,
			  valInt(pos->x)-valInt(ti->value_text->area->x),
			  valInt(pos->y));
}



static status
eventTextItem(TextItem ti, EventObj ev)
{ static Int origin;			/* Multithread dubious! */
  Name dir;

  if ( ti->editable == ON &&
       isAEvent(ev, NAME_msLeft) &&
       (dir=getIncDecTextItem(ti, ev)) )
  { if ( isUpEvent(ev) )
    { send(ti, dir, EAV);
      detachTimerTextItem(ti);
      statusTextItem(ti, NAME_active);
    } else
    { if ( isDownEvent(ev) )
      { send(ti, NAME_keyboardFocus, ON, EAV);
	attachTimerTextItem(ti);
      }

      statusTextItem(ti, dir);
    }

    succeed;
  }

  if ( completerShownDialogItem(ti) )
  { Browser c = CompletionBrowser();
    ListBrowser lb = c->list_browser;

    if ( isAEvent(ev, NAME_keyboard) )
    { KeyBinding kb = (ti->editable == ON ? KeyBindingTextItem()
			 		  : KeyBindingTextItemView());
      Name f = getFunctionKeyBinding(kb, ev);

      if ( f != NAME_complete && f != NAME_keyboardQuit )
      { postEvent(ev, (Graphical)lb, DEFAULT);

	f = getFunctionKeyBinding(lb->key_binding, ev);
	if ( f == NAME_backwardDeleteChar )
	{ Int autohide = getAttributeObject(lb->device, NAME_autoHide);
	
	  if ( autohide != ZERO )
	  { StringObj ss = lb->search_string;
	    if ( isNil(ss) || valInt(autohide) > valInt(getSizeCharArray(ss)) )
	    quitCompleterDialogItem(ti);
	  }
	}

	succeed;
      }

      return send(ti, NAME_typed, ev, EAV);
    }	 
    
    if ( forwardCompletionEvent(ev) )
      succeed;

    if ( isAEvent(ev, NAME_msLeftDown) &&
	 !insideEvent(ev, (Graphical)lb) )
      return quitCompleterDialogItem(ti);

    return postEvent(ev, (Graphical)c, DEFAULT);
  }

  if ( eventGraphical(ti, ev) )		/* recognisers */
    succeed;

  if ( ti->status != NAME_inactive )
  { if ( isAEvent(ev, NAME_keyboard) )
    { return send(ti, NAME_typed, ev, EAV);
    } else
    { if ( ti->editable == ON && isAEvent(ev, NAME_msMiddleUp) )
	return pasteTextItem(ti, DEFAULT);
    }
  }

  if ( eventDialogItem(ti, ev) )	/* TBD: does recognisers again */
    succeed;

  if ( ti->active == OFF )
    fail;

  if ( isAEvent(ev, NAME_msLeftDown) )
  { if ( 1 /* WantsKeyboardFocusTextItem(ti) */ )
    { int cbw;
      int wasactive = (ti->status == NAME_active);

      send(ti, NAME_keyboardFocus, ON, EAV);
      
      if ( (cbw = text_item_combo_width(ti)) > 0 &&
	   ti->editable == ON )
      { Int X, Y;
	int x, y;

	get_xy_event(ev, ti, ON, &X, &Y);
	x = valInt(X); y = valInt(Y);
	if ( y >= 0 &&
	     y <= valInt(ti->area->h) &&
	     x <= valInt(ti->area->w) &&
	     x >= valInt(ti->area->w) - cbw )
	{ Bool val = (completerShownDialogItem(ti) ? OFF : ON);

	  send(ti, NAME_showComboBox, val, EAV);
	  succeed;
	}
      }

      if ( getMulticlickEvent(ev) == NAME_double )
      { send(ti, NAME_selectAll, EAV);
	succeed;
      } else
      { origin = getPointedTextItem(ti, getPositionEvent(ev, ti));
	if ( origin && wasactive )
	{ send(ti->value_text, NAME_selection, NIL, EAV);
	  send(ti, NAME_caret, origin, EAV);

	  succeed;
	}
      }
    } else
      return alertGraphical((Graphical) ti);
  } else if ( isAEvent(ev, NAME_msLeftDrag) )
  { Int here = getPointedTextItem(ti, getPositionEvent(ev, ti));
    
    if ( here )
    { send(ti->value_text, NAME_selection, origin, here, EAV);
      send(ti, NAME_caret, here, EAV);
    }
  } else if ( isAEvent(ev, NAME_msLeftUp) )
  { if ( notNil(ti->selection) &&
	 getClassVariableValueObject(ti, NAME_autoCopy) == ON )
      send(ti, NAME_copy, EAV);
  } else if ( isAEvent(ev, NAME_focus) )
  { if ( isAEvent(ev, NAME_obtainKeyboardFocus) )
    { send(ti, NAME_status, NAME_active, EAV);
    } else if ( isAEvent(ev, NAME_releaseKeyboardFocus) )
    { send(ti, NAME_status, NAME_inactive, EAV);
    }

    return updateShowCaretTextItem(ti);
  }

  fail;
}


static status
keyboardQuitTextItem(TextItem ti)
{ quitCompleterDialogItem(ti);
  send(ti, NAME_alert, EAV);

  succeed;
}


static status
executeTextItem(TextItem ti)
{ Any av[1];

  av[0] = ON;

  return qadSendv(ti, NAME_apply, 1, av);
}


static status
keyTextItem(TextItem ti, Name key)
{ if ( ti->accelerator == key && WantsKeyboardFocusTextItem(ti) )
    return send(ti, NAME_keyboardFocus, ON, EAV);

  fail;
}


status
typedTextItem(TextItem ti, EventId id)
{ return typedKeyBinding(ti->editable == ON ? KeyBindingTextItem()
			 		    : KeyBindingTextItemView(),
			 id, (Graphical) ti);
}


static status
nextTextItem(TextItem ti)
{ return send(ti->device, NAME_advance, ti, EAV);
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
	 send(dev, NAME_typed, id, ON, EAV)) &&
       !isFreedObj(ti) )
  { int modified = (getModifiedTextItem(ti) == ON);
    Any av[1];

    av[0] = OFF;
    if ( qadSendv(ti, NAME_apply, 1, av) && !isFreedObj(ti) )
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
			(CharArray) ti->value_text->string, OFF) ? OFF : ON);
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
  return send(ti, NAME_selection, val, EAV);
}


status
applyTextItem(TextItem ti, Bool always)
{ Any val;

  if ( instanceOfObject(ti->message, ClassCode) &&
       (always == ON || getModifiedTextItem(ti) == ON) &&
       (val = getv(ti, NAME_selection, 0, NULL)) )
    return forwardReceiverCode(ti->message, ti, val, EAV);

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
       !send(ti->type, NAME_validate, ti->selection, EAV) )
  { Any value;
    Chain set;
    int ok = 0;

    if ( (value = get(ti->type, NAME_check, ti->value_text->string, EAV)) )
    { valueString(ti->print_name, ti->value_text->string);
      assign(ti, selection, value);
      ok++;
    } else if ( (set = getCompletionsTextItem(ti, ti->value_text->string)) )
    { Cell cell;

      for_cell(cell, set)
      { CharArray pn;

	if ( (pn = getv(ti, NAME_printNameOfValue, 1, &cell->value)) &&
	     equalCharArray(ti->value_text->string, pn, OFF) )
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
/*valueString(ti->print_name, (CharArray) NAME_); must indicate as modified!*/
  if ( modified && hasSendMethodObject(ti->device, NAME_modifiedItem) )
    send(ti->device, NAME_modifiedItem, ti, ON, EAV);
  
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
		       (CharArray)ti->print_name, OFF) )
  { stringText(ti->value_text, (CharArray) ti->print_name);
    requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


status
displayedValueTextItem(TextItem ti, CharArray txt)
{ if ( !equalCharArray(ti->value_text->string, txt, OFF) )
  { Bool oldm, newm;

    oldm = getModifiedTextItem(ti);
    TRY(stringText(ti->value_text, txt));
    newm = getModifiedTextItem(ti);
    requestComputeGraphical(ti, DEFAULT);
    if ( oldm != newm && hasSendMethodObject(ti->device, NAME_modifiedItem) )
      send(ti->device, NAME_modifiedItem, ti, newm, EAV);
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
      send(ti->device, NAME_advance, ti, EAV);
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
{ if ( ti->value_font != font )
  { assign(ti, value_font, font);
    requestComputeGraphical(ti, DEFAULT);
  }

  succeed;
}


static Int
getLabelWidthTextItem(TextItem ti)
{ int lw, lh;

  obtainClassVariablesObject(ti);
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


status
valueWidthTextItem(TextItem ti, Int val)
{ assign(ti, value_width, val);

  if ( notDefault(val) && instanceOfObject(ti->value_font, ClassFont) )
  { Int ex = getExFont(ti->value_font);
    int chars = (valInt(val) - text_item_combo_width(ti)) / valInt(ex);

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
catchAllTextItem(TextItem ti, Name sel, int argc, Any *argv)
{ if ( qadSendv(ti->value_text, NAME_hasSendMethod, 1, (Any*)&sel) )
  { Bool old = getModifiedTextItem(ti);
    status rval = vm_send(ti->value_text, sel, NULL, argc, argv);

    if ( rval )
    { Bool new;
    
      requestComputeGraphical(ti, DEFAULT);
      if ( (new = getModifiedTextItem(ti)) != old &&
	   hasSendMethodObject(ti->device, NAME_modifiedItem) )
	send(ti->device, NAME_modifiedItem, ti, new, EAV);
    }

    return rval;
  }

  return errorPce(ti, NAME_noBehaviour, CtoName("->"), sel);
}


static Any
getCatchAllTextItem(TextItem t, Name sel, int argc, Any *argv)
{ if ( qadSendv(t->value_text, NAME_hasGetMethod, 1, (Any*)&sel) )
  { assign(PCE, last_error, NIL);

    answer(vm_get(t->value_text, sel, NULL, argc, argv));
  }

  errorPce(t, NAME_noBehaviour, CtoName("<-"), sel);
  fail;
} 

static status
hasSendMethodTextItem(TextItem t, Name sel)
{ if ( hasSendMethodObject(t, sel) ||
       hasSendMethodObject(t->value_text, sel) )
    succeed;

  fail;
}


static status
hasGetMethodTextItem(TextItem t, Name sel)
{ if ( hasGetMethodObject(t, sel) ||
       hasGetMethodObject(t->value_text, sel) )
    succeed;

  fail;
}


static Tuple
getSendMethodTextItem(TextItem ti, Name sel)
{ Tuple t;

  if ( (t = getSendMethodObject(ti, sel)) )
    answer(t);

  answer(getSendMethodObject(ti->value_text, sel));
}


static Tuple
getGetMethodTextItem(TextItem ti, Name sel)
{ Tuple t;

  if ( (t = getGetMethodObject(ti, sel)) )
    answer(t);

  answer(getGetMethodObject(ti->value_text, sel));
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
static char *T_selectCompletion[] =
        { "value_set=chain", "prefix=[char_array]*",
	  "search=[char_array]*", "auto_hide=[0..]" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_selectedCompletion[] =
	{ "item=char_array", "apply=[bool]" };
static char *T_catchAll[] =
        { "selector=name", "argument=unchecked ..." };


/* Instance Variables */

static vardecl var_textItem[] =
{ SV(NAME_status, "{inactive,active,preview,execute,increment,decrement}",
     IV_GET|IV_STORE|IV_REDEFINE,
     statusTextItem,
     NAME_event, "Status for event-processing"),
  SV(NAME_selection, "any", IV_NONE|IV_STORE, selectionTextItem,
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
  IV(NAME_valueText, "text", IV_GET,
     NAME_visualisation, "Graphical text object for selection"),
  SV(NAME_editable, "bool", IV_GET|IV_STORE, editableTextItem,
     NAME_event, "TextItem may be edited"),
  IV(NAME_valueWidth, "[int]", IV_NONE,
     NAME_layout, "Width of the value-part in pixels"),
  IV(NAME_horStretch, "0..100", IV_BOTH,
     NAME_layout, "Horizontal stretchability"),
  IV(NAME_style, "{normal,combo_box,stepper}", IV_GET,
     NAME_appearance, "Show plain/combo-box/stepper")
};

/* Send Methods */

static senddecl send_textItem[] =
{ SM(NAME_unlink, 0, NULL, unlinkTextItem,
     DEFAULT, "Detach repeat timer"),
  SM(NAME_compute, 0, NULL, computeTextItem,
     DEFAULT, "Compute desired size"),
  SM(NAME_geometry, 4, T_geometry, geometryTextItem,
     DEFAULT, "Resize the image"),
  SM(NAME_initialise, 3, T_initialise, initialiseTextItem,
     DEFAULT, "Create from label, selection and message"),
  SM(NAME_activate, 1, "bool", activateTextItem,
     NAME_event, "Set active status of caret"),
  SM(NAME_selectAll, 0, NULL, selectAllTextItem,
     NAME_selection, "Select all text (double-click)"),
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
  SM(NAME_style, 1, "[{normal,combo_box,stepper}]", styleTextItem,
     DEFAULT, "Set style or termine default style"),
  SM(NAME_complete, 1, "[event_id]", completeTextItem,
     NAME_complete, "Complete current value"),
  SM(NAME_completeOrNext, 1, "[event_id]", completeOrNextTextItem,
     NAME_complete, "->complete or advance to next item"),
  SM(NAME_indicateDirectory, 1, "text=string", indicateDirectoryTextItem,
     NAME_complete, "Indicate current value is a `directory'"),
  SM(NAME_keyboardQuit, 0, NULL, keyboardQuitTextItem,
     NAME_complete, "Remove completer and ->alert"),
  SM(NAME_selectCompletion, 4, T_selectCompletion, selectCompletionTextItem,
     NAME_complete, "Select candidate expansion using browser"),
  SM(NAME_selectedCompletion, 2, T_selectedCompletion,
     selectedCompletionTextItem,
     NAME_complete, "Handle selection from browser"),
  SM(NAME_showComboBox, 1, "bool", showComboBoxTextItem,
     NAME_complete, "Show the combo-box browser"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, WantsKeyboardFocusTextItem,
     NAME_event, "Test if ready to accept input"),
  SM(NAME_event, 1, "event", eventTextItem,
     NAME_event, "Process user event"),
  SM(NAME_typed, 1, "event|event_id", typedTextItem,
     NAME_event, "Process event with given id"),
  SM(NAME_key, 1, "key=name", keyTextItem,
     NAME_accelerator, "Request keyboard if accelerator is typed"),
  SM(NAME_repeat, 0, NULL, repeatTextItem,
     DEFAULT, "Repeat in/decrement"),
  SM(NAME_increment, 0, NULL, failObject,
     NAME_selection, "(virtual) Increment the selection"),
  SM(NAME_decrement, 0, NULL, failObject,
     NAME_selection, "(virtual) Decrement the selection"),
  SM(NAME_labelWidth, 1, "[int]", labelWidthTextItem,
     NAME_layout, "Width of label in pixels"),
  SM(NAME_clear, 0, NULL, clearTextItem,
     NAME_selection, "Clear entry field"),
  SM(NAME_paste, 1, "[int]", pasteTextItem,
     NAME_selection, "Paste value of cut-buffer"),
  SM(NAME_displayedValue, 1, "char_array", displayedValueTextItem,
     NAME_textual, "Visible (typed) textual value"),
  SM(NAME_catchAll, 2, T_catchAll, catchAllTextItem,
     NAME_delegate, "Delegate to <-value_text"),
  SM(NAME_hasSendMethod, 1, "name", hasSendMethodTextItem,
     DEFAULT, "Test if text or <-value_text defines method"),
  SM(NAME_hasGetMethod, 1, "name", hasGetMethodTextItem,
     DEFAULT, "Test if text or <-value_text defines method")
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
  GM(NAME_pointed, 1, "index=int", "at=point", getPointedTextItem,
     NAME_event, "Convert position to character index"),
  GM(NAME_displayedValue, 0, "char_array", NULL, getDisplayedValueTextItem,
     NAME_textual, "Visible (typed) textual value"),
  GM(NAME_printNameOfValue, 1, "char_array", "any",
     getPrintNameOfValueTextItem,
     NAME_textual, "Determine printable representation"),
  GM(NAME_catchAll, 2, "unchecked", T_catchAll, getCatchAllTextItem,
     NAME_delegate, "Delegate to <-value_text"),
  GM(NAME_sendMethod, 1, "tuple", "name", getSendMethodTextItem,
     NAME_delegate, "Get send_method from self or <-value_text"),
  GM(NAME_getMethod, 1, "tuple", "name", getGetMethodTextItem,
     NAME_delegate, "Get get_method from self or <-value_text")
};

/* Resources */

static classvardecl rc_textItem[] =
{ RC(NAME_border, "0..", "4",
     "Border around <-value_text"),
  RC(NAME_length, "int", "25",
     "Width of area for selection (chars)"),
  RC(NAME_pen, "int", "1",
     "Thickness of line below selection"),
  RC(NAME_autoCopy, "bool", UXWIN("@on", "@off"),
     "Automatically copy selected text to the clipboard"),
  RC(NAME_autoSelect, "bool", "@off",
     "Automatically select all text when ->activate'd"),
  RC(NAME_searchIgnoreCase, "bool", "@on",
     "@on: ignore case for completion"),
  RC(NAME_repeatDelay, "real", "0.35",
     "Time to wait until start of repeat"),
  RC(NAME_repeatInterval, "real", "0.06",
     "Interval between repeats"),
  RC(NAME_look, RC_REFINE, UXWIN("gtk", "win"), NULL),
  RC(NAME_elevation, RC_REFINE,
     UXWIN("when(@colour_display, 1, @nil)", "@_txt_height"), NULL),
  RC(NAME_comboBoxHeigth, "1..", "6",
     "Maximum height of the combo-box shown for completions")
};

/* Class Declaration */

static Name textItem_termnames[] = { NAME_label, NAME_value, NAME_message };

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

