/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <rclass.h>			/* provides RESOURCE_CLASS */
					/* generated from Makefile */

static status	backgroundDisplay(DisplayObj, Colour);
static status	foregroundDisplay(DisplayObj d, Colour c);
static void	attach_font_families(Class class);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a display.  The display is not yet opened.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseDisplay(DisplayObj d, Name address, Name resource_class)
{ DisplayManager dm = TheDisplayManager();

  if ( isDefault(resource_class) )
    resource_class = CtoName(RESOURCE_CLASS);

  assign(d, size,        	NIL);
  assign(d, address,     	address);
  assign(d, resource_class,    	resource_class);
  assign(d, font_table,		newObject(ClassHashTable, 0));
  assign(d, frames,	 	newObject(ClassChain, 0));
  assign(d, inspect_handlers,	newObject(ClassChain, 0));
  assign(d, foreground,	 	DEFAULT);
  assign(d, background,	 	DEFAULT);
  assign(d, quick_and_dirty,	DEFAULT);
  assign(d, cache,	 	NIL);
  assign(d, window_manager,	DEFAULT);
  assign(d, colour_map,		DEFAULT);
  assign(d, display_manager,	dm);

  ws_init_display(d);
  appendDisplayManager(dm, d);
  protectObject(d);

  succeed;
}


static DisplayObj
getConvertDisplay(Class class, Any obj)
{ Name address;
  DisplayObj d;

  if ( (d = getMemberDisplayManager(TheDisplayManager(), obj)) )
    answer(d);

  if ( isDefault(obj) )
    answer(CurrentDisplay(obj));

  if ( instanceOfObject(obj, ClassVisual) )
    answer(get(obj, NAME_display, 0));

  if ( (address = checkType(obj, TypeName, class)) &&
       ws_legal_display_name(strName(address)) )
    answer(answerObject(ClassDisplay, address, 0));

  fail;
}


static status
attachCacheDisplay(DisplayObj d)
{ Size sz = getResourceValueObject(d, NAME_graphicsCache);

  if ( isDefault(sz) )
    sz = getSizeDisplay(d);
    
  send(d, NAME_cache, newObject(ClassImage, DEFAULT, sz->w, sz->h,
				NAME_pixmap, 0), 0);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open a display.  If necessary, the X toolkit is initialised first and
a context for the application is created.

As PCE  normally manages a  collection of main  windows an application
shell  widget is created to  serve as root for  all  the other (popup)
shells.  This widget is never realised (page 35 of Xt manual).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
openDisplay(DisplayObj d)
{ Code code;

  if ( ws_opened_display(d) )
    succeed;

  DEBUG(NAME_display, Cprintf("Opening display %s\n", pp(d)));

  ws_open_display(d);			/* generate exception on failure */
  if ( isDefault(d->foreground) )
    foregroundDisplay(d, getResourceValueObject(d, NAME_foreground));
  if ( isDefault(d->background) )
    backgroundDisplay(d, getResourceValueObject(d, NAME_background));
  ws_init_graphics_display(d);

  assign(d, quick_and_dirty, getResourceValueObject(d, NAME_quickAndDirty));
  BLACK_COLOUR = newObject(ClassColour, NAME_black, 0);
  WHITE_COLOUR = newObject(ClassColour, NAME_white, 0);
  attachCacheDisplay(d);

  if ( (code = getResourceValueObject(d, NAME_initialise)) &&
       instanceOfObject(code, ClassCode) )
    forwardReceiverCodev(code, d, 0, NULL);

  succeed;
}


Bool
getOpenDisplay(Any d)
{ answer(ws_opened_display(d) ? OFF : ON);
}


static status
foregroundDisplay(DisplayObj d, Colour c)
{ assign(d, foreground, c);
  ws_foreground_display(d, c);
  
  succeed;
}


static status
backgroundDisplay(DisplayObj d, Colour c)
{ assign(d, background, c);
  ws_background_display(d, c);
  
  succeed;
}


static status
colourMapDisplay(DisplayObj d, ColourMap cm)
{ assign(d, colour_map, cm);

  succeed;
}


status
drawInDisplay(DisplayObj d, Graphical gr, Point pos, Bool invert, Bool subtoo)
{ Int oldx, oldy;
  Device dev;

  if ( isDefault(invert) )
    invert = OFF;
  if ( isDefault(subtoo) )
    subtoo = OFF;

  if ( notDefault(pos) )
  { oldx = gr->area->x;
    oldy = gr->area->y;
    dev = gr->device;
    gr->device = NIL;
    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);
  } else
  { oldx = oldy = (Int) DEFAULT;
    dev = NIL;				/* keep compiler happy */
  }

  ComputeGraphical(gr);
  openDisplay(d);

  ws_draw_in_display(d, gr, invert, subtoo);

  if ( notDefault(oldx) )
  { setGraphical(gr, oldx, oldy, DEFAULT, DEFAULT);
    gr->device = dev;
  }

  succeed;
}


static Image
getImageDisplay(DisplayObj d, Area a)
{ int x, y, w, h;

  openDisplay(d);
  if ( isDefault(a) )
  { Size sz = getSizeDisplay(d);

    x = y = 0;
    w = valInt(sz->w);
    h = valInt(sz->h);
  } else
  { x = valInt(a->x);
    y = valInt(a->y);
    w = valInt(a->w);
    h = valInt(a->h);
  }

  return ws_grab_image_display(d, x, y, w, h);
}


status
grabServerDisplay(DisplayObj d, Bool val)
{ if ( ws_opened_display(d) )
  { if ( val == ON )
      ws_grab_server(d);
    else
      ws_ungrab_server(d);
  }

  succeed;
}


static Int
getConnectionFdDisplay(DisplayObj d)
{ if ( ws_opened_display(d) )
    answer(ws_display_connection_number(d));
    
  fail;
}


static status
eventQueuedDisplay(DisplayObj d)
{ if ( ws_opened_display(d) )
  { RedrawDisplayManager(d->display_manager);

    return ws_events_queued_display(d);
  }

  fail;
}


status
dispatchDisplay(DisplayObj d)
{ answer(dispatchDisplayManager(d->display_manager, DEFAULT, DEFAULT));
}


status
flushDisplay(DisplayObj d)
{ if ( ws_opened_display(d) )
  { RedrawDisplayManager(d->display_manager);
    ws_flush_display(d);
  }

  succeed;
}


status
synchroniseDisplay(DisplayObj d)
{ if ( ws_opened_display(d) )
  { RedrawDisplayManager(d->display_manager);

    ws_synchronise_display(d);
  }

  succeed;
}


static status
screenSaverDisplay(DisplayObj d, Bool val)
{ openDisplay(d);

  if ( val == ON )
    ws_activate_screen_saver(d);
  else
    ws_deactivate_screen_saver(d);

  succeed;
}


status
bellDisplay(DisplayObj d, Int vol)
{ openDisplay(d);

  if ( isDefault(vol) )
    vol = (Int) getResourceValueObject(d, NAME_volume);

  ws_bell_display(d, valInt(vol));

  succeed;
}


Size
getSizeDisplay(DisplayObj d)
{ int w=0, h=0;

  if ( notNil(d->size) )
    answer(d->size);

  openDisplay(d);
  ws_get_size_display(d, &w, &h);
  assign(d, size, newObject(ClassSize, toInt(w), toInt(h), 0));

  answer(d->size);
}


Int
getWidthDisplay(DisplayObj d)
{ answer(getSizeDisplay(d)->w);
}


Int
getHeightDisplay(DisplayObj d)
{ answer(getSizeDisplay(d)->h);
}


static Area
getBoundingBoxDisplay(DisplayObj d)
{ Size s = getSizeDisplay(d);

  answer( answerObject(ClassArea, ZERO, ZERO, s->w, s->h, 0) );
}


static Int
getDepthDisplay(DisplayObj d)
{ TRY(openDisplay(d));

  answer(toInt(ws_depth_display(d)));
}

static Name
getVisualTypeDisplay(DisplayObj d)
{ TRY(openDisplay(d));

  answer(ws_get_visual_type_display(d));
}

		/********************************
		*          CUT BUFFERS		*
		********************************/

static status
cutBufferDisplay(DisplayObj d, Int n, CharArray str)
{ String s = &str->data;
  TRY(openDisplay(d));

  if ( isDefault(n) )
    n = ZERO;

  return ws_set_cutbuffer(d, valInt(n), s);
}


static StringObj
getCutBufferDisplay(DisplayObj d, Int n)
{ TRY(openDisplay(d));

  if ( isDefault(n) )
    n = ZERO;

  return ws_get_cutbuffer(d, valInt(n));

}

		 /*******************************
		 *	SELECTION INTERFACE	*
		 *******************************/

static Real
getSelectionTimeoutDisplay(DisplayObj d)
{ ulong time = ws_get_selection_timeout();

  answer(CtoReal((float)time/1000.0));
}


static status
selectionTimeoutDisplay(DisplayObj d, Real time)
{ ws_set_selection_timeout((ulong)(valReal(time) * 1000.0));

  succeed;
}


static Any
getSelectionDisplay(DisplayObj d, Name which, Name target, Type type)
{ Any sel;

  TRY(openDisplay(d));

  if ( isDefault(which) )  which  = NAME_primary;
  if ( isDefault(target) ) target = NAME_text;
  if ( isDefault(type) )   type   = nameToType(NAME_string);
  
  if ( (sel = ws_get_selection(d, which, target)) )
    answer(checkType(sel, type, NIL));

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The owner of a selection is related using a hyper-object to the display.
This will inform the display if the selection onwner is deleted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Any
getSelectionOwnerDisplay(DisplayObj d, Name which)
{ if ( isDefault(which) )
    which = NAME_primary;

  answer(getHyperedObject(d,
			  getAppendName(which, NAME_selectionOwner),
			  DEFAULT));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD: * multiple hypers for the various selection-types.
     * proper call-back if the owner is unlinked.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


status
looseSelectionDisplay(DisplayObj d, Name which)
{ Hyper h;
  Code msg;
  Name hypername = getAppendName(which, NAME_selectionOwner);

  if ( (h = getFindHyperObject(d, hypername, DEFAULT)) &&
       (msg = getAttributeObject(h, NAME_looseMessage)) &&
       (msg = checkType(msg, TypeCode, NIL)) )
    forwardReceiverCode(msg, h->to, which, 0);

  freeHypersObject(d, hypername, DEFAULT);

  succeed;
}


static status
selectionOwnerDisplay(DisplayObj d, Any owner, Name selection,
		      Function convert, Code loose, Name type)
{ TRY(openDisplay(d));

  if ( isDefault(selection) )
    selection = NAME_primary;
  if ( isDefault(type) )
    type = NAME_text;

  if ( isNil(owner) )
  { Any old = getSelectionOwnerDisplay(d, selection);

    if ( old )
    { looseSelectionDisplay(d, selection);
      ws_disown_selection(d, selection);
    }
  } else
  { Any old = getSelectionOwnerDisplay(d, selection);
    Hyper h = NIL;
    Name hypername = getAppendName(selection, NAME_selectionOwner);

    if ( old && old != owner )
      looseSelectionDisplay(d, selection);

    if ( old != owner )
      h = newObject(ClassHyper, d, owner, hypername, 0);
    else
      h = getFindHyperObject(d, hypername, DEFAULT);

    attributeObject(h, NAME_convertFunction,
		     newObject(ClassQuoteFunction, convert, 0));
    attributeObject(h, NAME_looseMessage, loose);
    attributeObject(h, NAME_type, type);

#ifndef __WINDOWS__
    if ( !old )
#endif
    { if ( !ws_own_selection(d, selection, type) )
      { freeHypersObject(d, hypername, DEFAULT);
	return errorPce(owner, NAME_cannotBecomeSelectionOwner, selection);
      }
    }
  }

  succeed;
}

		 /*******************************
		 *  SIMPLE SELECTION INTERFACE	*
		 *******************************/

static status
selectionDisplay(DisplayObj d, Name which, StringObj data)
{ StringObj s2 = get(data, NAME_copy, 0);

  if ( s2 )
  { lockObject(s2, ON);

    return selectionOwnerDisplay(d,
				 s2, which,
				 newObject(ClassObtain,
					   RECEIVER, NAME_self, 0),
				 newObject(ClassMessage,
					   RECEIVER, NAME_free, 0),
				 NAME_text);
  }

  fail;
}


static status
copyDisplay(DisplayObj d, StringObj data)
{ int rval = (send(d, NAME_cutBuffer, ZERO, data, 0) |
	      send(d, NAME_selection, NAME_primary, data, 0) |
	      send(d, NAME_selection, NAME_clipboard, data, 0));


  return rval ? SUCCEED : FAIL;
}


static StringObj
getPasteDisplay(DisplayObj d)
{ StringObj s;
  status rval;

  catchErrorPce(PCE, NAME_getSelection);
  rval = ((s=get(d, NAME_selection, 0)) ||
	  (s=get(d, NAME_selection, DEFAULT, NAME_string, 0)) ||
	  (s=get(d, NAME_cutBuffer, ZERO, 0)));
  catchPopPce(PCE);

  if ( rval )
    answer(s);

  fail;
}


		/********************************
		*  WINDOW_MANAGER/LOOK-AND-FEEL	*
		********************************/

static Name
getWindowManagerDisplay(DisplayObj d)
{ Name wm;

  if ( notDefault(d->window_manager) )
    answer(d->window_manager);

  if ( (wm = getResourceValueObject(d, NAME_windowManager)) &&
       notDefault(wm) )
  { assign(d, window_manager, wm);
    answer(d->window_manager);
  }

  if ( (wm = ws_window_manager(d)) )
    assign(d, window_manager, wm);

  answer(d->window_manager);
}


		/********************************
		*        CONFIRM/INFORM		*
		********************************/

static status
create_confirmer(DisplayObj d)
{ Any p, m, h;

  if ( getAttributeObject(d, NAME_confirmer) )
    succeed;

  TRY( p = newObject(ClassWindow, DEFAULT, DEFAULT, d, 0) );
  TRY( m = newObject(ClassText, CtoName(""), NAME_center, 0) );
  TRY( h = newObject(ClassText, CtoName(""), NAME_center, 0) );

  send(m, NAME_font, getResourceValueObject(d, NAME_labelFont), 0);
  send(h, NAME_font, getResourceValueObject(d, NAME_valueFont), 0);
  send(p, NAME_display, m, 0);
  send(p, NAME_display, h, 0);
  send(p, NAME_kind, NAME_popup, 0);
  send(p, NAME_cursor, newObject(ClassCursor, NAME_mouse, 0), 0);
  send(p, NAME_border, toInt(3), 0);
  send(p, NAME_pen, toInt(3), 0);
  send(p, NAME_create, 0);
  send(get(p, NAME_frame, 0), NAME_border, ONE, 0);
  
  send(p, NAME_recogniser,
          newObject(ClassHandler, NAME_button,
		    newObject(ClassMessage,
			      d, NAME_ConfirmPressed, Arg(1), 0),
		    0),
       0);

  attributeObject(d, NAME_SeenDown, OFF);
  attributeObject(d, NAME_confirmer, p);
  attributeObject(p, NAME_helpText, h);
  attributeObject(p, NAME_messageText, m);
  
  succeed;
}


static status
ConfirmPressedDisplay(DisplayObj d, EventObj ev)
{ if ( isDownEvent(ev) )
    send(d, NAME_SeenDown, ON, 0);
  else if ( isUpEvent(ev) )
  { if ( get(d, NAME_SeenDown, 0) == ON )
    { Name code = getButtonEvent(ev);

      send(get(d, NAME_confirmer, 0), NAME_return, code, 0);
    } else
    { send(get(d, NAME_confirmer, 0), NAME_grabPointer, OFF, 0); /* HACK */
      send(get(d, NAME_confirmer, 0), NAME_grabPointer, ON, 0);
    }
  }

  succeed;
}


static Name
display_help(DisplayObj d, StringObj hlp, Name msg)
{ Any p;
  TextObj hlp_text, msg_text;
  int fx, fy, fw, fh, tx, ty;
  Name rval;

  create_confirmer(d);
  TRY( p        = getAttributeObject(d, NAME_confirmer) );
  TRY( hlp_text = getAttributeObject(p, NAME_helpText));
  TRY( msg_text = getAttributeObject(p, NAME_messageText));
  
  send(hlp_text, NAME_string, hlp, 0);
  send(msg_text, NAME_string, msg, 0);
  send(p, NAME_compute, 0);

  fw = max(valInt(hlp_text->area->w), valInt(msg_text->area->w)) + 40;
  fh = valInt(hlp_text->area->h) + valInt(msg_text->area->h) + 50;
  getSizeDisplay(d);			/* initialise size argument */
  fx = (valInt(d->size->w) - fw) / 2;
  fy = (valInt(d->size->h) - fh) / 2;

  tx = (fw - 12 - valInt(hlp_text->area->w)) / 2;
  send(hlp_text, NAME_set, toInt(tx), toInt(20), DEFAULT, DEFAULT, 0);
  tx = (fw - 12 - valInt(msg_text->area->w)) / 2;
  ty = valInt(hlp_text->area->h) + 30;
  send(msg_text, NAME_set, toInt(tx), toInt(ty), DEFAULT, DEFAULT, 0);

  send(get(p, NAME_frame, 0), NAME_set, toInt(fx), toInt(fy),
       					toInt(fw), toInt(fh), 0);

  send(d, NAME_SeenDown, OFF, 0);
  send(p, NAME_show, ON, 0);
  send(p, NAME_grabPointer, ON, 0);
  rval = get(p, NAME_confirm, DEFAULT, ON, 0);
  send(p, NAME_grabPointer, OFF, 0);
  send(p, NAME_show, OFF, 0);

  return rval;
}


status
confirmDisplay(DisplayObj d, CharArray fmt, int argc, Any *argv)
{ StringObj str;
  ArgVector(av, argc+1);
  int i;
  Name button;

  av[0] = (Any) fmt;
  for(i=0; i<argc; i++)
    av[i+1] = argv[i];

  TRY(str = answerObjectv(ClassString, argc+1, av));

  switch( ws_message_box(str, MBX_CONFIRM) )
  { case MBX_OK:
      succeed;
    case MBX_CANCEL:
      fail;
    default:
    { Name msg;

      msg = CtoName("Press LEFT button to confirm, RIGHT button to cancel");
      TRY(button = display_help(d, str, msg));
      doneObject(str);

      if ( button == NAME_left )
	succeed;
    }
  }

  fail;
}


status
informDisplay(DisplayObj d, CharArray fmt, int argc, Any *argv)
{ StringObj str;
  ArgVector(av, argc+1);
  int i;
  Name button;

  av[0] = (Any) fmt;
  for(i=0; i<argc; i++)
    av[i+1] = argv[i];

  TRY(str = answerObjectv(ClassString, argc+1, av));

  switch( ws_message_box(str, MBX_INFORM) )
  { case MBX_NOTHANDLED:
    { Name msg;

      msg = CtoName("Press any button to remove message");
      TRY(button = display_help(d, str, msg));
      doneObject(str);
    }
  }

  succeed;
}


static status
reportDisplay(DisplayObj d, Name kind, CharArray fmt, int argc, Any *argv)
{ if ( kind == NAME_error || kind == NAME_inform )
  { ArgVector(av, argc+1);
    StringObj str;

    av[0] = isDefault(fmt) ? (CharArray) CtoName("") : fmt;
    copyArgs(argc, argv, &av[1]);
    TRY(str = answerObjectv(ClassString, argc+1, av));
    if ( kind == NAME_error )
      alertReporteeVisual(d);

    switch( ws_message_box(str, MBX_ERROR) )
    { case MBX_NOTHANDLED:
      { Name msg, button;

	msg = CtoName("Press any button to remove message");
	TRY(button = display_help(d, str, msg));
	doneObject(str);
      }
    }
  } else if ( kind == NAME_warning )
    alertReporteeVisual(d);

  succeed;
}


status
busyCursorDisplay(DisplayObj d, CursorObj c, Bool block_events)
{ if ( instanceOfObject(d, ClassDisplay) )
  { Cell cell;

    for_cell(cell, d->frames)
      busyCursorFrame(cell->value, c, block_events);

    if ( notNil(c) )
      flushDisplay(d);
  }

  succeed;
}


		/********************************
		*          DEBUGGING		*
		********************************/

static status
inspectHandlerDisplay(DisplayObj d, Handler h)
{ return addChain(d->inspect_handlers, h);
}


status
inspectDisplay(DisplayObj d, Graphical gr, EventObj ev)
{ Cell cell;

  for_cell(cell, d->inspect_handlers)
  { Handler h = cell->value;

    if ( isAEvent(ev, h->event) &&
      	 forwardReceiverCode(h->message, gr, gr, ev, 0) )
      succeed;
  }

  fail;
}


static status
synchronousDisplay(DisplayObj d, Bool val)
{ TRY(openDisplay(d));

  if ( val == OFF )
    ws_asynchronous(d);
  else
    ws_synchronous(d);

  succeed;
}


static status
resetDisplay(DisplayObj d)
{ PceWindow sw;

  grabServerDisplay(d, OFF);

  if ( (sw = getAttributeObject(d, NAME_confirmer)) )
    send(sw, NAME_show, OFF, 0);

  return resetVisual((VisualObj) d);
}

extern status postscriptDisplay(DisplayObj d);

static status
quitDisplay(DisplayObj d)
{ ws_quit_display(d);

  succeed;
}

		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

status
loadResourceFileDisplay(DisplayObj d, FileObj f)
{
#ifdef O_NOX11RESOURCES
  return load_resource_file(f);
#else
  fail;
#endif
}


		/********************************
		*          FONT TABLES		*
		********************************/

static status
loadFontFamilyDisplay(DisplayObj d, Name fam)
{ Class class = classOfObject(d);

  if ( !getResourceClass(class, fam) )
    attach_resource(class, strName(fam), "chain", "[]", "Font family set");

  if ( !getResourceValueObject(d, fam) )
    return errorPce(d, NAME_noFontsInFamily, fam);

  succeed;
} 


static status
loadFontsDisplay(DisplayObj d)
{ Chain fams;
  static int done = FALSE;

  if ( done )
    succeed;
  done = TRUE;

  if ( (fams = getResourceValueObject(d, NAME_fontFamilies)) )
  { Cell cell;

    for_cell(cell, fams)
      send(d, NAME_loadFontFamily, cell->value, 0);
  }

  succeed;
}


static status
loadFontAliasesDisplay(DisplayObj d, Name res)
{ Chain ch = getResourceValueObject(d, res);

  if ( ch )
  { Cell cell;
    Type type_font = nameToType(NAME_font);

    for_cell(cell, ch)
    { Name name;
      FontObj font;
      Any n, f;

      if ( instanceOfObject(cell->value, ClassBinding) )
      { Binding b = cell->value;
	n = b->name;
	f = b->value;
      } else if ( instanceOfObject(cell->value, ClassTuple) )
      { Tuple t = cell->value;
	n = t->first;
	f = t->second;
      } else if ( instanceOfObject(cell->value, ClassAttribute) )
      { Attribute a = cell->value;
	n = a->name;
	f = a->value;
      } else
      { errorPce(cell->value, NAME_unexpectedType,
		 CtoType(":=|tuple|attribute"));
	continue;
      }

      if ( !(name = checkType(n, TypeName, d)) ||
	   !(font = checkType(f, type_font, d)) )
	errorPce(d, NAME_badFontAlias, n, f);
      else
	send(d, NAME_fontAlias, name, font, 0);
    }

    succeed;
  }

  fail;
}


static status
fontAliasDisplay(DisplayObj d, Name name, FontObj font, Bool force)
{ if ( force == ON || !getMemberHashTable(d->font_table, name) )
    appendHashTable(d->font_table, name, font);

  succeed;
}


static FontObj
getFontAliasDisplay(DisplayObj d, Name name)
{ FontObj f;

  if ( (f = getMemberHashTable(d->font_table, name)) )
    answer(f);

  makeBuiltinFonts();
       
  answer(getMemberHashTable(d->font_table, name));
}


		/********************************
		*             VISUAL		*
		********************************/

static Chain
getContainsDisplay(DisplayObj d)
{ answer(d->frames);
}


static Any
getContainedInDisplay(DisplayObj d)
{ answer(d->display_manager);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "address=[name]", "resource_class=[name]" };
static char *T_cutBuffer[] =
        { "buffer=[0..7]", "value=string" };
static char *T_busyCursor[] =
        { "cursor=[cursor]*", "block_input=[bool]" };
static char *T_drawIn[] =
        { "graphical", "at=[point]", "invert=[bool]", "subwindow=[bool]" };
static char *T_report[] =
        { "kind={status,inform,progress,done,warning,error}",
	  "format=[char_array]", "argument=any ..." };
static char *T_postscript[] =
        { "landscape=[bool]", "max_area=[area]" };
static char *T_fontAlias[] =
        { "name=name", "font=font", "force=[bool]" };
static char *T_name_any_XXX[] =
        { "name", "any ..." };
static char *T_selectionOwner[] =
        { "owner=object*", "which=[name]", "convert=[function]",
	  "loose=[code]",
#ifdef __WINDOWS__
	  "type=[{text,emf,wmf}]"	/* metafile types */
#else
	  "type=[{text}]"
#endif
	};
static char *T_getSelection[] =
        { "which=[name]", "target=[name]", "type=[type]" };
static char *T_selection[] =
        { "which=[name]", "value=char_array" };

/* Instance Variables */

static vardecl var_display[] =
{ IV(NAME_size, "size*", IV_NONE,
     NAME_dimension, "Size (width, height) of display"),
  IV(NAME_address, "[name]", IV_BOTH,
     NAME_address, "Host/screen on which display resides"),
  IV(NAME_resourceClass, "name", IV_GET,
     NAME_resource, "Resource class of display [Pce]"),
  IV(NAME_fontTable, "hash_table", IV_BOTH,
     NAME_font, "Mapping for logical font-names to fonts"),
  IV(NAME_frames, "chain", IV_GET,
     NAME_organisation, "Frames displayed on this display"),
  IV(NAME_inspectHandlers, "chain", IV_GET,
     NAME_event, "Chain of handlers to support inspector tools"),
  SV(NAME_foreground, "colour", IV_GET|IV_STORE, foregroundDisplay,
     NAME_appearance, "Windows default foreground colour"),
  SV(NAME_background, "colour", IV_GET|IV_STORE, backgroundDisplay,
     NAME_appearance, "Windows default background colour"),
  SV(NAME_colourMap, "[colour_map]*", IV_GET|IV_STORE, colourMapDisplay,
     NAME_appearance, "Default for `frame ->colour_map'"),
  IV(NAME_quickAndDirty, "bool", IV_BOTH,
     NAME_cache, "Painting quick or correct?"),
  IV(NAME_cache, "image*", IV_BOTH,
     NAME_cache, "Scratch image to avoid flickering"),
  IV(NAME_windowManager, "[{twm,olwm,mwm}|name]", IV_SEND,
     NAME_windowManager, "Window manager running on this display"),
  IV(NAME_displayManager, "display_manager", IV_GET,
     NAME_organisation, "The global display manager (@display_manager)"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_windowSystem, "Window-System reference")
};

/* Send Methods */

static senddecl send_display[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseDisplay,
     DEFAULT, "Create from address and resource class"),
  SM(NAME_reset, 0, NULL, resetDisplay,
     NAME_abort, "Closedown informer/confirmer"),
  SM(NAME_flush, 0, NULL, flushDisplay,
     NAME_animate, "Flush pending commands to X-server"),
  SM(NAME_grabServer, 1, "grab=bool", grabServerDisplay,
     NAME_animate, "Freeze all other applications"),
  SM(NAME_synchronise, 0, NULL, synchroniseDisplay,
     NAME_animate, "->flush and process pending events"),
  SM(NAME_synchronous, 1, "[bool]", synchronousDisplay,
     NAME_debugging, "Make communication to X-server synchronous"),
  SM(NAME_busyCursor, 2, T_busyCursor, busyCursorDisplay,
     NAME_event, "Define (temporary) cursor for all frames on the display"),
  SM(NAME_dispatch, 0, NULL, dispatchDisplay,
     NAME_event, "Dispatch events for 1/4th second"),
  SM(NAME_eventQueued, 0, NULL, eventQueuedDisplay,
     NAME_event, "Test if there are X-events waiting"),
  SM(NAME_inspectHandler, 1, "handler", inspectHandlerDisplay,
     NAME_event, "Register handler for inspect tool"),
  SM(NAME_loadResourceFile, 1, "file", loadResourceFileDisplay,
     NAME_resource, "Load resource-definitions from file"),
  SM(NAME_fontAlias, 3, T_fontAlias, fontAliasDisplay,
     NAME_font, "Define a logical name for a font"),
  SM(NAME_loadFontAliases, 1, "resource=name", loadFontAliasesDisplay,
     NAME_font, "Load font aliases from named resource"),
  SM(NAME_loadFontFamily, 1, "family=name", loadFontFamilyDisplay,
     NAME_font, "Create predefined fonts from family"),
  SM(NAME_loadFonts, 0, NULL, loadFontsDisplay,
     NAME_font, "Create predefined font set from resources"),
  SM(NAME_ConfirmPressed, 1, "event", ConfirmPressedDisplay,
     NAME_internal, "Handle confirmer events"),
  SM(NAME_open, 0, NULL, openDisplay,
     NAME_open, "Open connection to X-server and initialise"),
  SM(NAME_Postscript, 0, NULL, postscriptDisplay,
     NAME_postscript, "Create PostScript"),
  SM(NAME_quit, 0, NULL, quitDisplay,
     NAME_quit, "Destroy all window-system references"),
  SM(NAME_bell, 1, "volume=[int]", bellDisplay,
     NAME_report, "Ring the bell at volume"),
  SM(NAME_confirm, 2, T_name_any_XXX, confirmDisplay,
     NAME_report, "Test if the user confirms string"),
  SM(NAME_inform, 2, T_name_any_XXX, informDisplay,
     NAME_report, "Inform the user of something"),
  SM(NAME_report, 3, T_report, reportDisplay,
     NAME_report, "Report message using ->inform"),
  SM(NAME_drawIn, 4, T_drawIn, drawInDisplay,
     NAME_root, "Draw graphical in root window"),
  SM(NAME_cutBuffer, 2, T_cutBuffer, cutBufferDisplay,
     NAME_selection, "Set value of numbered X-cut buffer"),
  SM(NAME_selectionOwner, 5, T_selectionOwner, selectionOwnerDisplay,
     NAME_selection, "Define the owner of the X11 selection"),
  SM(NAME_selectionTimeout, 1, "real", selectionTimeoutDisplay,
     NAME_selection, "Set the timeout-time for getting the selection value"),
  SM(NAME_selection, 2, T_selection, selectionDisplay,
     NAME_selection, "Set the (textual) selection"),
  SM(NAME_copy, 1, "char_array", copyDisplay,
     NAME_selection, "Copy to selection and cut_buffer"),
  SM(NAME_screenSaver, 1, "bool", screenSaverDisplay,
     NAME_x, "Activate (@on) or deactivate (@off) screensaver")
};

/* Get Methods */

static getdecl get_display[] =
{ GM(NAME_containedIn, 0, "display_manager", NULL, getContainedInDisplay,
     DEFAULT, "Display manager"),
  GM(NAME_contains, 0, "chain", NULL, getContainsDisplay,
     DEFAULT, "Chain with frames contained"),
  GM(NAME_convert, 1, "display", "any", getConvertDisplay,
     DEFAULT, "Convert graphical or `host:display[.screen]'"),
  GM(NAME_depth, 0, "bits_per_pixel=int", NULL, getDepthDisplay,
     NAME_colour, "Number of bits/pixel"),
  GM(NAME_visualType, 0,
     "{monochrome,static_grey,grey_scale,static_colour,pseudo_colour,true_colour,direct_colour}",
     NULL, getVisualTypeDisplay,
     NAME_colour, "Type of display attached"),
  GM(NAME_height, 0, "int", NULL, getHeightDisplay,
     NAME_dimension, "Height of the display in pixels"),
  GM(NAME_size, 0, "size", NULL, getSizeDisplay,
     NAME_dimension, "Size of the display"),
  GM(NAME_width, 0, "int", NULL, getWidthDisplay,
     NAME_dimension, "Width of the display in pixels"),
  GM(NAME_fontAlias, 1, "font", "name=name", getFontAliasDisplay,
     NAME_font, "Lookup logical name"),
  GM(NAME_connectionFd, 0, "int", NULL, getConnectionFdDisplay,
     NAME_host, "Unix file descriptor for X-display connection"),
  GM(NAME_boundingBox, 0, "area", NULL, getBoundingBoxDisplay,
     NAME_postscript, "PostScript bounding box for the display"),
  GM(NAME_postscript, 2, "string", T_postscript, getPostscriptObject,
     NAME_postscript, "Get PostScript or (area of) display"),
  GM(NAME_image, 1, "image", "[area]", getImageDisplay,
     NAME_conversion, "Image with the pixels of a region from the display"),
  GM(NAME_cutBuffer, 1, "string", "buffer=[0..7]", getCutBufferDisplay,
     NAME_selection, "New string with value of cut-buffer"),
  GM(NAME_selection, 3, "any", T_getSelection, getSelectionDisplay,
     NAME_selection, "Query value of the X-window selection"),
  GM(NAME_selectionOwner, 1, "object", "which=[name]", getSelectionOwnerDisplay,
     NAME_selection, "Current object owning the X11 selection"),
  GM(NAME_selectionTimeout, 0, "real", NULL, getSelectionTimeoutDisplay,
     NAME_selection, "Get the current selection timeout time (seconds)"),
  GM(NAME_paste, 0, "string", NULL, getPasteDisplay,
     NAME_selection, "Simple interface to get clipboard value"),
  GM(NAME_windowManager, 0, "[{twm,olwm,mwm,fvwm}|name]", NULL,
     getWindowManagerDisplay,
     NAME_windowManager, "Window manager running on this display")
};

/* Resources */

static resourcedecl rc_display[] =
{ RC(NAME_background, "colour", "white",
     "Default background for windows"),
  RC(NAME_foreground, "colour", "black",
     "Default foreground for windows"),
  RC(NAME_graphicsCache, "[size]", "@default",
     "Size of cache image to avoid flickering"),
  RC(NAME_initialise, "code*", "@nil",
     "Code object to run on ->open"),
  RC(NAME_labelFont, "font", "bold",
     "Label font for confirm/inform"),
  RC(NAME_systemFonts, "chain",
     "[ normal    := font(helvetica, roman, 12),\n"
     "  bold      := font(helvetica, bold, 12),\n"
     "  italic    := font(helvetica, oblique, 12),\n"
     "  small     := font(helvetica, roman, 10),\n"
     "  large     := font(helvetica, roman, 14),\n"
     "  boldlarge := font(helvetica, bold, 14),\n"
     "  huge      := font(helvetica, roman, 18),\n"
     "  boldhuge  := font(helvetica, bold, 18),\n"
     "  fixed     := font(screen, roman, 13)\n"
     "]",
     "Predefined font-aliases"),
  RC(NAME_userFonts, "chain", "[]",
     "User font-aliases"),
  RC(NAME_noFont, "font", "fixed",
     "Replacement for undefined fonts"),
  RC(NAME_quickAndDirty, "bool", "@on",
     "Draw quick or correct"),
  RC(NAME_valueFont, "font", "normal",
     "Text font for confirm/inform"),
  RC(NAME_volume, "int", "0",
     "Default volume of ->bell"),
  RC(NAME_windowManager, "[name]", "@default",
     "Window manager running on this display")
#ifdef _WINDOWS
  ,
					/* @nil     --> no handling */
					/* @default --> task-level handling */
					/* name	    --> external dll */
  RC(NAME_whMouseDll, "[name]*", "xpcemh.dll",
     "DLL to generate area_enter/area_exit events")
#endif
};

/* Class Declaration */

static Name display_termnames[] = { NAME_address, NAME_resourceClass };

ClassDecl(display_decls,
          var_display, send_display, get_display, rc_display,
          2, display_termnames,
          "$Rev$");



status
makeClassDisplay(Class class)
{ DisplayObj TheDisplay;

  declareClass(class, &display_decls);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  TheDisplay = globalObject(NAME_display, ClassDisplay, 0);
  globalObject(NAME_colourDisplay, ClassGreater,
	       newObject(ClassObtain, TheDisplay, NAME_depth, 0),
	       ONE, 0);

  attach_font_families(class);

  succeed;
}
  

#ifdef __WINDOWS__
#define PFONT(n, p, x) { n, p }
#define ENDFONTLIST    { NULL, 0 }
#else
#define PFONT(n, p, x) { n, p, x }
#define ENDFONTLIST    { NULL, 0, NULL}
#endif

typedef struct
{ Name style;
  int  points;
#ifndef __WINDOWS__
  char *xname;
#endif
} fontdef, *FontDef;


static fontdef screen_fonts[] =
{ PFONT(NAME_roman, 10, "6x10"),
  PFONT(NAME_roman, 12, "6x12"),
  PFONT(NAME_roman, 13, "8x13"),
  PFONT(NAME_roman, 14, "7x14"),
  PFONT(NAME_roman, 15, "9x15"),
  PFONT(NAME_bold,  13, "8x13bold"),
  PFONT(NAME_bold,  14, "7x14bold"),
  PFONT(NAME_bold,  15, "9x15bold"),
  ENDFONTLIST
};


static fontdef courier_fonts[] =
{ PFONT(NAME_roman, 10,
	"-adobe-courier-medium-r-normal--10-100-75-75-m-60-iso8859-1"),
  PFONT(NAME_roman, 12,
	"-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"),
  PFONT(NAME_roman, 14,
	"-adobe-courier-medium-r-normal--14-140-75-75-m-90-iso8859-1"),
  PFONT(NAME_roman, 18,
	"-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1"),
  PFONT(NAME_roman, 24,
	"-adobe-courier-medium-r-normal--24-240-75-75-m-150-iso8859-1"),
  PFONT(NAME_bold, 10,
	"-adobe-courier-bold-r-normal--10-100-75-75-m-60-iso8859-1"),
  PFONT(NAME_bold, 12,
	"-adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1"),
  PFONT(NAME_bold, 14,
	"-adobe-courier-bold-r-normal--14-140-75-75-m-90-iso8859-1"),
  PFONT(NAME_bold, 18,
	"-adobe-courier-bold-r-normal--18-180-75-75-m-110-iso8859-1"),
  PFONT(NAME_bold, 24,
	"-adobe-courier-bold-r-normal--24-240-75-75-m-150-iso8859-1"),
  PFONT(NAME_oblique, 10,
	"-adobe-courier-medium-o-normal--10-100-75-75-m-60-iso8859-1"),
  PFONT(NAME_oblique, 12,
	"-adobe-courier-medium-o-normal--12-120-75-75-m-70-iso8859-1"),
  PFONT(NAME_oblique, 14,
	"-adobe-courier-medium-o-normal--14-140-75-75-m-90-iso8859-1"),
  PFONT(NAME_oblique, 18,
	"-adobe-courier-medium-o-normal--18-180-75-75-m-110-iso8859-1"),
  PFONT(NAME_oblique, 24,
	"-adobe-courier-medium-o-normal--24-240-75-75-m-150-iso8859-1"),
  ENDFONTLIST
};


static fontdef helvetica_fonts[] =
{ PFONT(NAME_bold, 10,
	"-adobe-helvetica-bold-r-normal--10-100-75-75-p-60-iso8859-1"),
  PFONT(NAME_bold, 12,
	"-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1"),
  PFONT(NAME_bold, 14,
	"-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"),
  PFONT(NAME_bold, 18,
	"-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1"),
  PFONT(NAME_bold, 24,
	"-adobe-helvetica-bold-r-normal--24-240-75-75-p-138-iso8859-1"),
  PFONT(NAME_roman, 10,
	"-adobe-helvetica-medium-r-normal--10-100-75-75-p-56-iso8859-1"),
  PFONT(NAME_roman, 12,
	"-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1"),
  PFONT(NAME_roman, 14,
	"-adobe-helvetica-medium-r-normal--14-140-75-75-p-77-iso8859-1"),
  PFONT(NAME_roman, 18,
	"-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1"),
  PFONT(NAME_roman, 24,
	"-adobe-helvetica-medium-r-normal--24-240-75-75-p-130-iso8859-1"),
  PFONT(NAME_oblique, 10,
	"-adobe-helvetica-medium-o-normal--10-100-75-75-p-57-iso8859-1"),
  PFONT(NAME_oblique, 12,
	"-adobe-helvetica-medium-o-normal--12-120-75-75-p-67-iso8859-1"),
  PFONT(NAME_oblique, 14,
	"-adobe-helvetica-medium-o-normal--14-140-75-75-p-78-iso8859-1"),
  PFONT(NAME_oblique, 18,
	"-adobe-helvetica-medium-o-normal--18-180-75-75-p-98-iso8859-1"),
  PFONT(NAME_oblique, 24,
	"-adobe-helvetica-medium-o-normal--24-240-75-75-p-130-iso8859-1"),
  ENDFONTLIST
};


static fontdef times_fonts[] =
{ PFONT(NAME_roman, 10,
	"-adobe-times-medium-r-normal--10-100-75-75-p-54-iso8859-1"),
  PFONT(NAME_roman, 12,
	"-adobe-times-medium-r-normal--12-120-75-75-p-64-iso8859-1"),
  PFONT(NAME_roman, 14,
	"-adobe-times-medium-r-normal--14-140-75-75-p-74-iso8859-1"),
  PFONT(NAME_roman, 18,
	"-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"),
  PFONT(NAME_roman, 24,
	"-adobe-times-medium-r-normal--24-240-75-75-p-124-iso8859-1"),
  PFONT(NAME_bold, 10,
	"-adobe-times-bold-r-normal--10-100-75-75-p-57-iso8859-1"),
  PFONT(NAME_bold, 12,
	"-adobe-times-bold-r-normal--12-120-75-75-p-67-iso8859-1"),
  PFONT(NAME_bold, 14,
	"-adobe-times-bold-r-normal--14-140-75-75-p-77-iso8859-1"),
  PFONT(NAME_bold, 18,
	"-adobe-times-bold-r-normal--18-180-75-75-p-99-iso8859-1"),
  PFONT(NAME_bold, 24,
	"-adobe-times-bold-r-normal--24-240-75-75-p-132-iso8859-1"),
  PFONT(NAME_italic, 10,
	"-adobe-times-medium-i-normal--10-100-75-75-p-52-iso8859-1"),
  PFONT(NAME_italic, 12,
	"-adobe-times-medium-i-normal--12-120-75-75-p-63-iso8859-1"),
  PFONT(NAME_italic, 14,
	"-adobe-times-medium-i-normal--14-140-75-75-p-73-iso8859-1"),
  PFONT(NAME_italic, 18,
	"-adobe-times-medium-i-normal--18-180-75-75-p-94-iso8859-1"),
  PFONT(NAME_italic, 24,
	"-adobe-times-medium-i-normal--24-240-75-75-p-125-iso8859-1"),
  ENDFONTLIST
};



static char *
default_font_list(Name fam, FontDef defs)
{ char buf[10240];
  char *s = buf;

  *s++ = '[';
  
  while(defs->style)
  {
#ifdef __WINDOWS__
    sprintf(s, "font(%s, %s, %d)",
	    strName(fam),
	    strName(defs->style),
	    defs->points);
#else
    sprintf(s, "font(%s, %s, %d, \"%s\")",
	    strName(fam),
	    strName(defs->style),
	    defs->points,
	    defs->xname);
#endif
    s += strlen(s);
    defs++;
    if ( defs->style )
      strcpy(s, ",\n");
    s += strlen(s);
  }

  *s++ = ']';
  *s = EOS;

  return save_string(buf);
}
	    

static void
attach_fonts(Class class, char *res, Name fam, FontDef defs)
{ attach_resource(class, res, "chain",
		  default_font_list(fam, defs),
		  "Font family set");
}


static void
attach_font_families(Class class)
{ attach_resource(class, "font_families",  "chain",
		  "[screen_fonts, courier_fonts," /* concat */
		  "helvetica_fonts, times_fonts]",
		  "Predefined font families");

  attach_fonts(class, "screen_fonts", NAME_screen, screen_fonts);
  attach_fonts(class, "courier_fonts", NAME_courier, courier_fonts);
  attach_fonts(class, "helvetica_fonts", NAME_helvetica, helvetica_fonts);
  attach_fonts(class, "times_fonts", NAME_times, times_fonts);
}
