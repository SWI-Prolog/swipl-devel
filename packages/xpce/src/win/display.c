/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	backgroundDisplay P((DisplayObj, Colour));
static status	foregroundDisplay(DisplayObj d, Colour c);
static void	attach_font_families(Class class);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a display.  The display is not yet opened.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseDisplay(DisplayObj d, Name address, Name resource_class)
{ DisplayManager dm = TheDisplayManager();

  if ( isDefault(resource_class) )
    resource_class = CtoName("Pce");

  assign(d, size,        	NIL);
  assign(d, address,     	address);
  assign(d, resource_class,    	resource_class);
  assign(d, frames,	 	newObject(ClassChain, 0));
  assign(d, inspect_handlers,	newObject(ClassChain, 0));
  assign(d, foreground,	 	DEFAULT);
  assign(d, background,	 	DEFAULT);
  assign(d, quick_and_dirty,	DEFAULT);
  assign(d, cache,	 	NIL);
  assign(d, window_manager,	DEFAULT);
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
{ if ( ws_opened_display(d) )
    succeed;

  DEBUG(NAME_display, printf("Opening display %s\n", pp(d)));

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


static status
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
{ ws_set_selection_timeout((ulong)(time->value * 1000.0));

  succeed;
}


static Any
getSelectionDisplay(DisplayObj d, Name which, Name target, Type type)
{ Any sel;

  TRY(openDisplay(d));

  if ( isDefault(which) )  which  = NAME_primary;
  if ( isDefault(target) ) target = NAME_string;
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
		      Function convert, Code loose)
{ TRY(openDisplay(d));

  if ( isDefault(selection) )
    selection = NAME_primary;

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

    attach_attribute(h, NAME_convertFunction,
		     newObject(ClassQuoteFunction, convert, 0));
    attach_attribute(h, NAME_looseMessage, loose);

    if ( !old )
    { if ( !ws_own_selection(d, selection) )
      { freeHypersObject(d, hypername, DEFAULT);
	return errorPce(owner, NAME_cannotBecomeSelectionOwner, selection);
      }
    }
  }

  succeed;
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

  attach_attribute(d, NAME_SeenDown, OFF);
  attach_attribute(d, NAME_confirmer, p);
  attach_attribute(p, NAME_helpText, h);
  attach_attribute(p, NAME_messageText, m);
  
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
  Name msg = CtoName("Press LEFT button to confirm, RIGHT button to cancel");
  ArgVector(av, argc+1);
  int i;
  Name button;

  av[0] = (Any) fmt;
  for(i=0; i<argc; i++)
    av[i+1] = argv[i];

  TRY(str = answerObjectv(ClassString, argc+1, av));
  TRY(button = display_help(d, str, msg));
  doneObject(str);

  if ( equalName(button, NAME_left) )
    succeed;
  fail;
}


status
informDisplay(DisplayObj d, CharArray fmt, int argc, Any *argv)
{ StringObj str;
  Name msg = CtoName("Press any button to remove message");
  ArgVector(av, argc+1);
  int i;

  av[0] = (Any) fmt;
  for(i=0; i<argc; i++)
    av[i+1] = argv[i];
  TRY(str = answerObjectv(ClassString, argc+1, av));
  display_help(d, str, msg);
  doneObject(str);
  
  succeed;
}


static status
reportDisplay(DisplayObj d, Name kind, CharArray fmt, int argc, Any *argv)
{ if ( equalName(kind, NAME_error) || equalName(kind, NAME_inform) )
  { ArgVector(av, argc+1);

    av[0] = isDefault(fmt) ? (CharArray) CtoName("") : fmt;
    copyArgs(argc, argv, &av[1]);
    if ( kind == NAME_error )
      alertReporteeVisual(d);

    sendv(d, NAME_inform, argc+1, av);
  } else if ( equalName(kind, NAME_warning) )
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

    if ( isAEvent(ev, h->event) == SUCCEED &&
      	 forwardReceiverCode(h->message, gr, gr, ev, 0) != FAIL )
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

extern postscriptDisplay(DisplayObj d);


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


status
makeClassDisplay(Class class)
{ sourceClass(class, makeClassDisplay, __FILE__, "$Revision$");

  localClass(class, NAME_size, NAME_dimension, "size*", NAME_none,
	     "Size (width, height) of display");
  localClass(class, NAME_address, NAME_address, "[name]", NAME_both,
	     "Host/screen on which display resides");
  localClass(class, NAME_resourceClass, NAME_resource, "name", NAME_get,
	     "Resource class of display [Pce]");
  localClass(class, NAME_frames, NAME_organisation, "chain", NAME_get,
	     "Frames displayed on this display");
  localClass(class, NAME_inspectHandlers, NAME_event, "chain", NAME_get,
	     "Chain of handlers to support inspector tools");
  localClass(class, NAME_foreground, NAME_appearance, "colour", NAME_get,
	     "Windows default foreground colour");
  localClass(class, NAME_background, NAME_appearance, "colour", NAME_get,
	     "Windows default background colour");
  localClass(class, NAME_quickAndDirty, NAME_cache, "bool", NAME_both,
	     "Painting quick or correct?");
  localClass(class, NAME_cache, NAME_cache, "image*", NAME_both,
	     "Scratch image to avoid flickering");
  localClass(class, NAME_windowManager, NAME_windowManager,
	     "[{twm,olwm,mwm}|name]", NAME_send,
	     "Window manager running on this display");
  localClass(class, NAME_displayManager, NAME_organisation,
	     "display_manager", NAME_get,
	     "The global display manager (@display_manager)");
  localClass(class, NAME_wsRef, NAME_windowSystem, "alien:WsRef", NAME_none,
	     "Window-System reference");

  termClass(class, "display", 2, NAME_address, NAME_resourceClass);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  storeMethod(class, NAME_foreground, foregroundDisplay);
  storeMethod(class, NAME_background, backgroundDisplay);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "address=[name]", "resource_class=[name]",
	     "Create from address and resource class",
	     initialiseDisplay);
  sendMethod(class, NAME_reset, NAME_abort, 0,
	     "Closedown informer/confirmer",
	     resetDisplay);
  sendMethod(class, NAME_confirm, NAME_report, 2, "name", "any ...",
	     "Test if the user confirms string",
	     confirmDisplay);
  sendMethod(class, NAME_ConfirmPressed, NAME_internal, 1, "event",
	     "Handle confirmer events",
	     ConfirmPressedDisplay);
  sendMethod(class, NAME_inform, NAME_report, 2, "name", "any ...",
	     "Inform the user of something",
	     informDisplay);
  sendMethod(class, NAME_busyCursor, NAME_event, 2,
	     "cursor=[cursor]*", "block_input=[bool]",
	     "Define (temporary) cursor for all frames on the display",
	     busyCursorDisplay);
  sendMethod(class, NAME_open, NAME_open, 0,
	     "Open connection to X-server and initialise",
	     openDisplay);
  sendMethod(class, NAME_eventQueued, NAME_event, 0,
	     "Test if there are X-events waiting",
	     eventQueuedDisplay);
  sendMethod(class, NAME_dispatch, NAME_event, 0,
	     "Dispatch events for 1/4th second",
	     dispatchDisplay);
  sendMethod(class, NAME_flush, NAME_animate, 0,
	     "Flush pending commands to X-server",
	     flushDisplay);
  sendMethod(class, NAME_synchronise, NAME_animate, 0,
	     "->flush and process pending events",
	     synchroniseDisplay);
  sendMethod(class, NAME_Postscript, NAME_postscript, 0,
	     "Create PostScript",
	     postscriptDisplay);
  sendMethod(class, NAME_bell, NAME_report, 1, "volume=[int]",
	     "Ring the bell at volume",
	     bellDisplay);
  sendMethod(class, NAME_cutBuffer, NAME_selection, 2,
	     "buffer=[0..7]", "value=string",
	     "Set value of numbered X-cut buffer",
	     cutBufferDisplay);
  sendMethod(class, NAME_synchronous, NAME_debugging, 1, "[bool]",
	     "Make communication to X-server synchronous",
	     synchronousDisplay);
  sendMethod(class, NAME_inspectHandler, NAME_event, 1, "handler",
	     "Register handler for inspect tool",
	     inspectHandlerDisplay);
  sendMethod(class, NAME_screenSaver, NAME_x, 1, "bool",
	     "Activate (@on) or deactivate (@off) screensaver",
	     screenSaverDisplay);
  sendMethod(class, NAME_report, NAME_report, 3,
	     "kind={status,inform,progress,done,warning,error}",
	     "format=[char_array]", "argument=any ...",
	     "Report message using ->inform",
	     reportDisplay);
  sendMethod(class, NAME_drawIn, NAME_root, 4,
	     "graphical", "at=[point]", "invert=[bool]", "subwindow=[bool]",
	     "Draw graphical in root window",
	     drawInDisplay);
  sendMethod(class, NAME_grabServer, NAME_animate, 1, "grab=bool",
	     "Freeze all other applications",
	     grabServerDisplay);
  sendMethod(class, NAME_selectionTimeout, NAME_selection, 1, "real",
	     "Set the timeout-time for getting the selection value",
	     selectionTimeoutDisplay);
  sendMethod(class, NAME_selectionOwner, NAME_selection, 4,
	     "owner=object*", "which=[name]", "convert=[function]",
	     "loose=[code]",
	     "Define the owner of the X11 selection",
	     selectionOwnerDisplay);
  sendMethod(class, NAME_loadFonts, NAME_font, 0,
	     "Create predefined font set from resources",
	     loadFontsDisplay);
  sendMethod(class, NAME_loadFontFamily, NAME_font, 1, "family=name",
	     "Create predefined fonts from family",
	     loadFontFamilyDisplay);


  getMethod(class, NAME_size, NAME_dimension, "size", 0,
	    "Size of the display",
	    getSizeDisplay);
  getMethod(class, NAME_width, NAME_dimension, "int", 0,
	    "Width of the display in pixels",
	    getWidthDisplay);
  getMethod(class, NAME_height, NAME_dimension, "int", 0,
	    "Height of the display in pixels",
	    getHeightDisplay);
  getMethod(class, NAME_boundingBox, NAME_postscript, "area", 0,
	    "PostScript bounding box for the display",
	    getBoundingBoxDisplay);
  getMethod(class, NAME_postscript, NAME_postscript, "string", 2,
	    "landscape=[bool]", "max_area=[area]",
	    "Get PostScript or (area of) display",
	    getPostscriptObject);
  getMethod(class, NAME_cutBuffer, NAME_selection, "string", 1,"buffer=[0..7]",
	    "New string with value of cut-buffer",
	    getCutBufferDisplay);
  getMethod(class, NAME_selectionTimeout, NAME_selection, "real", 0,
	    "Get the current selection timeout time (seconds)",
	    getSelectionTimeoutDisplay);
  getMethod(class, NAME_selection, NAME_selection, "any", 3,
	    "which=[name]", "target=[name]", "type=[type]",
	    "Query value of the X-window selection",
	    getSelectionDisplay);
  getMethod(class, NAME_selectionOwner, NAME_selection, "object", 1,
	    "which=[name]",
	    "Current object owning the X11 selection",
	    getSelectionOwnerDisplay);
  getMethod(class, NAME_depth, NAME_colour, "bits_per_pixel=int", 0,
	    "Number of bits/pixel",
	    getDepthDisplay);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "Chain with frames contained",
	    getContainsDisplay);
  getMethod(class, NAME_containedIn, DEFAULT, "display_manager", 0,
	    "Display manager",
	    getContainedInDisplay);
  getMethod(class, NAME_windowManager, NAME_windowManager,
	    "[{twm,olwm,mwm}|name]", 0,
	    "Window manager running on this display",
	    getWindowManagerDisplay);
  getMethod(class, NAME_convert, DEFAULT, "display", 1, "any",
	    "Convert graphical or `host:display[.screen]'",
	    getConvertDisplay);
  getMethod(class, NAME_connectionFd, NAME_host, "int", 0,
	    "Unix file descriptor for X-display connection",
	    getConnectionFdDisplay);

  initClass(class);
					/* set up the displays */
  globalObject(NAME_display, ClassDisplay, 0);

  attach_resource(class, "label_font",	   "font",    "@helvetica_bold_14",
		  "Label font for confirm/inform");
  attach_resource(class, "value_font",     "font",    "@helvetica_roman_14",
		  "Text font for confirm/inform");
  attach_resource(class, "no_font",	   "font",    "@screen_roman_13",
		  "Replacement for undefined fonts");
  attach_resource(class, "graphics_cache", "[size]",  "@default",
		  "Size of cache image to avoid flickering");
  attach_resource(class, "volume",	   "int", "0",
		  "Default volume of ->bell");
  attach_resource(class, "foreground",	   "colour",  "black",
		  "Default foreground for windows");
  attach_resource(class, "background",	   "colour",  "white",
		  "Default background for windows");
  attach_resource(class, "quick_and_dirty","bool",    "@on",
		  "Draw quick or correct");
  attach_resource(class, "window_manager", "[name]",  "@default",
		  "Window manager running on this display");
  attach_font_families(class);

#ifdef __WINDOWS__
  attach_resource(class, "wh_mouse_dll", "name*", "pcewh.dll",
		  "DLL to generate area_enter/area_exit events");
#endif

  succeed;
}
  
typedef struct
{ Name style;
  int  points;
  char *xname;
} fontdef, *FontDef;

static fontdef screen_fonts[] =
{ { NAME_roman,	10,	"6x10" },
  { NAME_roman,	12,	"6x12" },
  { NAME_roman,	13,	"8x13" },
  { NAME_roman,	14,	"7x14" },
  { NAME_roman,	15,	"9x15" },
  { NAME_bold,	13,	"8x13bold" },
  { NAME_bold,	14,	"7x14bold" },
  { NAME_bold,	15,	"9x15bold" },
  { NULL,	0,	NULL }
};


static fontdef courier_fonts[] =
{ { NAME_roman, 10,
      "-adobe-courier-medium-r-normal--10-100-75-75-m-60-iso8859-1" },
  { NAME_roman,	12,
      "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1" },
  { NAME_roman,	14,
      "-adobe-courier-medium-r-normal--14-140-75-75-m-90-iso8859-1" },
  { NAME_roman,	18,
      "-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1" },
  { NAME_roman,	24,
      "-adobe-courier-medium-r-normal--24-240-75-75-m-150-iso8859-1" },
  { NAME_bold,	10,
      "-adobe-courier-bold-r-normal--10-100-75-75-m-60-iso8859-1" },
  { NAME_bold,	12,
      "-adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1" },
  { NAME_bold,	14,
      "-adobe-courier-bold-r-normal--14-140-75-75-m-90-iso8859-1" },
  { NAME_bold,	18,
      "-adobe-courier-bold-r-normal--18-180-75-75-m-110-iso8859-1" },
  { NAME_bold,	24,
      "-adobe-courier-bold-r-normal--24-240-75-75-m-150-iso8859-1" },
  { NAME_oblique,	10,
      "-adobe-courier-medium-o-normal--10-100-75-75-m-60-iso8859-1" },
  { NAME_oblique,	12,
      "-adobe-courier-medium-o-normal--12-120-75-75-m-70-iso8859-1" },
  { NAME_oblique,	14,
      "-adobe-courier-medium-o-normal--14-140-75-75-m-90-iso8859-1" },
  { NAME_oblique,	18,
      "-adobe-courier-medium-o-normal--18-180-75-75-m-110-iso8859-1" },
  { NAME_oblique,	24,
      "-adobe-courier-medium-o-normal--24-240-75-75-m-150-iso8859-1" },
  { NULL, 0, NULL }   
};


static fontdef helvetica_fonts[] =
{ { NAME_bold,	10,
      "-adobe-helvetica-bold-r-normal--10-100-75-75-p-60-iso8859-1" },
  { NAME_bold,	12,
      "-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1" },
  { NAME_bold,	14,
      "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1" },
  { NAME_bold,	18,
      "-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1" },
  { NAME_bold,	24,
      "-adobe-helvetica-bold-r-normal--24-240-75-75-p-138-iso8859-1" },
  { NAME_roman,	10,
      "-adobe-helvetica-medium-r-normal--10-100-75-75-p-56-iso8859-1" },
  { NAME_roman,	12,
      "-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1" },
  { NAME_roman,	14,
      "-adobe-helvetica-medium-r-normal--14-140-75-75-p-77-iso8859-1" },
  { NAME_roman,	18,
      "-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1" },
  { NAME_roman,	24,
      "-adobe-helvetica-medium-r-normal--24-240-75-75-p-130-iso8859-1" },
  { NAME_oblique,	10,
      "-adobe-helvetica-medium-o-normal--10-100-75-75-p-57-iso8859-1" },
  { NAME_oblique,	12,
      "-adobe-helvetica-medium-o-normal--12-120-75-75-p-67-iso8859-1" },
  { NAME_oblique,	14,
      "-adobe-helvetica-medium-o-normal--14-140-75-75-p-78-iso8859-1" },
  { NAME_oblique,	18,
      "-adobe-helvetica-medium-o-normal--18-180-75-75-p-98-iso8859-1" },
  { NAME_oblique,	24,
      "-adobe-helvetica-medium-o-normal--24-240-75-75-p-130-iso8859-1" },
  { NULL, 0, NULL }
};


static fontdef times_fonts[] =
{ { NAME_roman,	10,
      "-adobe-times-medium-r-normal--10-100-75-75-p-54-iso8859-1" },
  { NAME_roman,	12,
      "-adobe-times-medium-r-normal--12-120-75-75-p-64-iso8859-1" },
  { NAME_roman,	14,
      "-adobe-times-medium-r-normal--14-140-75-75-p-74-iso8859-1" },
  { NAME_roman,	18,
      "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1" },
  { NAME_roman,	24,
      "-adobe-times-medium-r-normal--24-240-75-75-p-124-iso8859-1" },
  { NAME_bold,	10,
      "-adobe-times-bold-r-normal--10-100-75-75-p-57-iso8859-1" },
  { NAME_bold,	12,
      "-adobe-times-bold-r-normal--12-120-75-75-p-67-iso8859-1" },
  { NAME_bold,	14,
      "-adobe-times-bold-r-normal--14-140-75-75-p-77-iso8859-1" },
  { NAME_bold,	18,
      "-adobe-times-bold-r-normal--18-180-75-75-p-99-iso8859-1" },
  { NAME_bold,	24,
      "-adobe-times-bold-r-normal--24-240-75-75-p-132-iso8859-1" },
  { NAME_italic,	10,
      "-adobe-times-medium-i-normal--10-100-75-75-p-52-iso8859-1" },
  { NAME_italic,	12,
      "-adobe-times-medium-i-normal--12-120-75-75-p-63-iso8859-1" },
  { NAME_italic,	14,
      "-adobe-times-medium-i-normal--14-140-75-75-p-73-iso8859-1" },
  { NAME_italic,	18,
      "-adobe-times-medium-i-normal--18-180-75-75-p-94-iso8859-1" },
  { NAME_italic,	24,
      "-adobe-times-medium-i-normal--24-240-75-75-p-125-iso8859-1" },
  { NULL, 0, NULL }
};



static char *
default_font_list(Name fam, FontDef defs)
{ char buf[10240];
  char *s = buf;

  *s++ = '[';
  
  while(defs->style)
  { sprintf(s, "font(%s, %s, %d, \"%s\")",
	    strName(fam),
	    strName(defs->style),
	    defs->points,
	    defs->xname);
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
