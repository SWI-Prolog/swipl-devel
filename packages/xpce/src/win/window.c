/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	uncreateWindow(PceWindow sw);
static status   tileWindow(PceWindow sw, TileObj t);
static status   updateScrollbarValuesWindow(PceWindow sw);
static void	unlink_changes_data_window(PceWindow sw);


status
initialiseWindow(PceWindow sw, Name label, Size size, DisplayObj display)
{ initialiseDevice((Device) sw);

  assign(sw, pen,		   DEFAULT);
  assign(sw, cursor,		   DEFAULT);
  assign(sw, scroll_offset,	   newObject(ClassPoint, 0));
  assign(sw, input_focus,	   OFF);
  assign(sw, has_pointer,	   OFF);
  assign(sw, background,	   DEFAULT);
  assign(sw, selection_feedback,   DEFAULT);
  assign(sw, sensitive,		   ON);
  assign(sw, bounding_box,	   newObject(ClassArea, 0));
  obtainResourcesObject(sw);

  if ( isDefault(size) )
    TRY( size = getResourceValueObject(sw, NAME_size) );
  setArea(sw->area, ZERO, ZERO, size->w, size->h);

  sw->changes_data = NULL;
  sw->ws_ref = NULL;

  if ( notDefault(label) || notDefault(display) )
    frameWindow(sw, newObject(ClassFrame, label, DEFAULT, display, 0));

  succeed;
}


static PceWindow
getConvertWindow(Class class, Graphical gr)
{ answer(getWindowGraphical(gr));
}

		 /*******************************
		 *	     SAVE-LOAD		*
		 *******************************/

static status
storeWindow(PceWindow sw, FileObj file)
{ return storeSlotsObject(sw, file);
}


static status
loadWindow(PceWindow sw, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(sw, fd, def));

  sw->ws_ref = NULL;
  assign(sw, displayed, OFF);
  if ( isNil(sw->has_pointer) )
    assign(sw, has_pointer, OFF);

  succeed;
}

		/********************************
		*    WINDOW-SYSTEM INTERFACE	*
		********************************/

status
createdWindow(PceWindow sw)
{ return ws_created_window(sw);
}


static status
uncreateWindow(PceWindow sw)
{ DEBUG(NAME_window, Cprintf("uncreateWindow(%s)\n", pp(sw)));

  deleteChain(ChangedWindows, sw);
  assign(sw, displayed, OFF);
  
  ws_uncreate_window(sw);

  succeed;
}


status
grabPointerWindow(PceWindow sw, Bool val)
{ ws_grab_pointer_window(sw, val);

  succeed;
}


status
grabKeyboardWindow(PceWindow sw, Bool val)
{ ws_grab_keyboard_window(sw, val);

  succeed;
}


		/********************************
		*          DESTRUCTION		*
		********************************/

static status
freeWindow(PceWindow sw)
{ if ( notNil(sw->frame) )
    return send(sw->frame, NAME_free, 0);
  else if ( notNil(sw->decoration) )
    return send(sw->decoration, NAME_free, 0);
  else
    return freeObject(sw);
}


static status
destroyWindow(PceWindow sw)
{ if ( notNil(sw->frame) )
    return destroyVisual((VisualObj) sw->frame);
  else if ( notNil(sw->decoration) )
    return destroyVisual((VisualObj) sw->decoration);
  else
    return destroyVisual((VisualObj) sw);
}


status
unlinkWindow(PceWindow sw)
{ assign(sw, displayed, OFF);		/* avoid updates */
  uncreateWindow(sw);
  unlink_changes_data_window(sw);
  unlinkDevice((Device) sw);

  if ( notNil(sw->frame) )
  { deleteChain(sw->frame->members, sw);
    assign(sw, frame, NIL);
  }

  succeed;
}

		/********************************
		*           OPEN/CREATE		*
		********************************/

static status
openWindow(PceWindow sw, Point pos, Bool normalise)
{ if ( send(sw, NAME_create, 0) &&
       send(getFrameWindow(sw, DEFAULT), NAME_open,
	    pos, DEFAULT, normalise, 0) )
  succeed;

  fail;
}


static status
openCenteredWindow(PceWindow sw, Point pos)
{ if ( send(sw, NAME_create, 0) && 
       send(getFrameWindow(sw, DEFAULT), NAME_openCentered, pos, 0) )
    succeed;

  fail;
}


static Any
getConfirmWindow(PceWindow sw, Point pos, Bool grab, Bool normalise)
{ TRY( send(sw, NAME_create, 0) );

  answer(getConfirmFrame(getFrameWindow(sw, DEFAULT), pos, grab, normalise));
}


static Any
getConfirmCenteredWindow(PceWindow sw, Point pos, Bool grab)
{ TRY( send(sw, NAME_create, 0) );

  answer(getConfirmCenteredFrame(getFrameWindow(sw, DEFAULT), pos, grab));
}


static status
createWindow(PceWindow sw, PceWindow parent)
{ if ( createdWindow(sw) )		/* already done */
    succeed;

  DEBUG(NAME_window, Cprintf("createWindow(%s, %s)\n", pp(sw), pp(parent)));

  if ( isDefault(parent) )		/* do my manager first */
  { if ( notNil(sw->decoration) )
    { if ( !createdWindow(sw->decoration) )
	return send(sw->decoration, NAME_create, 0);
      succeed;
    } else
    { if ( isNil(sw->frame) )
	frameWindow(sw, DEFAULT);
      if ( !createdFrame(sw->frame) )
	return send(sw->frame, NAME_create, 0);
    }
  } else
  { if ( !createdWindow(parent) )
      send(parent, NAME_create, 0);
  }

					/* fix the default colours */
  if ( notDefault(parent) )
  { if ( isDefault(sw->colour) )
      assign(sw, colour, parent->colour);
    if ( isDefault(sw->background) )
      assign(sw, background, parent->background);
  } else 
  { DisplayObj d;

    if ( notNil(sw->frame) )
      d = sw->frame->display;
    else
      d = CurrentDisplay(sw);

    if ( isDefault(sw->colour) )
      assign(sw, colour, d->foreground);
    if ( isDefault(sw->background) )
      assign(sw, background, d->background);
  }

  ws_create_window(sw, parent);
  qadSendv(sw, NAME_resize, 0, NULL);

  addChain(ChangedWindows, sw);		/* force initial update */

  succeed;
}


static status
ComputeDesiredSizeWindow(PceWindow sw)
{ succeed;
}


		 /*******************************
		 *	   DECORATIONS		*
		 *******************************/

static status
decorateWindow(PceWindow sw, Name how, Int lb, Int tb, Int rb, Int bb,
	       PceWindow dw)
{ if ( isDefault(how)) how= NAME_grow;
  if ( isDefault(lb) ) lb = ZERO;
  if ( isDefault(rb) ) rb = ZERO;
  if ( isDefault(tb) ) tb = ZERO;
  if ( isDefault(bb) ) bb = ZERO;
  if ( isDefault(dw) ) dw = newObject(ClassWindow, 0);

  if ( isDefault(dw->colour) )     assign(dw, colour, sw->colour);
  if ( isDefault(dw->background) ) assign(dw, background, sw->background);

  ws_reassociate_ws_window(sw, dw);

  assign(dw, tile, sw->tile);
  if ( instanceOfObject(dw->tile, ClassTile) )
    assign(dw->tile, object, dw);
  assign(sw, tile, NIL);

  if ( notNil(sw->frame) )
  { replaceChain(sw->frame->members, sw, dw);
    assign(dw, frame, sw->frame);
    assign(sw, frame, NIL);
  } else if ( notNil(sw->device) )
  { replaceChain(sw->device->graphicals, sw, dw);
    assign(dw, device, sw->device);
    assign(sw, device, NIL);
  }
  assign(dw, displayed, sw->displayed);

  if ( how == NAME_grow )
  { send(dw, NAME_set,
	 sub(sw->area->x, lb),
	 sub(sw->area->y, tb),
	 add(sw->area->w, add(lb, rb)),
	 add(sw->area->h, add(tb, bb)), 0);
    send(sw, NAME_set, lb, tb, 0);
  } else
  { send(sw, NAME_set,
	 lb, tb,
	 sub(sw->area->w, add(lb, rb)),
	 sub(sw->area->h, add(tb, bb)), 0);
  }

  DeviceGraphical(sw, (Device) dw);
  assign(sw, decoration, dw);

  succeed;
}


PceWindow
userWindow(PceWindow sw)
{ if ( instanceOfObject(sw, ClassWindowDecorator) )
  { WindowDecorator dw = (WindowDecorator)sw;

    answer(dw->window);
  }

  answer(sw);
}


		/********************************
		*        GRAPHICAL ROLE		*
		********************************/

status
updatePositionWindow(PceWindow sw)
{ PceWindow parent = getWindowGraphical((Graphical) sw->device);

  if ( parent && createdWindow(parent) &&
       parent->displayed == ON &&
       getIsDisplayedGraphical((Graphical)sw, (Device)parent) == ON )
  { int ox, oy, x, y, w, h;
    int pen = valInt(sw->pen);

    offsetDeviceGraphical(sw, &x, &y);
    DEBUG(NAME_offset, Cprintf("x = %d, y = %d\n", x, y));
    offset_window(parent, &ox, &oy);
    DEBUG(NAME_offset, Cprintf("ox = %d, oy = %d\n", ox, oy));
    x += valInt(sw->area->x) + ox;
    y += valInt(sw->area->y) + oy;
    w  = valInt(sw->area->w);
    h  = valInt(sw->area->h);

    if ( !createdWindow(sw) )
      TRY(send(sw, NAME_create, parent, 0));

    ws_geometry_window(sw, x, y, w, h, pen);
  } else
    uncreateWindow(sw);

  succeed;
}


static void
updatePositionSubWindowsDevice(Device dev)
{ Cell cell;

  for_cell(cell, dev->graphicals)
  { if ( instanceOfObject(cell->value, ClassWindow) )
      updatePositionWindow(cell->value);
    else if ( instanceOfObject(cell->value, ClassDevice) )
      updatePositionSubWindowsDevice(cell->value);
  }
}




static status
reparentWindow(PceWindow sw)
{ if ( !getWindowGraphical((Graphical) sw->device) )
    uncreateWindow(sw);

  succeed;
}


static status
deviceWindow(PceWindow sw, Device dev)
{ if ( notNil(dev) )
  { if ( notNil(sw->frame) )
      send(sw->frame, NAME_delete, sw, 0);

    if ( notNil(sw->decoration) && dev != (Device) sw->decoration )
      return DeviceGraphical(sw->decoration, dev);
  }

  return deviceGraphical(sw, dev);
}


static status
displayedWindow(PceWindow sw, Bool val)
{ displayedGraphical(sw, val);

  if ( notNil(sw->decoration) )
    DisplayedGraphical(sw->decoration, val);

  if ( val == ON )
    addChain(ChangedWindows, sw);

  succeed;
}


status
resizeWindow(PceWindow sw)
{ if ( notNil(sw->resize_message) )
    forwardReceiverCode(sw->resize_message, sw, sw, getSizeArea(sw->area), 0);

  succeed;
}


static status
resizeMessageWindow(PceWindow sw, Code msg)
{ assign(sw, resize_message, msg);

  if ( createdWindow(sw) )
    qadSendv(sw, NAME_resize, 0, NULL);

  succeed;
}

  

		/********************************
		*           COMPUTING		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
offset_window(sw,  x, y) computes the offset  of the coordinate system
of the  window  as a device,  relative   to the X-window's  coordinate
system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
offset_window(PceWindow sw, int *x, int *y)
{ *x  = valInt(sw->scroll_offset->x);
  *y  = valInt(sw->scroll_offset->y);
}


void
compute_window(PceWindow sw, int *x, int *y, int *w, int *h)
{ *x  = 0;
  *y  = 0;
  *w  = valInt(sw->area->w);
  *h  = valInt(sw->area->h);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
frame_offset_window(window|frame, frame *, int *x, int *y)
    Determine the frame of the object and the relative position in this
    frame.  Used for computing event-offsets.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
frame_offset_window(Any obj, FrameObj *fr, int *X, int *Y)
{ if ( instanceOfObject(obj, ClassFrame) )
  { *fr = obj;
    *X = 0; *Y = 0;
    succeed;
  } else
  { int x = 0, y = 0;
    PceWindow w = obj;

    while(isNil(w->frame))
    { if ( notNil(w->device) )
      { PceWindow w2 = DEFAULT;
	Int ox, oy;
  
	get_absolute_xy_graphical((Graphical)w, (Device *)&w2, &ox, &oy);
	if ( instanceOfObject(w2, ClassWindow) )
	{ int ox2, oy2;
  
	  offset_window(w2, &ox2, &oy2);
	  x += valInt(ox) + ox2;
	  y += valInt(oy) + oy2;
  
	  w = w2;
	  continue;
	}
      }
  
      fail;
    }
  
    x += valInt(w->area->x);
    y += valInt(w->area->y);
  
    *fr = w->frame, *X = x, *Y = y;
  
    succeed;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the offset between a window and an arbitrary other window or
frame.  Used for event-position computations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
offset_windows(PceWindow w1, Any w2, int *X, int *Y)
{ FrameObj fr1, fr2;
  int ox1, oy1, ox2, oy2;

  if ( w1 == w2 || nonObject(w1) || nonObject(w2) )
  { *X = *Y = 0;
  } else if ( frame_offset_window(w1, &fr1, &ox1, &oy1) &&
	      frame_offset_window(w2, &fr2, &ox2, &oy2) )
  { if ( fr1 == fr2 )
    { *X = ox1 - ox2;
      *Y = oy1 - oy2;
    } else
    { Area a1 = fr1->area;
      Area a2 = fr2->area;

      *X = (ox1 + valInt(a1->x)) - (ox2 + valInt(a2->x));
      *Y = (oy1 + valInt(a1->y)) - (oy2 + valInt(a2->y));
    }
  } else				/* subwindows */
  { Cprintf("offset_windows(%s, %s) ???\n", pp(w1), pp(w2));
    *X = *Y = 0;
  }
}


		/********************************
		*        EVENT HANDLING		*
		********************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Support for `diaplay->inspect_handler'.  The naming of this is a bit old
fashioned.  Checks whether there is a handler   in the chain that may be
capable of handlign the event before doing anything.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
inspectWindow(PceWindow sw, EventObj ev)
{ DisplayObj d = getDisplayGraphical((Graphical)sw);

  if ( d )
  { Cell cell;

    for_cell(cell, d->inspect_handlers)
    { Handler h = cell->value;
      
      if ( isAEvent(ev, h->event) )
	return inspectDevice((Device) sw, ev);
    }
  }

  fail;
}



static status
eventWindow(PceWindow sw, EventObj ev)
{ int rval = FAIL;
  EventObj old_event;

  if ( sw->sensitive == OFF )
    fail;

  if ( sw->current_event == ev )	/* Hack to avoid a loop */
    return eventDevice((Device)sw, ev);

  old_event = sw->current_event;
  addCodeReference(old_event);
  assign(sw, current_event, ev);

  if ( isAEvent(ev, NAME_areaEnter) )
  { FrameObj fr = getFrameWindow(sw, DEFAULT);

    if ( notNil(fr) && !getKeyboardFocusFrame(fr) )
      send(fr, NAME_inputWindow, sw, 0);
    send(sw, NAME_hasPointer, ON, 0);
  } else if ( isAEvent(ev, NAME_areaExit) )
    send(sw, NAME_hasPointer, OFF, 0);

  if ( inspectWindow(sw, ev) )
    goto out;

  if ( isDownEvent(ev) && sw->input_focus == OFF )
    send(getFrameWindow(sw, DEFAULT), NAME_keyboardFocus, sw, 0);

  if ( isAEvent(ev, NAME_keyboard) )
  { PceWindow iw;
    FrameObj fr = getFrameWindow(sw, DEFAULT);

    if ( notNil(fr) &&
	 (iw = getKeyboardFocusFrame(fr)) &&
	 iw != sw )
    { rval = eventFrame(fr, ev);
      goto out;
    }

    if ( notNil(sw->keyboard_focus) )
    { rval = postEvent(ev, sw->keyboard_focus, DEFAULT);
      goto out;
    }
  }

  if ( notNil(sw->focus) )
  { rval = postEvent(ev, sw->focus,
		     isNil(sw->focus_recogniser) ? DEFAULT
		     				 : sw->focus_recogniser);

    if ( isFreedObj(sw) )
      return rval;

    if ( isUpEvent(ev) &&
	(isDefault(sw->focus_button) ||
	 getButtonEvent(ev) == sw->focus_button) )
      focusWindow(sw, NIL, NIL, NIL, NIL);

    goto out;
  }

  rval = postEvent(ev, (Graphical) sw, DEFAULT);

  if ( rval == FAIL &&
       notNil(sw->popup) &&
       isDownEvent(ev) &&
       (rval = postEvent(ev, (Graphical) sw, popupGesture())) )
    goto out;

out:
  if ( rval == FAIL && isAEvent(ev, NAME_keyboard) )
  { if ( (rval = send(sw, NAME_typed, ev, ON, 0)) )
      goto out;
  }

  updateCursorWindow(sw);

  assign(sw, current_event, old_event);
  delCodeReference(old_event);

  return rval;
}


status
typedWindow(PceWindow sw, EventId id, Bool delegate)
{  if ( delegate == ON )
   { if ( notNil(sw->frame) )
       return send(sw->frame, NAME_typed, id, 0);
     else if ( notNil(sw->device) &&
	       (sw = getWindowGraphical((Graphical)(sw->device))) )
       return send(sw, NAME_typed, id, delegate, 0);
   }

  fail;
}

		/********************************
		*             FOCUS		*
		********************************/

status
inputFocusWindow(PceWindow sw, Bool val)
{ DEBUG(NAME_keyboard, Cprintf("inputFocusWindow(%s, %s)\n", pp(sw), pp(val)));

  if ( sw->input_focus != val )
  { assign(sw, input_focus, val);

    if ( notNil(sw->keyboard_focus) )
      generateEventGraphical(sw->keyboard_focus,
			     val == ON ? NAME_activateKeyboardFocus
			   	       : NAME_deactivateKeyboardFocus);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Should we fetch the keyboard focus  of   our  frame? For keyboard driven
operation, this appears necessary. Otherwise, I don't know.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
keyboardFocusWindow(PceWindow sw, Graphical gr)
{ if ( !isNil(gr) && sw->input_focus == OFF )
  { FrameObj fr = getFrameWindow(sw, OFF);
  
    if ( fr )
      send(fr, NAME_keyboardFocus, sw, 0);
    
  }

  if ( sw->keyboard_focus != gr )
  { if ( notNil(sw->keyboard_focus) )
      generateEventGraphical(sw->keyboard_focus, NAME_releaseKeyboardFocus);

    assign(sw, keyboard_focus, gr);

    if ( notNil(gr) )
      generateEventGraphical(gr,
			     sw->input_focus == ON ? NAME_activateKeyboardFocus
			     			   : NAME_obtainKeyboardFocus);  }

  succeed;
}


status
focusWindow(PceWindow sw, Graphical gr, Recogniser recogniser,
	    CursorObj cursor, Name button)
{ if ( isNil(gr) )
  { if ( notNil(sw->focus) )
      generateEventGraphical(sw->focus, NAME_releaseFocus);

    assign(sw, focus, NIL);
    assign(sw, focus_recogniser, NIL);
    assign(sw, focus_cursor, NIL);
    assign(sw, focus_button, NIL);
    assign(sw, focus_event, NIL);
  } else
  { if ( sw->focus != gr )
    { if ( notNil(sw->focus) )
	generateEventGraphical(sw->focus, NAME_releaseFocus);
      assign(sw, focus, gr);
      generateEventGraphical(sw->focus, NAME_obtainFocus);
    }
    assign(sw, focus_recogniser, isDefault(recogniser) ? NIL : recogniser);
    if ( notDefault(cursor) )
      assign(sw, focus_cursor, cursor);
    if ( isDefault(button) &&
    	 notNil(sw->current_event) && isDownEvent(sw->current_event) )
      assign(sw, focus_button, getButtonEvent(sw->current_event));
    else
      assign(sw, focus_button, button);
    assign(sw, focus_event, sw->current_event);
  }

  succeed;
}

		/********************************
		*           COMPUTE		*
		********************************/

static status
computeBoundingBoxWindow(PceWindow sw)
{ if ( sw->badBoundingBox == ON )
  { Cell cell;
    Area a = sw->bounding_box;
    Int ax, ay, aw, ah;

    ax = a->x; ay = a->y; aw = a->w; ah = a->h;
    clearArea(a);

    for_cell(cell, sw->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON )
	unionNormalisedArea(a, gr->area);
    }

    relativeMoveArea(a, sw->offset);

    if ( ax != a->x || ay != a->y || aw != a->w || ah != a->h )
      send(sw, NAME_changedUnion, ax, ay, aw, ah, 0);

    assign(sw, badBoundingBox, OFF);
  }

  succeed;
}


static status
computeWindow(PceWindow sw)
{ if ( notNil(sw->request_compute) )
  { computeGraphicalsDevice((Device) sw);
    computeFormatDevice((Device) sw);
    computeBoundingBoxWindow(sw);

    assign(sw, request_compute, NIL);
  }

  succeed;
}

		/********************************
		*            REDRAW		*
		********************************/


static void
union_iarea(IArea c, IArea a, IArea b)
{ if ( b->w != 0 && b->h != 0 )
  { int cx, cy, cw, ch;

    cx = min(a->x, b->x);
    cy = min(a->y, b->y);
    cw = max(a->x+a->w, b->x+b->w) - cx;
    ch = max(a->y+a->h, b->y+b->h) - cy;

    c->x = cx; c->y = cy; c->w = cw; c->h = ch;
  }
}


static status
inside_iarea(IArea a, IArea b)
{ if ( b->w != 0 && b->h != 0 )
  { if ( b->x >= a->x && b->x + b->w <= a->x + a->w &&
	 b->y >= a->y && b->y + b->h <= a->y + a->h )
      succeed;

    fail;
  }

  succeed;
}



void
changed_window(PceWindow sw, int x, int y, int w, int h, int clear)
{ UpdateArea a;
  UpdateArea best = NULL;
  iarea new;
  int na;
  int ok = 4;				/* max badness */

  NormaliseArea(x, y, w, h);
  new.x = x; new.y = y; new.w = w; new.h = h;
  na = new.w * new.h;

  for(a=sw->changes_data; a; a = a->next)
  { if ( inside_iarea(&a->area, &new) && a->clear == clear )
    { return;				/* perfect */
    } else if ( clear == a->clear )
    { iarea u;
      int ua, aa;
      int nok;

      union_iarea(&u, &a->area, &new);
      ua = u.w * u.h;
      aa = a->area.w * a->area.h;
      nok = (10 * (ua - (aa + na))) / ua;
      if ( nok < ok )
      { ok = nok;
	best = a;
      }
    }
  }

  if ( best )
  { union_iarea(&best->area, &best->area, &new);
    if ( clear )
      best->clear = clear;
  } else
  { a =	alloc(sizeof(struct update_area));

    a->area = new;
    a->clear = clear;
    a->deleted = FALSE;
    a->next = sw->changes_data;
    sw->changes_data = a;
  }
}


static void
unlink_changes_data_window(PceWindow sw)
{ UpdateArea a, b;

  a = sw->changes_data;
  sw->changes_data = NULL;

  for(; a; a = b)
  { b = a->next;
    unalloc(sizeof(struct update_area), a);
  }

  deleteChain(ChangedWindows, sw);
}



static void
combine_changes_window(PceWindow sw)
{ UpdateArea a, b;

  for(a = sw->changes_data; a; a = a->next)
  { if ( !a->deleted )
    { for(b = sw->changes_data; b; b = b->next)
      { if ( !b->deleted && b != a &&
	     inside_iarea(&a->area, &b->area) ) /* B is in A */
	  b->deleted = TRUE;
      }
    }
  }
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Redraw an area of the picture due to an exposure or resize.  The area is
given in the coordinate system of the widget realizing the picture.

__WINDOWS__ note: this function is called  both from resize/exposure (in
the X11 version) and from global changes   to the window that require it
to be repainted entirely.  In the   Windows  version, the first bypasses
this function, so we just trap the latter  to cause the entire window to
be repainted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
redrawWindow(PceWindow sw, Area a)
{
#ifdef __WINDOWS__
  ws_invalidate_window(sw, DEFAULT);
#else
  int ox, oy, dw, dh;
  int w   = valInt(sw->area->w);
  int h   = valInt(sw->area->h);
  int tmp = FALSE;
  iarea ia;

  if ( sw->displayed == OFF )
    succeed;

  compute_window(sw, &ox, &oy, &dw, &dh);
  
  if ( isDefault(a) )
  { ia.x = 0;
    ia.y = 0;
    ia.w = valInt(sw->area->w);
    ia.h = valInt(sw->area->h);
  } else
  { ia.x = valInt(a->x);
    ia.y = valInt(a->y);
    ia.w = valInt(a->w);
    ia.h = valInt(a->h);
  }

  DEBUG(NAME_redraw, Cprintf("redrawWindow: w=%d, h=%d\n", w, h));

  ox += valInt(sw->scroll_offset->x);
  oy += valInt(sw->scroll_offset->y);

  ia.x -= ox;
  ia.y -= oy;

  RedrawAreaWindow(sw, &ia, TRUE);	/* clear */

  if ( tmp )
    considerPreserveObject(a);
#endif

  succeed;
}


status
RedrawWindow(PceWindow sw)
{ DEBUG(NAME_window, Cprintf("Redrawing %s\n", pp(sw)));

  if ( sw->displayed == ON && createdWindow(sw) )
  { UpdateArea a, b;
    AnswerMark mark;

    markAnswerStack(mark);

    ComputeGraphical(sw);
    combine_changes_window(sw);

    a = sw->changes_data;
    sw->changes_data = NULL;		/* if we crash, data will be fine! */
 					/* (only some bytes lost) */


    DEBUG(NAME_changesData, Cprintf("%s:\n", pp(sw)));
    for(; a; a = b)
    { b = a->next;
      if ( !a->deleted )
      { DEBUG(NAME_changesData,
	      Cprintf("\tUpdate %d %d %d %d (%s)\n",
		      a->area.x, a->area.y, a->area.w, a->area.h,
		      a->clear ? "clear" : "no clear"));
#ifdef __WINDOWS__
        ws_redraw_window(sw, &a->area, a->clear);
#else
	RedrawAreaWindow(sw, &a->area, a->clear);
#endif
      }
      unalloc(sizeof(struct update_area), a);
    }

    rewindAnswerStack(mark, NIL);
  }

  deleteChain(ChangedWindows, sw);

  succeed;
}


status
RedrawAreaWindow(PceWindow sw, IArea a, int clear)
{ static Area oa = NULL;		/* Object Area */

  if ( sw->displayed == OFF )
    succeed;

  if ( a->w != 0 && a->h != 0 )
  { int ox, oy, dw, dh;
    AnswerMark mark;

    markAnswerStack(mark);

    if ( !oa )
    { oa = newObject(ClassArea, 0);
      protectObject(oa);
    }

    compute_window(sw, &ox, &oy, &dw, &dh);
    ox += valInt(sw->scroll_offset->x);
    oy += valInt(sw->scroll_offset->y);

    d_offset(ox, oy);
    d_window(sw, a->x, a->y, a->w, a->h, clear, TRUE);

    assign(oa, x, toInt(a->x));
    assign(oa, y, toInt(a->y));
    assign(oa, w, toInt(a->w));
    assign(oa, h, toInt(a->h));

    qadSendv(sw, NAME_RedrawArea, 1, (Any *)&oa);

    d_done();
    rewindAnswerStack(mark, NIL);
  }

  succeed;
}


static status
redrawAreaWindow(PceWindow sw, Area a)
{ Cell cell;

  for_cell(cell, sw->graphicals)
  { Graphical gr = cell->value;

    if ( gr->displayed == ON && overlapArea(a, gr->area) )
      RedrawArea(gr, a);
  }

  succeed;
}



		/********************************
		*           SCROLLING		*
		********************************/

static status
scrollWindow(PceWindow sw, Int x, Int y, Bool ax, Bool ay)
{ int ox = valInt(sw->scroll_offset->x);
  int oy = valInt(sw->scroll_offset->y);
  int nx, ny;

  if ( notDefault(x) )
  { if ( ax == ON )
      nx = -valInt(x);
    else
      nx = ox - valInt(x);
  } else
    nx = ox;
  if ( notDefault(y) )
  { if ( ay == ON )
      ny = -valInt(y);
    else
      ny = oy - valInt(y);
  } else
    ny = oy;

  if ( ox != nx || ny != oy )
  { assign(sw->scroll_offset, x, toInt(nx));
    assign(sw->scroll_offset, y, toInt(ny));

    updateScrollbarValuesWindow(sw);
    updatePositionSubWindowsDevice((Device) sw);

#ifdef __WINDOWS__
    ws_scroll_window(sw, nx-ox, ny-oy);
#else
    redrawWindow(sw, DEFAULT);
#endif
  }

  succeed;
}


static status
scrollToWindow(PceWindow sw, Point pos)
{ return scrollWindow(sw, pos->x, pos->y, ON, ON);
}


static status
normalise_window(PceWindow sw, Area a)
{ int x, y, w, h;			/* see getVisibleWindow() */
  int p = valInt(sw->pen);
  int sx = -valInt(sw->scroll_offset->x);
  int sy = -valInt(sw->scroll_offset->y);
  int nsx = sx, nsy = sy;
  int ax = valInt(a->x), ay = valInt(a->y);
  int aw = valInt(a->w), ah = valInt(a->h);
  int shift;

  NormaliseArea(ax, ay, aw, ah);
  DEBUG(NAME_normalise, Cprintf("Normalise to: %d, %d %d x %d\n",
				ax, ay, aw, ah));

  compute_window(sw, &x, &y, &w, &h);
  x -= valInt(sw->scroll_offset->x) + p;
  y -= valInt(sw->scroll_offset->y) + p;
  DEBUG(NAME_normalise, Cprintf("Visible: %d, %d %d x %d\n", x, y, w, h));

  if ( ax + aw > x + w )
  { shift = (ax + aw) - (x + w);
    nsx += shift; x += shift;
    DEBUG(NAME_normalise, Cprintf("left by %d\n", shift));
  }
  if ( ay + ah > y + h )
  { shift = (ay + ah) - (y + h);
    nsy += shift; y += shift;
    DEBUG(NAME_normalise, Cprintf("up by %d\n", shift));
  }
  if ( ax < x )
  { nsx -= x - ax;
    DEBUG(NAME_normalise, Cprintf("right by %d\n", x - ax));
  }
  if ( ay < y )
  { nsy -= y - ay;
    DEBUG(NAME_normalise, Cprintf("down by %d\n", y - ay));
  }

  if ( nsx != sx || nsy != sy )
    scrollWindow(sw,
		 nsx != sx ? toInt(nsx) : (Int) DEFAULT,
		 nsy != sy ? toInt(nsy) : (Int) DEFAULT,
		 ON, ON);

  succeed;
}


static status
normaliseWindow(PceWindow sw, Any obj)
{ if ( instanceOfObject(obj, ClassArea) )
    return normalise_window(sw, obj);

  ComputeGraphical(sw);

  if ( instanceOfObject(obj, ClassGraphical) )
  { Graphical gr = obj;
    Area a = getAbsoluteAreaGraphical(gr, (Device) sw);

    normalise_window(sw, a);
    doneObject(a);
    succeed;
  }

  assert(instanceOfObject(obj, ClassChain));
  { Chain ch = obj;
    Cell cell;
    Area a = tempObject(ClassArea, 0);
    Graphical gr;

    for_cell(cell, ch)
      if ( (gr = checkType(cell->value, TypeGraphical, NIL)) )
      { Area a2 = getAbsoluteAreaGraphical(gr, (Device) sw);

	unionNormalisedArea(a, a2);
	doneObject(a2);
      }
    
    if ( a->w != ZERO && a->h != ZERO )
      normalise_window(sw, a);
    considerPreserveObject(a);

    succeed;
  }
}


static status
scrollHorizontalWindow(PceWindow sw, Name dir, Name unit, Int amount)
{ if ( equalName(unit, NAME_file) )
  { Area bb = sw->bounding_box;

    if ( dir == NAME_goto )
    { int h = ((valInt(bb->w)-valInt(sw->area->w)) * valInt(amount)) / 1000;

      scrollWindow(sw, toInt(h + valInt(bb->x)), DEFAULT, ON, ON);
    }
  } else if ( equalName(unit, NAME_page) )
  { Area a = sw->area;
    int d = (valInt(a->w) * valInt(amount)) / 1000;

    scrollWindow(sw, toInt(dir == NAME_forwards ? d : -d), DEFAULT, OFF, ON);
  } else if ( unit == NAME_line )
  { int d = 20 * valInt(amount);

    scrollWindow(sw, toInt(dir == NAME_forwards ? d : -d), DEFAULT, OFF, ON);
  }

  succeed;
}


static status
scrollVerticalWindow(PceWindow sw, Name dir, Name unit, Int amount)
{ if ( equalName(unit, NAME_file) )
  { Area bb = sw->bounding_box;

    if ( dir == NAME_goto )
    { int h = ((valInt(bb->h)-valInt(sw->area->h)) * valInt(amount)) / 1000;
      
      scrollWindow(sw, DEFAULT, toInt(h + valInt(bb->y)), ON, ON);
    }
  } else if ( equalName(unit, NAME_page) )
  { Area a = sw->area;
    int d = (valInt(a->h) * valInt(amount)) / 1000;

    scrollWindow(sw, DEFAULT, toInt(dir == NAME_forwards ? d : -d), ON, OFF);
  } else if ( unit == NAME_line )
  { int d = 20 * valInt(amount);

    scrollWindow(sw, DEFAULT, toInt(dir == NAME_forwards ? d : -d), ON, OFF);
  }

  succeed;
}


static status
updateScrollbarValuesWindow(PceWindow sw)
{ if ( notNil(sw->decoration) )
    requestComputeScrollbarsWindowDecorator((WindowDecorator)sw->decoration);

  succeed;
}


status
changedUnionWindow(PceWindow sw, Int ox, Int oy, Int ow, Int oh)
{ return updateScrollbarValuesWindow(sw);
}


static int
view_region(int x, int w, int rx, int rw)
{ if ( rx > x )
  { w -= rx - x;
    x  = rx;
  }

  if ( x+w > rx+rw )
    w = rx+rw - x;

  return w < 0 ? 2 : w;
}
	

status					/* update bubble of scroll_bar */
bubbleScrollBarWindow(PceWindow sw, ScrollBar sb)
{ Area bb = sw->bounding_box;
  int x, y, w, h;
  int hor    = (sb->orientation == NAME_horizontal);
  int start  = valInt(hor ? bb->x : bb->y);
  int length = valInt(hor ? bb->w : bb->h);
  int view;

  compute_window(sw, &x, &y, &w, &h);
  x -= valInt(sw->scroll_offset->x);
  y -= valInt(sw->scroll_offset->y);
					/* x, y, w, h: visible area */
  
  view = view_region(start, length,
		     hor ? -valInt(sw->scroll_offset->x)
			 : -valInt(sw->scroll_offset->y),
		     hor ? w : h);
  start  = (hor ? x : y) - start;
  if ( start < 0 ) start = 0;
  if ( start > length-view ) start = length-view;

  return bubbleScrollBar(sb, toInt(length), toInt(start), toInt(view));
}



		/********************************
		*        MOVE THE POINTER	*
		********************************/

status
pointerWindow(PceWindow sw, Point pos)
{ if ( createdWindow(sw) )
  { int ox, oy;

    offset_window(sw, &ox, &oy);
    ws_move_pointer(sw, valInt(pos->x) + ox, valInt(pos->y) + oy);
  }

  succeed;
}


		/********************************
		*             CURSOR		*
		********************************/

status
focusCursorWindow(PceWindow sw, CursorObj cursor)
{ assign(sw, focus_cursor, cursor);

  return updateCursorWindow(sw);
}


static CursorObj
getDisplayedCursorWindow(PceWindow sw)
{ CursorObj rval;

  if ( notNil(sw->focus) )
  { if ( notNil(sw->focus_cursor) )
      answer(sw->focus_cursor);
    if ( notNil(sw->focus->cursor) )
      answer(sw->focus->cursor);
  }    
  
  if ( notNil(rval = getDisplayedCursorDevice((Device) sw)) )
    answer(rval);

  answer(sw->cursor);
}


status
updateCursorWindow(PceWindow sw)
{ if ( ws_created_window(sw) )
  { CursorObj cursor = getDisplayedCursorWindow(sw);

    if ( sw->displayed_cursor != cursor )
    { assign(sw, displayed_cursor, cursor);
      ws_window_cursor(sw, cursor);
    }
  }

  succeed;
}


		/********************************
		*        AREA MANAGEMENT	*
		********************************/

status
geometryWindow(PceWindow sw, Int X, Int Y, Int W, Int H)
{ CHANGING_GRAPHICAL(sw,
		     { setArea(sw->area, X, Y, W, H);
		       if ( valInt(sw->area->w) <= 0 )
			 assign(sw->area, w, ONE);
		       if ( valInt(sw->area->h) <= 0 )
			 assign(sw->area, h, ONE);
		     });

  if ( notNil(sw->frame) && ws_created_window(sw) )
  { int x, y, w, h;
    int pen = valInt(sw->pen);

    x = valInt(sw->area->x);
    y = valInt(sw->area->y);
    w = valInt(sw->area->w);
    h = valInt(sw->area->h);

    ws_geometry_window(sw, x, y, w, h, pen);
  }

  succeed;
}


status
requestGeometryWindow(PceWindow sw, Int X, Int Y, Int W, Int H)
{ if ( notNil(sw->tile) )
  { int p = valInt(sw->pen);
    Int ww, wh;

    ww = (isDefault(W) ? (Int) DEFAULT : toInt(valInt(W) + 2*p));
    wh = (isDefault(H) ? (Int) DEFAULT : toInt(valInt(H) + 2*p));

    setTile(sw->tile, DEFAULT, DEFAULT, ww, wh);

    if ( notNil(sw->frame) && createdFrame(sw->frame) )
    { if ( !parentGoal(VmiSend, sw->frame, NAME_fit) ) /* avoid loop */
	send(sw->frame, NAME_fit, 0);
    }

    succeed;
  } else if ( notNil(sw->decoration) )
  { return send(sw->decoration, NAME_requestGeometry, X, Y, W, H, 0);
  } else
    return geometryWindow(sw, X, Y, W, H);
}


status					/* position on display */
get_display_position_window(PceWindow sw, int *X, int *Y)
{ int x, y;
  FrameObj fr;

  TRY(frame_offset_window(sw, &fr, &x, &y));
  x += valInt(fr->area->x);
  y += valInt(fr->area->y);
  
  *X = x; *Y = y;

  succeed;
}


static Area
getVisibleWindow(PceWindow sw)
{ int x, y, w, h; 
  int p = valInt(sw->pen);

  compute_window(sw, &x, &y, &w, &h);
  x -= valInt(sw->scroll_offset->x) + p;
  y -= valInt(sw->scroll_offset->y) + p;

  answer(answerObject(ClassArea, toInt(x), toInt(y), toInt(w), toInt(h), 0));
}


static Area
getBoundingBoxWindow(PceWindow w)
{ ComputeGraphical((Graphical) w);

  answer(w->bounding_box);
}


		/********************************
		*         LINK TO FRAME		*
		********************************/

static status
tileWindow(PceWindow sw, TileObj tile)
{ if ( isDefault(tile) )
  { if ( isNil(sw->tile) )
      assign(sw, tile, newObject(ClassTile, sw, 0));
  } else
    assign(sw, tile, tile);		/* TBD: check */

  succeed;
}


PceWindow
getUserWindow(PceWindow sw)
{ PceWindow w;

  if ( instanceOfObject(sw, ClassWindowDecorator) &&
       notNil(w = ((WindowDecorator)sw)->window) )
    answer(w);

  answer(sw);
}


static status
for_all_tile(TileObj tile, SendFunc f, Any arg)
{ if ( isNil(tile->members) )
    return (*f)(tile->object, arg);
  else
  { TileObj st;

    for_chain(tile->members, st,
	      TRY(for_all_tile(st, f, arg)));
    
    succeed;
  }
}


static status
frame_window(PceWindow sw, FrameObj frame)
{ if ( notNil(sw->decoration) )
    sw = sw->decoration;

  if ( sw->frame != frame )
  { DEBUG(NAME_frame, Cprintf("Making %s part of %s\n", pp(sw), pp(frame)));

    addCodeReference(sw);
    if ( notNil(sw->frame) )
      DeleteFrame(sw->frame, sw);
    assign(sw, frame, frame);
    if ( notNil(sw->frame) )
      AppendFrame(sw->frame, sw);
    delCodeReference(sw);
  }

  succeed;
}


status
frameWindow(PceWindow sw, FrameObj frame)
{ if ( notNil(sw->decoration) )
    sw = sw->decoration;

  if ( isDefault(frame) )
  { if ( isNil(sw->frame) )
      frame = newObject(ClassFrame, 0);
    else
      succeed;
  }

  tileWindow(sw, DEFAULT);
  for_all_tile(getRootTile(sw->tile), frame_window, frame);
  if ( frame->status == NAME_open )
    DisplayedGraphical(sw, ON);

  succeed;
}


TileObj
getTileWindow(PceWindow sw)
{ while( notNil(sw->decoration) )
    sw = sw->decoration;

  tileWindow(sw, DEFAULT);

  answer(sw->tile);
}


FrameObj
getFrameWindow(PceWindow sw, Bool create)
{ PceWindow root = (PceWindow) getRootGraphical((Graphical) sw);
  
  if ( instanceOfObject(root, ClassWindow) )
  { if ( create != OFF )
      frameWindow(root, DEFAULT);
    answer(root->frame);
  }    

  fail;
}


static status
mergeFramesWindow(PceWindow w1, PceWindow w2)
{ FrameObj fr1, fr2;

  if ( isNil(w1->frame) && isNil(w2->frame) )
  { /* frameWindow(w2, DEFAULT);
       frameWindow(w1, w2->frame);
    */
  } else if ( notNil(w1->frame) && notNil(w2->frame) )
  { if ( (fr1=w1->frame) != (fr2=w2->frame) )
    { Cell cell, c2;

      addCodeReference(fr1);
      for_cell_save(cell, c2, fr1->members)
	frame_window(cell->value, fr2);
      delCodeReference(fr1);
      freeableObj(fr1);
    }
  } else if ( notNil(w1->frame) )
    frameWindow(w2, w1->frame);
  else
    frameWindow(w1, w2->frame);
  
  succeed;
}


static status
relateWindow(PceWindow sw, Name how, Any to)
{ PceWindow w2 = instanceOfObject(to, ClassWindow) ? to : NIL;

  if ( notNil(sw->decoration) )
    return relateWindow(sw->decoration, how, to);
  if ( notNil(w2) && notNil(w2->decoration) )
    return relateWindow(sw, how, w2->decoration);
  
  DeviceGraphical((Graphical)sw, NIL);
  if ( notNil(w2) )
  { DeviceGraphical((Graphical)w2, NIL);
    tileWindow(w2, DEFAULT);
  }

  if ( createdWindow(sw) && notNil(sw->frame) )
    send(sw->frame, NAME_delete, sw, 0);
  
  tileWindow(sw, DEFAULT);

  if ( notNil(w2) )
  { TRY(send(sw->tile, how, w2->tile, 0));
  } else
  { TileObj t2 = to;

    TRY(send(sw->tile, how, t2, OFF, 0));
    while( isNil(t2->object) )
    { t2 = getHeadChain(t2->members);
      assert(t2);
    }

    w2 = t2->object;
  }
  
  return mergeFramesWindow(sw, w2);
}


static status
leftWindow(PceWindow w1, Any w2)
{ return relateWindow(w1, NAME_left, w2);
}


static status
rightWindow(PceWindow w1, Any w2)
{ return relateWindow(w1, NAME_right, w2);
}


static status
aboveWindow(PceWindow w1, Any w2)
{ return relateWindow(w1, NAME_above, w2);
}


static status
belowWindow(PceWindow w1, Any w2)
{ return relateWindow(w1, NAME_below, w2);
}


		/********************************
		*          ATTRIBUTES		*
		********************************/


static status
penWindow(PceWindow sw, Int pen)
{ if ( sw->pen != pen )
  { assign(sw, pen, pen);

    if ( ws_created_window(sw) )
    { int x, y, w, h;
      int pen = valInt(sw->pen);
      
      x = valInt(sw->area->x);
      y = valInt(sw->area->y);
      w = valInt(sw->area->w);
      h = valInt(sw->area->h);

      ws_geometry_window(sw, x, y, w, h, pen);
    }
  }

  succeed;
}


static status
colourWindow(PceWindow sw, Colour colour)
{ if ( isDefault(colour) && notNil(sw->frame) )
    colour = sw->frame->display->foreground;

  if ( sw->colour != colour )
  { assign(sw, colour, colour);
    redrawWindow(sw, DEFAULT);
  }

  succeed;
}


static status
backgroundWindow(PceWindow sw, Colour colour)
{ if ( isDefault(colour) && notNil(sw->frame) )
    colour = sw->frame->display->background;

  if ( sw->background != colour )
  { assign(sw, background, colour);
    ws_window_background(sw, colour);
    redrawWindow(sw, DEFAULT);
  }

  succeed;
}


static status
selectionFeedbackWindow(PceWindow sw, Any feedback)
{ if ( isDefault(feedback) )
    TRY(feedback = getResourceValueObject(sw, NAME_selectionFeedback));

  if ( feedback != sw->selection_feedback )
  { assign(sw, selection_feedback, feedback);
    redrawWindow(sw, DEFAULT);
  }

  succeed;
}


static Colour
getForegroundWindow(PceWindow sw)
{ answer(sw->colour);
}


		/********************************
		*            FLUSHING		*
		********************************/

status
flushWindow(PceWindow sw)
{ DisplayObj d = getDisplayGraphical((Graphical) sw);

  if ( d )
  { RedrawWindow(sw);
    ws_flush_display(d);
  }

  succeed;
}


		/********************************
		*             ALERT		*
		********************************/

status
flashWindow(PceWindow sw, Area a, Int time)
{ if ( sw->displayed == ON )
  { int t;
    
    if ( isDefault(time) )
      time = getResourceValueObject(sw, NAME_visualBellDuration);
    t = (isInteger(time) ? valInt(time) : 250);

    if ( isDefault(a) )
      ws_flash_window(sw, t);
    else
    { int x, y, w, h;

      x = valInt(a->x);
      y = valInt(a->y);
      w = valInt(a->w);
      h = valInt(a->h);
      NormaliseArea(x, y, w, h);

      ws_flash_area_window(sw, x, y, w, h, t);
    }
  }

  succeed;
}


		/********************************
		*           HIDE/EXPOSE		*
		********************************/

static status
exposeWindow(PceWindow sw)
{ if ( notNil(sw->decoration) )
    return exposeWindow(sw->decoration);

  if ( notNil(sw->frame) )
    return exposeFrame(sw->frame);

  ws_raise_window(sw); 

  succeed;
}


static status
hideWindow(PceWindow sw)
{ if ( notNil(sw->decoration) )
    return hideWindow(sw->decoration);

  if ( notNil(sw->frame) )
    return hideFrame(sw->frame);

  ws_lower_window(sw);

  succeed;
}


		/********************************
		*              VISUAL		*
		********************************/

static Any
getContainedInWindow(PceWindow sw)
{ if ( notNil(sw->frame) )  answer(sw->frame);
  if ( notNil(sw->device) ) answer(sw->device);

  fail;
}


static status
resetWindow(PceWindow sw)
{ assign(sw, current_event, NIL);
  focusWindow(sw, NIL, NIL, NIL, NIL);
  updateCursorWindow(sw);

  return resetVisual((VisualObj) sw);
}


static status
catchAllWindowv(PceWindow sw, Name selector, int argc, Any *argv)
{ if ( getSendMethodClass(ClassWindowDecorator, selector) )
  { newObject(ClassWindowDecorator, sw, 0);

    if ( notNil(sw->decoration) )
    { assign(PCE, last_error, NIL);
      return sendv(sw->decoration, selector, argc, argv);
    }
  }

  if ( getSendMethodClass(ClassFrame, selector) )
  { FrameObj fr = getFrameWindow(sw, DEFAULT);
    assign(PCE, last_error, NIL);
    return sendv(fr, selector, argc, argv);
  }	

  if ( getSendMethodClass(ClassTile, selector) )
  { if ( notNil(sw->decoration) )
      return catchAllWindowv(sw->decoration, selector, argc, argv);

    tileWindow(sw, DEFAULT);
    assign(PCE, last_error, NIL);
    return sendv(sw->tile, selector, argc, argv);
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_open[] =
        { "[point]", "normalise=[bool]" };
static char *T_LforwardsObackwardsOgotoL_LpageOfileOlineL_int[] =
        { "{forwards,backwards,goto}", "{page,file,line}", "int" };
static char *T_decorate[] =
        { "area=[{grow,shrink}]", "left_margin=[int]", "right_margin=[int]", "top_margin=[int]", "bottom_margin=[int]", "decorator=[window]" };
static char *T_confirmCentered[] =
        { "center=[point]", "grab=[bool]" };
static char *T_typed[] =
        { "event_id", "delegate=[bool]" };
static char *T_focus[] =
        { "graphical*", "[recogniser]*", "[cursor]*", "[name]*" };
static char *T_initialise[] =
        { "label=[name]", "size=[size]", "display=[display]" };
static char *T_catchAll[] =
        { "name", "unchecked ..." };
static char *T_changedUnion[] =
        { "ox=int", "oy=int", "ow=int", "oh=int" };
static char *T_confirm[] =
        { "position=[point]", "grab=[bool]", "normalise=[bool]" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_flash[] =
	{ "area=[area]", "time=[int]" };

/* Instance Variables */

static vardecl var_window[] =
{ IV(NAME_frame, "frame*", IV_NONE,
     NAME_organisation, "Frame the window is member of"),
  IV(NAME_decoration, "window_decorator*", IV_GET,
     NAME_appearance, "Window displaying me and my decorations"),
  IV(NAME_boundingBox, "area", IV_NONE,
     NAME_area, "Union of graphicals"),
  IV(NAME_tile, "tile*", IV_NONE,
     NAME_layout, "Tile that manages my area"),
  SV(NAME_resizeMessage, "code*", IV_GET|IV_STORE, resizeMessageWindow,
     NAME_resize, "Executed after window has resized"),
  IV(NAME_displayedCursor, "cursor*", IV_NONE,
     NAME_internal, "Currently displayed cursor"),
  SV(NAME_inputFocus, "bool", IV_GET|IV_STORE, inputFocusWindow,
     NAME_focus, "Window has input focus"),
  SV(NAME_keyboardFocus, "graphical*", IV_GET|IV_STORE, keyboardFocusWindow,
     NAME_focus, "Graphical in focus of keyboard events"),
  IV(NAME_focus, "graphical*", IV_GET,
     NAME_focus, "Graphical in focus"),
  IV(NAME_focusRecogniser, "recogniser*", IV_GET,
     NAME_focus, "Recogniser in focus"),
  SV(NAME_focusCursor, "cursor*", IV_GET|IV_STORE, focusCursorWindow,
     NAME_cursor, "Cursor while there is a focus"),
  IV(NAME_focusButton, "[button_name]*", IV_GET,
     NAME_focus, "Button that should terminate focus"),
  IV(NAME_focusEvent, "event*", IV_GET,
     NAME_focus, "<-current_event when ->focus was set"),
  IV(NAME_scrollOffset, "point", IV_NONE,
     NAME_internal, "How much the window is scrolled"),
  IV(NAME_popup, "popup*", IV_BOTH,
     NAME_menu, "Popup-menu of the window"),
  IV(NAME_currentEvent, "event*", IV_GET,
     NAME_event, "Event being processed now"),
  IV(NAME_sensitive, "bool", IV_BOTH,
     NAME_event, "Window accepts events"),
  SV(NAME_background, "colour|pixmap", IV_GET|IV_STORE, backgroundWindow,
     NAME_appearance, "Background colour or pattern"),
  IV(NAME_hasPointer, "bool", IV_BOTH,
     NAME_event, "If @on, pointer (mouse) is in window"),
  SV(NAME_selectionFeedback, "{invert,handles,colour}|elevation|colour*",
     IV_GET|IV_STORE, selectionFeedbackWindow,
     NAME_appearance, "How <-selected graphicals are visualised"),
  IV(NAME_changesData, "alien:UpdateArea", IV_NONE,
     NAME_repaint, "Summary info for redraw"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_windowSystem, "Window-System reference")
};

/* Send Methods */

static senddecl send_window[] =
{ SM(NAME_destroy, 0, NULL, destroyWindow,
     DEFAULT, "->destroy associated frame"),
  SM(NAME_device, 1, "device*", deviceWindow,
     DEFAULT, "Display window on device, take care of <-decoration"),
  SM(NAME_displayed, 1, "bool", displayedWindow,
     DEFAULT, "(Un)display window, take care of <-decoration"),
  SM(NAME_flush, 0, NULL, flushWindow,
     DEFAULT, "Update graphicals in this window immediately"),
  SM(NAME_free, 0, NULL, freeWindow,
     DEFAULT, "->free associated frame"),
  SM(NAME_geometry, 4, T_geometry, geometryWindow,
     DEFAULT, "Resize window inside its frame"),
  SM(NAME_initialise, 3, T_initialise, initialiseWindow,
     DEFAULT, "Create from label, size and display"),
  SM(NAME_move, 1, "point", positionGraphical,
     DEFAULT, "Move origin to argument"),
  SM(NAME_reparent, 0, NULL, reparentWindow,
     DEFAULT, "If no longer related to the window, ->uncreate"),
  SM(NAME_requestGeometry, 4, T_geometry, requestGeometryWindow,
     DEFAULT, "Resize window inside its frame"),
  SM(NAME_reset, 0, NULL, resetWindow,
     DEFAULT, "Reset window after an abort"),
  SM(NAME_unlink, 0, NULL, unlinkWindow,
     DEFAULT, "Destroy related X-resources"),
  SM(NAME_x, 1, "int", xGraphical,
     DEFAULT, "Move graphical horizontally"),
  SM(NAME_y, 1, "int", yGraphical,
     DEFAULT, "Move graphical vertically"),
  SM(NAME_colour, 1, "[colour|pixmap]", colourWindow,
     DEFAULT, "Default colour of graphicals"),
  SM(NAME_pen, 1, "0..", penWindow,
     DEFAULT, "Thickness of line around window"),
  SM(NAME_position, 1, "point", positionGraphical,
     DEFAULT, "Position in <-frame"),
  SM(NAME_typed, 2, T_typed, typedWindow,
     NAME_accelerator, "Handle accelerator (delegate to <-frame)"),
  SM(NAME_decorate, 6, T_decorate, decorateWindow,
     NAME_appearance, "Embed window for scrollbars, etc."),
  SM(NAME_foreground, 1, "colour", colourWindow,
     NAME_appearance, "Set foreground colour"),
  SM(NAME_resize, 0, NULL, resizeWindow,
     NAME_area, "Execute <-resize_message"),
  SM(NAME_catchAll, 2, T_catchAll, catchAllWindowv,
     NAME_delegate, "Handle frame methods when no frame is present"),
  SM(NAME_event, 1, "event", eventWindow,
     NAME_event, "Handle event"),
  SM(NAME_grabKeyboard, 1, "bool", grabKeyboardWindow,
     NAME_event, "Grab keyboard events"),
  SM(NAME_grabPointer, 1, "bool", grabPointerWindow,
     NAME_event, "Grab pointer (mouse) events"),
  SM(NAME_focus, 4, T_focus, focusWindow,
     NAME_focus, "Forward events to graphical"),
  SM(NAME_ComputeDesiredSize, 0, NULL, ComputeDesiredSizeWindow,
     NAME_layout, "Compute the desired size (no-op)"),
  SM(NAME_above, 1, "window|tile", aboveWindow,
     NAME_layout, "Put me above argument"),
  SM(NAME_below, 1, "window|tile", belowWindow,
     NAME_layout, "Put me below argument"),
  SM(NAME_left, 1, "window|tile", leftWindow,
     NAME_layout, "Put me left of argument"),
  SM(NAME_right, 1, "window|tile", rightWindow,
     NAME_layout, "Put me right of argument"),
  SM(NAME_create, 1, "[window]", createWindow,
     NAME_open, "Create associated X-window structure"),
  SM(NAME_open, 2, T_open, openWindow,
     NAME_open, "Open associated frame on the display"),
  SM(NAME_openCentered, 1, "[point]", openCenteredWindow,
     NAME_open, "Open frame centered around point"),
  SM(NAME_uncreate, 0, NULL, uncreateWindow,
     NAME_open, "Destroy associated X-window structure"),
  SM(NAME_pointer, 1, "point", pointerWindow,
     NAME_pointer, "Move the pointer relative to window"),
  SM(NAME_redraw, 1, "[area]", redrawWindow,
     NAME_repaint, "Redraw (area of) the window"),
  SM(NAME_flash, 2, T_flash, flashWindow,
     NAME_report, "Flash (part of) the window"),
  SM(NAME_bubbleScrollBar, 1, "scroll_bar", bubbleScrollBarWindow,
     NAME_scroll, "Update bubble of given scroll_bar object"),
  SM(NAME_changedUnion, 4, T_changedUnion, changedUnionWindow,
     NAME_scroll, "Request scroll_bar update"),
  SM(NAME_normalise, 1, "area|graphical|chain", normaliseWindow,
     NAME_scroll, "Ensure area|graphical|chain is visible"),
  SM(NAME_scrollHorizontal, 3, T_LforwardsObackwardsOgotoL_LpageOfileOlineL_int, scrollHorizontalWindow,
     NAME_scroll, "Trap message from horizontal scrollbar"),
  SM(NAME_scrollTo, 1, "point", scrollToWindow,
     NAME_scroll, "Make point top-left of window"),
  SM(NAME_scrollVertical, 3, T_LforwardsObackwardsOgotoL_LpageOfileOlineL_int, scrollVerticalWindow,
     NAME_scroll, "Trap message from vertical scrollbar"),
  SM(NAME_expose, 0, NULL, exposeWindow,
     NAME_stacking, "Expose (raise) related frame"),
  SM(NAME_hide, 0, NULL, hideWindow,
     NAME_stacking, "Hide (lower) related frame"),
  SM(NAME_compute, 0, NULL, computeWindow,
     NAME_update, "Recompute window")
};

/* Get Methods */

static getdecl get_window[] =
{ GM(NAME_containedIn, 0, "frame|device", NULL, getContainedInWindow,
     DEFAULT, "Frame/graphical device I'm contained in"),
  GM(NAME_convert, 1, "window", "graphical", getConvertWindow,
     DEFAULT, "Return graphical's <-window"),
  GM(NAME_frame, 1, "frame", "create=[bool]", getFrameWindow,
     DEFAULT, "Frame of window (create if not there)"),
  GM(NAME_tile, 0, "tile", NULL, getTileWindow,
     DEFAULT, "Tile of window (create if not there)"),
  GM(NAME_foreground, 0, "colour", NULL, getForegroundWindow,
     NAME_appearance, "Get foreground colour"),
  GM(NAME_boundingBox, 0, "area", NULL, getBoundingBoxWindow,
     NAME_area, "Union of graphicals"),
  GM(NAME_visible, 0, "area", NULL, getVisibleWindow,
     NAME_area, "New area representing visible part"),
  GM(NAME_displayedCursor, 0, "cursor*", NULL, getDisplayedCursorDevice,
     NAME_cursor, "Currently displayed cursor"),
  GM(NAME_confirm, 3, "any", T_confirm, getConfirmWindow,
     NAME_modal, "Run sub event-loop until ->return"),
  GM(NAME_confirmCentered, 2, "any", T_confirmCentered, getConfirmCenteredWindow,
     NAME_modal, "->confirm with frame centered around point")
};

/* Resources */

static resourcedecl rc_window[] =
{ RC(NAME_background, "colour|pixmap", "white",
     "Colour/fill pattern of the background"),
  RC(NAME_cursor, "cursor", "top_left_arrow",
     "Default window cursor"),
  RC(NAME_pen, "int", "1",
     "Thickness of outside line"),
  RC(NAME_selectionFeedback, NULL, "handles",
     NULL),
  RC(NAME_selectionHandles, RC_REFINE, "@nil",
     NULL),
  RC(NAME_size, "size", "size(200,100)",
     "Default size (pixels)")
};

/* Class Declaration */

static Name window_termnames[] = { NAME_label, NAME_size, NAME_display };

ClassDecl(window_decls,
          var_window, send_window, get_window, rc_window,
          3, window_termnames,
          "$Rev$");


status
makeClassWindow(Class class)
{ declareClass(class, &window_decls);
  setLoadStoreFunctionClass(class, loadWindow, storeWindow);

  delegateClass(class, NAME_frame);
  delegateClass(class, NAME_tile);
  delegateClass(class, NAME_decoration); /* label, scrollbars */
  cloneStyleClass(class, NAME_none);
  saveStyleVariableClass(class, NAME_device, NAME_normal);
  saveStyleVariableClass(class, NAME_currentEvent, NAME_nil);
  saveStyleVariableClass(class, NAME_focusEvent, NAME_nil);
  setRedrawFunctionClass(class, redrawAreaWindow);

  WindowTable = createHashTable(toInt(32), OFF);

  succeed;
}

