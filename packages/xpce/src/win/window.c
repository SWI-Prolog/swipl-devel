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
{ TRY( send(sw, NAME_create, 0) );
  TRY( send(getFrameWindow(sw), NAME_open, pos, DEFAULT, normalise, 0) );
  
  succeed;
}


static status
openCenteredWindow(PceWindow sw, Point pos)
{ TRY( send(sw, NAME_create, 0) );
  TRY( send(getFrameWindow(sw), NAME_openCentered, pos, 0) );

  succeed;
}


static Any
getConfirmWindow(PceWindow sw, Point pos, Bool grab, Bool normalise)
{ TRY( send(sw, NAME_create, 0) );

  answer(getConfirmFrame(getFrameWindow(sw), pos, grab, normalise));
}


static Any
getConfirmCenteredWindow(PceWindow sw, Point pos, Bool grab)
{ TRY( send(sw, NAME_create, 0) );

  answer(getConfirmCenteredFrame(getFrameWindow(sw), pos, grab));
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

  if ( notNil(sw->frame) )
  { replaceChain(sw->frame->members, sw, dw);
    assign(dw, frame, sw->frame);
    assign(sw, frame, NIL);
    assign(dw, tile, sw->tile);
    assign(dw->tile, object, dw);
    assign(sw, tile, NIL);
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


		/********************************
		*        GRAPHICAL ROLE		*
		********************************/

static status
RedrawAreaWindowAsGraphical(PceWindow sw, Area a)
{ if ( !createdWindow(sw) )
    updatePositionWindow(sw);

  return RedrawAreaGraphical(sw, a);
}


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


status
frame_offset_window(PceWindow w, FrameObj *fr, int *X, int *Y)
{ int x = 0, y = 0;

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


void
offset_windows(PceWindow w1, PceWindow w2, int *X, int *Y)
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
  { FrameObj fr = getFrameWindow(sw);

    if ( notNil(fr) && !getKeyboardFocusFrame(fr) )
      send(fr, NAME_inputWindow, sw, 0);
    send(sw, NAME_hasPointer, ON, 0);
  } else if ( isAEvent(ev, NAME_areaExit) )
    send(sw, NAME_hasPointer, OFF, 0);

  if ( !emptyChain(getDisplayGraphical((Graphical)sw)->inspect_handlers) &&
       inspectDevice((Device) sw, ev) )
    goto out;

  if ( isAEvent(ev, NAME_keyboard) )
  { PceWindow iw;
    FrameObj fr = getFrameWindow(sw);

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


static status
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
{ if ( sw->input_focus != val )
  { assign(sw, input_focus, val);

    if ( notNil(sw->keyboard_focus) )
      generateEventGraphical(sw->keyboard_focus,
			     val == ON ? NAME_activateKeyboardFocus
			   	       : NAME_deactivateKeyboardFocus);
  }

  succeed;
}


status
keyboardFocusWindow(PceWindow sw, Graphical gr)
{ if ( sw->keyboard_focus != gr )
  { if ( notNil(sw->keyboard_focus) )
      generateEventGraphical(sw->keyboard_focus, NAME_releaseKeyboardFocus);

    assign(sw, keyboard_focus, gr);

    if ( notNil(gr) )
      generateEventGraphical(gr,
			     sw->input_focus == ON ? NAME_activateKeyboardFocus
			     			   : NAME_obtainKeyboardFocus);
  }

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
  struct iarea new;
  int na;
  int ok = 4;				/* max badness */

  NormaliseArea(x, y, w, h);
  new.x = x; new.y = y; new.w = w; new.h = h;
  na = new.w * new.h;

  for(a=sw->changes_data; a; a = a->next)
  { if ( inside_iarea(&a->area, &new ) )
    { return;				/* perfect */
    } else if ( clear == a->clear )
    { struct iarea u;
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
  struct iarea ia;

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
  { Cell cell;
    int ox, oy, dw, dh;
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

    for_cell(cell, sw->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON && overlapArea(oa, gr->area) )
	RedrawArea(gr, oa);
    }

    d_done();
    rewindAnswerStack(mark, NIL);
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
getFindCursorWindow(PceWindow sw)
{ CursorObj rval;

  if ( notNil(sw->focus) )
  { if ( notNil(sw->focus_cursor) )
      answer(sw->focus_cursor);
    if ( notNil(sw->focus->cursor) )
      answer(sw->focus->cursor);
  }    
  
  if ( notNil(rval = getFindCursorDevice((Device) sw)) )
    answer(rval);

  answer(sw->cursor);
}


status
updateCursorWindow(PceWindow sw)
{ if ( ws_created_window(sw) )
  { CursorObj cursor = getFindCursorWindow(sw);

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
getFrameWindow(PceWindow sw)
{ PceWindow root = (PceWindow) getRootGraphical((Graphical) sw);
  
  if ( instanceOfObject(root, ClassWindow) )
  { frameWindow(root, DEFAULT);
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
relateWindow(PceWindow sw, Name how, PceWindow w2)
{ if ( notNil(sw->decoration) )
    return relateWindow(sw->decoration, how, w2);
  if ( notNil(w2->decoration) )
    return relateWindow(sw, how, w2->decoration);
  
  DeviceGraphical((Graphical)sw, NIL);
  DeviceGraphical((Graphical)w2, NIL);

  if ( createdWindow(sw) && notNil(sw->frame) )
    send(sw->frame, NAME_delete, sw, 0);
  
  tileWindow(sw, DEFAULT);
  tileWindow(w2, DEFAULT);

  TRY(send(sw->tile, how, w2->tile, 0));
  
  return mergeFramesWindow(sw, w2);
}


static status
leftWindow(PceWindow w1, PceWindow w2)
{ return relateWindow(w1, NAME_left, w2);
}


static status
rightWindow(PceWindow w1, PceWindow w2)
{ return relateWindow(w1, NAME_right, w2);
}


static status
aboveWindow(PceWindow w1, PceWindow w2)
{ return relateWindow(w1, NAME_above, w2);
}


static status
belowWindow(PceWindow w1, PceWindow w2)
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

static status
flashWindow(PceWindow sw)
{ if ( sw->displayed == ON )
  { int w = valInt(sw->area->w);
    int h = valInt(sw->area->h);

    d_offset(0, 0);
    d_window(sw, 0, 0, w, h, FALSE, FALSE);

    r_complement(0, 0, w, h);
    d_flush();
    msleep(valInt(getResourceValueObject(sw, NAME_visualBellDuration)));
    r_complement(0, 0, w, h);
    d_flush();

    d_done();
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
  { FrameObj fr = getFrameWindow(sw);
    assign(PCE, last_error, NIL);
    return sendv(fr, selector, argc, argv);
  }	

  if ( getSendMethodClass(ClassTile, selector) )
  { tileWindow(sw, DEFAULT);
    assign(PCE, last_error, NIL);
    return sendv(sw->tile, selector, argc, argv);
  }

  fail;
}


status
makeClassWindow(Class class)
{ sourceClass(class, makeClassWindow, __FILE__, "$Revision$");

  setLoadStoreFunctionClass(class, loadWindow, storeWindow);

  localClass(class, NAME_frame, NAME_organisation, "frame*", NAME_none,
	     "Frame the window is member of");
  localClass(class, NAME_decoration, NAME_appearance, "window_decorator*",
	     NAME_get,
	     "Window displaying me and my decorations");
  localClass(class, NAME_boundingBox, NAME_area, "area", NAME_none,
	     "Union of graphicals");
  localClass(class, NAME_tile, NAME_layout, "tile*", NAME_none,
	     "Tile that manages my area");
  localClass(class, NAME_resizeMessage, NAME_resize, "code*", NAME_get,
	     "Executed after window has resized");
  localClass(class, NAME_displayedCursor, NAME_internal, "cursor*", NAME_none,
	     "Currently displayed cursor");
  localClass(class, NAME_inputFocus, NAME_focus, "bool", NAME_get,
	     "Window has input focus");
  localClass(class, NAME_keyboardFocus, NAME_focus, "graphical*", NAME_get,
	     "Graphical in focus of keyboard events");
  localClass(class, NAME_focus, NAME_focus, "graphical*", NAME_get,
	     "Graphical in focus");
  localClass(class, NAME_focusRecogniser, NAME_focus, "recogniser*", NAME_get,
	     "Recogniser in focus");
  localClass(class, NAME_focusCursor, NAME_cursor, "cursor*", NAME_get,
	     "Cursor while there is a focus");
  localClass(class, NAME_focusButton, NAME_focus, "[button_name]*", NAME_get,
	     "Button that should terminate focus");
  localClass(class, NAME_focusEvent, NAME_focus, "event*", NAME_get,
	     "<-current_event when ->focus was set");
  localClass(class, NAME_scrollOffset, NAME_internal, "point", NAME_none,
	     "How much the window is scrolled");
  localClass(class, NAME_popup, NAME_menu, "popup*", NAME_both,
	     "Popup-menu of the window");
  localClass(class, NAME_currentEvent, NAME_event, "event*", NAME_get,
	     "Event beeing processed now");
  localClass(class, NAME_sensitive, NAME_event, "bool", NAME_both,
	     "Window accepts events");
  localClass(class, NAME_background, NAME_appearance, "colour|pixmap",NAME_get,
	     "Background colour or pattern");
  localClass(class, NAME_hasPointer, NAME_event, "bool", NAME_both,
	     "If @on, pointer (mouse) is in window");
  localClass(class, NAME_selectionFeedback, NAME_appearance,
	     "{invert,handles}|elevation|colour*", NAME_get,
	     "How <-selected graphicals are visualised");
  localClass(class, NAME_changesData, NAME_repaint, "alien:UpdateArea",
	     NAME_none, "Summary info for redraw");
  localClass(class, NAME_wsRef, NAME_windowSystem, "alien:WsRef", NAME_none,
	     "Window-System reference");

  termClass(class, "window", 3, NAME_label, NAME_size, NAME_display);
  delegateClass(class, NAME_frame);
  delegateClass(class, NAME_tile);
  delegateClass(class, NAME_decoration); /* label, scrollbars */
  cloneStyleClass(class, NAME_none);
  saveStyleVariableClass(class, NAME_currentEvent, NAME_nil);
  saveStyleVariableClass(class, NAME_focusEvent, NAME_nil);
  setRedrawFunctionClass(class, RedrawAreaWindowAsGraphical);

  storeMethod(class, NAME_resizeMessage, resizeMessageWindow);
  storeMethod(class, NAME_pen, penWindow);
  storeMethod(class, NAME_colour, colourWindow);
  storeMethod(class, NAME_background, backgroundWindow);
  storeMethod(class, NAME_keyboardFocus, keyboardFocusWindow);
  storeMethod(class, NAME_focusCursor, focusCursorWindow);
  storeMethod(class, NAME_selectionFeedback, selectionFeedbackWindow);
  storeMethod(class, NAME_inputFocus, inputFocusWindow);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "label=[name]", "size=[size]", "display=[display]",
	     "Create from label, size and display",
	     initialiseWindow);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Destroy related X-resources",
	     unlinkWindow);
  sendMethod(class, NAME_catchAll, NAME_delegate, 2, "name", "unchecked ...",
	     "Handle frame methods when no frame is present",
	     catchAllWindowv);
  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Resize window inside its frame",
	     geometryWindow);
  sendMethod(class, NAME_requestGeometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Resize window inside its frame",
	     requestGeometryWindow);
  sendMethod(class, NAME_resize, NAME_area, 0,
	     "Execute <-resize_message",
	     resizeWindow);
  sendMethod(class, NAME_device, DEFAULT, 1, "device*",
	     "Display window on device, take care of <-decoration",
	     deviceWindow);
  sendMethod(class, NAME_displayed, DEFAULT, 1, "bool",
	     "(Un)display window, take care of <-decoration",
	     displayedWindow);
  sendMethod(class, NAME_free, DEFAULT, 0,
	     "->free associated frame",
	     freeWindow);
  sendMethod(class, NAME_destroy, DEFAULT, 0,
	     "->destroy associated frame",
	     destroyWindow);
  sendMethod(class, NAME_compute, NAME_update, 0,
	     "Recompute window",
	     computeWindow);
  sendMethod(class, NAME_reparent, DEFAULT, 0,
	     "If no longr related to the window, ->uncreate",
	     reparentWindow);
  sendMethod(class, NAME_create, NAME_open, 1, "[window]",
	     "Create associated X-window structure",
	     createWindow);
  sendMethod(class, NAME_uncreate, NAME_open, 0,
	     "Destroy associated X-window structure",
	     uncreateWindow);
  sendMethod(class, NAME_open, NAME_open, 2, "[point]", "normalise=[bool]",
	     "Open associated frame on the display",
	     openWindow);
  sendMethod(class, NAME_openCentered, NAME_open, 1, "[point]",
	     "Open frame centered around point",
	     openCenteredWindow);
  sendMethod(class, NAME_ComputeDesiredSize, NAME_layout, 0,
	     "Compute the desired size (no-op)",
	     ComputeDesiredSizeWindow);
  sendMethod(class, NAME_reset, DEFAULT, 0,
	     "Reset window after an abort",
	     resetWindow);
  sendMethod(class, NAME_flush, DEFAULT, 0,
	     "Update graphicals in this window immediately",
	     flushWindow);

  sendMethod(class, NAME_foreground, NAME_appearance, 1, "colour",
	     "Set foreground colour",
	     colourWindow);
  sendMethod(class, NAME_redraw, NAME_repaint, 1, "[area]",
	     "Redraw (area of) the window",
	     redrawWindow);
  sendMethod(class, NAME_flash, NAME_report, 0,
	     "Flash the window",
	     flashWindow);
  sendMethod(class, NAME_grabKeyboard, NAME_event, 1, "bool",
	     "Grap keyboard events",
	     grabKeyboardWindow);
  sendMethod(class, NAME_grabPointer, NAME_event, 1, "bool",
	     "Grab pointer (mouse) events",
	     grabPointerWindow);
  sendMethod(class, NAME_pointer, NAME_pointer, 1, "point",
	     "Move the pointer relative to window",
	     pointerWindow);
  sendMethod(class, NAME_focus, NAME_focus,
	     4, "graphical*", "[recogniser]*", "[cursor]*", "[name]*",
	     "Forward events to graphical",
	     focusWindow);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Handle event",
	     eventWindow);
  sendMethod(class, NAME_typed, NAME_accelerator, 2,
	     "event_id", "delegate=[bool]",
	     "Handle accelerator (delegate to <-frame)",
	     typedWindow);

					/* redefined by device.  We need */
					/* the garphical one's */
  sendMethod(class, NAME_position, DEFAULT, 1, "point",
	     "Move origin to argument",
	     positionGraphical);
  sendMethod(class, NAME_move, DEFAULT, 1, "point",
	     "Move origin to argument",
	     positionGraphical);
  sendMethod(class, NAME_x, DEFAULT, 1, "int",
	     "Move graphical horizontally",
	     xGraphical);
  sendMethod(class, NAME_y, DEFAULT, 1, "int",
	     "Move graphical vertically",
	     yGraphical);


  sendMethod(class, NAME_above, NAME_layout, 1, "window",
	     "Put me above argument",
	     aboveWindow);
  sendMethod(class, NAME_below, NAME_layout, 1, "window",
	     "Put me below argument",
	     belowWindow);
  sendMethod(class, NAME_left, NAME_layout, 1, "window",
	     "Put me left of argument",
	     leftWindow);
  sendMethod(class, NAME_right, NAME_layout, 1, "window",
	     "Put me right of argument",
	     rightWindow);
  sendMethod(class, NAME_expose, NAME_stacking, 0,
	     "Expose (raise) related frame",
	     exposeWindow);
  sendMethod(class, NAME_hide, NAME_stacking, 0,
	     "Hide (lower) related frame",
	     hideWindow);

  sendMethod(class, NAME_scrollTo, NAME_scroll, 1, "point",
	     "Make point top-left of window",
	     scrollToWindow);
  sendMethod(class, NAME_normalise, NAME_scroll, 1, "area|graphical|chain",
	     "Ensure area|graphical|chain is visible",
	     normaliseWindow);
  sendMethod(class, NAME_scrollHorizontal, NAME_scroll,
	     3, "{forwards,backwards,goto}", "{page,file,line}", "int",
	     "Trap message from horizontal scrollbar",
	     scrollHorizontalWindow);
  sendMethod(class, NAME_scrollVertical, NAME_scroll,
	     3, "{forwards,backwards,goto}", "{page,file,line}", "int",
	     "Trap message from vertical scrollbar",
	     scrollVerticalWindow);
  sendMethod(class, NAME_bubbleScrollBar, NAME_scroll, 1, "scroll_bar",
	     "Update bubble of given scroll_bar object",
	     bubbleScrollBarWindow);
  sendMethod(class, NAME_changedUnion, NAME_scroll, 4,
	     "ox=int", "oy=int", "ow=int", "oh=int",
	     "Request scroll_bar update",
	     changedUnionWindow);
  sendMethod(class, NAME_decorate, NAME_appearance, 6,
	     "area=[{grow,shrink}]", "left_margin=[int]", "right_margin=[int]",
	     "top_margin=[int]", "bottom_margin=[int]", "decorator=[window]",
	     "Embed window for scrollbars, etc.",
	     decorateWindow);

  getMethod(class, NAME_convert, DEFAULT, "window", 1, "graphical",
	    "Return graphical's <-window",
	    getConvertWindow);
  getMethod(class, NAME_confirm, NAME_modal, "any", 3,
	    "position=[point]", "grab=[bool]", "normalise=[bool]",
	    "Run sub event-loop until ->return",
	    getConfirmWindow);
  getMethod(class, NAME_confirmCentered, NAME_modal, "any", 2,
	    "center=[point]", "grab=[bool]",
	    "->confirm with frame centered around point",
	    getConfirmCenteredWindow);
  getMethod(class, NAME_foreground, NAME_appearance, "colour", 0,
	    "Get foreground colour",
	    getForegroundWindow);
  getMethod(class, NAME_visible, NAME_area, "area", 0,
	    "New area representing visible part",
	    getVisibleWindow);
  getMethod(class, NAME_boundingBox, NAME_area, "area", 0,
	    "Union of graphicals",
	    getBoundingBoxWindow);
  getMethod(class, NAME_tile, DEFAULT, "tile", 0,
	    "Tile of window (create if not there)",
	    getTileWindow);
  getMethod(class, NAME_frame, DEFAULT, "frame", 0,
	    "Frame of window (create if not there)",
	    getFrameWindow);

  getMethod(class, NAME_containedIn, DEFAULT, "frame|device", 0,
	    "Frame/graphical device I'm contained in",
	    getContainedInWindow);


  WindowTable = createHashTable(toInt(32), OFF);

  refine_resource(class, "selection_handles", "@nil");
  attach_resource(class, "selection_feedback", NULL, "handles", NULL);
  attach_resource(class, "size", "size", "size(200,100)",
		  "Default size (pixels)");
  attach_resource(class, "pen", "int", "1",
		  "Thickness of outside line");
  attach_resource(class, "cursor", "cursor", "top_left_arrow",
		  "Default window cursor");
  attach_resource(class, "background", "colour|pixmap", "white",
		  "Colour/fill pattern of the background");

  succeed;
}

