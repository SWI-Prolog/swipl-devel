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
#include <h/graphics.h>
#include <time.h>

forwards void init_event_tree(void);

extern EventNodeObj getNodeEventTree(EventTreeObj t, Any value);

#define TOINT(x) x

static Int 	 last_buttons     = TOINT(ZERO); /* defaults for next event */
static Any	 last_window      = NIL;
static Int	 last_x		  = TOINT(ZERO);
static Int	 last_y		  = TOINT(ZERO);
static unsigned long last_time	  = 0L;

static Int	         last_down_bts    = ZERO;
static int	         last_down_x      = -1000; /* multiclick detection */
static int	         last_down_y	  = -1000;
static int	         last_down_time   = 0;
static unsigned int	 multi_click_time = 400;
static int	         multi_click_diff = 4;
static int	         last_click_type  = CLICK_TYPE_triple;
static int		 loc_still_posted = TRUE;
static unsigned long	 host_last_time   = 0;
static int		 loc_still_time	  = 400;

static status
initialiseEvent(EventObj e, Name id, Any window,
		Int x, Int y, Int bts, Int time)
{ unsigned long t = valInt(time);

  initialiseProgramObject(e);

  if ( notNil(EVENT->value) )
  { EventObj parent = EVENT->value;
    
    if ( isDefault(x) )      x      = parent->x;
    if ( isDefault(y) )      y      = parent->y;
    if ( isDefault(bts) )    bts    = parent->buttons;
    if ( isDefault(window) ) window = parent->window;
    if ( isDefault(time) )   t      = max(last_time, parent->time);
  } else
  { if ( isDefault(x) )      x      = last_x;
    if ( isDefault(y) )      y      = last_y;
    if ( isDefault(bts) )    bts    = last_buttons;
    if ( isDefault(window) ) window = last_window;
    if ( isDefault(time) )   t      = last_time;
  }

  host_last_time = mclock();
  last_time      = t;
  last_buttons   = bts;			/* save these values */
  last_x         = x;
  last_y         = y;

  assign(e, window,	window);
  assign(e, receiver,	window);
  assign(e, id,		id);
  assign(e, x,		x);
  assign(e, y,		y);
  assign(e, buttons,	bts);
  e->time = t;

  if ( isDownEvent(e) )
  { int clt = CLICK_TYPE_single;
    int px  = valInt(x);
    int py  = valInt(y);

    DEBUG(NAME_multiclick, Cprintf("t: %d (%d), x: %d (%d), y: %d (%d) --> ",
				   t, last_down_time, px, last_down_x,
				   py, last_down_y));

    if ( (valInt(e->buttons) & CLICK_TYPE_mask) == CLICK_TYPE_double )
    { switch( last_click_type )
      { case CLICK_TYPE_single:	clt = CLICK_TYPE_double; break;
	case CLICK_TYPE_double:	clt = CLICK_TYPE_triple; break;
	default:		clt = CLICK_TYPE_single; break;
      }
      e->buttons = toInt(valInt(e->buttons) & ~CLICK_TYPE_mask);
    } else
    { if ( (t - last_down_time) < multi_click_time &&
	   abs(last_down_x - px) <= multi_click_diff &&
	   abs(last_down_y - py) <= multi_click_diff &&
	   (valInt(last_down_bts)&BUTTON_mask) == (valInt(bts)&BUTTON_mask) &&
	   last_window == window )
      { switch( last_click_type )
	{ case CLICK_TYPE_single:	clt = CLICK_TYPE_double; break;
	  case CLICK_TYPE_double:	clt = CLICK_TYPE_triple; break;
	}
      }
    }

    last_click_type = clt;
    assign(e, buttons, toInt(valInt(e->buttons) | clt));

    DEBUG(NAME_multiclick, Cprintf("%s\n", strName(getMulticlickEvent(e))));

    last_down_bts     = bts;
    last_down_time    = t;
    last_down_x       = px;
    last_down_y       = py;
  } else if ( isUpEvent(e) )
  { assign(e, buttons, toInt(valInt(e->buttons) | last_click_type));
  }

  if ( !onFlag(window, F_FREED|F_FREEING) )
    last_window = window;

#if 0
  if ( e->id != NAME_locMove )
    loc_still_posted = TRUE;
  else
#endif
    loc_still_posted = FALSE;

  succeed;
}

		 /*******************************
		 *	    LOC-STILL		*
		 *******************************/

void
considerLocStillEvent()
{ if ( !loc_still_posted )
  { unsigned long now = mclock();

    if ( now - host_last_time < loc_still_time )
    { DEBUG(NAME_locStill, Cprintf("TimeDiff = %d (ignored)\n", now - host_last_time));
      return;
    }

    if ( !pceMTTryLock(LOCK_PCE) )
      return;
    if ( instanceOfObject(last_window, ClassWindow) &&
	 !onFlag(last_window, F_FREED|F_FREEING) )
    { ServiceMode(is_service_window(last_window),
		  { AnswerMark mark;
		    EventObj e;

		    markAnswerStack(mark);
		    e = newObject(ClassEvent,
				  NAME_locStill, last_window,
				  last_x, last_y, last_buttons,
				  toInt(last_time + now - host_last_time), EAV);
		    addCodeReference(e);
		    postNamedEvent(e, (Graphical) last_window, DEFAULT, NAME_postEvent);
		    delCodeReference(e);
		    freeableObj(e);
		    rewindAnswerStack(mark, NIL);		   
		  })
    }
    loc_still_posted = TRUE;
    pceMTUnlock(LOCK_PCE);
  }
}


		 /*******************************
		 *	     WINDOW		*
		 *******************************/

PceWindow
WindowOfLastEvent()
{ if ( !isProperObject(last_window) )
  { Cprintf("Warning: last_window = %s\n", pp(last_window));
    fail;
  }

  if ( instanceOfObject(last_window, ClassWindow) )
    return last_window;

  fail;
}


void
unlinkedWindowEvent(Any sw)
{ if ( sw == last_window )
    last_window = NIL;
}


		/********************************
		*          CONVERSION		*
		********************************/

static EventObj
getConvertEvent(Class class, Any def)
{ if ( isDefault(def) && instanceOfObject(EVENT->value, ClassEvent) )
    answer(EVENT->value);		/* @event */

  fail;
}


		/********************************
		*        TIME MANAGEMENT	*
		********************************/

unsigned long				/* time-stamp of last event */
LastEventTime(void)
{ return last_time;
}


void
setLastEventTime(unsigned long time)
{ last_time = time;
}


static Int
getTimeEvent(EventObj ev, EventObj ev2)
{ if ( notDefault(ev2) )
    answer(toInt(ev2->time - ev->time));
  else
    answer(toInt(ev->time % PCE_MAX_INT));
}


		/********************************
		*          EVENT_TYPES		*
		********************************/


status
isAEvent(EventObj e, Any id)
{ Name nm;
  EventNodeObj sb, super;

  if ( isInteger(id) )
    return e->id == id ? SUCCEED : FAIL;

  if ( isInteger(e->id) )
  { int c = valInt(e->id);

    if      ( c < 32 || c == 127 )		nm = NAME_control;
    else if ( c >= 32 && c < META_OFFSET )	nm = NAME_printable;
    else if ( c >= META_OFFSET   )		nm = NAME_meta;
    else 					fail;
  } else if ( isName(e->id) )
  { nm = e->id;
  } else
  { fail;
  }
  
  TRY( sb    = getNodeEventTree(EventTree, nm) );
  TRY( super = getNodeEventTree(EventTree, id) );
  
  return isAEventNode(sb, super);
}


status
eventName(Name name)
{ if ( !EventTree )
    realiseClass(ClassEvent);

  return getNodeEventTree(EventTree, name) ? SUCCEED : FAIL;
}

		/********************************
		*             BUTTONS		*
		********************************/

static status
allButtonsUpLastEvent(void)
{ if ( valInt(last_buttons) &
       (BUTTON_ms_left|BUTTON_ms_middle|BUTTON_ms_right) )
    fail;

  succeed;
}


status
allButtonsUpEvent(EventObj e)
{ if ( valInt(e->buttons) &
       (BUTTON_ms_left|
	BUTTON_ms_middle|
	BUTTON_ms_right|
	BUTTON_ms_button4|
	BUTTON_ms_button5) )
    fail;

  succeed;
}


status
isUpEvent(EventObj e)
{ if ( isName(e->id) && (equalName(e->id, NAME_msLeftUp) ||
			 equalName(e->id, NAME_msMiddleUp) ||
			 equalName(e->id, NAME_msRightUp) ||
			 equalName(e->id, NAME_msButton4Up) ||
			 equalName(e->id, NAME_msButton5Up)) )
    succeed;
  fail;
}


status
isDownEvent(EventObj e)
{ if ( isName(e->id) && (equalName(e->id, NAME_msLeftDown) ||
			 equalName(e->id, NAME_msMiddleDown) ||
			 equalName(e->id, NAME_msRightDown) ||
			 equalName(e->id, NAME_msButton4Down) ||
			 equalName(e->id, NAME_msButton5Down)) )
    succeed;
  fail;
}


Name
getButtonEvent(EventObj e)
{ if ( isAEvent(e, NAME_msLeft) )
    answer(NAME_left);
  if ( isAEvent(e, NAME_msMiddle) )
    answer(NAME_middle);
  if ( isAEvent(e, NAME_msRight) )
    answer(NAME_right);
  if ( isAEvent(e, NAME_msButton4) )
    answer(NAME_button4);
  if ( isAEvent(e, NAME_msButton5) )
    answer(NAME_button5);

  errorPce(e, NAME_noButtonEvent);
  fail;
}
   

status
isDragEvent(EventObj ev)
{ if ( isAEvent(ev, NAME_msLeftDrag) ||
       isAEvent(ev, NAME_msMiddleDrag) ||
       isAEvent(ev, NAME_msRightDrag) ||
       isAEvent(ev, NAME_msButton4Drag) ||
       isAEvent(ev, NAME_msButton5Drag) )
    succeed;

  fail;
}


		/********************************
		*            MODIFIERS		*
		********************************/

status
hasModifierEvent(EventObj ev, Modifier m)
{ 
#define DOWN(b) (valInt(ev->buttons) & b)
#define UP(b)   (!DOWN(b))
  if ( notDefault(m->shift) &&
       ((m->shift == NAME_down && UP(BUTTON_shift)) ||
	(m->shift == NAME_up   && DOWN(BUTTON_shift))) )
    fail;
  if ( notDefault(m->control) &&
       ((m->control == NAME_down && UP(BUTTON_control)) ||
	(m->control == NAME_up   && DOWN(BUTTON_control))) )
    fail;
  if ( notDefault(m->meta) &&
       ((m->meta == NAME_down && UP(BUTTON_meta)) ||
	(m->meta == NAME_up   && DOWN(BUTTON_meta))) )
    fail;
#undef UP
#undef DOWN

  succeed;
}


Name
getMulticlickEvent(EventObj e)
{ switch(valInt(e->buttons) & CLICK_TYPE_mask)
  { case CLICK_TYPE_single:	answer(NAME_single);
    case CLICK_TYPE_double:	answer(NAME_double);
    case CLICK_TYPE_triple:	answer(NAME_triple);
    default:			fail;
  }
}


Int
getClickTimeEvent(EventObj e)
{ answer(toInt(e->time - last_down_time));

  fail;
}


Int
getClickDisplacementEvent(EventObj e)
{ int dx = valInt(e->x) - last_down_x;
  int dy = valInt(e->y) - last_down_y;
    
  answer(toInt(isqrt(dx*dx + dy*dy)));
}


status
windowEvent(EventObj ev, PceWindow sw)
{ if ( ev->window != sw )
  { int x, y;

    offset_windows(sw, ev->window, &x, &y);
    assign(ev, x, toInt(valInt(ev->x) - x));
    assign(ev, y, toInt(valInt(ev->y) - y));
    assign(ev, window, sw);
  }

  succeed;
}


		/********************************
		*            POSITIONS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the position of an  event.  The `x'  and `y' fields  of the event
indicate  the   position  relative  to the   receiving  window.  These
functions allow you to find the position relative to:

	Display		The display on which the event occurred
	Frame		The frame of the window
	Device		Relative to the origin of the device
	Graphical	Some graphical in the window in which the
			event occurred
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
get_xy_event_window(EventObj ev, PceWindow w, Bool area, int *rx, int *ry)
{ int x, y;

  offset_windows(w, ev->window, &x, &y);

  if ( area == ON )
  { *rx = valInt(ev->x) - x;
    *ry = valInt(ev->y) - y;
  } else
  { offset_window(w, rx, ry);
    *rx = valInt(ev->x) - x - *rx;
    *ry = valInt(ev->y) - y - *ry;
  }
}


static void
get_xy_event_frame(EventObj ev, FrameObj fr, int *rx, int *ry)
{ FrameObj fr2;
  int frx, fry;
  
  get_xy_event_window(ev, ev->window, ON, rx, ry);
  DEBUG(NAME_drag, Cprintf("At %d, %d to %s\n", *rx, *ry, pp(ev->window)));
  frame_offset_window(ev->window, &fr2, &frx, &fry);
  *rx += frx;
  *ry += fry;
  DEBUG(NAME_drag, Cprintf("At %d, %d to %s\n", *rx, *ry, pp(fr2)));

  if ( fr != fr2 )
  { Area a1 = fr->area;			/* target frame area */
    Area a2 = fr2->area;		/* area of frame from event */

    *rx += valInt(a2->x) - valInt(a1->x);
    *ry += valInt(a2->y) - valInt(a1->y);
  }
}


static void
get_xy_event_display(EventObj ev, DisplayObj d, int *rx, int *ry)
{ FrameObj fr;
  int frx, fry;

  get_xy_event_window(ev, ev->window, ON, rx, ry);
  DEBUG(NAME_position, Cprintf("Ev at %d,%d relative to %s\n",
			       *rx, *ry, pp(ev->window)));
  frame_offset_window(ev->window, &fr, &frx, &fry);
  DEBUG(NAME_position, Cprintf("Frame offset: %d,%d\n", frx, fry));
  *rx += frx + valInt(fr->area->x);
  *ry += fry + valInt(fr->area->y);
}


static void
get_xy_event_device(EventObj ev, Device dev, int *rx, int *ry)
{ int ox, oy;
  PceWindow sw = getWindowGraphical((Graphical) dev);

  if ( !sw )
  { *rx = 0; *ry = 0;
    return;				/* generate an error? */
  }

  get_xy_event_window(ev, sw, OFF, rx, ry);
  offsetDeviceGraphical(dev, &ox, &oy);
  *rx -= ox + valInt(dev->offset->x);
  *ry -= oy + valInt(dev->offset->y);
}



static void
get_xy_event_graphical(EventObj ev, Graphical gr, int *rx, int *ry)
{ int ox, oy;
  PceWindow sw = getWindowGraphical(gr);
    
  if ( !sw )
    sw = ev->window;

  get_xy_event_window(ev, sw, OFF, rx, ry);
  offsetDeviceGraphical(gr, &ox, &oy);
  DEBUG(NAME_inside, Cprintf("At %d,%d: offset %s --> %s is %d,%d\n",
			     *rx, *ry,
			     pp(gr), pp(sw), ox, oy));
  *rx -= ox + valInt(gr->area->x);
  *ry -= oy + valInt(gr->area->y);
}


static void
get_xy_event_node(EventObj ev, Node node, int *rx, int *ry)
{ get_xy_event_graphical(ev, node->image, rx, ry);
}


status
get_xy_event(EventObj ev, Any obj, Bool area, Int *rx, Int *ry)
{ int x = 0, y = 0;

  if ( isNil(ev->window) )
  { *rx = ev->x;
    *ry = ev->y;
    succeed;
  } else if ( instanceOfObject(obj, ClassDisplay) )
    get_xy_event_display(ev, obj, &x, &y);
  else if ( instanceOfObject(obj, ClassFrame) )
    get_xy_event_frame(ev, obj, &x, &y);
  else if ( instanceOfObject(obj, ClassWindow) )
    get_xy_event_window(ev, obj, area, &x, &y);
  else if ( instanceOfObject(obj, ClassDevice) )
    get_xy_event_device(ev, obj, &x, &y);
  else if ( instanceOfObject(obj, ClassGraphical) )
    get_xy_event_graphical(ev, obj, &x, &y);
  else if ( instanceOfObject(obj, ClassNode) )
    get_xy_event_node(ev, obj, &x, &y);
  else
  { *rx = ev->x;
    *ry = ev->y;
    succeed;
  }

  if ( area == ON &&
       instanceOfObject(obj, ClassDevice) &&
       !instanceOfObject(obj, ClassWindow) )
  { Device dev = (Device) ev->receiver;
    x -= valInt(dev->area->x) - valInt(dev->offset->x);
    y -= valInt(dev->area->y) - valInt(dev->offset->y);
  }
  
  *rx = toInt(x);
  *ry = toInt(y);

  succeed;
}
  

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return x-y coordinates of  Event.  Normally these  are relative to the
window  involved, with graphical supplied   they  are  relative to the
graphicals coordinate system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Point
getPositionEvent(EventObj ev, Any obj)
{ Int x, y;

  if ( isDefault(obj) )
    obj = ev->receiver;

  TRY( get_xy_event(ev, obj, OFF, &x, &y) );
  
  if ( isNil(ev->position) )
    assign(ev, position, newObject(ClassPoint, x, y, EAV));
  else
    setPoint(ev->position, x, y);

  answer(ev->position);
}


Point
getAreaPositionEvent(EventObj ev, Graphical gr)
{ Int x, y;

  if ( isDefault(gr) )
    gr = ev->receiver;

  TRY( get_xy_event(ev, gr, ON, &x, &y) );

  if ( isNil(ev->position) )
    assign(ev, position, newObject(ClassPoint, x, y, EAV));
  else
    setPoint(ev->position, x, y);

  answer(ev->position);
}


Int
getXEvent(EventObj ev, Any obj)
{ Int x, y;

  if ( isDefault(obj) )
    obj = ev->receiver;
  TRY( get_xy_event(ev, obj, OFF, &x, &y) );

  answer(x);
}


Int
getYEvent(EventObj ev, Any obj)
{ Int x, y;

  if ( isDefault(obj) )
    obj = ev->receiver;
  TRY( get_xy_event(ev, obj, OFF, &x, &y) );

  answer(y);
}


status
insideEvent(EventObj ev, Graphical gr)
{ Int x, y;

  if ( isDefault(gr) )
    gr = ev->receiver;

  TRY( get_xy_event(ev, gr, ON, &x, &y) );
  DEBUG(NAME_inside, Cprintf("Event at %d,%d on %s\n",
			     valInt(x), valInt(y), pp(gr)));
  if ( instanceOfObject(gr, ClassWindow) )
  { int vx, vy, vw, vh; 
    PceWindow sw = (PceWindow) gr;
    int p = valInt(sw->pen);
    int ex = valInt(x);
    int ey = valInt(y);

    compute_window(sw, &vx, &vy, &vw, &vh);
    vx -= valInt(sw->scroll_offset->x) + p;
    vy -= valInt(sw->scroll_offset->y) + p;
    if ( ex >= vx && ex <= vx+vw &&
	 ey >= vy && ey <= vy+vh )
      succeed;

    fail;
  }

  return inEventAreaGraphical(gr, add(gr->area->x, x), add(gr->area->y, y));
}


static Any
getInsideSubWindow(EventObj ev, Any root)
{ return ws_event_in_subwindow(ev, root);
}


Int
getDistanceEvent(EventObj ev1, EventObj ev2)
{ if ( ev1->window == ev2->window )
  { int dx = valInt(ev1->x) - valInt(ev2->x);
    int dy = valInt(ev1->y) - valInt(ev2->y);

    answer(toInt(isqrt(dx * dx + dy * dy)));
  }

  fail;
}


		/********************************
		*         GET PARAMETERS        *
		********************************/

Any
getIdEvent(EventObj ev)
{ answer(ev->id);
}


Any
getReceiverEvent(EventObj ev)
{ answer(ev->receiver);
}


static Name
getKeyEvent(EventObj ev)
{ answer(characterName(ev));
}


		/********************************
		*         POSTING EVENTS	*
		********************************/

#define WindowOfEvent(ev) ((PceWindow)(ev)->window)

status
postNamedEvent(EventObj ev, Graphical obj, Recogniser rec, Name method)
{ Graphical old = ev->receiver;
  status rval;

  addCodeReference(ev);

  DEBUG(NAME_post,
	if ( ev->id != NAME_locMove &&
	     !isDragEvent(ev) )
	{ if ( isDefault(rec) )
	    Cprintf("Posting %s to %s->%s\n",
		    pp(ev->id), pp(obj), pp(method));
	  else
	    Cprintf("Posting %s to %s->%s (focus on %s)\n",
		    pp(ev->id), pp(obj), pp(method), pp(rec));
	});
		    

  withLocalVars({ assignVar(EVENT, ev, NAME_local);
		  assign(ev, receiver, obj);

		  rval = qadSendv(notDefault(rec) ? (Any)rec : (Any)obj,
				  method, 1, (Any *)&ev);

		  if ( !isFreedObj(ev) && isObject(old) && !isFreedObj(old) )
		  { if ( rval &&
			 instanceOfObject(ev->window, ClassWindow) &&
			 isNil(WindowOfEvent(ev)->focus) &&
			 isDownEvent(ev) && !allButtonsUpLastEvent() &&
			 instanceOfObject(obj, ClassGraphical) &&
			 getWindowGraphical(obj) == WindowOfEvent(ev) )
		      focusWindow(ev->window, obj, NIL, DEFAULT,
				  getButtonEvent(ev));
		    assign(ev, receiver, old);
		  }
		});

  if ( !isFreedObj(ev) )
    delCodeReference(ev);

  DEBUG(NAME_post,
	if ( ev->id != NAME_locMove &&
	     !isDragEvent(ev) )
	  Cprintf("--> post of %s to %s %s\n",
		  pp(ev->id), pp(obj), rval ? "succeeded" : "failed"));

  return rval;
}


status
postEvent(EventObj ev, Graphical obj, Recogniser rec)
{ return postNamedEvent(ev, obj, rec, NAME_event);
}


		/********************************
		*      REPORTING CONTEXT	*
		********************************/

Any
getMasterEvent(EventObj ev)
{ answer(getMasterGraphical(ev->receiver));
}


		/********************************
		*         MISCELLANEOUS		*
		********************************/

DisplayObj
getDisplayEvent(EventObj ev)
{ if ( instanceOfObject(ev->window, ClassWindow) )
    answer(getDisplayGraphical((Graphical) ev->window));
  else
    answer(((FrameObj)ev->window)->display);
}

		 /*******************************
		 *	     SCROLLING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deal with scroll-mice in X11. Such  mice   normally  report the wheel as
Z-axis motion events. As very  few   applications  know  about this, the
X-server normally maps  these  to  the   pointer-buttons  4  and  5. The
function below is called from editor, list_browser and window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
mapWheelMouseEvent(EventObj ev, Any rec)
{ if ( ev->id == NAME_wheel )
  { Name dir, unit;
    Int count;
    Int rot = getAttributeObject(ev, NAME_rotation);

    if ( !rot )
      fail;				/* Error? */

    if ( isDefault(rec) )
      rec = ev->receiver;

    if ( !hasSendMethodObject(rec, NAME_scrollVertical) )
      fail;
  
    if ( valInt(rot) > 0 )
      dir = NAME_backwards;
    else
      dir = NAME_forwards;
      
    if ( valInt(ev->buttons) & BUTTON_shift )
    { unit = NAME_line;
      count = toInt(1);
    } else if ( valInt(ev->buttons) & BUTTON_control )
    { unit = NAME_page;
      count = toInt(990);
    } else
    { unit = NAME_page;
      count = toInt(200);
    }
  
    send(rec, NAME_scrollVertical, dir, unit, count, EAV);
    succeed;				/* Or return? */
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "id=event_id", "origin=[window|frame]", "x=[int]", "y=[int]", "button_mask=[int]", "time=[int]" };
static char *T_post[] =
        { "to=graphical", "recogniser=[recogniser]" };

/* Instance Variables */

static vardecl var_event[] =
{ IV(NAME_window, "window|frame", IV_GET,
     NAME_context, "Window that generated event"),
  IV(NAME_receiver, "graphical|frame", IV_GET,
     NAME_context, "Object receiving event"),
  IV(NAME_id, "event_id", IV_GET,
     NAME_name, "Id of the event (type of event)"),
  IV(NAME_buttons, "mask=int", IV_GET,
     NAME_classify, "Bitmask with status of buttons"),
  IV(NAME_x, "pixels=int", IV_GET,
     NAME_position, "X-coordinate, relative to window"),
  IV(NAME_y, "pixels=int", IV_GET,
     NAME_position, "Y-coordinate, relative to window"),
  IV(NAME_position, "point*", IV_NONE,
     NAME_position, "Last calculated position"),
  IV(NAME_time, "alien:Time", IV_NONE,
     NAME_timing, "Window System Time stamp")
};

/* Send Methods */

static senddecl send_event[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseEvent,
     DEFAULT, "Create from type, window, x, y, buttons and time"),
  SM(NAME_hasModifier, 1, "modifier", hasModifierEvent,
     NAME_classify, "Test if event meets modifier spec"),
  SM(NAME_isA, 1, "super=event_id", isAEvent,
     NAME_classify, "Test if event matches type identifier"),
  SM(NAME_isDown, 0, NULL, isDownEvent,
     NAME_classify, "Test if event is a button-down event"),
  SM(NAME_isDrag, 0, NULL, isDragEvent,
     NAME_classify, "Test if event is a button-drag event"),
  SM(NAME_isUp, 0, NULL, isUpEvent,
     NAME_classify, "Test if event is a button-up event"),
  SM(NAME_post, 2, T_post, postEvent,
     NAME_forward, "Deliver the event at the argument"),
  SM(NAME_inside, 1, "[graphical]", insideEvent,
     NAME_position, "Test if event is in area of graphical")
};

/* Get Methods */

static getdecl get_event[] =
{ GM(NAME_convert, 1, "event", "[any]", getConvertEvent,
     DEFAULT, "Convert @default into current event (@event)"),
  GM(NAME_time, 1, "int", "[event]", getTimeEvent,
     DEFAULT, "Timestamp (relative to argument)"),
  GM(NAME_distance, 1, "int", "to=event", getDistanceEvent,
     NAME_calculate, "Rounded integer distance between events"),
  GM(NAME_button, 0, "button_name", NULL, getButtonEvent,
     NAME_classify, "Button-name of button-event"),
  GM(NAME_clickDisplacement, 0, "pixels=int", NULL, getClickDisplacementEvent,
     NAME_classify, "`up' events: distance since corresponding `down'"),
  GM(NAME_clickTime, 0, "milliseconds=int", NULL, getClickTimeEvent,
     NAME_classify, "`up' events: time since corresponding `down'"),
  GM(NAME_key, 0, "name", NULL, getKeyEvent,
     NAME_classify, "Key(-binding) description of event"),
  GM(NAME_multiclick, 0, "{single,double,triple}", NULL, getMulticlickEvent,
     NAME_classify, "Click type"),
  GM(NAME_display, 0, "display", NULL, getDisplayEvent,
     NAME_context, "Display on which the event occurred"),
  GM(NAME_master, 0, "any", NULL, getMasterEvent,
     NAME_context, "The <-master of <-receiver"),
  GM(NAME_name, 0, "event_id", NULL, getIdEvent,
     NAME_name, "Name of the event (synonym for <-id)"),
  GM(NAME_areaPosition, 1, "point", "relative_to=[graphical]", getAreaPositionEvent,
     NAME_position, "Position relative to top-left-corner"),
  GM(NAME_insideSubWindow, 1, "frame|window", "[display|frame|window]", getInsideSubWindow,
     NAME_position, "Frame or window event occurred in"),
  GM(NAME_position, 1, "point", "relative_to=[graphical|frame|display]", getPositionEvent,
     NAME_position, "Position relative to argument"),
  GM(NAME_x, 1, "int", "relative_to=[graphical|frame|display]", getXEvent,
     NAME_position, "X coordinate relative to argument"),
  GM(NAME_y, 1, "int", "relative_to=[graphical|frame|display]", getYEvent,
     NAME_position, "Y coordinate relative to argument")
};

/* Resources */

static classvardecl rc_event[] =
{ RC(NAME_x11WheelMouse, "bool", UXWIN("@on", "@off"),
     "Enable/disable wheel-mouse emulation on button 4 and 5"),
  RC(NAME_locStillTime, "int", "400",
     "Time before generating a loc_still event in milliseconds")
};

/* Class Declaration */

static Name event_termnames[] = { NAME_receiver, NAME_name, NAME_position, NAME_buttons, NAME_time };

ClassDecl(event_decls,
          var_event, send_event, get_event, rc_event,
          3, event_termnames,
          "$Rev$");


status
makeClassEvent(Class class)
{ Int t;

  declareClass(class, &event_decls);
  cloneStyleVariableClass(class, NAME_receiver, NAME_reference);
  cloneStyleVariableClass(class, NAME_window,   NAME_reference);
  init_event_tree();

  if ( (t=getClassVariableValueClass(class, NAME_locStillTime)) )
    loc_still_time = valInt(t);

  succeed;
}


static struct namepair
{ Name son;
  Name parent;
} initial_tree[] =
{ { NAME_mouse,		NAME_any },
  { NAME_keyboard,	NAME_any },
  { NAME_user,		NAME_any },
					/* Keyboard events */
  { NAME_ascii,		NAME_keyboard },
  { NAME_meta,		NAME_keyboard },
  { NAME_function,	NAME_keyboard },
  { NAME_control,	NAME_ascii },
  { NAME_printable,	NAME_ascii },
  { NAME_keyLeft,	NAME_function },
  { NAME_keyRight,	NAME_function },
  { NAME_keyTop,	NAME_function },
  { NAME_cursor,	NAME_function },
  { NAME_namedFunction, NAME_function },
  { NAME_keyLeft_1,	NAME_keyLeft },
  { NAME_keyLeft_2,	NAME_keyLeft },
  { NAME_keyLeft_3,	NAME_keyLeft },
  { NAME_keyLeft_4,	NAME_keyLeft },
  { NAME_keyLeft_5,	NAME_keyLeft },
  { NAME_keyLeft_6,	NAME_keyLeft },
  { NAME_keyLeft_7,	NAME_keyLeft },
  { NAME_keyLeft_8,	NAME_keyLeft },
  { NAME_keyLeft_9,	NAME_keyLeft },
  { NAME_keyLeft_10,	NAME_keyLeft },
  { NAME_keyRight_1,	NAME_keyRight },
  { NAME_keyRight_2,	NAME_keyRight },
  { NAME_keyRight_3,	NAME_keyRight },
  { NAME_keyRight_4,	NAME_keyRight },
  { NAME_keyRight_5,	NAME_keyRight },
  { NAME_keyRight_6,	NAME_keyRight },
  { NAME_keyRight_7,	NAME_keyRight },
  { NAME_keyRight_8,	NAME_keyRight },
  { NAME_keyRight_9,	NAME_keyRight },
  { NAME_keyRight_10,	NAME_keyRight },
  { NAME_keyRight_11,	NAME_keyRight },
  { NAME_keyRight_12,	NAME_keyRight },
  { NAME_keyRight_13,	NAME_keyRight },
  { NAME_keyRight_14,	NAME_keyRight },
  { NAME_keyRight_15,	NAME_keyRight },
  { NAME_keyTop_1,	NAME_keyTop },
  { NAME_keyTop_2,	NAME_keyTop },
  { NAME_keyTop_3,	NAME_keyTop },
  { NAME_keyTop_4,	NAME_keyTop },
  { NAME_keyTop_5,	NAME_keyTop },
  { NAME_keyTop_6,	NAME_keyTop },
  { NAME_keyTop_7,	NAME_keyTop },
  { NAME_keyTop_8,	NAME_keyTop },
  { NAME_keyTop_9,	NAME_keyTop },
  { NAME_keyTop_10,	NAME_keyTop },
					/* Mouse button events */
  { NAME_button,	NAME_mouse },
  { NAME_wheel,		NAME_mouse },
  { NAME_msLeft,	NAME_button },
  { NAME_msMiddle,	NAME_button },
  { NAME_msRight,	NAME_button },
  { NAME_msButton4,	NAME_button },
  { NAME_msButton5,	NAME_button },
  { NAME_msLeftDown,	NAME_msLeft },
  { NAME_msLeftUp,	NAME_msLeft },
  { NAME_msLeftDrag,	NAME_msLeft },
  { NAME_msRightDown,	NAME_msRight },
  { NAME_msRightUp,	NAME_msRight },
  { NAME_msRightDrag,	NAME_msRight },
  { NAME_msMiddleDown,	NAME_msMiddle },
  { NAME_msMiddleUp,	NAME_msMiddle },
  { NAME_msMiddleDrag,	NAME_msMiddle },
  { NAME_msButton4Down,	NAME_msButton4 },
  { NAME_msButton4Up,	NAME_msButton4 },
  { NAME_msButton4Drag,	NAME_msButton4 },
  { NAME_msButton5Down,	NAME_msButton5 },
  { NAME_msButton5Up,	NAME_msButton5 },
  { NAME_msButton5Drag,	NAME_msButton5 },


  { NAME_select,	NAME_namedFunction },
  { NAME_print,		NAME_namedFunction },
  { NAME_execute,	NAME_namedFunction },
  { NAME_insert,	NAME_namedFunction },
  { NAME_undo,		NAME_namedFunction },
  { NAME_redo,		NAME_namedFunction },
  { NAME_menu,		NAME_namedFunction },
  { NAME_find,		NAME_namedFunction },
  { NAME_cancel,	NAME_namedFunction },
  { NAME_help,		NAME_namedFunction },
  { NAME_break,		NAME_namedFunction },
  { NAME_backspace,	NAME_namedFunction },

  { NAME_cursorHome,	NAME_cursor },
  { NAME_cursorLeft,	NAME_cursor },
  { NAME_cursorRight,	NAME_cursor },
  { NAME_cursorUp,	NAME_cursor },
  { NAME_cursorDown,	NAME_cursor },
  { NAME_pageUp,	NAME_cursor },
  { NAME_pageDown,	NAME_cursor },
  { NAME_begin,		NAME_cursor },
  { NAME_end,		NAME_cursor },

  { NAME_area,		NAME_mouse },
  { NAME_areaEnter,	NAME_area },
  { NAME_areaExit,	NAME_area },
  { NAME_areaCancel,	NAME_areaExit },
  { NAME_areaResume,	NAME_areaEnter },

  { NAME_position,	NAME_mouse },
  { NAME_locMove,	NAME_position },
  { NAME_locStill,	NAME_position },

  { NAME_focus,				NAME_any },
  { NAME_deactivateKeyboardFocus,	NAME_focus },
  { NAME_releaseKeyboardFocus, 		NAME_deactivateKeyboardFocus },
  { NAME_obtainKeyboardFocus, 		NAME_focus },
  { NAME_activateKeyboardFocus, 	NAME_obtainKeyboardFocus },
  { NAME_releaseFocus,			NAME_focus },
  { NAME_obtainFocus,			NAME_focus },

  { 0,			0 }
};


static void
add_node(Name n, Name super)
{ EventNodeObj sn = getNodeEventTree(EventTree, super);
  EventNodeObj s  = newObject(ClassEventNode, n, EAV);

  send(sn, NAME_son, s, EAV);
}


static void
init_event_tree(void)
{ struct namepair *np;

  EventTree = globalObject(NAME_eventTree, ClassEventTree, EAV);

  send(EventTree, NAME_root, newObject(ClassEventNode, NAME_any, EAV), EAV);

  for(np = initial_tree; np->son; np++)
    add_node(np->son, np->parent);
}
