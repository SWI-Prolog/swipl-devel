/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <time.h>

static EventTreeObj  EventTree;		/* tree of event types */

forwards void		init_event_tree P((void));

extern EventNodeObj getNodeEventTree(EventTreeObj t, Any value);

					/* buttons bit mask */
#define CLICK_TYPE_mask		(0xC0)
#define CLICK_TYPE_single	(0x40)
#define CLICK_TYPE_double	(0x80)
#define CLICK_TYPE_triple	(0xC0)

#define TOINT(x) x

static Int 	 last_buttons     = TOINT(ZERO); /* defaults for next event */
static PceWindow last_window      = NIL;
static Int	 last_x		  = TOINT(ZERO);
static Int	 last_y		  = TOINT(ZERO);
static ulong	 last_time	  = 0L;

static Int	 last_down_bts    = ZERO;
static int	 last_down_x      = -1000; /* multiclick detection */
static int	 last_down_y	  = -1000;
static int	 last_down_time   = 0;
static int	 multi_click_time = 400;
static int	 multi_click_diff = 4;
static int	 last_click_type  = CLICK_TYPE_triple;

static status
initialiseEvent(EventObj e, Name id, PceWindow window,
		Int x, Int y, Int bts, Int time)
{ ulong t = valInt(time);

  initialiseProgramObject(e);

  if ( notNil(EVENT->value) )
  { EventObj parent = EVENT->value;
    
    if ( isDefault(x) )      x      = parent->x;
    if ( isDefault(y) )      y      = parent->y;
    if ( isDefault(bts) )    bts    = parent->buttons;
    if ( isDefault(window) ) window = parent->window;
    if ( isDefault(time) )   t      = parent->time;
  } else
  { if ( isDefault(x) )      x      = last_x;
    if ( isDefault(y) )      y      = last_y;
    if ( isDefault(bts) )    bts    = last_buttons;
    if ( isDefault(window) ) window = last_window;
    if ( isDefault(time) )   t      = last_time;
  }

  last_time    = t;
  last_buttons = bts;			/* save these values */
  last_x       = x;
  last_y       = y;

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

    if ( (t - last_down_time) < multi_click_time &&
	 abs(last_down_x - px) <= multi_click_diff &&
	 abs(last_down_y - py) <= multi_click_diff &&
	 (valInt(last_down_bts)&BUTTON_mask) == (valInt(bts)&BUTTON_mask) &&
	 last_window == window )
    { switch( last_click_type )
      { case CLICK_TYPE_single:	clt = CLICK_TYPE_double; break;
	case CLICK_TYPE_double:	clt = CLICK_TYPE_triple; break;
      }
    }

    last_click_type = clt;
    assign(e, buttons, toInt(valInt(e->buttons) | clt));

    last_down_bts     = bts;
    last_down_time    = t;
    last_down_x       = px;
    last_down_y       = py;
  } else if ( isUpEvent(e) )
  { assign(e, buttons, toInt(valInt(e->buttons) | last_click_type));
  }

  last_window = window;

  succeed;
}

		 /*******************************
		 *	     WINDOW		*
		 *******************************/

PceWindow
WindowOfLastEvent()
{ return last_window;
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

ulong					/* time-stamp of last event */
LastEventTime(void)
{ return last_time;
}


void
setLastEventTime(ulong time)
{ last_time = time;
}


Int
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
{ if ( valInt(e->buttons) & (BUTTON_ms_left|BUTTON_ms_middle|BUTTON_ms_right) )
    fail;

  succeed;
}


status
isUpEvent(EventObj e)
{ if ( isName(e->id) && (equalName(e->id, NAME_msLeftUp) ||
			 equalName(e->id, NAME_msMiddleUp) ||
			 equalName(e->id, NAME_msRightUp)) )
    succeed;
  fail;
}


status
isDownEvent(EventObj e)
{ if ( isName(e->id) && (equalName(e->id, NAME_msLeftDown) ||
			 equalName(e->id, NAME_msMiddleDown) ||
			 equalName(e->id, NAME_msRightDown)) )
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

  errorPce(e, NAME_noButtonEvent);
  fail;
}
   

status
isDragEvent(EventObj ev)
{ if ( isAEvent(ev, NAME_msLeftDrag) ||
       isAEvent(ev, NAME_msMiddleDrag) ||
       isAEvent(ev, NAME_msRightDrag) )
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

		/********************************
		*            POSITIONS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the position of an  event.  The `x'  and `y' fields  of the event
indicate  the   position  relative  to the   receiving  window.  These
functions allow you to find the position relative to:

	Display		The display on which the event occured
	Frame		The frame of the window
	Device		Relative to the origin of the device
	Graphical	Some graphical in the window in which the
			event occured
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
  DEBUG(NAME_drag, printf("At %d, %d to %s\n", *rx, *ry, pp(ev->window)));
  frame_offset_window(ev->window, &fr2, &frx, &fry);
  *rx += frx;
  *ry += fry;
  DEBUG(NAME_drag, printf("At %d, %d to %s\n", *rx, *ry, pp(fr2)));

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
  frame_offset_window(ev->window, &fr, &frx, &fry);
  *rx += frx + valInt(fr->area->x);
  *ry += fry + valInt(fr->area->y);
}


static void
get_xy_event_device(EventObj ev, Device dev, int *rx, int *ry)
{ int ox, oy;

  get_xy_event_window(ev, ev->window, OFF, rx, ry);
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
  *rx -= ox + valInt(gr->area->x);
  *ry -= oy + valInt(gr->area->y);
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
  else
  { *rx = ev->x;
    *ry = ev->y;
    succeed;
  }

  if ( area == ON &&
       instanceOfObject(ev->receiver, ClassDevice) && /* why ev->receiver? */
       !instanceOfObject(ev->receiver, ClassWindow) )
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
    assign(ev, position, newObject(ClassPoint, x, y, 0));
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
    assign(ev, position, newObject(ClassPoint, x, y, 0));
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

  if ( instanceOfObject(gr, ClassWindow) )
    succeed;				/* TBD: visible area? */

  TRY( get_xy_event(ev, gr, ON, &x, &y) );

  return inEventAreaGraphical(gr, add(gr->area->x, x), add(gr->area->y, y));
}


Any
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


Name
getKeyEvent(EventObj ev)
{ answer(characterName(ev->id));
}


		/********************************
		*         POSTING EVENTS	*
		********************************/

status
postEvent(EventObj ev, Graphical obj, Recogniser rec)
{ Graphical old = ev->receiver;
  status rval;

  addCodeReference(ev);

  DEBUG(NAME_post,
	if ( ev->id != NAME_locMove &&
	     !isDragEvent(ev) )
	  printf("Posting %s to %s\n", pp(ev->id), pp(obj)));

  withLocalVars({ assignVar(EVENT, ev, NAME_local);
		  assign(ev, receiver, obj);

		  rval = qadSendv(notDefault(rec) ? (Any)rec : (Any)obj,
				  NAME_event, 1, (Any *)&ev);

		  if ( !isFreedObj(ev) && isObject(old) && !isFreedObj(old) )
		  { if ( rval &&
			notNil(ev->window) && isNil(ev->window->focus) &&
			isDownEvent(ev) && !allButtonsUpLastEvent() &&
			instanceOfObject(obj, ClassGraphical) &&
			getWindowGraphical(obj) == ev->window )
		      focusWindow(ev->window, obj, NIL, DEFAULT,
				  getButtonEvent(ev));
		    assign(ev, receiver, old);
		  }
		});

  if ( !isFreedObj(ev) )
    delCodeReference(ev);

  return rval;
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
{ answer(getDisplayGraphical((Graphical) ev->window));
}


status
makeClassEvent(Class class)
{ sourceClass(class, makeClassEvent, __FILE__, "$Revision$");
  termClass(class, "event", 3, NAME_receiver, NAME_name, NAME_position,
	    		       NAME_buttons, NAME_time);

  localClass(class, NAME_window, NAME_context, "window", NAME_get,
	     "Window that generated event");
  localClass(class, NAME_receiver, NAME_context, "graphical", NAME_get,
	     "(Graphical) object receiving event");
  localClass(class, NAME_id, NAME_name, "event_id", NAME_get,
	     "Id of the event (type of event)");
  localClass(class, NAME_buttons, NAME_classify, "mask=int", NAME_get,
	     "Bitmask with status of buttons");
  localClass(class, NAME_x, NAME_position, "pixels=int", NAME_get,
	     "X-coordinate, relative to window");
  localClass(class, NAME_y, NAME_position, "pixels=int", NAME_get,
	     "Y-coordinate, relative to window");
  localClass(class, NAME_position, NAME_position, "point*", NAME_none,
	     "Last calculated position");
  localClass(class, NAME_time, NAME_timing, "alien:Time", NAME_none,
	     "Window Stystem Time stamp");

  sendMethod(class, NAME_initialise, DEFAULT, 6,
	     "id=event_id", "origin=[window]",
	     "x=[int]", "y=[int]", "button_mask=[int]", "time=[int]",
	     "Create from type, window, x, y, buttons and time",
	     initialiseEvent);
  sendMethod(class, NAME_isA, NAME_classify, 1, "super=event_id",
	     "Test if event matches type identifier",
	     isAEvent);
  sendMethod(class, NAME_post, NAME_forward, 2,
	     "to=graphical", "recogniser=[recogniser]",
	     "Deliver the event at the argument",
	     postEvent);
  sendMethod(class, NAME_isUp, NAME_classify, 0,
	     "Test if event is a button-up event",
	     isUpEvent);
  sendMethod(class, NAME_isDown, NAME_classify, 0,
	     "Test if event is a button-down event",
	     isDownEvent);
  sendMethod(class, NAME_hasModifier, NAME_classify, 1, "modifier",
	     "Test if event meets modifier spec",
	     hasModifierEvent);
  sendMethod(class, NAME_inside, NAME_position, 1, "[graphical]",
	     "Test if event is in area of graphical",
	     insideEvent);

  getMethod(class, NAME_master, NAME_context, "any", 0,
	    "The <-master of <-receiver",
	    getMasterEvent);
  getMethod(class, NAME_name, NAME_name, "event_id", 0,
	    "Name of the event (synomym for <-id)",
	    getIdEvent);
  getMethod(class, NAME_position, NAME_position, "point", 1,
	    "relative_to=[graphical|frame|display]",
	    "Position relative to argument",
	    getPositionEvent);
  getMethod(class, NAME_areaPosition, NAME_position, "point", 1,
	    "relative_to=[graphical]",
	    "Position relative to top-left-corner",
	    getAreaPositionEvent);
  getMethod(class, NAME_x, NAME_position, "int", 1,
	    "relative_to=[graphical|frame|display]",
	    "X coordinate relative to argument",
	    getXEvent);
  getMethod(class, NAME_y, NAME_position, "int", 1,
	    "relative_to=[graphical|frame|display]",
	    "Y coordinate relative to argument",
	    getYEvent);
  getMethod(class, NAME_distance, NAME_calculate, "int", 1, "to=event",
	    "Rounded integer distance between events",
	    getDistanceEvent);
  getMethod(class, NAME_button, NAME_classify, "button_name", 0,
	    "Button-name of button-event",
	    getButtonEvent);
  getMethod(class, NAME_multiclick, NAME_classify, "{single,double,triple}", 0,
	    "Click type",
	    getMulticlickEvent);
  getMethod(class, NAME_key, NAME_classify, "name", 0,
	    "Key(-binding) description of event",
	    getKeyEvent);
  getMethod(class, NAME_display, NAME_context, "display", 0,
	    "Display on which the event occured",
	    getDisplayEvent);
  getMethod(class, NAME_convert, DEFAULT, "event", 1, "[any]",
	    "Convert @default into current event (@event)",
	    getConvertEvent);
  getMethod(class, NAME_insideSubWindow, NAME_position, "frame|window", 1,
	    "[display|frame|window]",
	    "Frame or window event occurred in",
	    getInsideSubWindow);
  getMethod(class, NAME_time, DEFAULT, "int", 1, "[event]",
	    "Timestamp (relative to argument)",
	    getTimeEvent);

  init_event_tree();
  succeed;
}


static struct namepair
{ Name son;
  Name parent;
} initial_tree[] =
{ { NAME_mouse,		NAME_any },
  { NAME_keyboard,	NAME_any },
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
  { NAME_msLeft,	NAME_button },
  { NAME_msMiddle,	NAME_button },
  { NAME_msRight,	NAME_button },
  { NAME_msLeftDown,	NAME_msLeft },
  { NAME_msLeftUp,	NAME_msLeft },
  { NAME_msLeftDrag,	NAME_msLeft },
  { NAME_msRightDown,	NAME_msRight },
  { NAME_msRightUp,	NAME_msRight },
  { NAME_msRightDrag,	NAME_msRight },
  { NAME_msMiddleDown,	NAME_msMiddle },
  { NAME_msMiddleUp,	NAME_msMiddle },
  { NAME_msMiddleDrag,	NAME_msMiddle },

  { NAME_cursorHome,	NAME_cursor },
  { NAME_cursorLeft,	NAME_cursor },
  { NAME_cursorRight,	NAME_cursor },
  { NAME_cursorUp,	NAME_cursor },
  { NAME_cursorDown,	NAME_cursor },

  { NAME_area,		NAME_mouse },
  { NAME_areaEnter,	NAME_area },
  { NAME_areaExit,	NAME_area },
  { NAME_areaCancel,	NAME_areaExit },
  { NAME_areaResume,	NAME_areaEnter },

  { NAME_position,	NAME_mouse },
  { NAME_locMove,	NAME_position },

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
  EventNodeObj s  = newObject(ClassEventNode, n, 0);

  send(sn, NAME_son, s, 0);
}


static void
init_event_tree(void)
{ struct namepair *np;
  int oldtrace = TraceMode;

  TraceMode = TRACE_NEVER;
  EventTree = globalObject(NAME_eventTree, ClassEventTree, 0);

  send(EventTree, NAME_root, newObject(ClassEventNode, NAME_any, 0), 0);

  for(np = initial_tree; np->son; np++)
    add_node(np->son, np->parent);

  TraceMode = oldtrace;
}
