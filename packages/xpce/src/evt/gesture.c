/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A  gesture describes  a  sequence  of mouse-events,   starting  with a
button-down upto the corresponding button-up event.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
initialiseGesture(Gesture g, Name button, Modifier modifier)
{ assign(g, active,       ON);
  assign(g, button,	  button);
  assign(g, modifier,     modifier);
  assign(g, status,       NAME_inactive);
  assign(g, cursor,       DEFAULT);

  succeed;
}


status
eventGesture(Any obj, EventObj ev)
{ Gesture g = obj;

  if ( g->active == OFF )
    fail;

  obtainResourcesObject(g);

  if ( isDownEvent(ev) &&
       hasModifierEvent(ev, g->modifier) &&
       getButtonEvent(ev) == g->button &&
       (isNil(g->condition) || forwardReceiverCode(g->condition, g, ev, 0)) &&
       send(g, NAME_verify, ev, 0) )
  { TRY( send(g, NAME_initiate, ev, 0) );
    assign(g, status, NAME_active);
    send(ev->window, NAME_focus, ev->receiver, g, g->cursor, g->button, 0);
    succeed;
  } else if ( g->status != NAME_inactive )
  { if ( isDragEvent(ev) )
    { send(g, NAME_drag, ev, 0);
      succeed;
    } else if ( isUpEvent(ev) && getButtonEvent(ev) == g->button )
    { send(g, NAME_terminate, ev, 0);
      assign(g, status, NAME_inactive);
      succeed;
    }
  }

  fail;
}


static status
succeedGesture(Gesture g, EventObj ev)
{ succeed;
}


status
cancelGesture(Gesture g, EventObj ev)
{ PceWindow sw = ev->window;
  EventObj fe = sw->focus_event;

  addCodeReference(fe);
  assign(g, active, OFF);
  send(sw, NAME_focus, NIL, 0);
  send(sw, NAME_event, fe, 0);
  send(sw, NAME_event, ev, 0);
  assign(g, active, ON);
  delCodeReference(fe);
  freeableObj(fe);

  succeed;
}


status
makeClassGesture(Class class)
{ sourceClass(class, makeClassGesture, __FILE__, "$Revision$");

  localClass(class, NAME_button, NAME_event, "button_name", NAME_get,
	     "Mouse button to initiate on");
  localClass(class, NAME_modifier, NAME_event, "modifier", NAME_both,
	     "Key modifiers (shift, control and/or meta)");
  localClass(class, NAME_condition, NAME_event, "code*", NAME_both,
	     "Additional conditions");
  localClass(class, NAME_status, NAME_status, "{active,inactive}", NAME_both,
	     "Current status");
  localClass(class, NAME_cursor, NAME_cursor, "[cursor]", NAME_both,
	     "Cursor while active");

  termClass(class, "gesture", 2, NAME_button, NAME_modifier);

  sendMethod(class, NAME_initialise, DEFAULT,
	     2, "button=[button_name]", "modifier=[modifier]",
	     "Create from button and modifier",
	     initialiseGesture);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Handle an event",
	     eventGesture);
  sendMethod(class, NAME_verify, NAME_event, 1, "event",
	     "Verify additional conditions (just succeeds)",
	     succeedGesture);
  sendMethod(class, NAME_initiate, NAME_event, 1, "event",
	     "Initiate the gesture (just succeeds)",
	     succeedGesture);
  sendMethod(class, NAME_drag, NAME_event, 1, "event",
	     "Mouse has been dragged (just succeeds)",
	     succeedGesture);
  sendMethod(class, NAME_terminate, NAME_event, 1, "event",
	     "Mouse button went up (just succeeds)",
	     succeedGesture);
  sendMethod(class, NAME_cancel, NAME_cancel, 1, "event",
	     "Cancel this gesture and try the next",
	     cancelGesture);

  attach_resource(class, "modifier", "modifier", "",
		  "Condition on shift, control and meta");
  attach_resource(class, "button", "button_name", "left",
		  "Active on which button?");
  attach_resource(class, "cursor", "[cursor]", "@default",
		  "Cursor while active");

  succeed;
}
