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
{ if ( notDefault(button) )
    assign(g, button, button);
  if ( notDefault(modifier) )
    assign(g, modifier, modifier);

  assign(g, active, ON);
  assign(g, status, NAME_inactive);
  assign(g, cursor, DEFAULT);

  return obtainClassVariablesObject(g);
}


status
eventGesture(Any obj, EventObj ev)
{ Gesture g = obj;

  if ( g->active == OFF )
    fail;

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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cancel a gesture. Typically deals with  click-gestures after the pointer
has moved too far. The gesture  undoes   focus,  switches itself off and
then reposts the initial event to see   if another gesture wants to have
the event. The it posts the  event  on   which  is  was started. This is
normally window<-current event. Hence the  hack there. See eventWindow()
for further details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
cancelGesture(Gesture g, EventObj ev)
{ PceWindow sw = ev->window;
  EventObj fe = sw->focus_event;
  EventObj oev;

  addCodeReference(fe);
  assign(g, active, OFF);
  send(sw, NAME_focus, NIL, 0);
  send(sw, NAME_event, fe, 0);

  addCodeReference(ev);
  oev = sw->current_event;
  assign(sw, current_event, NIL);
  send(sw, NAME_event, ev, 0);
  assign(sw, current_event, oev);
  delCodeReference(ev);

  assign(g, active, ON);
  delCodeReference(fe);
  freeableObj(fe);
  assign(g, status, NAME_inactive);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static vardecl var_gesture[] =
{ IV(NAME_button, "button_name", IV_GET,
     NAME_event, "Mouse button to initiate on"),
  IV(NAME_modifier, "modifier", IV_BOTH,
     NAME_event, "Key modifiers (shift, control and/or meta)"),
  IV(NAME_condition, "code*", IV_BOTH,
     NAME_event, "Additional conditions"),
  IV(NAME_status, "{active,inactive}", IV_BOTH,
     NAME_status, "Current status"),
  IV(NAME_cursor, "[cursor]", IV_BOTH,
     NAME_cursor, "Cursor while active")
};

/* Send Methods */

static senddecl send_gesture[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseGesture,
     DEFAULT, "Create from button and modifier"),
  SM(NAME_cancel, 1, "event", cancelGesture,
     NAME_cancel, "Cancel this gesture and try the next"),
  SM(NAME_drag, 1, "event", succeedGesture,
     NAME_event, "Mouse has been dragged (just succeeds)"),
  SM(NAME_event, 1, "event", eventGesture,
     NAME_event, "Handle an event"),
  SM(NAME_initiate, 1, "event", succeedGesture,
     NAME_event, "Initiate the gesture (just succeeds)"),
  SM(NAME_terminate, 1, "event", succeedGesture,
     NAME_event, "Mouse button went up (just succeeds)"),
  SM(NAME_verify, 1, "event", succeedGesture,
     NAME_event, "Verify additional conditions (just succeeds)")
};

/* Get Methods */

#define get_gesture NULL
/*
static getdecl get_gesture[] =
{ 
};
*/

/* Resources */

static classvardecl rc_gesture[] =
{ RC(NAME_button, "button_name", "left",
     "Active on which button?"),
  RC(NAME_cursor, "[cursor]", "@default",
     "Cursor while active"),
  RC(NAME_modifier, "modifier", "",
     "Condition on shift, control and meta")
};

/* Class Declaration */

static Name gesture_termnames[] = { NAME_button, NAME_modifier };

ClassDecl(gesture_decls,
          var_gesture, send_gesture, get_gesture, rc_gesture,
          2, gesture_termnames,
          "$Rev$");

status
makeClassGesture(Class class)
{ return declareClass(class, &gesture_decls);
}
