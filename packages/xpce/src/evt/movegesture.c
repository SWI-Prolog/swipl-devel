/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

status
initialiseMoveGesture(MoveGesture g, Name button, Modifier modifier)
{ initialiseGesture((Gesture) g, button, modifier);
  assign(g, offset, newObject(ClassPoint, ZERO, ZERO, 0));
  
  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
verifyMoveGesture(MoveGesture g, EventObj ev)
{ if ( isNil(ev->receiver->device) )
    fail;

  succeed;
}


static status
initiateMoveGesture(MoveGesture g, EventObj ev)
{ copyPoint(g->offset, getAreaPositionEvent(ev, DEFAULT));

  succeed;
}


static status
dragMoveGesture(MoveGesture g, EventObj ev)
{ Int x, y;

  get_xy_event(ev, get(ev->receiver, NAME_device, 0), OFF, &x, &y);
  DEBUG(NAME_drag, writef("Receiver = %s; x = %d; y = %d\n",
			  ev->receiver, x, y));
  x = sub(x, g->offset->x);
  y = sub(y, g->offset->y);

  send(ev->receiver, NAME_doSet, x, y, 0);

  succeed;
}


static status
terminateMoveGesture(MoveGesture g, EventObj ev)
{ return dragMoveGesture(g, ev);
}


status
makeClassMoveGesture(Class class)
{ sourceClass(class, makeClassMoveGesture, __FILE__, "$Revision$");

  localClass(class, NAME_offset, NAME_internal, "point", NAME_get,
	     "Offset of down relative to object");

  termClass(class, "move_gesture", 2, NAME_button, NAME_modifier);

  sendMethod(class, NAME_initialise, DEFAULT,
	     2, "button=[button_name]", "modifier=[modifier]",
	     "Create from button and modifier",
	     initialiseMoveGesture);
  sendMethod(class, NAME_verify, DEFAULT, 1, "event",
	     "Verify object moved is displayed on a device",
	     verifyMoveGesture);
  sendMethod(class, NAME_initiate, DEFAULT, 1, "event",
	     "Initiate move",
	     initiateMoveGesture);
  sendMethod(class, NAME_drag, DEFAULT, 1, "event",
	     "Drag to next position",
	     dragMoveGesture);
  sendMethod(class, NAME_terminate, DEFAULT, 1, "event",
	     "Finish the move",
	     terminateMoveGesture);

  attach_resource(class, "button", "button_name", "middle",
		  "Active on which button (middle)");
  attach_resource(class, "cursor", "cursor", "fleur",
		  "Cursor while active");

  succeed;
}

