/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseMoveOutlineGesture(MoveOutlineGesture g,
			     Name button, Modifier modifier)
{ initialiseMoveGesture((MoveGesture) g, button, modifier);
  assign(g, outline, newObject(ClassBox, ZERO, ZERO, 0));
  send(g->outline, NAME_texture, getResourceValueObject(g, NAME_texture), 0);
  send(g->outline, NAME_recogniser,
       newObject(ClassMoveGesture, g->button, g->modifier, 0), 0);
  
  
  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
initiateMoveOutlineGesture(MoveOutlineGesture g, EventObj ev)
{ send(g->outline, NAME_area, ev->receiver->area, 0);
  send(ev->receiver->device, NAME_display, g->outline, 0);
  postEvent(ev, (Graphical) g->outline, DEFAULT);

  succeed;
}


static status
dragMoveOutlineGesture(MoveOutlineGesture g, EventObj ev)
{ postEvent(ev, (Graphical) g->outline, DEFAULT);

  succeed;
}


static status
terminateMoveOutlineGesture(MoveOutlineGesture g, EventObj ev)
{ Area a;

  send(g, NAME_drag, ev, 0);

  a = g->outline->area;
  send(ev->receiver, NAME_doSet, a->x, a->y, a->w, a->h, 0);
  send(g->outline, NAME_device, NIL, 0);

  succeed;
}


status
makeClassMoveOutlineGesture(Class class)
{ sourceClass(class, makeClassMoveOutlineGesture, __FILE__, "1.2");

  localClass(class, NAME_outline, NAME_feedback, "box", NAME_get,
	     "The outline moved");

  termClass(class, "move_outline_gesture", 2, NAME_button, NAME_modifier);

  sendMethod(class, NAME_initialise, DEFAULT,
	     2, "button=[button_name]", "modifier=[modifier]",
	     "Create from button and modifier",
	     initialiseMoveOutlineGesture);
  sendMethod(class, NAME_initiate, DEFAULT, 1, "event",
	     "Display outline and change cursor",
	     initiateMoveOutlineGesture);
  sendMethod(class, NAME_drag, DEFAULT, 1, "event",
	     "Drag outline to next position",
	     dragMoveOutlineGesture);
  sendMethod(class, NAME_terminate, DEFAULT, 1, "event",
	     "Move object and undisplay outline",
	     terminateMoveOutlineGesture);

  attach_resource(class, "texture", "texture_name", "dotted",
		  "Texture of the outline box");
  attach_resource(class, "button", "button_name", "middle",
		  "Active on which button (middle)");
  attach_resource(class, "cursor", "cursor", "fleur",
		  "Cursor while active");

  succeed;
}

