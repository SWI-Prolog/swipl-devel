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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static const vardecl var_moveOutlineGesture[] =
{ IV(NAME_outline, "box", IV_GET,
     NAME_feedback, "The outline moved")
};

/* Send Methods */

static const senddecl send_moveOutlineGesture[] =
{ SM(NAME_drag, 1, "event", dragMoveOutlineGesture,
     DEFAULT, "Drag outline to next position"),
  SM(NAME_initialise, 2, T_initialise, initialiseMoveOutlineGesture,
     DEFAULT, "Create from button and modifier"),
  SM(NAME_initiate, 1, "event", initiateMoveOutlineGesture,
     DEFAULT, "Display outline and change cursor"),
  SM(NAME_terminate, 1, "event", terminateMoveOutlineGesture,
     DEFAULT, "Move object and undisplay outline")
};

/* Get Methods */

static const getdecl get_moveOutlineGesture[] =
{ 
};

/* Resources */

static const resourcedecl rc_moveOutlineGesture[] =
{ RC(NAME_button, "button_name", "middle",
     "Active on which button (middle)"),
  RC(NAME_cursor, "cursor", "fleur",
     "Cursor while active"),
  RC(NAME_texture, "texture_name", "dotted",
     "Texture of the outline box")
};

/* Class Declaration */

static Name moveOutlineGesture_termnames[] = { NAME_button, NAME_modifier };

ClassDecl(moveOutlineGesture_decls,
          var_moveOutlineGesture, send_moveOutlineGesture,
	  get_moveOutlineGesture, rc_moveOutlineGesture,
          2, moveOutlineGesture_termnames,
          "$Rev$");

status
makeClassMoveOutlineGesture(Class class)
{ return declareClass(class, &moveOutlineGesture_decls);
}

