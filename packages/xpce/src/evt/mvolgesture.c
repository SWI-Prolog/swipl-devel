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

static status
initialiseMoveOutlineGesture(MoveOutlineGesture g,
			     Name button, Modifier modifier)
{ initialiseMoveGesture((MoveGesture) g, button, modifier);
  obtainClassVariablesObject(g);
  assign(g, outline, newObject(ClassBox, EAV));
  send(g->outline, NAME_texture,
       getClassVariableValueObject(g, NAME_texture), EAV);
  send(g->outline, NAME_recogniser,
       newObject(ClassMoveGesture, g->button, g->modifier, EAV), EAV);
  
  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
initiateMoveOutlineGesture(MoveOutlineGesture g, EventObj ev)
{ Graphical gr = ev->receiver;

  if ( !instanceOfObject(gr, ClassGraphical) )
    fail;
  send(g->outline, NAME_area, gr->area, EAV);
  send(gr->device, NAME_display, g->outline, EAV);
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

  send(g, NAME_drag, ev, EAV);

  a = g->outline->area;
  send(ev->receiver, NAME_doSet, a->x, a->y, a->w, a->h, EAV);
  send(g->outline, NAME_device, NIL, EAV);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static vardecl var_moveOutlineGesture[] =
{ IV(NAME_outline, "box", IV_GET,
     NAME_feedback, "The outline moved")
};

/* Send Methods */

static senddecl send_moveOutlineGesture[] =
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

#define get_moveOutlineGesture NULL
/*
static getdecl get_moveOutlineGesture[] =
{ 
};
*/

/* Resources */

static classvardecl rc_moveOutlineGesture[] =
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

