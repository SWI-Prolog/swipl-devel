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
initialiseEditTextGesture(EditTextGesture g, Name button, Modifier modifier)
{ initialiseGesture((Gesture) g, button, modifier);
  assign(g, selection_origin, ZERO);
  assign(g, activate, OFF);

  succeed;
}


static status
eventEditTextGesture(EditTextGesture g, EventObj ev)
{ Graphical t = ev->receiver;

  if ( get(t, NAME_showCaret, EAV) == ON &&
       isAEvent(ev, NAME_keyboard) )
    return send(t, NAME_typed, ev, EAV);
  else if ( isAEvent(ev, NAME_obtainKeyboardFocus) )
    return send(t, NAME_showCaret, ON, EAV);
  else if ( isAEvent(ev, NAME_releaseKeyboardFocus) )
    return send(t, NAME_showCaret, OFF, EAV);
  
  return eventGesture(g, ev);
}
 

		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
initiateEditTextGesture(EditTextGesture g, EventObj ev)
{ Graphical t = ev->receiver;
  Int origin = get(t, NAME_pointed, getPositionEvent(ev, DEFAULT), EAV);

  if ( getMulticlickEvent(ev) != NAME_single )
    fail;

  if ( origin )
  { assign(g, selection_origin, origin);
    send(t, NAME_caret, origin, EAV);
    send(t, NAME_selection, NIL, EAV);
    assign(g, activate, ON);

    succeed;
  }

  fail;
}


static status
dragEditTextGesture(EditTextGesture g, EventObj ev)
{ Graphical t = ev->receiver;
  Int end = get(t, NAME_pointed, getPositionEvent(ev, DEFAULT), EAV);

  if ( notNil(g->max_drag_distance) )
  { PceWindow sw;

    if ( instanceOfObject((sw=ev->window), ClassWindow) &&
	 valInt(getDistanceEvent(sw->focus_event, ev)) >
	 valInt(g->max_drag_distance) )
      assign(g, activate, OFF);
  }

  if ( end )
  { send(t, NAME_selection, g->selection_origin, end, EAV);
    send(t, NAME_caret, end, EAV);

    succeed;
  }

  fail;
}


static status
terminateEditTextGesture(EditTextGesture g, EventObj ev)
{ TextObj t = ev->receiver;

  if ( instanceOfObject(t, ClassText) && notNil(t->selection) )
    send(t, NAME_copy, EAV);

  if ( g->activate == ON )
  { PceWindow sw = getWindowGraphical((Graphical)t);

    if ( sw )
      send(sw, NAME_keyboardFocus, t, EAV);
  }    

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static vardecl var_editTextGesture[] =
{ IV(NAME_selectionOrigin, "int", IV_BOTH,
     NAME_internal, "Where the selection started"),
  IV(NAME_maxDragDistance, "int*", IV_BOTH,
     NAME_cancel, "Click if dragged less"),
  IV(NAME_activate, "bool", IV_BOTH,
     NAME_internal, "@on: activate on ->terminate")
};

/* Send Methods */

static senddecl send_editTextGesture[] =
{ SM(NAME_event, 1, "event", eventEditTextGesture,
     DEFAULT, "Handle typing and selection management"),
  SM(NAME_drag, 1, "event", dragEditTextGesture,
     DEFAULT, "Extend selection"),
  SM(NAME_initialise, 2, T_initialise, initialiseEditTextGesture,
     DEFAULT, "Create from button and modifier"),
  SM(NAME_initiate, 1, "event", initiateEditTextGesture,
     DEFAULT, "Clear selection and set caret"),
  SM(NAME_terminate, 1, "event", terminateEditTextGesture,
     DEFAULT, "Activate on click"),
};

/* Get Methods */

#define get_editTextGesture NULL
/*
static getdecl get_editTextGesture[] =
{ 
};
*/

/* Resources */

static classvardecl rc_editTextGesture[] =
{ RC(NAME_button, "button_name", "left",
     "Active on which button (middle)"),
  RC(NAME_cursor, "[cursor]", "@default",
     "Cursor while active"),
  RC(NAME_maxDragDistance, "int*", "5",
     "Cancel after dragging this far")
};

/* Class Declaration */

static Name editTextGesture_termnames[] = { NAME_button, NAME_modifier };

ClassDecl(editTextGesture_decls,
          var_editTextGesture,
	  send_editTextGesture,
	  get_editTextGesture,
	  rc_editTextGesture,
          2, editTextGesture_termnames,
          "$Rev$");

status
makeClassEditTextGesture(Class class)
{ return declareClass(class, &editTextGesture_decls);
}

