/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseEditTextGesture(EditTextGesture g, Name button, Modifier modifier)
{ initialiseGesture((Gesture) g, button, modifier);
  assign(g, selection_origin, ZERO);
  
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

  if ( origin )
  { PceWindow sw = getWindowGraphical((Graphical)t);

    assign(g, selection_origin, origin);
    send(t, NAME_caret, origin, EAV);
    send(t, NAME_selection, NIL, EAV);

    if ( sw )
      send(sw, NAME_keyboardFocus, t, EAV);
    
    succeed;
  }

  fail;
}


static status
dragEditTextGesture(EditTextGesture g, EventObj ev)
{ Graphical t = ev->receiver;
  Int end = get(t, NAME_pointed, getPositionEvent(ev, DEFAULT), EAV);

  if ( end )
  { send(t, NAME_selection, g->selection_origin, end, EAV);
    send(t, NAME_caret, end, EAV);

    succeed;
  }

  fail;
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
     NAME_internal, "Where the selection started")
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
     DEFAULT, "Clear selection and set caret")
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
     "Cursor while active")
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

