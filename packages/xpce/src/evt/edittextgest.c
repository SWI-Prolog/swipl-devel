/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

status
initialiseEditTextGesture(EditTextGesture g, Name button, Modifier modifier)
{ initialiseGesture((Gesture) g, button, modifier);
  assign(g, selection_origin, ZERO);
  
  succeed;
}


static status
eventEditTextGesture(EditTextGesture g, EventObj ev)
{ Graphical t = ev->receiver;

  if ( get(t, NAME_showCaret, 0) == ON &&
       isAEvent(ev, NAME_keyboard) )
    return send(t, NAME_typed, ev, 0);
  else if ( isAEvent(ev, NAME_obtainKeyboardFocus) )
    return send(t, NAME_showCaret, ON, 0);
  else if ( isAEvent(ev, NAME_releaseKeyboardFocus) )
    return send(t, NAME_showCaret, OFF, 0);
  
  return eventGesture(g, ev);
}
 

		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
initiateEditTextGesture(EditTextGesture g, EventObj ev)
{ Graphical t = ev->receiver;
  Int origin = get(t, NAME_pointed, getPositionEvent(ev, DEFAULT), 0);

  if ( origin )
  { PceWindow sw = getWindowGraphical((Graphical)t);

    assign(g, selection_origin, origin);
    send(t, NAME_caret, origin, 0);
    send(t, NAME_selection, NIL, 0);

    if ( sw )
      send(sw, NAME_keyboardFocus, t, 0);
    
    succeed;
  }

  fail;
}


static status
dragEditTextGesture(EditTextGesture g, EventObj ev)
{ Graphical t = ev->receiver;
  Int end = get(t, NAME_pointed, getPositionEvent(ev, DEFAULT), 0);

  if ( end )
  { send(t, NAME_selection, g->selection_origin, end, 0);
    send(t, NAME_caret, end, 0);

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

static resourcedecl rc_editTextGesture[] =
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

