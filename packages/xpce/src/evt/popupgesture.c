/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialisePopupGesture(PopupGesture g, PopupObj popup,
		       Name button, Modifier modifier)
{ if ( isDefault(popup) )
    popup = NIL;

  initialiseGesture((Gesture) g, button, modifier);
  assign(g, popup, popup);
   
  succeed;
}


static status
cancelPopupGesture(PopupGesture g)
{ assign(g, current, NIL);
  assign(g, context, NIL);

  succeed;
}


static status
updatePopupGesture(PopupGesture g, EventObj ev)
{ PopupObj p;

  if ( notNil(g->popup) )
  { if ( instanceOfObject(g->popup, ClassCode) )
    { TRY( p = getForwardFunction((Function) g->popup,
				  getMasterEvent(ev), ev, 0) );
      TRY( p = checkType(p, nameToType(NAME_popup), g));
    } else
      p = g->popup;
  } else
  { TRY( p = get(getMasterEvent(ev), NAME_popup, 0) );
  }

  assign(g, current, p);
  if ( isNil(g->context) )
    assign(g, context, notNil(g->current->context) ? g->current->context
	   					   : getMasterEvent(ev));

  send(p, NAME_update, g->context, 0);
  if ( p->active == OFF || emptyChain(p->members) )
  { cancelPopupGesture(g);
    fail;
  }

  succeed;
}


static status
eventPopupGesture(PopupGesture g, EventObj ev)
{ if ( g->status == NAME_active && isUpEvent(ev) )
  { PceWindow sw = ev->window;

    if ( valInt(getClickTimeEvent(ev)) < 400 &&
	 valInt(getClickDisplacementEvent(ev)) < 10 &&
	 getAttributeObject(g, NAME_Stayup) != ON )
    { attributeObject(g, NAME_Stayup, ON);
      grabPointerWindow(sw, ON);
      focusWindow(sw, ev->receiver, (Recogniser) g, g->cursor, NIL);
    } else
    { send(g, NAME_terminate, 0);
      if ( isNil(g->current) )
      { grabPointerWindow(sw, OFF);
	focusWindow(sw, NIL, NIL, NIL, NIL);
	deleteAttributeObject(g, NAME_Stayup);
	assign(g, status, NAME_inactive);
      }
    }

    succeed;
  } else if ( notNil(g->current) )
    return postEvent(ev, (Graphical) g->current, DEFAULT);
      
  if ( eventGesture(g, ev) )
    succeed;

  if ( isAEvent(ev, NAME_keyboard) )
  { Name key;

    TRY(updatePopupGesture(g, ev));
    key = characterName(getIdEvent(ev));

    if ( send(g->current, NAME_key, key, 0) )
    { Any context = g->context;
      PopupObj current = g->current;
      
      assign(g, context, NIL);
      assign(g, current, NIL);

      send(current, NAME_execute, context, 0);
      succeed;
    } else
      cancelPopupGesture(g);
  }

  fail;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
verifyPopupGesture(PopupGesture g, EventObj ev)
{ return updatePopupGesture(g, ev);
}


static status
initiatePopupGesture(PopupGesture g, EventObj ev)
{ send(g->current, NAME_open, ev->receiver,
       getAreaPositionEvent(ev, DEFAULT), 0);
  postEvent(ev, (Graphical) g->current, DEFAULT);
  succeed;
}


static status
dragPopupGesture(PopupGesture g, EventObj ev)
{ if ( notNil(g->current) )
    return postEvent(ev, (Graphical) g->current, DEFAULT);

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To avoid dangling references, the context and current are first copied
to local variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
terminatePopupGesture(PopupGesture g, EventObj ev)
{ Any context = g->context;
  PopupObj current = g->current;

  if ( notNil(current) )
  { postEvent(ev, (Graphical) current, DEFAULT);
  
    if ( current->displayed == OFF )	/* for stayup */
    { assign(g, context, NIL);
      assign(g, current, NIL);

      grabPointerWindow(ev->window, OFF);
      send(current, NAME_execute, context, 0);
      focusWindow(ev->window, NIL, NIL, NIL, NIL);
    }
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "popup=[popup|function]", "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static vardecl var_popupGesture[] =
{ IV(NAME_popup, "popup|function*", IV_BOTH,
     NAME_popup, "Popup displayed"),
  IV(NAME_current, "popup*", IV_NONE,
     NAME_popup, "Currently visible popup"),
  IV(NAME_context, "any", IV_BOTH,
     NAME_context, "Context to be send with the ->execute")
};

/* Send Methods */

static senddecl send_popupGesture[] =
{ SM(NAME_drag, 1, "event", dragPopupGesture,
     DEFAULT, "Pass drag events to popup"),
  SM(NAME_initialise, 3, T_initialise, initialisePopupGesture,
     DEFAULT, "Create from popup, button and modifier"),
  SM(NAME_initiate, 1, "event", initiatePopupGesture,
     DEFAULT, "Show popup"),
  SM(NAME_terminate, 1, "event", terminatePopupGesture,
     DEFAULT, "Unshow popup and execute selected item"),
  SM(NAME_verify, 1, "event", verifyPopupGesture,
     DEFAULT, "Verify popup can be activated"),
  SM(NAME_event, 1, "event", eventPopupGesture,
     NAME_accelerator, "Handle accelerators")
};

/* Get Methods */

#define get_popupGesture NULL
/*
static getdecl get_popupGesture[] =
{ 
};
*/

/* Resources */

static resourcedecl rc_popupGesture[] =
{ RC(NAME_button, "button_name", "right",
     "Active on which button (right)"),
  RC(NAME_cursor, "cursor", "right_ptr",
     "Cursor while active"),
  RC(NAME_modifier, "modifier", "",
     "Condition on shift, control and meta")
};

/* Class Declaration */

static Name popupGesture_termnames[] = { NAME_popup, NAME_button, NAME_modifier };

ClassDecl(popupGesture_decls,
          var_popupGesture, send_popupGesture,
	  get_popupGesture, rc_popupGesture,
          3, popupGesture_termnames,
          "$Rev$");

status
makeClassPopupGesture(Class class)
{ return declareClass(class, &popupGesture_decls);
}



Recogniser
popupGesture()
{ if ( GESTURE_popup == NULL )
    GESTURE_popup = globalObject(NAME_PopupGesture, ClassPopupGesture, 0);

  return (Recogniser) GESTURE_popup;
}
  
