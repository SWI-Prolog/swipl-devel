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
initialisePopupGesture(PopupGesture g, PopupObj popup,
		       Name button, Modifier modifier)
{ if ( isDefault(popup) )
    popup = NIL;

  initialiseGesture((Gesture) g, button, modifier);
  assign(g, popup, popup);
   
  succeed;
}


static status
cancelPopupGesture(PopupGesture g, EventObj ev)
{ assign(g, current, NIL);
  assign(g, context, NIL);

  return cancelGesture((Gesture)g, ev);
}


static status
updatePopupGesture(PopupGesture g, EventObj ev)
{ PopupObj p;
  Any rec = getMasterEvent(ev);

  DEBUG(NAME_popup, Cprintf("updatePopupGesture(): rec=%s\n", pp(rec)));

  if ( notNil(g->popup) )
  { if ( instanceOfObject(g->popup, ClassFunction) )
    { TRY( p = getForwardReceiverFunction((Function) g->popup, rec,
				  rec, ev, EAV) );
      TRY( p = checkType(p, nameToType(NAME_popup), g));
    } else
      p = g->popup;
  } else
  { if ( !(p = get(rec, NAME_popup, EAV)) ||
	 !instanceOfObject(p, ClassPopup) )
      fail;
  }

  assign(g, current, p);
  if ( isNil(g->context) )
    assign(g, context, notNil(p->context) ? p->context : rec);
  send(p, NAME_update, g->context, EAV);

  if ( p->active == OFF || emptyChain(p->members) )
  { send(g, NAME_cancel, ev, EAV);
    fail;
  }

  succeed;
}


static status
eventPopupGesture(PopupGesture g, EventObj ev)
{ if ( g->status == NAME_active && isUpEvent(ev) )
  { PceWindow sw;
    
    if ( !(sw = getWindowGraphical(ev->receiver)) )
      sw = ev->window;
    
    if ( notNil(g->current) && g->current->displayed == OFF )
    { send(g->current, NAME_open, ev->receiver,
	   getAreaPositionEvent(ev, DEFAULT), EAV);
      attributeObject(g, NAME_Stayup, ON);
      grabPointerWindow(sw, ON);
      focusWindow(sw, ev->receiver, (Recogniser) g, g->cursor, NIL);
    } else if ( valInt(getClickTimeEvent(ev)) < 400 &&
		getAttributeObject(g, NAME_Stayup) != ON )
    { attributeObject(g, NAME_Stayup, ON);
      grabPointerWindow(sw, ON);
      focusWindow(sw, ev->receiver, (Recogniser) g, g->cursor, NIL);
    } else
    { send(g, NAME_terminate, EAV);
      if ( isNil(g->current) )
      { grabPointerWindow(sw, OFF);
	focusWindow(sw, NIL, NIL, NIL, NIL);
	deleteAttributeObject(g, NAME_Stayup);
	assign(g, status, NAME_inactive);
      }
    }

    succeed;
  } else if ( notNil(g->current) && g->current->displayed == ON )
    return postEvent(ev, (Graphical) g->current, DEFAULT);
      
  if ( eventGesture(g, ev) )
    succeed;

  if ( g->status == NAME_active && isAEvent(ev, NAME_keyboard) )
  { Name key;

    TRY(updatePopupGesture(g, ev));
    key = characterName(getIdEvent(ev));

    if ( send(g->current, NAME_key, key, EAV) )
    { Any context = g->context;
      PopupObj current = g->current;
      
      assign(g, context, NIL);
      assign(g, current, NIL);

      send(current, NAME_execute, context, EAV);
      succeed;
    } else
      send(g, NAME_cancel, ev, EAV);
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
{ if ( isNil(g->max_drag_distance) )
  { send(g->current, NAME_open, ev->receiver,
	 getAreaPositionEvent(ev, DEFAULT), EAV);
    postEvent(ev, (Graphical) g->current, DEFAULT);
  }

  succeed;
}


static status
dragPopupGesture(PopupGesture g, EventObj ev)
{ if ( notNil(g->current) && g->current->displayed == ON )
  { DEBUG(NAME_popup, Cprintf("Posting drag to %s\n", pp(g->current)));
    return postEvent(ev, (Graphical) g->current, DEFAULT);
  } else
  { if ( notNil(g->max_drag_distance) )
    { PceWindow sw;

      if ( instanceOfObject((sw=ev->window), ClassWindow) &&
	   valInt(getDistanceEvent(sw->focus_event, ev)) >
	   valInt(g->max_drag_distance) )
	send(g, NAME_cancel, ev, EAV);
    }
  }

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
    { PceWindow sw;

      if ( !(sw = getWindowGraphical(ev->receiver)) )
	sw = ev->window;

      assign(g, context, NIL);
      assign(g, current, NIL);

      grabPointerWindow(sw, OFF);
      send(current, NAME_execute, context, EAV);
      focusWindow(sw, NIL, NIL, NIL, NIL);
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
     NAME_context, "Context to be send with the ->execute"),
  IV(NAME_maxDragDistance, "int*", IV_BOTH,
     NAME_cancel, "Cancel after dragging this far")
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
  SM(NAME_cancel, 1, "event", cancelPopupGesture,
     NAME_cancel, "Cancel this gesture and try the next"),
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

static classvardecl rc_popupGesture[] =
{ RC(NAME_button, "button_name", "right",
     "Active on which button (right)"),
  RC(NAME_cursor, "cursor", "right_ptr",
     "Cursor while active"),
  RC(NAME_modifier, "modifier", "",
     "Condition on shift, control and meta"),
  RC(NAME_maxDragDistance, "int*", "5",
     "Cancel after dragging this far")
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
    GESTURE_popup = globalObject(NAME_PopupGesture, ClassPopupGesture, EAV);

  return (Recogniser) GESTURE_popup;
}
  
