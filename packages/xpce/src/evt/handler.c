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
initialiseHandler(Handler h, Any id, Message msg, RegionObj reg)
{ assign(h, active, ON),
  assign(h, event, id);
  assign(h, message, msg);
  assign(h, region, reg);

  succeed;
}


static status
eventHandler(Handler h, EventObj ev)
{ DEBUG(NAME_post, Cprintf("eventHandler(%s, %s)\n", pp(h), pp(ev)));

  if ( isAEvent(ev, h->event) )
  { if (isDefault(h->region))
    { if (isNil(h->message))
	succeed;
      return forwardReceiverCodev(h->message, getMasterEvent(ev),
				  1, (Any *)&ev);
    } else
    { Graphical gr = ev->receiver;

      if ( insideRegion(h->region, gr->area, 
		       	getAreaPositionEvent(ev, gr)) == SUCCEED )
      { if ( notNil(h->message) )
	  return forwardReceiverCodev(h->message, getMasterEvent(ev),
				      1, (Any *)&ev);
	
	succeed;
      }
    }
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "event=event_id", "message=code*", "restrict_to=[region]" };

/* Instance Variables */

static vardecl var_handler[] =
{ IV(NAME_event, "event_id", IV_GET,
     NAME_condition, "Type of the event"),
  IV(NAME_message, "code*", IV_BOTH,
     NAME_action, "Code executed when event matches"),
  IV(NAME_region, "[region]", IV_BOTH,
     NAME_condition, "Region of graphical the event must be in")
};

/* Send Methods */

static senddecl send_handler[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseHandler,
     DEFAULT, "Create from event-type, message and region"),
  SM(NAME_event, 1, "event", eventHandler,
     NAME_event, "Process an event")
};

/* Get Methods */

#define get_handler NULL
/*
static getdecl get_handler[] =
{ 
};
*/

/* Resources */

#define rc_handler NULL
/*
static classvardecl rc_handler[] =
{ 
};
*/

/* Class Declaration */

static Name handler_termnames[] = { NAME_event, NAME_message, NAME_region };

ClassDecl(handler_decls,
          var_handler, send_handler, get_handler, rc_handler,
          3, handler_termnames,
          "$Rev$");


status
makeClassHandler(Class class)
{ return declareClass(class, &handler_decls);
}
