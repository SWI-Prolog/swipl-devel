/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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


status
makeClassHandler(Class class)
{ sourceClass(class, makeClassHandler, __FILE__, "$Revision$");


  localClass(class, NAME_event, NAME_condition, "event_id", NAME_get,
	     "Type of the event");
  localClass(class, NAME_message, NAME_action, "code*", NAME_both,
	     "Code executed when event matches");
  localClass(class, NAME_region, NAME_condition, "[region]", NAME_both,
	     "Region of graphical the event must be in");

  termClass(class, "handler", 3, NAME_event, NAME_message, NAME_region);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "event=event_id", "message=code*", "restrict_to=[region]",
	     "Create from event-type, message and region",
	     initialiseHandler);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Process an event",
	     eventHandler);

  succeed;
}
