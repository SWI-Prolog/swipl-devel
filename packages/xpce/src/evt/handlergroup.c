/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status appendHandlerGroup(HandlerGroup, Recogniser);

static status
initialiseHandlerGroupv(HandlerGroup h, int argc, Any *argv)
{ int i;

  assign(h, members, newObject(ClassChain, 0));
  assign(h, active, ON);

  for (i=0; i<argc; i++)
    appendHandlerGroup(h, argv[i]);

  succeed;
}


static status
eventHandlerGroup(HandlerGroup h, EventObj ev)
{ Cell cell;

  if ( h->active == OFF )
    fail;

  for_cell(cell, h->members)
  { if ( qadSendv(cell->value, NAME_event, 1, (Any *)&ev) )
      succeed;
  }

  fail;
}


static status
appendHandlerGroup(HandlerGroup h, Recogniser r)
{ return appendChain(h->members, r);
}


static status
deleteHandlerGroup(HandlerGroup h, Recogniser r)
{ return deleteChain(h->members, r);
}


static Int
getArityHandlerGroup(HandlerGroup h)
{ answer(getSizeChain(h->members));
}


static Any
getArgHandlerGroup(HandlerGroup h, Int n)
{ extern Any getNth1Chain(Chain ch, Int index);
  answer(getNth1Chain(h->members, n));
}


status
makeClassHandlerGroup(Class class)
{ sourceClass(class, makeClassHandlerGroup, __FILE__, "$Revision$");

  localClass(class, NAME_members, NAME_list, "chain", NAME_get,
	     "Members of the collection");

  termClass(class, "handler_group", ARGC_UNKNOWN);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "member=recogniser ...",
	     "Create from given recognisers",
	     initialiseHandlerGroupv);
  sendMethod(class, NAME_append, NAME_list, 1, "recogniser",
	     "Append a recogniser",
	     appendHandlerGroup);
  sendMethod(class, NAME_delete, NAME_list, 1, "recogniser",
	     "Delete first occurrence of recogniser",
	     deleteHandlerGroup);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Process an event",
	     eventHandlerGroup);

  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity of term description",
	    getArityHandlerGroup);
  getMethod(class, NAME_Arg, DEFAULT, "recogniser", 1, "int",
	    "Nth-1 argument or term description",
	    getArgHandlerGroup);

  succeed;
}
