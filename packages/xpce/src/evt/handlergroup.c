/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_handlerGroup[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_list, "Members of the collection")
};

/* Send Methods */

static senddecl send_handlerGroup[] =
{ SM(NAME_initialise, 1, "member=recogniser ...", initialiseHandlerGroupv,
     DEFAULT, "Create from given recognisers"),
  SM(NAME_event, 1, "event", eventHandlerGroup,
     NAME_event, "Process an event"),
  SM(NAME_append, 1, "recogniser", appendHandlerGroup,
     NAME_list, "Append a recogniser"),
  SM(NAME_delete, 1, "recogniser", deleteHandlerGroup,
     NAME_list, "Delete first occurrence of recogniser")
};

/* Get Methods */

static getdecl get_handlerGroup[] =
{ GM(NAME_Arg, 1, "recogniser", "int", getArgHandlerGroup,
     DEFAULT, "Nth-1 argument or term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityHandlerGroup,
     DEFAULT, "Arity of term description")
};

/* Resources */

#define rc_handlerGroup NULL
/*
static classvardecl rc_handlerGroup[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(handlerGroup_decls,
          var_handlerGroup, send_handlerGroup,
	  get_handlerGroup, rc_handlerGroup,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassHandlerGroup(Class class)
{ return declareClass(class, &handlerGroup_decls);
}
