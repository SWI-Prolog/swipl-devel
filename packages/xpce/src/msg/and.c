/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseAndv(And a, int argc, Any *argv)
{ int n;

  initialiseCode((Code) a);
  assign(a, members, newObject(ClassChain, 0));

  for(n=0; n<argc; n++)
    appendChain(a->members, argv[n]);

  succeed;
}


		/********************************
		*           EXECUTION		*
		********************************/

static status
ExecuteAnd(And a)
{ Cell cell;

  for_cell(cell, a->members)
    TRY(executeCode(cell->value));

  succeed;
}

		/********************************
		*      TERM REPRESENTATION	*
		********************************/

static Int
getArityAnd(And a)
{ answer(getArityChain(a->members));
}


static Any
getArgAnd(And a, Int n)
{ answer(getArgChain(a->members, n));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_and[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_statement, "Tests that must succeed")
};

/* Send Methods */

static senddecl send_and[] =
{ SM(NAME_Execute, 0, NULL, ExecuteAnd,
     DEFAULT, "Evaluate and"),
  SM(NAME_initialise, 1, "test=code ...", initialiseAndv,
     DEFAULT, "Create and from tests")
};

/* Get Methods */

static getdecl get_and[] =
{ GM(NAME_Arg, 1, "code", "int", getArgAnd,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityAnd,
     DEFAULT, "Arity for term description")
};

/* Resources */

#define rc_and NULL
/*
static resourcedecl rc_and[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(and_decls,
          var_and, send_and, get_and, rc_and,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassAnd(Class class)
{ declareClass(class, &and_decls);
  delegateClass(class, NAME_members);

  succeed;
}

