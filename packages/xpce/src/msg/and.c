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
  { TRY( executeCode(cell->value) )
  }

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


status
makeClassAnd(Class class)
{ sourceClass(class, makeClassAnd, __FILE__, "$Revision$");

  localClass(class, NAME_members, NAME_statement, "chain", NAME_get,
	     "Tests that must succeed");

  termClass(class, "and", ARGC_UNKNOWN);
  delegateClass(class, NAME_members);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "test=code ...",
	     "Create and from tests",
	     initialiseAndv);
  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate and",
	     ExecuteAnd);

  getMethod(class, NAME_Arg, DEFAULT, "code", 1, "int",
	    "Nth-1 argument for term description",
	    getArgAnd);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity for term description",
	    getArityAnd);

  succeed;
}

