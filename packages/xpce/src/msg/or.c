/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseOrv(Or or, int argc, Any *argv)
{ int n;

  initialiseCode((Code) or);
  assign(or, members,   newObject(ClassChain, 0));

  for(n=0; n<argc; n++)
    appendChain(or->members, argv[n]);

  succeed;
}


		/********************************
		*           EXECUTION		*
		********************************/

static status
ExecuteOr(Or or)
{ Cell cell;

  for_cell(cell, or->members)
  { if ( executeCode(cell->value) != FAIL )
      succeed;
  }

  fail;
}

		/********************************
		*      TERM REPRESENTATION	*
		********************************/

static Int
getArityOr(Or or)
{ answer(getArityChain(or->members));
}


static Any
getArgOr(Or or, Int n)
{ answer(getArgChain(or->members, n));
}


status
makeClassOr(Class class)
{ sourceClass(class, makeClassOr, __FILE__, "$Revision$");

  localClass(class, NAME_members, NAME_statement, "chain", NAME_get,
	     "One of these must succeed");

  termClass(class, "or", ARGC_UNKNOWN);
  delegateClass(class, NAME_members);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "test=code ...",
	     "Create from tests",
	     initialiseOrv);
  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate tests until one succeeds",
	     ExecuteOr);

  getMethod(class, NAME_Arg, DEFAULT, "code", 1, "int",
	    "Nth-1 argument for term description",
	    getArgOr);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity for term description",
	    getArityOr);

  succeed;
}

