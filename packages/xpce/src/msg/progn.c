/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


static status
initialisePrognv(Progn p, int argc, Any *argv)
{ int n;

  initialiseFunction((Function) p);
  assign(p, members, newObject(ClassChain, 0));

  for(n=0; n<argc; n++)
    appendChain(p->members, argv[n]);

  succeed;
}



		/********************************
		*           EXECUTION		*
		********************************/

static Any
getExecuteProgn(Progn p)
{ Cell cell;
  Any rval = FAIL;
  
  if ( emptyChain(p->members) )
  { errorPce(p, NAME_lastIsNoFunction);
    fail;
  }
  
  withLocalVars(for_cell(cell, p->members)
	      { if ( notNil(cell->next) )
		{ if ( !instanceOfObject(cell->value, ClassCode) )
		  { errorPce(cell->value, NAME_cannotExecute);
		    break;
		  }

		  if ( !executeCode(cell->value) )
		    break;

		  continue;
		}

		rval = expandCodeArgument(cell->value);
	      });

  answer(rval);
}


static status
appendProgn(Progn p, Code statement)
{ return appendChain(p->members, statement);
}


		/********************************
		*      TERM REPRESENTATION	*
		********************************/

static Int
getArityProgn(Progn p)
{ answer(getArityChain(p->members));
}


static Any
getArgProgn(Progn p, Int n)
{ answer(getArgChain(p->members, n));
}


status
makeClassProgn(Class class)
{ sourceClass(class, makeClassProgn, __FILE__, "$Revision$");

  localClass(class, NAME_members, NAME_statement, "chain", NAME_get,
	     "Statements and return function");

  termClass(class, "progn", ARGC_UNKNOWN);
  /* delegateClass(class, NAME_members);	Functions Can't delegate! */

  sendMethod(class, NAME_initialise, DEFAULT, 1, "statement=code|any ...",
	     "Create progn from statements and return",
	     initialisePrognv);
  sendMethod(class, NAME_append, NAME_list, 1, "code|any",
	     "Append a statement",
	     appendProgn);

  getMethod(class, NAME_Arg, DEFAULT, "code", 1, "int",
	    "Nth-1 argument for term description",
	    getArgProgn);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity for term description",
	    getArityProgn);
  getMethod(class, NAME_Execute, DEFAULT, "unchecked", 0,
	    "Execute the progn",
	    getExecuteProgn);

  succeed;
}

