/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseBlockv(Block b, int argc, Any *argv)
{ int n;

  initialiseCode((Code) b);
  assign(b, members, newObject(ClassChain, 0));

  for(n=0; n<argc; n++)
  { if ( instanceOfObject(argv[n], ClassVar) )
    { if ( isNil(b->parameters) )
	assign(b, parameters, newObjectv(ClassCodeVector, 1, &argv[n]));
      else
	appendVector(b->parameters, 1, &argv[n]);
    } else
      break;
  }

  for( ; n < argc; n++ )
    appendChain(b->members, argv[n]);

  succeed;
}


static Int
getArityBlock(Block b)
{ int n = (isNil(b->parameters) ? 0 : valInt(getArityVector(b->parameters)));
  
  n += valInt(getArityChain(b->members));

  answer(toInt(n));
}


static Any
getArgBlock(Block b, Int n)
{ if ( isNil(b->parameters) )
    answer(getArgChain(b->members, n));
  else
  { int s = valInt(getArityVector(b->parameters));
    
    if ( valInt(n) <= s )
      answer(getArgVector(b->parameters, n));
    else
      answer(getArgChain(b->members, toInt(valInt(n)-s)));
  }
}


status
makeClassBlock(Class class)
{ sourceClass(class, makeClassBlock, __FILE__, "$Revision$");
  termClass(class, "block", ARGC_UNKNOWN);

  localClass(class, NAME_parameters, NAME_argument, "code_vector*", NAME_both,
	     "Vector with formal parameters");

  sendMethod(class, NAME_initialise, DEFAULT, 1, "var|code ...",
	     "Create from parameters and statements",
	     initialiseBlockv);
  sendMethod(class, NAME_forward, NAME_execute, 1, "any ...",
	     "Push <-parameters, @arg1 ... and execute",
	     forwardBlockv);

  getMethod(class, NAME_Arg, DEFAULT, "code", 1, "int",
	    "Nth-1 argument for term description",
	    getArgBlock);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity for term description",
	    getArityBlock);

  succeed;
}

