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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static const vardecl var_block[] =
{ IV(NAME_parameters, "code_vector*", IV_BOTH,
     NAME_argument, "Vector with formal parameters")
};

/* Send Methods */

static const senddecl send_block[] =
{ SM(NAME_initialise, 1, "var|code ...", initialiseBlockv,
     DEFAULT, "Create from parameters and statements"),
  SM(NAME_forward, 1, "any ...", forwardBlockv,
     NAME_execute, "Push <-parameters, @arg1 ... and execute")
};

/* Get Methods */

static const getdecl get_block[] =
{ GM(NAME_Arg, 1, "code", "int", getArgBlock,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityBlock,
     DEFAULT, "Arity for term description")
};

/* Resources */

static const resourcedecl rc_block[] =
{ 
};

/* Class Declaration */

ClassDecl(block_decls,
          var_block, send_block, get_block, rc_block,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassBlock(Class class)
{ return declareClass(class, &block_decls);
}

