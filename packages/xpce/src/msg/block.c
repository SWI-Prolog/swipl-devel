/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>

static status
initialiseBlockv(Block b, int argc, Any *argv)
{ int n;

  initialiseCode((Code) b);
  assign(b, members, newObject(ClassChain, EAV));

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

static vardecl var_block[] =
{ IV(NAME_parameters, "code_vector*", IV_BOTH,
     NAME_argument, "Vector with formal parameters")
};

/* Send Methods */

static senddecl send_block[] =
{ SM(NAME_initialise, 1, "var|code ...", initialiseBlockv,
     DEFAULT, "Create from parameters and statements"),
  SM(NAME_forward, 1, "any ...", forwardBlockv,
     NAME_execute, "Push <-parameters, @arg1 ... and execute")
};

/* Get Methods */

static getdecl get_block[] =
{ GM(NAME_Arg, 1, "code", "int", getArgBlock,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityBlock,
     DEFAULT, "Arity for term description")
};

/* Resources */

#define rc_block NULL
/*
static classvardecl rc_block[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(block_decls,
          var_block, send_block, get_block, rc_block,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassBlock(Class class)
{ return declareClass(class, &block_decls);
}

