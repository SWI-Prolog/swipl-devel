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
initialiseAndv(And a, int argc, Any *argv)
{ int n;

  initialiseCode((Code) a);
  assign(a, members, newObject(ClassChain, EAV));

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
static classvardecl rc_and[] =
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

