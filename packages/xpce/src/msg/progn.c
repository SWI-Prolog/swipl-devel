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
initialisePrognv(Progn p, int argc, Any *argv)
{ int n;

  initialiseFunction((Function) p);
  assign(p, members, newObject(ClassChain, EAV));

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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_progn[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_statement, "Statements and return function")
};

/* Send Methods */

static senddecl send_progn[] =
{ SM(NAME_initialise, 1, "statement=code|any ...", initialisePrognv,
     DEFAULT, "Create progn from statements and return"),
  SM(NAME_append, 1, "code|any", appendProgn,
     NAME_list, "Append a statement")
};

/* Get Methods */

static getdecl get_progn[] =
{ GM(NAME_Arg, 1, "code", "int", getArgProgn,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityProgn,
     DEFAULT, "Arity for term description"),
  GM(NAME_Execute, 0, "unchecked", NULL, getExecuteProgn,
     DEFAULT, "Execute the progn")
};

/* Resources */

#define rc_progn NULL
/*
static classvardecl rc_progn[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(progn_decls,
          var_progn, send_progn, get_progn, rc_progn,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassProgn(Class class)
{ return declareClass(class, &progn_decls);
}

