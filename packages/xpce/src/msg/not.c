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
initialiseNotv(Not n, Code arg)
{ initialiseCode((Code) n);
  assign(n, argument, arg);

  succeed;
}


		/********************************
		*           EXECUTION		*
		********************************/

static status
ExecuteNot(Not n)
{ if ( executeCode(n->argument) != FAIL )
    fail;

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_not[] =
{ IV(NAME_argument, "code", IV_BOTH,
     NAME_statement, "Test to negate")
};

/* Send Methods */

static senddecl send_not[] =
{ SM(NAME_Execute, 0, NULL, ExecuteNot,
     DEFAULT, "Evaluate argument test and negate result"),
  SM(NAME_initialise, 1, "test=code", initialiseNotv,
     DEFAULT, "Create from test")
};

/* Get Methods */

#define get_not NULL
/*
static getdecl get_not[] =
{ 
};
*/

/* Resources */

#define rc_not NULL
/*
static classvardecl rc_not[] =
{ 
};
*/

/* Class Declaration */

static Name not_termnames[] = { NAME_argument };

ClassDecl(not_decls,
          var_not, send_not, get_not, rc_not,
          1, not_termnames,
          "$Rev$");


status
makeClassNot(Class class)
{ return declareClass(class, &not_decls);
}

