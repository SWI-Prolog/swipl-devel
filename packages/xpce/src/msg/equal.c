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
initialiseEqual(Equal eq, Any left, Any right)
{ assign(eq, left,  left);
  assign(eq, right, right);

  return initialiseCode((Code) eq);
}


static status
ExecuteEqual(Equal eq)
{ Any left  = expandCodeArgument(eq->left);
  Any right = expandCodeArgument(eq->right);

  if ( left == FAIL || right == FAIL )
    fail;

  if ( left == right )			/* left ->equal: right? */
    succeed;

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "left=any|function", "right=any|function" };

/* Instance Variables */

static vardecl var_equal[] =
{ IV(NAME_left, "any|function", IV_BOTH,
     NAME_operant, "Left-hand side"),
  IV(NAME_right, "any|function", IV_BOTH,
     NAME_operant, "Right-hand side")
};

/* Send Methods */

static senddecl send_equal[] =
{ SM(NAME_Execute, 0, NULL, ExecuteEqual,
     DEFAULT, "Evaluate both sides and test on equal"),
  SM(NAME_initialise, 2, T_initialise, initialiseEqual,
     DEFAULT, "Create from left- and right-hand")
};

/* Get Methods */

#define get_equal NULL
/*
static getdecl get_equal[] =
{ 
};
*/

/* Resources */

#define rc_equal NULL
/*
static classvardecl rc_equal[] =
{ 
};
*/

/* Class Declaration */

static Name equal_termnames[] = { NAME_left, NAME_right };

ClassDecl(equal_decls,
          var_equal, send_equal, get_equal, rc_equal,
          2, equal_termnames,
          "$Rev$");

status
makeClassEqual(Class class)
{ return declareClass(class, &equal_decls);
}


