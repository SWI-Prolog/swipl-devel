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
initialiseNonEqual(NonEqual c, Any left, Any right)
{ assign(c, left,  left);
  assign(c, right, right);

  return initialiseCode((Code) c);
}


static status
ExecuteNonEqual(NonEqual c)
{ Any left  = expandCodeArgument(c->left);
  Any right = expandCodeArgument(c->right);

  if ( left == FAIL || right == FAIL )
    fail;

  if ( left != right )
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

static vardecl var_nonEqual[] =
{ IV(NAME_left, "any|function", IV_BOTH,
     NAME_operant, "Left-hand side"),
  IV(NAME_right, "any|function", IV_BOTH,
     NAME_operant, "Right-hand side")
};

/* Send Methods */

static senddecl send_nonEqual[] =
{ SM(NAME_Execute, 0, NULL, ExecuteNonEqual,
     DEFAULT, "Evaluate both sides and test on non-equal"),
  SM(NAME_initialise, 2, T_initialise, initialiseNonEqual,
     DEFAULT, "Create from left- and right-hand")
};

/* Get Methods */

#define get_nonEqual NULL
/*
static getdecl get_nonEqual[] =
{ 
};
*/

/* Resources */

#define rc_nonEqual NULL
/*
static classvardecl rc_nonEqual[] =
{ 
};
*/

/* Class Declaration */

static Name nonEqual_termnames[] = { NAME_left, NAME_right };

ClassDecl(nonEqual_decls,
          var_nonEqual, send_nonEqual, get_nonEqual, rc_nonEqual,
          2, nonEqual_termnames,
          "$Rev$");


status
makeClassNonEqual(Class class)
{ return declareClass(class, &nonEqual_decls);
}

