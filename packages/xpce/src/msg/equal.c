/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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


