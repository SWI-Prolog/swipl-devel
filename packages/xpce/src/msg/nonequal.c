/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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

static const char *T_initialise[] =
        { "left=any|function", "right=any|function" };

/* Instance Variables */

static const vardecl var_nonEqual[] =
{ IV(NAME_left, "any|function", IV_BOTH,
     NAME_operant, "Left-hand side"),
  IV(NAME_right, "any|function", IV_BOTH,
     NAME_operant, "Right-hand side")
};

/* Send Methods */

static const senddecl send_nonEqual[] =
{ SM(NAME_Execute, 0, NULL, ExecuteNonEqual,
     DEFAULT, "Evaluate both sides and test on non-equal"),
  SM(NAME_initialise, 2, T_initialise, initialiseNonEqual,
     DEFAULT, "Create from left- and right-hand")
};

/* Get Methods */

static const getdecl get_nonEqual[] =
{ 
};

/* Resources */

static const resourcedecl rc_nonEqual[] =
{ 
};

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

