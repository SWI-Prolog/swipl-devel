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


status
makeClassNonEqual(Class class)
{ sourceClass(class, makeClassNonEqual, __FILE__, "$Revision$");

  localClass(class, NAME_left, NAME_operant, "any|function", NAME_both,
	     "Left-hand side");
  localClass(class, NAME_right, NAME_operant, "any|function", NAME_both,
	     "Right-hand side");

  termClass(class, "\\==", 2, NAME_left, NAME_right);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "left=any|function", "right=any|function",
	     "Create from left- and right-hand",
	     initialiseNonEqual);
  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate both sides and test on non-equal",
	     ExecuteNonEqual);

  succeed;
}


