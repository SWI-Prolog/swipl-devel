/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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


status
makeClassEqual(Class class)
{ sourceClass(class, makeClassEqual, __FILE__, "$Revision$");
  localClass(class, NAME_left, NAME_operant, "any|function", NAME_both,
	     "Left-hand side");
  localClass(class, NAME_right, NAME_operant, "any|function", NAME_both,
	     "Right-hand side");

  termClass(class, "==", 2, NAME_left, NAME_right);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "left=any|function", "right=any|function",
	     "Create from left- and right-hand",
	     initialiseEqual);
  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate both sides and test on equal",
	     ExecuteEqual);

  succeed;
}


