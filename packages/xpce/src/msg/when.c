/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


static status
initialiseWhen(When w, Code cond, Function when_true, Function when_false)
{ initialiseFunction((Function) w);

  assign(w, condition, cond);
  assign(w, then_branch, when_true);
  assign(w, else_branch, when_false);

  succeed;
}


static Any
getExecuteWhen(When w)
{ if ( executeCode(w->condition) )
    return expandCodeArgument(w->then_branch);
  else
    return expandCodeArgument(w->else_branch);
}


status
makeClassWhen(Class class)
{ sourceClass(class, makeClassWhen, __FILE__, "$Revision$");

  localClass(class, NAME_condition, NAME_statement, "code", NAME_both,
	     "Condition to be tested");
  localClass(class, NAME_then, NAME_statement, "any|function", NAME_both,
	     "Executed if condition is true");
  localClass(class, NAME_else, NAME_statement, "any|function", NAME_both,
	     "Executed if condition is false");

  termClass(class, "when", 3, NAME_condition, NAME_then, NAME_else);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "condition=code", "then=any|function", "else=any|function",
	     "Create from condition, when- and else",
	     initialiseWhen);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "temp; just to trap",
	     succeedObject);

  getMethod(class, NAME_Execute, DEFAULT, "unchecked", 0,
	     "Test condition and eveluate <-then or <-else",
	     getExecuteWhen);

  succeed;
}


