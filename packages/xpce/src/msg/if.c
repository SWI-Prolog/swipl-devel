/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


static status
initialiseIf(If i, Code cond, Code if_true, Code if_false)
{ initialiseCode((Code) i);

  if ( isDefault(if_true) )  if_true  = NIL;
  if ( isDefault(if_false) ) if_false = NIL;

  assign(i, condition, cond);
  assign(i, then_branch, if_true);
  assign(i, else_branch, if_false);

  succeed;
}


static status
ExecuteIf(If i)
{ if ( executeCode(i->condition) )
  { if ( notNil(i->then_branch) )
      return executeCode(i->then_branch) ? SUCCEED : FAIL;
    succeed;
  } else
  { if ( notNil(i->else_branch) )
      return executeCode(i->else_branch) ? SUCCEED : FAIL;
    succeed;
  }
}


status
makeClassIf(Class class)
{ sourceClass(class, makeClassIf, __FILE__, "$Revision$");

  localClass(class, NAME_condition, NAME_statement, "code", NAME_both,
	     "Condition to be tested");
  localClass(class, NAME_then, NAME_statement, "code*", NAME_both,
	     "Executed if condition is true");
  localClass(class, NAME_else, NAME_statement, "code*", NAME_both,
	     "Executed if condition is false");

  termClass(class, "if", 3, NAME_condition, NAME_then, NAME_else);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "condition=code", "then=[code]*", "else=[code]*",
	     "Create from condition, if- and else",
	     initialiseIf);
  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Test condition and branch",
	     ExecuteIf);

  succeed;
}


