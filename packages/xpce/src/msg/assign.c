/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>


static status
initialiseAssignment(Assignment b, Var var, Any value, Name scope)
{ initialiseCode((Code) b);

  if ( isDefault(scope) )
    scope = NAME_local;

  assign(b, var,   var);
  assign(b, value, value);
  assign(b, scope, scope);

  succeed;
}


static status
ExecuteAssignment(Assignment b)
{ Any val;

  TRY(val = expandCodeArgument(b->value));
  return assignVar(b->var, val, b->scope);
}


status
makeClassAssign(Class class)
{ sourceClass(class, makeClassAssign, __FILE__, "$Revision$");

  localClass(class, NAME_var, NAME_storage, "var", NAME_both,
	     "Variable to be bound");
  localClass(class, NAME_value, NAME_storage, "any|function", NAME_both,
	     "Value to give to the assignment");
  localClass(class, NAME_scope, NAME_scope, "{local,outer,global}", NAME_both,
	     "Scope of assignment");
  
  termClass(class, "assign", 3, NAME_var, NAME_value, NAME_scope);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "variable=var", "value=any|function",
	     "scope=[{local,outer,global}]",
	     "Create assignment from var and value",
	     initialiseAssignment);

  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Bind the variable",
	     ExecuteAssignment);

  succeed;
}

