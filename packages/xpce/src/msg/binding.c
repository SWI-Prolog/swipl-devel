/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseBinding(Binding att, Any name, Any value)
{ assign(att, name, name);
  assign(att, value, value);

  setFlag(att, F_ISBINDING);

  succeed;
}


status
makeClassBinding(Class class)
{ sourceClass(class, makeClassBinding, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_argument, "name", NAME_both,
	     "Name of the binding");
  localClass(class, NAME_value, NAME_value, "any|function", NAME_both,
	     "Value of the binding");

  termClass(class, ":=", 2, NAME_name, NAME_value);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "name=name", "value=any|function",
	     "Create binding from name and value",
	     initialiseBinding);

  succeed;
}

