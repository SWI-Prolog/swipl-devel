/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


status
initialiseTuple(Tuple t, Any first, Any second)
{ assign(t, first, first);
  assign(t, second, second);

  succeed;
}


status
makeClassTuple(Class class)
{ sourceClass(class, makeClassTuple, __FILE__, "$Revision$");

  localClass(class, NAME_first, NAME_storage, "any", NAME_both,
	     "First of the tuple");
  localClass(class, NAME_second, NAME_storage, "any", NAME_both,
	     "Second of the tuple");

  termClass(class, "tuple", 2, NAME_first, NAME_second);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "first=any", "second=any",
	     "Create tuple from first and second",
	     initialiseTuple);

  succeed;
}

