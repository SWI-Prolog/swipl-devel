/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>

status
initialiseVmi(Vmi vmi, Name name)
{ initialiseProgramObject(vmi);
  assign(vmi, name, name);

  succeed;
}


status
makeClassVmi(Class class)
{ localClass(class, NAME_name, NAME_name, "name", NAME_get,
	     "Name of the vmi");

  sourceClass(class, makeClassVmi, __FILE__, "$Revision$");
  termClass(class, "vmi", 1, NAME_name);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "name=name",
	     "Create named vmi",
	     initialiseVmi);

  succeed;
}

