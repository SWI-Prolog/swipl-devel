/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

CPointer
CtoCPointer(void *ptr)
{ return answerObjectv(ClassCPointer, 1, &ptr);
}


static status
initialiseCPointer(CPointer p, void *value)
{ p->pointer = value;

  succeed;
}


static StringObj
getPrintNameCPointer(CPointer p)
{ char buf[20];

  sprintf(buf, "0x%lx", (ulong)p->pointer);
  answer(CtoString(buf));
}


static status
equalCPointer(CPointer p1, CPointer p2)
{ if ( p1->pointer == p2->pointer )
    succeed;
  fail;
}


status
makeClassCPointer(Class class)
{ sourceClass(class, makeClassCPointer, __FILE__, "$Revision$");

  localClass(class, NAME_pointer, NAME_storage, "alien:void *", NAME_none,
	     "Address of the pointer");

  termClass(class, "c_pointer", 1, NAME_printName);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "alien:void *",
	     "Create c_pointer from alien pointer",
	     initialiseCPointer);
  sendMethod(class, NAME_equal, NAME_compare, 1, "to=c_pointer",
	     "Test if argument is same position",
	     equalCPointer);

  getMethod(class, NAME_printName, NAME_textual, "string", 0,
	    "Printed representation as 0x%lx",
	    getPrintNameCPointer);

  succeed;
}
