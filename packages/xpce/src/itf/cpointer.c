/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

CPointer
CtoCPointer(void *ptr)
{ CPointer p = answerObjectv(ClassCPointer, 0, NULL);
  
  p->pointer = ptr;

  return p;
}


static status
initialiseCPointer(CPointer p, CPointer value)
{ if ( notDefault(value) )
    p->pointer = value->pointer;

  succeed;
}


static StringObj
getPrintNameCPointer(CPointer p)
{ char buf[20];

  sprintf(buf, "%p", p->pointer);
  answer(CtoString(buf));
}


static status
equalCPointer(CPointer p1, CPointer p2)
{ if ( p1->pointer == p2->pointer )
    succeed;
  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_cPointer[] =
{ IV(NAME_pointer, "alien:void *", IV_NONE,
     NAME_storage, "Address of the pointer")
};

/* Send Methods */

static senddecl send_cPointer[] =
{ SM(NAME_initialise, 1, "[c_pointer]", initialiseCPointer,
     DEFAULT, "Create c_pointer from other c_pointer"),
  SM(NAME_equal, 1, "to=c_pointer", equalCPointer,
     NAME_compare, "Test if argument is same position")
};

/* Get Methods */

static getdecl get_cPointer[] =
{ GM(NAME_printName, 0, "string", NULL, getPrintNameCPointer,
     NAME_textual, "Printed representation as 0x%lx")
};

/* Resources */

#define rc_cPointer NULL
/*
static classvardecl rc_cPointer[] =
{ 
};
*/

/* Class Declaration */

static Name cPointer_termnames[] = { NAME_printName };

ClassDecl(cPointer_decls,
          var_cPointer, send_cPointer, get_cPointer, rc_cPointer,
          1, cPointer_termnames,
          "$Rev$");


status
makeClassCPointer(Class class)
{ return declareClass(class, &cPointer_decls);
}
