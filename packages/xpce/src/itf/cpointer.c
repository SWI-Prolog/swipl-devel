/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
