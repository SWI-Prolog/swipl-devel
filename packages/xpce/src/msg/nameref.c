/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>

typedef struct assoc *Assoc;

NewClass(assoc)
  ABSTRACT_CODE
  Name	reference;			/* reference of the object */
  Any	object;				/* Object to reference */
End;


static status
initialiseAssoc(Assoc a, Name name, Any object)
{ initialiseCode((Code) a);

  assign(a, reference, name);
  assign(a, object,    object);

  succeed;
}


static status
ExecuteAssoc(Assoc a)
{ return nameReferenceObject(a->object, a->reference);
}


status
makeClassAssoc(Class class)
{ sourceClass(class, makeClassAssoc, __FILE__, "$Revision$");

  localClass(class, NAME_reference, NAME_reference, "name", NAME_both,
	     "Reference to give to the object");
  localClass(class, NAME_object, NAME_reference, "object|function", NAME_both,
	     "Object to assign reference");
  
  termClass(class, "@=", 2, NAME_reference, NAME_object);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "reference=name", "object=object|function",
	     "Create from reference and object",
	     initialiseAssoc);

  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Assign the reference",
	     ExecuteAssoc);

  succeed;
}

