/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


static status
initialiseBool(Bool b)
{ return errorPce(classOfObject(b), NAME_cannotCreateInstances);
}


static status
unlinkBool(Bool b)
{ fail;					/* should not happen! */
}


static Bool
getConvertBool(Class class, Any obj)
{ answer(toBool(obj));
}


static Bool
getNegateBool(Bool b)
{ answer(b == ON ? OFF : ON);
}


status
makeClassBool(Class class)
{ sourceClass(class, makeClassBool, __FILE__, "$Revision$");
  termClass(class, "bool", 1, NAME_self);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create bool (cannot be created)",
	     initialiseBool);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Destroy boolean (cannot be done)",
	     unlinkBool);

  getMethod(class, NAME_convert, DEFAULT, "bool", 1, "any",
	    "Converts true, false and integer",
	    getConvertBool);
  getMethod(class, NAME_negate, NAME_calculate, "bool", 0,
	    "Maps @on <-> @off",
	    getNegateBool);

  ON->class = OFF->class = class;
  newAssoc(NAME_on,  ON);
  newAssoc(NAME_off, OFF);

  ON->name = OFF->name = NIL;
  ON->summary = OFF->summary = NIL;
  assign(ON, name, NAME_on);
  assign(OFF, name, NAME_off);
  assign(ON, summary, CtoString("Boolean true"));
  assign(OFF, summary, CtoString("Boolean false"));

  succeed;
}

