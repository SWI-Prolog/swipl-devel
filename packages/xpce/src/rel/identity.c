/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status	forwardsIdentity(Identity id, Any from, Any to);

static status
initialiseIdentity(Identity id, Name from, Name to)
{ assign(id, from, from);
  assign(id, to, isDefault(to) ? from : to);

  succeed;
}


static status
createIdentity(Identity id, Any from, Any to)
{ if (isNil(from) || isNil(to))
    succeed;
  return forwardsIdentity(id, from, to);
}


static status
forwardsIdentity(Identity id, Any from, Any to)
{ Any value;
  status rval;

  TRY(value = get(from, id->from, 0));
  rval = send(to, id->to, value, 0);
  if ( isObject(value) )
    doneObject(value);
  
  return rval;
}


static status
backwardsIdentity(Identity id, Any from, Any to)
{ Any value;
  status rval;

  TRY(value = get(to, id->to, 0));
  rval = send(from, id->from, value, 0);
  if ( isObject(value) )
    doneObject(value);
  
  return rval;
}


status
makeClassIdentity(Class class)
{ sourceClass(class, makeClassIdentity, __FILE__, "$Revision$");

  localClass(class, NAME_from, NAME_selector, "selector1=name", NAME_both,
	     "Attribute at the `From' side");
  localClass(class, NAME_to, NAME_selector, "selector2=name", NAME_both,
	     "Attribute at the `To' side");

  termClass(class, "identity", 2, NAME_from, NAME_to);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "selector1=name", "selector2=[name]",
	     "Create from attribute names",
	     initialiseIdentity);

  sendMethod(class, NAME_backwards, DEFAULT, 2,
	     "from=object", "to=object",
	     "Update after `from' object has changed",
	     backwardsIdentity);
  sendMethod(class, NAME_create, DEFAULT, 2,
	     "from=object*", "to=object*",
	     "Update after instantiation",
	     createIdentity);
  sendMethod(class, NAME_forwards, DEFAULT, 2,
	     "from=object", "to=object",
	     "Update after `to' object has changed",
	     forwardsIdentity);

  succeed;
}

