/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
createRelation(Relation r, Any from, Any to)
{ return send(r, NAME_forwards, from, to, 0);
}

static status
ignoreReleation(Relation r, Any from, Any to)
{ succeed;
}


status
makeClassRelation(Class class)
{ sourceClass(class, makeClassRelation, __FILE__, "$Revision$");

  termClass(class, "relation", 0);

  sendMethod(class, NAME_create, NAME_constraint, 2,
	     "from=object", "to=object",
	     "Called to initiate the relation",
	     createRelation);
  sendMethod(class, NAME_forwards, NAME_constraint, 2,
	     "from=object", "to=object",
	     "Called to update after a change of `from'",
	     ignoreReleation);
  sendMethod(class, NAME_backwards, NAME_constraint, 2,
	     "from=object", "to=object",
	     "Called to update after a change of `to'",
	     ignoreReleation);

  succeed;
}
