/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


static status
initialiseConstant(Constant c, Name name, StringObj summary)
{ protectObject(c);

  assign(c, name, name);
  if ( notDefault(summary) )
    assign(c, summary, summary);

  succeed;
}


status
makeClassConstant(Class class)
{ localClass(class, NAME_name, NAME_name, "name", NAME_both,
	     "Name of the constant");
  localClass(class, NAME_summary, NAME_manual, "string*", NAME_both,
	     "Short description");

  sourceClass(class, makeClassConstant, __FILE__, "$Revision$");
  termClass(class, "constant", 1, NAME_self);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "name=name", "summary=string",
	     "Create constant",
	     initialiseConstant);

  newAssoc(NAME_nil, NIL);
  newAssoc(NAME_default, DEFAULT);

  ((Constant)NIL)->name = ((Constant)DEFAULT)->name = NIL;
  ((Constant)NIL)->summary = ((Constant)DEFAULT)->summary = NIL;
  assign(((Constant)NIL), name, NAME_nil);
  assign(((Constant)DEFAULT), name, NAME_default);
  assign(((Constant)NIL), summary,
	 CtoString("Representation of not-filled, nothing"));
  assign(((Constant)DEFAULT), summary,
	 CtoString("Representation of default/optional"));

  succeed;
}

