/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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


static void
mkconstant(Class class, Constant c, Name name, const char *summary)
{ initHeaderObj(c, class);
  setProtectedObj(c);
  clearCreatingObj(c);
  
  assign(c, name,    name);
  assign(c, summary, staticCtoString(summary));

  newAssoc(name, c);
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

  mkconstant(class,
	     NIL,
	     NAME_nil,
	     "Representation of not-filled, nothing");
  mkconstant(class,
	     DEFAULT,
	     NAME_default,
	     "Representation of default/optional");
  mkconstant(class,
	     CLASSDEFAULT,
	     NAME_classDefault,
	     "Use class-variable value");

  assign(class, no_created, toInt(3));

  succeed;
}

