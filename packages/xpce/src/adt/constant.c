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

