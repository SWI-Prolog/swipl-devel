/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

status
initialiseBehaviour(Behaviour b, Name name, Any ctx)
{ initialiseProgramObject(b);

  if ( isDefault(ctx) )
    ctx = NIL;

  assign(b, name, name);
  assign(b, context, ctx);

  succeed;
}


status
makeClassBehaviour(Class class)
{ localClass(class, NAME_name, NAME_name, "name", NAME_get,
	     "Selector of this behaviour");
  localClass(class, NAME_context, NAME_whole, "class|object*", NAME_get,
	     "Definition context of this method");
  
  sourceClass(class, makeClassBehaviour, __FILE__, "$Revision$");
  termClass(class, "behaviour", 2, NAME_name, NAME_context);
  cloneStyleVariableClass(class, NAME_context, NAME_reference);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "name=name", "context=[class|object*]",
	     "Create from <-name and <-context",
	     initialiseBehaviour);

  succeed;
}

