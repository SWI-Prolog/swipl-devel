/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>

static status
initialiseAttribute(Attribute att, Any name, Any value)
{ initialiseProgramObject(att);

  assign(att, name, name);
  assign(att, value, value);

  succeed;
}


static Attribute
getConvertAttribute(Class class, Any name)
{ answer(answerObject(ClassAttribute, name, NIL, 0));
}


static status
sendAttribute(Attribute att, Any rec, int argc, Any *argv)
{ goal goal;
  Goal g = &goal;
  status rval = SUCCEED;

  pushGoal(g, att, rec, att->name, argc, argv);
  traceEnter(g);
  if ( argc != 1 )
  { errorPce(att, NAME_argumentCount, ONE);
    failGoal;
  }
  assign(att, value, argv[0]);

out:
  traceReturn(g, rval);
  popGoal();

  return rval;
}


static Any
getAttribute(Attribute att, Any rec, int argc, Any *argv)
{ goal goal;
  Goal g = &goal;
  Any rval;

  pushGoal(g, att, rec, att->name, argc, argv);
  traceEnter(g);
  if ( argc != 0 )
  { errorPce(att, NAME_argumentCount, ONE);
    failGoal;
  }
  rval = att->value;

out:
  traceAnswer(g, rval);
  popGoal();

  answer(rval);
}


		/********************************
		*            TRACING		*
		********************************/

#ifndef O_RUNTIME
static void
traceAttribute(Attribute att, Goal g, Name port)
{ int i;

  writef("A %O <->%s: ", g->receiver, att->name);
  for(i = 0; i < g->argc; i++)
  { if ( i == 0 )
      writef("%O", g->argv[i]);
    else
      writef(", %O", g->argv[i]);
  }
}
#endif


static Type
getArgumentTypeAttribute(Attribute att, Int n)
{ if ( isDefault(n) || n == ONE )
    answer(TypeAny);

  fail;
}


status
makeClassAttribute(Class class)
{ sourceClass(class, makeClassAttribute, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_storage, "any", NAME_both,
	     "Name of the attribute");
  localClass(class, NAME_value, NAME_storage, "any", NAME_both,
	     "Value of the attribute");

  termClass(class, "attribute", 2, NAME_name, NAME_value);
  setTraceFunctionClass(class, traceAttribute);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "name=any", "value=any",
	     "Create attribute from name and value",
	     initialiseAttribute);
  sendMethod(class, NAME_send, NAME_execute, 2,
	     "context=object", "argument=unchecked ...",
	     "Invoke (write) object-attribute",
	     sendAttribute);

  getMethod(class, NAME_convert, DEFAULT, "attribute", 1, "any",
	    "Converts name to attribute(name, @nil)",
	    getConvertAttribute);
  getMethod(class, NAME_get, NAME_execute, "value=unchecked", 2,
	    "context=object", "argument=unchecked ...",
	     "Invoke (read) object-attribute",
	     getAttribute);
  getMethod(class, NAME_argumentType, NAME_meta, "type", 1, "index=[int]",
	    "Type of n-th1 argument",
	    getArgumentTypeAttribute);

  succeed;
}

