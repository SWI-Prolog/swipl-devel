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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_contextAobject_argumentAunchecked_XXX[] =
        { "context=object", "argument=unchecked ..." };
static char *T_initialise[] =
        { "name=any", "value=any" };

/* Instance Variables */

static vardecl var_attribute[] =
{ IV(NAME_name, "any", IV_BOTH,
     NAME_storage, "Name of the attribute"),
  IV(NAME_value, "any", IV_BOTH,
     NAME_storage, "Value of the attribute")
};

/* Send Methods */

static senddecl send_attribute[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseAttribute,
     DEFAULT, "Create attribute from name and value"),
  SM(NAME_send, 2, T_contextAobject_argumentAunchecked_XXX, sendAttribute,
     NAME_execute, "Invoke (write) object-attribute")
};

/* Get Methods */

static getdecl get_attribute[] =
{ GM(NAME_convert, 1, "attribute", "any", getConvertAttribute,
     DEFAULT, "Converts name to attribute(name, @nil)"),
  GM(NAME_get, 2, "value=unchecked", T_contextAobject_argumentAunchecked_XXX, getAttribute,
     NAME_execute, "Invoke (read) object-attribute"),
  GM(NAME_argumentType, 1, "type", "index=[int]", getArgumentTypeAttribute,
     NAME_meta, "Type of n-th1 argument")
};

/* Resources */

#define rc_attribute NULL
/*
static resourcedecl rc_attribute[] =
{ 
};
*/

/* Class Declaration */

static Name attribute_termnames[] = { NAME_name, NAME_value };

ClassDecl(attribute_decls,
          var_attribute, send_attribute, get_attribute, rc_attribute,
          2, attribute_termnames,
          "$Rev$");


status
makeClassAttribute(Class class)
{ declareClass(class, &attribute_decls);
  setTraceFunctionClass(class, traceAttribute);

  succeed;
}

