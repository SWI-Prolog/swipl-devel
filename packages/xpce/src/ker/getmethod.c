/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


GetMethod
createGetMethod(Name name, Type rtype, Vector types, StringObj doc, Func action)
{ GetMethod m = alloc(sizeof(struct get_method));

  initHeaderObj(m, ClassGetMethod);
  m->return_type = (Type) NIL;
  assign(m, return_type, rtype);
  createMethod((Method) m, name, types, doc, action);

  return m;
}


status
initialiseGetMethod(GetMethod m, Name name, Type rtype,
		    Vector types, Function msg,
		    StringObj doc, SourceLocation loc, Name group)
{ if ( isDefault(rtype) )
    rtype = TypeUnchecked;

  TRY(initialiseMethod((Method) m, name, types, (Code) msg, doc, loc, group));
  assign(m, return_type, rtype);

  succeed;
}


Any
getGetGetMethod(GetMethod m, Any receiver, int argc, const Any argv[])
{ Any rval, rv2;
  AnswerMark mark;
  goal goal;
  Goal g = &goal;

  pushGoal(g, m, receiver, m->name, argc, argv);
  traceEnter(g);

  markAnswerStack(mark);

  Mode(MODE_SYSTEM,
       rval = invokeMethod((Method) m, NAME_get, receiver, argc, argv);
       if ( rval )
       { if ( !(rv2 = checkType(rval, m->return_type, NIL)) )
	   errorPce(m, NAME_badReturnValue, rval, m->return_type);
         else if ( rv2 != rval )
	   errorPce(m, NAME_convertedReturnValue, rval, rv2);
       } else
         rv2 = rval;
       rewindAnswerStack(mark, rv2));

  traceAnswer(g, rv2);
  popGoal();

  return rv2;
}


status
makeClassGetMethod(Class class)
{ sourceClass(class, makeClassGetMethod, __FILE__, "$Revision$");

  localClass(class, NAME_returnType, NAME_type, "type", NAME_get,
	     "Tupe of value returned");

  termClass(class, "get_method",
	    6, NAME_name, NAME_returnType, NAME_types,
	       NAME_message, NAME_summary, NAME_source);

  sendMethod(class, NAME_initialise, DEFAULT, 7,
	     "name=name", "return=[type]", "types=[vector]",
	     "implementation=function", "summary=[string]*",
	     "source=[source_location]*", "group=[name]*",
	     "->selector, return_type, types, msg, doc, location",
	     initialiseGetMethod);

  getMethod(class, NAME_get, NAME_execute, "value=unchecked", 2,
	    "receiver=object", "argument=unchecked ...",
	    "Invoke get-method",
	    getGetGetMethod);

  succeed;
}
