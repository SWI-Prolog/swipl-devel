/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (F) 1992 University of Amsterdam. All rights reserved.
*/

#define INLINE_UTILITIES 1
#include <h/kernel.h>

status
initialiseFunction(Function f)
{ setFlag(f, F_ACTIVE);

  return initialiseCode((Code) f);
}


static Function
getConvertFunction(Class class, QuoteFunction q)
{ answer(q->function);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
				FORWARDING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getForwardFunctionv(Function f, int argc, Any *argv)
{ Any rval;

  withArgs(argc, argv, rval = getExecuteFunction(f));

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		      FORWARDING WITH PUSH OF @RECEIVER

The test of `if ( RECEIVER->value != receiver )' is dubious: we should
check whether the message actually is send to @receiver
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getForwardReceiverFunctionv(Function f, Any receiver, int argc, Any *argv)
{ if ( RECEIVER->value != receiver )
  { Any receiver_save = RECEIVER->value;
    Any receiver_class_save = RECEIVER_CLASS->value;
    Any rval;

    RECEIVER->value = receiver;
    RECEIVER_CLASS->value = classOfObject(receiver);
    rval = getForwardFunctionv(f, argc, argv);
    RECEIVER_CLASS->value = receiver_class_save;
    RECEIVER->value = receiver_save;

    return rval;
  } else
    return getForwardFunctionv(f, argc, argv);  
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  ARGLIST FUNCTION INVOKATION
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getForwardFunction(Function f, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, f);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);
  
  return getForwardFunctionv(f, argc, argv);
}


Any
getForwardReceiverFunction(Function f, Any receiver, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, receiver);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);
  
  return getForwardReceiverFunctionv(f, receiver, argc, argv);
}

static Any
getNoExecuteFunction(Function f)
{ errorPce(f, NAME_cannotExecute);
  
  fail;
}


Any
getSendMethodFunction(Function f, Name selector)
{ Behaviour b;

  if ( (b = getMemberHashTable(classOfObject(f)->send_table,
			       selector)) &&
       notNil(b) &&
       isAClass(b->context, ClassFunction) )
    answer(b);

  fail;
}


Any
getGetMethodFunction(Function f, Name selector)
{ Behaviour b;

  if ( (b = getMemberHashTable(classOfObject(f)->get_table,
			       selector)) &&
       notNil(b) &&
       isAClass(b->context, ClassFunction) )
    answer(b);

  fail;
}


status
makeClassFunction(Class class)
{ sourceClass(class, makeClassFunction, __FILE__, "$Revision$");

  termClass(class, "function", 0);

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Initialise function",
	     initialiseFunction);
  sendMethod(class, NAME_Free, DEFAULT, 0,
	     "Free obtainer instead of result",
	     freeObject);
#ifndef O_RUNTIME
  sendMethod(class, NAME_Inspect, DEFAULT, 1, "bool",
	     "Inspect obtainer instead of result",
	     inspectObject);
#endif
  sendMethod(class, NAME_InstanceOf, DEFAULT, 1, "class",
	     "Test class of obtainer instead of result",
	     instanceOfObject);
  sendMethod(class, NAME_Check, DEFAULT, 1, "[bool]",
	     "Check obtainer instead of result",
	     CheckObject);
  sendMethod(class, NAME_SameReference, NAME_function, 1, "to=any|function",
	     "Test equivalence instead of result",
	     sameReferenceObject);
  sendMethod(class, NAME_NameReference, NAME_reference, 1, "name*",
	     "Change named (atomic) reference",
	     nameReferenceObject);
  sendMethod(class, NAME_initialiseNewSlot, DEFAULT, 1, "variable",
	     "Initialise <-dflags",
	     initialiseNewSlotProgramObject);
  sendMethod(class, NAME_convertLoadedObject, DEFAULT, 2, "int", "int",
	     "Called by File <-object if conversion might be required",
	     convertLoadedObjectObject);
  sendMethod(class, NAME_Error, NAME_report, 2,
	     "error=error", "context=any ...",
	     "Initiate an error",
	     errorObjectv);
  sendMethod(class, NAME_sendSuper, NAME_programming, 2,
	     "selector=name", "argument=unchecked ...",
	     "Send using method of super-class",
	     sendSuperObject);

  getMethod(class, NAME_convert, DEFAULT, "function", 1,"quote=quote_function",
	    "Convert quoted function to quoted value",
	    getConvertFunction);
  getMethod(class, NAME_ClassName, DEFAULT, "name", 0,
	    "Class name instead of result's",
	    getClassNameObject);
  getMethod(class, NAME_Class, DEFAULT, "class", 0,
	    "Class instead of result's",
	    getClassObject);
  getMethod(class, NAME_Slot, DEFAULT, "any", 1, "name|int",
	    "Slot value instead of result's",
	    getSlotObject);
  getMethod(class, NAME_References, DEFAULT, "int", 0,
	    "Reference count instead of result's",
	    getReferencesObject);

  getMethod(class, NAME_functor, DEFAULT, "name", 0,
	    "Functor of term instead of result's",
	    getFunctorObject);
  getMethod(class, NAME_Arg, DEFAULT, "unchecked", 1, "int",
	    "Nth-1 argument of term instead of result's",
	    getArgObject);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity of term instead of result's",
	    getArityObject);
  getMethod(class, NAME_Flags, DEFAULT, "name", 0,
	    "Flags instead of result's",
	    getFlagsObject);
#ifndef O_RUNTIME
  getMethod(class, NAME_Inspect, DEFAULT, "bool", 0,
	    "Inspect instead of result's",
	    getInspectObject);
  getMethod(class, NAME_ManId, DEFAULT, "name", 0,
	    "Manual identifier instead of result's",
	    getManIdObject);
#endif

  getMethod(class,  NAME_forward, NAME_execute, "unchecked", 1, "any ...",
	    "Push @arg1, ...; execute and return value",
	    getForwardFunctionv);
  getMethod(class,  NAME_Forward, NAME_execute, "unchecked", 1, "any ...",
	    "Push @arg1, ...; execute and return value",
	    getForwardFunctionv);
  getMethod(class,  NAME_execute, NAME_execute, "unchecked", 0,
	    "Execute the function object (yielding value)",
	    getExecuteFunction);
  getMethod(class,  NAME_Execute, NAME_execute, "unchecked", 0,
	    "Execute and return value",
	    getNoExecuteFunction);

  succeed;
}

