/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define INLINE_UTILITIES 1
#include <h/kernel.h>

status
initialiseCode(Code c)
{ return initialiseProgramObject(c);
}

		/********************************
		*           FORWARDING		*
		********************************/

static status
forwardVarsCodev(Code c, int argc, Assignment *argv)
{ status rval;
  int errors = 0;
  int i;

  withLocalVars({ for(i=0; i<argc; i++, argv++)
		  { Any value;

		    if ( (value = expandCodeArgument(argv[0]->value)) )
		    { assignVar(argv[0]->var, value, NAME_local);
		      if ( argv[0]->var == RECEIVER && isObject(value) )
			assignVar(RECEIVER_CLASS, classOfObject(value),
				  NAME_local);
		    } else
		    { errors++;
		      break;
		    }
		  }

		  rval = (errors ? FAIL : executeCode(c));
		});

  return rval;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		      FORWARDING WITH PUSH OF @RECEIVER

The test of `if ( RECEIVER->value != receiver )' is dubious: we should
check whether the message actually is send to @receiver
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
forwardReceiverCodev(Code c, Any receiver, int argc, const Any argv[])
{ if ( RECEIVER->value != receiver )
  { Any receiver_save = RECEIVER->value;
    Any receiver_class_save = RECEIVER_CLASS->value;
    status rval;

    RECEIVER->value = receiver;
    RECEIVER_CLASS->value = classOfObject(receiver);
    rval = forwardCodev(c, argc, argv);
    RECEIVER_CLASS->value = receiver_class_save;
    RECEIVER->value = receiver_save;

    return rval;
  } else
    return forwardCodev(c, argc, argv);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  VECTOR BASED FORWARDING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
forwardVectorCodev(Code c, int argc, const Any argv[])
{ Vector v;
  int shift;
  int args;

  if ( argc == 0 )
    goto usage;
  if ( argc >= 2 && isInteger(argv[argc-1]) )
  { v = argv[argc-2];
    shift = valInt(argv[argc-1]);
    args = argc-2;
  } else
  { v = argv[argc-1];
    shift = 0;
    args = argc-1;
  }

  if ( !instanceOfObject(v, ClassVector) )
    goto usage;
  else
  { int argn = args+valInt(v->size)-shift;
    ArgVector(av, args+valInt(v->size)-shift);
    int i, n;

    for(i=0; i<args; i++)
      av[i] = argv[i];
    for(n=shift; n<=valInt(v->size); n++)
      av[i++] = v->elements[n];

    return forwardCodev(c, argn, av);
  }

usage:
  return errorPce(c, NAME_badVectorUsage);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  ARGLIST CODE INVOKATION
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
forwardCode(Code c, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, c);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);
  
  return forwardCodev(c, argc, argv);
}


status
forwardReceiverCode(Code c, Any rec, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, rec);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);
  
  return forwardReceiverCodev(c, rec, argc, argv);
}


static status
ExecuteCode(Code c)
{ Class cl = classOfObject(c);

  if ( cl->get_function )
    return (*cl->get_function)(c) ? SUCCEED : FAIL;

  return errorPce(c, NAME_cannotExecute);
}

static Any
getExecuteCode(Code c)
{ errorPce(c, NAME_noFunction);

  fail;
}


static void
traceCode(Code c)
{ Any f;
  int arity;

  f = get(c, NAME_functor, 0);
  writef("%s", f);
  arity = valInt(get(c, NAME_Arity, 0));
  if ( arity > 0 )
  { int i;

    writef("(");
    for(i=1; i<=arity; i++)
    { Any arg = get(c, NAME_Arg, toInt(i), 0);

      if ( i == 1 )
	writef("%O", arg);
      else
	writef(", %O", arg);
    }
    writef(")");
  }
}


		/********************************
		*         CLASS CODE_VECTOR	*
		********************************/

Vector
createCodeVectorv(int argc, const Any argv[])
{ Vector v = alloc(sizeof(struct vector));
  int n;

  initHeaderObj(v, ClassCodeVector);
  v->offset      = ZERO;
  v->size        = toInt(argc);
  v->elements    = alloc(argc * sizeof(Any));

  for(n=0; n < argc; n++)
  { v->elements[n] = argv[n];
    if ( isObject(argv[n]) && !isProtectedObj(argv[n]) )
      addRefObj(argv[n]);
  }

  clearCreatingObj(v);

  return v;
}


static status
unlinkCodeVector(Vector v)
{ if ( v->elements != NULL )
  { int size = valInt(v->size);
    int n;
    Any *argv = v->elements;

    for(n=0; n<size; n++)
    { if ( isObject(argv[n]) && !isProtectedObj(argv[n]) )
	delRefObj(argv[n]);
    }

    unalloc(valInt(v->size)*sizeof(Any), v->elements);
    v->elements = NULL;
  }

  succeed;
}


void
doneCodeVector(Vector v)
{ if ( isVirginObj(v) )
  { unlinkCodeVector(v);
    unalloc(sizeof(struct vector), v);
  }
}


status
makeClassCodeVector(Class class)
{ assign(class, un_answer, OFF);
  assign(class, summary, CtoString("Argument vector"));

  sourceClass(class, makeClassCodeVector, __FILE__, "$Revision$");

  sendMethod(class, NAME_initialise, DEFAULT, 1, "element=any|function ...",
	     "",
	     initialiseVectorv);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "",
	     unlinkCodeVector);
  sendMethod(class, NAME_fill, DEFAULT, 3,
	     "value=any|function", "from=[int]", "to=[int]",
	     "",
	     fillVector);
  sendMethod(class, NAME_element, DEFAULT, 2,
	     "index=int", "value=any|function",
	     "",
	     elementVector);
  sendMethod(class, NAME_append, DEFAULT, 1,
	     "value=any|function ...",
	     "",
	     appendVector);

  succeed;
}


status
makeClassCode(Class class)
{ sourceClass(class, makeClassCode, __FILE__, "$Revision$");
  setTraceFunctionClass(class, traceCode);
  termClass(class, "code", 0);
  cloneStyleClass(class, NAME_none);
  assign(class, un_answer, OFF);

  sendMethod(class, NAME_forward, NAME_execute, 1, "any ...",
	     "Push @arg1, ... and execute",
	     forwardCodev);
  sendMethod(class, NAME_forwardVars, NAME_execute, 1, "assign ...",
	     "Push vars and execute",
	     forwardVarsCodev);
  sendMethod(class, NAME_forwardVector, NAME_execute, 1, "any ...",
	     "Push @arg1, ... from a vector and execute",
	     forwardVectorCodev);
  sendMethod(class, NAME_execute, NAME_execute, 0,
	     "Execute code",
	     executeCode);
  sendMethod(class, NAME_Execute, NAME_internal, 0,
	     "Execute the code object (redefined)",
	     ExecuteCode);

  getMethod(class, NAME_Execute, NAME_internal, "unchecked", 0,
	     "Execute the function object (error)",
	     getExecuteCode);

  succeed;
}

