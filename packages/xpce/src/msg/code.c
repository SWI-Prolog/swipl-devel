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

#define INLINE_UTILITIES 1
#include <h/kernel.h>

status
initialiseCode(Code c)
{ return initialiseProgramObject(c);
}


static Function
getConvertCode(Class class, QuoteFunction q)
{ answer(q->function);
}


static status
debugClassCode(Code c, Name cls)
{ if ( cls == NAME_user )
    clearDFlag(c, D_SERVICE);
  else
    setDFlag(c, D_SERVICE);

  succeed;
}


static Name
getDebugClassCode(Code c)
{ if ( onDFlag(c, D_SERVICE) )
    return NAME_service;
  else
    return NAME_user;
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

  FixGetFunctionClass(cl, NAME_Execute);
  if ( cl->get_function )
  { status rval;

    if ( onDFlag(c, D_SERVICE) )
    { ServiceMode(PCE_EXEC_SERVICE,
		  rval = ((*cl->get_function)(c) ? SUCCEED : FAIL));
    } else
      rval = (*cl->get_function)(c) ? SUCCEED : FAIL;

    return rval;
  }

  return errorPce(c, NAME_cannotExecute);
}

static Any
getExecuteCode(Code c)
{ errorPce(c, NAME_noFunction);

  fail;
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
  v->allocated   = v->size;
  v->elements    = alloc(argc * sizeof(Any));

  for(n=0; n < argc; n++)
  { v->elements[n] = argv[n];
    if ( isObject(argv[n]) && !isProtectedObj(argv[n]) )
      addRefObj(argv[n]);
  }

  clearCreatingObj(v);

  return v;
}


static Vector
getConvertCodeVector(Any ctx, Any in)
{ if ( in == name_nil )
    answer(createCodeVectorv(0, NULL));

  fail;
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

    unalloc(valInt(v->allocated)*sizeof(Any), v->elements);
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_element[] =
        { "index=int", "value=any|function" };
static char *T_fill[] =
        { "value=any|function", "from=[int]", "to=[int]" };

/* Instance Variables */

#define var_codeVector NULL
/*
static vardecl var_codeVector[] =
{ 
};
*/

/* Send Methods */

static senddecl send_codeVector[] =
{ SM(NAME_append, 1, "value=any|function ...", appendVector,
     DEFAULT, NULL),
  SM(NAME_element, 2, T_element, elementVector,
     DEFAULT, NULL),
  SM(NAME_fill, 3, T_fill, fillVector,
     DEFAULT, NULL),
  SM(NAME_initialise, 1, "element=any|function ...", initialiseVectorv,
     DEFAULT, NULL),
  SM(NAME_unlink, 0, NULL, unlinkCodeVector,
     DEFAULT, NULL)
};

/* Get Methods */

static getdecl get_codeVector[] =
{ GM(NAME_convert, 1, "code_vector", "any", getConvertCodeVector,
     DEFAULT, "Convert [] into empty code-vector"),

};

/* Resources */

#define rc_codeVector NULL
/*
static classvardecl rc_codeVector[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(codeVector_decls,
          var_codeVector, send_codeVector, get_codeVector, rc_codeVector,
          ARGC_INHERIT, NULL,
          "$Rev$");

status
makeClassCodeVector(Class class)
{ declareClass(class, &codeVector_decls);

  assign(class, un_answer, OFF);
  assign(class, summary, CtoString("Argument vector"));

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

#define var_code NULL
/*
static vardecl var_code[] =
{ 
};
*/

/* Send Methods */

static senddecl send_code[] =
{ SM(NAME_execute, 0, NULL, executeCode,
     NAME_execute, "Execute code"),
  SM(NAME_forward, 1, "any ...", forwardCodev,
     NAME_execute, "Push @arg1, ... and execute"),
  SM(NAME_forwardVars, 1, "assign ...", forwardVarsCodev,
     NAME_execute, "Push vars and execute"),
  SM(NAME_forwardVector, 1, "any ...", forwardVectorCodev,
     NAME_execute, "Push @arg1, ... from a vector and execute"),
  SM(NAME_Execute, 0, NULL, ExecuteCode,
     NAME_internal, "Execute the code object (redefined)"),
  SM(NAME_debugClass, 1, "{user,service}", debugClassCode,
     NAME_debugging, "Specify debug-capabilities")
};

/* Get Methods */

static getdecl get_code[] =
{ GM(NAME_Execute, 0, "unchecked", NULL, getExecuteCode,
     NAME_internal, "Execute the function object (error)"),
  GM(NAME_convert, 1, "function", "quote=quote_function", getConvertCode,
     DEFAULT, "Convert quoted function to value quoted"),
  GM(NAME_debugClass, 0, "{user,service}", NULL, getDebugClassCode,
     NAME_debugging, "Specify debug-capabilities")
};

/* Resources */

#define rc_code NULL
/*
static classvardecl rc_code[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(code_decls,
          var_code, send_code, get_code, rc_code,
          0, NULL,
          "$Rev$");


status
makeClassCode(Class class)
{ declareClass(class, &code_decls);

  cloneStyleClass(class, NAME_none);
  assign(class, un_answer, OFF);

  succeed;
}

