/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define INLINE_UTILITIES 1
#include <h/kernel.h>
#include <h/trace.h>
#include <h/interface.h>
#include <itf/c.h>
#include <h/unix.h>

static status	typesMethod(Method m, Vector types);
static Name	getContextNameMethod(Method m);
static Name	getAccessArrowMethod(Method m);

status
createMethod(Method m, Name name, Vector types, StringObj doc, Func action)
{ m->name        = name;
  m->group 	 = NIL;
  m->message	 = NIL;
  m->types	 = NIL;
  m->function    = action;
  m->summary	 = NIL;
  m->context     = NIL;
#ifndef O_RUNTIME
  m->source	 = NIL;
#endif
  m->dflags      = (ulong) ZERO;

  initialiseMethod(m, name, types, NIL, doc, NIL, DEFAULT);
  createdObject(m, NAME_new);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a new method object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
initialiseMethod(Method m, Name name, Vector types, Code msg, StringObj doc, SourceLocation loc, Name group)
{ initialiseBehaviour((Behaviour) m, name, NIL);

  if ( isDefault(loc) )
    loc = NIL;

  assign(m, group,   group);
  assign(m, message, msg);
  assign(m, summary, doc);
#ifndef O_RUNTIME
  assign(m, source,  loc);
#endif

  return typesMethod(m, types);
}


static status
storeFdMethod(Method m, FileObj file)
{ if ( storeSlotsObject(m, file) &&
       storeWordFile(file, (Any)m->function))
    succeed;

  fail;
}


static status
loadMethod(Method m, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(m, fd, def));
  m->function = (Func) loadWord(fd);;

  succeed;
}


static Method
getInstantiateTemplateMethod(Method m)
{ Method m2 = getCloneObject(m);

  if ( m2 )
  { setFlag(m2, F_TEMPLATE_METHOD);
    assign(m2, context, NIL);
  }

  answer(m2);
}


static status
typesMethod(Method m, Vector types)
{ int n;
  Type type;

  if ( isDefault(types) )
  { assign(m, types, newObject(ClassVector, 0));
    succeed;
  }

  for(n = 1; n <= valInt(types->size); n++)
  { Any elm = getElementVector(types, toInt(n));

    if ( !(type = toType(elm)) )
      return errorPce(types, NAME_elementType, toInt(n), TypeType);

    if ( type != elm )
      elementVector(types, toInt(n), type);
  }

  assign(m, types, types);
  succeed;
}


static Int
getArgumentCountMethod(Method m)
{ Type type;

  if ( (type = getTailVector(m->types)) && type->vector == ON )
    answer(sub(m->types->size, ONE));
  else
    answer(m->types->size);
}


Type
getArgumentTypeMethod(Method m, Int n)
{ Type type;

  if ( (type = getElementVector(m->types, n)) )
    answer(type);

  if ( (type = getTailVector(m->types)) && type->vector == ON )
    answer(type);

  fail;
}
  

#define isBinding(obj)	(isObject(obj) && onFlag(obj, F_ISBINDING))

Any
invokeMethod(Method m, Name c, Any receiver, int argc, const Any argv[])
{ Any rval;
  Type *types = (Type *)m->types->elements;
  int ntypes = valInt(m->types->size);
  int vararg = ((ntypes > 0 && types[ntypes-1]->vector == ON) ? ntypes - 1
							      : -1);
  int ac = (vararg >= 0 ? ntypes + 1 : ntypes);
  Any *av = (Any *) alloca(sizeof(Any) * ac);
  const Any *aie = &argv[argc], *ai;
  Any *aoe = &av[ntypes], *ao;
  Type *t, *te = &types[ntypes];
  int i;

  ExecuteCalls++;
#ifdef __WIN32__
  { static int tick;

    if ( !(++tick & 0xff) )
      ws_check_intr();			/* check for interrupt */
  }
#endif
					/* Resolve selector */
					/* initialise arguments */
  for(i=ntypes, ao = av; i--; ao++)
    *ao = DEFAULT;

  if ( vararg >= 0 )
  { av[vararg]   = (Any) 0;
    av[vararg+1] = (Any *) alloca(sizeof(Any) * argc);

    aoe--;
    for( ai = argv, ao = av; ai < aie; )
    { if ( ao == aoe )
      { Any *sav = aoe[1];
	int sac = 0;

	for( ; ai < aie; ai++)
	{ if ( !(sav[sac++] = CheckType(*ai, types[vararg], receiver)) )
	  { if ( offDFlag(m, D_TYPENOWARN) &&
		 CheckTypeError != CTE_OBTAINER_FAILED )
	      errorTypeMismatch(receiver, m, ntypes, types[vararg]);
	    fail;
	  }
	}	
	*ao = (Any) sac;
	break;
      } else if ( isBinding(*ai) )
	break;
      else
	*ao++ = *ai++;
    }
  } else
  { for( ai = argv, ao = av; ai < aie && !isBinding(*ai); )
    { if ( ao >= aoe )
      { if ( offDFlag(m, D_TYPENOWARN) )
	  errorPce(m, NAME_argumentCount, m->types->size);
	fail;
      }
      *ao++ = *ai++;
    }
  }

					/* bound arguments */
#define ADDVARARG(v) { int idx = (int) av[vararg]; \
		       ((Any *) av[vararg+1])[idx++] = (v); \
		       av[vararg] = (Any) idx; \
		     }

  for( ; ai < aie; ai++)
  { Binding b = *ai;

    if ( !isBinding(b) )
    { errorPce(m, NAME_unboundAfterBoundArgument);
      fail;
    }
      
    for(t = types; t < te; t++)
    { if ( (*t)->argument_name == b->name )
      { int index = t-types;

	if ( index == vararg )
	{ ADDVARARG(b->value);
	} else
	  av[index] = b->value;

	goto next_arg;
      }
    }
    if ( vararg >= 0 )			/* add them to the vararg */
    { Any v;

      if ( (v = CheckType(b, types[vararg], receiver)) )
      { ADDVARARG(v);
	goto next_arg;
      }
    }
    errorPce(m, NAME_noNamedArgument, b->name);
    fail;

  next_arg:;
  }

					/* Check argument types */
  for( ao=av, t = types; ao < aoe; ao++, t++)
  { if ( !(*ao = CheckType(*ao, *t, receiver)) )
    { if ( offDFlag(m, D_TYPENOWARN) &&
	  CheckTypeError != CTE_OBTAINER_FAILED )
	errorTypeMismatch(receiver, m, t - types + 1, *t);
      fail;
    }
  }

  if ( m->function )
  { Func f = m->function;
    Any rec = receiver;

#if O_CPLUSPLUS
    if ( onDFlag(m, D_CXX) )
    { Any receiver_save;
      Class receiver_class_save;

      if ( vararg >= 0 )
      { av[vararg] = createCodeVectorv((int)av[vararg], (Any *)av[vararg+1]);
	addCodeReference(av[vararg]);
	ac--;
      }

      receiver_save = RECEIVER->value;
      receiver_class_save = RECEIVER_CLASS->value;

      RECEIVER->value = receiver;
      RECEIVER_CLASS->value = m->context;

      if ( c == NAME_send )
	rval = (Any) callCPlusPlusPceMethodProc(rec, f, ac, av);
      else
	rval = callCPlusPlusPceMethodFunc(rec, f, ac, av);

      RECEIVER_CLASS->value = receiver_class_save;
      RECEIVER->value = receiver_save;

      if ( vararg >= 0 )
      { delCodeReference(av[vararg]);
	doneCodeVector(av[vararg]);
      }

      return rval;
    }
#endif

    switch(ac)
    { case 0:
	rval = (*f)(rec); break;
      case 1:
	rval = (*f)(rec,av[0]); break;
      case 2:
	rval = (*f)(rec,av[0],av[1]); break;
      case 3:
	rval = (*f)(rec,av[0],av[1],av[2]); break;
      case 4:
	rval = (*f)(rec,av[0],av[1],av[2],av[3]); break;
      case 5:
	rval = (*f)(rec,av[0],av[1],av[2],av[3],av[4]); break;
      case 6:
	rval = (*f)(rec,av[0],av[1],av[2],av[3],av[4],av[5]); break;
      case 7:
	rval = (*f)(rec,av[0],av[1],av[2],av[3],av[4],av[5],av[6]); break;
      case 8:
	rval = (*f)(rec,av[0],av[1],av[2],av[3],av[4],av[5],av[6],
		    av[7]); break;
      case 9:
	rval = (*f)(rec,av[0],av[1],av[2],av[3],av[4],av[5],av[6],
		    av[7],av[8]); break;
      case 10:
	rval = (*f)(rec,av[0],av[1],av[2],av[3],av[4],av[5],av[6],
		    av[7],av[8],av[9]); break;
      case 11:
	rval = (*f)(rec,av[0],av[1],av[2],av[3],av[4],av[5],av[6],
		    av[7],av[8],av[9],av[10]); break;
      default:
	rval = FAIL;			/* keep compiler happy */
	NOTREACHED;
    }
  } else				/* code object implementation */
  { Any receiver_save;
    Class receiver_class_save;

    if ( isNil(m->message) )
    { errorPce(m, NAME_noImplementation);
      fail;
    }
      
    if ( vararg >= 0 )
    { av[vararg] = createCodeVectorv((int)av[vararg], (Any *)av[vararg+1]);
      addCodeReference(av[vararg]);
    }

    receiver_save = RECEIVER->value;
    receiver_class_save = RECEIVER_CLASS->value;

    RECEIVER->value = receiver;
    RECEIVER_CLASS->value = m->context;

    if ( instanceOfObject(m->message, ClassCode) )
    { if ( c == NAME_send )
	rval = (Any) forwardCodev(m->message, ntypes, av);
      else
	rval = getForwardFunctionv((Function) m->message, ntypes, av);
    } else
    { if ( c == NAME_send )
	rval = (Any) hostCallProc(m->message, receiver, m->name, ntypes, av);
      else
	rval = hostCallFunc(m->message, receiver, m->name, ntypes, av);
    }

    RECEIVER_CLASS->value = receiver_class_save;
    RECEIVER->value = receiver_save;

    if ( vararg >= 0 )
    { delCodeReference(av[vararg]);
      doneCodeVector(av[vararg]);
    }
  }

  return rval;
}



		/********************************
		*            TRACING		*
		********************************/

#ifndef O_RUNTIME
static void
traceMethod(Method m, Goal g, Name port)
{ Vector args;
  
  args = tempObjectv(ClassCodeVector, g->argc, g->argv);
  send(m, NAME_printTraceMessage, port, g->receiver, args, 0);
  considerPreserveObject(args);
}


static status
printTraceMessageMethod(Method m, Name port, Any rec, Vector args)
{ if ( m->context == classOfObject(rec) )
    writef("M %O", rec);
  else
    writef("M %O(%s)", rec, getContextNameMethod(m));

  writef(" %s%s", getAccessArrowMethod(m), m->name);

  if ( args->size != ZERO )
  { int n;

    for(n=0; n<valInt(args->size); n++)
    { if ( n == 0 )
	writef(": ");
      else if ( n != valInt(args->size) )
	writef(", ");

      writef("%O", args->elements[n]);
    }
  }

  succeed;
}
#endif /*O_RUNTIME*/


Method
getInheritedFromMethod(Method m)
{ Class class = m->context;
  int sm = instanceOfObject(m, ClassSendMethod);

  for(class = class->super_class; notNil(class); class = class->super_class)
  { Chain ch = (sm ? class->send_methods : class->get_methods);
    Cell cell;

    for_cell(cell, ch)
    { Method m2 = cell->value;

      if ( m2->name == m->name )
	answer(m2);
    }
  }

  fail;
}



static Name
getGroupMethod(Method m)
{ if ( isDefault(m->group) )
  { Class class = m->context;
    int sm = instanceOfObject(m, ClassSendMethod);

    while( instanceOfObject(class, ClassClass) )
    { Vector v = class->instance_variables;
      int n;

      for(n=0; n<valInt(v->size); n++)
      { Variable var = v->elements[n];

	if ( var->name == m->name && notDefault(var->group) )
	  answer(var->group);
      }

      if ( notNil(class = class->super_class) )
      { Chain ch = (sm ? class->send_methods : class->get_methods);
	Cell cell;

	for_cell(cell, ch)
	{ Method m2 = cell->value;

	  if ( m2->name == m->name && notDefault(m2->group) )
	    answer(m2->group);
	}
      }
    }

    fail;
  }

  answer(m->group);
}

		/********************************
		*         MANUAL SUPPORT	*
		********************************/

static Name
getContextNameMethod(Method m)
{ if ( instanceOfObject(m->context, ClassClass) )
  { Class class = m->context;

    answer(class->name);
  }

  answer(CtoName("SELF"));
}


static Name
getAccessArrowMethod(Method m)
{ if ( instanceOfObject(m, ClassSendMethod) )
    answer(CtoName("->"));
  else
    answer(CtoName("<-"));
}


static StringObj
getSummaryMethod(Method m)
{ if ( isNil(m->summary) )
    fail;
  if ( notDefault(m->summary) )
    answer(m->summary);
  else
  { Class class = m->context;

    if ( instanceOfObject(class, ClassClass) )
    { Variable var;

      if ( (var = getInstanceVariableClass(class, m->name)) &&
	   instanceOfObject(var->summary, ClassCharArray) )
	answer(var->summary);
      while( (m = getInheritedFromMethod(m)) )
	if ( instanceOfObject(m->summary, ClassCharArray) )
	  answer(m->summary);
    }
  }

  fail;
}


#ifndef O_RUNTIME
static Name
getManIdMethod(Method m)
{ char buf[LINESIZE];

  sprintf(buf, "M.%s.%c.%s",
	  strName(getContextNameMethod(m)),
	  instanceOfObject(m, ClassSendMethod) ? 'S' : 'G',
	  strName(m->name));

  answer(CtoName(buf));
}


static Name
getManIndicatorMethod(Method m)
{ answer(CtoName("M"));
}


static StringObj
getManSummaryMethod(Method m)
{ char buf[LINESIZE];
  Vector types = m->types;
  StringObj s;
  char *e;

  buf[0] = EOS;
  strcat(buf, "M\t");

  strcat(buf, strName(getContextNameMethod(m)));
  strcat(buf, " ");
  
  strcat(buf, strName(getAccessArrowMethod(m)));
  strcat(buf, strName(m->name));

  e = buf + strlen(buf);
  if ( types->size != ZERO )
  { int i;

    strcat(e, ": ");
    for(i = 1; i <= valInt(types->size); i++)
    { Type t = getElementVector(types, toInt(i));

      if ( i != 1 )
	strcat(e, ", ");

      strcat(e, strName(t->fullname));
    }
  }

  if ( instanceOfObject(m, ClassGetMethod) )
  { GetMethod gm = (GetMethod) m;

    strcat(e, " -->");
    strcat(e, strName(gm->return_type->fullname));
  }

  if ( (s = getSummaryMethod(m)) )
  { strcat(buf, "\t");
    strcat(buf, strName(s));
  }
  if ( send(m, NAME_manDocumented, 0) )
    strcat(buf, " (+)");

  answer(CtoString(buf));
}

#else

static status
rtSourceMethod(Method m, SourceLocation src)
{ succeed;
}

#endif /*O_RUNTIME*/


Method
getMethodFromFunction(Any f)
{ for_hash_table(classTable, s,
	         { Class class = s->value;

		   if ( class->realised == ON )
		   { Cell cell;

		     for_cell(cell, class->send_methods)
		     { Method m = cell->value;
		       if ( (Any) m->function == f )
			 answer(m);
		     }
		     for_cell(cell, class->get_methods)
		     { Method m = cell->value;
		       if ( (Any) m->function == f )
			 answer(m);
		     }
		   }
		 });

  answer(NIL);
}


static Name
getPrintNameMethod(Method m)
{ char buf[LINESIZE];

  sprintf(buf, "%s %s%s",
	  strName(getContextNameMethod(m)),
	  strName(getAccessArrowMethod(m)),
	  strName(m->name));

  answer(CtoName(buf));
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=name", "types=[vector]",
	  "implementation=code|c_pointer", "summary=[string]*",
	  "source=[source_location]*", "group=[name]*" };
#ifndef O_RUNTIME
static char *T_printTraceMessage[] =
        { "port={enter,exit,fail}*", "receiver=unchecked",
	  "arguments=vector" };
#endif

/* Instance Variables */

static vardecl var_method[] =
{ IV(NAME_group, "[name]", IV_NONE,
     NAME_manual, "Conceptual group of method"),
  IV(NAME_types, "vector", IV_GET,
     NAME_type, "Argument type specification"),
  IV(NAME_summary, "[string]*", IV_NONE,
     NAME_manual, "Summary documentation"),
#ifndef O_RUNTIME
  IV(NAME_source, "source_location*", IV_BOTH,
     NAME_manual, "Location of definition in the sources"),
#endif
  IV(NAME_message, "code|c_pointer*", IV_BOTH,
     NAME_implementation, "If implemented in PCE: the code object"),
  IV(NAME_function, "alien:Func", IV_NONE,
     NAME_implementation, "If implemented in C: function pointer")
};

/* Send Methods */

static senddecl send_method[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseMethod,
     DEFAULT, "Create from name, types, code and doc"),
#ifndef O_RUNTIME
  SM(NAME_printTraceMessage, 3, T_printTraceMessage, printTraceMessageMethod,
     NAME_debugging, "Print standard trace message"),
#else /*O_RUNTIME*/
  SM(NAME_source, 1, "source_location*", rtSourceMethod,
     NAME_runtime, "Dummy method"),
#endif /*O_RUNTIME*/
  SM(NAME_types, 1, "[vector]", typesMethod,
     NAME_type, "Set type-check")
};

/* Get Methods */

static getdecl get_method[] =
{ GM(NAME_summary, 0, "string", NULL, getSummaryMethod,
     DEFAULT, "<-summary or try to infer summary"),
  GM(NAME_accessArrow, 0, "{<-,->}", NULL, getAccessArrowMethod,
     NAME_manual, "Arrow indicating send- or get-access"),
  GM(NAME_group, 0, "name", NULL, getGroupMethod,
     NAME_manual, "(Possible inherited) group-name"),
#ifndef O_RUNTIME
  GM(NAME_manId, 0, "name", NULL, getManIdMethod,
     NAME_manual, "Card Id for method"),
  GM(NAME_manIndicator, 0, "name", NULL, getManIndicatorMethod,
     NAME_manual, "Manual type indicator (`M')"),
  GM(NAME_manSummary, 0, "string", NULL, getManSummaryMethod,
     NAME_manual, "New string with documentation summary"),
#endif /*O_RUNTIME*/
  GM(NAME_argumentCount, 0, "int", NULL, getArgumentCountMethod,
     NAME_meta, "Minimum number of arguments required"),
  GM(NAME_argumentType, 1, "type", "int", getArgumentTypeMethod,
     NAME_meta, "Get type for nth-1 argument"),
  GM(NAME_inheritedFrom, 0, "method", NULL, getInheritedFromMethod,
     NAME_meta, "Method I'm a refinement of"),
  GM(NAME_instantiateTemplate, 0, "method", NULL, getInstantiateTemplateMethod,
     NAME_template, "Instantiate a method for use_class_template/1"),
  GM(NAME_printName, 0, "name", NULL, getPrintNameMethod,
     NAME_textual, "Class <->Selector")
};

/* Resources */

#define rc_method NULL
/*
static resourcedecl rc_method[] =
{ 
};
*/

/* Class Declaration */

static Name method_termnames[] = { NAME_name, NAME_types, NAME_message, NAME_summary, NAME_source };

ClassDecl(method_decls,
          var_method, send_method, get_method, rc_method,
          5, method_termnames,
          "$Rev$");


status
makeClassMethod(Class class)
{ declareClass(class, &method_decls);

  setLoadStoreFunctionClass(class, loadMethod, storeFdMethod);
  setTraceFunctionClass(class, traceMethod);
					/* for sharing of templates */
  cloneStyleVariableClass(class, NAME_types, NAME_reference);
  cloneStyleVariableClass(class, NAME_summary, NAME_reference);
  cloneStyleVariableClass(class, NAME_source, NAME_reference);
  cloneStyleVariableClass(class, NAME_message, NAME_reference);

  succeed;
}

