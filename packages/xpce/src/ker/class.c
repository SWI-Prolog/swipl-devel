/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <itf/c.h>
#include <h/graphics.h>			/* resource access functions */

static status	recordInstancesClass(Class class, Bool keep, Bool recursive);
static status	fill_slots_class(Class class, Class super);
static Variable	getLocaliseInstanceVariableClass(Class class, Name name);

#ifndef O_RUNTIME
#define CLASS_PCE_SLOTS 43
#else
#define CLASS_PCE_SLOTS 42		/* no source slot! */
#endif

#define InstanceSize(c)	((int) &((Instance) NULL)->slots[valInt((c)->slots)])
#define SlotsClass(c) \
      ((sizeof(struct c) - ((int) &((Instance) NULL)->slots[0])) / sizeof(Any))

static void
resetSlotsClass(Class class, Name name)
{ int i;
  int slots = SlotsClass(class);

  setProtectedObj(class);

  for(i=0; i<CLASS_PCE_SLOTS; i++)
    ((Instance)class)->slots[i] = DEFAULT;
  for( ; i < slots; i++ )
    ((Instance)class)->slots[i] = NULL;

  class->created_messages       = NIL;
  class->freed_messages         = NIL;
  class->make_class_message     = NIL;
  class->instances              = NIL;
  class->super_class            = NIL;
  class->sub_classes            = NIL;

  assign(class, name,	    name);
  assign(class, no_created, ZERO);
  assign(class, no_freed,   ZERO);
}


Class
typeClass(Name name)
{ Class class = alloc(sizeof(struct class));

  initHeaderObj(class, ClassClass);
  resetSlotsClass(class, name);

  return class;
}


Class
nameToTypeClass(Name name)
{ Type type;

  if ( (type = nameToType(name)) )
  { if ( !inBoot &&
	 ( !isClassType(type) ||
	   type->vector != OFF ||
	   notNil(type->supers)
	 ) )
    { errorPce(type, NAME_notClassType);
      fail;
    }

    if ( !instanceOfObject(type->context, ClassClass) )
    { if ( type->context == name )
      { assign(type, context, typeClass(name));
      } else
      { errorPce(type, NAME_notClassType);
	fail;
      }
    }
    
    return type->context;
  }

  fail;
}


static void
linkSubClass(Class super, Class sub)
{ if ( isNil(super->sub_classes) )
  { assign(super, sub_classes, newObject(ClassChain, sub, 0));
  } else
  { Cell cell;
    int done = FALSE;

    for_cell(cell, super->sub_classes)
    { Class class = cell->value;

      if ( class->name == sub->name )
      { if ( class != sub )
	  deleteChain(super->sub_classes, class);
        else
	  done = TRUE;
      }
    }

    if ( !done )
      appendChain(super->sub_classes, sub);
  }

  assign(sub, super_class, super);
}


Class
defineClass(Name name, Name super, StringObj summary, SendFunc makefunction)
{ Class class, superclass;

  TRY(class = nameToTypeClass(name));
  class->make_class_function = makefunction;
  if ( notNil(super) )
  { TRY(superclass = nameToTypeClass(super));
    linkSubClass(superclass, class);
  }
  if ( isDefault(class->creator) )
    assign(class, creator, inBoot ? NAME_builtIn : NAME_host);
  if ( notDefault(summary) )
    assign(class, summary, summary);

  if ( notDefault(class->realised) )
    return class;			/* existing (boot) class */

  if ( isDefault(class->sub_classes) )
    assign(class, sub_classes, NIL);

  assign(class, realised, OFF);
  { char tmp[LINESIZE];
    char *s, *d = tmp;

    appendHashTable(classTable, class->name, class);
    for(s = strName(class->name); (*d++ = *s++); );
    d--;
    for(s = "_class"; (*d++ = *s++); );
    newAssoc(CtoKeyword(tmp), class);
  }
  appendHashTable(classTable, name, class);
  protectObject(class);
  createdObject(class, NAME_new);

  return class;
}


status
defineClasses(struct class_definition *classes)
{ for(; classes->name; classes++)
  { Class class = defineClass(classes->name, classes->super,
			      CtoString(classes->summary),
			      classes->makefunction);

    if ( classes->global )
      *classes->global = class;
  }

  numberTreeClass(ClassObject, 0);

  succeed;
}


static inline status
call_make_function(SendFunc f, Class class)
{ status rval;

#if O_CPLUSPLUS
  if ( isCppFunctionPointer(f) )
  { void *fcpp = valCppFunctionPointer(f);
    
    rval = callCPlusPlusProc(fcpp, 1, (Any *)&class);
  } else 
#endif
    rval = (*f)(class);

  return rval;
}


status
realiseClass(Class class)
{ if ( class->realised != ON )
  { status rval;

    if ( notNil(class->super_class) )
      TRY(realiseClass(class->super_class));

    Mode(MODE_SYSTEM,
	 if ( class->make_class_function )
	 { assign(class, realised, ON);
	   rval = (fill_slots_class(class, class->super_class) &&
		   call_make_function(class->make_class_function, class) &&
		   initClass(class));
	 } else
	   rval = FAIL;);

    return rval;
  }

  succeed;
}


status
realiseBootClass(Class class)
{ assign(class, realised, OFF);

  DEBUG_BOOT(Cprintf("Realising boot class %s ...", strName(class->name)));
  realiseClass(class);
  deleteHashTable(class->send_table, NAME_initialise);
  deleteHashTable(class->get_table, NAME_lookup);
  assign(class, initialise_method, DEFAULT); /* rebind cache */
  assign(class, lookup_method,     DEFAULT);
  DEBUG_BOOT(Cprintf("ok.\n"));
  succeed;
}


static status
fill_slots_class(Class class, Class super)
{ if ( notNil(super) )
    linkSubClass(super, class);

  initialiseProgramObject(class);
  assign(class, realised,        ON);
  assign(class, send_methods,    newObject(ClassChain, 0));
  assign(class, get_methods,     newObject(ClassChain, 0));
  assign(class, resources,       newObject(ClassChain, 0));
  assign(class, term_functor,    class->name);
  assign(class, send_table,      newObject(ClassHashTable, 0));
  assign(class, get_table,       newObject(ClassHashTable, 0));
  assign(class, local_table,     newObject(ClassHashTable, 0));
  assign(class, resource_table,  NIL);
  assign(class, selection_style, NIL);
  assign(class, rcs_revision,	 NIL);
#ifndef O_RUNTIME
  assign(class, source,		 NIL);
#endif
  if ( isDefault(class->summary) )
    assign(class, summary,	 NIL);

					/* special method cache */
  assign(class, send_catch_all,	   DEFAULT);
  assign(class, get_catch_all,	   DEFAULT);
  assign(class, convert_method,	   DEFAULT);
  assign(class, lookup_method,	   DEFAULT);
  if ( !class->boot )
    assign(class, initialise_method, DEFAULT);
  class->send_function = NULL;
  class->get_function = NULL;

  if ( notNil(super) )
  { assign(class, term_names,	        super->term_names);
    assign(class, delegate,	        getCopyChain(super->delegate));
    assign(class, instance_variables,   getCopyVector(super->instance_variables));
    assign(class, cloneStyle,	        super->cloneStyle);
    assign(class, saveStyle,	        super->saveStyle);
    assign(class, features,		getCopySheet(super->features));
    assign(class, solid,	        super->solid);
    assign(class, handles,	        getCopyChain(super->handles));
    assign(class, un_answer,	        super->un_answer);
    assign(class, slots,	        super->slots);

    if ( !class->boot )
    { assign(class, instance_size,	super->instance_size);
      assign(class, has_init_functions, super->has_init_functions);
    }
    assign(class, changed_messages,     getCopyChain(super->changed_messages));
    assign(class, created_messages,     getCopyChain(super->created_messages));
    assign(class, freed_messages,       getCopyChain(super->freed_messages));
    if ( isDefault(class->resolve_method_message) )
      assign(class, resolve_method_message, super->resolve_method_message);

    if ( notNil(super->instances) )
      recordInstancesClass(class, ON, OFF);

    class->saveFunction			= super->saveFunction;
    class->loadFunction			= super->loadFunction;
    class->cloneFunction		= super->cloneFunction;
    class->redrawFunction		= super->redrawFunction;
    class->changedFunction		= super->changedFunction;
    class->in_event_area_function	= super->in_event_area_function;
    class->trace_function		= super->trace_function;
  } else
  { assign(class, term_names,	        NIL);
    assign(class, delegate,	        newObject(ClassChain, 0));
    assign(class, instance_variables,	newObject(ClassVector, 0));
    assign(class, cloneStyle,	        NAME_recursive);
    assign(class, saveStyle,	        NAME_normal);
    assign(class, features,		NIL);
    assign(class, solid,	        OFF);
    assign(class, instance_size,        toInt(sizeof(struct object)));
    assign(class, slots,	        ZERO);
    assign(class, un_answer,	        ON);
    assign(class, handles,		NIL);
    assign(class, changed_messages,	NIL);
    assign(class, resolve_method_message, NIL);

    assign(class, has_init_functions,	OFF);
    assign(class, changed_messages,     NIL);
    assign(class, created_messages,     NIL);
    assign(class, freed_messages,       NIL);
  }

  { char tmp[LINESIZE];

    appendHashTable(classTable, class->name, class);
    sprintf(tmp, "%s_class", strName(class->name));
    newAssoc(CtoKeyword(tmp), class);
  }

  protectObject(class);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bootClass() should be called to  initialise the most  vital classes of
the system.

Note that the initialise_method  is locked to  prevent drop-out during
the real class definition: freeObject doesn't yet work properly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Class
_bootClass(Name name, Name super_name, int size, int slots, SendFunc initF, int argc, va_list args)
{ Type type = nameToType(name);
  Class cl = type->context;
  Class super;

  if ( notNil(super_name) )
  { Type super_type = nameToType(super_name);
    super = super_type->context;
    assert(notNil(super->initialise_method)); /* No super-class */
  } else
    super = NIL;

  DEBUG_BOOT(Cprintf("Boot Class %s ... ", pp(name)));

  cl->boot = slots;
  if ( notNil(super) )
    cl->boot += super->boot;

  assign(cl, realised, ON);
  assign(cl, super_class, super);
  assign(cl, instance_size, toInt(size));
  assign(cl, slots, toInt((size - ((int) &((Instance) NULL)->slots[0]))
			   / sizeof(Any)));

  { int i;
    Type types[VA_PCE_MAX_ARGS];
    Vector tv;

    for(i=0; i<argc; i++)
    { char *type = va_arg(args, char *);

      if ( !(types[i] = CtoType(type)) )
	sysPce("Bad type in bootClass(): %s: %s\n", pp(name), type);
    }

    tv = createVectorv(argc, (Any *)types);

    assign(cl, initialise_method,
	   createSendMethod(NAME_initialise, tv, NIL, initF));
    lockObj(cl->initialise_method);	/* avoid reclaim on sdcClass */
    assign(cl, lookup_method, NIL);
    assign(cl, has_init_functions, OFF); /* not support for boot stuff */
    assign(cl, resolve_method_message, NIL);
  }

  DEBUG_BOOT(Cprintf("ok\n"));

  return cl;
}


Class
bootClass(Name name, Name super_name, int size, int slots,
	  SendFunc newF, int argc, ...)
{ va_list args;
  Class class;

  va_start(args, argc);
  class = _bootClass(name, super_name, size, slots, newF, argc, args);
  va_end(args);
  
  return class;
}


void
lookupBootClass(Class class, Func f, int argc, ...)
{ int i;
  Type types[VA_PCE_MAX_ARGS];
  Vector tv;
  va_list args;
  GetMethod m;

  va_start(args, argc);
  for(i=0; i<argc; i++)
  { char *type = va_arg(args, char *);

    if ( !(types[i] = CtoType(type)) )
      sysPce("Bad type in lookupBootClass(): %s: %s",
	     pp(class->name), type);
  }
  va_end(args);

  tv = createVectorv(argc, (Any *)types);
  m = createGetMethod(NAME_lookup, TypeAny, tv, NIL, f);
  lockObj(m);				/* avoid reclaim on sdcClass */
  setDFlag(m, D_TYPENOWARN);

  assign(class, lookup_method, m);
}


Class
getConvertClass(Class class_class, Any obj)
{ Class class;
  Name name;

  if ( instanceOfObject(obj, ClassClass) )
    return obj;

  if ( instanceOfObject(obj, ClassType) )
  { Type t = obj;
    
    if ( isClassType(t) )
      return t->context;
  }

  if ( instanceOfObject(obj, ClassClassStub) )
    return get(obj, NAME_realise, 0);
      
  if ( (name = toName(obj)) )
  { if ( !(class = getMemberHashTable(classTable, name)) )
    { exceptionPce(PCE, NAME_undefinedClass, name, 0);
      if ( !(class = getMemberHashTable(classTable, name)) )
	fail;
    }

    if ( !instanceOfObject(class, ClassClass) )
      return get(class, NAME_realise, 0);

    return class;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called from clearCacheClass().  Change this if this class is to do anything
else ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
installClass(Class class)
{ if ( ClassFunction && isAClass(class, ClassFunction) )
  { Cell cell;
    Class cl;

    for(cl = class; ; cl = cl->super_class)
    { for_cell(cell, cl->send_methods)
      { SendMethod m = cell->value;

      	if ( !getMemberHashTable(class->send_table, m->name) )
	  getResolveSendMethodClass(class, m->name);
      }

      for_cell(cell, cl->get_methods)
      { GetMethod m = cell->value;

	if ( !getMemberHashTable(class->get_table, m->name) )
	  getResolveGetMethodClass(class, m->name);
      }

      for_vector(cl->instance_variables, Variable v,
		 { if ( sendAccessVariable(v) )
		     getResolveSendMethodClass(class, v->name);
		   if ( getAccessVariable(v) )
		     getResolveGetMethodClass(class, v->name);
		 });

      if ( cl == ClassFunction )
	break;
    }
  }

  succeed;
}


status
initClass(Class class)
{ class->boot = 0;

#if 0
  if ( InstanceSize(class) != valInt(class->instance_size) ) /* TBD */
    Cprintf("Class %s has %d alien slots\n",
	    pp(class->name),
	    (valInt(class->instance_size) - InstanceSize(class)) /
	    sizeof(Any));
#endif

  installClass(class);

  DEBUG_BOOT(Cprintf("ok\n"));

  succeed;
}


status
prepareClass(Class class)		/* prepare for making instances */
{ Bool has_init_functions = OFF;

  for_vector(class->instance_variables, Variable v,
	     { if ( isFunction(v->init_function) )
	       { has_init_functions = ON;
		 break;
	       }
	     });

  assign(class, has_init_functions, has_init_functions);

  succeed;
}


void
makeBuiltInClasses(VoidFunc *f)
{ for(; *f; f++)
  { AnswerMark mark;

    markAnswerStack(mark);
    (*(*f))();
    rewindAnswerStack(mark, NIL);
  }
}

		/********************************
		*     USER-DEFINED CLASSES	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Creating classes.  Hairy.  Actually it is getLookupClass() that takes
care of ceating new classes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseClass(Class class, Name name, Class super)
{ Type type;
  Class cl;

  if ( (cl = getMemberHashTable(classTable, name)) &&
       instanceOfObject(cl, ClassClass) )
    fail;				/* failure from getLookupClass() */

  resetSlotsClass(class, name);
  appendHashTable(classTable, name, class);

  type = nameToType(name);
  if ( !isClassType(type) ||
       type->vector != OFF ||
       notNil(type->supers) )
  { errorPce(type, NAME_notClassType);
    fail;
  }
  assign(type, context, class);

  if ( isDefault(super) )
    super = ClassObject;

  realiseClass(super);
  fill_slots_class(class, super);
  assign(class, creator, inBoot ? NAME_builtIn : NAME_host);
  assign(class, no_created, ZERO);
  assign(class, no_freed,   ZERO);
  numberTreeClass(ClassObject, 0);
  createdObject(class, NAME_new);

  succeed;
}


static Class
getLookupClass(Class class, Name name, Class super)
{ Class cl;

  if ( (cl = getMemberHashTable(classTable, name)) &&
       instanceOfObject(cl, ClassClass) ) /* fail on class_stub objects */
  { if ( notNil(cl->super_class) )	/* no longer a typeClass() */
    { if ( isDefault(super) || cl->super_class == super )
	answer(cl);

      errorPce(cl, NAME_cannotChangeSuperClass);
      fail;
    }
    if ( name == NAME_object )		/* class(object) has no super! */
      answer(cl);
  } else if ( isDefault(super) )	/* lookup: class(box) or so */
  { exceptionPce(PCE, NAME_undefinedClass, name, 0);
    if ( (cl = getMemberHashTable(classTable, name)) )
      answer(cl);
  }

  fail;
}


static status
unlinkClass(Class cl)
{ assert(0);				/* classes cannot be unlinked */
  fail;
}


static Class
getSubClassClass(Class super, Name name)
{ realiseClass(super);

  if ( notNil(super->sub_classes) )
  { Cell cell;

    for_cell(cell, super->sub_classes)
    { Class sub = cell->value;

      if ( sub->name == name )
	answer(sub);
    }
  }
  
  answer(newObject(super->class, name, super, 0));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Adding local variables to classes
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
fixSubClassVariableClass(Class class, Variable old, Variable new)
{ if ( class->realised == ON )
  { Cell cell;
    Int offset = new->offset;

    if ( (getElementVector(class->instance_variables, offset) == old) || !old )
    { deleteHashTable(class->get_table,   new->name);
      deleteHashTable(class->send_table,  new->name);
      deleteHashTable(class->local_table, new->name);

      elementVector(class->instance_variables, offset, new);

					/* TBD: function subclass? */
      if ( old && notNil(class->sub_classes) )
      { for_cell(cell, class->sub_classes)
	  fixSubClassVariableClass(cell->value, old, new);
      } 
    }
  }
}


static Variable
getLocaliseInstanceVariableClass(Class class, Name name)
{ Variable var;

  realiseClass(class);
  if ( (var = getInstanceVariableClass(class, name)) )
  { if ( var->context != class )
    { Variable var2 = getCloneObject(var);
      assign(var2, context, class);
      fixSubClassVariableClass(class, var, var2);

      if ( ClassDelegateVariable &&
	   instanceOfObject(var2, ClassDelegateVariable) )
	delegateClass(class, var2->name);
    
      answer(var2);
    }
  }

  answer(var);
}

status
instanceVariableClass(Class class, Variable var)
{ Variable old;
  Int offset;

  realiseClass(class);
  if ( class->proto )
    unallocInstanceProtoClass(class);
					/* redefinition of a variable */
  if ( (old = getInstanceVariableClass(class, var->name)) )
  { if ( old->context != class )
      errorPce(class, NAME_cannotRefineVariable, var->name);

    offset = old->offset;
  } else
  { if ( !inBoot )
    { if ( class->no_created != class->no_freed )
	return errorPce(class, NAME_hasInstances);
      if ( notNil(class->sub_classes) )
      { Cell cell;

	for_cell(cell, class->sub_classes) 
	{ Class sub = cell->value;
	  if ( sub->realised == ON )
	    return errorPce(class, NAME_hasSubClasses);
	}
      }
    }
    offset = class->slots;
    assign(class, slots, incrInt(class->slots));
    if ( InstanceSize(class) > valInt(class->instance_size) )
      assign(class, instance_size, toInt(InstanceSize(class)));
  }

  assign(var, offset, offset);
  assign(var, context, class);
  fixSubClassVariableClass(class, old, var);

  if ( ClassDelegateVariable && instanceOfObject(var, ClassDelegateVariable) )
    delegateClass(class, var->name);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Method manipulation
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
codeExecuteCode(Code c)
{ return vm_send(c, NAME_Execute, classOfObject(c), 0, NULL);
}


void
fixSendFunctionClass(Class class, Name selector)
{ SendMethod m = getSendMethodClass(class, selector);

  class->send_function = (m ? (SendFunc) m->function : (SendFunc) NULL);

  if ( !class->send_function )
    class->send_function = codeExecuteCode;
}


static Any
codeGetExecuteCode(Code c)
{ return vm_get(c, NAME_Execute, classOfObject(c), 0, NULL);
}


void
fixGetFunctionClass(Class class, Name selector)
{ GetMethod m = getGetMethodClass(class, selector);

  class->get_function = (m ? (GetFunc) m->function : (GetFunc) NULL);

  if ( !class->get_function )
    class->get_function = codeGetExecuteCode;
}


static void
fixSubClassSendMethodsClass(Class class, Method m)
{ if ( class->realised == ON )
  { Cell cell;

    deleteHashTable(class->send_table, m->name);
    if ( notNil(class->sub_classes) )
    { for_cell(cell, class->sub_classes)
	fixSubClassSendMethodsClass(cell->value, m);
    }
    if ( m->name == NAME_initialise )
      assign(class, initialise_method, DEFAULT);
    else if ( m->name == NAME_catchAll )
      assign(class, send_catch_all, DEFAULT);
  }
}


status
sendMethodClass(Class class, SendMethod m)
{ Cell cell;

  realiseClass(class);

  if ( notNil(m->context) )
    return errorPce(class, NAME_alreadyPartOf, m, m->context);

  fixSubClassSendMethodsClass(class, (Method) m);
  for_cell(cell, class->send_methods)
  { SendMethod old = cell->value;

    if ( old->name == m->name && old != m )
    {
#ifndef O_RUNTIME
      deleteChain(class->send_methods, old);
#else
      if ( onFlag(old, F_TEMPLATE_METHOD) )
	deleteChain(class->send_methods, old);
      else
	return errorPce(getMethodFromFunction(sendMethodClass),
			NAME_runtimeVersion);
#endif
      break;
    }
  }

  appendChain(class->send_methods, m);
  assign(m, context, class);

  succeed;
}


static void
fixSubClassGetMethodsClass(Class class, Method m)
{ if ( class->realised == ON )
  { Cell cell;

    deleteHashTable(class->get_table, m->name);
    if ( notNil(class->sub_classes) )
    { for_cell(cell, class->sub_classes)
	fixSubClassGetMethodsClass(cell->value, m);
    }
    if ( m->name == NAME_lookup )
      assign(class, lookup_method, DEFAULT);
    else if ( m->name == NAME_convert )
      assign(class, convert_method, DEFAULT);
  }
}

status
getMethodClass(Class class, GetMethod m)
{ Cell cell;

  realiseClass(class);

  if ( notNil(m->context) )
    return errorPce(class, NAME_alreadyPartOf, m, m->context);

					/* delete old definition */
  fixSubClassGetMethodsClass(class, (Method) m);

  for_cell(cell, class->get_methods)
  { GetMethod old = cell->value;

    if ( old->name == m->name && old != m )
    {
#ifndef O_RUNTIME
      deleteChain(class->get_methods, old);
#else
      if ( onFlag(old, F_TEMPLATE_METHOD) )
	deleteChain(class->get_methods, old);
      else
	return errorPce(getMethodFromFunction(sendMethodClass),
			NAME_runtimeVersion);
#endif
      break;
    }
  }
					/* Insert new one */
  appendChain(class->get_methods, m);
  assign(m, context, class);

  succeed;
}


status
setChangedFunctionClass(Class class, SendFunc func)
{ class->changedFunction = func;

  succeed;
}


#ifndef O_RUNTIME
status
setTraceFunctionClass(Class class, VoidFunc func)
{ class->trace_function = func;

  succeed;
}
#endif

status
setInEventAreaFunctionClass(Class class, SendFunc func)
{ class->in_event_area_function = func;

  succeed;
}


status
isPceSlot(Class class, int n)
{ Variable var = class->instance_variables->elements[n];

  return var->type->kind == NAME_alien ? FAIL : SUCCEED;
}


status
allPceSlotsClass(Class class)
{ for_vector(class->instance_variables, Variable var,
	     if ( var->type->kind == NAME_alien )
	       fail;);

  succeed;
}

		/********************************
		*        OBJECT -> TERM		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
termClass(Class, FunctorName, Arity, Selector1 ...)
    Define the term representation of Class to be

	FunctorName(obj?Selector1, ...)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
_termClass(Class class, char *name, int argc, va_list args)
{ realiseClass(class);

  assign(class, term_functor, CtoName(name));
  if ( argc == ARGC_UNKNOWN )
  { assign(class, term_names, NIL);
  } else
  { ArgVector(names, argc);
    int n;

    for(n=0; n<argc; n++)
    { names[n] = va_arg(args, Any);

      if ( !isProperObject(names[n]) || notName(names[n]) )
      { sysPce("Illegal selector (arg %d) to termClass of class %s",
	       n+1, pp(class->name));
	return;
      }
    }
    
    assign(class, term_names, newObjectv(ClassVector, argc, names));
  }
}


void
termClass(Class class, char *name, int argc, ...)
{ va_list args;

  va_start(args, argc);
  _termClass(class, name, argc, args);
  va_end(args);
}


static inline status
_sendMethod(Class class, Name name, Name group, int argc, va_list args)
{ SendMethod m;
  Type types[METHOD_MAX_ARGS];
  int i;
  SendFunc f;
  Vector tv;
  char *rawdoc;
  StringObj doc;

  for(i=0; i<argc; i++)
  { char *type = va_arg(args, char *);

    assert(i < METHOD_MAX_ARGS);
    if ( !(types[i] = CtoType(type)) )
      sysPce("Bad type in sendMethod(): %s->%s: %s",
	     pp(class->name), pp(name), type);
  }

  tv = inBoot ? createVectorv(argc, (Any *)types)
              : answerObjectv(ClassVector, argc, (Any *)types);

  if ( (rawdoc = va_arg(args, char *)) )
  { checkSummaryCharp(class->name, name, rawdoc);
    doc = rawdoc[0] == EOS ? (StringObj) NIL : CtoString(rawdoc);
  } else
    doc = NIL;

  f = va_arg(args, SendFunc);
  m = createSendMethod(name, tv, doc, f);
  if ( notDefault(group) )
    assign(m, group, group);
  
  assign(m, context, class);
  appendChain(class->send_methods, m);

  if ( isNil(m->summary) )
  { SendMethod super;

    if ( (super = (SendMethod) getInheritedFromMethod((Method)m)) )
      assign(m, summary, super->summary);
  }

  succeed;
}


status
sendMethod(Class class, Name name, Name group, int argc, ...)
{ va_list args;
  status rval;

  va_start(args, argc);
  rval = _sendMethod(class, name, group, argc, args);
  va_end(args);

  return rval;
}


status
storeMethod(Class class, Name name, SendFunc function)
{ Variable var = getInstanceVariableClass(class, (Any) name);
  Vector tv;
  SendMethod m;

  if ( !var )
    return sysPce("storeMethod(): no variable %s on class %s",
		  pp(name), pp(class->name));
  tv = inBoot ? createVectorv(1, (Any *)(&var->type))
              : answerObjectv(ClassVector, 1, (Any *)(&var->type));

  m = createSendMethod(name, tv, var->summary, function);
  assign(m, context, class);
  assign(m, group, var->group);
  appendChain(class->send_methods, m);

  succeed;
}


status
fetchMethod(Class class, Name name, void *function)
{ Variable var = getInstanceVariableClass(class, (Any) name);
  Vector tv;
  GetMethod m;

  if ( !var )
    return sysPce("fetchMethod(): no variable %s on class %s",
		  pp(name), pp(class->name));
  tv = inBoot ? createVectorv(0, NULL)
              : answerObjectv(ClassVector, 0, NULL);

  m = createGetMethod(name, var->type, tv, var->summary, function);
  assign(m, context, class);
  assign(m, group, var->group);
  appendChain(class->get_methods, m);

  succeed;
}


static inline status
_getMethod(Class class, Name name, Name group, char *rtype, int argc, va_list args)
{ GetMethod m;
  Type rt;
  Type types[METHOD_MAX_ARGS];
  int i;
  Func f;
  Vector tv;
  char *rawdoc;
  StringObj doc;

  if ( !(rt = CtoType(rtype)) )
    return sysPce("Bad return type in getMethod(): %s<-%s: %s",
		  pp(class->name), pp(name), rtype);
  for(i=0; i<argc; i++)
  { char *type = va_arg(args, char *);

    assert(i<METHOD_MAX_ARGS);
    if ( !(types[i] = CtoType(type)) )
      sysPce("Bad type in getMethod(): %s<-%s: %s",
	     pp(class->name), pp(name), type);
  }
  tv = inBoot ? createVectorv(argc, (Any *)types)
              : answerObjectv(ClassVector, argc, (Any *)types);

  if ( (rawdoc = va_arg(args, char *)) )
  { checkSummaryCharp(class->name, name, rawdoc);
    doc = rawdoc[0] == EOS ? (StringObj) NIL : CtoString(rawdoc);
  } else
    doc = NIL;

  f = va_arg(args, Func);
  m = createGetMethod(name, rt, tv, doc, f);
  if ( notDefault(group) )
    assign(m, group, group);
  
  assign(m, context, class);
  appendChain(class->get_methods, m);
  if ( isNil(m->summary) )
  { GetMethod super;

    if ( (super = (GetMethod) getInheritedFromMethod((Method)m)) )
      assign(m, summary, super->summary);
  }

  succeed;
}

status
getMethod(Class class, Name name, Name group, char *rtype, int argc, ...)
{ va_list args;
  status rval;

  va_start(args, argc);
  rval = _getMethod(class, name, group, rtype, argc, args);
  va_end(args);

  return rval;
}


		/********************************
		*       RESERVED FUNCTIONS	*
		********************************/

status
cloneStyleClass(Class class, Name style)
{ realiseClass(class);
  assign(class, cloneStyle, style);

  succeed;
}


status
cloneStyleVariableClass(Class class, Name slot, Name style)
{ Variable var;

  if ( (var = getLocaliseInstanceVariableClass(class, slot)) )
    return cloneStyleVariable(var, style);

  fail;
}


status
saveStyleVariableClass(Class class, Name slot, Name style)
{ Variable var;

  if ( (var = getLocaliseInstanceVariableClass(class, slot)) )
    return saveStyleVariable(var, style);

  fail;
}


status
saveStyleClass(Class class, Name style)
{ realiseClass(class);
  assign(class, saveStyle, style);

  succeed;
}


status
setCloneFunctionClass(Class class, SendFunc function)
{ class->cloneFunction = function;
  succeed;
}


status
setRedrawFunctionClass(Class class, SendFunc function)
{ class->redrawFunction = function;

  sendMethod(class, NAME_RedrawArea, NAME_repaint, 1, "area",
	     "Repaint the argument area",
	     function);
  succeed;
}


status
setLoadStoreFunctionClass(Class class, SendFunc load, SendFunc store)
{ class->loadFunction = load;
  class->saveFunction = store;
  succeed;
}


static status
handleClass(Class class, Handle handle)
{ realiseClass(class);
  if ( isNil(class->handles) )
    assign(class, handles, newObject(ClassChain, handle, 0));
  else
    appendChain(class->handles, handle);

  succeed;
}


status
solidClass(Class class, Bool val)
{ realiseClass(class);
  assign(class, solid, val);

  succeed;
}


status
sourceClass(Class class, SendFunc f, char *file, char *rcs)
{ static char rev[] = "$Revision: ";
  char *s, *q;
  char buf[100];
  int l;

  class->make_class_function = f;
#ifndef O_RUNTIME
  assign(class, source, newObject(ClassSourceLocation, CtoName(file), 0));
#endif
  
  for(s=rcs, q=rev; *q && *s == *q; s++, q++)
    ;
  strcpy(buf, s);
  l = strlen(buf);
  if ( l >= 2 && streq(&buf[l-2], " $") )
    buf[l-2] = EOS;
      
  assign(class, rcs_revision, CtoName(buf));

  succeed;
}


#ifdef O_RUNTIME
static status
rtSourceClass(Class class, SourceLocation src)
{ succeed;
}
#endif


void
localClass(Class class, Name name, Name group,
	   char *type, Name access, char *doc)
{ Variable v;
  Type t;

  if ( !(t = CtoType(type)) )
    sysPce("Bad type in localClass(): %s.%s: %s",
	   pp(class->name), pp(name), type);

  v = createVariable(name, t, access);

  checkSummaryCharp(class->name, name, doc);
  if ( strlen(doc) > 0 )
    assign(v, summary, CtoString(doc));
  if ( notDefault(group) )
    assign(v, group, group);

  instanceVariableClass(class, v);
}


void
superClass(Class class, Name name, Name group, char *type, Name access, Name wrapper, char *doc)
{ Variable v;
  Type t;
  StringObj summary;

  if ( !(t = CtoType(type)) )
    sysPce("Bad type in localClass(): %s.%s: %s",
	   pp(class->name), pp(name), type);

  checkSummaryCharp(class->name, name, doc);
  summary = (strlen(doc) > 0 ? CtoString(doc) : NIL);

  v = newObject(ClassDelegateVariable, name, t, access, wrapper,
		summary, group, 0);

  instanceVariableClass(class, v);
}


status
delegateClass(Class class, Name name)
{ Variable var = getInstanceVariableClass(class, name);

  if ( var )
  { deleteChain(class->delegate, var);
    appendChain(class->delegate, var);

    succeed;
  }

  return errorPce(class, NAME_noVariable, name);
}


status
prependDelegateClass(Class class, Name name)
{ Variable var = getInstanceVariableClass(class, name);

  if ( var )
  { deleteChain(class->delegate, var);
    prependChain(class->delegate, var);

    succeed;
  }

  return errorPce(class, NAME_noVariable, name);
}


static Any
getInstanceClassv(Class class, int argc, Any *argv)
{ answer(answerObjectv(class, argc, argv));
}


Variable
getInstanceVariableClass(Class class, Any which)
{ Variable var;

  realiseClass(class);

  if ( isInteger(which) )
    answer( getElementVector(class->instance_variables, (Int) which));

  if ( (var = getMemberHashTable(class->local_table, which)) != FAIL )
    answer(var);

  for_vector(class->instance_variables, var,
	     { if ( var->name == which )
	       { appendHashTable(class->local_table, which, var);
		 answer(var);
	       }
	     });

  fail;					/* no such variable */
}


static status
boundSendMethodClass(Class class, Name name)
{ if ( class->realised == ON )
  { Cell cell;

    for_cell(cell, class->send_methods)
    { SendMethod m = cell->value;
    
      if ( m->name == name )
	succeed;
    }
    for_vector(class->instance_variables, Variable var,
	       { if ( var->name == name &&
		      sendAccessVariable(var) &&
		      var->context == class )
		   succeed;
	       });
  }

  fail;
}


static status
boundGetMethodClass(Class class, Name name)
{ if ( class->realised == ON )
  { Cell cell;

    for_cell(cell, class->get_methods)
    { SendMethod m = cell->value;
    
      if ( m->name == name )
	succeed;
    }
    for_vector(class->instance_variables, Variable var,
	       { if ( var->name == name &&
		      getAccessVariable(var) &&
		      var->context == class )
		   succeed;
	       });
  }

  fail;
}


Any
getResolveSendMethodClass(Class class, Name name)
{ Cell cell;
  Class super;

  realiseClass(class);

  for(super = class; notNil(super); super = super->super_class)
  { Code c;
    Any sm;

    if ( (sm = getMemberHashTable(super->send_table, name)) )
    { if ( class != super )
	appendHashTable(class->send_table, name, sm);
      answer(sm);
    }

    if ( notNil((c=super->resolve_method_message)) && notDefault(c) )
      forwardReceiverCode(c,
			  super,
			  NAME_send,
			  super->name, 
			  name, 0);

    for_cell(cell, super->send_methods)
    { SendMethod m = cell->value;

      if ( m->name == name )
      { appendHashTable(class->send_table, name, m);
	answer(m);
      }
    }

    for_vector(super->instance_variables, Variable var,
	       { if ( var->name == name &&
		      sendAccessVariable(var) &&
		      var->context == super )
		 { appendHashTable(class->send_table, name, var);
		   answer(var);
		 }
	       });
  }

  appendHashTable(class->send_table, name, NIL);
  fail;
}


Any
getResolveGetMethodClass(Class class, Name name)
{ Cell cell;
  Class super;

  realiseClass(class);

  for(super = class; notNil(super); super = super->super_class)
  { Code c;
    Any gm;

    if ( (gm = getMemberHashTable(super->get_table, name)) )
    { if ( class != super )
	appendHashTable(class->get_table, name, gm);
      answer(gm);
    }

    if ( notNil(c=super->resolve_method_message) && notDefault(c) )
      forwardReceiverCode(c,
			  super,
			  NAME_get,
			  super->name, 
			  name, 0);

    for_cell(cell, super->get_methods)
    { GetMethod m = cell->value;

      if ( m->name == name )
      { appendHashTable(class->get_table, name, m);
	answer(m);
      }
    }

    for_vector(super->instance_variables, Variable var,
	       { if ( var->name == name &&
		      getAccessVariable(var) &&
		      var->context == super )
		 { appendHashTable(class->get_table, name, var);
		   answer(var);
		 }
	       });
  }

  appendHashTable(class->get_table, name, NIL);
  fail;
}


static status
clearCacheClass(Class class)
{ if ( class->realised == ON )
  { clearHashTable(class->send_table);
    clearHashTable(class->get_table);

    assign(class, initialise_method, DEFAULT);
    assign(class, lookup_method,     DEFAULT);

    installClass(class);		/* Enter function special methods */
  }
    
  succeed;
}


Int
getNoCreatedClass(Class class)
{ Cell cell;
  Int rval = class->no_created;

  if ( notNil(class->sub_classes) )
    for_cell(cell, class->sub_classes)
      rval = add(rval, getNoCreatedClass(cell->value));

  answer(rval);
}


Int
getNoFreedClass(Class class)
{ Cell cell;
  Int rval = class->no_freed;

  if ( notNil(class->sub_classes) )
    for_cell(cell, class->sub_classes)
      rval = add(rval, getNoFreedClass(cell->value));

  answer(rval);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  KEEP TRACK OF INSTANCES
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
createdClass(Class class, Any instance, Name how)
{ incrInt(class->no_created);
  clearCreatingObj(instance);

  if ( notNil(class->created_messages) )
  { Cell cell;

    addCodeReference(instance);		/* avoid drop-out */
    for_cell(cell, class->created_messages)
      forwardCode(cell->value, class->name, instance, how, 0);
    delCodeReference(instance);
  } 
     
  if ( notNil(class->instances) )
    appendHashTable(class->instances, instance, ON);

  succeed;
}


status
freedClass(Class class, Any instance)
{ clearFlag(instance, F_INSPECT);

  incrInt(class->no_freed);
  if ( notNil(class->freed_messages) )
  { Cell cell;

    addCodeReference(instance);		/* avoid looping */
    for_cell(cell, class->freed_messages)
      forwardCode(cell->value, class->name, instance, 0);
    if ( !isFreedObj(instance) )
      delCodeReference(instance);
  } 

  if ( notNil(class->instances) )
    deleteHashTable(class->instances, instance);

  succeed;
}


static status
recordInstancesClass(Class class, Bool keep, Bool recursive)
{ realiseClass(class);

  if ( keep != OFF && isNil(class->instances) )
    assign(class, instances, createHashTable(toInt(16), OFF));
  else if ( keep == OFF && notNil(class->instances) )
    assign(class, instances, NIL);

  if ( recursive != OFF && notNil(class->sub_classes) )
  { Cell cell;

    for_cell(cell, class->sub_classes)
      recordInstancesClass(cell->value, keep, recursive);
  }

  succeed;
}


static status
changedMessageClass(Class class, Code msg)
{ realiseClass(class);

  if ( isNil(class->changed_messages) )
  { assign(class, changed_messages, newObject(ClassChain, msg, 0));
    succeed;
  }
  
  return addChain(class->changed_messages, msg);
}


static status
createdMessageClass(Class class, Code msg)
{ realiseClass(class);

  if ( isNil(class->created_messages) )
  { assign(class, created_messages, newObject(ClassChain, msg, 0));
    succeed;
  }
  
  return addChain(class->created_messages, msg);
}


static status
freedMessageClass(Class class, Code msg)
{ realiseClass(class);

  if ( isNil(class->freed_messages) )
  { assign(class, freed_messages, newObject(ClassChain, msg, 0));
    succeed;
  }
  
  return addChain(class->freed_messages, msg);
}


static Name
getSuperClassNameClass(Class cl)
{ if ( notNil(cl->super_class) )
    answer(cl->super_class->name);

  answer((Name) NIL);
}

#ifndef O_RUNTIME

		/********************************
		*        MANUAL SUPPORT		*
		********************************/

static StringObj
getManHeaderClass(Class cl)
{ char buf[LINESIZE];
  Vector nms;

  realiseClass(cl);
  nms = cl->term_names;

  strcpy(buf, strName(cl->name));
  strcat(buf, "(");

  if ( isNil(nms) )
    strcat(buf, "...object...");
  else
  { int i;
    
    for(i=1; i<=valInt(nms->size); i++)
    { if ( i != 1 )
    	strcat(buf, ", ");
      strcat(buf, strName(getElementVector(nms, toInt(i))));
    }
  }
  strcat(buf, ")");

  answer(CtoString(buf));
}


static StringObj
getManSummaryClass(Class cl)
{ char buf[LINESIZE];
  Vector nms;

  realiseClass(cl);
  nms = cl->term_names;	

  buf[0] = EOS;
  strcat(buf, "C\t");

  strcat(buf, strName(cl->name));
  strcat(buf, "(");

  if ( isNil(nms) )
    strcat(buf, "...object...");
  else
  { int i;
    
    for(i=1; i<=valInt(nms->size); i++)
    { if ( i != 1 )
    	strcat(buf, ", ");
      strcat(buf, strName(getElementVector(nms, toInt(i))));
    }
  }
  strcat(buf, ")");

  if ( notNil(cl->summary) )
  { strcat(buf, "\t");
    strcat(buf, strName(cl->summary));
  }
  if ( send(cl, NAME_manDocumented, 0) != FAIL )
    strcat(buf, " (+)");

  answer(CtoString(buf));
}
#endif /*O_RUNTIME*/


status
isAClass(Class class, Class super)
{ return class->tree_index >= super->tree_index &&
         class->tree_index <  super->neighbour_index;
}


int
numberTreeClass(Class class, int n)
{ Cell cell;

  DEBUG(NAME_class, Cprintf("numberTreeClass(%s, %d)\n", pp(class->name), n));
  class->tree_index = n++;
  if ( notNil(class->sub_classes) )
  { for_cell(cell, class->sub_classes	)
    { if ( instanceOfObject(cell->value, ClassClass) ) /* stubs ... */
	n = numberTreeClass(cell->value, n);
    }
  }
  class->neighbour_index = n;

  return n;
}


		 /*******************************
		 *	     FEATURE ITF	*
		 *******************************/

status
featureClass(Class class, Name name, Any value)
{ realiseClass(class);

  if ( isDefault(value) )
    value = ON;

  if ( isNil(class->features) )
    assign(class, features, newObject(ClassSheet, 0));

  return valueSheet(class->features, name, value);
}


static status
hasFeatureClass(Class class, Name name, Any value)
{ realiseClass(class);

  if ( notNil(class->features) )
  { Any fval;
    if ( (fval = getValueSheet(class->features, name)) &&
	 (isDefault(value) || value == fval) )
      succeed;
  }

  fail;
}


static Any
getFeatureClass(Class class, Name name)
{ realiseClass(class);

  if ( notNil(class->features) )
    return getValueSheet(class->features, name);

  fail;
}



Chain
getSendMethodsClass(Class class)
{ realiseClass(class);

  if ( notNil(class->resolve_method_message) )
    forwardReceiverCode(class->resolve_method_message,
			class,
			NAME_send,
			class->name, 
			DEFAULT, 0);

  answer(class->send_methods);
}


static Chain
getGetMethodsClass(Class class)
{ realiseClass(class);

  if ( notNil(class->resolve_method_message) )
    forwardReceiverCode(class->resolve_method_message,
			class,
			NAME_get,
			class->name, 
			DEFAULT, 0);

  answer(class->get_methods);
}



status
makeClassClass(Class class)
{ sourceClass(class, makeClassClass, __FILE__, "$Revision$");

  localClass(class, NAME_instanceVariables, NAME_behaviour, "vector", NAME_get,
	     "Vector object holding all instance variables");
  localClass(class, NAME_sendMethods, NAME_behaviour, "chain", NAME_none,
	     "Send methods not inherited");
  localClass(class, NAME_getMethods, NAME_behaviour, "chain", NAME_none,
	     "Get methods not inherited");
  localClass(class, NAME_termFunctor, NAME_term, "name", NAME_both,
	     "Functor used for term description");
  localClass(class, NAME_termNames, NAME_term, "vector*", NAME_both,
	     "Selectors to obtain term arguments");
  localClass(class, NAME_delegate, NAME_behaviour, "chain", NAME_get,
	     "Instance variables for delegation");
  localClass(class, NAME_resources, NAME_resource, "chain", NAME_get,
	     "User setable resources (via Xdefaults)");
  localClass(class, NAME_cloneStyle, NAME_copy,
	     "{recursive,none,relation}", NAME_both,
	     "How to clone instances");
  localClass(class, NAME_saveStyle, NAME_file,
	     "{normal,external,nil}",NAME_both,
	     "How to save instances to file");
  localClass(class, NAME_features, NAME_version, "sheet*", NAME_none,
	     "Defined features on this class");
  localClass(class, NAME_noCreated, NAME_statistics, "int", NAME_get,
	     "Number of instances created");
  localClass(class, NAME_noFreed, NAME_statistics, "int", NAME_get,
	     "Number of instances freed");
  localClass(class, NAME_solid, NAME_repaint, "bool", NAME_none,
	     "Graphicals: image affects ALL pixels");
  localClass(class, NAME_selectionStyle, NAME_selection,
	     "{none,invert,corner_handles,side_handles,corner_and_side_handles,line_handles,path_handles}*",
	     NAME_both,
	     "Graphicals: visual feedback of selected");
  localClass(class, NAME_handles, NAME_relation, "chain*", NAME_both,
	     "Graphicals: connection points for links");
  localClass(class, NAME_instanceSize, NAME_oms, "int", NAME_get,
	     "Size of an instance in bytes");
  localClass(class, NAME_slots, NAME_oms, "int", NAME_get,
	     "Total number of instance variables");
#ifndef O_RUNTIME
  localClass(class, NAME_source, NAME_manual, "source_location*", NAME_both,
	     "Location in the sources");
#endif /*O_RUNTIME*/
  localClass(class, NAME_rcsRevision, NAME_version, "name*", NAME_get,
	     "RCS revision of sourcefile");
  localClass(class, NAME_changedMessages, NAME_change, "chain*", NAME_both,
	     "Report (forward) changes to instances");
  localClass(class, NAME_createdMessages, NAME_change, "chain*", NAME_both,
	     "Report (forward) created instances");
  localClass(class, NAME_freedMessages, NAME_change, "chain*", NAME_both,
	     "Report (forward) freed instances");
  localClass(class, NAME_unAnswer, NAME_oms, "bool", NAME_both,
	     "Incremental garbage collection hint");

  localClass(class, NAME_makeClassMethod, NAME_realise, "code*", NAME_get,
	     "Code object to ->realise the class");
	     
  localClass(class, NAME_initialiseMethod, NAME_cache, "[send_method]",
	     NAME_none,
	     "Used to initialise a new instance");
  localClass(class, NAME_sendCatchAll, NAME_cache, "[send_method]*", NAME_none,
	     "Handle not-yet-handled send messages");
  localClass(class, NAME_getCatchAll, NAME_cache, "[get_method]*", NAME_none,
	     "Handle not-yet-handled get messages");
  localClass(class, NAME_convertMethod, NAME_cache, "[get_method]*", NAME_none,
	     "Type conversion");
  localClass(class, NAME_lookupMethod, NAME_cache, "[get_method]*", NAME_none,
	     "Type conversion");

  localClass(class, NAME_resolveMethodMessage, NAME_cache, "[code]*",NAME_both,
	     "Hook for lazy attachment of methods");

  localClass(class, NAME_sendTable, NAME_cache, "hash_table", NAME_get,
	     "Hash table for all send methods");
  localClass(class, NAME_getTable, NAME_cache, "hash_table", NAME_get,
	     "Hash table for all get methods");
  localClass(class, NAME_localTable, NAME_cache, "hash_table", NAME_get,
	     "Hash table for all instance variables");
  localClass(class, NAME_resourceTable, NAME_cache, "hash_table*", NAME_get,
	     "Hash table for all resources");

  localClass(class, NAME_instances, NAME_debugging, "hash_table*", NAME_get,
	     "Hash table holding existing instances");
  localClass(class, NAME_realised, NAME_realise, "bool", NAME_get,
	     "@on if class is realised");
  localClass(class, NAME_hasInitFunctions, NAME_cache, "bool", NAME_get,
	     "@on if one or more variables use functions");

  localClass(class, NAME_proto, NAME_cache, "alien:InstanceProto", NAME_none,
	     "Prototype instance + info for fast creation");
  localClass(class, NAME_treeIndex, NAME_cache, "alien:int", NAME_none,
	     "Index in depth-first numbering of hierarchy");
  localClass(class, NAME_neighbourIndex, NAME_cache, "alien:int", NAME_none,
	     "Index of neighbour in hierarchy");
  localClass(class, NAME_getFunction, NAME_internal, "alien:GetFunc", NAME_none,
	     "Execute function-objects");
  localClass(class, NAME_sendFunction, NAME_internal,
	     "alien:SendFunc", NAME_none,
	     "Execute code-objects");
  localClass(class, NAME_saveFunction, NAME_internal,
	     "alien:SendFunc", NAME_none,
	     "C-function to save alien data");
  localClass(class, NAME_loadFunction, NAME_internal,
	     "alien:SendFunc", NAME_none,
	     "C-function to reload alien data");
  localClass(class, NAME_cloneFunction, NAME_internal,
	     "alien:SendFunc", NAME_none,
	     "C function to clone alien data");
  localClass(class, NAME_redrawFunction, NAME_internal,
	     "alien:SendFunc", NAME_none,
	     "C function to repaint graphicals");
  localClass(class, NAME_changedFunction, NAME_internal,
	     "alien:SendFunc", NAME_none,
	     "C function to trap slot changes");
  localClass(class, NAME_inEventArea, NAME_internal,
	     "alien:SendFunc", NAME_none,
	     "Graphicals: test if event in area");
  localClass(class, NAME_makeClassFunction, NAME_internal,
	     "alien:VoidFunc", NAME_none,
	     "C-function that created the class");
  localClass(class, NAME_traceFunction, NAME_internal,
	     "alien:VoidFunc", NAME_none,
	     "C-function to handle trace message");
  localClass(class, NAME_boot, NAME_internal,
	     "alien:int", NAME_none,
	     "#PCE slots when booting; 0 otherwise");

  termClass(class, "class", 2, NAME_name, NAME_superClassName);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  fetchMethod(class, NAME_sendMethods, getSendMethodsClass);
  fetchMethod(class, NAME_getMethods, getGetMethodsClass);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "name=name", "super=[class]*",
	     "Create from name and super class",
	     initialiseClass);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Remove from tables",
	     unlinkClass);
  sendMethod(class, NAME_changedMessage, NAME_change, 1, "code",
	     "Add message to trap changed slots",
	     changedMessageClass);
  sendMethod(class, NAME_createdMessage, NAME_change, 1, "code",
	     "Add message to trap created instances",
	     createdMessageClass);
  sendMethod(class, NAME_freedMessage, NAME_change, 1, "code",
	     "Add message to trap freed instances",
	     freedMessageClass);
  sendMethod(class, NAME_recordInstances, NAME_debugging, 2,
	     "record=[bool]", "recursive=[bool]",
	     "Maintain <-instances table",
	     recordInstancesClass);
  sendMethod(class, NAME_sendMethod, NAME_behaviour, 1, "send_method",
	     "Add/redefine send method",
	     sendMethodClass);
  sendMethod(class, NAME_getMethod, NAME_behaviour, 1, "get_method",
	     "Add/redefine get method",
	     getMethodClass);
  sendMethod(class, NAME_instanceVariable, NAME_behaviour, 1, "variable",
	     "Add/redefine instance variable",
	     instanceVariableClass);
  sendMethod(class, NAME_isA, NAME_type, 1, "class",
	     "Test if I'm a subclass of argument",
	     isAClass);
  sendMethod(class, NAME_handle, NAME_relation, 1, "handle",
	     "Add handle for graphical instances",
	     handleClass);
  sendMethod(class, NAME_cloneStyleVariable, NAME_copy, 2,
	     "variable=name|int",
	     "style={recursive,reference,value,alien,nil}",
	     "Set <->clone_style of named variable",
	     cloneStyleVariableClass);
  sendMethod(class, NAME_saveStyleVariable, NAME_file, 2,
	     "variable=name|int", "style={normal,nil}",
	     "Set the save style for named variable",
	     saveStyleVariableClass);
  sendMethod(class, NAME_delegate, NAME_behaviour, 1,
	     "variable=name|int",
	     "Add instance-variable for delegation",
	     delegateClass);
  sendMethod(class, NAME_prependDelegate, NAME_behaviour, 1,
	     "variable=name|int",
	     "Add instance-variable for delegation (as first)",
	     prependDelegateClass);
  sendMethod(class, NAME_realise, NAME_realise, 0,
	     "Declare methods/variables, etc.",
	     realiseClass);
  sendMethod(class, NAME_install, NAME_behaviour, 0,
	     "Prepare class for creating instances",
	     installClass);
  sendMethod(class, NAME_clearCache, NAME_cache, 0,
	     "Clear method resolution cache",
	     clearCacheClass);
  sendMethod(class, NAME_feature, NAME_version, 2,
	     "feature=name", "value=[any]",
	     "Register class feature",
	     featureClass);
  sendMethod(class, NAME_hasFeature, NAME_version, 2,
	     "feature=name", "value=[any]",
	     "Test if class has feature",
	     hasFeatureClass);
#ifdef O_RUNTIME
  sendMethod(class, NAME_source, NAME_runtime, 1, "source_location*",
	     "Dummy method",
	     rtSourceClass);
#endif
  sendMethod(class, NAME_boundSendMethod, NAME_cache, 1, "name",
	     "Test if class defines send_method `name'",
	     boundSendMethodClass);
  sendMethod(class, NAME_boundGetMethod, NAME_cache, 1, "name",
	     "Test if class defines get_method `name'",
	     boundGetMethodClass);

  getMethod(class, NAME_subClass, NAME_oms, "class", 1, "name",
	    "Create a class below this one (or return existing)",
	    getSubClassClass);
  getMethod(class, NAME_instance, NAME_oms, "object", 1,
	    "argument=unchecked ...",
	    "Create instance of the class from argument",
	    getInstanceClassv);
  getMethod(class, NAME_instanceVariable, NAME_meta, "variable", 1, "name|int",
	    "Get instance variable from name of offset",
	    getInstanceVariableClass);
#ifndef O_RUNTIME
  getMethod(class, NAME_manHeader, NAME_manual, "string", 0,
	    "New string with with term description",
	    getManHeaderClass);
  getMethod(class, NAME_manSummary, NAME_manual, "string", 0,
	    "New string with header and summary",
	    getManSummaryClass);
#endif
  getMethod(class, NAME_getMethod, NAME_meta, "get_method|variable", 1, "name",
	    "Method implementing named get behaviour",
	    getGetMethodClass);
  getMethod(class, NAME_sendMethod, NAME_meta, "send_method|variable", 1,
	    "name",
	    "Method implementing named get behaviour",
	    getSendMethodClass);
  getMethod(class, NAME_superClassName, NAME_type, "name", 0,
	    "Name of super-class or @nil (term description",
	    getSuperClassNameClass);
  getMethod(class, NAME_convert, DEFAULT, "class", 1, "any",
	    "Convert class name",
	    getConvertClass);
  getMethod(class, NAME_lookup, NAME_oms, "class", 2,
	    "name=name", "super=[class]",
	    "Lookup in @classes and verify super",
	    getLookupClass);
  getMethod(class, NAME_feature, NAME_version, "any", 1, "feature=name",
	    "Get value of given feature",
	    getFeatureClass);

		 /*******************************
		 *	   RESOURCE FUNCTIONS	*
		 *******************************/

  sendMethod(class, NAME_resource, NAME_resource, 1, "resource",
	     "Attach a resource to a class",
	     resourceClass);
  sendMethod(class, NAME_resourceValue, NAME_resource, 2, "name", "any",
	     "Set value of named resource",
	     resourceValueClass);
  getMethod(class, NAME_resource, NAME_resource, "resource", 1, "name",
	    "Associated resource from name",
	    getResourceClass);
  getMethod(class, NAME_catchAll, NAME_resource, "value=any", 1,
	    "resource=name",
	    "Get resource-value",
	    getResourceValueClass);

  succeed;
}

