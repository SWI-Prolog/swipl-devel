/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>

static status	initialiseVariable(Variable var, Name name, Type type,
				   Name access, StringObj doc, Name group);
static status	typeVariable(Variable var, Type type);

Variable
createVariable(Name name, Type type, Name access)
{ Variable var;

  var = alloc(sizeof(struct variable));
  initHeaderObj(var, ClassVariable);
  var->name          = var->access = (Name) NIL;
  var->group	     = NIL;
  var->offset        = (Int) NIL;
  var->type	     = (Type) NIL;
  var->dflags	     = (ulong) ZERO;
  var->context	     = NIL;
  var->summary       = NIL;
  var->init_function = NIL;
  var->alloc_value   = NIL;

  TRY( initialiseVariable(var, name, type, access, DEFAULT, DEFAULT) );
  createdObject(var, NAME_new);

  return var;
}


static status
initialiseVariable(Variable var, Name name, Type type, Name access,
		   StringObj doc, Name group)
{ initialiseBehaviour((Behaviour) var, name, NIL);

  if ( isDefault(type) )   type   = TypeAny;
  if ( isDefault(access) ) access = NAME_both;
  if ( isDefault(doc) )    doc    = NIL;

  assign(var, group,   group);
  assign(var, access,  access);
  assign(var, offset,  ZERO);
  assign(var, summary, doc);

  var->alloc_value = NIL;
  typeVariable(var, type);
  
  succeed;
}


static status
typeVariable(Variable var, Type type)
{ assign(var, type, type);
  clearDFlag(var, D_CLONE|D_SAVE);
  
  if ( type->kind == NAME_alien )
  { setDFlag(var, D_CLONE_ALIEN|D_ALIEN);
    var->alloc_value = NULL;
  } else
  { setDFlag(var, D_SAVE_NORMAL);
    setDFlag(var, D_CLONE_RECURSIVE);
  }

  succeed;
}


status
cloneStyleVariable(Variable var, Name style)
{ clearDFlag(var, D_CLONE);

  if ( equalName(style, NAME_recursive) )
    setDFlag(var, D_CLONE_RECURSIVE);
  else if ( equalName(style, NAME_reference) )
    setDFlag(var, D_CLONE_REFERENCE);
  else if ( equalName(style, NAME_value) )
    setDFlag(var, D_CLONE_VALUE);
  else if ( equalName(style, NAME_alien) )
    setDFlag(var, D_CLONE_ALIEN);
  else if ( equalName(style, NAME_nil) )
    setDFlag(var, D_CLONE_NIL);
  else
    fail;

  succeed;
}


status
saveStyleVariable(Variable var, Name style)
{ clearDFlag(var, D_SAVE);

  if ( equalName(style, NAME_normal) )
    setDFlag(var, D_SAVE_NORMAL);
  else if ( equalName(style, NAME_nil) )
    setDFlag(var, D_SAVE_NIL);
  else
    fail;

  succeed;
}


static Name
getCloneStyleVariable(Variable var)
{ if ( onDFlag(var, D_CLONE_RECURSIVE) )
    answer(NAME_recursive);
  if ( onDFlag(var, D_CLONE_REFERENCE) )
    answer(NAME_reference);
  if ( onDFlag(var, D_CLONE_VALUE) )
    answer(NAME_value);
  if ( onDFlag(var, D_CLONE_ALIEN) )
    answer(NAME_alien);
  if ( onDFlag(var, D_CLONE_NIL) )
    answer(NAME_nil);

  fail;
}


static Name
getSaveStyleVariable(Variable var)
{ if ( onDFlag(var, D_SAVE_NORMAL) )
    answer(NAME_normal);
  if ( onDFlag(var, D_SAVE_NIL) )
    answer(NAME_nil);

  fail;
}


status
sendAccessVariable(Variable var)
{ if ( var->access == NAME_both || var->access == NAME_send )
    succeed;
  fail;
}


status
getAccessVariable(Variable var)
{ if ( var->access == NAME_both || var->access == NAME_get )
    succeed;
  fail;
}


static Type
getArgumentTypeVariable(Variable var, Int n)
{ if ( sendAccessVariable(var) && (isDefault(n) || n == ONE) )
    answer(var->type);

  fail;
}


static Type
getReturnTypeVariable(Variable var)
{ if ( getAccessVariable(var) )
    answer(var->type);

  fail;
}

		 /*******************************
		 *	   INITIAL VALUE	*
		 *******************************/

status
allocValueVariable(Variable var, Any value)
{ Any old = var->alloc_value;

  var->alloc_value = value;
  if ( isObject(value) && !isProtectedObj(value) )
    addRefObject(var, value);
  if ( isObject(old) && !isProtectedObj(old) )
    delRefObject(var, old);

  succeed;
}


Any
getAllocValueVariable(Variable var)
{ answer(var->alloc_value);		/* alien = NULL --> fail */
}


status
initFunctionVariable(Variable var, Any f)
{ assign(var, init_function, f);

  if ( instanceOfObject(var->context, ClassClass) )
    prepareClass(var->context);		/* update class <-has_init_functions */

  succeed;
}


status
initialValueVariable(Variable var, Any value)
{ if ( instanceOfObject(value, ClassConstant) ||
       instanceOfObject(value, ClassName) ||
       isInteger(value) )
  { allocValueVariable(var, value);
    initFunctionVariable(var, NIL);
  } else
  { allocValueVariable(var, NIL);
    initFunctionVariable(var, value);
  }

  succeed;
}


		/********************************
		*          EXECUTION		*
		********************************/

status
sendVariable(Variable var, Any rec, int argc, const Any argv[])
{ goal goal;
  Goal g = &goal;
  status rval;
  Any value, old;
  Any *field = &(((Instance)rec)->slots[valInt(var->offset)]);

  pushGoal(g, var, rec, var->name, argc, argv);
  traceEnter(g);

  if ( argc != 1 )
  { errorPce(var, NAME_argumentCount, ONE);
    failGoal;
  }
  if ( !(value = checkType(argv[0], var->type, rec)) &&
       !(isFreeingObj(rec) && isNil(value = argv[0])) &&
       !(isCreatingObj(rec) && isDefault(value = argv[0])) )
	
  { errorTypeMismatch(rec, var, 1, var->type);
    failGoal;
  }

  rval = SUCCEED;

  if ( (old = *field) != value )
  { *field = value;

    if ( isObject(value) && !isProtectedObj(value) )
      addRefObject(rec, value);
    if ( isObject(old) && !isProtectedObj(old) )
      delRefObject(rec, old);
    if ( onFlag(rec, F_INSPECT) )
      (*(classOfObject(rec))->changedFunction)(rec, field);
  }

out:
  traceReturn(g, rval);
  popGoal();
  return rval;
}


Any
getGetVariable(Variable var, Any rec, int argc, const Any argv[])
{ goal goal;
  Goal g = &goal;
  Any *field = &(((Instance)rec)->slots[valInt(var->offset)]);
  Any rval = *field;

  pushGoal(g, var, rec, var->name, argc, argv);
  traceEnter(g);

  if ( argc != 0 )
  { errorPce(var, NAME_argumentCount, ZERO);
    failGoal;
  }

  if ( isDefault(rval) && !mayBeDefaultType(var->type) )
  { Any value;

    if ( (value = getResourceValueObject(rec, var->name)) )
    { Any v2;

      if ( (v2 = checkType(value, var->type, rec)) )
      { assignField(rec, field, v2);
	outGoal(v2);
      } else
      { errorPce(var, NAME_incompatibleResource, 0);
	failGoal;
      }
    } else if ( instanceOfObject(rec, ClassClass) &&
		((Class)rec)->realised != ON )
    { realiseClass(rec);
      rval = *field;
    } else
    { errorPce(var, NAME_noResource, 0);
      failGoal;
    }
  }

out:
  traceAnswer(g, rval);
  popGoal();
  return rval;
}


		/********************************
		*            TRACING		*
		********************************/

static void
traceVariable(Variable v, Goal g, Name port)
{ int i;

  writef("V %O %s%s: ", g->receiver, getAccessArrowVariable(v), v->name);
  for(i = 0; i < g->argc; i++)
  { if ( i == 0 )
      writef("%O", g->argv[i]);
    else
      writef(", %O", g->argv[i]);
  }
}


Name
getGroupVariable(Variable v)
{ if ( isDefault(v->group) )
  { Class class = v->context;

    TRY( instanceOfObject(class, ClassClass) );
    for( class = class->super_class; notNil(class); class = class->super_class)
    { Vector vector = class->instance_variables;
      int n;
      
      for(n=0; n<valInt(vector->size); n++)
      { Variable var = vector->elements[n];

	if ( var->name == v->name && notDefault(var->group) )
	  answer(var->group);
      }
    }

    fail;
  }

  answer(v->group);
}

		/********************************
		*        MANUAL SUPPORT		*
		********************************/

Name
getAccessArrowVariable(Variable v)
{ if ( equalName(v->access, NAME_none) ) return CtoName("-");
  if ( equalName(v->access, NAME_get)  ) return CtoName("<-");
  if ( equalName(v->access, NAME_send) ) return CtoName("->");
  if ( equalName(v->access, NAME_both) ) return CtoName("<->");

  fail;
}


static Name
getContextNameVariable(Variable v)
{ if ( instanceOfObject(v->context, ClassClass) )
  { Class class = v->context;

    answer(class->name);
  }
  
  answer(CtoName("???"));
}


static Name
getManIdVariable(Variable v)
{ char buf[LINESIZE];

  sprintf(buf, "V.%s.%s",
	  strName(getContextNameVariable(v)),
	  strName(v->name));

  answer(CtoName(buf));
}


static Name
getManIndicatorVariable(Variable v)
{ answer(CtoName("V"));
}


static StringObj
getManSummaryVariable(Variable v)
{ char buf[LINESIZE];

  buf[0] = EOS;
  strcat(buf, "V\t");

  if ( instanceOfObject(v->context, ClassClass) )
  { Class class = v->context;

    strcat(buf, strName(class->name));
    strcat(buf, " ");
  }
  
  strcat(buf, strName(getAccessArrowVariable(v)));
  strcat(buf, strName(v->name));
  strcat(buf, ": ");
  strcat(buf, strName(v->type->fullname));
  if ( notNil(v->summary) )
  { strcat(buf, "\t");
    strcat(buf, strName(v->summary));
  }
  if ( send(v, NAME_manDocumented, 0) != FAIL )
    strcat(buf, " (+)");

  answer(CtoString(buf));
}


static Name
getPrintNameVariable(Variable var)
{ char buf[LINESIZE];

  sprintf(buf, "%s %s%s",
	  strName(getContextNameVariable(var)),
	  strName(getAccessArrowVariable(var)),
	  strName(var->name));

  answer(CtoName(buf));
}


status
makeClassVariable(Class class)
{ sourceClass(class, makeClassVariable, __FILE__, "$Revision$");

  localClass(class, NAME_group, NAME_manual, "[name]", NAME_none,
	     "Conceptual group of variable");
  localClass(class, NAME_access, NAME_behaviour,
	     "{none,send,get,both}", NAME_get,
	     "Read/write access");
  localClass(class, NAME_type, NAME_type, "type", NAME_get,
	     "Type check");
  localClass(class, NAME_offset, NAME_storage, "int", NAME_get,
	     "Offset in instance structure");
  localClass(class, NAME_summary, NAME_manual, "string*", NAME_both,
	     "Summary documentation");
  localClass(class, NAME_initFunction, NAME_oms, "any*", NAME_both,
	     "Function to initialise the variable");
  localClass(class, NAME_allocValue, NAME_oms, "alien:void *", NAME_both,
	     "Value used to when allocating");

  termClass(class, "variable", 3, NAME_name, NAME_type, NAME_access);
  setTraceFunctionClass(class, traceVariable);

  storeMethod(class, NAME_type, typeVariable);
  storeMethod(class, NAME_initFunction, initFunctionVariable);

  sendMethod(class, NAME_initialise, DEFAULT,
	     5, "name=name", "type=[type]", "access=[{none,send,get,both}]",
	     "summary=[string]*", "group=[name]",
	     "Create from name, type, access and doc",
	     initialiseVariable);
  sendMethod(class, NAME_send, NAME_execute, 2, "receiver=object",
	     "value=unchecked ...",
	     "Invoke (write) variable in object",
	     sendVariable);
  sendMethod(class, NAME_getAccess, NAME_meta, 0,
	     "Test if variable has read access",
	     getAccessVariable);
  sendMethod(class, NAME_sendAccess, NAME_meta, 0,
	     "Test if variable has write access",
	     sendAccessVariable);
  sendMethod(class, NAME_cloneStyle, NAME_copy,
	     1, "{recursive,reference,value,alien,nil}",
	     "Clone-style for this slot",
	     cloneStyleVariable);
  sendMethod(class, NAME_saveStyle, NAME_file, 1, "{normal,nil}",
	     "Slot saved as @nil or its value",
	     saveStyleVariable);
  sendMethod(class, NAME_allocValue, NAME_oms, 1, "any|function",
	     "Value after allocation when instantiated",
	     initialValueVariable);
  sendMethod(class, NAME_initialValue, NAME_oms, 1, "any|function",
	     "Initial value for this variable",
	     initialValueVariable);

  getMethod(class, NAME_get, NAME_execute, "unchecked", 2,
	    "receiver=object", "unchecked ...",
	    "Invoke (read) variable in object",
	    getGetVariable);
  getMethod(class, NAME_manId, NAME_manual, "name", 0,
	    "Card Id for variable",
	    getManIdVariable);
  getMethod(class, NAME_manIndicator, NAME_manual, "name", 0,
	    "Manual type indicator (`V')",
	    getManIndicatorVariable);
  getMethod(class, NAME_manSummary, NAME_manual, "string", 0,
	    "New string with summary",
	    getManSummaryVariable);
  getMethod(class, NAME_argumentType, NAME_meta, "type", 1, "index=[int]",
	    "Type of n-th1 argument if <-access includes `send'",
	    getArgumentTypeVariable);
  getMethod(class, NAME_returnType, NAME_meta, "type", 0,
	    "Return type if <-access includes `get'",
	    getReturnTypeVariable);
  getMethod(class, NAME_cloneStyle, NAME_copy, "name", 0,
	    "Clone style for this slot",
	    getCloneStyleVariable);
  getMethod(class, NAME_saveStyle, NAME_file, "{normal,nil}", 0,
	    "Save style for this slot",
	    getSaveStyleVariable);
  getMethod(class, NAME_contextName, NAME_manual, "name", 0,
	    "Name of context class",
	    getContextNameVariable);
  getMethod(class, NAME_accessArrow, NAME_manual, "{-,<-,->,<->}", 0,
	    "Arrow indicating access-rights",
	    getAccessArrowVariable);
  getMethod(class, NAME_printName, NAME_textual, "name", 0,
	    "Class <->Name",
	    getPrintNameVariable);
  getMethod(class, NAME_group, NAME_manual, "name", 0,
	    "(Possible inherited) group-name",
	    getGroupVariable);
  getMethod(class, NAME_allocValue, NAME_oms, "any|function", 0,
	    "Initial value when instantiated",
	     getAllocValueVariable);

  succeed;
}


		/********************************
		*    CLASS DELEGATE-VARIABLE	*
		********************************/

static status
initialiseDelegateVariable(DelegateVariable var, Name name, Type type,
			   Name access, Name wrapper,
			   StringObj doc, Name group)
{ initialiseVariable((Variable) var, name, type, access, doc, group);

  assign(var, wrapper, wrapper);
  succeed;
}


status
makeClassDelegateVariable(Class class)
{ sourceClass(class, makeClassDelegateVariable, __FILE__, "$Revision$");

  localClass(class, NAME_wrapper, NAME_change, "name*", NAME_both,
	     "Wrapper to take care of side-effects");

  termClass(class, "delegate_variable",
	    4, NAME_name, NAME_type, NAME_access, NAME_wrapper);

  sendMethod(class, NAME_initialise, DEFAULT, 6,
	     "name=name", "type=[type]", "access=[{none,send,get,both}]",
	     "wrapper=name", "summary=[string]*", "group=[name]*",
	     "Create from name, type, access and doc",
	     initialiseDelegateVariable);

  succeed;
}


