/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>

static status	initialiseVariable(Variable var, Name name, Type type,
				   Name access, StringObj doc, Name group,
				   Any initial);
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

  TRY(initialiseVariable(var, name, type, access, DEFAULT, DEFAULT, DEFAULT));
  createdObject(var, NAME_new);

  return var;
}


static status
initialiseVariable(Variable var, Name name, Type type, Name access,
		   StringObj doc, Name group, Any initial)
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
  if ( notDefault(initial) )
    initialValueVariable(var, initial);
  
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

  if ( style == NAME_recursive )
    setDFlag(var, D_CLONE_RECURSIVE);
  else if ( style == NAME_reference )
    setDFlag(var, D_CLONE_REFERENCE);
  else if ( style == NAME_value )
    setDFlag(var, D_CLONE_VALUE);
  else if ( style == NAME_alien )
    setDFlag(var, D_CLONE_ALIEN);
  else if ( style == NAME_nil )
    setDFlag(var, D_CLONE_NIL);
  else if ( style == NAME_referenceChain )
    setDFlag(var, D_CLONE_REFCHAIN);
  else
    fail;

  succeed;
}


status
saveStyleVariable(Variable var, Name style)
{ clearDFlag(var, D_SAVE);

  if ( style == NAME_normal )
    setDFlag(var, D_SAVE_NORMAL);
  else if ( style == NAME_nil )
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
  if ( onDFlag(var, D_CLONE_REFCHAIN) )
    answer(NAME_referenceChain);
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

#ifdef O_RUNTIME
#undef failGoal
#undef outGoal
#define failGoal rval = FAIL; goto out
#define outGoal(v) rval = (v); goto out
#endif


status
sendVariable(Variable var, Any rec, int argc, const Any argv[])
{ status rval;
  Any value, old;
  Any *field = &(((Instance)rec)->slots[valInt(var->offset)]);
#ifndef O_RUNTIME
  goal goal;
  Goal g = &goal;

  pushGoal(g, var, rec, var->name, argc, argv);
  traceEnter(g);
#endif

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
#ifndef O_RUNTIME
    if ( onFlag(rec, F_INSPECT) )
      (*(classOfObject(rec))->changedFunction)(rec, field);
#endif
  }

out:
#ifndef O_RUNTIME
  traceReturn(g, rval);
  popGoal();
#endif
  return rval;
}


Any
getGetVariable(Variable var, Any rec, int argc, const Any argv[])
{ Any *field = &(((Instance)rec)->slots[valInt(var->offset)]);
  Any rval = *field;

#ifndef O_RUNTIME
  goal goal;
  Goal g = &goal;

  pushGoal(g, var, rec, var->name, argc, argv);
  traceEnter(g);
#endif

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
#ifndef O_RUNTIME
  traceAnswer(g, rval);
  popGoal();
#endif
  return rval;
}


		/********************************
		*            TRACING		*
		********************************/

#ifndef O_RUNTIME
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
#endif


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


#ifndef O_RUNTIME
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
#endif /*O_RUNTIME*/


static Name
getPrintNameVariable(Variable var)
{ char buf[LINESIZE];

  sprintf(buf, "%s %s%s",
	  strName(getContextNameVariable(var)),
	  strName(getAccessArrowVariable(var)),
	  strName(var->name));

  answer(CtoName(buf));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static const char *T_initialise[] =
        { "name=name", "type=[type]", "access=[{none,send,get,both}]", "summary=[string]*", "group=[name]", "initial_value=[any|function]" };
static const char *T_get[] =
        { "receiver=object", "unchecked ..." };
static const char *T_send[] =
        { "receiver=object", "value=unchecked ..." };

/* Instance Variables */

static const vardecl var_variable[] =
{ IV(NAME_group, "[name]", IV_NONE,
     NAME_manual, "Conceptual group of variable"),
  IV(NAME_access, "{none,send,get,both}", IV_GET,
     NAME_behaviour, "Read/write access"),
  SV(NAME_type, "type", IV_GET|IV_STORE, typeVariable,
     NAME_type, "Type check"),
  IV(NAME_offset, "int", IV_GET,
     NAME_storage, "Offset in instance structure"),
  IV(NAME_summary, "string*", IV_BOTH,
     NAME_manual, "Summary documentation"),
  SV(NAME_initFunction, "any*", IV_BOTH|IV_STORE, initFunctionVariable,
     NAME_oms, "Function to initialise the variable"),
  IV(NAME_allocValue, "alien:void *", IV_BOTH,
     NAME_oms, "Value used to when allocating")
};

/* Send Methods */

static const senddecl send_variable[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseVariable,
     DEFAULT, "Create from name, type, access, doc, group and initial value"),
  SM(NAME_cloneStyle, 1, "{recursive,reference,reference_chain,value,alien,nil}", cloneStyleVariable,
     NAME_copy, "Clone-style for this slot"),
  SM(NAME_send, 2, T_send, sendVariable,
     NAME_execute, "Invoke (write) variable in object"),
  SM(NAME_saveStyle, 1, "{normal,nil}", saveStyleVariable,
     NAME_file, "Slot saved as @nil or its value"),
  SM(NAME_getAccess, 0, NULL, getAccessVariable,
     NAME_meta, "Test if variable has read access"),
  SM(NAME_sendAccess, 0, NULL, sendAccessVariable,
     NAME_meta, "Test if variable has write access"),
  SM(NAME_allocValue, 1, "any|function", initialValueVariable,
     NAME_oms, "Value after allocation when instantiated"),
  SM(NAME_initialValue, 1, "any|function", initialValueVariable,
     NAME_oms, "Initial value for this variable")
};

/* Get Methods */

static const getdecl get_variable[] =
{ GM(NAME_cloneStyle, 0, "name", NULL, getCloneStyleVariable,
     NAME_copy, "Clone style for this slot"),
  GM(NAME_get, 2, "unchecked", T_get, getGetVariable,
     NAME_execute, "Invoke (read) variable in object"),
  GM(NAME_saveStyle, 0, "{normal,nil}", NULL, getSaveStyleVariable,
     NAME_file, "Save style for this slot"),
  GM(NAME_accessArrow, 0, "{-,<-,->,<->}", NULL, getAccessArrowVariable,
     NAME_manual, "Arrow indicating access-rights"),
  GM(NAME_contextName, 0, "name", NULL, getContextNameVariable,
     NAME_manual, "Name of context class"),
  GM(NAME_group, 0, "name", NULL, getGroupVariable,
     NAME_manual, "(Possible inherited) group-name"),
#ifndef O_RUNTIME
  GM(NAME_manId, 0, "name", NULL, getManIdVariable,
     NAME_manual, "Card Id for variable"),
  GM(NAME_manIndicator, 0, "name", NULL, getManIndicatorVariable,
     NAME_manual, "Manual type indicator (`V')"),
  GM(NAME_manSummary, 0, "string", NULL, getManSummaryVariable,
     NAME_manual, "New string with summary"),
#endif /*O_RUNTIME*/
  GM(NAME_argumentType, 1, "type", "index=[int]", getArgumentTypeVariable,
     NAME_meta, "Type of n-th1 argument if <-access includes `send'"),
  GM(NAME_returnType, 0, "type", NULL, getReturnTypeVariable,
     NAME_meta, "Return type if <-access includes `get'"),
  GM(NAME_allocValue, 0, "any|function", NULL, getAllocValueVariable,
     NAME_oms, "Initial value when instantiated"),
  GM(NAME_printName, 0, "name", NULL, getPrintNameVariable,
     NAME_textual, "Class <->Name")
};

/* Resources */

static const resourcedecl rc_variable[] =
{ 
};

/* Class Declaration */

static Name variable_termnames[] = { NAME_name, NAME_type, NAME_access };

ClassDecl(variable_decls,
          var_variable, send_variable, get_variable, rc_variable,
          3, variable_termnames,
          "$Rev$");


status
makeClassVariable(Class class)
{ declareClass(class, &variable_decls);
  setTraceFunctionClass(class, traceVariable);

  succeed;
}


		/********************************
		*    CLASS DELEGATE-VARIABLE	*
		********************************/

static status
initialiseDelegateVariable(DelegateVariable var, Name name, Type type,
			   Name access, Name wrapper,
			   StringObj doc, Name group, Any initial)
{ initialiseVariable((Variable) var, name, type, access, doc, group, initial);

  assign(var, wrapper, wrapper);
  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static const char *T_initialise_delegate[] =
        { "name=name", "type=[type]", "access=[{none,send,get,both}]",
	  "wrapper=name", "summary=[string]*", "group=[name]*",
	  "initial_value=[any|function]" };

/* Instance Variables */

static const vardecl var_delegateVariable[] =
{ IV(NAME_wrapper, "name*", IV_BOTH,
     NAME_change, "Wrapper to take care of side-effects")
};

/* Send Methods */

static const senddecl send_delegateVariable[] =
{ SM(NAME_initialise, 7, T_initialise_delegate, initialiseDelegateVariable,
     DEFAULT, "Create from name, type, access and doc")
};

/* Get Methods */

static const getdecl get_delegateVariable[] =
{ 
};

/* Resources */

static const resourcedecl rc_delegateVariable[] =
{ 
};

/* Class Declaration */

static Name delegateVariable_termnames[] =
	{ NAME_name, NAME_type, NAME_access, NAME_wrapper };

ClassDecl(delegateVariable_decls,
          var_delegateVariable, send_delegateVariable,
	  get_delegateVariable, rc_delegateVariable,
          4, delegateVariable_termnames,
          "$Rev$");


status
makeClassDelegateVariable(Class class)
{ return declareClass(class, &delegateVariable_decls);
}


