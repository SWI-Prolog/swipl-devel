/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>
#include <h/graphics.h>
#include <h/interface.h>

static int	check_object(Any, Bool, HashTable, int);
static status	makeTempObject(Any obj);
static status	unlinkObject(Any obj);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  CREATING OBJECTS	

PCE objects are created by C-functions.   Some objects  are  used as a
temporary   argument    to  a   method  (e.g.    send(@box,   move,
point(30,30)).   Others  are created   as  an `end-point' object (e.g.
new(@box,  box(50,50)))  and yet others  are stored as an attribute to
other objects (e.g.  send(@sheet, size, size(50,50))).
 
To deal with this problem, the following schemas may be used:

(1)	someFunction()
	{ Any obj = globalObject(Name, Class, ....);
	
	  .....
	}

This schema is to be used if the object will be used as an `end-point'
object.  The object will be locked  againts  the garbage collector and
does not have a reference.

(2)	someFunction(me)
	{ assign(me, field, newObject(....));
	
	  .....
	}

This schema is  to be used if  the object is  immediately connected to
another object.  This will give the object a reference and preserve it
until it is detached from its last object.


(3)	someFunction()
	{ .....
	  
	  answer( answerObject(...) );
	}

This  construct   is  used  if the    return  value  of some  function
(method) is a new object.  It  indicates nobody has declared itself
responsible for  the object.  The object is  marked  with the F_ANSWER
flag and added to the   answerTable.   If the   object is assigned  to
another object, it  will become  a normal attribute  object.  If it is
locked,  it will become an object  under  program control.   If non of
these  happens, it will be considered  garbage as soon  as the garbage
collector  is activated.  Status `answer'   is  also used for  objects
created from the host language.


(4)	someFunction()
	{ Any obj = tempObject(....);
	
	  .....
	  
	  considerPreserveObject(obj);
	}

This schema is to be used for objects that are created to be passed as
an argument to some method.   The object will  be given a reference
to avoid drop-out.    The function   considerPreserveObject()  acts as
follows:

   First it lowers the reference count by 1, next:

	1) If the object is locked, it does nothing (`end-point')
	2) If the object has references, it does nothing (attribute)
	3) Otherwise the object is freed (argument)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
allocObject(Class class, int funcs)
{ Instance obj;
  int size = valInt(class->instance_size);
  int i, slots = valInt(class->slots);

  obj = alloc(size);
  initHeaderObj(obj, class);

  if ( class->boot )			/* TBD: use prototypes? */
  { slots = (size - ((int) &((Instance) NULL)->slots[0])) / sizeof(Any);

    for (i = 0; i < slots; i++)
      obj->slots[i] = ((i < class->boot) ? NIL : (Any) NULL);
  } else
  { Variable *var = (Variable *) &class->instance_variables->elements[0];
    Any *field = &obj->slots[0];

    for( ; --slots >= 0; var++, field++)
      *field = (*var)->alloc_value;
  }

  return obj;
}


static inline status
init_slots(Instance obj, int slots, Variable *var, Any *field)
{ for( ; --slots >= 0; var++, field++)
  { Any value;
    Function f = (*var)->init_function;

    if ( notNil(f) )
    { if ( !(value = expandCodeArgument(f)) ||
	   !sendVariable(*var, obj, 1, &value) ) /* assignField? */
	return errorPce(*var, NAME_initVariableFailed, obj);
    }
  }

  succeed;
}


status
initialiseObject(Instance obj, int argc, const Any argv[])
{ Class class = classOfObject(obj);
  int slots = valInt(class->slots);
  Variable *var = (Variable *) &class->instance_variables->elements[0];
  Any *field = &obj->slots[0];
  status rval;

  if ( class->has_init_functions == ON )
  { Any receiver_save = RECEIVER->value;
    Any receiver_class_save = RECEIVER_CLASS->value;

    RECEIVER->value = obj;
    RECEIVER_CLASS->value = classOfObject(obj);
    withArgs(argc, argv, rval = init_slots(obj, slots, var, field));
    RECEIVER_CLASS->value = receiver_class_save;
    RECEIVER->value = receiver_save;
  } else
    rval = init_slots(obj, slots, var, field);

  return rval;
}


static void
unallocObject(Any obj)
{ unalloc(valInt(classOfObject(obj)->instance_size), obj);
}


Any
createObjectv(Name assoc, Class class, int argc, const Any argv[])
{ Any rval;
  goal goal;
  Goal g = &goal;

  pushGoal(g, VmiNew, class, NAME_new, argc, argv);
  traceEnter(g);
  
  if ( !instanceOfObject(class, ClassClass) )
  { Class c2;

    if ( !(c2 = checkType(class, TypeClass, NIL)) )
    { errorPce(VmiNew, NAME_noClass, class);
      failGoal;
    } else
      class = c2;
  }
  if ( class->realised != ON )
    realiseClass(class);

  if ( notNil(class->lookup_method) )
  { if ( (rval = getGetGetMethod(class->lookup_method,
				 class, argc, argv)) )
      goto out;
  }

  if ( notNil(assoc) )
  { if ( getObjectAssoc(assoc) )
      exceptionPce(PCE, NAME_redefinedAssoc, assoc, 0);
    if ( getObjectAssoc(assoc) )
    { errorPce(VmiNew, NAME_redefinedAssoc, assoc, 0);
      failGoal;
    }
  }

  rval = allocObject(class, TRUE);
  if ( notNil(assoc) )
    newAssoc(assoc, rval);

  if ( sendSendMethod(class->initialise_method, rval, argc, argv) )
  { createdClass(class, rval, NAME_new);
  } else
  { ArgVector(av, argc+1);
    int ac = 0, i = 0;

    av[ac++] = rval;			/* @arg1 */
    for(; i<argc; i++)
      av[ac++] = argv[i];

    exceptionPcev(PCE, NAME_initialiseFailed, ac, av);
    deleteAssoc(rval);
    unallocObject(rval);
    rval = FAIL;
  }

out:
  traceAnswer(g, rval);
  popGoal();

  answer(rval);
}


Any
newObjectv(Class class, int argc, const Any argv[])
{ return createObjectv(NIL, class, argc, argv);
}


static Any
globalObjectv(Name assoc, Class class, int argc, const Any argv[])
{ Any rval;

  DEBUG_BOOT(Cprintf("globalObject @%s ... ", pp(assoc)));
  rval = createObjectv(assoc, class, argc, argv);
  DEBUG_BOOT(Cprintf("ok\n"););

  return rval;
}


Any
tempObjectv(Class class, int argc, const Any argv[])
{ Any rval = newObjectv(class, argc, argv);

  makeTempObject(rval);

  return rval;
}


static status
makeTempObject(Any obj)
{ if ( isObject(obj) )
    addCodeReference(obj);
  
  succeed;
}


status
considerPreserveObject(Any obj)
{ if ( isObject(obj) && !isFreedObj(obj) )
  { if ( ((Instance)obj)->references < ONE_CODE_REF )
      errorPce(obj, NAME_negativeCodeReferenceCount);
    delCodeReference(obj);
    freeableObj(obj);
  }

  succeed;
}


Any
answerObjectv(Class class, int argc, Any *argv)
{ Any rval = newObjectv(class, argc, argv);

  if ( rval != FAIL )
    pushAnswerObject(rval);

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			       VARARGS VERSIONS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
newObject(Class class, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, class);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc < VA_PCE_MAX_ARGS );
  va_end(args);

  return newObjectv(class, argc, argv);
}

Any
tempObject(Class class, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;
  Any rval;

  va_start(args, class);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  rval = newObjectv(class, argc, argv);
  makeTempObject(rval);

  return rval;
}


Any
globalObject(Name assoc, Class class, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, class);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc < VA_PCE_MAX_ARGS);
  va_end(args);

  return globalObjectv(assoc, class, argc, argv);
}


Any
answerObject(Class class, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;
  Any rval;

  va_start(args, class);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc < VA_PCE_MAX_ARGS);
  va_end(args);

  rval = newObjectv(class, argc, argv);
  if ( rval )
    pushAnswerObject(rval);

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			    DESTROYING OBJECTS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
freeObject(Any obj)
{ Instance inst = obj;
  Class class;
  status rval;
  goal goal;
  Goal g = &goal;

  if ( nonObject(inst) || onFlag(inst, F_FREED|F_FREEING) )
    succeed;

  pushGoal(g, VmiFree, obj, NAME_free, 0, NULL);
  traceEnter(g);

  if ( isProtectedObj(inst) )		/* cannot be freed */
  { rval = FAIL;
    goto out;
  }

  class = classOfObject(inst);
  freedClass(class, inst);

  unlockObj(inst);			/* release possible lock */
  deleteAnswerObject(inst);		/* delete from AnswerStack */
  setFreeingObj(inst);			/* mark */
  deleteAssoc(inst);			/* delete name association */

  if ( class->unlink_function && !TraceMode )
    rval = (*class->unlink_function)(inst);
  else
    rval = simpleSendv(inst, NAME_unlink, 0, NULL);

  if ( !rval )
    errorPce(inst, NAME_unlinkFailed);

  rval = SUCCEED;
  unlinkObject(inst);
  setFreedObj(inst);			/* freeing finished */

  if ( refsObject(inst) == 0 )
    unallocObject(inst);
  else
  { deferredUnalloced++;
    DEBUG(NAME_free, Cprintf("%s has %ld refs.  Deferring unalloc\n",
			     pp(inst), refsObject(inst)));
  }

out:
  traceReturn(g, rval);
  popGoal();

  return rval;
}


static status
unlinkObject(Any obj)
{ Instance inst = obj;
  Class class = classOfObject(obj);
  int slots = valInt(class->slots);
  int i;

  for(i=0; i < slots; i++)
    if ( isPceSlot(class, i) && isObject(inst->slots[i]) )
      assignField(inst, &inst->slots[i], NIL);

  if ( onFlag(obj, F_ATTRIBUTE|F_CONSTRAINT|F_SENDMETHOD|
	           F_GETMETHOD|F_RECOGNISER|F_HYPER) )
  { if ( onFlag(obj, F_CONSTRAINT) )
    { Chain ch = getAllConstraintsObject(obj, ON);
      Constraint c;

      clearFlag(obj, F_CONSTRAINT);
      for_chain(ch, c, freeObject(c));
      deleteHashTable(ObjectConstraintTable, obj);
    }
    if ( onFlag(obj, F_HYPER) )
    { Chain ch = getAllHypersObject(obj, ON);
      Hyper h;

      clearFlag(obj, F_HYPER);
      for_chain(ch, h,
		{ if ( !onFlag(h, F_FREED|F_FREEING) )
		  { if ( h->from == obj )
		      sendv(h, NAME_unlinkFrom, 0, NULL);
		    else
		      sendv(h, NAME_unlinkTo, 0, NULL);
		  }
		});
      deleteHashTable(ObjectHyperTable, obj);
    }
    if ( onFlag(obj, F_ATTRIBUTE) )
    { clearFlag(obj, F_ATTRIBUTE);
      deleteHashTable(ObjectAttributeTable, obj);
    }
    if ( onFlag(obj, F_SENDMETHOD) )
    { clearFlag(obj, F_SENDMETHOD);
      deleteHashTable(ObjectSendMethodTable, obj);
    }
    if ( onFlag(obj, F_GETMETHOD) )
    { clearFlag(obj, F_GETMETHOD);
      deleteHashTable(ObjectGetMethodTable, obj);
    }
    if ( onFlag(obj, F_RECOGNISER) )
    { clearFlag(obj, F_RECOGNISER);
      deleteHashTable(ObjectRecogniserTable, obj);
    }
  }

  succeed;
}


static status
unlinkingObject(Any obj)
{ if ( isFreeingObj(obj) )
    succeed;

  fail;
}


status
createdObject(Any obj, Name how)
{ Class class;

  if ( (class = classOfObject(obj)) )
    createdClass(class, obj, how);

  succeed;
}


status
succeedObject(Any obj, ...)
{ succeed;
}


status
failObject(Any obj, ...)
{ fail;
}


Any
getFailObject(Any obj)
{ fail;
}


status
virtualObject(Any obj)
{ fail;
}


Any
getVirtualObject(Any obj)
{ fail;
}


Int
getReferencesObject(Any obj)
{ answer(toInt(refsObject(obj)));
}


Int
getCodeReferencesObject(Any obj)
{ answer(toInt(codeRefsObject(obj)));
}


Name
getFlagsObject(Any obj)
{ char tmp[100];
  char *s = tmp;

#define DoFlag(f, c)	*s++ = (onFlag(obj, f) ? c : '-')
  DoFlag(F_PROTECTED, 'P');
  DoFlag(F_LOCKED,    'L');
  DoFlag(F_ANSWER,    'A');
  *s = EOS;
#undef DoFlag

  answer(CtoName(tmp));
}


status
protectObject(Any obj)
{ deleteAnswerObject(obj);		/* status is clear now */

  setProtectedObj(obj);
  succeed;
}


static Bool
getProtectObject(Any obj)
{ answer(isProtectedObj(obj) ? ON : OFF);
}


status
doneObject(Any obj)
{ deleteAnswerObject(obj);
  freeableObj(obj);

  succeed;
}


status
lockObject(Any obj, Bool val)
{ if (val == ON)
  { deleteAnswerObject(obj);		/* status is clear now */
    lockObj(obj);
  } else
  { unlockObj(obj);
  /*freeableObj(obj);			??? */
  }
  succeed;
}


static Bool
getLockObject(Any obj)
{ answer(lockedObj(obj) ? ON : OFF);
}


#ifndef O_RUNTIME
status
inspectObject(Any obj, Bool val)
{ if ( val == ON )
  { setFlag(obj, F_INSPECT);
  } else
  { clearFlag(obj, F_INSPECT);
  }
  
  succeed;
}


Bool
getInspectObject(Any obj)
{ answer(onFlag(obj, F_INSPECT) ? ON : OFF);
}
#endif /*O_RUNTIME*/


Name
getClassNameObject(Any obj)
{ answer(classOfObject(obj)->name);
}


Class
getClassObject(Any obj)
{ answer(classOfObject(obj));
}


Any
getSelfObject(Any obj)
{ answer(obj);
}


/*
instanceOfObject(obj, super)
Any obj;
Class super;
{ if ( isObject(obj) )
  { Class class = classOfObject(obj);
    
    return class == super || (class->tree_index >= super->tree_index &&
			      class->tree_index <  super->neighbour_index);
  }

  fail;
}
*/

status
setSlotInstance(Any obj, Variable var, Any value)
{ Instance inst = obj;

  assignField(inst, &inst->slots[valInt(var->offset)], value);
  succeed;
}


static status
sameClassObject(Any obj1, Any obj2)
{ if ( classOfObject(obj1) == classOfObject(obj2))
    succeed;
  fail;
}


status
nameReferenceObject(Any obj, Name name)
{ Any old;

  if ( (old = getObjectAssoc(name)) == obj )
    succeed;
  if ( old )
    exceptionPce(PCE, NAME_redecaredReference, name, 0);
  if ( (old = getObjectAssoc(name)) )
    errorPce(obj, NAME_redecaredReference, name);

  deleteAssoc(obj);
  if ( notNil(name) )
    newAssoc(name, obj);

  succeed;
}


static Any
getObjectReferenceObject(Any obj)
{ Name name;

  if ( (name = getNameAssoc(obj)) != FAIL )
    answer(name);

  answer(PointerToInt(obj));
}


		/********************************
		*       OBJECT-EXTENSIONS	*
		********************************/


status
constraintObject(Any obj, Constraint c)
{ Chain ch = getAllConstraintsObject(obj, ON);

  return addChain(ch, c);
}


status
deleteConstraintObject(Any obj, Constraint c)
{ Chain ch;

  TRY(ch = getAllConstraintsObject(obj, OFF));

  return deleteChain(ch, c);
}


static status
sendMethodObject(Any obj, Method m)
{ Chain ch = getAllSendMethodsObject(obj, ON);
  
  return prependChain(ch, m);
}

static status
getMethodObject(Any obj, Method m)
{ Chain ch = getAllGetMethodsObject(obj, ON);
  
  return prependChain(ch, m);
}


status
attachHyperObject(Any obj, Hyper h, Any to)
{ Chain ch = getAllHypersObject(obj, ON);
  
  return addChain(ch, h);
}


status
deleteHyperObject(Any obj, Hyper h)
{ Chain ch;

  TRY(ch = getAllHypersObject(obj, OFF));

  return deleteChain(ch, h);
}


status
attributeObject(Any obj, Any name, Any value)
{ Chain ch = getAllAttributesObject(obj, ON);
  Cell cell;

  if ( instanceOfObject(name, ClassAttribute) )
  { Attribute att = (Attribute) name;

    for_cell(cell, ch)
    { Attribute a = cell->value;

      if ( a->name == att->name )
      { assign(a, value, att->value);
	succeed;
      }
    }

    if ( getInstanceVariableClass(classOfObject(obj), att->name) )
      return errorPce(obj, NAME_classHasVariable, att->name);

    return appendChain(ch, att);
  } else /* if instanceOfObject(att, ClassName) */
  { for_cell(cell, ch)
    { Attribute a = cell->value;

      if ( a->name == name )
      { assign(a, value, value);
	succeed;
      }
    }

    if ( getInstanceVariableClass(classOfObject(obj), name) )
      return errorPce(obj, NAME_classHasVariable, name);

    return appendChain(ch, newObject(ClassAttribute, name, value, 0));
  }
}


status
deleteAttributeObject(Any obj, Any att)
{ Chain ch;
  Cell cell;

  TRY(ch = getAllAttributesObject(obj, OFF));

  if ( instanceOfObject(att, ClassAttribute) )
    return deleteChain(ch, att);

  for_cell(cell, ch)
  { Attribute a = cell->value;

    if ( a->name == att )
      return deleteChain(ch, a);
  }

  fail;
}


Any
getAttributeObject(Any obj, Name name)
{ Chain ch;
  Cell cell;

  TRY(ch = getAllAttributesObject(obj, OFF));

  for_cell(cell, ch)
  { Attribute a = cell->value;

    if ( a->name == name )
      answer(a->value);
  }

  fail;
}

		/********************************
		*       RESOLVING METHODS	*
		********************************/

static Tuple
getSendMethodObject(Any obj, Name selector)
{ Any m, rec;

  TRY( m = resolveSendMethodObject(obj, NULL, selector, &rec, NULL) );

  answer(answerObject(ClassTuple, rec, m, 0));
}


static Tuple
getGetMethodObject(Any obj, Name selector)
{ Any m, rec;

  TRY( m = resolveGetMethodObject(obj, NULL, selector, &rec) );

  answer(answerObject(ClassTuple, rec, m, 0));
}


status
hasSendMethodObject(Any obj, Name selector)
{ Any m, rec;

  TRY(m = resolveSendMethodObject(obj, NULL, selector, &rec, NULL));
  succeed;
}


status
hasGetMethodObject(Any obj, Name selector)
{ Any m, rec;

  TRY(m = resolveGetMethodObject(obj, NULL, selector, &rec));
  succeed;
}


		 /*******************************
		 *	  COLLECT METHODS	*
		 *******************************/

static void
mergeMethod(Chain rval, Any m, HashTable done, Code cond)
{ Behaviour b = m;

  if ( !getMemberHashTable(done, b->name) )
  { appendHashTable(done, b->name, b);
    if ( isDefault(cond) || forwardCodev(cond, 1, &m) )
      appendChain(rval, m);
  }
}


static void
mergeMethods(Chain rval, Chain mts, HashTable done, Code cond)
{ Cell cell;

  for_cell(cell, mts)
    mergeMethod(rval, cell->value, done, cond);
}



static void
mergeSendMethodsObject(Any obj, Chain ch, HashTable done, Code cond)
{ Chain methods;
  Class class;
  Cell cell;

  if ( (methods = getAllSendMethodsObject(obj, OFF)) )
    mergeMethods(ch, methods, done, cond);
  if ( (methods = getAllAttributesObject(obj, OFF)) )
    mergeMethods(ch, methods, done, cond);

  for(class = classOfObject(obj); notNil(class); class = class->super_class)
  { Variable var;

    mergeMethods(ch, class->send_methods, done, cond);
    for_vector(class->instance_variables, var,
	       if ( sendAccessVariable(var) )
	         mergeMethod(ch, var, done, cond));
  }

  for_cell(cell, classOfObject(obj)->delegate)
  { Variable var = cell->value;
    Any val;

    if ( (val = getGetVariable(var, obj, 0, NULL)) )
      mergeSendMethodsObject(val, ch, done, cond);
  }
}


static Chain
getFindAllSendMethodsObject(Any obj, Code cond)
{ Chain ch = answerObject(ClassChain, 0);
  static HashTable done = NULL;
  
  if ( !done )
    done = createHashTable(toInt(32), OFF);

  mergeSendMethodsObject(obj, ch, done, cond);
  clearHashTable(done);

  answer(ch);
}


		/********************************
		*        OBJECT ATTRIBUTES	*
		********************************/

Chain
getAllConstraintsObject(Any obj, Bool create)
{ if ( onFlag(obj, F_CONSTRAINT) )
    answer(getMemberHashTable(ObjectConstraintTable, obj));

  if ( create == ON )
  { Chain ch = newObject(ClassChain, 0);

    setFlag(obj, F_CONSTRAINT);
    appendHashTable(ObjectConstraintTable, obj, ch);

    answer(ch);
  }

  fail;
}


Chain
getAllHypersObject(Any obj, Bool create)
{ if ( onFlag(obj, F_HYPER) )
    answer(getMemberHashTable(ObjectHyperTable, obj));

  if ( create == ON )
  { Chain ch = newObject(ClassChain, 0);

    setFlag(obj, F_HYPER);
    appendHashTable(ObjectHyperTable, obj, ch);

    answer(ch);
  }

  fail;
}


Chain
getAllAttributesObject(Any obj, Bool create)
{ if ( onFlag(obj, F_ATTRIBUTE) )
    answer(getMemberHashTable(ObjectAttributeTable, obj));

  if ( create == ON )
  { Chain ch = newObject(ClassChain, 0);

    setFlag(obj, F_ATTRIBUTE);
    appendHashTable(ObjectAttributeTable, obj, ch);

    answer(ch);
  }

  fail;
}


Chain
getAllSendMethodsObject(Any obj, Bool create)
{ if ( onFlag(obj, F_SENDMETHOD) )
    answer(getMemberHashTable(ObjectSendMethodTable, obj));

  if ( create == ON )
  { Chain ch = newObject(ClassChain, 0);

    setFlag(obj, F_SENDMETHOD);
    appendHashTable(ObjectSendMethodTable, obj, ch);

    answer(ch);
  }

  fail;
}


Chain
getAllGetMethodsObject(Any obj, Bool create)
{ if ( onFlag(obj, F_GETMETHOD) )
    answer(getMemberHashTable(ObjectGetMethodTable, obj));

  if ( create == ON )
  { Chain ch = newObject(ClassChain, 0);

    setFlag(obj, F_GETMETHOD);
    appendHashTable(ObjectGetMethodTable, obj, ch);

    answer(ch);
  }

  fail;
}


		/********************************
		*           KLONING		*
		********************************/


typedef struct clone_field *CloneField;

struct clone_field
{ Any		instance;
  Any	       *field;
  Any		old_value;
  CloneField	next;
};

static	HashTable	CloneTable;
static  CloneField     	CloneFields;

static void
addCloneField(Any obj, Any *field, Any old)
{ CloneField kf = alloc(sizeof(struct clone_field));

  kf->instance  = obj;
  kf->field     = field;
  kf->old_value = old;
  kf->next      = CloneFields;
  CloneFields   = kf;
}

static void
destroyCloneFields()
{ while(CloneFields != NULL)
  { CloneField kf = CloneFields;
    CloneFields = kf->next;
    unalloc(sizeof(struct clone_field), kf);
  }
}


Any
getCloneObject(Any obj)
{ Any clone;
  CloneField kf;

  if ( CloneTable == NULL )
    CloneTable = createHashTable(toInt(32), OFF);

  clearHashTable(CloneTable);		/* security for handling reset */
  CloneFields = NULL;

  clone = getClone2Object(obj);
  for(kf = CloneFields; kf != NULL; kf = kf->next)
  { Any kl;

    if ( (kl = getMemberHashTable(CloneTable, kf->old_value)) != FAIL )
      assignField(kf->instance, kf->field, kl);
  }

  clearHashTable(CloneTable);
  destroyCloneFields();

  pushAnswerObject(clone);
  answer(clone);
}


status
clonePceSlots(Any org, Any Clone)
{ Instance clone = Clone;
  Instance me = org;
  Class class = classOfObject(org);
  int i;

  for_vector(class->instance_variables, Variable var,
	     i = valInt(var->offset);
	     if ( onDFlag(var, D_CLONE_RECURSIVE) )
	     { assign(clone, slots[i], getClone2Object(me->slots[i]));
	     } else if ( onDFlag(var, D_CLONE_REFERENCE) )
	     { assign(clone, slots[i], me->slots[i]);
	       addCloneField(clone, &clone->slots[i], me->slots[i]);
	     } else if ( onDFlag(var, D_CLONE_VALUE) )
	     { assign(clone, slots[i], me->slots[i]);
	     } else if ( onDFlag(var, D_CLONE_ALIEN) )
	     { clone->slots[i] = me->slots[i];
	     } else if ( onDFlag(var, D_CLONE_NIL) )
	     { assign(clone, slots[i], NIL);
	       addCloneField(clone, &clone->slots[i], me->slots[i]);
	     });
	   
  succeed;
}


static void
cloneExtenstions(Any me, Any clone)
{ Chain ch;

  if ( (ch = getAllConstraintsObject(me, OFF)) )
  { Chain ch2 = getClone2Object(ch);

    setFlag(clone, F_CONSTRAINT);
    appendHashTable(ObjectConstraintTable, clone, ch2);
  }
  if ( (ch = getAllHypersObject(me, OFF)) )
  { Chain ch2 = getClone2Object(ch);

    setFlag(clone, F_HYPER);
    appendHashTable(ObjectHyperTable, clone, ch2);
  }
  if ( (ch = getAllAttributesObject(me, OFF)) )
  { Chain ch2 = getClone2Object(ch);

    setFlag(clone, F_ATTRIBUTE);
    appendHashTable(ObjectAttributeTable, clone, ch2);
  }
  if ( (ch = getAllSendMethodsObject(me, OFF)) )
  { Chain ch2 = getClone2Object(ch);

    setFlag(clone, F_SENDMETHOD);
    appendHashTable(ObjectSendMethodTable, clone, ch2);
  }
  if ( (ch = getAllGetMethodsObject(me, OFF)) )
  { Chain ch2 = getClone2Object(ch);

    setFlag(clone, F_GETMETHOD);
    appendHashTable(ObjectGetMethodTable, clone, ch2);
  }
  if ( (ch = getAllRecognisersGraphical(me, OFF)) )
  { Chain ch2 = getClone2Object(ch);

    setFlag(clone, F_RECOGNISER);
    appendHashTable(ObjectRecogniserTable, clone, ch2);
  }
}


Any
getClone2Object(Any obj)
{ Class class;
  Instance clone;
  Instance me;

  if ( nonObject(obj) )
    answer(obj);			/* untyped data-structures */

  if ( (clone = getMemberHashTable(CloneTable, obj)) )
  { DEBUG(NAME_clone, Cprintf("%s already cloned into %s\n",
			      pp(obj), pp(clone)));
    answer(clone);
  }

  me = obj;
  class = classOfObject(me);

  if ( equalName(class->cloneStyle, NAME_none) )
    answer(me);
  if ( equalName(class->cloneStyle, NAME_nil) )
    answer(NIL);

  clone = (Instance) allocObject(class, FALSE);
  DEBUG(NAME_clone, Cprintf("%s cloned into %s\n", pp(me), pp(clone)));
  appendHashTable(CloneTable, me, clone);

  cloneExtenstions(me, clone);

  if ( class->cloneFunction != NULL )
    (*class->cloneFunction)(me, clone);
  else
    clonePceSlots(me, clone);

  createdClass(class, clone, NAME_clone);
  answer(clone);
}


Int
getArityObject(Any obj)
{ Class class = classOfObject(obj);  

  if ( isNil(class->term_names) )
    fail;

  answer(class->term_names->size);
}


Name
getFunctorObject(Any obj)
{ answer( classOfObject(obj)->term_functor );
}


Any
getArgObject(Any obj, Int arg)
{ Class class = classOfObject(obj);
  Name selector;

  if ( isNil(class->term_names) )
    fail;

  if ( isName(selector = getElementVector(class->term_names, arg)) )
    answer( get(obj, selector, 0) );

  fail;
}


Any
getSlotObject(Any obj, Any which)
{ Class class = classOfObject(obj);
  Variable var;
  Instance inst = (Instance) obj;

  if ( (var = getInstanceVariableClass(class, which)) )
  { if ( var->type->kind == NAME_alien &&
	 var->name != CtoName("alien:Any") )
      answer(toInt((long)inst->slots[valInt(var->offset)]));
    else
      answer(getGetVariable(var, obj, 0, NULL));
  }

  fail;
}


status
slotObject(Any obj, Any which, Any value)
{ Variable var;

  if ( (var = getInstanceVariableClass(classOfObject(obj), which)) )
    return sendVariable(var, obj, 1, &value);

  return errorPce(obj, NAME_noVariable, which);
}


static status
isOnObject(Any obj, Name selector)
{ if ( get(obj, selector, 0) == ON)
    succeed;
  fail;
}


static status
isOffObject(Any obj, Name selector)
{ if ( get(obj, selector, 0) == OFF)
    succeed;
  fail;
}


static status
hasValueObject(Any obj, Name selector, Any value)
{ if (get(obj, selector, 0) == value)
    succeed;
  fail;
}


static status
notHasValueObject(Any obj, Name selector, Any value)
{ if (get(obj, selector, 0) != value)
    succeed;
  fail;
}

		/********************************
		*           EQUALITY		*
		********************************/

status
equalObject(Any o1, Any o2)
{ return o1 == o2;
}


status
sameReferenceObject(Any o1, Any o2)
{ return o1 == o2;
}

		/********************************
		*         SPECIAL SENDS		*
		********************************/

static status
sendSubObject(Any obj, Name selector, int argc, Any *argv)
{ if ( obj == RECEIVER->value )
  { return sendv(obj, selector, argc, argv);
  } else
    return errorPce(obj, NAME_mustBeToReceiver, RECEIVER->value);
}


static Any
getGetSubObject(Any obj, Name selector, int argc, Any *argv)
{ if ( obj == RECEIVER->value )
    return getv(obj, selector, argc, argv);

  errorPce(obj, NAME_mustBeToReceiver, RECEIVER->value);
  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Object ->send_super: Selector, ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
sendSuperObject(Any obj, Name selector, int argc, const Any argv[])
{ if ( obj == RECEIVER->value )
  { Class current = RECEIVER_CLASS->value;
    status rval;
    
    RECEIVER_CLASS->value = current->super_class;
    if ( notNil(RECEIVER_CLASS->value) )
      rval = vm_send(obj, selector, RECEIVER_CLASS->value, argc, argv);
    else
      rval = FAIL;
    RECEIVER_CLASS->value = current;

    return rval;
  }

  errorPce(obj, NAME_mustBeToReceiver, RECEIVER->value);
  fail;
}


Any
getGetSuperObject(Any obj, Name selector, int argc, const Any argv[])
{ if ( obj == RECEIVER->value )
  { Class current = RECEIVER_CLASS->value;
    Any rval;
    
    RECEIVER_CLASS->value = current->super_class;
    rval = vm_get(obj, selector, RECEIVER_CLASS->value, argc, argv);
    RECEIVER_CLASS->value = current;

    return rval;
  }

  errorPce(obj, NAME_mustBeToReceiver, RECEIVER->value);
  fail;
}


static status
sendClassObject(Any obj, Name selector, int argc, Any *argv)
{ if ( obj == RECEIVER->value )
  { Class current = RECEIVER_CLASS->value;
    status rval;
    
    RECEIVER_CLASS->value = classOfObject(obj);
    rval = vm_send(obj, selector, RECEIVER_CLASS->value, argc, argv);
    RECEIVER_CLASS->value = current;

    return rval;
  }

  errorPce(obj, NAME_mustBeToReceiver, RECEIVER->value);
  fail;
}


static Any
getGetClassObject(Any obj, Name selector, int argc, Any *argv)
{ if ( obj == RECEIVER->value )
  { Class current = RECEIVER_CLASS->value;
    Any rval;
    
    RECEIVER_CLASS->value = classOfObject(obj);
    rval = vm_get(obj, selector, RECEIVER_CLASS->value, argc, argv);
    RECEIVER_CLASS->value = current;

    return rval;
  }

  errorPce(obj, NAME_mustBeToReceiver, RECEIVER->value);
  fail;
}


static status
sendVectorObject(Any obj, int argc, Any *argv)
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

  if ( !(v = checkType(v, TypeVector, NIL)) )
    goto usage;
  else
  { int argn = args+valInt(v->size)-shift;
    ArgVector(av, argn);
    int i, n;
    
    for(i=0; i<args; i++)
      av[i] = argv[i];
    for(n=shift; n<valInt(v->size); n++)
      av[i++] = v->elements[n];

    if ( argn >= 1 )
      return sendv(obj, av[0], argn-1, av+1);
    fail;
  }

usage:
  return errorPce(obj, NAME_badVectorUsage);
}


static Any
getVectorObject(Any obj, int argc, Any *argv)
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

  if ( !(v = checkType(v, TypeVector, NIL)) )
    goto usage;
  else
  { int argn = args+valInt(v->size)-shift;
    ArgVector(av, argn);
    int i, n;
    
    for(i=0; i<args; i++)
      av[i] = argv[i];
    for(n=shift; n<valInt(v->size); n++)
      av[i++] = v->elements[n];

    if ( argn >= 1 )
      return getv(obj, (Name) av[0], argn-1, av+1);
    fail;
  }

usage:
  errorPce(obj, NAME_badVectorUsage);
  fail;
}


static status
sendSuperVectorObject(Any obj, int argc, Any *argv)
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
    ArgVector(av, argn);
    int i, n;
    
    for(i=0; i<args; i++)
      av[i] = argv[i];
    for(n=shift; n<valInt(v->size); n++)
      av[i++] = v->elements[n];

    if ( argn >= 1 )
      return sendSuperObject(obj, (Name) av[0], argn-1, av+1);
    fail;
  }

usage:
  return errorPce(obj, NAME_badVectorUsage);
}


static status
sendHyperObject(Any obj, Name hname, Name selector, int argc, Any *argv)
{ Chain ch;
  status rval = FAIL;

  if ( (ch = getAllHypersObject(obj, OFF)) )
  { Hyper h;
    
    for_chain(ch, h,
	      { if ( h->from == obj )
		{ if ( (hname == h->forward_name || isDefault(hname)) &&
		       sendv(h->to, selector, argc, argv) )
		    rval = SUCCEED;
		} else
		{ if ( (hname == h->backward_name || isDefault(hname)) &&
		       sendv(h->from, selector, argc, argv) )
		    rval = SUCCEED;
		}
	      });
  }

  return rval;
}


static Any
getHyperObject(Any obj, Name hname, Name selector, int argc, Any *argv)
{ Chain ch;

  if ( (ch = getAllHypersObject(obj, OFF)) )
  { Hyper h;
    Any rval;
    
    for_chain(ch, h,
	      { if ( h->from == obj )
		{ if ( (hname == h->forward_name || isDefault(hname)) &&
		       (rval = getv(h->to, selector, argc, argv)) )
		    answer(rval);
		} else
		{ if ( (hname == h->backward_name || isDefault(hname)) &&
		       (rval = getv(h->to, selector, argc, argv)) )
		    answer(rval);
		}
	      });
  }

  fail;
}


Any
getFindHyperObject(Any obj, Name hname, Code cond)
{ Chain ch;

  if ( (ch = getAllHypersObject(obj, OFF)) )
  { Cell cell;

    for_cell(cell, ch)
    { Hyper h = cell->value;

      if ( h->from == obj )
      { if ( (hname == h->forward_name || isDefault(hname)) &&
	     (isDefault(cond) || forwardCode(cond, h->from, h, h->to, 0)) )
	  answer(h);
      } else
      { if ( (hname == h->backward_name || isDefault(hname)) &&
	     (isDefault(cond) || forwardCode(cond, h->to, h, h->from, 0)) )
	  answer(h);
      }
    }
  }

  fail;
}


Any
getHyperedObject(Any obj, Name hname, Code cond)
{ Hyper h;

  if ( (h = getFindHyperObject(obj, hname, cond)) )
    answer(h->from == obj ? h->to : h->from);

  fail;
}


status
freeHypersObject(Any obj, Name hname, Code cond)
{ Chain ch;

  if ( (ch = getAllHypersObject(obj, OFF)) )
  { Hyper h;

    for_chain(ch, h,
	      { if ( ( ( h->from == obj &&
			 (hname == h->forward_name || isDefault(hname))
		       ) ||
		       ( h->to == obj &&
			 (hname == h->backward_name || isDefault(hname))
		       )
		     ) &&
		     ( isDefault(cond) ||
		       forwardCode(cond, h->to, h, h->from, 0)
		     ) )
		  freeObject(h);
	      });
  }

  succeed;
}


		/********************************
		*        SLOT ASSIGNMENT	*
		********************************/

inline void
addRefObject(Any from, Any to)
{ if ( inBoot || classOfObject(from)->un_answer == ON )
    deleteAnswerObject(to);

  addRefObj(to);

  if ( onFlag(to, F_INSPECT) )
  { addCodeReference(from);
    changedObject(to, NAME_addReference, from, 0);
    delCodeReference(from);
  }
}


inline void
delRefObject(Any from, Any to)
{ if ( refsObject(to) <= 0 )
  { if ( onFlag(to, F_CREATING|F_FREEING|F_FREED) )
      errorPce(PCE, NAME_negativeRefCountInCreate, to);
    else
      errorPce(PCE, NAME_negativeRefCount, to);

    return;				/* safer to leave it around! */
  }

  delRefObj(to);
  
  if ( isFreedObj(to) )
  { if ( refsObject(to) == 0 )
    { DEBUG(NAME_free, Cprintf("Doing deferred unalloc on %s\n", pp(to)));
      unallocObject(to);
      deferredUnalloced--;
    }
  } else if ( onFlag(to, F_INSPECT) )
  { addCodeReference(to);
    addCodeReference(from);
    changedObject(to, NAME_delReference, from, 0);
    delCodeReference(from);
    delCodeReference(to);
    freeableObj(to);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is responsible  for assignments to  an instance variable
(slot) of any object.  It is a wrapper arround  C's assignment to take
care of reference counts and the garbage collection issues.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


void
assignField(Instance instance, Any *field, Any value)
{ Any old;

  if ((old = *field) == value)		/* no change */
    return;

  if ( TraceMode &&
       (!onFlag(instance, F_CREATING) || TraceMode == TRACE_ALWAYS) )
  { int offset = field - &instance->slots[0];
    Class class = classOfObject(instance);
    Variable v = getElementVector(class->instance_variables, toInt(offset));

    if ( !v )
      goto nodebug;

    sendVariable(v, instance, 1, &value);
  } else
  { nodebug:

    *field = value;
    if ( isObject(value) && !isProtectedObj(value) )
      addRefObject(instance, value);
    if ( isObject(old) && !isProtectedObj(old) )
      delRefObject(instance, old);

    if ( onFlag(instance, F_INSPECT) )
      (*(classOfObject(instance))->changedFunction)(instance, field);
  }
}


		/********************************
		*        TRAPPING CHANGES	*
		********************************/

static inline status
_changedObject(Any obj, va_list args)
{ Class class = classOfObject(obj);

  if ( notNil(class->changed_messages) && !onFlag(obj, F_FREEING|F_CREATING) )
  { Cell cell;
    Any argv[VA_PCE_MAX_ARGS];
    int argc;

    if ( changedLevel )
    { errorPce(obj, NAME_changedLoop);
      succeed;
    }

    changedLevel++;

    argv[0] = obj;
    for(argc = 1; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
      ;
    for_cell(cell, class->changed_messages)
      forwardCodev(cell->value, argc, argv);

    changedLevel--;
  }
  
  succeed;
}


status
changedObject(Any obj, ...)
{ va_list args;
  status rval;

  if ( onFlag(obj, F_INSPECT) ) 
  { va_start(args, obj);
    rval = _changedObject(obj, args);
    va_end(args);

    return rval;
  } else
    succeed;  
}


status
changedFieldObject(Any obj, Any *field)
{ if ( onFlag(obj, F_INSPECT) )
  { Class class = classOfObject(obj);

    if ( notNil(class->changed_messages) &&
	 !onFlag(obj, F_CREATING|F_FREEING) )
    { Instance inst = obj;
      int offset = field - &inst->slots[0];
      Variable v = getInstanceVariableClass(class, (Any) toInt(offset));

      if ( v != FAIL )
      { Cell cell;

	if ( changedLevel )
	{ errorPce(obj, NAME_changedLoop);
	  succeed;
	}
	changedLevel++;
	for_cell(cell, class->changed_messages)
	  forwardCode(cell->value, obj, v->name, 0);
	changedLevel--;
      }
    }
  }  

  succeed;
}

		/********************************
		*           RESOURCES		*
		********************************/

Any
getResourceValueObject(Any obj, Name name)
{ if ( !isObject(obj) )
    fail;
  
  answer(getResourceValueClass(classOfObject(obj), name));
}


status
obtainResourcesObject(Any obj)
{ if ( offFlag(obj, F_RESOURCES_OBTAINED) )
  { Instance inst = obj;
    Class class = classOfObject(obj);
    int slots = valInt(class->slots);
    int i;
    status rval = SUCCEED;

    for(i=0; i<slots; i++)
    { if ( isDefault(inst->slots[i]) )
      { Variable var = class->instance_variables->elements[i];
	
	if ( !mayBeDefaultType(var->type) )
	{ Any value;

	  if ( (value = getResourceValueObject(obj, var->name)) )
	  { Any v2;

	    if ( (v2 = checkType(value, var->type, obj)) )
	      assignField(inst, &inst->slots[i], v2);
	    else
	    { errorPce(var, NAME_incompatibleResource);
	      rval = FAIL;
	    }
	  } else
	  { errorPce(var, NAME_noResource);
	    rval = FAIL;
	  }
	}
      }
    }

    setFlag(obj, F_RESOURCES_OBTAINED);
    return rval;
  }

  succeed;
}


		/********************************
		*         MISCELLANEOUS		*
		********************************/

status
convertLoadedObjectObject(Any obj, Int oldversion, Int currentversion)
{ succeed;
}


static status
initialiseNewSlotObject(Any obj, Variable var)
{ fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translates text of the form

  <blank>*@<blank>*<digit>+		integer reference
  <blank>*@<blank>*<alnum>+		atomic reference
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getConvertObject(Class class, Any x)
{ char *s;

  if ( (s = toCharp(x)) )
  { char *start;

    for( ; *s && isblank(*s); s++)	/* skip leading blanks */
      ;
    if ( s[0] != '@' )			/* verify starting '@' */
      fail;
    for( s++ ; *s && isblank(*s); s++)	/* skip blanks */
      ;
    start = s;

					/* check for @35435623 */
    for( ; isdigit(*s); s++ )
      ;
    if ( *s == EOS )
      answer(getObjectFromReferencePce(PCE, toInt(atol(start))));

					/* check for @name (exception?) */
    for( s=start; isalnum(*s); s++ )
      ;
    if ( *s == EOS )
      answer(getObjectAssoc(CtoKeyword(start)));
  }

  fail;
}


		/********************************
		*           CHECK		*
		********************************/

static int
checkExtensonsObject(Any obj, Bool recursive, HashTable done, int errs)
{ Any val;

#define CheckExt(att, get, attname) \
  { if ( onFlag(obj, att) ) \
    { if ( !(val = get(obj, OFF)) ) \
      { errorPce(obj, NAME_noExtension, attname); \
	errs++; \
      } \
      errs = check_object(val, recursive, done, errs); \
    } \
  }

  if ( onFlag(obj, F_CONSTRAINT|F_ATTRIBUTE|F_SENDMETHOD|F_GETMETHOD|
	           F_HYPER|F_RECOGNISER) )
  { CheckExt(F_CONSTRAINT, getAllConstraintsObject, NAME_allConstraints); 
    CheckExt(F_ATTRIBUTE,  getAllAttributesObject,  NAME_allAttributes);
    CheckExt(F_SENDMETHOD, getAllSendMethodsObject, NAME_allSendMethods);
    CheckExt(F_GETMETHOD,  getAllGetMethodsObject,  NAME_allGetMethods);
    CheckExt(F_HYPER,      getAllHypersObject,      NAME_allHypers);
    CheckExt(F_RECOGNISER, getAllRecognisersGraphical, NAME_allRecognisers);
  }
#undef CheckExtension

  return errs;
}


static int
check_object(Any obj, Bool recursive, HashTable done, int errs)
{ Instance inst = obj;
  Class class;
  int slots;
  int i;
  
  if ( recursive == ON )
  { if ( getMemberHashTable(done, obj) )
      return errs;
    appendHashTable(done, obj, NIL);
  }

  if ( !isProperObject(obj) )
  { errorPce(CtoName(pp(obj)), NAME_noProperObject);
    return errs + 1;
  }

  if ( isCreatingObj(obj) )
  { if ( instanceOfObject(obj, ClassClass) )	/* HACK: see typeClass() */
      return errs;

    errorPce(obj, NAME_creating);
    errs++;
  }

  DEBUG(NAME_codeReferences,
	if ( codeRefsObject(obj) != 0 )
	  writef("\t%s has %d code references\n",
		 obj,
		 toInt(codeRefsObject(obj))));

  class = classOfObject(inst);
  slots = valInt(class->slots);

#define Test(x) if ( isObject(x) ) \
		     (errs = check_object(x, recursive, done, errs))

  for(i=0; i<slots; i++)
  { if ( isPceSlot(class, i) )
    { Variable var = getInstanceVariableClass(class, toInt(i));
      Any value = inst->slots[i];

      if ( var == FAIL )
      { errorPce(obj, NAME_noVariable, toInt(i));
      	continue;
      }

      if ( isDefault(value) &&
	   (getResourceClass(class, var->name) ||
	    (instanceOfObject(obj, ClassClass) &&
	     ((Class)obj)->realised != ON)) )
	continue;

      if ( !validateType(var->type, value, obj) )
      { errorPce(obj, NAME_badSlotValue, var, value);
	errs++;
      } else if ( isObject(value) )
      { if ( isFreedObj(value) )
	{ errorPce(obj, NAME_freedSlotValue, var, CtoName(pp(value)));
	  errs++;
	} else if ( recursive == ON && isObject(value) )
	{ if ( !isProperObject(value) )
	  { errorPce(obj, NAME_badSlotValue, var, CtoName(pp(value)));
	    errs++;
	  } else
	    Test(value);
	}
      }
    }
  }

  errs = checkExtensonsObject(obj, recursive, done, errs);

  if ( instanceOfObject(obj, ClassChain) )
  { Cell cell;
    int i = 1;

    for_cell(cell, (Chain) obj)
    { if ( isObject(cell->value) )
      { if ( isFreedObj(cell->value) )
	{ errorPce(obj, NAME_freedCellValue,
		   toInt(i), CtoName(pp(cell->value)));
	  errs++;
	} else if ( recursive == ON && isObject(call->value) )
	{ if ( !isProperObject(cell->value) )
	  { errorPce(obj, NAME_badCellCalue,
		     toInt(i), CtoName(pp(cell->value)));
	    errs++;
	  } else
	    Test(cell->value);
	}
      }
      i++;
    }
  } else if ( instanceOfObject(obj, ClassVector) )
  { for_vector((Vector) obj, Any value,
	       if ( isObject(value) )
	       { if ( isFreedObj(value) )
		 { errorPce(obj, NAME_freedElementValue,
			    toInt(_iv), CtoName(pp(value)));
		   errs++;
		 } else if ( recursive == ON && isObject(value) )
		 { if ( !isProperObject(value) )
		   { errorPce(obj, NAME_badElementValue,
			      toInt(_iv), CtoName(pp(value)));
		     errs++;
		   } else
		     Test(value);
		 }
	       });
  } else if ( instanceOfObject(obj, ClassHashTable) )
  { HashTable ht = (HashTable)obj;

    if ( (valInt(ht->size) * 4) > 3 * ht->buckets)
    { errorPce(ht, NAME_tooFewBuckets, ht->size, ht->buckets);
      errs++;
    }

    for_hash_table(ht, s,
		   { if ( isObject(s->name) )
		     { if ( isFreedObj(s->name) )
		       { errorPce(ht, NAME_freedKeyValue,
				  CtoName(pp(s->name)), s->value);
			 errs++;
		       } else if ( recursive == ON && isObject(s->name) )
		       { if ( !isProperObject(s->name) )
			 { errorPce(ht, NAME_badKeyValue,
				    CtoName(pp(s->name)), s->value);
			   errs++;
			 } else
			   Test(s->name);
		       }
		     }
		     if ( isObject(s->value) )
		     { if ( isFreedObj(s->value) )
		       { errorPce(ht, NAME_freedValueValue,
				  s->name, CtoName(pp(s->value)));
			 errs++;
		       } else if ( recursive == ON && isObject(s->value) )
		       { if ( !isProperObject(s->value) )
			 { errorPce(ht, NAME_badValueValue,
				    s->name, CtoName(pp(s->value)));
			   errs++;
			 } else
			   Test(s->value);
		       }
		     }
		   });
  }
#undef Test

  return errs;
}


status
CheckObject(Any obj, Bool recursive)
{ HashTable done = NIL;
  int errs;

  if ( isDefault(recursive) ) 
    recursive = ON;
  
  if ( recursive == ON )
  { checkNames(TRUE);
    done = createHashTable(toInt(200), OFF);
  }

  errs = check_object(obj, recursive, done, 0);

  if ( notNil(done) )
  { errorPce(obj, NAME_checkedObjects, done->size);
    freeHashTable(done);
  }

  return errs ? FAIL : SUCCEED;
}


static status
for_slot_reference_object(Any obj, Code msg, Bool recursive, HashTable done)
{ Instance inst = obj;
  Class class;
  int slots;
  int i;
  
  if ( !isProperObject(obj) )
  { errorPce(CtoName(pp(obj)), NAME_noProperObject);
    fail;
  }

  class = classOfObject(inst);
  slots = valInt(class->slots);

  if ( recursive == ON )
  { if ( getMemberHashTable(done, obj) != FAIL )
      succeed;
    appendHashTable(done, obj, NIL);
  }

  for(i=0; i<slots; i++)
  { if ( isPceSlot(class, i) )
    { Variable var = getInstanceVariableClass(class, (Any) toInt(i));
      Any value = inst->slots[i];

      if ( var == FAIL )
      { errorPce(obj, NAME_noVariable, toInt(i));
      	continue;
      }

      if ( isDefault(value) && getResourceClass(class, var->name) )
	value = getGetVariable(var, inst, 0, NULL);

      forwardCode(msg, inst, NAME_slot, var->name, value, 0);
      if ( recursive == ON && isObject(value) )
	for_slot_reference_object(value, msg, recursive, done);
    }
  }

  if ( instanceOfObject(obj, ClassChain) )
  { Cell cell;
    int n = 1;

    for_cell(cell, (Chain) obj)
    { forwardCode(msg, obj, NAME_cell, toInt(n), cell->value, 0);

      if ( recursive == ON && isObject(cell->value) )
	for_slot_reference_object(cell->value, msg, recursive, done);
      n++;
    }
  } else if ( instanceOfObject(obj, ClassVector) )
  { for_vector((Vector) obj, Any value,
	       forwardCode(msg, NAME_element, obj, toInt(_iv), value, 0);
	       if ( recursive == ON && isObject(value) )
		 for_slot_reference_object(value, msg, recursive, done););
  } else if ( instanceOfObject(obj, ClassHashTable) )
  { for_hash_table((HashTable) obj, s,
		   { forwardCode(msg, obj, NAME_key, s->name, s->value, 0);

		     if ( recursive == ON )
		     { if ( isObject(s->name) )
			 for_slot_reference_object(s->name, msg,
						   recursive, done);
		       if ( isObject(s->value) )
			 for_slot_reference_object(s->value, msg,
						   recursive, done);
		     }
		   });
  }

  succeed;
}


static status
forSlotReferenceObject(Any obj, Code msg, Bool recursive)
{ HashTable done = NULL;

  if ( isDefault(recursive) ) 
    recursive = ON;
  if ( recursive == ON )
    done = createHashTable(toInt(200), OFF);

  for_slot_reference_object(obj, msg, recursive, done);

  if ( notNil(done) )
    freeHashTable(done);

  succeed;
}


		/********************************
		*       ERRORS/FEEDBACK		*
		********************************/

status
errorObjectv(Any obj, Error e, int argc, Any *argv)
{ if ( e->kind == NAME_ignored )
    fail;

  assign(PCE, last_error, e->id);

  if ( !catchedErrorPce(PCE, e->id) || e->kind == NAME_fatal )
  { ArgVector(av, argc+1);
    int i;

    if ( e->kind == NAME_error || e->kind == NAME_fatal )
    { Goal g = CurrentGoal;
      int i;

      for(i=0; i<2 && g; i++)
	g = g->parent;
      if ( g )
	setGFlag(g, G_EXCEPTION);
    }

    av[0] = obj;
    for(i=0; i<argc; i++)
      av[i+1] = argv[i];
  
    sendv(e, NAME_display, argc+1, av);
  }

  fail;
}


static Any
getReportToObject(Any obj)
{ if ( notNil(EVENT->value) )		/* associate to @event?receiver */
    answer(getReceiverEvent(EVENT->value));
  
  fail;
}


static status
reportObject(Any obj, Name kind, CharArray fmt, int argc, Any *argv)
{ Any to;

  if ( (to = get(obj, NAME_reportTo, 0)) )
  { ArgVector(av, argc + 2);

    av[0] = kind;
    av[1] = fmt;
    copyArgs(argc, argv, &av[2]);

    return sendv(to, NAME_report, argc+2, av);
  } else				/* no event: print it */
  { char buf[FORMATSIZE];
    Any av[2];

    if ( isDefault(fmt) )
      fmt = (CharArray) (kind == NAME_done ? NAME_done : CtoName(""));
    TRY(swritefv(buf, fmt, argc, argv));
    av[0] = kind;
    av[1] = CtoTempString(buf);
    formatPcev(PCE,
	       (CharArray) CtoName(kind == NAME_progress ? "[PCE: %I%s ... " :
				   kind == NAME_done     ? "%I%s]\n" :
				   			   "[PCE: %s: %s]\n"),
	       2, av);
    if ( kind == NAME_progress )
      hostAction(HOST_FLUSH);
    considerPreserveObject(av[1]);

    succeed;
  }
}


		/********************************
		*        MANUAL SUPPORT		*
		********************************/

Name
getManIdObject(Any obj)
{ Name name;

  if ( isName(name = getObjectReferenceObject(obj)) )
  { char buf[LINESIZE];

    sprintf(buf, "O.%s", strName(name));
    answer(CtoName(buf));
  }

  fail;
}


static Name
getManIndicatorObject(Any obj)
{ answer(CtoName("O"));
}


static CharArray
getPrintNameObject(Any obj)
{ CharArray name;

  if ( hasGetMethodObject(obj, NAME_name) &&
       (name = get(obj, NAME_name, 0)) &&
       (name = checkType(name, TypeCharArray, NIL)) )
    answer(name);
  else
    answer((CharArray) CtoString(pp(obj)));
}


status
makeClassObject(Class class)
{ sourceClass(class, makeClassObject, __FILE__, "$Revision$");

  termClass(class, "object", 0);
  setChangedFunctionClass(class, changedFieldObject);

  sendMethod(class, NAME_initialise, NAME_oms, 1, "unchecked ...",
	     "Initialise variables and resources",
	     initialiseObject);
  sendMethod(class, NAME_attribute, NAME_storage, 2,
	     "attribute|name", "value=[any]",
	     "Append/change object-level attribute",
	     attributeObject);
  sendMethod(class, NAME_deleteAttribute, NAME_storage, 1, "name|attribute",
	     "Delete object-level attribute",
	     deleteAttributeObject);
  sendMethod(class, NAME_sendMethod, NAME_programming, 1, "send_method|chain",
	     "Add an object-level send_method",
	     sendMethodObject);
  sendMethod(class, NAME_getMethod, NAME_programming, 1, "get_method|chain",
	     "Add an object-level get_method",
	     getMethodObject);
  sendMethod(class, NAME_attachHyper, NAME_relation, 2, "hyper", "object",
	     "Attach a hyper to an object",
	     attachHyperObject);
  sendMethod(class, NAME_deleteHyper, NAME_relation, 1, "hyper",
	     "Detach a hyper from an object",
	     deleteHyperObject);
  sendMethod(class, NAME_deleteHypers, NAME_relation, 2,
	     "name=[name]", "condition=[code]",
	     "Delete all matching hypers",
	     freeHypersObject);
  sendMethod(class, NAME_free, NAME_oms, 0,
	     "Delete object from the object-base",
	     freeObject);
  sendMethod(class, NAME_done, NAME_oms, 0,
	     "Indicate I'm done with some answer",
	     doneObject);
  sendMethod(class, NAME_unlink, NAME_oms, 0,
	     "Unlink from environment",
	     succeedObject);
  sendMethod(class, NAME_unlinking, NAME_oms, 0,
	     "Try if ->unlink is in progress",
	     unlinkingObject);
  sendMethod(class, NAME_lockObject, NAME_oms, 1, "bool",
	     "Lock object for incremental garbage collection",
	     lockObject);
  sendMethod(class, NAME_protect, NAME_oms, 0,
	     "Lock object for destruction with ->free",
	     protectObject);
#ifndef O_RUNTIME
  sendMethod(class, NAME_inspect, NAME_debugging, 1, "bool",
	     "Forward changes via classes' changed_messages",
	     inspectObject);
#endif
  sendMethod(class, NAME_nameReference, NAME_reference, 1, "name*",
	     "Change named (atomic) reference",
	     nameReferenceObject);
  sendMethod(class, NAME_sameClass, NAME_type, 1, "object",
	     "Is object of the same class as argument",
	     sameClassObject);
  sendMethod(class, NAME_saveInFile, NAME_file, 1, "file",
	     "Save object and it's context in a file",
	     saveInFileObject);
  sendMethod(class, NAME_isOn, NAME_test, 1, "name",
	     "Test if Obj <-name returns @on",
	     isOnObject);
  sendMethod(class, NAME_isOff, NAME_test, 1, "name",
	     "Test if Obj <-name returns @off",
	     isOffObject);
  sendMethod(class, NAME_instanceOf, NAME_type, 1, "class",
	     "Test of object is an intance of class",
	     instanceOfObject);
  sendMethod(class, NAME_hasValue, NAME_test, 2, "name", "any",
	     "Test if Obj <-name equals 2nd argument",
	     hasValueObject);
  sendMethod(class, NAME_notHasValue, NAME_test, 2, "name", "any",
	     "Test if Obj <-name not-equal 2nd argument",
	     notHasValueObject);
  sendMethod(class, NAME_slot, NAME_programming, 2, "name|int", "unchecked",
	     "Set value of an instance variable",
	     slotObject);
  sendMethod(class, NAME_sendHyper, NAME_relation, 3,
	     "hyper_name=[name]", "selector=name", "argument=unchecked ...",
	     "Send message using named hypers",
	     sendHyperObject);
  sendMethod(class, NAME_sendSub, NAME_programming, 2,
	     "selector=name", "argument=unchecked ...",
	     "Send using method of subclass",
	     sendSubObject);
  sendMethod(class, NAME_sendSuper, NAME_programming, 2,
	     "selector=name", "argument=unchecked ...",
	     "Send using method of super-class",
	     sendSuperObject);
  sendMethod(class, NAME_sendClass, NAME_programming, 2,
	     "selector=name", "argument=unchecked ...",
	     "Send using method of class of object",
	     sendClassObject);
  sendMethod(class, NAME_sendVector, NAME_programming, 1, "unchecked ...",
	     "Varargs: any ..., vector, [int]",
	     sendVectorObject);
  sendMethod(class, NAME_sendSuperVector, NAME_programming, 1, "unchecked ...",
	     "Varargs: any ..., vector, [int]",
	     sendSuperVectorObject);
  sendMethod(class, NAME_error, NAME_report, 2,
	     "error=error", "context=any ...",
	     "Initiate an error: id, context ...",
	     errorObjectv);
  sendMethod(class, NAME_report, NAME_report, 3,
	     "kind={status,inform,progress,done,warning,error}",
	     "format=[char_array]", "argument=any ...",
	     "Report message (send to @event <-receiver)",
	     reportObject);
#ifndef O_RUNTIME
  sendMethod(class, NAME_Check, NAME_debugging, 1, "recursive=[bool]",
	     "Check types for all instance-variables of object",
	     CheckObject);
#endif
  sendMethod(class, NAME_obtainResources, NAME_resource, 0,
	     "Obtain resources for @default-valued slots",
	     obtainResourcesObject);
  sendMethod(class, NAME_convertLoadedObject, NAME_file, 2,
	     "old_version=int", "current_version=int",
	     "Called by File <-object if conversion might be required",
	     convertLoadedObjectObject);
  sendMethod(class, NAME_initialiseNewSlot, NAME_file, 1, "new=variable",
	     "Called by File <-object if a new slot is found",
	     initialiseNewSlotObject);
  sendMethod(class, NAME_forSlotReference, NAME_debugging, 2,
	     "action=code", "recursive=[bool]",
	     "Run code on object-slot-value references",
	     forSlotReferenceObject);
  sendMethod(class, NAME_equal, NAME_compare, 1, "to=any",
	     "Test if i'm equal to the argument",
	     equalObject);
  sendMethod(class, NAME_sameReference, NAME_compare, 1, "to=any",
	     "Test if i'm the same object as the argument",
	     sameReferenceObject);
  sendMethod(class, NAME_hasSendMethod, NAME_meta, 1, "selector=name",
	     "Test if object defines send_method",
	     hasSendMethodObject);
  sendMethod(class, NAME_hasGetMethod, NAME_meta, 1, "selector=name",
	     "Test if object defines get_method",
	     hasGetMethodObject);
  
  getMethod(class, NAME_class, NAME_type, "class", 0,
	    "Class the object belongs to",
	    getClassObject);
  getMethod(class, NAME_className, NAME_type, "name", 0,
	    "Name of the class the object belongs to",
	    getClassNameObject);
  getMethod(class, NAME_clone, NAME_copy, "object", 0,
	    "New object that is a (recursive) copy)",
	    getCloneObject);
  getMethod(class, NAME_lockObject, NAME_oms, "bool", 0,
	    "Boolean to indicate locked for GC",
	    getLockObject);
  getMethod(class, NAME_protect, NAME_oms, "bool", 0,
	    "Boolean to indicate locked for ->free",
	    getProtectObject);
#ifndef O_RUNTIME
  getMethod(class, NAME_inspect, NAME_debugging, "bool", 0,
	    "Boolean to indicate changes forwarding",
	    getInspectObject);
#endif
  getMethod(class, NAME_objectReference, NAME_reference, "name|int", 0,
	    "Name of the object (e.g. @pce)",
	    getObjectReferenceObject);
  getMethod(class, NAME_references, NAME_debugging, "int", 0,
	    "Number of references to this object",
	    getReferencesObject);
  getMethod(class, NAME_codeReferences, NAME_debugging, "int", 0,
	    "Number of code-references to this object",
	    getCodeReferencesObject);
  getMethod(class, NAME_self, NAME_oms, "object", 0,
	    "Returns itself",
	    getSelfObject);
  getMethod(class, NAME_Arity, NAME_term, "int", 0,
	    "Number of arguments of term description",
	    getArityObject);
  getMethod(class, NAME_functor, NAME_term, "name", 0,
	    "Functor (name) of term description",
	    getFunctorObject);
  getMethod(class, NAME_Arg, NAME_term, "unchecked", 1, "int",
	    "Nth-1 argument of term description",
	    getArgObject);
  getMethod(class, NAME_Flags, NAME_debugging, "name", 0,
	    "Name width {P, L and A} flags",
	    getFlagsObject);
  getMethod(class, NAME_slot, NAME_programming, "unchecked", 1, "name|int",
	    "Get value of a slot",
	    getSlotObject);
  getMethod(class, NAME_attribute, NAME_storage, "unchecked", 1, "name",
	    "Get value of a object-level attribute",
	    getAttributeObject);
  getMethod(class, NAME_getSub, NAME_programming, "unchecked", 2,
	    "selector=name", "argument=unchecked ...",
	    "Get, using method of sub-class",
	    getGetSubObject);
  getMethod(class, NAME_getSuper, NAME_programming, "unchecked", 2,
	    "selector=name", "argument=unchecked ...",
	    "Get, using method of super-class",
	    getGetSuperObject);
  getMethod(class, NAME_getClass, NAME_programming, "unchecked", 2,
	    "selector=name", "argument=unchecked ...",
	    "Get, using method of class of object",
	    getGetClassObject);
  getMethod(class, NAME_getVector, NAME_programming, "unchecked", 1,
	    "unchecked ...",
	    "Varargs: any ..., vector, [int]",
	    getVectorObject);
  getMethod(class, NAME_getHyper, NAME_relation, "unchecked", 3,
	    "hyper_name=[name]", "selector=name", "argument=unchecked ...",
	     "Get-operation using named hypers",
	     getHyperObject);

  getMethod(class, NAME_allConstraints, NAME_meta, "chain", 1,
	    "create=[bool]",
	    "Chain with all constraints",
	    getAllConstraintsObject);
  getMethod(class, NAME_allHypers, NAME_meta, "chain", 1,
	    "create=[bool]",
	    "Chain with all hypers",
	    getAllHypersObject);
  getMethod(class, NAME_allAttributes, NAME_meta, "chain", 1,
	    "create=[bool]",
	    "Chain with object-level attributes",
	    getAllAttributesObject);
  getMethod(class, NAME_allSendMethods, NAME_meta, "chain", 1,
	    "create=[bool]",
	    "Chain with all send methods",
	    getAllSendMethodsObject);
  getMethod(class, NAME_allGetMethods, NAME_meta, "chain", 1,
	    "create=[bool]",
	    "Chain with all get methods",
	    getAllGetMethodsObject);
  getMethod(class, NAME_printName, NAME_textual, "text=char_array", 0,
	    "Calls <-name",
	    getPrintNameObject);

  sendMethod(class, NAME_Free, NAME_function, 0,
	     "Equivalent to ->free",
	     freeObject);
#ifndef O_RUNTIME
  sendMethod(class, NAME_Inspect, NAME_function, 1, "bool",
	     "Equivalent to ->inspect",
	     inspectObject);
#endif
  sendMethod(class, NAME_InstanceOf, NAME_function, 1, "class",
	     "Equivalent to ->instance_of",
	     instanceOfObject);
  sendMethod(class, NAME_SameReference, NAME_function, 1, "to=any|function",
	     "Equivalent to ->same_reference",
	     sameReferenceObject);

  getMethod(class, NAME_ClassName, NAME_function, "name", 0,
	    "Equivalent to <-class_name",
	    getClassNameObject);
  getMethod(class, NAME_Class, NAME_function, "class", 0,
	    "Equivalent to <-class",
	    getClassObject);
  getMethod(class, NAME_Slot, NAME_function, "unchecked", 1, "name|int",
	    "Equivalent to <-slot",
	    getSlotObject);
  getMethod(class, NAME_References, NAME_function, "int", 0,
	    "Equivalent to <-references",
	    getReferencesObject);

#ifndef O_RUNTIME
  getMethod(class, NAME_Inspect, NAME_function, "bool", 0,
	    "Equivalent to <-inspect",
	    getInspectObject);
  getMethod(class, NAME_ManId, NAME_function, "name", 0,
	    "Equivalent to <-man_id",
	    getManIdObject);
  getMethod(class, NAME_manId, NAME_manual, "name", 0,
	    "Card Id for global object",
	    getManIdObject);
  getMethod(class, NAME_manIndicator, NAME_manual, "name", 0,
	    "Manual type indicator (`O')",
	    getManIndicatorObject);
#endif /*O_RUNTIME*/

  getMethod(class, NAME_resourceValue, NAME_resource, "any", 1, "name",
	    "Get value of associated resource",
	    getResourceValueObject);
  getMethod(class, NAME_sendMethod, NAME_meta, "tuple",
	    1, "name",
	    "Tuple containing receiver and implementing object",
	    getSendMethodObject);
  getMethod(class, NAME_getMethod, NAME_meta, "tuple",
	    1, "name",
	    "Tuple containing receiver and implementing object",
	    getGetMethodObject);
  getMethod(class, NAME_findAllSendMethods, NAME_meta, "chain", 1,
	    "condition=[code]",
	    "New chain with all send-methods satisfying code",
	    getFindAllSendMethodsObject);
  getMethod(class, NAME_reportTo, NAME_report, "object", 0,
	    "Object for ->report (@event <-receiver)",
	    getReportToObject);
  getMethod(class, NAME_hypered, NAME_relation, "object", 2,
	    "hyper_name=[name]", "test=[code]",
	    "Find hyper-related object",
	    getHyperedObject);
  getMethod(class, NAME_findHyper, NAME_relation, "hyper", 2,
	    "hyper_name=[name]", "test=[code]",
	    "Find hyper-relation object",
	    getFindHyperObject);
  getMethod(class, NAME_convert, NAME_oms, "object", 1, "char_array",
	    "Convert '@reference' into object",
	    getConvertObject);

  succeed;
}


