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

#include <h/kernel.h>
#include <h/trace.h>
#include <h/graphics.h>
#include <h/interface.h>
#include <rel/proto.h>

static int	check_object(Any, Bool, HashTable, int);
static status	makeTempObject(Any obj);

		/********************************
		*        SLOT ASSIGNMENT	*
		********************************/

static inline void
unallocObject(Any obj)
{ unalloc(valInt(classOfObject(obj)->instance_size), obj);
}


inline void
addRefObject(Any from, Any to)
{ if ( inBoot || classOfObject(from)->un_answer == ON )
    deleteAnswerObject(to);

  addRefObj(to);

  if ( onFlag(to, F_INSPECT) )
  { addCodeReference(from);
    changedObject(to, NAME_addReference, from, EAV);
    delCodeReference(from);
  }
}


inline void
delRefObject(Any from, Any to)
{ delRefObj(to);

  if ( onFlag(to, F_INSPECT) )
  { addCodeReference(to);
    addCodeReference(from);
    changedObject(to, NAME_delReference, from, EAV);
    delCodeReference(from);
    delCodeReference(to);
  }

  if ( refsObject(to) <= 0 )
  { if ( refsObject(to) == 0 )
    { if ( isFreedObj(to) )
      { DEBUG(NAME_free, Cprintf("Doing deferred unalloc on %s\n", pp(to)));
	unallocObject(to);
	deferredUnalloced--;
      } else
      { freeableObj(to);
      }
    } else
    { if ( onFlag(to, F_CREATING|F_FREEING|F_FREED) )
	errorPce(PCE, NAME_negativeRefCountInCreate, to);
      else
	errorPce(PCE, NAME_negativeRefCount, to);
    }
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

  if ( PCEdebugging && !onFlag(instance, F_CREATING|F_FREEING) )
  { int offset = field - &instance->slots[0];
    Class class = classOfObject(instance);
    Variable v = getElementVector(class->instance_variables, toInt(offset));

    if ( v && DebuggingProgramObject(v, D_TRACE) )
      writef("V %O ->%s: %O --> %O\n", instance, v->name, old, value);
  }

  *field = value;
  if ( isObject(value) && !isProtectedObj(value) )
    addRefObject(instance, value);
  if ( isObject(old) && !isProtectedObj(old) )
    delRefObject(instance, old);

  if ( onFlag(instance, F_INSPECT) )
    (*(classOfObject(instance))->changedFunction)(instance, field);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  CREATING OBJECTS	

PCE objects are created by  C-functions.  Some   objects  are  used as a
temporary argument to a  method   (e.g.  send(@box, move, point(30,30)).
Others are created as an `end-point' object (e.g. new(@box, box(50,50)))
and yet others are  stored  as  an   attribute  to  other  objects (e.g.
send(@sheet, size, size(50,50))).
 
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

#undef offset
#define offset(t, f) ((int)(&((struct t *)0)->f))

static status
hasClassVariableVariable(Variable v, Class class)
{ for( ; notNil(class); class=class->super_class )
  { Cell cell;

    for_cell(cell, class->class_variables)
    { ClassVariable cv = cell->value;

      if ( cv->name == v->name )
	succeed;
    }
  }
  
  fail;
}



static void
updateInstanceProtoClass(Class class)
{ int slots = valInt(class->slots);
  int size = valInt(class->instance_size);
  Variable *var = (Variable *) &class->instance_variables->elements[0];
  Any *field;
  Instance obj;

  class->proto = alloc(offset(instance_proto, proto) + size);
  class->proto->size = size;
  obj = (Instance) &class->proto->proto;
  initHeaderObj(obj, class);

  field = &obj->slots[0];
  for( ; --slots >= 0; var++, field++)
  { Variable v = *var;

    if ( isNil(v->alloc_value) &&
	 hasClassVariableVariable(v, class) )
    { *field = CLASSDEFAULT;
      setFlag(obj, F_OBTAIN_CLASSVARS);
      DEBUG(NAME_classVariable,
	    Cprintf("Set %s-%s to @class_default\n",
		    pp(class->name), pp(v->name)));
    } else
      *field = v->alloc_value;
  }
}


void
unallocInstanceProtoClass(Class class)
{ if ( class->proto )
  { unalloc(offset(instance_proto, proto) + class->proto->size, class->proto);
    class->proto = NULL;
  }
}


Any
allocObject(Class class, int funcs)
{ Instance obj;
  int size;

again:
  if ( class->proto )
  { size = class->proto->size;
    obj = alloc(size);
    cpdata((Any)obj, (Any)&class->proto->proto, Any, size/sizeof(Any));

    return obj;
  }

  if ( class->boot )
  { int size = valInt(class->instance_size);
    int slots = (size - offset(instance, slots[0])) / sizeof(Any);
    int i;

    obj = alloc(size);
    initHeaderObj(obj, class);

    for (i = 0; i < slots; i++)
      obj->slots[i] = ((i < class->boot) ? NIL : (Any) NULL);

    return obj;
  } else
  { updateInstanceProtoClass(class);
    goto again;
  }
}


static inline status
init_slots(Instance obj, int slots, Variable *var, Any *field)
{ for( ; --slots >= 0; var++, field++)
  { Any value;
    Function f = (*var)->init_function;

    if ( notNil(f) )
    { if ( !(value = expandCodeArgument(f)) ||
	   !sendVariable(*var, obj, value) ) 		/* assignField? */
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


Any
createObjectv(Name assoc, Class class, int argc, const Any argv[])
{ Any rval;

					/* Resolve the class (caller?) */
  if ( !instanceOfObject(class, ClassClass) )
  { Class c2;

    if ( (c2 = getMemberHashTable(classTable, class)) ||
	 (c2 = checkType(class, TypeClass, NIL)) )
    { class = c2;
    } else
    { errorPce(class, NAME_noClass);
      fail;
    }
  }
					/* Prepare the class */
  if ( class->realised != ON )
    realiseClass(class);
  if ( isDefault(class->lookup_method) ||
       isDefault(class->initialise_method) )
    bindNewMethodsClass(class);


					/* Try lookup of existing object */
  if ( notNil(class->lookup_method) )
  { if ( (rval = getGetGetMethod(class->lookup_method,
				 class, argc, argv)) )
      answer(rval);
  }

					/* Check assoc redefinition */
  if ( notNil(assoc) )
  { if ( getObjectAssoc(assoc) )
      exceptionPce(PCE, NAME_redefinedAssoc, assoc, EAV);
    if ( getObjectAssoc(assoc) )
    { errorPce(PCE, NAME_redefinedAssoc, assoc, 0);
      fail;
    }
  }

					/* Allocate the object */
  rval = allocObject(class, TRUE);
  addCodeReference(rval);		/* avoid drop-out */
  if ( notNil(assoc) )			/* Create name association */
    newAssoc(assoc, rval);

					/* Initialise the object */
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
    fail;
  }

  delCodeReference(rval);
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
answerObjectv(Class class, int argc, const Any *argv)
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


static inline void
unlinkHypersObject(Any obj)
{ if ( onFlag(obj, F_HYPER) )
  { Chain ch = getAllHypersObject(obj, ON);
    Hyper h;

    clearFlag(obj, F_HYPER);
    for_chain(ch, h,
	      { if ( !onFlag(h, F_FREED|F_FREEING) )
		{ if ( h->from == obj )
		    sendv(h, NAME_unlinkFrom, 0, NULL);
		  else
		    sendv(h, NAME_unlinkTo, 0, NULL);

		  if ( !isFreedObj(h) )	/* ensure it has gone! */
		    freeObject(h);
		}
	      });
    deleteHashTable(ObjectHyperTable, obj);
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unlinkObject()

Disconnect the object from its environment.   The  first loop resets all
instance-variables to NIL that  do  not   contain  integers  of reusable
objects. This process could be  optimised   a  little  further by closer
examination of the variable properties  of   the  class  and adding this
information (for example) to the prototype used in createObject().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
unlinkObject(Any obj)
{ Instance  inst  = obj;
  Class     class = classOfObject(obj);
  Variable *var   = (Variable *)class->instance_variables->elements;
  Any      *field = inst->slots;
  int i;

  for(i=valInt(class->slots); --i >= 0; var++, field++)
  { if ( var[0]->type->kind != NAME_alien )
    { if ( isObject(*field) && !isProtectedObj(*field) )
      { Any old = *field;

	*field = NIL;
	delRefObject(inst, old);
      }
    }
  }

  if ( onFlag(obj, F_ATTRIBUTE|F_CONSTRAINT|F_SENDMETHOD|
	           F_GETMETHOD|F_RECOGNISER) )
  { if ( onFlag(obj, F_CONSTRAINT) )
    { Chain ch = getAllConstraintsObject(obj, ON);
      Constraint c;

      clearFlag(obj, F_CONSTRAINT);
      for_chain(ch, c, freeObject(c));
      deleteHashTable(ObjectConstraintTable, obj);
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
}


status
freeObject(Any obj)
{ Instance inst = obj;
  Class class;

  if ( nonObject(inst) || onFlag(inst, F_FREED|F_FREEING) )
    succeed;
  if ( isProtectedObj(inst) )		/* cannot be freed */
    fail;

  class = classOfObject(inst);
  freedClass(class, inst);

  unlockObj(inst);			/* release possible lock */
  deleteAnswerObject(inst);		/* delete from AnswerStack */
  setFreeingObj(inst);			/* mark */

  if ( !qadSendv(inst, NAME_unlink, 0, NULL) )
    errorPce(inst, NAME_unlinkFailed);

  if ( onFlag(obj, F_ASSOC) )
    deleteAssoc(inst);			/* delete name association */

  unlinkHypersObject(inst);

  unlinkObject(inst);
  setFreedObj(inst);			/* freeing finished */

  if ( refsObject(inst) == 0 )
    unallocObject(inst);
  else
  { deferredUnalloced++;
    DEBUG(NAME_free, Cprintf("%s has %ld refs.  Deferring unalloc\n",
			     pp(inst), refsObject(inst)));
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


status
virtualObject1(Any obj, Any a1)
{ fail;
}


status
virtualObject2(Any obj, Any a1, Any a2)
{ fail;
}


Any
getVirtualObject(Any obj)
{ fail;
}


Any
getVirtualObject1(Any obj, Any a1)
{ fail;
}


Any
getVirtualObject2(Any obj, Any a1, Any a2)
{ fail;
}


Int
getReferencesObject(Any obj)
{ answer(toInt(refsObject(obj)));
}


static Int
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
    freeableObj(obj);
  }
  succeed;
}


static Any
getUnlockObject(Any obj)
{ unlockObj(obj);
  pushAnswerObject(obj);

  answer(obj);
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
    exceptionPce(PCE, NAME_redeclaredReference, name, EAV);
  if ( (old = getObjectAssoc(name)) )
    errorPce(obj, NAME_redeclaredReference, name);

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


static status
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

    return appendChain(ch, newObject(ClassAttribute, name, value, EAV));
  }
}


status
deleteAttributeObject(Any obj, Any att)
{ Chain ch;
  status rval = FAIL;

  TRY(ch = getAllAttributesObject(obj, OFF));

  if ( instanceOfObject(att, ClassAttribute) )
    rval = deleteChain(ch, att);
  else
  { Cell cell;

    for_cell(cell, ch)
    { Attribute a = cell->value;

      if ( a->name == att )
      { rval = deleteChain(ch, a);
	break;
      }
    }
  }

  if ( rval && emptyChain(ch) )
  { deleteHashTable(ObjectAttributeTable, obj);
    clearFlag(obj, F_ATTRIBUTE);
  }

  return rval;
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

		 /*******************************
		 *          CONSTRAINTS		*
		 *******************************/

status
updateConstraintsObject(Any obj)
{ if ( onFlag(obj, F_CONSTRAINT) && !isFreedObj(obj) )
  { Chain constraints = getAllConstraintsObject(obj, ON);
    Cell cell;

    DEBUG(NAME_constraint,
	  Cprintf("Called %s->update_constraints\n", pp(obj)));

    for_cell(cell, constraints)
      lockConstraint(cell->value, obj);

    for_cell(cell, constraints)
      executeConstraint(cell->value, obj);

    for_cell(cell, constraints)
      unlockConstraint(cell->value, obj);
  }

  succeed;
}


		/********************************
		*       RESOLVING METHODS	*
		********************************/

Tuple
getSendMethodObject(Any obj, Name selector)
{ Any m, rec;

  TRY( m = resolveSendMethodObject(obj, NULL, selector, &rec) );

  answer(answerObject(ClassTuple, rec, m, EAV));
}


Tuple
getGetMethodObject(Any obj, Name selector)
{ Any m, rec;

  TRY( m = resolveGetMethodObject(obj, NULL, selector, &rec) );

  answer(answerObject(ClassTuple, rec, m, EAV));
}


status
hasSendMethodObject(Any obj, Name selector)
{ Any m, rec;

  TRY(m = resolveSendMethodObject(obj, NULL, selector, &rec));
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

    mergeMethods(ch, getSendMethodsClass(class), done, cond);
    for_vector(class->instance_variables, var,
	       if ( sendAccessVariable(var) )
	         mergeMethod(ch, var, done, cond));
  }

  for_cell(cell, classOfObject(obj)->delegate)
  { Variable var = cell->value;
    Any val;

    if ( (val = getGetVariable(var, obj)) )
      mergeSendMethodsObject(val, ch, done, cond);
  }
}


static Chain
getFindAllSendMethodsObject(Any obj, Code cond)
{ Chain ch = answerObject(ClassChain, EAV);
  static HashTable done = NULL;
  
  if ( !done )
    done = createHashTable(toInt(32), NAME_none);

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
  { Chain ch = newObject(ClassChain, EAV);

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
  { Chain ch = newObject(ClassChain, EAV);

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
  { Chain ch = newObject(ClassChain, EAV);

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
  { Chain ch = newObject(ClassChain, EAV);

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
  { Chain ch = newObject(ClassChain, EAV);

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
  unsigned long flags;
  CloneField	next;
};

static	HashTable	CloneTable;
static  CloneField     	CloneFields;

static void
addCloneField(Any obj, unsigned long flags, Any *field, Any old)
{ CloneField kf = alloc(sizeof(struct clone_field));

  kf->instance  = obj;
  kf->field     = field;
  kf->old_value = old;
  kf->flags     = flags;
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
    CloneTable = createHashTable(toInt(32), NAME_none);

  clearHashTable(CloneTable);		/* security for handling reset */
  CloneFields = NULL;

  clone = getClone2Object(obj);
  for(kf = CloneFields; kf != NULL; kf = kf->next)
  { Any kl;

    if ( kf->flags & D_CLONE_REFCHAIN )
    { Cell cell;
      Chain clch = newObject(ClassChain, EAV);

      assignField(kf->instance, kf->field, clch);
      for_cell(cell, (Chain)kf->old_value)
      { if ( (kl = getMemberHashTable(CloneTable, cell->value)) )
	  appendChain(clch, kl);
      }
    } else
    { if ( (kl = getMemberHashTable(CloneTable, kf->old_value)) != FAIL )
	assignField(kf->instance, kf->field, kl);
    }
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
	       addCloneField(clone, D_CLONE_REFERENCE,
			     &clone->slots[i], me->slots[i]);
	     } else if ( onDFlag(var, D_CLONE_VALUE) )
	     { assign(clone, slots[i], me->slots[i]);
	     } else if ( onDFlag(var, D_CLONE_ALIEN) )
	     { clone->slots[i] = me->slots[i];
	     } else if ( onDFlag(var, D_CLONE_NIL) )
	     { assign(clone, slots[i], NIL);
	       addCloneField(clone, D_CLONE_NIL,
			     &clone->slots[i], me->slots[i]);
	     } else if ( onDFlag(var, D_CLONE_REFCHAIN) )
	     { addCloneField(clone, D_CLONE_REFCHAIN,
			     &clone->slots[i], me->slots[i]);
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

  if ( class->cloneStyle == NAME_none )
    answer(me);
  if ( class->cloneStyle == NAME_nil )
    answer(NIL);

  clone = (Instance) allocObject(class, FALSE);
  if ( offFlag(obj, F_OBTAIN_CLASSVARS) )
    clearFlag(clone, F_OBTAIN_CLASSVARS);
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
{ answer(classOfObject(obj)->name);
}


Any
getArgObject(Any obj, Int arg)
{ Class class = classOfObject(obj);
  Name selector;

  if ( isNil(class->term_names) )
    fail;

  if ( isName(selector = getElementVector(class->term_names, arg)) )
    answer( get(obj, selector, EAV) );

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
      answer(getGetVariable(var, obj));
  }

  fail;
}


status
slotObject(Any obj, Any which, Any value)
{ Variable var;

  if ( (var = getInstanceVariableClass(classOfObject(obj), which)) )
    return sendVariable(var, obj, value);

  return errorPce(obj, NAME_noVariable, which);
}


static status
isOnObject(Any obj, Name selector)
{ if ( get(obj, selector, EAV) == ON)
    succeed;
  fail;
}


static status
isOffObject(Any obj, Name selector)
{ if ( get(obj, selector, EAV) == OFF)
    succeed;
  fail;
}


static status
hasValueObject(Any obj, Name selector, Any value)
{ if (get(obj, selector, EAV) == value)
    succeed;
  fail;
}


static status
notHasValueObject(Any obj, Name selector, Any value)
{ if (get(obj, selector, EAV) != value)
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


static Any
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
{ Any a;
  Vector v;
  int shift;
  int args;

  if ( argc == 0 )
    goto usage;
  if ( argc >= 2 && isInteger(argv[argc-1]) )
  { a = argv[argc-2];
    shift = valInt(argv[argc-1]);
    args = argc-2;
  } else
  { a = argv[argc-1];
    shift = 0;
    args = argc-1;
  }

  if ( !(v = checkType(a, TypeVector, NIL)) )
  { if ( a == name_nil )
    { Name sel;

      if ( args >= 1 && (sel = checkType(argv[0], TypeName, NIL)) )
        return sendv(obj, sel, args-1, argv+1);
      fail;
    }
    goto usage;
  } else
  { int argn = args+valInt(v->size)-shift;
    ArgVector(av, argn);
    int i, n;
    
    for(i=0; i<args; i++)
      av[i] = argv[i];
    for(n=shift; n<valInt(v->size); n++)
      av[i++] = v->elements[n];

    if ( argn >= 1 )
    { Name sel;

      if ( (sel = checkType(av[0], TypeName, NIL)) )
	return sendv(obj, sel, argn-1, av+1);
      goto usage;
    }

    fail;
  }

usage:
  return errorPce(obj, NAME_badVectorUsage);
}


static Any
getVectorObject(Any obj, int argc, Any *argv)
{ Any a;
  Vector v;
  int shift;
  int args;

  if ( argc == 0 )
    goto usage;
  if ( argc >= 2 && isInteger(argv[argc-1]) )
  { a = argv[argc-2];
    shift = valInt(argv[argc-1]);
    args = argc-2;
  } else
  { a = argv[argc-1];
    shift = 0;
    args = argc-1;
  }

  if ( !(v = checkType(a, TypeVector, NIL)) )
  { if ( a == name_nil )
    { if ( args >= 1 )
	return getv(obj, (Name) argv[0], args-1, argv+1);
      fail;
    }
    goto usage;
  } else
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
	     (isDefault(cond) || forwardCode(cond, h->from, h, h->to, EAV)) )
	  answer(h);
      } else
      { if ( (hname == h->backward_name || isDefault(hname)) &&
	     (isDefault(cond) || forwardCode(cond, h->to, h, h->from, EAV)) )
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
	      { if ( h->from == obj )
		{ if ( (hname == h->forward_name || isDefault(hname)) &&
		       (isDefault(cond) ||
			forwardCode(cond, h->from, h, h->to, EAV)) )
		    freeObject(h);
		} else
		{ if ( (hname == h->backward_name || isDefault(hname)) &&
		       (isDefault(cond) ||
			forwardCode(cond, h->to, h, h->from, EAV)) )
		    freeObject(h);
		}
	      });
  }

  succeed;
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
	  forwardCode(cell->value, obj, v->name, EAV);
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
getClassVariableValueObject(Any obj, Name name)
{ if ( !isObject(obj) )
    fail;
  
  answer(getClassVariableValueClass(classOfObject(obj), name));
}


status
obtainClassVariablesObject(Any obj)
{ if ( onFlag(obj, F_OBTAIN_CLASSVARS) )
  { Instance inst = obj;
    Class class = classOfObject(obj);
    int slots = valInt(class->slots);
    int i;
    status rval = SUCCEED;

    for(i=0; i<slots; i++)
    { if ( isClassDefault(inst->slots[i]) )
      { Variable var = class->instance_variables->elements[i];
	Any value;

	if ( (value = getClassVariableValueObject(obj, var->name)) )
	{ Any v2;

	  if ( (v2 = checkType(value, var->type, obj)) )
	    assignField(inst, &inst->slots[i], v2);
	  else
	  { errorPce(var, NAME_incompatibleResource);
	    rval = FAIL;
	  }
	} else
	{ errorPce(var, NAME_noClassVariable);
	  rval = FAIL;
	}
      }
    }

    clearFlag(obj, F_OBTAIN_CLASSVARS);
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
{ if ( validateType(var->type, NIL, obj) )
    succeed;
  if ( validateType(var->type, DEFAULT, obj) )
    return sendVariable(var, obj, DEFAULT);

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translates text of the form

  <blank>*@<blank>*<digit>+		integer reference
  <blank>*@<blank>*<alnum>+		atomic reference
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getConvertObject(Any ctx, Any x)
{ char *s;
  Any rval = FAIL;

  if ( isInteger(x) )
    rval = answerObject(ClassNumber, x, EAV);

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
      rval = getObjectFromReferencePce(PCE, toInt(atol(start)));
    else
    {					/* check for @name (exception?) */
      for( s=start; isalnum(*s); s++ )
	;
      if ( *s == EOS )
	rval = getObjectAssoc(CtoKeyword(start));
    }
  }

  answer(rval);
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

  if ( onFlag(obj, F_OBTAIN_CLASSVARS) )
    errorPce(obj, NAME_classVariablesNotObtained);

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

      if ( isClassDefault(value) &&
	   getClassVariableClass(class, var->name) )
	continue;
      if ( isClassDefault(value) &&
	   instanceOfObject(obj, ClassClass) &&
	   ((Class)obj)->realised != ON )
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
	} else if ( recursive == ON && isObject(cell->value) )
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
    done = createHashTable(toInt(200), NAME_none);
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

      if ( isDefault(value) && getClassVariableClass(class, var->name) )
	value = getGetVariable(var, inst);

      forwardCode(msg, inst, NAME_slot, var->name, value, EAV);
      if ( recursive == ON && isObject(value) )
	for_slot_reference_object(value, msg, recursive, done);
    }
  }

  if ( instanceOfObject(obj, ClassChain) )
  { Cell cell;
    int n = 1;

    for_cell(cell, (Chain) obj)
    { forwardCode(msg, obj, NAME_cell, toInt(n), cell->value, EAV);

      if ( recursive == ON && isObject(cell->value) )
	for_slot_reference_object(cell->value, msg, recursive, done);
      n++;
    }
  } else if ( instanceOfObject(obj, ClassVector) )
  { for_vector((Vector) obj, Any value,
	       forwardCode(msg, NAME_element, obj, toInt(_iv), value, EAV);
	       if ( recursive == ON && isObject(value) )
		 for_slot_reference_object(value, msg, recursive, done););
  } else if ( instanceOfObject(obj, ClassHashTable) )
  { for_hash_table((HashTable) obj, s,
		   { forwardCode(msg, obj, NAME_key, s->name, s->value, EAV);

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
    done = createHashTable(toInt(200), NAME_none);

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
    PceGoal g = CurrentGoal;
    int i;

    av[0] = obj;
    for(i=0; i<argc; i++)
      av[i+1] = argv[i];

    for(i=0; i++ < 1 && isProperGoal(g); ) /* go one up for the real error */
      g = g->parent;

    if ( e->kind == NAME_error && isProperGoal(g) )
    { g->flags |= PCE_GF_EXCEPTION;
      g->errcode = PCE_ERR_ERROR;
      g->errc1   = e;
      g->errc2   = createCodeVectorv(argc+1, av);
    }

    if ( e->feedback == NAME_throw && e->kind != NAME_fatal )
    {					/* See if host wants to catch */
					/* the error.  If so, put it into */
					/* the goal and return silently */
      for( ; isProperGoal(g); g = g->parent )
      { if ( g->flags & PCE_GF_CATCH )
	{ g->flags |= PCE_GF_THROW;
	  g->errcode = PCE_ERR_ERROR;
	  g->errc1   = e->id;
	  g->errc2   = createCodeVectorv(argc+1, av);

	  fail;
	}
      }
    }
  
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

  if ( !(to = get(obj, NAME_reportTo, EAV)) )
  {
#ifdef O_RUNTIME
    to = CurrentDisplay(NIL);
#else
    if ( PCE->trap_errors == OFF )	/* Separate flag? */
      to = CurrentDisplay(NIL);
    else if ( obj != PCE )
      to = PCE;
#endif
  }

  if ( to && notNil(to) )
  { ArgVector(av, argc + 2);

    av[0] = kind;
    av[1] = fmt;
    copyArgs(argc, argv, &av[2]);

    return sendv(to, NAME_report, argc+2, av);
  } else				/* no event: print it */
  { char buf[FORMATSIZE];
    Any av[2];

    if ( isDefault(fmt) )
      fmt = (CharArray) (kind == NAME_done ? NAME_done : NAME_);
    swritefv(buf, NULL, fmt, argc, argv);
    av[0] = kind;
    av[1] = CtoTempString(buf);
    formatPcev(PCE,
	       (CharArray) CtoName(kind == NAME_progress ? "[PCE: %I%s ... " :
				   kind == NAME_done     ? "%I%s]\n" :
				   			   "[PCE: %s: %s]\n"),
	       2, av);
    if ( kind == NAME_progress )
      Cflush();
    considerPreserveObject(av[1]);

    succeed;
  }
}


		/********************************
		*        MANUAL SUPPORT		*
		********************************/

#ifndef O_RUNTIME
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
#endif


static CharArray
getPrintNameObject(Any obj)
{ CharArray name;

  if ( hasGetMethodObject(obj, NAME_name) &&
       (name = get(obj, NAME_name, EAV)) &&
       (name = checkType(name, TypeCharArray, NIL)) )
    answer(name);
  else
    answer((CharArray) CtoString(pp(obj)));
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Public Type declaractions */

char *T_report[] =
        { "kind={status,inform,progress,done,warning,error,fatal}",
	  "format=[char_array]",
	  "argument=any ..."
	};

/* Type declaractions */

static char *T_forSlotReference[] =
        { "action=code", "recursive=[bool]" };
static char *T_attribute[] =
        { "attribute|name", "value=[any]" };
static char *T_error[] =
        { "error=error", "context=unchecked ..." };
static char *T_hyper_nameADnameD_selectorAname_argumentAunchecked_XXX[] =
        { "hyper_name=[name]", "selector=name", "argument=unchecked ..." };
static char *T_hyper_nameADnameD_testADcodeD[] =
        { "hyper_name=[name]", "test=[code]" };
static char *T_attachHyper[] =
        { "hyper", "object" };
static char *T_deleteHypers[] =
        { "name=[name]", "condition=[code]" };
static char *T_slot[] =
        { "name|int", "unchecked" };
static char *T_name_any[] =
        { "name", "any" };
static char *T_convertLoadedObject[] =
        { "old_version=int", "current_version=int" };
static char *T_relayed_invocation[] =
        { "selector=name", "argument=unchecked ..." };

/* Instance Variables */

#define var_object NULL
/*
vardecl var_object[] =
{ 
};
*/

/* Send Methods */

static senddecl send_object[] =
{ SM(NAME_equal, 1, "to=any", equalObject,
     NAME_compare, "Test if i'm equal to the argument"),
  SM(NAME_sameReference, 1, "to=any", sameReferenceObject,
     NAME_compare, "Test if i'm the same object as the argument"),
  SM(NAME_forSlotReference, 2, T_forSlotReference, forSlotReferenceObject,
     NAME_debugging, "Run code on object-slot-value references"),
  SM(NAME_convertLoadedObject, 2, T_convertLoadedObject, convertLoadedObjectObject,
     NAME_file, "Called by File <-object if conversion might be required"),
  SM(NAME_initialiseNewSlot, 1, "new=variable", initialiseNewSlotObject,
     NAME_file, "Called by File <-object if a new slot is found"),
  SM(NAME_saveInFile, 1, "file", saveInFileObject,
     NAME_file, "Save object and it's context in a file"),
  SM(NAME_Free, 0, NULL, freeObject,
     NAME_function, "Equivalent to ->free"),
#ifndef O_RUNTIME
  SM(NAME_Inspect, 1, "bool", inspectObject,
     NAME_function, "Equivalent to ->inspect"),
#endif
  SM(NAME_InstanceOf, 1, "class", instanceOfObject,
     NAME_function, "Equivalent to ->instance_of"),
  SM(NAME_SameReference, 1, "to=unchecked", sameReferenceObject,
     NAME_function, "Equivalent to ->same_reference"),
  SM(NAME_hasGetMethod, 1, "selector=name", hasGetMethodObject,
     NAME_meta, "Test if object defines get_method"),
  SM(NAME_hasSendMethod, 1, "selector=name", hasSendMethodObject,
     NAME_meta, "Test if object defines send_method"),
  SM(NAME_done, 0, NULL, doneObject,
     NAME_oms, "Indicate I'm done with some answer"),
  SM(NAME_free, 0, NULL, freeObject,
     NAME_oms, "Delete object from the object-base"),
  SM(NAME_initialise, 1, "unchecked ...", initialiseObject,
     NAME_oms, "Initialise variables"),
  SM(NAME_lockObject, 1, "bool", lockObject,
     NAME_oms, "Lock object for incremental garbage collection"),
  SM(NAME_protect, 0, NULL, protectObject,
     NAME_oms, "Lock object for destruction with ->free"),
  SM(NAME_unlink, 0, NULL, succeedObject,
     NAME_oms, "Unlink from environment"),
  SM(NAME_unlinking, 0, NULL, unlinkingObject,
     NAME_oms, "Try if ->unlink is in progress"),
  SM(NAME_getMethod, 1, "get_method|chain", getMethodObject,
     NAME_programming, "Add an object-level get_method"),
  SM(NAME_sendClass, 2, T_relayed_invocation, sendClassObject,
     NAME_programming, "Send using method of class of object"),
  SM(NAME_sendMethod, 1, "send_method|chain", sendMethodObject,
     NAME_programming, "Add an object-level send_method"),
  SM(NAME_sendSub, 2, T_relayed_invocation, sendSubObject,
     NAME_programming, "Send using method of subclass"),
  SM(NAME_sendSuper, 2, T_relayed_invocation, sendSuperObject,
     NAME_programming, "Send using method of super-class"),
  SM(NAME_sendSuperVector, 1, "unchecked ...", sendSuperVectorObject,
     NAME_programming, "Varargs: any ..., vector, [int]"),
  SM(NAME_sendVector, 1, "unchecked ...", sendVectorObject,
     NAME_programming, "Varargs: any ..., vector, [int]"),
  SM(NAME_slot, 2, T_slot, slotObject,
     NAME_programming, "Set value of an instance variable"),
  SM(NAME_nameReference, 1, "name*", nameReferenceObject,
     NAME_reference, "Change named (atomic) reference"),
  SM(NAME_attachHyper, 2, T_attachHyper, attachHyperObject,
     NAME_relation, "Attach a hyper to an object"),
  SM(NAME_deleteHyper, 1, "hyper", deleteHyperObject,
     NAME_relation, "Detach a hyper from an object"),
  SM(NAME_deleteHypers, 2, T_deleteHypers, freeHypersObject,
     NAME_relation, "Delete all matching hypers"),
  SM(NAME_sendHyper, 3, T_hyper_nameADnameD_selectorAname_argumentAunchecked_XXX, sendHyperObject,
     NAME_relation, "Send message using named hypers"),
  SM(NAME_error, 2, T_error, errorObjectv,
     NAME_report, "Initiate an error: id, context ..."),
  SM(NAME_report, 3, T_report, reportObject,
     NAME_report, "Report message (send to @event <-receiver)"),
  SM(NAME_obtainClassVariables, 0, NULL, obtainClassVariablesObject,
     NAME_resource, "Obtain class-variable values for @default-valued slots"),
  SM(NAME_attribute, 2, T_attribute, attributeObject,
     NAME_storage, "Append/change object-level attribute"),
  SM(NAME_deleteAttribute, 1, "name|attribute", deleteAttributeObject,
     NAME_storage, "Delete object-level attribute"),
  SM(NAME_hasValue, 2, T_name_any, hasValueObject,
     NAME_test, "Test if Obj <-name equals 2nd argument"),
  SM(NAME_isOff, 1, "name", isOffObject,
     NAME_test, "Test if Obj <-name returns @off"),
  SM(NAME_isOn, 1, "name", isOnObject,
     NAME_test, "Test if Obj <-name returns @on"),
  SM(NAME_notHasValue, 2, T_name_any, notHasValueObject,
     NAME_test, "Test if Obj <-name not-equal 2nd argument"),
  SM(NAME_instanceOf, 1, "class", instanceOfObject,
     NAME_type, "Test of object is an instance of class"),
  SM(NAME_sameClass, 1, "object", sameClassObject,
     NAME_type, "Is object of the same class as argument"),
  SM(NAME_updateConstraints, 0, NULL, updateConstraintsObject,
     NAME_constraint, "Execute all constraints")
#ifndef O_RUNTIME
  ,
  SM(NAME_inspect, 1, "bool", inspectObject,
     NAME_debugging, "Forward changes via classes' changed_messages"),
  SM(NAME_Check, 1, "recursive=[bool]", CheckObject,
     NAME_debugging, "Check types for all instance-variables of object")
#endif /*O_RUNTIME*/
};

/* Get Methods */

static getdecl get_object[] =
{ GM(NAME_clone, 0, "object", NULL, getCloneObject,
     NAME_copy, "New object that is a (recursive) copy)"),
  GM(NAME_Flags, 0, "name", NULL, getFlagsObject,
     NAME_debugging, "Name width {P, L and A} flags"),
  GM(NAME_codeReferences, 0, "int", NULL, getCodeReferencesObject,
     NAME_debugging, "Number of code-references to this object"),
  GM(NAME_references, 0, "int", NULL, getReferencesObject,
     NAME_debugging, "Number of references to this object"),
  GM(NAME_storageReference, 0, "any", NULL, getFailObject,
     NAME_file, "Description name for ->save_in_file"),
  GM(NAME_Class, 0, "class", NULL, getClassObject,
     NAME_function, "Equivalent to <-class"),
  GM(NAME_ClassName, 0, "name", NULL, getClassNameObject,
     NAME_function, "Equivalent to <-class_name"),
  GM(NAME_References, 0, "int", NULL, getReferencesObject,
     NAME_function, "Equivalent to <-references"),
  GM(NAME_Slot, 1, "unchecked", "name|int", getSlotObject,
     NAME_function, "Equivalent to <-slot"),
  GM(NAME_allAttributes, 1, "chain", "create=[bool]", getAllAttributesObject,
     NAME_meta, "Chain with object-level attributes"),
  GM(NAME_allConstraints, 1, "chain", "create=[bool]", getAllConstraintsObject,
     NAME_meta, "Chain with all constraints"),
  GM(NAME_allGetMethods, 1, "chain", "create=[bool]", getAllGetMethodsObject,
     NAME_meta, "Chain with all get methods"),
  GM(NAME_allHypers, 1, "chain", "create=[bool]", getAllHypersObject,
     NAME_meta, "Chain with all hypers"),
  GM(NAME_allSendMethods, 1, "chain", "create=[bool]", getAllSendMethodsObject,
     NAME_meta, "Chain with all send methods"),
  GM(NAME_findAllSendMethods, 1, "chain", "condition=[code]", getFindAllSendMethodsObject,
     NAME_meta, "New chain with all send-methods satisfying code"),
  GM(NAME_getMethod, 1, "tuple", "name", getGetMethodObject,
     NAME_meta, "Tuple containing receiver and implementing object"),
  GM(NAME_sendMethod, 1, "tuple", "name", getSendMethodObject,
     NAME_meta, "Tuple containing receiver and implementing object"),
  GM(NAME_convert, 1, "object", "int|char_array", getConvertObject,
     NAME_oms, "Convert '@reference' into object"),
  GM(NAME_unlock, 0, "unchecked", NULL, getUnlockObject,
     NAME_oms, "Unlock object and return <-self"),
  GM(NAME_lockObject, 0, "bool", NULL, getLockObject,
     NAME_oms, "Boolean to indicate locked for GC"),
  GM(NAME_protect, 0, "bool", NULL, getProtectObject,
     NAME_oms, "Boolean to indicate locked for ->free"),
  GM(NAME_self, 0, "object", NULL, getSelfObject,
     NAME_oms, "Returns itself"),
  GM(NAME_getClass, 2, "unchecked", T_relayed_invocation, getGetClassObject,
     NAME_programming, "Get, using method of class of object"),
  GM(NAME_getSub, 2, "unchecked", T_relayed_invocation, getGetSubObject,
     NAME_programming, "Get, using method of sub-class"),
  GM(NAME_getSuper, 2, "unchecked", T_relayed_invocation, getGetSuperObject,
     NAME_programming, "Get, using method of super-class"),
  GM(NAME_getVector, 1, "unchecked", "unchecked ...", getVectorObject,
     NAME_programming, "Varargs: any ..., vector, [int]"),
  GM(NAME_slot, 1, "unchecked", "name|int", getSlotObject,
     NAME_programming, "Get value of a slot"),
  GM(NAME_objectReference, 0, "name|int", NULL, getObjectReferenceObject,
     NAME_reference, "Name of the object (e.g. @pce)"),
  GM(NAME_findHyper, 2, "hyper", T_hyper_nameADnameD_testADcodeD, getFindHyperObject,
     NAME_relation, "Find hyper-relation object"),
  GM(NAME_getHyper, 3, "unchecked", T_hyper_nameADnameD_selectorAname_argumentAunchecked_XXX, getHyperObject,
     NAME_relation, "Get-operation using named hypers"),
  GM(NAME_hypered, 2, "object", T_hyper_nameADnameD_testADcodeD, getHyperedObject,
     NAME_relation, "Find hyper-related object"),
  GM(NAME_reportTo, 0, "object", NULL, getReportToObject,
     NAME_report, "Object for ->report (@event <-receiver)"),
  GM(NAME_classVariableValue, 1, "any", "name", getClassVariableValueObject,
     NAME_default, "Get value of associated Default"),
  GM(NAME_attribute, 1, "unchecked", "name", getAttributeObject,
     NAME_storage, "Get value of a object-level attribute"),
  GM(NAME_Arg, 1, "unchecked", "int", getArgObject,
     NAME_term, "Nth-1 argument of term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityObject,
     NAME_term, "Number of arguments of term description"),
  GM(NAME_functor, 0, "name", NULL, getFunctorObject,
     NAME_term, "Functor (name) of term description"),
  GM(NAME_printName, 0, "text=char_array", NULL, getPrintNameObject,
     NAME_textual, "Calls <-name"),
  GM(NAME_class, 0, "class", NULL, getClassObject,
     NAME_type, "Class the object belongs to"),
  GM(NAME_className, 0, "name", NULL, getClassNameObject,
     NAME_type, "Name of the class the object belongs to")
#ifndef O_RUNTIME
  ,
  GM(NAME_inspect, 0, "bool", NULL, getInspectObject,
     NAME_debugging, "Boolean to indicate changes forwarding"),
  GM(NAME_Inspect, 0, "bool", NULL, getInspectObject,
     NAME_function, "Equivalent to <-inspect"),
  GM(NAME_ManId, 0, "name", NULL, getManIdObject,
     NAME_function, "Equivalent to <-man_id"),
  GM(NAME_manId, 0, "name", NULL, getManIdObject,
     NAME_manual, "Card Id for global object"),
  GM(NAME_manIndicator, 0, "name", NULL, getManIndicatorObject,
     NAME_manual, "Manual type indicator (`O')")
#endif /*O_RUNTIME*/
};

/* Resources */

#define rc_object NULL
/*
static classvardecl rc_object[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(object_decls,
          var_object, send_object, get_object, rc_object,
          0, NULL,
          "$Rev$");


status
makeClassObject(Class class)
{ declareClass(class, &object_decls);
  setChangedFunctionClass(class, changedFieldObject);


  succeed;
}


