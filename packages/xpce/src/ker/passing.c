/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define INLINE_UTILITIES 1
#include <h/kernel.h>
#include <h/trace.h>
#include <itf/c.h>
#include <rel/proto.h>

static int resolve_errno;

#define RE_FUNCTION_FAILED	0
#define RE_NOIMPLEMENTATION	1

		/********************************
		*       RESOLVING METHODS	*
		********************************/

typedef struct dCell *DCell;

struct dCell
{ Any	implementation;			/* Implementing method */
  Any	receiver;			/* Receiver */
  int	size;				/* size of the dlist */
  DCell next;				/* Next in list */
};

static inline void
appendDList(DCell *l, Any implementation, Any receiver)
{ if ( l )
  { DCell c = alloc(sizeof(struct dCell));
    c->implementation = implementation;
    c->receiver       = receiver;
    c->size	      = (*l ? (*l)->size + 1 : 1);
    c->next           = *l;
    

    *l = c;
  }
}


static void
freeDList(DCell l)
{ if ( l )
  { freeDList(l->next);
    unalloc(sizeof(struct dCell), l);
  }
} 


status
sendImplementation(Any implementation, Any receiver, int argc, Any *argv)
{ Class class = classOfObject(implementation);

  FixSendFunctionClass(class, NAME_send);
  return (*class->send_function)(implementation, receiver, argc, argv);
}


static Method
getMethodMethodList(Any list, Name sel)
{ if ( instanceOfObject(list, ClassMethod) )
  { Method m = list;

    if ( m->name == sel )
      answer(m);

    fail;
  } else if ( instanceOfObject(list, ClassChain) )
  { Chain ch = list;
    Cell cell;
    Method m;

    for_cell(cell, ch)
    { if ( (m = getMethodMethodList(cell->value, sel)) )
	answer(m);
    }

    fail;
  } else
  { errorPce(list, NAME_unexpectedType, CtoType("method|chain"));
    fail;
  }
}


Any
resolveSendMethodObject(Any obj, Class class, Name sel,
			Any *receiver, DelegateList dlist)
{ Any m;
  Cell cell;
  
  if ( receiver )
    *receiver = obj;

  if ( !class )
  { if ( onFlag(obj, F_ACTIVE|F_SENDMETHOD|F_ATTRIBUTE) )
    { while(isFunction(obj))
      { if ( (m = getSendMethodFunction(obj, sel)) )
	  return m;

	if ( (obj = getExecuteFunction((Function) obj)) )
	{ if ( isInteger(obj) )
	    obj = answerObject(ClassNumber, obj, 0);
	  if ( receiver )
	    *receiver = obj;
	} else
	{ resolve_errno = RE_FUNCTION_FAILED;
	  fail;
	}
      }

      if ( onFlag(obj, F_SENDMETHOD) )
      { Chain ch = getAllSendMethodsObject(obj, ON);

	for_cell(cell, ch)
	{ if ( (m = getMethodMethodList(cell->value, sel)) )
	    return m;
	}
      }

      if ( onFlag(obj, F_ATTRIBUTE) )
      { Chain ch = getAllAttributesObject(obj, ON);

	for_cell(cell, ch)
	{ Attribute att = cell->value;

	  if ( att->name == sel )
	    return att;
	}
      }
    }

    class = classOfObject(obj);
  }

  if ( (m = getSendMethodClass(class, sel)) )
    return m;

  for_cell(cell, class->delegate)
  { Variable var = cell->value;
    Any val, r;

    if ( (val = getGetVariable(var, obj, 0, NULL)) &&
	 (m = resolveSendMethodObject(val, NULL, sel, &r, dlist)) )
    { if ( instanceOfObject(var, ClassDelegateVariable) )
      { DelegateVariable dvar = (DelegateVariable) var;

	if ( notNil(dvar->wrapper) )
	{ SendMethod dm;

	  if ( (dm = getSendMethodClass(class, dvar->wrapper)) )
	    appendDList(dlist, dm, obj);
	  else
	    fail;
	}
      }

      if ( receiver )
	*receiver = r;
      return m;
    }
  }

  resolve_errno = RE_NOIMPLEMENTATION;
  fail;
}


Any
resolveGetMethodObject(Any obj, Class class, Name sel, Any *receiver)
{ Any m;
  Cell cell;

  if ( receiver )
    *receiver = obj;

  if ( !class )
  { if ( onFlag(obj, F_ACTIVE|F_GETMETHOD|F_ATTRIBUTE) )
    { while(isFunction(obj))
      { if ( (m = getGetMethodFunction(obj, sel)) )
	  return m;

	if ( (obj = getExecuteFunction((Function) obj)) )
	{ if ( isInteger(obj) )
	    obj = answerObject(ClassNumber, obj, 0);
	  if ( receiver )
	    *receiver = obj;
	} else
	{ resolve_errno = RE_FUNCTION_FAILED;
	  fail;
	}
      }

      if ( onFlag(obj, F_GETMETHOD) )
      { Chain ch = getAllGetMethodsObject(obj, ON);

	for_cell(cell, ch)
	{ if ( (m = getMethodMethodList(cell->value, sel)) )
	    return m;
	}
      }

      if ( onFlag(obj, F_ATTRIBUTE) )
      { Chain ch = getAllAttributesObject(obj, ON);

	for_cell(cell, ch)
	{ Attribute att = cell->value;

	  if ( att->name == sel )
	    return att;
	}
      }
    }

    class = classOfObject(obj);
  }

  if ( (m = getGetMethodClass(class, sel)) )
    return m;

  for_cell(cell, class->delegate)
  { Variable var = cell->value;
    Any val, m, r;

    if ( (val = getGetVariable(var, obj, 0, NULL)) &&
	 (m = resolveGetMethodObject(val, NULL, sel, &r)) )
    { if ( receiver )
	*receiver = r;
      return m;
    }
  }

  resolve_errno = RE_NOIMPLEMENTATION;
  fail;
}


		/********************************
		*             SEND		*
		********************************/

status
vm_send(Any receiver, Name selector, Class class, int argc, const Any argv[])
{ Any implementation;
  status rval;
  Any rec;
  goal goal;
  Goal g = &goal;
  DCell dlist = NULL;

  pushGoal(g, VmiSend, receiver, selector, argc, argv);

  if ( notName(selector) )
  { Name sel;

    if ( !(sel = checkType(selector, TypeName, receiver)) )
    { errorPce(VmiSend, NAME_badSelector, selector);
      failGoal;
    }

    g->selector = selector = sel;
  }
  traceEnter(g);			/* delayed till we know selector */

					/* Int and freed object */
  if ( isInteger(receiver) )
  { receiver = answerObject(ClassNumber, receiver, 0);
  } else if ( isFreedObj(receiver) )
  { if ( selector == NAME_free )
      outGoal(SUCCEED);
    errorPce(VmiSend, NAME_freedObject, receiver);
    failGoal;
  }
					/* Find implementation */
  implementation = resolveSendMethodObject(receiver, class, selector,
					   &rec, &dlist);

  if ( implementation )			/* Execute implementation */
  { if ( dlist )			/* Delegation with side-effects */
    { ArgVector(av, dlist->size*2 + argc);
      int i, ac = 0;
      DCell c;
      Any dm = dlist->implementation;
      Any dmrec = dlist->receiver;
      Class dmcl = classOfObject(dm);

      for( c = dlist->next; c; c = c->next )
      { av[ac++] = c->implementation;
	av[ac++] = c->receiver;
      }
      av[ac++] = implementation;
      av[ac++] = rec;
      for(i=0; i<argc; i++)
	av[ac++] = argv[i];
      freeDList(dlist);
      FixSendFunctionClass(dmcl, NAME_send);
      rval = (*dmcl->send_function)(dm, dmrec, ac, av);
    } else
    { Class ic = classOfObject(implementation);

      FixSendFunctionClass(ic, NAME_send);
      rval = (*ic->send_function)(implementation, rec, argc, argv);
    }
  } else
  { Class cl = class ? class : classOfObject(rec);

    if ( isDefault(cl->send_catch_all) )
    { SendMethod m;

      if ( (m=getSendMethodClass(cl, NAME_catchAll)) )
      { setDFlag(m, D_TYPENOWARN);
	assign(cl, send_catch_all, m);
      } else
	assign(cl, send_catch_all, NIL);
    }

    if ( notNil(cl->send_catch_all) )
    { int ac;
      ArgVector(av, argc+1);

      av[0] = selector;
      for(ac = 1; ac <= argc; ac++)
	av[ac] = argv[ac-1];

      assign(PCE, last_error, NAME_noBehaviour);
      if ( (rval = sendSendMethod(cl->send_catch_all, rec, ac, av)) )
      { if ( PCE->last_error == NAME_noBehaviour )
	  assign(PCE, last_error, NIL);
      } else
      { if ( PCE->last_error == NAME_noBehaviour )
#ifndef O_RUNTIME
	  errorPce(VmiSend, NAME_noBehaviour, rec, CtoName("->"), selector);
#else
	  errorPce(VmiSend, NAME_noBehaviour);
#endif
      }
    } else
    { if ( resolve_errno == RE_NOIMPLEMENTATION )
#ifndef O_RUNTIME
        errorPce(VmiSend, NAME_noBehaviour, rec, CtoName("->"), selector);
#else
	errorPce(VmiSend, NAME_noBehaviour);
#endif

      rval = FAIL;			/* undefined method */
    }
  }
					/* Handle constraints */
  if ( onFlag(receiver, F_CONSTRAINT) &&
       !class &&
       rval &&
       !isFreedObj(receiver) )
  { Chain constraints = getAllConstraintsObject(receiver, ON);
    Cell cell;

    for_cell(cell, constraints)
      lockConstraint(cell->value, receiver);

    for_cell(cell, constraints)
      executeConstraint(cell->value, receiver);

    for_cell(cell, constraints)
      unlockConstraint(cell->value, receiver);
  }

out:
  traceReturn(g, rval);
  popGoal();

  return rval;
}


#undef sendv
status
sendv(Any receiver, Name selector, int argc, Any *argv)
{ return vm_send(receiver, selector, NULL, argc, argv);
}


status
simpleSendv(Any receiver, Name selector, int argc, Any *argv)
{ return vm_send(receiver, selector, classOfObject(receiver), argc, argv);
}


status					/* QuickAndDirtySend */
qadSendv(Any r, Name selector, int ac, Any *av)
{ if ( !TraceMode)
  { SendMethod implementation = getSendMethodClass(classOfObject(r), selector);

#define F (implementation->function)
    if ( instanceOfObject(implementation, ClassSendMethod) &&
	 F &&
	 offDFlag(implementation, D_CXX))
    { switch(ac)
      { case 0: return (status)(*F)(r);
	case 1: return (status)(*F)(r, av[0]);
	case 2: return (status)(*F)(r, av[0],av[1]);
	case 3: return (status)(*F)(r, av[0],av[1],av[2]);
	case 4: return (status)(*F)(r, av[0],av[1],av[2],av[3]);
	case 5: return (status)(*F)(r, av[0],av[1],av[2],av[3],av[4]);
	case 6: return (status)(*F)(r, av[0],av[1],av[2],av[3],av[4],av[5]);
      }
    }
  }
#undef F

  return simpleSendv(r, selector, ac, av);
}


Any
vm_get(Any receiver, Name selector, Class class, int argc, const Any argv[])
{ Any implementation;
  Any rval;
  Any rec;
  goal goal;
  Goal g = &goal;
  
  pushGoal(g, VmiGet, receiver, selector, argc, argv);
					/* Resolve selector */
  if ( notName(selector) )
  { Name sel;

    if ( !(sel = checkType(selector, TypeName, receiver)) )
    { errorPce(VmiGet, NAME_badSelector, selector);
      failGoal;
    }

    g->selector = selector = sel;
  }
  traceEnter(g);			/* delayed till selector is known */

					/* Int and freed object */
  if ( isInteger(receiver) )
    receiver = answerObject(ClassNumber, receiver, 0);
  else if ( isFreedObj(receiver) )
  { errorPce(VmiGet, NAME_freedObject, receiver);
    failGoal;
  }

					/* Find implementation */
  implementation = resolveGetMethodObject(receiver, class, selector, &rec);

  if ( implementation )			/* Execute implementation */
  { Class ic = classOfObject(implementation);

    FixGetFunctionClass(ic, NAME_get);
    rval = (*ic->get_function)(implementation, rec, argc, argv);
  } else
  { Class cl = class ? class : classOfObject(rec);

    if ( isDefault(cl->get_catch_all) )
    { GetMethod m;

      if ( (m=getGetMethodClass(cl, NAME_catchAll)) )
      { setDFlag(m, D_TYPENOWARN);
	assign(cl, get_catch_all, m);
      } else
	assign(cl, get_catch_all, NIL);
    }

    if ( notNil(cl->get_catch_all) )
    { int ac;
      ArgVector(av, argc+1);

      av[0] = selector;
      for(ac = 1; ac <= argc; ac++)
	av[ac] = argv[ac-1];

      assign(PCE, last_error, NAME_noBehaviour);
      if ( (rval = getGetGetMethod(cl->get_catch_all, rec, ac, av)) )
      { if ( PCE->last_error == NAME_noBehaviour )
	  assign(PCE, last_error, NIL);
      } else
      { if ( PCE->last_error == NAME_noBehaviour )
#ifndef O_RUNTIME
	  errorPce(VmiGet, NAME_noBehaviour, rec, CtoName("<-"), selector);
#else
	  errorPce(VmiGet, NAME_noBehaviour);
#endif
      }
    } else
    { if ( resolve_errno == RE_NOIMPLEMENTATION )
#ifndef O_RUNTIME
	errorPce(VmiGet, NAME_noBehaviour, rec, CtoName("<-"), selector);
#else
	errorPce(VmiGet, NAME_noBehaviour);
#endif

      rval = FAIL;	
    }
  }

out:
  traceAnswer(g, rval);
  popGoal();

  return rval;
}


#undef getv
Any
getv(Any receiver, Name selector, int argc, Any *argv)
{ return vm_get(receiver, selector, NULL, argc, argv);
}


Any					/* QuickAndDirtyGet */
qadGetv(Any r, Name selector, int ac, Any *av)
{ if ( !TraceMode)
  { GetMethod implementation = getGetMethodClass(classOfObject(r), selector);

#define F (implementation->function)
    if ( instanceOfObject(implementation, ClassGetMethod) &&
	 F &&
	 offDFlag(implementation, D_CXX) )
    { switch(ac)
      { case 0: return (*F)(r);
	case 1: return (*F)(r, av[0]);
	case 2: return (*F)(r, av[0],av[1]);
	case 3: return (*F)(r, av[0],av[1],av[2]);
	case 4: return (*F)(r, av[0],av[1],av[2],av[3]);
	case 5: return (*F)(r, av[0],av[1],av[2],av[3],av[4]);
	case 6: return (*F)(r, av[0],av[1],av[2],av[3],av[4],av[5]);
      }
    }
  }
#undef F

  return vm_get(r, selector, classOfObject(r), ac, av);
}


status
errorTypeMismatch(Any rec, Any impl, int arg, Type type)
{ Type argtype;
  Name argname = NIL;

  if ( instanceOfObject(impl, ClassMethod) )
  { Method m = impl;

    argtype = m->types->elements[arg-1];
  } else if ( instanceOfObject(impl, ClassVariable) )
  { Variable v = impl;
    argtype = v->type;
  } else
  { argtype = type;
  }

  if ( instanceOfObject(argtype, ClassType) )
    argname = argtype->argument_name;
  if ( isNil(argname) )
    argname = CtoName("?");

  return errorPce(impl, NAME_argumentType,
		  toInt(arg), argname, getNameType(type));
}


		/********************************
		*        VARARG VERSIONS	*
		********************************/

status
send(Any receiver, Name selector, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, selector);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return vm_send(receiver, selector, NULL, argc, argv);
}


Any
get(Any receiver, Name selector, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc;

  va_start(args, selector);
  for(argc=0; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return vm_get(receiver, selector, NULL, argc, argv);
}

