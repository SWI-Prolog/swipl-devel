/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define INLINE_UTILITIES 1
#include <h/kernel.h>


SendMethod
createSendMethod(Name name, Vector types, StringObj doc, SendFunc action)
{ SendMethod m = alloc(sizeof(struct send_method));

  initHeaderObj(m, ClassSendMethod);
  createMethod((Method) m, name, types, doc, (Func) action);

  return m;
}


#if 0
status
sendSendMethod(SendMethod m, Any receiver, int argc, const Any argv[])
{ status rval;
  AnswerMark mark;
  goal goal;
  Goal g = &goal;

  pushGoal(g, m, receiver, m->name, argc, argv);
  traceEnter(g);

  markAnswerStack(mark);
  Mode(MODE_SYSTEM,
       rval = (status) invokeMethod((Method) m, NAME_send,
				    receiver, argc, argv);
       rewindAnswerStack(mark, NIL));

  traceReturn(g, rval);
  popGoal();

  return rval ? SUCCEED : FAIL;
}
#endif


status
sendSendMethod(SendMethod m, Any receiver, int argc, const Any argv[])
{ status rval;
  AnswerMark mark;
  goal goal;
  Goal g = &goal;
  int _xmode = ExecuteMode;

  pushGoal(g, m, receiver, m->name, argc, argv);
  traceEnter(g);

  markAnswerStack(mark);
  ExecuteMode = MODE_SYSTEM;

  if ( m->types->size == ZERO )		/* no argument method */
  { ExecuteCalls++;
  
    if ( argc > 0 )
    { if ( offDFlag(m, D_TYPENOWARN) )
	errorPce(m, NAME_argumentCount, ZERO);
      rval = FAIL;
    } else
    { if ( m->function )
      { SendFunc f = (SendFunc) m->function;

#if O_CPLUSPLUS
        if ( isCppFunctionPointer(f) )
	{ void *cppf = valCppFunctionPointer(f);

	  withReceiver(receiver, m->context,
		       rval = callCPlusPlusPceMethodProc(rec, cppf, 0, NULL));
	} else
#endif O_CPLUSPLUS
        { rval = (*f)(receiver);
	}
      } else				/* code implementation */
      { if ( isNil(m->message) )
	{ errorPce(m, NAME_noImplementation);
	  fail;
	}
	
	withReceiver(receiver, m->context,
		     withArgs(0, (Any *)NULL, rval = executeCode(m->message)));
      }
    }
  } else if ( m->types->size == ONE &&
	      ((Type *)m->types->elements)[0]->vector == OFF )
  { Type t = m->types->elements[0];	/* type */
    Any arg;				/* the argument */

    ExecuteCalls++;

    if ( argc == 0 )
      arg = DEFAULT;
    else if ( argc == 1 )
      arg = argv[0];
    else
    { if ( offDFlag(m, D_TYPENOWARN) )
	errorPce(m, NAME_argumentCount, ONE);
      rval = FAIL;
      goto out;
    }

    if ( !(arg = CheckType(arg, t, receiver)) )
    { if ( offDFlag(m, D_TYPENOWARN) &&
	  CheckTypeError != CTE_OBTAINER_FAILED )
	errorTypeMismatch(receiver, m, 1, t);
      rval = FAIL;
      goto out;
    }

    if ( m->function )
    { SendFunc f = (SendFunc) m->function;

#if O_CPLUSPLUS
      if ( isCppFunctionPointer(f) )
      { void *cppf = valCppFunctionPointer(f);

	withReceiver(receiver, m->context,
		     rval = callCPlusPlusPceMethodProc(rec, cppf, 1, &arg));
      } else
#endif O_CPLUSPLUS
      { rval = (*f)(receiver, arg);
      }
    } else				/* code implementation */
    { if ( isNil(m->message) )
      { errorPce(m, NAME_noImplementation);
	rval = FAIL;
      } else
      { withReceiver(receiver, m->context,
		     rval = forwardCodev(m->message, 1, &arg));
      }
    }
  } else
  { rval = (invokeMethod((Method) m, NAME_send, receiver, argc, argv) ?
	      						SUCCEED : FAIL);
  }

out:
  rewindAnswerStack(mark, NIL);
  ExecuteMode = _xmode;
  traceReturn(g, rval);
  popGoal();

  return rval;
}


status
makeClassSendMethod(Class class)
{ sourceClass(class, makeClassSendMethod, __FILE__, "$Revision$");

  sendMethod(class, NAME_send, NAME_execute, 2,
	     "receiver=object", "argument=unchecked ...",
	     "Invoke send method on object",
	     sendSendMethod);

  succeed;
}
