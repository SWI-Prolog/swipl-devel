/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


SendMethod
createSendMethod(Name name, Vector types, StringObj doc, SendFunc action)
{ SendMethod m = alloc(sizeof(struct send_method));

  initHeaderObj(m, ClassSendMethod);
  createMethod((Method) m, name, types, doc, (Func) action);

  return m;
}


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


status
makeClassSendMethod(Class class)
{ sourceClass(class, makeClassSendMethod, __FILE__, "$Revision$");

  sendMethod(class, NAME_send, NAME_execute, 2,
	     "receiver=object", "argument=unchecked ...",
	     "Invoke send method on object",
	     sendSendMethod);

  succeed;
}
