/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: it is highly dubious that the ->execute_method can both be a send
and a get method.  For the moment we will leave this.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseObtainv(Obtain obt, Any receiver, Name selector, int argc, Any *argv)
{ assign(obt, receiver, receiver);
  assign(obt, selector, selector);

  if ( argc )
    assign(obt, arguments, newObjectv(ClassCodeVector, argc, argv));

  return initialiseFunction((Function) obt);
}


static Int
getArityObtain(Obtain msg)
{ if ( isNil(msg->arguments) )
    answer(TWO);
  else
    answer(add(msg->arguments->size, TWO));
}


static Any
getArgObtain(Obtain msg, Int arg)
{ int n = valInt(arg);

  switch(n)
  { case 1:	answer(msg->receiver);
    case 2:	answer((Any) msg->selector);
    default:	if ( n < 1 || n > valInt(getArityObtain(msg)) )
		  fail;
                answer(msg->arguments->elements[n-3]);
  }
}


static Any
getExecuteObtain(Obtain obt)
{ Any receiver;

  TRY(receiver = expandCodeArgument(obt->receiver));

  if ( isNil(obt->arguments) )
  { answer(getv(receiver, obt->selector, 0, NULL));
  } else
  { int n;
    int argc = valInt(obt->arguments->size);
    ArgVector(argv, argc);
    Any *elms = obt->arguments->elements;

    for(n = 0; n < argc; n++)
      TRY(argv[n] = expandCodeArgument(elms[n]));

    answer(getv(receiver, obt->selector, argc, argv));
  }
}


status
makeClassObtain(Class class)
{ sourceClass(class, makeClassObtain, __FILE__, "$Revision$");
  termClass(class, "?", ARGC_UNKNOWN);

  localClass(class, NAME_receiver, NAME_storage, "object|function", NAME_none,
	     "Receiver of the operation");
  localClass(class, NAME_selector, NAME_storage, "name|function", NAME_none,
	     "Name of the operation");
  localClass(class, NAME_arguments, NAME_storage, "code_vector*", NAME_none,
	     "Vector of arguments");

  termClass(class, "?", 2, NAME_receiver, NAME_selector);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "receiver=object|function", "selector=name|function",
	     "argument=any|function ...",
	     "Create from receiver, selector and args",
	     initialiseObtainv);

  getMethod(class, NAME_Arg, DEFAULT, "unchecked", 1, "int",
	    "Nth-1 argument of term instead of result's",
	    getArgObtain);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity of term instead of result's",
	    getArityObtain);
  getMethod(class, NAME_Execute, DEFAULT, "unchecked", 0,
	    "Execute the get-operation",
	    getExecuteObtain);

  succeed;
}

