/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define INLINE_UTILITIES 1
#include <h/kernel.h>


static status
initialiseMessagev(Message msg, Any rec, Name sel, int argc, Any *argv)
{ assign(msg, receiver, rec);
  assign(msg, selector, sel);

  if ( argc )
    assign(msg, arguments, newObjectv(ClassCodeVector, argc, argv));

  return initialiseCode((Code) msg);
}


static Int
getArityMessage(Message msg)
{ if ( isNil(msg->arguments) )
    answer(TWO);
  else
    answer(add(msg->arguments->size, TWO));
}


static Any
getArgMessage(Message msg, Int arg)
{ int n = valInt(arg);

  switch(n)
  { case 1:	answer(msg->receiver);
    case 2:	answer((Any) msg->selector);
    default:	if ( n < 1 || n > valInt(getArityMessage(msg)) )
		  fail;
                answer(msg->arguments->elements[n-3]);
  }
}


static status
argumentMessage(Message msg, Int n, Any val)
{ if ( valInt(n) < 1 )
    fail;

  if ( isNil(msg->arguments) )
    assign(msg, arguments, newObject(ClassVector, 0));

  return elementVector(msg->arguments, n, val);
}


static Any
getArgumentMessage(Message msg, Int n)
{ if ( valInt(n) < 1 )
    fail;

  if ( isNil(msg->arguments) )
    fail;

  return getElementVector(msg->arguments, n);
}


status
ExecuteMessage(Message msg)
{ Any receiver;

  TRY(receiver = expandCodeArgument(msg->receiver));

  if ( isNil(msg->arguments) )
  { return sendv(receiver, msg->selector, 0, NULL);
  } else
  { int n;
    int argc = valInt(msg->arguments->size);
    ArgVector(argv, argc);
    Any *elms = msg->arguments->elements;
    Any *av = argv;

    for(n = argc; --n >=0 ; )
      TRY(*av++ = expandCodeArgument(*elms++));

    return sendv(receiver, msg->selector, argc, argv);
  }
}


status
makeClassMessage(Class class)
{ sourceClass(class, makeClassMessage, __FILE__, "$Revision$");
  termClass(class, "message", ARGC_UNKNOWN);

  localClass(class, NAME_receiver, NAME_storage, "object|function", NAME_both,
	     "Receiver of the operation");
  localClass(class, NAME_selector, NAME_storage, "name|function", NAME_both,
	     "Name of the operation");
  localClass(class, NAME_arguments, NAME_storage, "code_vector*", NAME_get,
	     "Vector of arguments");

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "receiver=object|function", "selector=name|function",
	     "argument=any|function ...",
	     "Create from receiver, selector and args",
	     initialiseMessagev);
  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Send the message",
	     ExecuteMessage);
  sendMethod(class, NAME_argument, NAME_argument, 2,
	     "index=int", "value=any|function",
	     "Set nth-1 argument",
	     argumentMessage);

  getMethod(class, NAME_Arg, DEFAULT, "any|function", 1, "int",
	    "Nth-1 argument for term description",
	    getArgMessage);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity for term description",
	    getArityMessage);
  getMethod(class, NAME_argument, NAME_argument, "value=any|function", 1,
	    "index=int",
	    "Nth-1 argument",
	    getArgumentMessage);

  succeed;
}



