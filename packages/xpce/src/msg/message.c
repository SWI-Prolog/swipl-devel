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
{ assign(msg, receiver,  rec);
  assign(msg, selector,  sel);
  assign(msg, arg_count, toInt(argc));

  switch(argc)
  { case 0:
      break;
    case 1:
      assign(msg, arguments, argv[0]);
      break;
    default:
      assign(msg, arguments, newObjectv(ClassCodeVector, argc, argv));
      break;
  }

  return initialiseCode((Code) msg);
}


static Int
getArityMessage(Message msg)
{ answer(add(TWO, msg->arg_count));
}


static Any
getArgMessage(Message msg, Int arg)
{ int n = valInt(arg);

  switch(n)
  { case 1:	answer(msg->receiver);
    case 2:	answer((Any) msg->selector);
    default:	if ( n < 1 || n > valInt(getArityMessage(msg)) )
		  fail;
    		if ( msg->arg_count == ONE )
		  answer(msg->arguments);
		else
		  answer(msg->arguments->elements[n-3]);
  }
}


static status
argumentMessage(Message msg, Int n, Any val)
{ int i = valInt(n);

  if ( i < 1 || i > valInt(getArityMessage(msg)) )
    fail;

  if ( msg->arg_count == ONE )
    assign(msg, arguments, val);

  return elementVector(msg->arguments, n, val);
}


static Any
getArgumentMessage(Message msg, Int n)
{ int i = valInt(n);

  if ( i < 1 || i > valInt(getArityMessage(msg)) )
    fail;

  if ( msg->arg_count == ONE )
    answer(msg->arguments);

   answer(msg->arguments->elements[i-1]);
}


status
ExecuteMessage(Message msg)
{ Any receiver;

  TRY(receiver = expandCodeArgument(msg->receiver));

  if ( msg->arg_count == ZERO )
  { return sendv(receiver, msg->selector, 0, NULL);
  } else if ( msg->arg_count == ONE )
  { Any arg;

    TRY(arg = expandCodeArgument(msg->arguments));

    return sendv(receiver, msg->selector, 1, &arg);
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_argument[] =
        { "index=int", "value=any|function" };
static const char *T_initialise[] =
        { "receiver=object|function", "selector=name|function",
	  "argument=any|function ..." };

/* Instance Variables */

static const vardecl var_message[] =
{ IV(NAME_receiver, "object|function", IV_BOTH,
     NAME_storage, "Receiver of the operation"),
  IV(NAME_selector, "name|function", IV_BOTH,
     NAME_storage, "Name of the operation"),
  IV(NAME_argCount, "0..", IV_GET,
     NAME_storage, "Argument-count"),
  IV(NAME_arguments, "code_vector|any|function*", IV_GET,
     NAME_storage, "Vector of arguments")
};

/* Send Methods */

static const senddecl send_message[] =
{ SM(NAME_Execute, 0, NULL, ExecuteMessage,
     DEFAULT, "Send the message"),
  SM(NAME_initialise, 3, T_initialise, initialiseMessagev,
     DEFAULT, "Create from receiver, selector and args"),
  SM(NAME_argument, 2, T_argument, argumentMessage,
     NAME_argument, "Set nth-1 argument")
};

/* Get Methods */

static const getdecl get_message[] =
{ GM(NAME_Arg, 1, "any|function", "int", getArgMessage,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityMessage,
     DEFAULT, "Arity for term description"),
  GM(NAME_argument, 1, "value=any|function", "index=int", getArgumentMessage,
     NAME_argument, "Nth-1 argument")
};

/* Resources */

static const resourcedecl rc_message[] =
{ 
};

/* Class Declaration */

ClassDecl(message_decls,
          var_message, send_message, get_message, rc_message,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassMessage(Class class)
{ return declareClass(class, &message_decls);
}
