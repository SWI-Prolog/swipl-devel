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

  if ( TheCallbackFunctions.getHostContext )
  { Any context = (*TheCallbackFunctions.getHostContext)(rec);

    assign(msg, context, context);
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
  Any selector;
  Any savedcontext;
  status rval = FAIL;

  if ( notNil(msg->context) &&
       TheCallbackFunctions.setHostContext )
  { savedcontext =
	(*TheCallbackFunctions.setHostContext)(msg->context);
  } else
    savedcontext = NIL;

  if ( !(receiver = expandCodeArgument(msg->receiver)) )
    goto out;
  if ( !(selector = checkSelector(msg->selector)) )
    goto out;

  if ( msg->arg_count == ZERO )
  { return sendv(receiver, selector, 0, NULL);
  } else if ( msg->arg_count == ONE )
  { Any arg;

    if ( !(arg = expandCodeArgument(msg->arguments)) )
      goto out;

    rval = sendv(receiver, selector, 1, &arg);
  } else
  { int n;
    int argc = valInt(msg->arguments->size);
    ArgVector(argv, argc);
    Any *elms = msg->arguments->elements;
    Any *av = argv;

    for(n = argc; --n >=0 ; )
    { if ( !(*av++ = expandCodeArgument(*elms++)) )
	goto out;
    }

    rval = sendv(receiver, selector, argc, argv);
  }

out:
  if ( notNil(savedcontext) )
    (*TheCallbackFunctions.setHostContext)(savedcontext);

  return rval;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_argument[] =
        { "index=int", "value=any|function" };
static char *T_initialise[] =
        { "receiver=object|function", "selector=name|function",
	  "argument=any|function ..." };

/* Instance Variables */

static vardecl var_message[] =
{ IV(NAME_receiver, "object|function", IV_BOTH,
     NAME_storage, "Receiver of the operation"),
  IV(NAME_selector, "name|function", IV_BOTH,
     NAME_storage, "Name of the operation"),
  IV(NAME_argCount, "0..", IV_GET,
     NAME_storage, "Argument-count"),
  IV(NAME_arguments, "code_vector|any|function*", IV_GET,
     NAME_storage, "Vector of arguments"),
  IV(NAME_context, "any*", IV_GET,
     NAME_storage, "Host context information")
};

/* Send Methods */

static senddecl send_message[] =
{ SM(NAME_Execute, 0, NULL, ExecuteMessage,
     DEFAULT, "Send the message"),
  SM(NAME_initialise, 3, T_initialise, initialiseMessagev,
     DEFAULT, "Create from receiver, selector and args"),
  SM(NAME_argument, 2, T_argument, argumentMessage,
     NAME_argument, "Set nth-1 argument")
};

/* Get Methods */

static getdecl get_message[] =
{ GM(NAME_Arg, 1, "any|function", "int", getArgMessage,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityMessage,
     DEFAULT, "Arity for term description"),
  GM(NAME_argument, 1, "value=any|function", "index=int", getArgumentMessage,
     NAME_argument, "Nth-1 argument")
};

/* Resources */

#define rc_message NULL
/*
static classvardecl rc_message[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(message_decls,
          var_message, send_message, get_message, rc_message,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassMessage(Class class)
{ return declareClass(class, &message_decls);
}
