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

  if ( TheCallbackFunctions.getHostContext )
  { Any context = (*TheCallbackFunctions.getHostContext)(receiver);

    assign(obt, context, context);
  }

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
  Name selector;
  Any rval = FAIL;
  void *savedcontext;

  if ( notNil(obt->context) &&
       TheCallbackFunctions.setHostContext )
  { savedcontext =
	(*TheCallbackFunctions.setHostContext)(obt->context);
  } else
    savedcontext = NIL;

  if ( !(receiver = expandCodeArgument(obt->receiver)) )
    goto out;
  if ( !(selector = checkSelector(obt->selector)) )
    goto out;

  if ( isNil(obt->arguments) )
  { rval = getv(receiver, selector, 0, NULL);
  } else
  { int n;
    int argc = valInt(obt->arguments->size);
    ArgVector(argv, argc);
    Any *elms = obt->arguments->elements;

    for(n = 0; n < argc; n++)
    { if ( !(argv[n] = expandCodeArgument(elms[n])) )
	goto out;
    }

    rval = getv(receiver, selector, argc, argv);
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

static char *T_initialise[] =
        { "receiver=object|function", "selector=name|function", "argument=any|function ..." };

/* Instance Variables */

static vardecl var_obtain[] =
{ IV(NAME_receiver, "object|function", IV_NONE,
     NAME_storage, "Receiver of the operation"),
  IV(NAME_selector, "name|function", IV_NONE,
     NAME_storage, "Name of the operation"),
  IV(NAME_arguments, "code_vector*", IV_NONE,
     NAME_storage, "Vector of arguments"),
  IV(NAME_Context, "any*", IV_GET,
     NAME_storage, "Host context information")
};

/* Send Methods */

static senddecl send_obtain[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseObtainv,
     DEFAULT, "Create from receiver, selector and args")
};

/* Get Methods */

static getdecl get_obtain[] =
{ GM(NAME_Arg, 1, "unchecked", "int", getArgObtain,
     DEFAULT, "Nth-1 argument of term instead of result's"),
  GM(NAME_Arity, 0, "int", NULL, getArityObtain,
     DEFAULT, "Arity of term instead of result's"),
  GM(NAME_Execute, 0, "unchecked", NULL, getExecuteObtain,
     DEFAULT, "Execute the get-operation")
};

/* Resources */

#define rc_obtain NULL
/*
static classvardecl rc_obtain[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(obtain_decls,
          var_obtain, send_obtain, get_obtain, rc_obtain,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassObtain(Class class)
{ return declareClass(class, &obtain_decls);
}

