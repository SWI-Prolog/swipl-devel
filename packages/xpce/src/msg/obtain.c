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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_initialise[] =
        { "receiver=object|function", "selector=name|function", "argument=any|function ..." };

/* Instance Variables */

static const vardecl var_obtain[] =
{ IV(NAME_receiver, "object|function", IV_NONE,
     NAME_storage, "Receiver of the operation"),
  IV(NAME_selector, "name|function", IV_NONE,
     NAME_storage, "Name of the operation"),
  IV(NAME_arguments, "code_vector*", IV_NONE,
     NAME_storage, "Vector of arguments")
};

/* Send Methods */

static const senddecl send_obtain[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseObtainv,
     DEFAULT, "Create from receiver, selector and args")
};

/* Get Methods */

static const getdecl get_obtain[] =
{ GM(NAME_Arg, 1, "unchecked", "int", getArgObtain,
     DEFAULT, "Nth-1 argument of term instead of result's"),
  GM(NAME_Arity, 0, "int", NULL, getArityObtain,
     DEFAULT, "Arity of term instead of result's"),
  GM(NAME_Execute, 0, "unchecked", NULL, getExecuteObtain,
     DEFAULT, "Execute the get-operation")
};

/* Resources */

static const resourcedecl rc_obtain[] =
{ 
};

/* Class Declaration */

ClassDecl(obtain_decls,
          var_obtain, send_obtain, get_obtain, rc_obtain,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassObtain(Class class)
{ return declareClass(class, &obtain_decls);
}

