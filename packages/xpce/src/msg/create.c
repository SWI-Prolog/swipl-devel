/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


static status
initialiseCreatev(Create c, Class class, int argc, const Any argv[])
{ initialiseFunction((Function) c);

  assign(c, c_class, class);
  if ( argc > 0 )
    assign(c, arguments, newObjectv(ClassCodeVector, argc, argv));

  succeed;
}


static Int
getArityCreate(Create c)
{ if ( isNil(c->arguments) )
    answer(ONE);
  else
    answer(add(c->arguments->size, ONE));
}


static Any
getArgCreate(Create c, Int arg)
{ int n = valInt(arg);

  switch(n)
  { case 1:	answer(c->c_class);
    default:	if ( n < 1 || n > valInt(getArityCreate(c)) )
		  fail;
                answer(c->arguments->elements[n-2]);
  }
}


static status
argumentCreate(Create c, Int n, Any val)
{ if ( valInt(n) < 1 )
    fail;

  if ( isNil(c->arguments) )
    assign(c, arguments, newObject(ClassVector, 0));

  return elementVector(c->arguments, n, val);
}


static Any
getArgumentCreate(Create c, Int n)
{ if ( valInt(n) < 1 )
    fail;

  if ( isNil(c->arguments) )
    fail;

  return getElementVector(c->arguments, n);
}


static Any
getExecuteCreate(Create c)
{ if ( !instanceOfObject(c->c_class, ClassClass) )
  { Class class;

    if ( !(class = getConvertClass(ClassClass, c->c_class)) )
    { errorPce(c, NAME_noClass, class);
      fail;
    }
    assign(c, c_class, class);
  }

  if ( isNil(c->arguments) )
    return answerObjectv(c->c_class, 0, NULL);
  else
  { int n;
    int argc = valInt(c->arguments->size);
    ArgVector(argv, argc);
    Any *elms = c->arguments->elements;

    for(n = 0; n < argc; n++)
      TRY(argv[n] = expandCodeArgument(elms[n]));

    return answerObjectv(c->c_class, argc, argv);
  }
}


status
makeClassCreate(Class class)
{ sourceClass(class, makeClassCreate, __FILE__, "$Revision$");

  localClass(class, NAME_class, NAME_class, "name|class", NAME_both,
	     "Class (name) to create instance of");
  localClass(class, NAME_argument, NAME_argument, "code_vector*", NAME_both,
	     "Arguments used to create instance");

  termClass(class, "create", ARGC_UNKNOWN);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "class=name|class", "argument=any|function ...",
	     "Create from class (name) and arguments",
	     initialiseCreatev);
  sendMethod(class, NAME_argument, NAME_argument, 2,
	     "index=int", "value=any|function",
	     "Set nth-1 argument",
	     argumentCreate);

  getMethod(class, NAME_Arg, DEFAULT, "any|function", 1, "int",
	    "Nth-1 argument for term description",
	    getArgCreate);
  getMethod(class, NAME_Arity, DEFAULT, "int", 0,
	    "Arity for term description",
	    getArityCreate);
  getMethod(class, NAME_argument, NAME_argument, "value=any|function", 1,
	    "index=int",
	    "Nth-1 argument",
	    getArgumentCreate);

  getMethod(class, NAME_Execute, DEFAULT, "unchecked", 0,
	     "Create instance and return it",
	     getExecuteCreate);

  succeed;
}


