/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


static status
initialiseNotv(Not n, Code arg)
{ initialiseCode((Code) n);
  assign(n, argument, arg);

  succeed;
}


		/********************************
		*           EXECUTION		*
		********************************/

static status
ExecuteNot(Not n)
{ if ( executeCode(n->argument) != FAIL )
    fail;

  succeed;
}


status
makeClassNot(Class class)
{ sourceClass(class, makeClassNot, __FILE__, "$Revision$");

  localClass(class, NAME_argument, NAME_statement, "code", NAME_both,
	     "Test to negate");

  termClass(class, "not", 1, NAME_argument);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "test=code",
	     "Create from test",
	     initialiseNotv);
  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Evaluate argument test and negate result",
	     ExecuteNot);

  succeed;
}

