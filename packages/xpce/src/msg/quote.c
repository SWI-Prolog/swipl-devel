/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>

static status
initialiseQuoteFunction(QuoteFunction q, Function f)
{ assign(q, function, f);

  succeed;
}


status
makeClassQuoteFunction(Class class)
{ sourceClass(class, makeClassQuoteFunction, __FILE__, "$Revision$");

  localClass(class, NAME_function, NAME_storage, "function", NAME_both,
	     "Quoted function");

  termClass(class, "quote_function", 1, NAME_function);
  delegateClass(class, NAME_function);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "function=function",
	     "Initialise",
	     initialiseQuoteFunction);

  succeed;
}

