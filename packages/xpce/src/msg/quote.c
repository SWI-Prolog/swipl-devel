/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>

static status
initialiseQuoteFunction(QuoteFunction q, Function f)
{ assign(q, function, f);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_quoteFunction[] =
{ IV(NAME_function, "function", IV_BOTH,
     NAME_storage, "Quoted function")
};

/* Send Methods */

static senddecl send_quoteFunction[] =
{ SM(NAME_initialise, 1, "function=function", initialiseQuoteFunction,
     DEFAULT, "Initialise")
};

/* Get Methods */

#define get_quoteFunction NULL
/*
static getdecl get_quoteFunction[] =
{ 
};
*/

/* Resources */

#define rc_quoteFunction NULL
/*
static classvardecl rc_quoteFunction[] =
{ 
};
*/

/* Class Declaration */

static Name quoteFunction_termnames[] = { NAME_function };

ClassDecl(quoteFunction_decls,
          var_quoteFunction, send_quoteFunction,
	  get_quoteFunction, rc_quoteFunction,
          1, quoteFunction_termnames,
          "$Rev$");

status
makeClassQuoteFunction(Class class)
{ declareClass(class, &quoteFunction_decls);
  delegateClass(class, NAME_function);

  succeed;
}

