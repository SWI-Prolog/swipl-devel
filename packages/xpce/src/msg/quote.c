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

