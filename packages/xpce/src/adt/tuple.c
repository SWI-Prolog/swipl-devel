/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


status
initialiseTuple(Tuple t, Any first, Any second)
{ assign(t, first, first);
  assign(t, second, second);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "first=any", "second=any" };

/* Instance Variables */

static vardecl var_tuple[] =
{ IV(NAME_first, "any", IV_BOTH,
     NAME_storage, "First of the tuple"),
  IV(NAME_second, "any", IV_BOTH,
     NAME_storage, "Second of the tuple")
};

/* Send Methods */

static senddecl send_tuple[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseTuple,
     DEFAULT, "Create tuple from first and second")
};

/* Get Methods */

#define get_tuple NULL
/*
static getdecl get_tuple[] =
{ 
};
*/

/* Resources */

#define rc_tuple NULL
/*
static classvardecl rc_tuple[] =
{ 
};
*/

/* Class Declaration */

static Name tuple_termnames[] = { NAME_first, NAME_second };

ClassDecl(tuple_decls,
          var_tuple, send_tuple, get_tuple, rc_tuple,
          2, tuple_termnames,
          "$Rev$");


status
makeClassTuple(Class class)
{ return declareClass(class, &tuple_decls);
}

