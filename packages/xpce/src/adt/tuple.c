/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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

static const char *T_initialise[] =
        { "first=any", "second=any" };

/* Instance Variables */

static const vardecl var_tuple[] =
{ IV(NAME_first, "any", IV_BOTH,
     NAME_storage, "First of the tuple"),
  IV(NAME_second, "any", IV_BOTH,
     NAME_storage, "Second of the tuple")
};

/* Send Methods */

static const senddecl send_tuple[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseTuple,
     DEFAULT, "Create tuple from first and second")
};

/* Get Methods */

static const getdecl get_tuple[] =
{ 
};

/* Resources */

static const resourcedecl rc_tuple[] =
{ 
};

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

