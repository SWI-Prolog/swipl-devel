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

