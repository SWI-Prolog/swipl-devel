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
initialiseBinding(Binding att, Any name, Any value)
{ assign(att, name, name);
  assign(att, value, value);

  setFlag(att, F_ISBINDING);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=name", "value=any|function" };

/* Instance Variables */

static vardecl locals_binding[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_argument, "Name of the binding"),
  IV(NAME_value, "any|function", IV_BOTH,
     NAME_value, "Value of the binding")
};

/* Send Methods */

static senddecl send_binding[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBinding,
     DEFAULT, "Create binding from name and value")
};

/* Get Methods */

#define get_binding NULL
/*
static getdecl get_binding[] =
{ 
};
*/

/* Resources */

#define rc_binding NULL
/*
static classvardecl rc_binding[] =
{ 
};
*/

/* Class Declaration */

static Name binding_termnames[] = { NAME_name, NAME_value };

ClassDecl(binding_decls,
          locals_binding, send_binding, get_binding, rc_binding,
          2, binding_termnames,
          "$Rev$");

status
makeClassBinding(Class class)
{ return declareClass(class, &binding_decls);
}

