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

