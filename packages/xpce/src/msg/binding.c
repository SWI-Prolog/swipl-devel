/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
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

static const char *T_initialise[] =
        { "name=name", "value=any|function" };

/* Instance Variables */

static const vardecl locals_binding[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_argument, "Name of the binding"),
  IV(NAME_value, "any|function", IV_BOTH,
     NAME_value, "Value of the binding")
};

/* Send Methods */

static const senddecl send_binding[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBinding,
     DEFAULT, "Create binding from name and value")
};

/* Get Methods */

static const getdecl get_binding[] =
{ 
};

/* Resources */

static const resourcedecl rc_binding[] =
{ 
};

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

