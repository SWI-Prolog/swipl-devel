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
initialiseAssignment(Assignment b, Var var, Any value, Name scope)
{ initialiseCode((Code) b);

  if ( isDefault(scope) )
    scope = NAME_local;

  assign(b, var,   var);
  assign(b, value, value);
  assign(b, scope, scope);

  succeed;
}


static status
ExecuteAssignment(Assignment b)
{ Any val;

  TRY(val = expandCodeArgument(b->value));
  return assignVar(b->var, val, b->scope);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "variable=var", "value=any|function", "scope=[{local,outer,global}]" };

/* Instance Variables */

static vardecl var_assign[] =
{ IV(NAME_var, "var", IV_BOTH,
     NAME_storage, "Variable to be bound"),
  IV(NAME_value, "any|function", IV_BOTH,
     NAME_storage, "Value to give to the assignment"),
  IV(NAME_scope, "{local,outer,global}", IV_BOTH,
     NAME_scope, "Scope of assignment")
};

/* Send Methods */

static senddecl send_assign[] =
{ SM(NAME_Execute, 0, NULL, ExecuteAssignment,
     DEFAULT, "Bind the variable"),
  SM(NAME_initialise, 3, T_initialise, initialiseAssignment,
     DEFAULT, "Create assignment from var and value")
};

/* Get Methods */

#define get_assign NULL
/*
static getdecl get_assign[] =
{ 
};
*/

/* Resources */

#define rc_assign NULL
/*
static classvardecl rc_assign[] =
{ 
};
*/

/* Class Declaration */

static Name assign_termnames[] = { NAME_var, NAME_value, NAME_scope };

ClassDecl(assign_decls,
          var_assign, send_assign, get_assign, rc_assign,
          3, assign_termnames,
          "$Rev$");

status
makeClassAssign(Class class)
{ return declareClass(class, &assign_decls);
}

