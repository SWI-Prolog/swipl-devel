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
initialiseWhen(When w, Code cond, Function when_true, Function when_false)
{ initialiseFunction((Function) w);

  assign(w, condition, cond);
  assign(w, then_branch, when_true);
  assign(w, else_branch, when_false);

  succeed;
}


static Any
getExecuteWhen(When w)
{ if ( executeCode(w->condition) )
    return expandCodeArgument(w->then_branch);
  else
    return expandCodeArgument(w->else_branch);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "condition=code", "then=any|function", "else=any|function" };

/* Instance Variables */

static vardecl var_when[] =
{ IV(NAME_condition, "code", IV_BOTH,
     NAME_statement, "Condition to be tested"),
  IV(NAME_then, "any|function", IV_BOTH,
     NAME_statement, "Executed if condition is true"),
  IV(NAME_else, "any|function", IV_BOTH,
     NAME_statement, "Executed if condition is false")
};

/* Send Methods */

static senddecl send_when[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseWhen,
     DEFAULT, "Create from condition, when- and else"),
  SM(NAME_unlink, 0, NULL, succeedObject,
     DEFAULT, "temp; just to trap")
};

/* Get Methods */

static getdecl get_when[] =
{ GM(NAME_Execute, 0, "unchecked", NULL, getExecuteWhen,
     DEFAULT, "Test condition and evaluate <-then or <-else")
};

/* Resources */

#define rc_when NULL
/*
static classvardecl rc_when[] =
{ 
};
*/

/* Class Declaration */

static Name when_termnames[] = { NAME_condition, NAME_then, NAME_else };

ClassDecl(when_decls,
          var_when, send_when, get_when, rc_when,
          3, when_termnames,
          "$Rev$");

status
makeClassWhen(Class class)
{ return declareClass(class, &when_decls);
}


