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
initialiseWhile(While w, Code cond, Code body)
{ initialiseCode((Code) w);

  if ( isDefault(body) )
    body = NIL;

  assign(w, condition, cond);
  assign(w, body,      body);

  succeed;
}


static status
ExecuteWhile(While w)
{ while ( executeCode(w->condition) )
  { if ( notNil(w->body) )
    { TRY( executeCode(w->body) );
    }
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "condition=code", "statement=[code]*" };

/* Instance Variables */

static vardecl var_while[] =
{ IV(NAME_condition, "code", IV_BOTH,
     NAME_statement, "Condition to be tested"),
  IV(NAME_body, "code*", IV_BOTH,
     NAME_statement, "Statement to execute")
};

/* Send Methods */

static senddecl send_while[] =
{ SM(NAME_Execute, 0, NULL, ExecuteWhile,
     DEFAULT, "Execute body until test fails"),
  SM(NAME_initialise, 2, T_initialise, initialiseWhile,
     DEFAULT, "Create from condition and statement")
};

/* Get Methods */

#define get_while NULL
/*
static getdecl get_while[] =
{ 
};
*/

/* Resources */

#define rc_while NULL
/*
static classvardecl rc_while[] =
{ 
};
*/

/* Class Declaration */

static Name while_termnames[] = { NAME_condition, NAME_body };

ClassDecl(while_decls,
          var_while, send_while, get_while, rc_while,
          2, while_termnames,
          "$Rev$");

status
makeClassWhile(Class class)
{ return declareClass(class, &while_decls);
}



