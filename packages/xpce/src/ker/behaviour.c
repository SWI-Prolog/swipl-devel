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
initialiseBehaviour(Behaviour b, Name name, Any ctx)
{ initialiseProgramObject(b);

  if ( isDefault(ctx) )
    ctx = NIL;

  assign(b, name, name);
  assign(b, context, ctx);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=name", "context=[class|object*]" };

/* Instance Variables */

static vardecl var_behaviour[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Selector of this behaviour"),
  IV(NAME_context, "class|object*", IV_GET,
     NAME_whole, "Definition context of this method")
};

/* Send Methods */

static senddecl send_behaviour[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBehaviour,
     DEFAULT, "Create from <-name and <-context")
};

/* Get Methods */

#define get_behaviour NULL
/*
static getdecl get_behaviour[] =
{ 
};
*/

/* Resources */

#define rc_behaviour NULL
/*
static classvardecl rc_behaviour[] =
{ 
};
*/

/* Class Declaration */

static Name behaviour_termnames[] = { NAME_name, NAME_context };

ClassDecl(behaviour_decls,
          var_behaviour, send_behaviour, get_behaviour, rc_behaviour,
          2, behaviour_termnames,
          "$Rev$");


status
makeClassBehaviour(Class class)
{ declareClass(class, &behaviour_decls);
  cloneStyleVariableClass(class, NAME_context, NAME_reference);

  succeed;
}

