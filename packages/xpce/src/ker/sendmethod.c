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

#define INLINE_UTILITIES 1
#include <h/kernel.h>
#include <h/interface.h>
#include <itf/c.h>


SendMethod
createSendMethod(Name name, Vector types, StringObj doc, SendFunc action)
{ SendMethod m = alloc(sizeof(struct send_method));

  initHeaderObj(m, ClassSendMethod);
  createMethod((Method) m, name, types, doc, (Func) action);

  return m;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_send[] =
        { "receiver=object", "argument=unchecked ..." };

/* Instance Variables */

#define var_sendMethod NULL
/*
vardecl var_sendMethod[] =
{ 
};
*/

/* Send Methods */

static senddecl send_sendMethod[] =
{ SM(NAME_send, 2, T_send, sendSendMethod,
     NAME_execute, "Invoke send method on object")
};

/* Get Methods */

#define get_sendMethod NULL
/*
static getdecl get_sendMethod[] =
{ 
};
*/

/* Resources */

#define rc_sendMethod NULL
/*
static classvardecl rc_sendMethod[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(sendMethod_decls,
          var_sendMethod, send_sendMethod, get_sendMethod, rc_sendMethod,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassSendMethod(Class class)
{ declareClass(class, &sendMethod_decls);
					/* fix up bootClass stuff */
  assign(class, initialise_method,
	 getSendMethodClass(ClassMethod, NAME_initialise));

  succeed;
}
