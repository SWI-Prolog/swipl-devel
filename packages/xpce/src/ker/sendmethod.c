/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
