/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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

static const char *T_initialise[] =
        { "name=name", "context=[class|object*]" };

/* Instance Variables */

static const vardecl var_behaviour[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Selector of this behaviour"),
  IV(NAME_context, "class|object*", IV_GET,
     NAME_whole, "Definition context of this method")
};

/* Send Methods */

static const senddecl send_behaviour[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBehaviour,
     DEFAULT, "Create from <-name and <-context")
};

/* Get Methods */

static const getdecl get_behaviour[] =
{ 
};

/* Resources */

static const resourcedecl rc_behaviour[] =
{ 
};

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

