/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>

status
initialiseVmi(Vmi vmi, Name name)
{ initialiseProgramObject(vmi);
  assign(vmi, name, name);

  succeed;
}


static status
parentGoalVmi(Vmi vmi, Any receiver, Name selector)
{ if ( vmi == VmiSend )
    return parentGoal(VmiSend, receiver, selector);

  fail;					/* error */
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_parent[] =
	{ "receiver=unchecked", "selector=name" };

/* Instance Variables */

static vardecl var_vmi[] =
{ IV(NAME_name, "name", IV_GET,
     NAME_name, "Name of the vmi")
};

/* Send Methods */

static senddecl send_vmi[] =
{ SM(NAME_initialise, 1, "name=name", initialiseVmi,
     DEFAULT, "Create named vmi"),
  SM(NAME_parentGoal, 2, T_parent, parentGoalVmi,
     NAME_stack, "Test if operation is on goal-stack")
};

/* Get Methods */

#define get_vmi NULL
/*
static getdecl get_vmi[] =
{ 
};
*/

/* Resources */

#define rc_vmi NULL
/*
static classvardecl rc_vmi[] =
{ 
};
*/

/* Class Declaration */

static Name vmi_termnames[] = { NAME_name };

ClassDecl(vmi_decls,
          var_vmi, send_vmi, get_vmi, rc_vmi,
          1, vmi_termnames,
          "$Rev$");

status
makeClassVmi(Class class)
{ return declareClass(class, &vmi_decls);
}

