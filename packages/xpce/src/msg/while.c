/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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
static resourcedecl rc_while[] =
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



