/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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
static resourcedecl rc_when[] =
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


