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


status
makeClassWhile(Class class)
{ sourceClass(class, makeClassWhile, __FILE__, "$Revision$");

  localClass(class, NAME_condition, NAME_statement, "code", NAME_both,
	     "Condition to be tested");
  localClass(class, NAME_body, NAME_statement, "code*", NAME_both,
	     "Statement to execute");

  termClass(class, "while", 2, NAME_condition, NAME_body);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "condition=code", "statement=[code]*",
	     "Create from condition and statement",
	     initialiseWhile);
  sendMethod(class, NAME_Execute, DEFAULT, 0,
	     "Execute body until test fails",
	     ExecuteWhile);

  succeed;
}



