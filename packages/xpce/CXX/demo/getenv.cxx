/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Call.h>
#include <stdlib.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Very tricky demo to illustrating how a C++  function may be wrapped in a
XPCE function object. It creates the XPCE object @getenv, that evaluates
to the value of @arg1 in the environment.

You can test it like this:

	% make getenv.so
	% xpce
	?- load_foreign_library(getenv).
	?- get(@getenv, '_forward', 'HOME', Home).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PceArg
PceGetenv(PceArg name)
{ char *s = getenv(name);

  if ( s )
    return s;
  else
    return FAIL;
}


PceStatus
pceInitApplication(int argc, char *argv[])
{ PceFuncall f(PceGetenv, TheArg1);

  f.send("_name_reference", "getenv");  // name the object @getenv

  return SUCCEED;
}
