/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Call.h>
#include <stdlib.h>

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
