/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <stdlib.h>
#include <pce/Pce.h>
#include <pce/Call.h>
#include <pce/View.h>


PceStatus
done(PceArg v)
{ PceArg cl = v.get("class");

  v.send("destroy");
  if ( cl.get("no_created") == cl.get("no_freed") )
    exit(0);

  return SUCCEED;
}


PceStatus
view(char *file)
{ PceView v(file);

  v.send("load", file);
  v.send("done_message", PceCall(done, v));
  v.send("open");

  return SUCCEED;
}


PceStatus
pceInitApplication(int argc, char *argv[])
{ int i;

  for( i = 1; i < argc; i++ )
    view(argv[i]);

  return SUCCEED;
}
