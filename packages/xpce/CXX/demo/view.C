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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a very simple example.  Typing   ./view  file ... will create as
many `view' objects as you entered files   on  the commandline. The done
message construct shows how a C++ function   is called from an XPCE code
object.  If all views are gone, the application, exists.  To use it:

	% make view
	% export LD_LIBRARY_PATH=../../<arch>
	% export PCEHOME=../..
	% ./view *.C

Type Control-C to stop the application if you   do not want to close all
views created.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
