/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <stdlib.h>
#include <pce/Pce.h>
#include <pce/Chain.h>
#include <pce/Directory.h>
#include <pce/Message.h>


PceStatus
pceInitApplication(int argc, char *argv[])
{ int i;

  for(i=1; i < argc; i++)
  { PceDirectory d(argv[i]);

    PceChain ch = AsChain(d.get("directories"));
    ch.send("for_all", PceMessage(ThePce, "write_ln", TheArg1));
  }

  ThePce.send("die");
}
