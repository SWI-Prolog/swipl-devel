/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include "md.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <h/interface.h>

extern int pceInitApplication(int argc, char **argv);

int
main(int argc, char* argv[])
{ if ( !pceInitialise(0, argc, argv) )
  { Cprintf("Sorry, failed to initialise XPCE\n");
    exit(1);
  }
  
  if ( !pceInitApplication(argc, argv) )
  { Cprintf("Failed to run pceInitApplication()\n");
    exit(1);
  }

  for(;;)
    pceDispatch(0, 1000);
}
