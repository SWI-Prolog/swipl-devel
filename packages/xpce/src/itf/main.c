/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "md.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <h/interface.h>

extern int pceInitApplication(int argc, char **argv);

int
main(int argc, char* argv[])
{ if ( !pceInitialise(0, NULL, argc, argv) )
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
