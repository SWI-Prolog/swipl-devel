/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Windows (NT) specific stuff
*/

#include <windows.h>
#include "pl-itf.h"

int
main(int argc, char **argv, char **env)
{ if ( !PL_initialise(argc, argv, env) )
    PL_halt(1);
  
  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


