/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Windows (NT) specific stuff
*/

#include <windows.h>
#include "pl-itf.h"

#if 0
#include <signal.h>

static BOOL
consoleHandlerRoutine(DWORD id)
{ switch(id)
  { case CTRL_C_EVENT:
      raise(SIGINT);
      return TRUE;
  }
  
  return FALSE;
}
#endif

int
main(int argc, char **argv)
{
#if 0
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)consoleHandlerRoutine, TRUE);
#endif

  if ( !PL_initialise(argc, argv) )
    PL_halt(1);
  
  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


