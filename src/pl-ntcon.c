/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
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


