/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam, VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <winsock2.h>
#include <windows.h>
#include "SWI-Stream.h"
#include "SWI-Prolog.h"
#include <signal.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the simple main  program   of  swipl.exe; the SWI-Prolog console
application. It can be used as   a  basis for console-based applications
that have SWI-Prolog embedded.

The default version does Control-C  processing   and  decodes ANSI color
sequences to support colors in the console window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef O_CTRLC
#define O_CTRLC 1
#endif
#ifndef O_ANSI_COLORS
#define O_ANSI_COLORS 1
#endif

		 /*******************************
		 *	     INTERRUPT		*
		 *******************************/

#if O_CTRLC
static DWORD main_thread_id;

static BOOL
consoleHandlerRoutine(DWORD id)
{ switch(id)
  { case CTRL_C_EVENT:
      PL_w32thread_raise(main_thread_id, SIGINT);
      return TRUE;
  }

  return FALSE;
}
#endif


		 /*******************************
		 *	       MAIN		*
		 *******************************/

int
main(int argc, char **argv)
{
#if O_CTRLC
  main_thread_id = GetCurrentThreadId();
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)consoleHandlerRoutine, TRUE);
#endif

#if O_ANSI_COLORS
  PL_w32_wrap_ansi_console();		/* decode ANSI color sequences (ESC[...m) */
#endif

  if ( !PL_initialise(argc, argv) )
    PL_halt(1);

  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


