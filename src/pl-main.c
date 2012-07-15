/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <stdio.h>
#ifndef __WINDOWS__
#if defined(_MSC_VER) || defined(__MINGW32__)
#define __WINDOWS__ 1
#endif
#endif

#ifdef __WINDOWS__
#include <winsock2.h>
#include <windows.h>
#include "os/SWI-Stream.h"
#include "SWI-Prolog.h"
#include <signal.h>

#ifndef O_CTRLC
#define O_CTRLC 1
#endif
#ifndef O_ANSI_COLORS
#define O_ANSI_COLORS 1
#endif

#else /* non-Windows version */

#include "SWI-Prolog.h"

#define READLINE 1			/* use readline interface */

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is SWI-Prolog's main(),  creating   swipl  or  swipl.exe (Windows).
SWI-Prolog itself is in  the   library  libswipl.{a,so,dll,...}, this is
merely a main() routine that sets up I/O and uses SWI-Prolog's embedding
interface to get the system going.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
		 *	     READLINE		*
		 *******************************/

#ifdef READLINE
static void
install_readline(int argc, char**argv)
{ PL_install_readline();
}
#endif


		 /*******************************
		 *		MAIN		*
		 *******************************/


int
main(int argc, char **argv)
{
#if O_CTRLC
  main_thread_id = GetCurrentThreadId();
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)consoleHandlerRoutine, TRUE);
#endif

#if O_ANSI_COLORS
  PL_w32_wrap_ansi_console();	/* decode ANSI color sequences (ESC[...m) */
#endif
#ifdef READLINE
  PL_initialise_hook(install_readline);
#endif

  if ( !PL_initialise(argc, argv) )
    PL_halt(1);

  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


