/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdbool.h>
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

#if !SWIPL_EPILOG
#ifndef O_CTRLC
#define O_CTRLC 1
#endif
#ifndef O_ANSI_COLORS
#define O_ANSI_COLORS 1
#endif
#endif
#ifndef HAVE_WMAIN
#define HAVE_WMAIN 1
#endif

#else /* non-Windows version */

#include "SWI-Prolog.h"

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
#ifdef O_PLMT
      PL_w32thread_raise(main_thread_id, SIGINT);
#else
      PL_raise(SIGINT);
#endif
      return true;
  }

  return false;
}
#endif

		 /*******************************
		 *	      TCMALLOC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If  we  link  the  main   program    against   an  alternative  malloc()
implementation we better ensure the main   program  depends on malloc(),
otherwise the linker may still  decide   to  put  the alternative malloc
library further down in the  link   dependencies.  We should also ensure
this dependency is not optimized away.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void *_PL_tc_malloc_base;

static void
force_malloc_dependency(void)
{ _PL_tc_malloc_base = malloc(1);
}

		 /*******************************
		 *		MAIN		*
		 *******************************/

int
#if HAVE_WMAIN
#define PL_initialise PL_winitialise
wmain(int argc, wchar_t **argv)
#else
main(int argc, char **argv)
#endif
{
#if O_CTRLC
  main_thread_id = GetCurrentThreadId();
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)consoleHandlerRoutine, true);
#endif

#if O_ANSI_COLORS
  PL_w32_wrap_ansi_console();	/* decode ANSI color sequences (ESC[...m) */
#endif

  force_malloc_dependency();

#if SWIPL_EPILOG
  PL_set_prolog_flag("epilog", PL_BOOL, true);
#endif
  if ( !PL_initialise(argc, argv) )
    PL_halt(1);

  int status = PL_toplevel() ? 0 : 1;
  PL_halt(status);

  return status;
}


		 /*******************************
		 *   AddressSanitizer support   *
		 *******************************/

/* Clang way to detect address_sanitizer */
#ifndef __has_feature
  #define __has_feature(x) 0
#endif
#ifndef __SANITIZE_ADDRESS__
#if __has_feature(address_sanitizer)
#define __SANITIZE_ADDRESS__
#endif
#endif

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/lsan_interface.h>
#endif

#ifdef __SANITIZE_ADDRESS__
const char*
__asan_default_options()
{ return "detect_leaks=0";
}
#endif
