/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Windows (NT) specific stuff
*/

#include <windows.h>
#ifndef PL_WIN
#define PL_KERNEL 1
#endif
#include <stdio.h>
#include "pl-itf.h"
#include "pl-stream.h"
#include <console.h>


		 /*******************************
		 *	BIND STREAM STUFF	*
		 *******************************/

static int
Srlc_read(void *handle, char *buffer, int size)
{ return rlc_read(buffer, size);
}


static int
Srlc_write(void *handle, char *buffer, int size)
{ return rlc_write(buffer, size);
}


static void
rlc_bind_terminal()
{ static IOFUNCTIONS funcs;

  funcs = *Sinput->functions;
  funcs.read     = Srlc_read;
  funcs.write    = Srlc_write;

  Sinput->functions  = &funcs;
  Soutput->functions = &funcs;
  Serror->functions  = &funcs;
}


		 /*******************************
		 *	   CONSOLE STUFF	*
		 *******************************/

foreign_t
pl_window_title(term_t old, term_t new)
{ char buf[256];
  char *n;

  if ( !PL_get_atom_chars(new, &n) )
    return PL_warning("window_title/2: instantiation fault");

  rlc_title(n, buf, sizeof(buf));

  return PL_unify_atom_chars(old, buf);
}

		 /*******************************
		 *	       MAIN		*
		 *******************************/


static void
set_window_title()
{ char title[256];

  Ssprintf(title, "SWI-Prolog (version %s)", PLVERSION);
  rlc_title(title, NULL, 0);
}

IOSTREAM *S__iob;

int
win32main(int argc, char **argv, char **env)
{ S__iob = S__getiob();
  rlc_bind_terminal();

  if ( !PL_initialise(argc, argv, env) )
    PL_halt(1);
  
  PL_async_hook(4000, rlc_check_intr);
  rlc_interrupt_hook(PL_interrupt);
  set_window_title();
  PL_register_foreign("window_title", 2, pl_window_title, 0);
  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


int PASCAL
WinMain(HANDLE hInstance, HANDLE hPrevInstance,
	LPSTR lpszCmdLine, int nCmdShow)
{ return rlc_main(hInstance, hPrevInstance, lpszCmdLine, nCmdShow,
		  win32main, LoadIcon(hInstance, "SWI_Icon"));
}
