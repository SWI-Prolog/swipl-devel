/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Windows (NT) specific stuff
*/

#include <windows.h>
#define PL_KERNEL 1
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


static int
readkey(int fd)
{ int chr = getkey();

  return chr == 04 ? EOF : chr;
}



		 /*******************************
		 *	       MAIN		*
		 *******************************/


int
win32main(int argc, char **argv, char **env)
{ if ( !PL_initialise(argc, argv, env) )
    PL_halt(1);

  PL_async_hook(4000, rlc_check_intr);
  rlc_interrupt_hook(PL_interrupt);
  PL_getkey_hook(readkey);
  { char title[256];

    Ssprintf(title, "SWI-Prolog (version %s)", PLVERSION);
    rlc_title(title, NULL, 0);
  }
  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


int PASCAL
WinMain(HANDLE hInstance, HANDLE hPrevInstance,
	LPSTR lpszCmdLine, int nCmdShow)
{ rlc_bind_terminal();

  return rlc_main(hInstance, hPrevInstance, lpszCmdLine, nCmdShow,
		  win32main, LoadIcon(hInstance, "SWI_Icon"));
}
