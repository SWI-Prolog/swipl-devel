/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Provide plwin.exe
*/

#include <windows.h>
#include <stdio.h>
#include "pl-itf.h"
#include "pl-stream.h"
#include <console.h>
#include <signal.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Main program for running SWI-Prolog from   a window. The window provides
X11-xterm like features: scrollback for a   predefined  number of lines,
cut/paste and the GNU readline library for command-line editing.

Basically, this module  combines   libpl.dll,  console.dll, readline.lib
with some glue to produce the final executable plwin.exe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	BIND STREAM STUFF	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First step: bind the  console  I/O   to  the  Sinput/Soutput  and Serror
streams.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
		 *	      SIGNALS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Capturing fatal signals doesn't appear to work   inside  a DLL, hence we
cpature them in the application and tell   Prolog to print the stack and
abort.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
fatalSignal(int sig)
{ char *name;

  switch(sig)
  { case SIGABRT:	name = "abort"; break;
    case SIGFPE:	name = "floating point exeception"; break;
    case SIGILL:	name = "illegal instruction"; break;
    case SIGSEGV:	name = "general protection fault"; break;
    default:		name = "(unknown)"; break;
  }

  PL_warning("Trapped signal %d (%s), aborting ...", sig, name);

  PL_action(PL_ACTION_BACKTRACE, (void *)10);
  signal(sig, fatalSignal);
  PL_action(PL_ACTION_ABORT, NULL);

}


static void
initSignals()
{ signal(SIGABRT, fatalSignal);
  signal(SIGFPE,  fatalSignal);
  signal(SIGILL,  fatalSignal);
  signal(SIGSEGV, fatalSignal);
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


PL_extension extensions[] =
{
/*{ "name",	arity,  function,	PL_FA_<flags> },*/

  { "window_title", 2,  pl_window_title, 0 },
  { NULL,	    0, 	NULL,		 0 }	/* terminating line */
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is called back from the   console.dll main loop to provide
the main for the application.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
win32main(int argc, char **argv, char **env)
{ set_window_title();
  rlc_bind_terminal();

  PL_register_extensions(extensions);
  if ( !PL_initialise(argc, argv, env) )
    PL_halt(1);
  
  PL_install_readline();
  PL_async_hook(4000, rlc_check_intr);
  rlc_interrupt_hook(PL_interrupt);
  initSignals();
  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
And this is the  real  application's  main   as  Windows  sees  it.  See
console.c for further details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int PASCAL
WinMain(HANDLE hInstance, HANDLE hPrevInstance,
	LPSTR lpszCmdLine, int nCmdShow)
{ return rlc_main(hInstance, hPrevInstance, lpszCmdLine, nCmdShow,
		  win32main, LoadIcon(hInstance, "SWI_Icon"));
}
