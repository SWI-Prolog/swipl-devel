/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

#include <windows.h>
#include <stdio.h>
#include "pl-itf.h"
#include "pl-stream.h"
#include <ctype.h>
#include <console.h>
#include <signal.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Main program for running SWI-Prolog from   a window. The window provides
X11-xterm like features: scrollback for a   predefined  number of lines,
cut/paste and the GNU readline library for command-line editing.

Basically, this module combines libpl.dll and console.dll with some glue
to produce the final executable plwin.exe.
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
{ int fd = (int) handle;
  int ttymode = PL_ttymode(fd);

  if ( ttymode == PL_RAWTTY )
  { int chr = getkey();
      
    if ( chr == 04 || chr == 26 || chr == -1 )
      return 0;			/* EOF */

    buffer[0] = chr & 0xff;
    return 1;
  }

  return rlc_read(buffer, size);
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


#ifndef HAVE_LIBREADLINE

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: actually rlc_add_history() removes  duplicates   and  empty lines.
Also, read_line() already updates the history.   Maybe  this should just
return TRUE? This however would not allow for programmatically inserting
things in the history. This shouldn't matter.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_rl_add_history(term_t text)
{ atom_t a;
  static atom_t last = 0;

  if ( PL_get_atom(text, &a) )
  { if ( a != last )
    { if ( last )
	PL_unregister_atom(last);
      last = a;
      PL_register_atom(last);

      rlc_add_history(PL_atom_chars(a));
    }

    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_rl_read_init_file(term_t file)
{ PL_succeed;
}


		 /*******************************
		 *	    COMPLETION		*
		 *******************************/

static RlcCompleteFunc file_completer;

static int
prolog_complete(RlcCompleteData data)
{ Line ln = data->line;

  switch(data->call_type)
  { case COMPLETE_INIT:
    { int start = ln->point;
      int c;

      if ( !ln->data )			/* we donot want to complete on all atoms */
	return FALSE;

      while(start > 0 && (isalnum((c=ln->data[start-1])) || c == '_') )
	start--;
      if ( start > 0 )
      { int cs = ln->data[start-1];
	
	if ( strchr("'/\\.~", cs) )
	  return FALSE;			/* treat as a filename */
      }
      if ( islower(ln->data[start]) )	/* Lower, Aplha ...: an atom */
      { int patlen = ln->point - start;
	char *s;

	memcpy(data->buf_handle, &ln->data[start], patlen);
	data->buf_handle[patlen] = '\0';
	
	if ( (s = PL_atom_generator(data->buf_handle, FALSE)) )
	{ strcpy(data->candidate, s);
	  data->replace_from = start;
	  data->function = prolog_complete;
	  return TRUE;
	}
      }

      return FALSE;
    }
    case COMPLETE_ENUMERATE:
    { char *s = PL_atom_generator(data->buf_handle, TRUE);
      if ( s )
      { strcpy(data->candidate, s);
	return TRUE;
      }
      return FALSE;
    }
    case COMPLETE_CLOSE:
      return TRUE;
    default:
      return FALSE;
  }
}


static int
do_complete(RlcCompleteData data)
{ if ( prolog_complete(data) )
    return TRUE;

  if ( file_completer )
    return (*file_completer)(data);
  return FALSE;
}


#endif /*HAVE_LIBREADLINE*/

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


static void
call_menu(const char *name)
{ fid_t fid = PL_open_foreign_frame();
  predicate_t pred = PL_predicate("on_menu", 1, "prolog");
  module_t m = PL_new_module(PL_new_atom("prolog"));
  term_t a0 = PL_new_term_ref();

  PL_put_atom_chars(a0, name);
  PL_call_predicate(m, PL_Q_NORMAL, pred, a0);

  PL_discard_foreign_frame(fid);
}


foreign_t
pl_win_insert_menu_item(foreign_t menu, foreign_t label, foreign_t before)
{ char *m, *l, *b;

  if ( !PL_get_atom_chars(menu, &m) ||
       !PL_get_atom_chars(label, &l) ||
       !PL_get_atom_chars(before, &b) )
    return FALSE;
  
  if ( strcmp(b, "-") == 0 )
    b = NULL;
  if ( strcmp(l, "--") == 0 )
    l = NULL;				/* insert a separator */
    
  return rlc_insert_menu_item(m, l, b);
}


foreign_t
pl_win_insert_menu(foreign_t label, foreign_t before)
{ char *l, *b;

  if ( !PL_get_atom_chars(label, &l) ||
       !PL_get_atom_chars(before, &b) )
    return FALSE;
  
  if ( strcmp(b, "-") == 0 )
    b = NULL;
    
  return rlc_insert_menu(l, b);
}


		 /*******************************
		 *	      SIGNALS		*
		 *******************************/

static DWORD main_thread_id;		/* ThreadId of main thread */
#define WM_SIGNALLED (WM_USER+1)
#define WM_MENU	     (WM_USER+2)

static WINAPI
pl_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ switch(message)
  { case WM_SIGNALLED:
      PL_handle_signals();
      return 0;
    case WM_MENU:
    { const char *name = (const char *)lParam;

      call_menu(name);

      return 0;
    }
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}


static char *
HiddenFrameClass()
{ static char winclassname[32];
  static WNDCLASS wndClass;
  HINSTANCE instance = rlc_hinstance();

  if ( !winclassname[0] )
  { sprintf(winclassname, "SWI-Prolog-hidden-win%d", instance);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) pl_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= instance;
    wndClass.hIcon		= NULL;
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= winclassname;

    RegisterClass(&wndClass);
  }

  return winclassname;
}


static void
destroy_hidden_window(int status, void *data)
{ HWND win = (HWND) data;

  DestroyWindow(win);
}


HWND
PL_hidden_window()
{ static HWND window;

  if ( !window )
  { window = CreateWindow(HiddenFrameClass(),
			  "SWI-Prolog hidden window",
			  0,
			  0, 0, 32, 32,
			  NULL, NULL, rlc_hinstance(), NULL);
    PL_on_halt(destroy_hidden_window, (void *)window);
  }

  return window;
}

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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If we are in  the  same  thread,   we  can  call  the interrupt-handling
rountine immediately. Otherwise we will   use  Prolog's signal scheduler
and wait for it to be raised.

Maybe we should wait for a little until the signal is handled by Prolog,
and indicate if this is not the case within a few seconds.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
interrupt(int sig)
{ if ( GetCurrentThreadId() == main_thread_id )
    PL_interrupt(sig);
  else
  { PL_raise(sig);
    PostMessage(PL_hidden_window(), WM_SIGNALLED, 0, 0);
  }
}


static void
menu_select(const char *name)
{ if ( GetCurrentThreadId() == main_thread_id )
    call_menu(name);
  else
  { PostMessage(PL_hidden_window(), WM_MENU, 0, (LONG)name);
  }
}


		 /*******************************
		 *	       MAIN		*
		 *******************************/


static void
set_window_title()
{ char title[256];
  long v = PL_query(PL_QUERY_VERSION);
  int major = v / 10000;
  int minor = (v / 100) % 100;
  int patch = v % 100;

  Ssprintf(title, "SWI-Prolog (version %d.%d.%d)", major, minor, patch);
  rlc_title(title, NULL, 0);
}


PL_extension extensions[] =
{
/*{ "name",	arity,  function,	PL_FA_<flags> },*/

  { "system:window_title",          2, pl_window_title,         0 },
  { "system:$win_insert_menu_item", 3, pl_win_insert_menu_item, 0 },
  { "system:win_insert_menu",       2, pl_win_insert_menu,      0 },
  { NULL,		            0, NULL,		        0 }
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is called back from the   console.dll main loop to provide
the main for the application.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
install_readline(int argc, char **argv)
{ 
#ifdef HAVE_LIBREADLINE
  PL_install_readline();
#else /*HAVE_LIBREADLINE*/  
  rlc_init_history(FALSE, 50);
  file_completer = rlc_complete_hook(do_complete);

  PL_register_foreign("system:rl_add_history", 1, pl_rl_add_history, 0);
  PL_register_foreign("system:rl_read_init_file", 1, pl_rl_read_init_file, 0);

  PL_set_feature("tty_control", PL_BOOL, TRUE);
  PL_set_feature("readline",    PL_BOOL, TRUE);
#endif /*HAVE_LIBREADLINE*/
}

static void
closeWin(int s, void *a)
{ rlc_close();
}

int
win32main(int argc, char **argv)
{ set_window_title();
  rlc_bind_terminal();

  PL_register_extensions(extensions);
  PL_initialise_hook(install_readline);
  PL_action(PL_ACTION_GUIAPP, TRUE);
  PL_on_halt(closeWin, NULL);
  atexit(rlc_close);
  if ( !PL_initialise(argc, argv) )
    PL_halt(1);
  
  PL_hidden_window();			/* create in main thread */
  main_thread_id = GetCurrentThreadId();
  PL_set_feature("hwnd", PL_INTEGER, (long)rlc_hwnd());
  rlc_interrupt_hook(interrupt);
  rlc_menu_hook(menu_select);
  PL_set_feature("console_menu", PL_BOOL, TRUE);
#if !defined(O_DEBUG) && !defined(_DEBUG)
  initSignals();
#endif
  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
And this is the  real  application's  main   as  Windows  sees  it.  See
console.c for further details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int PASCAL
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
	LPSTR lpszCmdLine, int nCmdShow)
{
  return rlc_main(hInstance, hPrevInstance, lpszCmdLine, nCmdShow,
		  win32main, LoadIcon(hInstance, "SWI_Icon"));
}
