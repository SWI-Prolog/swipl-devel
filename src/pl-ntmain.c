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
#include "pl-stream.h"
#include "pl-itf.h"
#include <ctype.h>
#include <console.h>
#include <signal.h>
#ifdef O_PLMT
#include <pthread.h>
#endif

#ifndef streq
#define streq(s,q) (strcmp((s), (q)) == 0)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Main program for running SWI-Prolog from   a window. The window provides
X11-xterm like features: scrollback for a   predefined  number of lines,
cut/paste and the GNU readline library for command-line editing.

This module combines libpl.dll and plterm.dll  with some glue to produce
the final executable plwin.exe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static rlc_console	current_console();
static int		type_error(term_t actual, const char *expected);
static int		domain_error(term_t actual, const char *expected);
static HWND		create_prolog_hidden_window(rlc_console c);
static int		get_chars_arg_ex(int a, term_t t, char **v, unsigned flags);

#define RLC_PROLOG_WINDOW	RLC_VALUE(0) /* GetCurrentThreadID() */
#define RLC_PROLOG_INPUT	RLC_VALUE(1) /* Input  stream (IOSTREAM*) */
#define RLC_PROLOG_OUTPUT	RLC_VALUE(2) /* Output stream (IOSTREAM*) */
#define RLC_PROLOG_ERROR	RLC_VALUE(3) /* Error  stream (IOSTREAM*) */
#define RLC_REGISTER		RLC_VALUE(4) /* Trap destruction */

		 /*******************************
		 *	    CONSOLE ADM		*
		 *******************************/

CRITICAL_SECTION mutex;
#define LOCK()   EnterCriticalSection(&mutex)
#define UNLOCK() LeaveCriticalSection(&mutex)

static rlc_console *consoles;		/* array of consoles */
static int consoles_length;		/* size of this array */

static void
unregisterConsole(unsigned long data)
{ rlc_console c = (rlc_console)data;
  rlc_console *p;
  int n;

  LOCK();
  for(p=consoles, n=0; n++<consoles_length; p++)
  { if ( *p == c )
    { *p = NULL;
      break;
    }
  }
  UNLOCK();
}


static void
registerConsole(rlc_console c)
{ rlc_console *p;
  int n;

  LOCK();
  for(;;)
  { for(p=consoles, n=0; n++<consoles_length; p++)
    { if ( !*p )
      { *p = c;
        rlc_set(c, RLC_REGISTER, (unsigned long)c, unregisterConsole);
	UNLOCK();
	return;
      }
    }
    if ( consoles_length )
    { int bytes = consoles_length*sizeof(rlc_console);

      consoles = PL_realloc(consoles, bytes*2);
      memset(consoles+consoles_length, 0, bytes);
      consoles_length *= 2;
    } else
    { consoles_length = 10;
      consoles = PL_malloc(consoles_length*sizeof(rlc_console));
      memset(consoles, 0, consoles_length*sizeof(rlc_console));
    }
  }
}


void
closeConsoles()
{ int i;
  rlc_console *p;

  LOCK();
  for(i=0, p=consoles; i<consoles_length; i++, p++)
  { if ( *p )
      rlc_close(*p);
  }
  UNLOCK();
}


		 /*******************************
		 *	BIND STREAM STUFF	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First step: bind the  console  I/O   to  the  Sinput/Soutput  and Serror
streams.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
Srlc_read(void *handle, char *buffer, int size)
{ rlc_console c = handle;
  int bytes;

  PL_write_prompt(TRUE);

  if ( Suser_input->handle == c && PL_ttymode(Suser_input) == PL_RAWTTY )
  { int chr = getkey(c);
      
    if ( chr == 04 || chr == 26 || chr == -1 )
    { bytes = 0;
    } else
    { buffer[0] = chr & 0xff;
      bytes = 1;
    }
  } else
  { bytes = rlc_read(c, buffer, size);
  }

  if ( bytes == 0 || buffer[bytes-1] == '\n' )
    PL_prompt_next(0);

  return bytes;
}


static int
Srlc_write(void *handle, char *buffer, int size)
{ rlc_console c = handle;

  return rlc_write(c, buffer, size);
}


static int
Srlc_close(void *handle)
{ rlc_console c = handle;
  unsigned long v;
  int closed = 0;

  if ( rlc_get(handle, RLC_PROLOG_INPUT, &v) && v &&
       ((IOSTREAM *)v)->flags && SIO_CLOSING )
  { rlc_set(handle, RLC_PROLOG_INPUT, 0L, NULL);
    closed++;
  } else if ( rlc_get(handle, RLC_PROLOG_OUTPUT, &v) && v &&
	      ((IOSTREAM *)v)->flags && SIO_CLOSING )
  { rlc_set(handle, RLC_PROLOG_OUTPUT, 0L, NULL);
    closed++;
  } else if ( rlc_get(handle, RLC_PROLOG_ERROR, &v) && v &&
	      ((IOSTREAM *)v)->flags && SIO_CLOSING )
  { rlc_set(handle, RLC_PROLOG_ERROR, 0L, NULL);
  }

  if ( closed &&
       rlc_get(handle, RLC_PROLOG_INPUT, &v)  && v == 0L &&
       rlc_get(handle, RLC_PROLOG_OUTPUT, &v) && v == 0L )
    rlc_close(c);

  return 0;
}

static IOFUNCTIONS rlc_functions;

static void
rlc_bind_terminal(rlc_console c)
{ rlc_functions	      = *Sinput->functions;
  rlc_functions.read  = Srlc_read;
  rlc_functions.write = Srlc_write;
  rlc_functions.close = Srlc_close;

  Sinput->functions  = &rlc_functions;
  Soutput->functions = &rlc_functions;
  Serror->functions  = &rlc_functions;
  
  Sinput->handle  = c;
  Soutput->handle = c;
  Serror->handle  = c;
}


static int
process_console_options(rlc_console_attr *attr, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t opt = PL_new_term_ref();

  while(PL_get_list(tail, opt, tail))
  { atom_t name;
    const char *s;
    int arity;

    if ( !PL_get_name_arity(opt, &name, &arity) )
      return type_error(opt, "compound");
    s = PL_atom_chars(name);
    if ( streq(s, "registry_key") && arity == 1 )
    { if ( !get_chars_arg_ex(1, opt, (char **)&attr->key, CVT_ALL|BUF_RING) )
	return FALSE;
    } else
      return domain_error(opt, "window_option");
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");
}


static void				/* handle console destruction */
free_stream(unsigned long handle)
{ IOSTREAM *s = (IOSTREAM*) handle;

  Sclose(s);
}


static foreign_t
pl_win_open_console(term_t title, term_t input, term_t output, term_t error,
		    term_t options)
{ rlc_console_attr attr;
  rlc_console c;
  IOSTREAM *in, *out, *err;
  char *s;

  memset(&attr, 0, sizeof(attr));
  if ( !PL_get_chars(title, &s, CVT_ALL|BUF_RING) )
    return type_error(title, "text");
  attr.title = (const char *) s;

  if ( !process_console_options(&attr, options) )
    return FALSE;

  c = rlc_create_console(&attr);
  create_prolog_hidden_window(c);	/* for sending messages */
  registerConsole(c);

#define STREAM_COMMON (SIO_TEXT|	/* text-stream */ 		\
		       SIO_NOCLOSE|	/* do no close on abort */	\
		       SIO_ISATTY|	/* terminal */			\
		       SIO_NOFEOF)	/* reset on end-of-file */

  in  = Snew(c,  SIO_INPUT|SIO_LBUF|STREAM_COMMON, &rlc_functions);
  out = Snew(c, SIO_OUTPUT|SIO_LBUF|STREAM_COMMON, &rlc_functions);
  err = Snew(c, SIO_OUTPUT|SIO_NBUF|STREAM_COMMON, &rlc_functions);

  in->position  = &in->posbuf;		/* record position on same stream */
  out->position = &in->posbuf;
  err->position = &in->posbuf;

  if ( !PL_unify_stream(input, in) ||
       !PL_unify_stream(output, out) ||
       !PL_unify_stream(error, err) )
  { Sclose(in);
    Sclose(out);
    Sclose(err);
    rlc_close(c);

    return FALSE;
  }

  rlc_set(c, RLC_PROLOG_INPUT,  (unsigned long)in,  NULL);
  rlc_set(c, RLC_PROLOG_OUTPUT, (unsigned long)out, NULL);
  rlc_set(c, RLC_PROLOG_ERROR,  (unsigned long)err, free_stream);

  return TRUE;
}

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

      rlc_add_history(current_console(), PL_atom_chars(a));
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


		 /*******************************
		 *	   CONSOLE STUFF	*
		 *******************************/

static rlc_console
current_console()
{ if ( Suser_input->functions->read = Srlc_read )
    return Suser_input->handle;

  return NULL;
}


foreign_t
pl_window_title(term_t old, term_t new)
{ char buf[256];
  char *n;

  if ( !PL_get_atom_chars(new, &n) )
    return type_error(new, "atom");

  rlc_title(current_console(), n, buf, sizeof(buf));

  return PL_unify_atom_chars(old, buf);
}


static int
type_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "type_error", 2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}

static int
domain_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "domain_error", 2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}

static int
get_chars_arg_ex(int a, term_t t, char **v, unsigned flags)
{ term_t arg = PL_new_term_ref();

  PL_get_arg(a, t, arg);
  if ( PL_get_chars(arg, v, flags) )
    return TRUE;

  return type_error(arg, "text");
}


static int
get_int_arg_ex(int a, term_t t, int *v)
{ term_t arg = PL_new_term_ref();

  PL_get_arg(a, t, arg);
  if ( PL_get_integer(arg, v) )
    return TRUE;

  return type_error(arg, "integer");
}


static int
get_bool_arg_ex(int a, term_t t, int *v)
{ term_t arg = PL_new_term_ref();

  PL_get_arg(a, t, arg);
  if ( PL_get_bool(arg, v) )
    return TRUE;

  return type_error(arg, "boolean");
}


foreign_t
pl_window_pos(term_t options)
{ int x = 0, y = 0, w = 0, h = 0;
  HWND z = HWND_TOP;
  UINT flags = SWP_NOACTIVATE|SWP_NOZORDER|SWP_NOSIZE|SWP_NOMOVE;
  term_t opt = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(options);

  while(PL_get_list(tail, opt, tail))
  { atom_t name;
    const char *s;
    int arity;
    
    if ( !PL_get_name_arity(opt, &name, &arity) )
      return type_error(opt, "compound");
    s = PL_atom_chars(name);
    if ( streq(s, "position") && arity == 2 )
    { if ( !get_int_arg_ex(1, opt, &x) ||
	   !get_int_arg_ex(2, opt, &y) )
	return FALSE;
      flags &= ~SWP_NOMOVE;
    } else if ( streq(s, "size") && arity == 2 )
    { if ( !get_int_arg_ex(1, opt, &w) ||
	   !get_int_arg_ex(2, opt, &h) )
	return FALSE;
      flags &= ~SWP_NOSIZE;
    } else if ( streq(s, "zorder") && arity == 1 )
    { term_t t = PL_new_term_ref();
      char *v;

      PL_get_arg(1, opt, t);
      if ( !PL_get_atom_chars(t, &v) )
	return type_error(t, "atom");
      if ( streq(v, "top") )
	z = HWND_TOP;
      else if ( streq(v, "bottom") )
	z = HWND_BOTTOM;
      else if ( streq(v, "topmost") )
	z = HWND_TOPMOST;
      else if ( streq(v, "notopmost") )
	z = HWND_NOTOPMOST;
      else
	return domain_error(t, "hwnd_insert_after");

      flags &= ~SWP_NOZORDER;
    } else if ( streq(s, "show") && arity == 1 )
    { int v;

      if ( !get_bool_arg_ex(1, opt, &v) )
	return FALSE;
      flags &= ~(SWP_SHOWWINDOW|SWP_HIDEWINDOW);
      if ( v )
	flags |= SWP_SHOWWINDOW;
      else
	flags |= SWP_HIDEWINDOW;
    } else if ( streq(s, "activate") && arity == 0 )
    { flags &= ~SWP_NOACTIVATE;
    } else
      return domain_error(opt, "window_option");
  }
  if ( !PL_get_nil(tail) )
   return type_error(tail, "list");

  rlc_window_pos(current_console(), z, x, y, w, h, flags);

  return TRUE;
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
    
  return rlc_insert_menu_item(current_console(), m, l, b);
}


foreign_t
pl_win_insert_menu(foreign_t label, foreign_t before)
{ char *l, *b;

  if ( !PL_get_atom_chars(label, &l) ||
       !PL_get_atom_chars(before, &b) )
    return FALSE;
  
  if ( strcmp(b, "-") == 0 )
    b = NULL;
    
  return rlc_insert_menu(current_console(), l, b);
}

		 /*******************************
		 *	      THREADS		*
		 *******************************/

#ifdef O_PLMT

static void
free_interactor(void *closure)
{ PL_thread_destroy_engine();
}


static void *
run_interactor(void *closure)
{ predicate_t pred;

  PL_thread_attach_engine(NULL);
  pthread_cleanup_push(free_interactor, NULL);
  

  pred = PL_predicate("thread_run_interactor", 0, "user");
  PL_call_predicate(NULL, PL_Q_NORMAL, pred, 0);

  pthread_cleanup_pop(1);

  return NULL;
}


static void
create_interactor()
{ pthread_attr_t attr;
  pthread_t child;

  pthread_attr_init(&attr);
  pthread_create(&child, &attr, run_interactor, NULL);
}

#endif /*O_PLMT*/

		 /*******************************
		 *	      SIGNALS		*
		 *******************************/

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
destroy_hidden_window(unsigned long hwnd)
{ DestroyWindow((HWND)hwnd);
}


static HWND
create_prolog_hidden_window(rlc_console c)
{ unsigned long hwnd;

  if ( rlc_get(c, RLC_PROLOG_WINDOW, &hwnd) && hwnd )
    return (HWND)hwnd;

  hwnd = (unsigned long)CreateWindow(HiddenFrameClass(),
				     "SWI-Prolog hidden window",
				     0,
				     0, 0, 32, 32,
				     NULL, NULL, rlc_hinstance(), NULL);

  rlc_set(c, RLC_PROLOG_WINDOW, hwnd, destroy_hidden_window);

  return (HWND)hwnd;
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
Callbacks from the console. Trouble is that these routines are called in
the thread updating the console rather   than the thread running Prolog.
We can inform Prolog using  the  hidden   window  which  is  in Prolog's
thread. For the interrupt to work if Prolog   is  working we need to set
the signalled mask in the  proper   thread.  This  is accomplished using
PL_w32thread_raise(ID, sig). In the single-threaded   version  this call
simply calls PL_raise(sig).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
interrupt(rlc_console c, int sig)
{ DWORD tid;

  if ( rlc_get(c, RLC_APPLICATION_THREAD_ID, &tid) )
  { unsigned long hwnd;

    PL_w32thread_raise((DWORD)tid, sig);
    if ( rlc_get(c, RLC_PROLOG_WINDOW, &hwnd) )
      PostMessage((HWND)hwnd, WM_SIGNALLED, 0, 0);
  }
}


static void
menu_select(rlc_console c, const char *name)
{ 
#ifdef O_PLMT
  if ( streq(name, "&New thread") )
  { create_interactor();
  } else
#endif /*O_PLMT*/
  { unsigned long hwnd;

    if ( rlc_get(c, RLC_PROLOG_WINDOW, &hwnd) )
      PostMessage((HWND)hwnd, WM_MENU, 0, (LONG)name);
  }
}


		 /*******************************
		 *	       MAIN		*
		 *******************************/


static void
set_window_title(rlc_console c)
{ char title[256];
  long v = PL_query(PL_QUERY_VERSION);
  int major = v / 10000;
  int minor = (v / 100) % 100;
  int patch = v % 100;

#ifdef O_PLMT
  Ssprintf(title, "SWI-Prolog (Multi-threaded, version %d.%d.%d)", major, minor, patch);
#else
  Ssprintf(title, "SWI-Prolog (version %d.%d.%d)", major, minor, patch);
#endif
  rlc_title(c, title, NULL, 0);
}


PL_extension extensions[] =
{
/*{ "name",	arity,  function,	PL_FA_<flags> },*/

  { "system:window_title",          2, pl_window_title,         0 },
  { "system:$win_insert_menu_item", 3, pl_win_insert_menu_item, 0 },
  { "system:win_insert_menu",       2, pl_win_insert_menu,      0 },
  { "system:win_window_pos",        1, pl_window_pos,           0 },
  { NULL,		            0, NULL,		        0 }
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
win32main() is called back from the plterm.dll  main loop to provide the
main for the application.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
install_readline(rlc_console c)
{ rlc_init_history(c, 50);
  file_completer = rlc_complete_hook(do_complete);

  PL_register_foreign("system:rl_add_history",    1, pl_rl_add_history,    0);
  PL_register_foreign("system:rl_read_init_file", 1, pl_rl_read_init_file, 0);

  PL_set_feature("tty_control", PL_BOOL, TRUE);
  PL_set_feature("readline",    PL_BOOL, TRUE);
}

/* destroy the console on exit.  Using PL_on_halt() is the clean, but somewhat
   uncertain way.  using atexit() is more reliable, but we must be sure we don't
   do it twice.
*/

static rlc_console main_console;

static void
closeWin(int s, void *a)
{ rlc_console c = a;

//  closeConsoles();

  if ( c == main_console )
  { main_console = NULL;
    rlc_close(c);
  }
}

int
win32main(rlc_console c, int argc, char **argv)
{ set_window_title(c);
  rlc_bind_terminal(c);

  PL_register_extensions(extensions);
  install_readline(c);
  PL_action(PL_ACTION_GUIAPP, TRUE);
  main_console = c;
  PL_on_halt(closeWin, c);

  create_prolog_hidden_window(c);
  PL_set_feature("hwnd", PL_INTEGER, (long)rlc_hwnd(c));
  rlc_interrupt_hook(interrupt);
  rlc_menu_hook(menu_select);
  PL_set_feature("console_menu", PL_BOOL, TRUE);
#ifdef O_PLMT
  rlc_insert_menu_item(c, "&Run", "&New thread", NULL);
#endif
#if !defined(O_DEBUG) && !defined(_DEBUG)
  initSignals();
#endif
  PL_register_foreign("system:win_open_console", 5,
		      pl_win_open_console, 0);

  if ( !PL_initialise(argc, argv) )
    PL_halt(1);
  
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
{ InitializeCriticalSection(&mutex);

  return rlc_main(hInstance, hPrevInstance, lpszCmdLine, nCmdShow,
		  win32main, LoadIcon(hInstance, "SWI_Icon"));
}
