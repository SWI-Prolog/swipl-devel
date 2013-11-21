/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
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

#define _UNICODE 1
#define UNICODE 1

#ifdef WIN64
#include "config/win64.h"
#else
#include "config/win32.h"
#endif

#include <windows.h>
#include <tchar.h>
#include <malloc.h>
#include <stdio.h>
#include "os/SWI-Stream.h"
#include "SWI-Prolog.h"
#include <ctype.h>
#include "win32/console/console.h"
#include <signal.h>
#ifdef O_PLMT
#include <pthread.h>
#endif

#include "os/pl-utf8.c"			/* we're not in the libpl.dll module */

#ifndef streq
#define streq(s,q) (strcmp((s), (q)) == 0)
#endif

#ifndef _TINT
typedef wint_t _TINT;
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Main program for running SWI-Prolog from   a window. The window provides
X11-xterm like features: scrollback for a   predefined  number of lines,
cut/paste and the GNU readline library for command-line editing.

This module combines swipl.dll and plterm.dll  with some glue to produce
the final executable swipl-win.exe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

__declspec(dllexport)	rlc_console	PL_current_console(void);
__declspec(dllexport)	int		PL_set_menu_thread(void);
static HWND		create_prolog_hidden_window(rlc_console c, int replace);
static int		get_chars_arg_ex(int a, term_t t, TCHAR **v);

#define RLC_PROLOG_WINDOW	RLC_VALUE(0) /* GetCurrentThreadID() */
#define RLC_PROLOG_INPUT	RLC_VALUE(1) /* Input  stream (IOSTREAM*) */
#define RLC_PROLOG_OUTPUT	RLC_VALUE(2) /* Output stream (IOSTREAM*) */
#define RLC_PROLOG_ERROR	RLC_VALUE(3) /* Error  stream (IOSTREAM*) */
#define RLC_REGISTER		RLC_VALUE(4) /* Trap destruction */

		 /*******************************
		 *	  CONSOLE ADMIN		*
		 *******************************/

static CRITICAL_SECTION mutex;
#define LOCK()   EnterCriticalSection(&mutex)
#define UNLOCK() LeaveCriticalSection(&mutex)

static rlc_console *consoles;		/* array of consoles */
static int consoles_length;		/* size of this array */

static void
unregisterConsole(uintptr_t data)
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
        rlc_set(c, RLC_REGISTER, (uintptr_t)c, unregisterConsole);
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
closeConsoles(void)
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

static ssize_t
Srlc_read(void *handle, char *buffer, size_t size)
{ rlc_console c = handle;
  size_t bytes;
  int is_user_input = (Suser_input && Suser_input->handle == c);
  term_t ex;

  PL_write_prompt(TRUE);

  if ( is_user_input &&
       PL_ttymode(Suser_input) == PL_RAWTTY )
  { int chr = getkey(c);
    TCHAR *tbuf = (TCHAR*)buffer;

    if ( chr == 04 || chr == 26 || chr == -1 )
    { bytes = 0;
    } else
    { tbuf[0] = chr;
      bytes = sizeof(TCHAR);
    }
  } else
  { bytes = rlc_read(c, (TCHAR*)buffer, size/sizeof(TCHAR));
    bytes *= sizeof(TCHAR);
  }

  if ( is_user_input && (ex=PL_exception(0)) )
  { Sset_exception(Suser_input, ex);
    return -1;
  }

  if ( bytes == 0 || buffer[bytes-1] == '\n' )
    PL_prompt_next(0);

  return bytes;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The user streams for swipl-win.exe run  using the 'wchar' encoding. This
means size must be a multiple of   sizeof(wchar_t),  but not if the user
cheats and either switches the encoding   or uses put_byte/1 or similar.
The flushing code will remember `half'   characters  and re-send them as
more data comes ready. This means however  that after a put_byte(X), the
wchar_t stream is out-of-sync and produces   unreadable  output. We will
therefore pad it with '?' characters to re-sync the stream.

The downside of this is  that  Sputc()   and  Sputcode()  do not work on
unbuffered streams and thus Serror  must   be  locked before using these
functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ssize_t
Srlc_write(void *handle, char *buffer, size_t size)
{ rlc_console c = handle;
  ssize_t n;

  n = rlc_write(c, (TCHAR*)buffer, size/sizeof(TCHAR));
  n *= sizeof(TCHAR);

  if ( n < (ssize_t)size && size-n < sizeof(TCHAR) )
  { char buf[sizeof(TCHAR)];		/* Pad to TCHAR */
    size_t i = sizeof(TCHAR) - (size-n);

    memcpy(buf, buffer+n, i);
    for(; i<sizeof(TCHAR); i++)
      buf[i] = '?';
    rlc_write(c, (TCHAR*)buffer, 1);

    return size;
  }

  return n;
}


static int
Srlc_close(void *handle)
{ rlc_console c = handle;
  uintptr_t v;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The role of this function is to stop  changing the encoding of the plwin
output. We must return -1 for  SIO_SETENCODING   for  this. As we do not
implement any of the other control operations   we  simply return -1 for
all commands we may be requested to handle.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
Srlc_control(void *handle, int cmd, void *closure)
{ return -1;
}


static IOFUNCTIONS rlc_functions;

static void
rlc_bind_terminal(rlc_console c)
{ rlc_functions	        = *Sinput->functions;
  rlc_functions.read    = Srlc_read;
  rlc_functions.write	= Srlc_write;
  rlc_functions.close   = Srlc_close;
  rlc_functions.control = Srlc_control;

  Sinput->functions  = &rlc_functions;
  Soutput->functions = &rlc_functions;
  Serror->functions  = &rlc_functions;

  Sinput->handle  = c;
  Soutput->handle = c;
  Serror->handle  = c;

  Sinput->encoding  = ENC_WCHAR;
  Soutput->encoding = ENC_WCHAR;
  Serror->encoding  = ENC_WCHAR;

  Sinput->flags  &= ~SIO_FILE;
  Soutput->flags &= ~SIO_FILE;
  Serror->flags  &= ~SIO_FILE;
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
      return PL_type_error("compound", opt);
    s = PL_atom_chars(name);
    if ( streq(s, "registry_key") && arity == 1 )
    { TCHAR *key;

      if ( !get_chars_arg_ex(1, opt, &key) )
	return FALSE;

      attr->key = key;
    } else
      return PL_domain_error("window_option", opt);
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  return TRUE;
}


static void				/* handle console destruction */
free_stream(uintptr_t handle)
{ IOSTREAM *s = (IOSTREAM*) handle;

  Sclose(s);
}


static foreign_t
pl_win_open_console(term_t title, term_t input, term_t output, term_t error,
		    term_t options)
{ rlc_console_attr attr;
  rlc_console c;
  IOSTREAM *in, *out, *err;
  TCHAR *s;
  size_t len;

  memset(&attr, 0, sizeof(attr));
  if ( !PL_get_wchars(title, &len, &s, CVT_ALL|BUF_RING|CVT_EXCEPTION) )
    return FALSE;
  attr.title = (const TCHAR*) s;

  if ( !process_console_options(&attr, options) )
    return FALSE;

  c = rlc_create_console(&attr);
  create_prolog_hidden_window(c, FALSE);	/* for sending messages */
  registerConsole(c);

#define STREAM_COMMON (SIO_TEXT|	/* text-stream */		\
		       SIO_NOCLOSE|	/* do no close on abort */	\
		       SIO_ISATTY|	/* terminal */			\
		       SIO_NOFEOF)	/* reset on end-of-file */

  in  = Snew(c,  SIO_INPUT|SIO_LBUF|STREAM_COMMON, &rlc_functions);
  out = Snew(c, SIO_OUTPUT|SIO_LBUF|STREAM_COMMON, &rlc_functions);
  err = Snew(c, SIO_OUTPUT|SIO_NBUF|STREAM_COMMON, &rlc_functions);

  in->position  = &in->posbuf;		/* record position on same stream */
  out->position = &in->posbuf;
  err->position = &in->posbuf;

  in->encoding  = ENC_WCHAR;
  out->encoding = ENC_WCHAR;
  err->encoding = ENC_WCHAR;

  if ( !PL_unify_stream(input, in) ||
       !PL_unify_stream(output, out) ||
       !PL_unify_stream(error, err) )
  { Sclose(in);
    Sclose(out);
    Sclose(err);
    rlc_close(c);

    return FALSE;
  }

  rlc_set(c, RLC_PROLOG_INPUT,  (uintptr_t)in,  NULL);
  rlc_set(c, RLC_PROLOG_OUTPUT, (uintptr_t)out, NULL);
  rlc_set(c, RLC_PROLOG_ERROR,  (uintptr_t)err, free_stream);

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

  if ( PL_get_atom_ex(text, &a) )
  { if ( a != last )
    { TCHAR *s;

      if ( last )
	PL_unregister_atom(last);
      last = a;
      PL_register_atom(last);

      if ( PL_get_wchars(text, NULL, &s, CVT_ATOM) )
	rlc_add_history(PL_current_console(), s);
    }

    return TRUE;
  }

  return FALSE;
}


static int
add_line(void *h, int no, const TCHAR *line)
{ term_t tail = (term_t)h;
  term_t head = PL_new_term_ref();

  if ( !PL_unify_wchars(head, PL_ATOM, (size_t)-1, line) ||
       !PL_cons_list(tail, head, tail) )
    return -1;

  PL_reset_term_refs(head);

  return 0;
}


static foreign_t
pl_rl_history(term_t list)
{ term_t tail = PL_new_term_ref();

  if ( !PL_unify_nil(tail) )
    return FALSE;

  if ( rlc_for_history(PL_current_console(), add_line, (void*)tail) == 0 )
    return PL_unify(tail, list);

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
    { size_t start = ln->point;
      wint_t c;

      if ( !ln->data )		/* we donot want to complete on all atoms */
	return FALSE;

      while(start > 0 && (_istalnum((c=ln->data[start-1])) || c == '_') )
	start--;
      if ( start > 0 )
      { _TINT cs = ln->data[start-1];

	if ( _tcschr(_T("'/\\.~"), cs) )
	  return FALSE;			/* treat as a filename */
      }
      if ( _istlower(ln->data[start]) )	/* Lower, Aplha ...: an atom */
      { size_t patlen = ln->point - start;

	_tcsncpy(data->buf_handle, &ln->data[start], patlen);
	data->buf_handle[patlen] = '\0';

	if ( PL_atom_generator_w(data->buf_handle,
				 data->candidate,
				 sizeof(data->candidate)/sizeof(TCHAR),
				 FALSE) )
	{ data->replace_from = (int)start;
	  data->function = prolog_complete;
	  return TRUE;
	}
      }

      return FALSE;
    }
    case COMPLETE_ENUMERATE:
    { if ( PL_atom_generator_w(data->buf_handle,
			       data->candidate,
			       sizeof(data->candidate)/sizeof(TCHAR),
			       TRUE) )
	return TRUE;

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

rlc_console
PL_current_console(void)
{ if ( Suser_input->functions->read == Srlc_read )
    return Suser_input->handle;

  return NULL;
}


static rlc_console main_console;

int
PL_set_menu_thread(void)
{ if ( main_console )
  { create_prolog_hidden_window(main_console, TRUE);
    return TRUE;
  }

  return FALSE;
}


foreign_t
pl_window_title(term_t old, term_t new)
{ TCHAR buf[256];
  TCHAR *n;

  if ( !PL_get_wchars(new, NULL, &n, CVT_ALL|CVT_EXCEPTION) )
    return FALSE;

  rlc_title(PL_current_console(), n, buf, sizeof(buf)/sizeof(TCHAR));

  return PL_unify_wchars(old, PL_ATOM, _tcslen(buf), buf);
}


static int
get_chars_arg_ex(int a, term_t t, TCHAR **v)
{ term_t arg = PL_new_term_ref();

  if ( PL_get_arg(a, t, arg) &&
       PL_get_wchars(arg, NULL, v, CVT_ALL|BUF_RING|CVT_EXCEPTION) )
    return TRUE;

  return FALSE;
}


static int
get_int_arg_ex(int a, term_t t, int *v)
{ term_t arg = PL_new_term_ref();

  _PL_get_arg(a, t, arg);
  if ( PL_get_integer_ex(arg, v) )
    return TRUE;

  return FALSE;
}


static int
get_bool_arg_ex(int a, term_t t, int *v)
{ term_t arg = PL_new_term_ref();

  _PL_get_arg(a, t, arg);
  if ( PL_get_bool_ex(arg, v) )
    return TRUE;

  return FALSE;
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
      return PL_type_error("compound", opt);
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

      _PL_get_arg(1, opt, t);
      if ( !PL_get_chars(t, &v, CVT_ATOM|CVT_EXCEPTION) )
	return FALSE;
      if ( streq(v, "top") )
	z = HWND_TOP;
      else if ( streq(v, "bottom") )
	z = HWND_BOTTOM;
      else if ( streq(v, "topmost") )
	z = HWND_TOPMOST;
      else if ( streq(v, "notopmost") )
	z = HWND_NOTOPMOST;
      else
	return PL_domain_error("hwnd_insert_after", t);

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
      return PL_domain_error("window_option", opt);
  }
  if ( !PL_get_nil_ex(tail) )
   return FALSE;

  rlc_window_pos(PL_current_console(), z, x, y, w, h, flags);

  return TRUE;
}


static void
call_menu(const TCHAR *name)
{ fid_t fid = PL_open_foreign_frame();
  predicate_t pred = PL_predicate("on_menu", 1, "prolog");
  module_t m = PL_new_module(PL_new_atom("prolog"));
  term_t a0 = PL_new_term_ref();
  size_t len = _tcslen(name);

  if ( PL_unify_wchars(a0, PL_ATOM, len, name) )
    PL_call_predicate(m, PL_Q_NORMAL, pred, a0);

  PL_discard_foreign_frame(fid);
}


foreign_t
pl_win_insert_menu_item(foreign_t menu, foreign_t label, foreign_t before)
{ TCHAR *m, *l, *b;

  if ( !PL_get_wchars(menu, NULL, &m, CVT_ATOM) ||
       !PL_get_wchars(label, NULL, &l, CVT_ATOM) ||
       !PL_get_wchars(before, NULL, &b, CVT_ATOM) )
    return FALSE;

  if ( _tcscmp(b, _T("-")) == 0 )
    b = NULL;
  if ( _tcscmp(l, _T("--")) == 0 )
    l = NULL;				/* insert a separator */

  return rlc_insert_menu_item(PL_current_console(), m, l, b);
}


foreign_t
pl_win_insert_menu(foreign_t label, foreign_t before)
{ TCHAR *l, *b;

  if ( !PL_get_wchars(label, NULL, &l, CVT_ATOM) ||
       !PL_get_wchars(before, NULL, &b, CVT_ATOM) )
    return FALSE;

  if ( _tcscmp(b, _T("-")) == 0 )
    b = NULL;

  return rlc_insert_menu(PL_current_console(), l, b);
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
  PL_thread_attr_t attr = {0};

  attr.flags = PL_THREAD_NO_DEBUG;
  PL_thread_attach_engine(&attr);
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

static LRESULT WINAPI
pl_wnd_proc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{ switch(message)
  { case WM_SIGNALLED:
      PL_handle_signals();
      return 0;
    case WM_MENU:
    { const TCHAR *name = (const TCHAR *)lParam;

      call_menu(name);

      return 0;
    }
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}


static TCHAR *
HiddenFrameClass()
{ static TCHAR winclassname[32];
  static WNDCLASS wndClass;
  HINSTANCE instance = rlc_hinstance();

  if ( !winclassname[0] )
  { snwprintf(winclassname, sizeof(winclassname)/sizeof(TCHAR),
	      _T("SWI-Prolog-hidden-win%d"), instance);

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
destroy_hidden_window(uintptr_t hwnd)
{ DestroyWindow((HWND)hwnd);
}


static HWND
create_prolog_hidden_window(rlc_console c, int replace)
{ uintptr_t hwnd;

  if ( rlc_get(c, RLC_PROLOG_WINDOW, &hwnd) && hwnd )
  { if ( replace )
      DestroyWindow((HWND)hwnd);
    else
      return (HWND)hwnd;
  }

  hwnd = (uintptr_t)CreateWindow(HiddenFrameClass(),
				     _T("SWI-Prolog hidden window"),
				     0,
				     0, 0, 32, 32,
				     NULL, NULL, rlc_hinstance(), NULL);

  rlc_set(c, RLC_PROLOG_WINDOW, hwnd, destroy_hidden_window);

  return (HWND)hwnd;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Capturing fatal signals doesn't appear to work   inside  a DLL, hence we
capture them in the application and tell   Prolog to print the stack and
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
{ uintptr_t val;

  if ( rlc_get(c, RLC_APPLICATION_THREAD_ID, &val) )
  { DWORD tid = (DWORD)val;

    PL_w32thread_raise(tid, sig);
    if ( rlc_get(c, RLC_PROLOG_WINDOW, &val) )
    { HWND hwnd = (HWND)val;

      PostMessage((HWND)hwnd, WM_SIGNALLED, 0, 0);
    }
  }
}


static void
menu_select(rlc_console c, const TCHAR *name)
{
#ifdef O_PLMT
  if ( _tcscmp(name, _T("&New thread")) == 0 )
  { create_interactor();
  } else
#endif /*O_PLMT*/
  { uintptr_t hwnd;

    if ( rlc_get(c, RLC_PROLOG_WINDOW, &hwnd) )
      PostMessage((HWND)hwnd, WM_MENU, 0, (LPARAM)name);
  }
}

static LRESULT
message_proc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{ switch( PL_win_message_proc(hwnd, message, wParam, lParam) )
  { case PL_MSG_HANDLED:
      return TRUE;
    default:
      return FALSE;
  }
}

		 /*******************************
		 *	       MAIN		*
		 *******************************/


static void
set_window_title(rlc_console c)
{ TCHAR title[256];
  int v = (int)PL_query(PL_QUERY_VERSION);
  int major = v / 10000;
  int minor = (v / 100) % 100;
  int patch = v % 100;
#ifdef O_PLMT
  TCHAR *mt = _T("Multi-threaded, ");
#else
  TCHAR *mt = _T("");
#endif
#ifdef WIN64
  TCHAR *w64 = _T("AMD64, ");			/* TBD: IA64 */
#else
  TCHAR *w64 = _T("");
#endif

  snwprintf(title, sizeof(title)/sizeof(TCHAR),
	    _T("SWI-Prolog (%s%sversion %d.%d.%d)"),
	    w64, mt, major, minor, patch);

  rlc_title(c, title, NULL, 0);
}


PL_extension extensions[] =
{
/*{ "name",	arity,  function,	PL_FA_<flags> },*/

  { "window_title",          2, pl_window_title,         0 },
  { "$win_insert_menu_item", 3, pl_win_insert_menu_item, 0 },
  { "win_insert_menu",       2, pl_win_insert_menu,      0 },
  { "win_window_pos",        1, pl_window_pos,           0 },
  { NULL,                    0, NULL,                    0 }
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
win32main() is called back from the plterm.dll  main loop to provide the
main for the application.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
install_readline(rlc_console c)
{ rlc_init_history(c, 50);
  file_completer = rlc_complete_hook(do_complete);

  PL_register_foreign_in_module(
      "system", "rl_add_history",    1, pl_rl_add_history,    0);
  PL_register_foreign_in_module(
      "system", "rl_read_init_file", 1, pl_rl_read_init_file, 0);
  PL_register_foreign_in_module(
      "system", "$rl_history",       1, pl_rl_history,        0);

  PL_set_prolog_flag("tty_control", PL_BOOL, TRUE);
  PL_set_prolog_flag("readline",    PL_BOOL, TRUE);
}

/* destroy the console on exit.  Using PL_on_halt() is the clean, but somewhat
   uncertain way.  using atexit() is more reliable, but we must be sure we don't
   do it twice.
*/

static rlc_console main_console;

static int
closeWin(int s, void *a)
{ rlc_console c = a;

//  closeConsoles();

  if ( c == main_console )
  { main_console = NULL;
    rlc_close(c);
  }

  return 0;
}

#define MAX_ARGC 100

static size_t
utf8_required_len(const wchar_t *s)
{ size_t l = 0;
  char tmp[6];
  char *q;

  for( ; *s; s++)
  { q = utf8_put_char(tmp, *s);
    l += q-tmp;
  }

  return l;
}


int
win32main(rlc_console c, int argc, TCHAR **argv)
{ char *av[MAX_ARGC+1];
  int i;

  main_console = c;
  set_window_title(c);
  rlc_bind_terminal(c);

  PL_register_extensions_in_module("system", extensions);
  install_readline(c);
  PL_action(PL_ACTION_GUIAPP, TRUE);
  main_console = c;
  PL_on_halt(closeWin, c);

  create_prolog_hidden_window(c, FALSE);
  PL_set_prolog_flag("hwnd", PL_INTEGER, (intptr_t)rlc_hwnd(c));
  rlc_interrupt_hook(interrupt);
  rlc_menu_hook(menu_select);
  rlc_message_hook(message_proc);
  PL_set_prolog_flag("console_menu", PL_BOOL, TRUE);
#ifdef O_PLMT
  rlc_insert_menu_item(c, _T("&Run"), _T("&New thread"), NULL);
#endif
#if !defined(O_DEBUG) && !defined(_DEBUG)
  initSignals();
#endif
  PL_register_foreign_in_module("system", "win_open_console", 5,
				pl_win_open_console, 0);

  if ( argc > MAX_ARGC )
    argc = MAX_ARGC;
  for(i=0; i<argc; i++)
  { char *s;
    TCHAR *q;

    av[i] = alloca(utf8_required_len(argv[i])+1);
    for(s=av[i], q=argv[i]; *q; q++)
    { s = utf8_put_char(s, *q);
    }
    *s = '\0';
  }
  av[i] = NULL;

  if ( !PL_initialise(argc, av) )
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
{ LPTSTR cmdline;

  InitializeCriticalSection(&mutex);

  cmdline = GetCommandLine();

  return rlc_main(hInstance, hPrevInstance, cmdline, nCmdShow,
		  win32main, LoadIcon(hInstance, _T("SWI_Icon")));
}
