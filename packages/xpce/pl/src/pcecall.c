/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <h/interface.h>

#ifdef WIN32

#include <windows.h>

#else /*WIN32*/

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#define HAVE_UNISTD_H 1

#endif /*WIN32*/

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef _REENTRANT
#include <pthread.h>

static pthread_mutex_t pce_dispatch_mutex = PTHREAD_MUTEX_INITIALIZER;
#define DLOCK() pthread_mutex_lock(&pce_dispatch_mutex)
#define DUNLOCK() pthread_mutex_unlock(&pce_dispatch_mutex)
#else
#define DLOCK()
#define DUNLOCK()
#define pthread_cleanup_push(h,a)
#define pthread_cleanup_pop(e)
#endif

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif


		 /*******************************
		 *	       TYPES		*
		 *******************************/

typedef struct
{ module_t module;			/* module to call in */
  record_t goal;			/* the term to call */
} prolog_goal;


typedef struct
{
#ifdef WIN32
  HINSTANCE	hinstance;
  HWND		window;
#else /*WIN32*/
  int		pipe[2];
  XtInputId 	id;
#endif /*WIN32*/
} context_t;

static int init_prolog_goal(prolog_goal *g, term_t goal);
static void call_prolog_goal(prolog_goal *g);

static context_t context;


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static int
resource_error(const char *error)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "resource_error", 1,
		        PL_CHARS, error,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
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

#ifdef WIN32

		 /*******************************
		 *	  WINDOWS SOLUTION	*
		 *******************************/

#define WM_CALL	(WM_USER+56)

static int WINAPI
call_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ switch( message )
  { case WM_CALL:
    { prolog_goal *g = (prolog_goal *)lParam;

      call_prolog_goal(g);
      PL_free(g);
      pceRedraw(FALSE);

      return 0;
    }
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}

static char *
HiddenFrameClass()
{ static char *name;
  static WNDCLASS wndClass;

  if ( !name )
  { char buf[50];

    context.hinstance = GetModuleHandle("xpce2pl");
    sprintf(buf, "PceCallWin%d", (int)context.hinstance);
    name = strdup(buf);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) call_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= context.hinstance;
    wndClass.hIcon		= NULL;
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= name;

    RegisterClass(&wndClass);
  }

  return name;
}


static void
unsetup(int code, void *closure)
{ if ( context.window )
  { DestroyWindow(context.window);
    context.window = 0;
  }
}


static int
setup()
{ if ( context.window )
    return TRUE;
  
  DLOCK();
  if ( !context.window )
  { context.window = CreateWindow(HiddenFrameClass(),
				  "XPCE/SWI-Prolog call window",
				  WS_POPUP,
				  0, 0, 32, 32,
				  NULL, NULL, context.hinstance, NULL);
    PL_on_halt(unsetup, NULL);
  }
  DUNLOCK();

  return TRUE;
}


static foreign_t
pl_pce_call(term_t goal)
{ prolog_goal *g = PL_malloc(sizeof(*g));
  int rc;

  if ( !init_prolog_goal(g, goal) )
  { PL_free(g);
    return FALSE;
  }

  PostMessage(context.window, WM_CALL, (WPARAM)0, (LPARAM)g);

  return TRUE;
}


#else /*WIN32*/

		 /*******************************
		 *	   X11 SCHEDULING	*
		 *******************************/

static void
on_input(XtPointer xp, int *source, XtInputId *id)
{ context_t *ctx = (context_t *)xp;
  prolog_goal g;
  int n;

  if ( (n=read(ctx->pipe[0], &g, sizeof(g))) == sizeof(g) )
  { call_prolog_goal(&g);
  } else if ( n == 0 )		/* EOF: quit */
  { close(ctx->pipe[0]);
    ctx->pipe[0] = -1;
  }
}


static int
setup()
{ if ( context.pipe[0] > 0 )
    return TRUE;

  DLOCK();
  if ( context.pipe[0] == -1 )
  { if ( pipe(context.pipe) == -1 )
    { DUNLOCK();
      return resource_error("open_files");
    }

    context.id = XtAppAddInput(pceXtAppContext(NULL),
			       context.pipe[0],
			       (XtPointer)(XtInputReadMask),
			       on_input, &context);
  }
  DUNLOCK();

  return TRUE;
}
  

static foreign_t
pl_pce_call(term_t goal)
{ prolog_goal g;
  int rc;

  if ( !setup() )
    return FALSE;

  if ( !init_prolog_goal(&g, goal) )
    return FALSE;

					/* must be locked? */
  rc = write(context.pipe[1], &g, sizeof(g));

  if ( rc == sizeof(g) )
    return TRUE;

  return FALSE;
}

#endif /*WIN32*/


		 /*******************************
		 *	CREATE/EXECUTE GOAL	*
		 *******************************/

static int
init_prolog_goal(prolog_goal *g, term_t goal)
{ term_t plain = PL_new_term_ref();

  g->module = NULL;
  PL_strip_module(goal, &g->module, plain);
  if ( !(PL_is_compound(plain) || PL_is_atom(plain)) )
    return type_error(goal, "callable");
  g->goal = PL_record(plain);

  return TRUE;
}


static void
call_prolog_goal(prolog_goal *g)
{ fid_t fid = PL_open_foreign_frame();
  term_t t = PL_new_term_ref();
  static predicate_t pred = NULL;

  if ( !pred )
    pred = PL_predicate("call", 1, "user");

  PL_recorded(g->goal, t);
  PL_erase(g->goal);
  PL_call_predicate(g->module, PL_Q_NORMAL, pred, t);
  PL_discard_foreign_frame(fid);
}


		 /*******************************
		 *	       INSTALL		*
		 *******************************/

install_t
install_pcecall()
{
#ifdef WIN32
  if ( PL_thread_self() != 1 )
    PL_warning("in_pce_thread/1 must be loaded from main thread");
  setup();
#else
  context.pipe[0] = context.pipe[1] = -1;
#endif

  PL_register_foreign("in_pce_thread", 1, pl_pce_call, PL_FA_TRANSPARENT);
}
