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

#ifndef streq
#define streq(s, q) (strcmp(s, q) == 0)
#endif

typedef struct
{ module_t module;			/* module to call in */
  record_t goal;			/* the term to call */
} prolog_goal;


typedef struct
{ int		     owner;		/* owning thread */
#ifdef WIN32
  HWND		     window;		/* Window for pce_call/1 */
  HINSTANCE	     hinstance;		/* Our instance */
#else
  int		     pipe[2];		/* pipe to talk to main process */
#endif
  int		     flags;		/* general options */
  PL_dispatch_hook_t hook;		/* saved Prolog dispatch hook */
  PL_thread_attr_t   thread_options;	/* options for the thread */
} dispatch_context;

#define DISPATCH_CONSOLE 0x0001		/* Attach a console window */
#define DISPATCH_END	 0x0002		/* Break out of the loop */

static dispatch_context context;

static int end_dispatch(int id);	/* stop dispatch-loop */
static int init_prolog_goal(prolog_goal *g, term_t goal);
static void call_prolog_goal(prolog_goal *g);

static void
undispatch(void *closure)
{ dispatch_context *ctx = closure;

  DLOCK();

  if ( ctx->owner )
  { ctx->hook = NULL;

#ifdef WIN32
    if ( ctx->window )
    { DestroyWindow(ctx->window);
      ctx->window = 0;
    }
#else
    if ( ctx->pipe[0] >= 0 )
    { close(ctx->pipe[0]);
      ctx->pipe[0] = -1;
    }
    if ( ctx->pipe[1] >= 0 )
    { close(ctx->pipe[1]);
      ctx->pipe[1] = -1;
    }
#endif

    ctx->owner = 0;
  }

  DUNLOCK();
}


static int
permission_error(const char *type, const char *op, const char *obj)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "permission_error", 3,
		        PL_CHARS, type,
		        PL_CHARS, op,
		        PL_CHARS, obj,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


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

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_type_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
domain_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_domain_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}

#ifdef WIN32

#define WM_CALL (WM_USER+1)
#define WM_END  (WM_USER+2)

static int WINAPI
dispatch_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ switch( message )
  { case WM_CALL:
    { prolog_goal *g = (prolog_goal *)lParam;
      call_prolog_goal(g);
      PL_free(g);
      return 0;
    }
    case WM_END:
    { dispatch_context *ctx = (dispatch_context*)lParam;

      ctx->flags |= DISPATCH_END;
      return 0;
    }
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}

static char *
DispatchFrameClass()
{ static char *name;
  static WNDCLASS wndClass;

  if ( !name )
  { char buf[50];

    context.hinstance = GetModuleHandle("pl2xpce");
    sprintf(buf, "DispatchWin%d", (int)context.hinstance);
    name = strdup(buf);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) dispatch_wnd_proc;
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


static HWND
CreateDispatchWindow(dispatch_context *ctx)
{ if ( !ctx->window )
  { ctx->window = CreateWindow(DispatchFrameClass(),
			       "XPCE dispatch window",
			       WS_POPUP,
			       0, 0, 32, 32,
			       NULL, NULL, context.hinstance, NULL);
    assert(ctx->window);
  }

  return ctx->window;
}


static void
dispatch(dispatch_context *context)
{ MSG msg;
  
  CreateDispatchWindow(context);

  while( !(context->flags & DISPATCH_END) &&
	 GetMessage(&msg, NULL, 0, 0) )
  { TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
}

static foreign_t
pl_pce_end_dispatch()
{ if ( context.window )
  { if ( PostMessage(context.window, WM_END, 0, (LPARAM)&context) )
      return TRUE;
  }

  return FALSE;
}

static foreign_t
pl_pce_call(term_t goal)
{ DLOCK();

  if ( context.window )
  { prolog_goal *g = PL_malloc(sizeof(*g));

    if ( !init_prolog_goal(g, goal) )
    { DUNLOCK();
      return FALSE;
    }
    if ( PostMessage(context.window, WM_CALL, 0, (LPARAM)g) )
    { DUNLOCK();
      return TRUE;
    }
  }

  DUNLOCK();
  return FALSE;
}

#else /*WIN32*/

static int
input_on_fd(int fd)
{ fd_set rfds;
  struct timeval tv;

  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  return select(fd+1, &rfds, NULL, NULL, &tv) != 0;
}


static void
dispatch(dispatch_context *context)
{ pthread_cleanup_push(undispatch, context);

  for(;;)
  { DEBUG(Sdprintf("Dispatch\n"));

    if ( pceDispatch(context->pipe[0], 250) == PCE_DISPATCH_INPUT &&
	 input_on_fd(context->pipe[0]) )
    { prolog_goal g;
      int n;

      if ( (n=read(context->pipe[0], &g, sizeof(g))) == sizeof(g) )
      { call_prolog_goal(&g);
      } else if ( n == 0 )		/* EOF: quit */
      { break;
      }
    }
  }

  DEBUG(Sdprintf("dispatch loop ended\n"));

  pthread_cleanup_pop(0);
  undispatch(context);
}


static foreign_t
pl_pce_call(term_t goal)
{ DLOCK();

  if ( context.pipe[1] >= 0 )
  { prolog_goal g;

    if ( !init_prolog_goal(&g, goal) )
    { DUNLOCK();
      return FALSE;
    }

    if ( write(context.pipe[1], &g, sizeof(g)) == sizeof(g) )
    { DUNLOCK();
      return TRUE;
    }
  }
  DUNLOCK();

  return FALSE;
}


static foreign_t
pl_pce_end_dispatch()
{ int fd;

  DLOCK();
  if ( (fd=context.pipe[1]) >= 0 )
  { context.pipe[1] = -1;
    DUNLOCK();

    PL_dispatch_hook(context.hook);
    close(fd);

    return TRUE;
  }
  DUNLOCK();

  return FALSE;
}

#endif /*WIN32*/

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


static int
end_dispatch(int id)
{ DEBUG(Sdprintf("Close down %d\n", id));

  pl_pce_end_dispatch();
#ifdef HAVE_SCHED_YIELD
  sched_yield();
#endif

  return TRUE;
}


static void *
dispatch_thread_function(void *closure)
{ dispatch_context *ctx = closure;

  ctx->thread_options.cancel = end_dispatch;
  PL_thread_attach_engine(&ctx->thread_options);
  if ( ctx->flags & DISPATCH_CONSOLE )
    PL_action(PL_ACTION_ATTACH_CONSOLE);
  dispatch(ctx);
  PL_thread_destroy_engine();

  return NULL;
}


static int
set_options(dispatch_context *ctx, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg = PL_new_term_ref();

  memset(&ctx->thread_options, 0, sizeof(ctx->thread_options));
  ctx->thread_options.alias = "pce";

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( PL_get_name_arity(head, &name, &arity) && arity == 1 )
    { long v;
      int b;
      const char *s = PL_atom_chars(name);

      PL_get_arg(1, head, arg);

      if ( streq(s, "console") )
      { if ( !PL_get_bool(arg, &b) )
	  return type_error(arg, "boolean");
	ctx->flags |= DISPATCH_CONSOLE;
	continue;
      }

      if ( !PL_get_long(arg, &v) )
	return type_error(arg, "integer");

      if ( streq(s, "local") )
	ctx->thread_options.local_size = v;
      else if ( streq(s, "global") )
	ctx->thread_options.global_size = v;
      else if ( streq(s, "trail") )
	ctx->thread_options.trail_size = v;
      else
	return domain_error(head, "thread_option");
    } else
      return domain_error(head, "thread_option");
  }

  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  return TRUE;
}



static foreign_t
pl_pce_dispatch(term_t options)
{ DLOCK();

  if ( context.owner )
    return permission_error("dispatch_loop", "create", "pce");
  context.flags = 0;
  if ( !set_options(&context, options) )
    return FALSE;

#ifndef WIN32
  if ( pipe(context.pipe) == -1 )
    return resource_error("open_files");
#endif

  context.owner = PL_thread_self();
  context.hook = PL_dispatch_hook(NULL);
  DUNLOCK();

				/* force creation of application context */
#ifndef WIN32
  pceXtAppContext(NULL);
  pceExistsAssoc(cToPceName("display_manager"));
#endif

#ifdef _REENTRANT
  if ( context.owner > 0 )		/* threaded environment */
  { pthread_t tid;
    pthread_attr_t attr;

    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

    pthread_create(&tid, &attr, dispatch_thread_function, &context);
  } else
#endif
  { dispatch(&context);
  }

  return TRUE;
}


install_t
pce_install_dispatch()
{
#ifndef WIN32
  context.pipe[0] = -1;
  context.pipe[1] = -1;
#endif

  PL_register_foreign("pce_dispatch",     1, pl_pce_dispatch, 0);
  PL_register_foreign("pce_end_dispatch", 0, pl_pce_end_dispatch, 0);
  PL_register_foreign("pce_call",         1, pl_pce_call, 0);
}
