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
#endif

typedef struct
{ module_t module;			/* module to call in */
  record_t goal;			/* the term to call */
} prolog_goal;


typedef struct
{ int		     owner;		/* owning thread */
  PL_dispatch_hook_t hook;		/* saved Prolog dispatch hook */
  int		     pipe[2];		/* pipe to talk to main process */
} dispatch_context;

static dispatch_context context;

static void
undispatch(void *closure)
{ DLOCK();

  if ( context.owner )
  { PL_dispatch_hook(context.hook);
    context.hook = NULL;

    if ( context.pipe[0] >= 0 )
    { close(context.pipe[0]);
      context.pipe[0] = -1;
    }
    if ( context.pipe[1] >= 0 )
    { close(context.pipe[1]);
      context.pipe[1] = -1;
    }

    context.owner = 0;
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


static foreign_t
pl_pce_dispatch(term_t options)
{ DLOCK();

  if ( context.owner )
    return permission_error("dispatch_loop", "create", "pce");

  if ( pipe(context.pipe) == -1 )
    return resource_error("open_files");

  context.owner = PL_thread_self();
  context.hook = PL_dispatch_hook(NULL);
  DUNLOCK();

				/* force creation of application context */
  pceXtAppContext(NULL);
  pceExistsAssoc(cToPceName("display_manager"));

  pthread_cleanup_push(undispatch, &context);
  for(;;)
  { Sdprintf("Dispatch\n");
    if ( pceDispatch(context.pipe[0], 0) == PCE_DISPATCH_INPUT )
    { prolog_goal g;
      int n;

      if ( (n=read(context.pipe[0], &g, sizeof(g))) == sizeof(g) )
      { fid_t fid = PL_open_foreign_frame();
	term_t t = PL_new_term_ref();
	static predicate_t pred = NULL;

	if ( !pred )
	  pred = PL_predicate("call", 1, "user");

	PL_recorded(g.goal, t);
	PL_erase(g.goal);
	PL_call_predicate(g.module, PL_Q_NORMAL, pred, t);
	PL_discard_foreign_frame(fid);
      } else if ( n == 0 )		/* EOF: quit */
      { break;
      }
    }
  }
  pthread_cleanup_pop(0);

  undispatch(&context);

  return TRUE;
}


static foreign_t
pl_pce_call(term_t goal)
{ DLOCK();

  if ( context.pipe[1] >= 0 )
  { term_t plain = PL_new_term_ref();
    prolog_goal g;
    g.module = NULL;

    PL_strip_module(goal, &g.module, plain);
    if ( !(PL_is_compound(plain) || PL_is_atom(plain)) )
    { DUNLOCK();
      return type_error(goal, "callable");
    }
    g.goal = PL_record(plain);

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

    close(fd);
    return TRUE;
  }
  DUNLOCK();

  return FALSE;
}


install_t
pce_install_dispatch()
{ context.pipe[0] = -1;
  context.pipe[1] = -1;

  PL_register_foreign("pce_dispatch",     1, pl_pce_dispatch, 0);
  PL_register_foreign("pce_end_dispatch", 0, pl_pce_end_dispatch, 0);
  PL_register_foreign("pce_call",         1, pl_pce_call, 0);
}
