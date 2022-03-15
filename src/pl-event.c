/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2022, University of Amsterdam
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

#include "pl-incl.h"
#include "pl-comp.h"
#include "pl-event.h"
#include "pl-dbref.h"
#include "pl-copyterm.h"
#include "pl-tabling.h"
#include "pl-proc.h"
#include "pl-attvar.h"
#include "pl-fli.h"
#include "pl-trace.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Event interface
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	free_event_callback(event_callback *cb);

#ifdef O_PLMT
#define INIT_LIST_LOCK(l) recursiveMutexInit(&(l)->lock)
#define DELETE_LIST_LOCK(l) recursiveMutexDelete(&(l)->lock)
#define LOCK_LIST(l)   recursiveMutexLock(&(l)->lock)
#define UNLOCK_LIST(l) recursiveMutexUnlock(&(l)->lock)
#else
#define INIT_LIST_LOCK(l)
#define DELETE_LIST_LOCK(l)
#define LOCK_LIST(l)
#define UNLOCK_LIST(l)
#endif

#define EV_GLOBAL(name) (&GD->event.hook.name)
#define EV_LOCAL(name)  (&((PL_local_data_t*)NULL)->event.hook.name)
#define GEVENT(id, name, ac, loc) {id, name, ac, FALSE, EV_GLOBAL(loc) }
#define LEVENT(id, name, ac, loc) {id, name, ac, TRUE,  EV_LOCAL(loc)  }

const event_type PL_events[] =
{ GEVENT(PLEV_ABORT,            ATOM_abort,            0, onabort),
  GEVENT(PLEV_ERASED_CLAUSE,    ATOM_erase,            1, onerase),
  GEVENT(PLEV_ERASED_RECORD,    ATOM_erase,            1, onerase),
  GEVENT(PLEV_BREAK,            ATOM_break,            3, onbreak),
  GEVENT(PLEV_BREAK_EXISTS,     ATOM_break,            3, onbreak),
  GEVENT(PLEV_NOBREAK,          ATOM_break,            3, onbreak),
  GEVENT(PLEV_GCNOBREAK,        ATOM_break,            3, onbreak),
  GEVENT(PLEV_FRAMEFINISHED,    ATOM_frame_finished,   1, onframefinish),
  GEVENT(PLEV_UNTABLE,		ATOM_untable,          1, onuntable),
#ifdef O_PLMT
  GEVENT(PLEV_THREAD_START,     ATOM_thread_start,     1, onthreadstart),
  GEVENT(PLEV_THREAD_EXIT,      ATOM_thread_exit,      1, onthreadexit),
  LEVENT(PLEV_THIS_THREAD_EXIT, ATOM_this_thread_exit, 0, onthreadexit),
#endif
  {0}
};

static int
link_event(event_list *list, event_callback *cb, int last)
{ LOCK_LIST(list);
  if ( !list->head )
  { list->head = list->tail = cb;
  } else if ( last )
  { list->tail->next = cb;
    list->tail = cb;
  } else
  { cb->next = list->head;
    list->head = cb;
  }
  UNLOCK_LIST(list);

  return TRUE;
}


#define get_callback(closure, m, cb) LDFUNC(get_callback, closure, m, cb)
static int
get_callback(DECL_LD term_t closure, Module *m, term_t cb)
{ if ( !PL_strip_module(closure, m, cb) )
    return FALSE;
  if ( !PL_is_callable(cb) )
    return PL_type_error("callable", closure);

  return TRUE;
}


static int
add_event_hook(event_list *list, atom_t name, int last, term_t closure, int argc)
{ GET_LD
  Module m = NULL;
  event_callback *cb;
  atom_t pname;
  term_t t = PL_new_term_ref();

  if ( !get_callback(closure, &m, t) )
    return FALSE;

  if ( name )
  { event_callback *ev;

    LOCK_LIST(list);

    for(ev = list->head; ev; ev = ev->next)
    { if ( ev->name == name && !ev->function )
      { struct fastheap_term *r = ev->closure.term;

	ev->closure.term = term_to_fastheap(closure);
	if ( r )
	  free_fastheap(r);

	return TRUE;
      }
    }

    UNLOCK_LIST(list);
  }

  cb = PL_malloc(sizeof(*cb));
  memset(cb, 0, sizeof(*cb));
  if ( name )
  { cb->name = name;
    PL_register_atom(name);
  }
  cb->argc = argc;
  cb->module = m;

  if ( PL_get_atom(t, &pname) )
  { cb->procedure = resolveProcedure(PL_new_functor(pname, argc), m);
  } else
  { cb->procedure    = PL_predicate("call", argc+1, "system");
    cb->closure.term = term_to_fastheap(closure);
  }

  return link_event(list, cb, last);
}


static event_list *
get_event_list(event_list **list)
{ if ( !*list )
  { PL_LOCK(L_EVHOOK);
    if ( !*list )
    { event_list *l = PL_malloc(sizeof(*l));

      memset(l, 0, sizeof(*l));
      INIT_LIST_LOCK(l);
      *list = l;
    }
    PL_UNLOCK(L_EVHOOK);
  }

  return *list;
}

int
register_event_hook(event_list **list, atom_t name, int last, term_t closure, int argc)
{ return add_event_hook(get_event_list(list), name, last, closure, argc);
}


#define get_event_listp(type, listpp, argc) LDFUNC(get_event_listp, type, listpp, argc)
static int
get_event_listp(DECL_LD term_t type, event_list ***listpp, size_t *argc)
{ atom_t name;
  size_t arity;

  if ( PL_get_name_arity(type, &name, &arity) )
  { const event_type *et;
    Procedure proc;

    if ( arity == 0 )
    { for(et=PL_events; et->name; et++)
      { if ( et->name == name )
	{ assert(et->id == et-PL_events);
	  *listpp = event_list_location(et->id);
	  *argc   = et->argc;

	  return TRUE;
	}
      }
    } else if ( get_procedure(type, &proc, 0, GP_RESOLVE|GP_NAMEARITY) )
    { *listpp = &proc->definition->events;
      *argc   = 2;				/* action, cref */

      return TRUE;
    }

    return PL_domain_error("event", type);
  }

  return PL_type_error("callable", type);
}

static const opt_spec prolog_listen_options[] =
{ { ATOM_as,		 OPT_ATOM },
  { ATOM_name,		 OPT_ATOM },
  { NULL_ATOM,		 0 }
};

#define prolog_listen(type, closure, options) LDFUNC(prolog_listen, type, closure, options)
static int
prolog_listen(DECL_LD term_t type, term_t closure, term_t options)
{ event_list **listp;
  size_t argc;
  atom_t as = ATOM_first;
  atom_t name = 0;

  if ( options && !scan_options(options, 0, /*OPT_ALL,*/
				ATOM_prolog_listen_option, prolog_listen_options,
				&as, &name) )
    return FALSE;

  if ( !(as == ATOM_first || as == ATOM_last) )
  { term_t ex = PL_new_term_ref();
    return PL_put_atom(ex, as) && PL_domain_error("as", ex);
  }

  if ( get_event_listp(type, &listp, &argc) )
    return register_event_hook(listp, name, as == ATOM_last, closure, argc);

  return FALSE;
}

static
PRED_IMPL("prolog_listen", 2, prolog_listen, META)
{ PRED_LD

  return prolog_listen(A1, A2, 0);
}

static
PRED_IMPL("prolog_listen", 3, prolog_listen, META)
{ PRED_LD

  return prolog_listen(A1, A2, A3);
}


static
PRED_IMPL("prolog_unlisten", 2, prolog_unlisten, 0)
{ PRED_LD
  event_list **listp;
  size_t argc;

  if ( get_event_listp(A1, &listp, &argc) )
  { event_list *list;

    if ( (list = *listp) )
    { Module m = NULL;
      term_t t = PL_new_term_ref();
      atom_t name = 0;
      event_callback *ev, *next, *prev = NULL;
      fid_t fid = PL_open_foreign_frame();
      term_t tmp = PL_new_term_ref();
      Procedure proc = NULL;

      if ( !get_callback(A2, &m, t) )
	return FALSE;
      if ( PL_get_atom(t, &name) )
	proc = resolveProcedure(PL_new_functor(name, argc), m);

      LOCK_LIST(list);
      for(ev = list->head; ev; ev = next)
      { next = ev->next;

	if ( !ev->function )
	{ if ( proc )
	  { if ( ev->procedure->definition == proc->definition )
	    { delete:
	      if ( prev )
	      { prev->next = ev->next;
	      } else
	      { list->head = ev->next;
		if ( !list->head )
		  list->tail = NULL;
	      }
	      free_event_callback(ev);
	      continue;
	    }
	  } else if ( ev->closure.term )
	  { if ( put_fastheap(ev->closure.term, tmp) &&
		 PL_unify(A2, tmp) )
	      goto delete;
	    if ( PL_exception(0) )
	    { UNLOCK_LIST(list);
	      return FALSE;
	    }
	    PL_rewind_foreign_frame(fid);
	  }
	}

	prev = ev;
      }
      UNLOCK_LIST(list);
    }

    return TRUE;
  }

  return FALSE;
}


int
register_event_function(event_list **list, atom_t name, int last, int (*func)(),
			void *closure, int argc)
{ event_callback *cb = PL_malloc(sizeof(*cb));
  memset(cb, 0, sizeof(*cb));
  cb->argc = argc;
  cb->function = func;
  cb->closure.pointer = closure;

  return link_event(get_event_list(list), cb, last);
}


static void
free_event_callback(event_callback *cb)
{ if ( !cb->function && cb->closure.term )
    free_fastheap(cb->closure.term);
  if ( cb->name )
    PL_unregister_atom(cb->name);

  PL_free(cb);
}

void
destroy_event_list(event_list **listp)
{ event_list *list = *listp;

  if ( list )
  { event_callback *cb, *next;

    *listp = NULL;
    for(cb=list->head; cb; cb = next)
    { next = cb->next;
      free_event_callback(cb);
    }
    DELETE_LIST_LOCK(list);
    PL_free(list);
  }
}

void
cleanupEvents(void)
{ const event_type *ep;

  for(ep=PL_events; ep->name; ep++)
  { if ( !ep->local )
      destroy_event_list(ep->location);
  }
}



#define call_event_list(list, argc, argv) LDFUNC(call_event_list, list, argc, argv)
static int
call_event_list(DECL_LD event_list *list, int argc, term_t argv)
{ int rc = TRUE;

  if ( list )
  { event_callback *ev;

    LOCK_LIST(list);
    for(ev = list->head; ev; ev = ev->next)
    { if ( ev->function )
      { switch(argc)
	{ case 0:
	    rc = (*ev->function)(ev->closure.pointer);
	    break;
	  case 1:
	    rc = (*ev->function)(ev->closure.pointer, argv+1);
	    break;
	  case 2:
	    rc = (*ev->function)(ev->closure.pointer, argv+1, argv+2);
	    break;
	  default:
	    rc = FALSE;
	    assert(0);
	}
      } else if ( ev->closure.term )
      { rc = rc &&
	     ( put_fastheap(ev->closure.term, argv) &&
	       PL_call_predicate(ev->module, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION,
				 ev->procedure, argv) );
      } else
      { assert(ev->argc == argc);
	rc = rc && PL_call_predicate(NULL, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION,
				     ev->procedure, argv+1);
      }
      if ( !rc && PL_exception(0) )
	break;
    }
    UNLOCK_LIST(list);
  }

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
callEventHook() is used to call Prolog   in debugger related events that
happen in the system. In some cases,   these  events are generated while
the  system  holds  locks.  Such  code  should  call  delayEvents()  and
sendDelayedEvents(). These calls must be   properly  nested. Delaying is
currently only implemented for PLEV_BREAK and PLEV_NOBREAK.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct delayed_event
{ pl_event_type		type;		/* PLEV_* */
  union
  { struct
    { Clause clause;
      int    offset;
    } pc;
    struct
    { Clause clause;
    } clause;
    struct
    { Procedure proc;
    } proc;
  } value;
} delayed_event;


static int
delayEvent(pl_event_type ev, va_list args)
{ GET_LD

  if ( LD->event.buffered )
  { delayed_event dev;

    dev.type = ev;

    switch(ev)
    { case PLEV_BREAK_EXISTS:
      case PLEV_BREAK:
      case PLEV_NOBREAK:
      case PLEV_GCNOBREAK:
	dev.value.pc.clause = va_arg(args, Clause);
	dev.value.pc.offset = va_arg(args, int);
	acquire_clause(dev.value.pc.clause);
	break;
      case PLEV_ERASED_CLAUSE:
	dev.value.clause.clause = va_arg(args, Clause);
	acquire_clause(dev.value.pc.clause);
        break;
      case PLEV_UNTABLE:
	dev.value.proc.proc = va_arg(args, Procedure);
        break;
      default:
	assert(0);
    }

    addBuffer(LD->event.buffered, dev, delayed_event);
  }

  return TRUE;
}


int
delayEvents(void)
{ GET_LD

  if ( !LD->event.delay_nesting++ )
  { assert(!LD->event.buffered);

    if ( (LD->event.buffered = malloc(sizeof(tmp_buffer))) )
    { initBuffer(LD->event.buffered);
      return TRUE;
    }
  }

  return FALSE;
}


/* Returns
     -1: an exception occurred while sending events
      N: number of events sent
*/

int
sendDelayedEvents(int noerror)
{ GET_LD
  int sent = 0;

  if ( --LD->event.delay_nesting == 0 )
  { Buffer b = LD->event.buffered;
    delayed_event *dev = baseBuffer(b, delayed_event);
    int count = entriesBuffer(b, delayed_event);

    LD->event.buffered = NULL;

    for(; count-- > 0; dev++)
    { if ( noerror )
      { switch(dev->type)
	{ case PLEV_BREAK_EXISTS:
	  case PLEV_BREAK:
	  case PLEV_NOBREAK:
	  case PLEV_GCNOBREAK:
	    noerror = callEventHook(dev->type,
				    dev->value.pc.clause, dev->value.pc.offset);
	    sent++;
	    release_clause(dev->value.pc.clause);
	    break;
	  case PLEV_ERASED_CLAUSE:
	    noerror = callEventHook(dev->type, dev->value.clause.clause);
	    sent++;
	    release_clause(dev->value.pc.clause);
	    break;
	  case PLEV_UNTABLE:
	    noerror = callEventHook(dev->type, dev->value.proc.proc);
	    sent++;
	    break;
	  default:
	    assert(0);
	}
      }
    }

    discardBuffer(b);
    free(b);
  }

  return noerror ? sent	: -1;
}


int
PL_call_event_hook(pl_event_type ev, ...)
{ event_list **listp = event_list_location(ev);

  if ( *listp && GD->cleaning != CLN_DATA )
  { va_list args;
    int rc;

    va_start(args, ev);
    rc = PL_call_event_hook_va(ev, args);
    va_end(args);

    return rc;
  }

  return TRUE;
}


/* Returns FALSE iff there is an exception inside the execution of the
 * hook
 */

int
PL_call_event_hook_va(pl_event_type ev, va_list args)
{ GET_LD
  wakeup_state wstate;
  int rc = TRUE;
  event_list *list = *event_list_location(ev);
  term_t av;
  const event_type *event_decl = &PL_events[ev];

  if ( LD->event.delay_nesting )
  { delayEvent(ev, args);
    return TRUE;
  }

  if ( !saveWakeup(&wstate, TRUE) )
    return FALSE;
  av = PL_new_term_refs(event_decl->argc+1);

  switch(ev)
  { case PLEV_ABORT:
      break;
    case PLEV_ERASED_CLAUSE:
    { Clause cl = va_arg(args, Clause);		/* object erased */

      rc = PL_put_clref(av+1, cl);
      break;
    }
    case PLEV_ERASED_RECORD:
    { RecordRef r = va_arg(args, RecordRef);	/* object erased */

      rc = PL_unify_recref(av+1, r);
      break;
    }
    case PLEV_BREAK:
    case PLEV_BREAK_EXISTS:
    case PLEV_NOBREAK:
    case PLEV_GCNOBREAK:
    { Clause cl = va_arg(args, Clause);
      int offset = va_arg(args, int);

      rc = ( PL_put_atom(av+1,
			 ev == PLEV_BREAK     ? ATOM_true :
			 ev == PLEV_NOBREAK   ? ATOM_false :
			 ev == PLEV_GCNOBREAK ? ATOM_gc :
			                        ATOM_exist) &&
	     PL_put_clref(av+2, cl) &&
	     PL_put_intptr(av+3, offset) );
      break;
    }
    case PLEV_FRAMEFINISHED:
    { LocalFrame fr = va_arg(args, LocalFrame);

      rc = PL_put_frame(av+1, fr);
      break;
    }
#ifdef O_PLMT
    case PLEV_THREAD_START:
    case PLEV_THREAD_EXIT:
    { PL_thread_info_t *info = va_arg(args, PL_thread_info_t*);

      rc = unify_thread_id(av+1, info);
      break;
    }
    case PLEV_THIS_THREAD_EXIT:
      break;
#endif
    case PLEV_UNTABLE:
    { Procedure proc = va_arg(args, Procedure);
      rc = unify_definition(NULL, av+1, proc->definition,
			    0, GP_QUALIFY|GP_NAMEARITY);
      break;
    }
    default:
      rc = warning("callEventHook(): unknown event: %d", ev);
      goto out;
  }

  if ( rc )
  { rc = call_event_list(list, event_decl->argc, av);

    if ( !rc && PL_exception(0) )
      set(&wstate, WAKEUP_KEEP_URGENT_EXCEPTION);
    else
      rc = TRUE;				/* only FALSE on error */
  }

out:
  restoreWakeup(&wstate);

  return rc;
}


int
predicate_update_event(DECL_LD Definition def, atom_t action, Clause cl,
		       unsigned flags)
{ wakeup_state wstate;
  int rc;

  if ( (rc=saveWakeup(&wstate, TRUE)) )
  { term_t av;

    rc = ( (av=PL_new_term_refs(3)) && /* closure, action, clause */
	   PL_put_atom(av+1, action) &&
	   PL_put_clref(av+2, cl) );

    if ( rc && (flags&P_EVENT_ROLLBACK) )
      rc = PL_cons_functor(av+1, FUNCTOR_rollback1, av+1);

    if ( rc )
      rc = call_event_list(def->events, 2, av);

    restoreWakeup(&wstate);
  }

  return rc;
}

int
table_answer_event(DECL_LD Definition def, atom_t action, term_t answer)
{ wakeup_state wstate;
  int rc;

  if ( (rc=saveWakeup(&wstate, TRUE)) )
  { term_t av;

    rc = ( (av=PL_new_term_refs(3)) && /* closure, action, answer */
	   PL_put_atom(av+1, action) &&
	   PL_put_term(av+2, answer) );

    if ( rc )
    { tbl_status tblstat;

      save_tabling_status(&tblstat);
      rc = call_event_list(def->events, 2, av);
      restore_tabling_status(&tblstat);
    }

    restoreWakeup(&wstate);
  }

  return rc;
}


int
retractall_event(DECL_LD Definition def, term_t head, functor_t start)
{ wakeup_state wstate;
  term_t av;
  int rc = TRUE;

  if ( !saveWakeup(&wstate, TRUE) )
    return FALSE;
  av = PL_new_term_refs(4);			/* closure, action, start/end, head */

  if ( !PL_put_atom(av+1, def->module->name) ||
       !PL_put_term(av+2, head) ||
       !PL_cons_functor_v(av+2, FUNCTOR_colon2, av+1) ||
       !PL_cons_functor_v(av+2, start, av+2) ||
       !PL_put_atom(av+1, ATOM_retractall) )
    return FALSE;

  rc = call_event_list(def->events, 2, av);

  restoreWakeup(&wstate);

  return rc;
}



		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define META PL_FA_TRANSPARENT

BeginPredDefs(event)
  PRED_DEF("prolog_listen",   2, prolog_listen,   META)
  PRED_DEF("prolog_listen",   3, prolog_listen,   META)
  PRED_DEF("prolog_unlisten", 2, prolog_unlisten, META)
EndPredDefs
