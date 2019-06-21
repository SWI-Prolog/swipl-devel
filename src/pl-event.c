/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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
#include "pl-event.h"
#include "pl-dbref.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Event interface
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	  PROLOG EVENT HOOK	*
		 *******************************/

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
	break;
      case PLEV_ERASED_CLAUSE:
	dev.value.clause.clause = va_arg(args, Clause);
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
	    break;
	  case PLEV_ERASED_CLAUSE:
	    noerror = callEventHook(dev->type, dev->value.clause.clause);
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
{ if ( PROCEDURE_event_hook1->definition->impl.any.defined &&
       GD->cleaning != CLN_DATA )
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
  int rc;
  term_t arg;

  if ( LD->event.delay_nesting )
  { delayEvent(ev, args);
    return TRUE;
  }

  if ( !saveWakeup(&wstate, TRUE PASS_LD) )
    return FALSE;
  arg = PL_new_term_ref();

  switch(ev)
  { case PLEV_ABORT:
    { rc = PL_unify_atom(arg, ATOM_abort);
      break;
    }
    case PLEV_ERASED_CLAUSE:
    { Clause cl = va_arg(args, Clause);		/* object erased */
      term_t dbref;

      rc = (  (dbref = PL_new_term_ref()) &&
	      PL_unify_clref(dbref, cl) &&
	      PL_unify_term(arg,
			    PL_FUNCTOR, FUNCTOR_erased1,
			      PL_TERM, dbref)
	   );
      break;
    }
    case PLEV_ERASED_RECORD:
    { RecordRef r = va_arg(args, RecordRef);	/* object erased */
      term_t dbref;

      rc = (  (dbref = PL_new_term_ref()) &&
	      PL_unify_recref(dbref, r) &&
	      PL_unify_term(arg,
			    PL_FUNCTOR, FUNCTOR_erased1,
			      PL_TERM, dbref)
	   );
      break;
    }
    case PLEV_DEBUGGING:
    { int dbg = va_arg(args, int);

      rc = PL_unify_term(arg,
			 PL_FUNCTOR, FUNCTOR_debugging1,
			   PL_ATOM, dbg ? ATOM_true : ATOM_false);
      break;
    }
    case PLEV_TRACING:
    { int trc = va_arg(args, int);

      rc = PL_unify_term(arg,
			 PL_FUNCTOR, FUNCTOR_tracing1,
			   PL_ATOM, trc ? ATOM_true : ATOM_false);
      break;
    }
    case PLEV_BREAK:
    case PLEV_BREAK_EXISTS:
    case PLEV_NOBREAK:
    case PLEV_GCNOBREAK:
    { Clause clause = va_arg(args, Clause);
      int offset = va_arg(args, int);
      term_t cref;

      rc = ( (cref = PL_new_term_ref()) &&
	     PL_unify_clref(cref, clause) &&
	     PL_unify_term(arg,
			   PL_FUNCTOR, FUNCTOR_break3,
			     PL_TERM, cref,
			     PL_INT, offset,
			     PL_ATOM, ev == PLEV_BREAK     ? ATOM_true :
				      ev == PLEV_NOBREAK   ? ATOM_false :
				      ev == PLEV_GCNOBREAK ? ATOM_gc :
							     ATOM_exist)
	   );
      break;
    }
    case PLEV_FRAMEFINISHED:
    { LocalFrame fr = va_arg(args, LocalFrame);
      term_t ref = PL_new_term_ref();

      rc = ( (ref = PL_new_term_ref()) &&
	     (PL_put_frame(ref, fr),TRUE) &&
	     PL_unify_term(arg,
			   PL_FUNCTOR, FUNCTOR_frame_finished1,
			     PL_TERM, ref)
	   );
      break;
    }
#ifdef O_PLMT
    case PL_EV_THREADFINISHED:
    { PL_thread_info_t *info = va_arg(args, PL_thread_info_t*);
      term_t id;

      rc = ( (id = PL_new_term_ref()) &&
	     unify_thread_id(id, info) &&
	     PL_unify_term(arg,
			   PL_FUNCTOR_CHARS, "thread_finished", 1,
			     PL_TERM, id)
	   );

      break;
    }
#endif
    default:
      rc = warning("callEventHook(): unknown event: %d", ev);
      goto out;
  }

  if ( rc )
  { rc = PL_call_predicate(MODULE_user, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION,
			   PROCEDURE_event_hook1, arg);
    if ( !rc && PL_exception(0) )
      set(&wstate, WAKEUP_KEEP_URGENT_EXCEPTION);
    else
      rc = TRUE;				/* only FALSE on error */
  }

out:
  restoreWakeup(&wstate PASS_LD);

  return rc;
}
