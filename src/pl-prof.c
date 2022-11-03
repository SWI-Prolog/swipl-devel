/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2019, University of Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-comp.h"
#include "pl-prof.h"
#include "pl-wam.h"
#include "pl-setup.h"
#include "pl-fli.h"
#include "pl-proc.h"
#include "pl-gc.h"
#include "pl-util.h"
#include "pl-pro.h"
#include "pl-modul.h"

#undef LD
#define LD LOCAL_LD

#ifdef O_PROFILE

#define PROFTYPE_MAGIC 0x639a2fb1

#if USE_LD_MACROS
#define	profile(count)		LDFUNC(profile, count)
#define	thread_prof_ticks(_)	LDFUNC(thread_prof_ticks, _)
#endif

#define LDFUNC_DECLARATIONS

static int  identify_def(term_t t, void *handle);
static int  get_def(term_t t, void **handle);
static void prof_release_def(void *handle);
static void profile(intptr_t count);
static int  thread_prof_ticks(void);

#undef LDFUNC_DECLARATIONS

static PL_prof_type_t prof_default_type =
{ identify_def,					/* unify a Definition */
  get_def,					/* Find a definition */
  NULL,						/* Acquire */
  prof_release_def,				/* Release a definition */
  .magic = PROFTYPE_MAGIC
};

#define MAX_PROF_TYPES 10
static PL_prof_type_t *types[MAX_PROF_TYPES] = { &prof_default_type };

#define PROFNODE_MAGIC 0x7ae38f24

typedef struct call_node
{ intptr_t	    magic;		/* PROFNODE_MAGIC */
  struct call_node *parent;
  void *            handle;		/* handle to procedure-id */
  PL_prof_type_t   *type;
  uintptr_t	    calls;		/* Calls from the parent */
  uintptr_t	    redos;		/* redos while here */
  uintptr_t	    exits;		/* exits to the parent */
  uintptr_t	    recur;		/* recursive calls */
  uintptr_t	    ticks;		/* time-statistics */
  uintptr_t	    sibling_ticks;	/* ticks in a siblings */
  struct call_node *next;		/* next in siblings chain */
  struct call_node *siblings;		/* my offspring */
} call_node;

#if USE_LD_MACROS
#define	collectSiblingsTime(_)	LDFUNC(collectSiblingsTime, _)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static void	freeProfileData(void);
static void	collectSiblingsTime(void);

#undef LDFUNC_DECLARATIONS

#define WITH_LD_IF_PROFILING(_)		WITH_LD(GD->profile.thread) if(LD _)
#define WITH_LD_IF_PROFILING_AND(cond)	WITH_LD_IF_PROFILING( && (cond))

int
activateProfiler(DECL_LD prof_status active)
{ int i;
  PL_local_data_t *profiling;

  PL_LOCK(L_THREAD);

  if ( active && (profiling=GD->profile.thread) )
  { term_t tid = PL_new_term_ref();
    char msg[100];

    PL_unify_thread_id(tid, LD->thread.info->pl_tid);
    Ssprintf(msg, "Already profiling thread %d",
	     profiling->thread.info->pl_tid);

    PL_UNLOCK(L_THREAD);

    return PL_error(NULL, 0, msg, ERR_PERMISSION,
		    ATOM_profile, ATOM_thread, tid);
  }

  LD->profile.active = active;
  for(i=0; i<MAX_PROF_TYPES; i++)
  { if ( types[i] && types[i]->activate )
      (*types[i]->activate)(active);
  }

  if ( active )
  { LD->profile.time_at_last_tick =
    LD->profile.time_at_start     = active == PROF_CPU
					? ThreadCPUTime(CPU_USER)
					: WallTime();

    GD->profile.thread = LD;
  } else
  { GD->profile.thread = NULL;
  }

  PL_UNLOCK(L_THREAD);

  updateAlerted(LD);

  LD->profile.sum_ok = FALSE;

  return TRUE;
}


static int
thread_prof_ticks(DECL_LD)
{ double t0 = LD->profile.time_at_last_tick;
  double t1 = LD->profile.active == PROF_CPU ? ThreadCPUTime(CPU_USER)
				             : WallTime();

  LD->profile.time_at_last_tick = t1;

  DEBUG(MSG_PROF_TICKS,
	Sdprintf("%d ms\n", (int)((t1-t0)*1000.0)));

  return (int)((t1-t0)*1000.0);			/* milliseconds */
}


#ifdef __WINDOWS__

static UINT         timer;			/* our MM timer */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MS-Windows version
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if (_MSC_VER < 1400) && !defined(__MINGW32__)
typedef DWORD DWORD_PTR;
#endif

static void CALLBACK
callTimer(UINT id, UINT msg, DWORD_PTR dwuser, DWORD_PTR dw1, DWORD_PTR dw2)
{ WITH_LD_IF_PROFILING()
  { int newticks;

    if ( (newticks = thread_prof_ticks()) )
    { if ( newticks < 0 )			/* Windows 95/98/... */
	newticks = 1;
    }
    profile(newticks);
  }
}


static bool
startProfiler(prof_status how)
{ GET_LD
  MMRESULT rval;
  (void)how;

  rval = timeSetEvent(10,
		      5,		/* resolution (milliseconds) */
		      callTimer,
		      (DWORD_PTR)0,
		      TIME_PERIODIC);

  if ( rval )
    timer = rval;
  else
    return PL_error(NULL, 0, NULL, ERR_SYSCALL, "timeSetEvent");

  return activateProfiler(PROF_CPU);
}


void
stopItimer(void)
{ if ( timer )
  { timeKillEvent(timer);
    timer = 0;
  }
}

#else /*__WINDOWS__*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
POSIX version
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

static struct itimerval value, ovalue;	/* itimer controlling structures */
static int itimer = -1;			/* ITIMER_* */
static int timer_signal;		/* SIG* */

static void
sig_profile(int sig)
{ (void)sig;

#if !defined(BSD_SIGNALS) && !defined(HAVE_SIGACTION)
  signal(SIGPROF, sig_profile);
#endif

  WITH_LD_IF_PROFILING()
  { int newticks;

    if ( (newticks = thread_prof_ticks()) )
    { if ( newticks < 0 )			/* Windows 95/98/... */
	newticks = 1;
    }
    profile(newticks);
  }
}


static bool
startProfiler(prof_status how)
{ GET_LD
  int sig, timer;

  if ( how == PROF_CPU )
  { sig   = SIGPROF;
    timer = ITIMER_PROF;
  } else
  { sig   = SIGALRM;
    timer = ITIMER_REAL;
  }

  set_sighandler(sig, sig_profile);
  timer_signal = sig;

  value.it_interval.tv_sec  = 0;
  value.it_interval.tv_usec = 5000;		/* 5ms for real; also ok for cpu */
  value.it_value.tv_sec  = 0;			/* on systems where 0 means now */
  value.it_value.tv_usec = 5000;

  if ( setitimer(timer, &value, &ovalue) != 0 )
    return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, setitimer);

  itimer = timer;

  return activateProfiler(how);
}

void
stopItimer(void)
{ if ( itimer != -1 )
  { value.it_interval.tv_sec  = 0;
    value.it_interval.tv_usec = 0;
    value.it_value.tv_sec  = 0;
    value.it_value.tv_usec = 0;

    if ( setitimer(itimer, &value, &ovalue) != 0 )
    { warning("Failed to stop interval timer: %s", OsError());
      return;
    }

    itimer = -1;
  }
}

#endif /*__WINDOWS__*/

static int
stopProfiler(void)
{ WITH_LD_IF_PROFILING_AND(LD->profile.active)
  { double tend = LD->profile.active == PROF_CPU ? ThreadCPUTime(CPU_USER)
						 : WallTime();

    LD->profile.time += tend - LD->profile.time_at_start;

    stopItimer();
    activateProfiler(PROF_INACTIVE);
#ifndef __WINDOWS__
    set_sighandler(timer_signal, SIG_IGN);
    timer_signal = 0;
#endif
  }

  return TRUE;
}


/** profiler(-Old, +New)

Unify Old with the state of the profiler and set it according to New.
*/

static int
get_prof_status(term_t t, prof_status *s)
{ GET_LD
  atom_t a;

  if ( PL_get_atom_ex(t, &a) )
  { switch(a)
    { case ATOM_false:
	*s = PROF_INACTIVE;
        return TRUE;
      case ATOM_true:
      case ATOM_cputime:
	*s = PROF_CPU;
        return TRUE;
      case ATOM_walltime:
	*s = PROF_WALL;
        return TRUE;
      default:
	PL_domain_error("profile_status", t);
        return FALSE;
    }
  }

  return FALSE;
}


static
PRED_IMPL("profiler", 2, profiler, 0)
{ PRED_LD
  prof_status val;

  if ( !PL_unify_atom(A1,
		      LD->profile.active == PROF_INACTIVE ? ATOM_false :
		      LD->profile.active == PROF_CPU ? ATOM_cputime :
		      ATOM_walltime) )
    return FALSE;
  if ( PL_compare(A1, A2) == 0 )
    return TRUE;
  if ( !get_prof_status(A2, &val) )
    return FALSE;
  if ( val == LD->profile.active )
    succeed;

  if ( val )
    return startProfiler(val);
  else
    return stopProfiler();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Prolog query API:

$prof_sibling_of(?Child, ?Parent)
	Generate hierachy.  If Parent is '-', generate the roots

$prof_node(+Node, -Pred, -Calls, -Redos, -Exits,
	   -Recursive, -Ticks, -SiblingTicks)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define get_node(t, node) LDFUNC(get_node, t, node)
static int
get_node(DECL_LD term_t t, call_node **node)
{ if ( PL_is_functor(t, FUNCTOR_dprof_node1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    _PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &ptr) )
    { call_node *n = ptr;

      if ( n->magic == PROFNODE_MAGIC )
      { *node = n;
        succeed;
      }
    }
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_profile_node, t);
}


#define unify_node(t, node) LDFUNC(unify_node, t, node)
static int
unify_node(DECL_LD term_t t, call_node *node)
{ return PL_unify_term(t,
		       PL_FUNCTOR, FUNCTOR_dprof_node1,
		         PL_POINTER, node);
}


static
PRED_IMPL("$prof_sibling_of", 2, prof_sibling_of, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  call_node *parent = NULL;
  call_node *sibling = NULL;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { atom_t a;

      if ( !PL_is_variable(A1) )
      { if ( get_node(A1, &sibling) )
	{ if ( sibling->parent )
	    return unify_node(A2, sibling->parent);
	}
	fail;
      } else
      { if ( PL_get_atom(A2, &a) && a == ATOM_minus )
	  sibling = LD->profile.roots;
	else if ( get_node(A2, &parent) )
	  sibling = parent->siblings;
	else
	  fail;
      }

      if ( !sibling )
	fail;

      goto return_sibling;
    }
    case FRG_REDO:
    { sibling = CTX_PTR;

    return_sibling:
      if ( !unify_node(A1, sibling) )
	fail;
      if ( sibling->next )
	ForeignRedoPtr(sibling->next);
      succeed;
    }
    case FRG_CUTTED:
    default:
      succeed;
  }
}


static int
identify_def(term_t t, void *handle)
{ return unify_definition(MODULE_user, t, handle, 0, GP_QUALIFY|GP_NAMEARITY);
}


static int
unify_node_id(term_t t, call_node *n)
{ if ( n->type->magic == PROFTYPE_MAGIC )
  { return (*n->type->unify)(t, n->handle);
  } else
  { GET_LD

    return PL_unify_pointer(t, n->handle);
  }
}


static
PRED_IMPL("$prof_node", 8, prof_node, 0)
{ PRED_LD
  call_node *n = NULL;

  if ( !get_node(A1, &n) )
    return FALSE;

  collectSiblingsTime();

  return ( unify_node_id(A2, n) &&
	   PL_unify_integer(A3, n->calls) &&
	   PL_unify_integer(A4, n->redos) &&
	   PL_unify_integer(A5, n->exits) &&
	   PL_unify_integer(A6, n->recur) &&
	   PL_unify_integer(A7, n->ticks) &&
	   PL_unify_integer(A8, n->sibling_ticks) );
}


		 /*******************************
		 *	    COLLECT DATA	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
prof_procedure_data(+PredicateIndicator,
		    -TimeSelf, -TimeSiblings, -Parents, -Siblings)
    Where Parents  = list_of(Relative)
      And Siblings = list_of(Relative)
      and Relative = node(Pred, CycleID, Ticks, SiblingTicks,
			  Calls, Redos, Exits)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct prof_ref
{ struct prof_ref *next;		/* next in chain */
  void * handle;			/* Procedure handle */
  PL_prof_type_t   *type;
  int   cycle;
  uintptr_t ticks;
  uintptr_t sibling_ticks;
  uintptr_t calls;			/* calls to/from this predicate */
  uintptr_t redos;			/* redos to/from this predicate */
  uintptr_t exits;			/* exits to/from this predicate */
} prof_ref;


typedef struct
{ Definition def;
  uintptr_t ticks;
  uintptr_t sibling_ticks;
  uintptr_t calls;
  uintptr_t redos;
  uintptr_t exits;
  uintptr_t recur;
  prof_ref *callers;
  prof_ref *callees;
} node_sum;


static void
free_relatives(prof_ref *r)
{ prof_ref *n;

  for( ; r; r=n)
  { n = r->next;
    freeHeap(r, sizeof(*r));
  }
}


#define DEF_SPONTANEOUS (Definition)0
#define DEF_RECURSIVE   (Definition)1


static void
add_parent_ref(node_sum *sum,
	       call_node *self,
	       void *handle, PL_prof_type_t *type,
	       int cycle)
{ prof_ref *r;

  sum->calls += self->calls;
  sum->redos += self->redos;
  sum->exits += self->exits;

  for(r=sum->callers; r; r=r->next)
  { if ( r->handle == handle && r->cycle == cycle )
    { r->calls += self->calls;
      r->redos += self->redos;
      r->exits += self->exits;
      r->ticks += self->ticks;
      r->sibling_ticks += self->sibling_ticks;

      return;
    }
  }

  r = allocHeapOrHalt(sizeof(*r));
  r->calls = self->calls;
  r->redos = self->redos;
  r->exits = self->exits;
  r->ticks = self->ticks;
  r->sibling_ticks = self->sibling_ticks;
  r->handle = handle;
  r->type = type;
  r->cycle = cycle;
  r->next = sum->callers;
  sum->callers = r;
}


static void
add_recursive_ref(node_sum *sum, uintptr_t count, int cycle)
{ prof_ref *r;

  for(r=sum->callers; r; r=r->next)
  { if ( r->handle == DEF_RECURSIVE && r->cycle == cycle )
    { r->calls += count;

      return;
    }
  }

  r = allocHeapOrHalt(sizeof(*r));
  memset(r, 0, sizeof(*r));
  r->calls = count;
  r->handle = DEF_RECURSIVE;
  r->cycle = cycle;
  r->next = sum->callers;
  sum->callers = r;
}


static void
add_sibling_ref(node_sum *sum, call_node *sibling, int cycle)
{ prof_ref *r;

  for(r=sum->callees; r; r=r->next)
  { if ( r->handle == sibling->handle && r->cycle == cycle )
    { r->calls += sibling->calls;
      r->redos += sibling->redos;
      r->exits += sibling->exits;
      r->ticks += sibling->ticks;
      r->sibling_ticks += sibling->sibling_ticks;

      return;
    }
  }

  r = allocHeapOrHalt(sizeof(*r));
  r->calls = sibling->calls;
  r->redos = sibling->redos;
  r->exits = sibling->exits;
  r->ticks = sibling->ticks;
  r->sibling_ticks = sibling->sibling_ticks;
  r->handle = sibling->handle;
  r->type = sibling->type;
  r->cycle = cycle;
  r->next = sum->callees;
  sum->callees = r;
}




#define sumProfile(n, handle, type, sum, seen) LDFUNC(sumProfile, n, handle, type, sum, seen)
static int
sumProfile(DECL_LD call_node *n, void *handle, PL_prof_type_t *type,
	   node_sum *sum, int seen)
{ call_node *s;
  int count = 0;

  if ( n->handle == handle )
  { count++;
    if ( !seen )
    { sum->ticks         += n->ticks;
      sum->sibling_ticks += n->sibling_ticks;
    }

    if ( n->parent )
      add_parent_ref(sum, n, n->parent->handle, n->parent->type, seen);
    else
      add_parent_ref(sum, n, DEF_SPONTANEOUS, NULL, seen);

    if ( n->recur )
      add_recursive_ref(sum, n->recur, seen);

    for(s=n->siblings; s; s = s->next)
      add_sibling_ref(sum, s, seen);

    seen++;
  }

  for(s=n->siblings; s; s = s->next)
    count += sumProfile(s, handle, type, sum, seen);

  return count;
}


#define unify_relatives(list, r) LDFUNC(unify_relatives, list, r)
static int
unify_relatives(DECL_LD term_t list, prof_ref *r)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  term_t tmp = PL_new_term_ref();

  for( ; r; r=r->next)
  { int rc;

    if ( !PL_unify_list(tail, head, tail) )
      fail;

    PL_put_variable(tmp);
    if ( r->handle == DEF_SPONTANEOUS )
      rc=PL_unify_atom_chars(tmp, "<spontaneous>");
    else if ( r->handle == DEF_RECURSIVE )
      rc=PL_unify_atom_chars(tmp, "<recursive>");
    else
      rc=(*r->type->unify)(tmp, r->handle);

    if ( !rc ||
	 !PL_unify_term(head, PL_FUNCTOR, FUNCTOR_node7,
			PL_TERM, tmp,
			PL_INT,  r->cycle,
			PL_LONG, r->ticks,
			PL_LONG, r->sibling_ticks,
			PL_LONG, r->calls,
			PL_LONG, r->redos,
		        PL_LONG, r->exits) )
      fail;
  }

  return PL_unify_nil(tail);
}


static int
get_def(term_t t, void **handle)
{ Procedure proc;

  if ( get_procedure(t, &proc, 0, GP_FIND|GP_NAMEARITY) )
  { *handle = proc->definition;
    succeed;
  }

  fail;
}


static int
get_handle(term_t t, void **handle)
{ int i;

  for(i=0; i<MAX_PROF_TYPES; i++)
  { if ( types[i] && types[i]->get )
    { switch( (*types[i]->get)(t, handle) )
      { case TRUE:
	  succeed;
	case FALSE:
	  break;
	default:
	  assert(0);
      }
    }
  }

  fail;
}

static void
prof_release_def(void *handle)
{ Definition def = handle;

  releaseModule(def->module);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$prof_procedure_data(+PredicateIndicator,
		     -Ticks, -TicksSiblings,
		     -Calls, -Redos, -Exits,
		     -Callers, -Callees)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static
PRED_IMPL("$prof_procedure_data", 8, prof_procedure_data, PL_FA_TRANSPARENT)
{ PRED_LD
  void *handle;
  node_sum sum;
  call_node *n;
  int rc;
  int count = 0;

  if ( !get_handle(A1, &handle) )
    fail;

  collectSiblingsTime();
  memset(&sum, 0, sizeof(sum));
  for(n=LD->profile.roots; n; n=n->next)
    count += sumProfile(n, handle, &prof_default_type, &sum, 0);

  if ( count == 0 )
    fail;				/* nothing known about this one */

  rc = ( PL_unify_integer(A2, sum.ticks) &&
	 PL_unify_integer(A3, sum.sibling_ticks) &&
	 PL_unify_integer(A4, sum.calls) &&
	 PL_unify_integer(A5, sum.redos) &&
	 PL_unify_integer(A6, sum.exits) &&
	 unify_relatives(A7, sum.callers) &&
	 unify_relatives(A8, sum.callees)
       );

  free_relatives(sum.callers);
  free_relatives(sum.callees);

  return rc ? TRUE : FALSE;
}


/** '$prof_statistics'(-Samples, -Ticks, -AccountingTicks, -Time, -Nodes)

@arg Samples is the number of times the statistical profiler was called.
@arg Ticks   is the number of virtual ticks during profiling
@arg AccountingTicks are tick spent on accounting
@arg Time    is the total CPU time spent profiling
@arg Nodes   is the number of nodes in the call tree
*/

static
PRED_IMPL("$prof_statistics", 5, prof_statistics, 0)
{ PRED_LD
  if ( PL_unify_integer(A1, LD->profile.samples) &&
       PL_unify_integer(A2, LD->profile.ticks) &&
       PL_unify_integer(A3, LD->profile.accounting_ticks) &&
       PL_unify_float(  A4, LD->profile.time) &&
       PL_unify_integer(A5, LD->profile.nodes) )
    succeed;

  fail;
}


		 /*******************************
		 *	       RESET		*
		 *******************************/

#if USE_LD_MACROS
#define prof_clear_environments(fr) LDFUNC(prof_clear_environments, fr)
#define prof_clear_choicepoints(ch) LDFUNC(prof_clear_choicepoints, ch)
#define prof_clear_stacks(fr, ch) LDFUNC(prof_clear_stacks, fr, ch)
#endif

static QueryFrame
prof_clear_environments(DECL_LD LocalFrame fr)
{ if ( fr == NULL )
    return NULL;

  for(;;)
  { if ( true(fr, FR_MARKED) )
      return NULL;
    set(fr, FR_MARKED);
    LD->gc._local_frames++;

    fr->prof_node = NULL;

    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
      return queryOfFrame(fr);
  }
}


static void
prof_clear_choicepoints(DECL_LD Choice ch)
{ for( ; ch; ch = ch->parent )
  { LD->gc._choice_count++;
    prof_clear_environments(ch->frame);
  }
}


static void
prof_clear_stacks(DECL_LD LocalFrame fr, Choice ch)
{ QueryFrame qf;

  while(fr)
  { qf = prof_clear_environments(fr);
    assert(qf->magic == QID_MAGIC);
    prof_clear_choicepoints(ch);
    if ( qf->parent )
    { QueryFrame pqf = qf->parent;

      if ( !(fr = pqf->registers.fr) )
	fr = qf->saved_environment;
      ch = qf->saved_bfr;
    } else
      break;
  }
}


bool
resetProfiler(DECL_LD)
{ stopProfiler();

  assert(LD->gc._local_frames == 0);
  assert(LD->gc._choice_count == 0);

  prof_clear_stacks(environment_frame, LD->choicepoints);
  unmark_stacks(environment_frame, LD->choicepoints, FR_MARKED);

  assert(LD->gc._local_frames == 0);
  assert(LD->gc._choice_count == 0);

  freeProfileData();
  LD->profile.samples          = 0;
  LD->profile.ticks            = 0;
  LD->profile.accounting_ticks = 0;
  LD->profile.time             = 0.0;
  LD->profile.accounting       = FALSE;

  succeed;
}


static
PRED_IMPL("reset_profiler", 0, reset_profiler, 0)
{ resetProfiler();

  succeed;
}


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

static
PRED_IMPL("$profile", 2, profile, PL_FA_TRANSPARENT)
{ int rc;
  prof_status val;

  if ( !get_prof_status(A2, &val) )
    return FALSE;

  resetProfiler();
  startProfiler(val);
  rc = callProlog(NULL, A1, PL_Q_PASS_EXCEPTION, NULL);
  stopProfiler();

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is responsible for collection  the profiling statistics at
run time. It is called from a  dedicated profiler thread that by default
triggers this function every 5ms.  First, this calls thread_prof_ticks()
to determine the number of  ms  the   relevant  clock  (CPU or wall) has
progressed and then calls this function with `count` set to the relevant
clock increment, i.e., count is a number in the range 0..5.

This function just ticks the leaf node   in  the dynamic call graph. The
function collectSiblingsTime() propagates these   upward when collecting
statistics.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
profile(DECL_LD intptr_t count)
{ call_node *node;

  if ( !HAS_LD )
    return;

  LD->profile.samples++;
  LD->profile.ticks += count;

  if ( LD->profile.accounting )			/* we are updating nodes */
  { LD->profile.accounting_ticks += count;
  } else if ( (node=LD->profile.current) && node->magic == PROFNODE_MAGIC )
  { node->ticks += count;
  }
}

		 /*******************************
		 *     HIERARCHY ACCOUNTING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
profCall(Definition handle)

A call was made from the  current  node   to  handle.  This  builds up a
dynamic call tree. THe tree is constructed as follows:

  - If there is no current node
    - If the root-set contains a node for `handle`, use it
    - Else create a new root node.
  - If the current node has the same handle, increment `node->recur`
  - If somewhere in the parent chain we find the same parent-child
    transition, we tick this node and return it.  For example, given
    p->q->r->p, a call to q returns the q parent node and increments
    it recursion count.
    JW: This seems wrong
	- It breaks the propagation of self-ticks in the tree.
	- I think the second `p` also creates a second cycle.
  - If the current node has a sibling with `handle`, return it.
  - Else, add a new sibling.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG
static char *
node_name(call_node *n)
{ static char buf[100];

  if ( n->type == &prof_default_type )
    return predicateName(n->handle);

  Ssprintf(buf, "%p", n->handle);
  return buf;
}
#endif

#define prof_call(handle, type) LDFUNC(prof_call, handle, type)
static call_node *
prof_call(DECL_LD void *handle, PL_prof_type_t *type)
{ call_node *node = LD->profile.current;

  LD->profile.accounting = TRUE;

  if ( !node )				/* root-node of the profile */
  { for(node = LD->profile.roots; node; node=node->next)
    { if ( node->handle == handle )
      { node->calls++;
	LD->profile.current = node;
	DEBUG(MSG_PROF_CALLTREE,
	      Sdprintf("Call: existing root %s\n", node_name(node)));

	LD->profile.accounting = FALSE;
	return node;
      }
    }

    node = allocHeapOrHalt(sizeof(*node));
    memset(node, 0, sizeof(*node));
    LD->profile.nodes++;

    node->magic = PROFNODE_MAGIC;
    node->handle = handle;
    node->type = type;
    node->calls++;
    node->next = LD->profile.roots;
    LD->profile.roots = node;
    LD->profile.current = node;
    LD->profile.accounting = FALSE;
    DEBUG(MSG_PROF_CALLTREE,
	  Sdprintf("Call: new root %s\n", node_name(node)));

    return node;
  }

					/* straight recursion */
  if ( node->handle == handle )
  { node->recur++;
    DEBUG(MSG_PROF_CALLTREE,
	  Sdprintf("Call: direct recursion on %s\n", node_name(node)));
    LD->profile.accounting = FALSE;
    return node;
  } else				/* from some parent */
  { void *parent = node->handle;

    for(node=node->parent; node; node = node->parent)
    { if ( node->handle == handle &&
	   node->parent &&
	   node->parent->handle == parent )
      { node->recur++;

	LD->profile.current = node;
	DEBUG(MSG_PROF_CALLTREE,
	      Sdprintf("Call: indirect recursion on %s\n", node_name(node)));
	LD->profile.accounting = FALSE;
	return node;
      }
    }
  }

  for(node=LD->profile.current->siblings; node; node=node->next)
  { if ( node->handle == handle )
    { LD->profile.current = node;
      node->calls++;
      DEBUG(MSG_PROF_CALLTREE,
	    Sdprintf("Call: existing child %s\n", node_name(node)));
      LD->profile.accounting = FALSE;
      return node;
    }
  }

  node = allocHeapOrHalt(sizeof(*node));
  memset(node, 0, sizeof(*node));
  LD->profile.nodes++;
  node->magic = PROFNODE_MAGIC;
  node->handle = handle;
  node->type = type;
  node->parent = LD->profile.current;
  node->calls++;
  node->next = LD->profile.current->siblings;
  LD->profile.current->siblings = node;
  LD->profile.current = node;
  DEBUG(MSG_PROF_CALLTREE,
	Sdprintf("Call: new child %s\n", node_name(node)));
  LD->profile.accounting = FALSE;

  return node;
}


call_node *
profCall(DECL_LD Definition def)
{ if ( true(def, P_NOPROFILE) )
    return LD->profile.current;

  acquireModulePtr(def->module);

  return prof_call(def, &prof_default_type);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Exit, resuming execution in node. Note that   we  ignore the node if the
magic doesn't fit. That can  happen   using  tprofile/1 because the stop
doesn't need to match the  start.   Actually,  we should clear prof_node
references when clearing the data, but  this is rather complicated (must
be synchornised with atom-gc) and  it   still  doesn't cope with foreign
code.  Considering this is development only, we'll leave this for now.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
profResumeParent(DECL_LD struct call_node *node)
{ call_node *n;

  if ( node && node->magic != PROFNODE_MAGIC )
    return;

  LD->profile.accounting = TRUE;
  for(n=LD->profile.current; n && n != node; n=n->parent)
  { DEBUG(MSG_PROF_CALLTREE,
	  Sdprintf("Exit: %s\n", node_name(n)));
    n->exits++;
  }
  LD->profile.accounting = FALSE;

  LD->profile.current = node;
}


void
profExit(DECL_LD struct call_node *node)
{ if ( !node || node->magic != PROFNODE_MAGIC )
    return;

  profResumeParent(node->parent);
}


void
profRedo(DECL_LD struct call_node *node)
{ if ( node && node->magic != PROFNODE_MAGIC )
    return;

  if ( node )
  { if ( LD->profile.current )
    { struct call_node *n;

      DEBUG(MSG_PROF_CALLTREE,
	    Sdprintf("Redo: on %s; current is %s\n",
		     node_name(node), node_name(LD->profile.current) ));

      for(n=node; n && n != LD->profile.current; n = n->parent)
      { DEBUG(MSG_PROF_CALLTREE,
	      Sdprintf("Redo: %s\n", node_name(n)));
	n->redos++;
      }
    } else
    { DEBUG(MSG_PROF_CALLTREE,
	    Sdprintf("Redo: %s\n", node_name(node)));
      node->redos++;
    }
  }
  LD->profile.current = node;
}


void
profSetHandle(struct call_node *node, void *handle)
{ node->handle = handle;
}


		 /*******************************
		 *	 FOREIGN ACCESS		*
		 *******************************/

int
PL_register_profile_type(PL_prof_type_t *type)
{ int i;

  for(i=0; i<MAX_PROF_TYPES; i++)
  { if ( types[i] == type )
      return TRUE;
  }
  for(i=0; i<MAX_PROF_TYPES; i++)
  { if ( !types[i] )
    { types[i] = type;
      type->magic = PROFTYPE_MAGIC;
      return TRUE;
    }
  }

  assert(0);
  return FALSE;
}


void *
PL_prof_call(void *handle, PL_prof_type_t *type)
{ GET_LD

  return prof_call(handle, type);
}

void
PL_prof_exit(void *node)
{ GET_LD
  struct call_node *n = node;

  profResumeParent(n->parent);
}


		 /*******************************
		 *	       COLLECT		*
		 *******************************/


static uintptr_t
collectSiblingsNode(call_node *n)
{ call_node *s;
  uintptr_t count = 0;

  for(s=n->siblings; s; s=s->next)
  { count += collectSiblingsNode(s);
    n->sibling_ticks = count;
  }

  return count+n->ticks;
}


static void
collectSiblingsTime(DECL_LD)
{ if ( !LD->profile.sum_ok )
  { call_node *n;

    for(n=LD->profile.roots; n; n=n->next)
      collectSiblingsNode(n);

    LD->profile.sum_ok = TRUE;
  }
}


#define freeProfileNode(node) LDFUNC(freeProfileNode, node)
static void
freeProfileNode(DECL_LD call_node *node)
{ call_node *n, *next;

  assert(node->magic == PROFNODE_MAGIC);

  for(n=node->siblings; n; n=next)
  { next = n->next;

    if ( n->type && n->type->release )
      (*n->type->release)(n->handle);

    freeProfileNode(n);
  }

  node->magic = 0;
  freeHeap(node, sizeof(*node));
  LD->profile.nodes--;
}


static void
freeProfileData(void)
{ GET_LD
  call_node *n, *next;

  n = LD->profile.roots;
  LD->profile.roots = NULL;
  LD->profile.current = NULL;

  for(; n; n=next)
  { next = n->next;
    freeProfileNode(n);
  }

  assert(LD->profile.nodes == 0);
}

#else /* O_PROFILE */

		 /*******************************
		 *	    NO PROFILER		*
		 *******************************/

void
stopItimer(void)
{
}

static
PRED_IMPL("profiler", 2, profiler, 0)
{ return notImplemented("profile", 2);
}

static
PRED_IMPL("reset_profiler", 0, reset_profiler, 0)
{ return notImplemented("reset_profile", 0);
}

static
PRED_IMPL("$prof_node", 8, prof_node, 0)
{ return notImplemented("profile_node", 8);
}

static
PRED_IMPL("$prof_sibling_of", 2, prof_sibling_of, PL_FA_NONDETERMINISTIC)
{ return notImplemented("profile_sibling_of", 2);
}

static
PRED_IMPL("$profile", 2, profile, PL_FA_TRANSPARENT)
{ return notImplemented("$profile", 2);
}

static
PRED_IMPL("$prof_procedure_data", 8, prof_procedure_data, PL_FA_TRANSPARENT)
{ return notImplemented("$prof_procedure_data", 8);
}

static
PRED_IMPL("$prof_statistics", 5, prof_statistics, 0)
{ return notImplemented("$prof_statistics", 5);
}

/* Foreign interface of the profiler
*/

int
PL_register_profile_type(PL_prof_type_t *type)
{ return FALSE;				/* not supported */
}

void *
PL_prof_call(void *handle, PL_prof_type_t *type)
{ return NULL;
}

void
PL_prof_exit(void *node)
{
}

#endif /* O_PROFILE */

#ifdef O_PROF_PENTIUM
#include "pentium.c"

PRED_IMPL("show_pentium_profile", 0, show_pentium_profile, 0)
{ prof_report();

  succeed;
}

PRED_IMPL("reset_pentium_profile", 0, reset_pentium_profile, 0)
{ prof_reset();

  succeed;
}
#endif

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(profile)
  PRED_DEF("$profile", 2, profile, PL_FA_TRANSPARENT)
  PRED_DEF("profiler", 2, profiler, 0)
  PRED_DEF("reset_profiler", 0, reset_profiler, 0)
  PRED_DEF("$prof_node", 8, prof_node, 0)
  PRED_DEF("$prof_sibling_of", 2, prof_sibling_of, PL_FA_NONDETERMINISTIC)
  PRED_DEF("$prof_procedure_data", 8, prof_procedure_data, PL_FA_TRANSPARENT)
  PRED_DEF("$prof_statistics", 5, prof_statistics, 0)
#ifdef O_PROF_PENTIUM
  PRED_DEF("show_pentium_profile", 0, show_pentium_profile, 0)
  PRED_DEF("reset_pentium_profile", 0, reset_pentium_profile, 0)
#endif
EndPredDefs
