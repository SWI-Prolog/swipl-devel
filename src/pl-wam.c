/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2022, University of Amsterdam
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

/*#define O_DEBUG 1*/
#define USE_FLI_INLINES 1
#define USE_ALLOC_INLINES 1

#include "pl-wam.h"
#include "pl-comp.h"
#include "pl-arith.h"
#include "pl-inline.h"
#include "pl-dbref.h"
#include "pl-wrap.h"
#include "pl-prof.h"
#include "pl-event.h"
#include "pl-tabling.h"
#include "pl-undo.h"
#include "pl-util.h"
#include "pl-gc.h"
#include "pl-trace.h"
#include "pl-pro.h"
#include "pl-modul.h"
#include "pl-funct.h"
#include "pl-fli.h"
#include "pl-proc.h"
#include "pl-attvar.h"
#include "pl-setup.h"
#include "pl-prims.h"
#include "pl-write.h"
#include "pl-supervisor.h"
#include "pl-index.h"
#include "pl-cont.h"
#include "pl-coverage.h"
#include <fenv.h>
#ifdef _MSC_VER
#pragma warning(disable: 4102)		/* unreferenced labels */
#endif

#define	     BFR (LD->choicepoints)	/* choicepoint registration */

#if sun
#include <prof.h>			/* in-function profiling */
#else
#define MARK(label)
#endif

#define newChoice(type, fr) LDFUNC(newChoice, type, fr)
static Choice	newChoice(DECL_LD choice_type type, LocalFrame fr);

#if COUNTING

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The counting code has been added   while investigating the time critical
WAM  instructions.  The  current  implementation  runs  on  top  of  the
information  provided  by  code_info   (from    pl-comp.c)   and  should
automatically addapt to modifications in the VM instruction set.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ code  code;
  int	times;
  int  *vartimesptr;
} count_info;

#define MAXVAR 8

static count_info counting[I_HIGHEST];

static void
count(code c, Code PC)
{ const code_info *info = &codeTable[c];

  counting[c].times++;
  switch(info->argtype)
  { case CA1_VAR:
    case CA1_FVAR:
    case CA1_CHP:
    { int v = (int)*PC;

      v -= ARGOFFSET/sizeof(word);
      assert(v>=0);
      if ( v >= MAXVAR )
	v = MAXVAR-1;

      if ( !counting[c].vartimesptr )
      { int bytes = sizeof(int)*MAXVAR;

	counting[c].vartimesptr = allocHeapOrHalt(bytes);
	memset(counting[c].vartimesptr, 0, bytes);
      }
      counting[c].vartimesptr[v]++;
    }
  }
}


static void
countHeader()
{ int m;
  int amax = MAXVAR;
  char last[20];

  Sfprintf(Scurout, "%-13s %8s ", "Instruction", "times");
  for(m=0; m < amax-1; m++)
    Sfprintf(Scurout, " %8d", m);
  Ssprintf(last, ">%d", m);
  Sfprintf(Scurout, " %8s\n", last);
  for(m=0; m<(31+amax*8); m++)
    Sputc('=', Scurout);
  Sfprintf(Scurout, "\n");
}


static int
cmpcounts(const void *p1, const void *p2)
{ const count_info *c1 = p1;
  const count_info *c2 = p2;

  return c2->times - c1->times;
}


word
pl_count()
{ int i;
  count_info counts[I_HIGHEST];
  count_info *c;

  countHeader();

  memcpy(counts, counting, sizeof(counts));
  for(i=0, c=counts; i<I_HIGHEST; i++, c++)
    c->code = i;
  qsort(counts, I_HIGHEST, sizeof(count_info), cmpcounts);

  for(c = counts, i=0; i<I_HIGHEST; i++, c++)
  { const code_info *info = &codeTable[c->code];

    Sfprintf(Scurout, "%-13s %8d ", info->name, c->times);
    if ( c->vartimesptr )
    { int n, m=MAXVAR;

      while(m>0 && c->vartimesptr[m-1] == 0 )
	m--;
      for(n=0; n<m; n++)
	Sfprintf(Scurout, " %8d", c->vartimesptr[n]);
    }
    Sfprintf(Scurout, "\n");
  }

  succeed;
}

#else /* ~COUNTING */

#define count(id, pc)			/* no debugging not counting */

#endif /* COUNTING */

		 /*******************************
		 *	     DEBUGGING		*
		 *******************************/

#if defined(O_DEBUG) || defined(SECURE_GC) || defined(O_MAINTENANCE)
#define loffset(p) LDFUNC(loffset, p)
static intptr_t
loffset(DECL_LD void *p)
{ if ( p == NULL )
    return 0;

  assert((intptr_t)p % sizeof(word) == 0);
  return (Word)p-(Word)lBase;
}
#endif

#ifdef O_DEBUG

static void
DbgPrintInstruction(LocalFrame FR, Code PC)
{ static LocalFrame ofr = NULL;		/* not thread-safe */

  IF_DEBUGGING(MSG_VMI)
  { GET_LD

    if ( ofr != FR )
    { Sfprintf(Serror, "#%ld at [%ld] predicate %s\n",
	       loffset(FR),
	       levelFrame(FR),
	       predicateName(FR->predicate));
      ofr = FR;
    }

    { Code relto = NULL;
      intptr_t offset;

      if ( FR->predicate->codes &&
	   (offset = (PC - FR->predicate->codes)) >= 0 &&
	   (offset < (intptr_t)FR->predicate->codes[-1] ||
	    (offset == 0 && FR->predicate->codes[-1] == 0)) ) /* see initSupervisors() */
      { relto = FR->predicate->codes;
      } else if ( FR->clause )
      { relto = FR->clause->value.clause->codes;
      } else
	relto = NULL;

      Sdprintf("\t%4ld %s\n", (long)(PC-relto), codeTable[decode(*PC)].name);
    }
  }
}

#else

#define DbgPrintInstruction(fr, pc)

#endif




		 /*******************************
		 *	     SIGNALS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LD->alerted indicates the system is running in  some sort of `safe' mode
and therefore should perform various checks. It   is  a disjunction of a
number of conditions that would ortherwise  have to be tested one-by-one
in several virtual machine instructions.  Currently covers:

	* Pending signals
	* pthread_cancel() requested
        * Activation of the profiler
	* out-of-stack signalled
	* active depth-limit
	* attributed variable wakeup
	* debugmode active
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
updateAlerted(PL_local_data_t *ld)
{ int mask = 0;

  WITH_LD(ld)
  { if ( is_signalled() )		        mask |= ALERT_SIGNAL;
    if ( !truePrologFlag(PLFLAG_VMI_BUILTIN) ||
	 ld->prolog_flag.occurs_check != OCCURS_CHECK_FALSE )
      ld->slow_unify = TRUE;  /* see VMI B_UNIFY_VAR */
    else
      ld->slow_unify = FALSE;
  }
#ifdef O_PROFILE
  if ( ld->profile.active )			mask |= ALERT_PROFILE;
#endif
#ifdef O_PLMT
  if ( ld->exit_requested )			mask |= ALERT_EXITREQ;
#endif
#ifdef O_LIMIT_DEPTH
  if ( ld->depth_info.limit != DEPTH_NO_LIMIT ) mask |= ALERT_DEPTHLIMIT;
#endif
#ifdef O_INFERENCE_LIMIT
  if ( ld->inference_limit.limit != INFERENCE_NO_LIMIT )
						mask |= ALERT_INFERENCELIMIT;
#endif
#ifdef O_ATTVAR
					/* is valTermRef(ld->attvar.head) */
  if ( ld->stacks.local.base &&
       !isVar(((Word)ld->stacks.local.base)[ld->attvar.head]) )
						mask |= ALERT_WAKEUP;
#endif
#ifdef O_DEBUGGER
  if ( ld->_debugstatus.debugging )		mask |= ALERT_DEBUG;
#endif
  if ( ld->fli.string_buffers.top )		mask |= ALERT_BUFFER;
  if ( UNDO_SCHEDULED(ld) )			mask |= ALERT_UNDO;
  if ( LD->coverage.active )			mask |= ALERT_COVERAGE;

  ld->alerted = mask;
}


/* raiseSignal() sets a signal in a target thread.  This implies manipulating
   the mask and setting ld->alerted. Note that we cannot call
   updateAlerted() because the O_ATTVAR might go wrong if the target
   thread performs a stack-shift.
*/

int
raiseSignal(PL_local_data_t *ld, int sig)
{ if ( IS_VALID_SIGNAL(sig) && ld )
  { int alerted;

    WSIGMASK_SET(ld->signal.pending, sig);

    do
    { alerted = ld->alerted;
    } while ( !COMPARE_AND_SWAP_INT(&ld->alerted, alerted, alerted|ALERT_SIGNAL) );

    return TRUE;
  }

  return FALSE;
}


int
pendingSignal(PL_local_data_t *ld, int sig)
{ if ( IS_VALID_SIGNAL(sig) && ld )
  { return WSIGMASK_ISSET(ld->signal.pending, sig) ? TRUE : FALSE;
  }

  return -1;
}


		 /*******************************
		 *	   STACK-LAYOUT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Brief description of the local stack-layout.  This stack contains:

	* struct localFrame structures for the Prolog stackframes.
	* argument vectors and local variables for Prolog goals.
	* choice-points (struct choice)
	* term-references for foreign code.  The layout:


	lTop  -->| first free location |
		 -----------------------
		 | local variables     |
		 |        ...	       |
		 | arguments for goal  |
		 | localFrame struct   |
		 | queryFrame struct   |
		 -----------------------
		 |        ...	       |
		 | term-references     |
		 -----------------------
	lBase -->| # fliFrame struct   |
		 -----------------------

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */



		 /*******************************
		 *	    FOREIGN FRAME	*
		 *******************************/

#define open_foreign_frame(_) LDFUNC(open_foreign_frame, _)
static fid_t
open_foreign_frame(DECL_LD)
{ FliFrame fr = (FliFrame) lTop;

  assert((LocalFrame)(fr+1) <= lMax);
  lTop = (LocalFrame)(fr+1);
  fr->size = 0;
  Mark(fr->mark);
  DEBUG(CHK_SECURE, assert(fr>fli_context));
  fr->parent = fli_context;
  fr->magic = FLI_MAGIC;
  fli_context = fr;

  return consTermRef(fr);
}


void
PL_close_foreign_frame(DECL_LD fid_t id)
{ FliFrame fr = (FliFrame) valTermRef(id);

  if ( !id || fr->magic != FLI_MAGIC )
    sysError("PL_close_foreign_frame(): illegal frame: %d", id);
  DiscardMark(fr->mark);
  fr->magic = FLI_MAGIC_CLOSED;
  fli_context = fr->parent;
  lTop = (LocalFrame) fr;
}


fid_t
PL_open_foreign_frame(DECL_LD)
{ size_t lneeded = sizeof(struct fliFrame) + MINFOREIGNSIZE*sizeof(word);

  if ( !ensureLocalSpace(lneeded) )
    return 0;

  return open_foreign_frame();
}


API_STUB(fid_t)
(PL_open_foreign_frame)(void)
( return PL_open_foreign_frame(); )


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open a foreign frame to handle a signal.  We must skip MAXARITY words to
deal with the fact that the WAM write-mode   writes above the top of the
stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fid_t
PL_open_signal_foreign_frame(int sync)
{ GET_LD
  FliFrame fr;
  size_t minspace = sizeof(struct localFrame) + MINFOREIGNSIZE*sizeof(word);
  size_t margin   = sync ? 0 : MAXARITY*sizeof(word);

  if ( (char*)lTop + minspace + margin  > (char*)lMax )
  { if ( sync )
    { int rc;

      if ( (rc=growLocalSpace(minspace, ALLOW_SHIFT)) != TRUE )
	return 0;
    } else
    { return 0;
    }
  }

  fr = addPointer(lTop, margin);
  fr->magic = FLI_MAGIC;
  fr->size = 0;
  Mark(fr->mark);
  fr->parent = fli_context;
  lTop = (LocalFrame)(fr+1);
  fli_context = fr;

  return consTermRef(fr);
}


API_STUB(void)
(PL_close_foreign_frame)(fid_t id)
( PL_close_foreign_frame(id); )


void
PL_rewind_foreign_frame(fid_t id)
{ GET_LD
  FliFrame fr = (FliFrame) valTermRef(id);

  fli_context = fr;
  Undo(fr->mark);
  lTop = addPointer(fr, sizeof(struct fliFrame));
  fr->size = 0;
}


void
PL_discard_foreign_frame(fid_t id)
{ GET_LD
  FliFrame fr = (FliFrame) valTermRef(id);

  DEBUG(8, Sdprintf("Discarding foreign frame %p\n", fr));
  fli_context = fr->parent;
  Undo(fr->mark);
  DiscardMark(fr->mark);
  lTop = (LocalFrame) fr;
}


#define determinism_error(fr, bfr, found) \
	LDFUNC(determinism_error, fr, bfr, found)

static int
determinism_error(DECL_LD LocalFrame fr, Choice bfr, atom_t found)
{ fid_t fid;
  int rc = FALSE;
  atom_t a = ATOM_error;

  if ( found == ATOM_nondet )
  { for(; bfr && bfr->type == CHP_DEBUG; bfr=bfr->parent)
      ;

    if ( (void*)bfr < (void*)fr )
      return TRUE;
  }

  PL_current_prolog_flag(ATOM_determinism_error, PL_ATOM, &a);
  if ( a == ATOM_silent )
    return TRUE;

  if ( (fid=PL_open_foreign_frame()) )
  { Definition def = fr->predicate;
    atom_t decl;

    if ( true(fr, FR_DETGUARD) )
    { if ( true(fr, FR_DETGUARD_SET) )
      { decl = ATOM_guard;
      } else
      { LocalFrame fr2;

	decl = ATOM_guard_in_caller;
	for(fr2=fr->parent; fr2; fr2=fr2->parent)
	{ if ( true(fr2, FR_DETGUARD_SET) )
	  { def = fr2->predicate;
	    break;
	  }
	}
      }
    } else
    { decl = ATOM_property;

      if ( false(def, P_DET) )
      { LocalFrame fr2;

	for(fr2=fr->parent; fr2; fr2=fr2->parent)
	{ Definition def2 = fr2->predicate;
	  if ( true(def2, P_DET) )
	  { def = def2;
	    break;
	  }
	}
      }
    }

    if ( a == ATOM_warning )
    { term_t pi = PL_new_term_ref();

      rc = ( (pi=PL_new_term_ref()) &&
	     unify_definition(MODULE_user, pi, def, 0,
			      GP_NAMEARITY|GP_HIDESYSTEM) &&
	     printMessage(ATOM_warning,
			  PL_FUNCTOR, FUNCTOR_error2,
			    PL_FUNCTOR, FUNCTOR_determinism_error4,
			      PL_TERM, pi,
			      PL_ATOM, ATOM_det,
			      PL_ATOM, found,
			      PL_ATOM, decl,
			    PL_VARIABLE) );
    } else
    { rc = PL_error(NULL, 0, NULL, ERR_DETERMINISM,
		    def, ATOM_det, found, decl);
    }

    PL_close_foreign_frame(fid);
  }

  return rc;
}


#define ssu_or_det_failed(fr) LDFUNC(ssu_or_det_failed, fr)
static int
ssu_or_det_failed(DECL_LD LocalFrame fr)
{ fid_t fid;
  int rc = FALSE;

  if ( false(fr, FR_SSU_DET) )
    return determinism_error(fr, NULL, ATOM_fail);

  if ( (fid = PL_open_foreign_frame()) )
  { term_t goal;

    rc = ( (goal=PL_new_term_ref()) &&
	   put_frame_goal(goal, fr) &&
	   PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_matching_rule, goal)
	 );

    PL_close_foreign_frame(fid);
  }

  return rc;
}


		/********************************
		*         FOREIGN CALLS         *
		*********************************/

#define CALL_FCUTTED(argc, f, c) \
  { switch(argc) \
    { case 0: \
	f(c); \
        break; \
      case 1: \
	f(0,(c)); \
	break; \
      case 2: \
	f(0,0,(c)); \
        break; \
      case 3: \
	f(0,0,0,(c)); \
        break; \
      case 4: \
	f(0,0,0,0,(c)); \
        break; \
      case 5: \
	f(0,0,0,0,0,(c)); \
        break; \
      case 6: \
	f(0,0,0,0,0,0,(c)); \
        break; \
      case 7: \
	f(0,0,0,0,0,0,0,(c)); \
        break; \
      case 8: \
	f(0,0,0,0,0,0,0,0,(c)); \
        break; \
      case 9: \
	f(0,0,0,0,0,0,0,0,0,(c)); \
        break; \
      case 10: \
	f(0,0,0,0,0,0,0,0,0,0,(c)); \
        break; \
      default: \
	assert(0); \
    } \
  }

#define discardForeignFrame(fr) LDFUNC(discardForeignFrame, fr)
static void
discardForeignFrame(DECL_LD LocalFrame fr)
{ Definition def = fr->predicate;
  int argc       = (int)def->functor->arity;
  Func function  = def->impl.foreign.function;
  struct foreign_context context;
  fid_t fid;

  DEBUG(5, Sdprintf("\tCut %s, context = %p\n",
		    predicateName(def), fr->clause));

  switch((word)fr->clause & FRG_REDO_MASK)
  { case REDO_INT:
      context.context = (word)fr->clause >> FRG_REDO_BITS;
      break;
    case REDO_PTR:
      context.context = (word)fr->clause;
      break;
    case YIELD_PTR:
      context.context = (word)fr->clause & ~FRG_REDO_MASK;
      break;
  }
  context.control = FRG_CUTTED;
  context.engine  = LD;

  fid = PL_open_foreign_frame();
  if ( true(def, P_VARARG) )
  { (*function)(0, argc, &context);
  } else
  { CALL_FCUTTED(argc, (*function), &context);
  }
  PL_close_foreign_frame(fid);
}


typedef struct finish_reason
{ atom_t	name;			/* name of the reason */
  int		is_exception;		/* is an exception reason */
} finish_reason;

enum finished
{ FINISH_EXIT = 0,			/* keep consistent with reason_decls[] */
  FINISH_FAIL,
  FINISH_CUT,
  FINISH_EXITCLEANUP,
  FINISH_EXTERNAL_EXCEPT_UNDO,
  FINISH_EXTERNAL_EXCEPT,
  FINISH_EXCEPT
};

static const finish_reason reason_decls[] =
{ { ATOM_exit,               FALSE },	/* keep consistent with enum finished */
  { ATOM_fail,               FALSE },
  { ATOM_cut,                FALSE },
  { ATOM_exit,               FALSE },
  { ATOM_external_exception, TRUE },
  { ATOM_external_exception, TRUE },
  { ATOM_exception,          TRUE }
};


static inline int
is_exception_finish(enum finished reason)
{ return reason_decls[reason].is_exception;
}


static int
unify_finished(term_t catcher, enum finished reason)
{ GET_LD

  /* make sure declaration is consistent */
  DEBUG(0, assert(reason_decls[FINISH_EXCEPT].name == ATOM_exception));

  if ( is_exception_finish(reason) )
  { functor_t f = (reason == FINISH_EXCEPT ? FUNCTOR_exception1
					   : FUNCTOR_external_exception1);

    DEBUG(CHK_SECURE, checkData(valTermRef(exception_bin)));

    return PL_unify_term(catcher,
			 PL_FUNCTOR, f,
			   PL_TERM, exception_bin);
  } else if ( reason == FINISH_EXIT )
  { fail;
  } else
  { return PL_unify_atom(catcher, reason_decls[reason].name);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call_term() calls a term from C. The  sound   and  simple way is to call
call/1 with the term as argument, but in   most cases we can avoid that.
As frameFinished() is rather time critical this seems worthwhile.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#define call1(mdef, goal) LDFUNC(call1, mdef, goal)
static int
call1(DECL_LD Module mdef, term_t goal)
{ predicate_t pred;
  qid_t qid;
  int rc;

  pred = _PL_predicate("call", 1, "system", &GD->procedures.call1);

  qid = PL_open_query(mdef, PL_Q_PASS_EXCEPTION, pred, goal);
  rc = PL_next_solution(qid);
  PL_cut_query(qid);

  return rc;
}


#define call_term(mdef, goal) LDFUNC(call_term, mdef, goal)
static int
call_term(DECL_LD Module mdef, term_t goal)
{ Word p = valTermRef(goal);
  Module module = mdef;

  deRef(p);
  if ( (p=stripModule(p, &module, 0)) )
  { functor_t functor;
    term_t av;
    Procedure proc;
    qid_t qid;
    int rval;

    if ( isAtom(*p) )
    { if ( isTextAtom(*p) )
      { functor = lookupFunctorDef(*p, 0);
	av = 0;
      } else
	return call1(mdef, goal);
    } else if ( isTerm(*p) )
    { Functor f = valueTerm(*p);
      FunctorDef fd = valueFunctor(f->definition);

      if ( isTextAtom(fd->name) &&
	   false(fd, CONTROL_F) &&
	   !(fd->name == ATOM_call && fd->arity > 8) )
      { size_t arity = fd->arity;
	Word args = f->arguments;
	Word ap;
	size_t i;

	av = PL_new_term_refs(arity);
	ap = valTermRef(av);

	for(i=0; i<arity; i++, ap++)
	  *ap = linkValG(&args[i]);
	functor = f->definition;
      } else
	return call1(mdef, goal);
    } else
    { return PL_type_error("callable", goal);
    }

    proc = resolveProcedure(functor, module);
    qid = PL_open_query(module, PL_Q_PASS_EXCEPTION, proc, av);
    rval = PL_next_solution(qid);
    PL_cut_query(qid);

    return rval;
  } else
    return FALSE;				/* exception in env */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
frameFinished() is used for two reasons:   providing hooks for the (GUI)
debugger  for  updating   the   stack-view    and   for   dealing   with
call_cleanup/3.  Both may call-back the Prolog engine.

Note that the cleanup handler is called while protected against signals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define callCleanupHandler(fr, reason) LDFUNC(callCleanupHandler, fr, reason)
static void
callCleanupHandler(DECL_LD LocalFrame fr, enum finished reason)
{ if ( false(fr, FR_CATCHED) )		/* from handler */
  { size_t fref = consTermRef(fr);
    fid_t cid;
    term_t catcher;

    assert(fr->predicate == PROCEDURE_setup_call_catcher_cleanup4->definition);

    if ( !(cid=PL_open_foreign_frame()) )
      return;				/* exception is in the environment */

    fr = (LocalFrame)valTermRef(fref);
    catcher = consTermRef(argFrameP(fr, 2));

    set(fr, FR_CATCHED);
    if ( unify_finished(catcher, reason) )
    { term_t clean;
      wakeup_state wstate;

      fr = (LocalFrame)valTermRef(fref);
      clean = consTermRef(argFrameP(fr, 3));
      if ( saveWakeup(&wstate, FALSE) )
      { int rval;

	startCritical();
        rval = call_term(contextModule(fr), clean);
	if ( !endCritical() )
	  rval = FALSE;
	if ( !rval && exception_term )
	  wstate.flags |= WAKEUP_KEEP_URGENT_EXCEPTION;
	restoreWakeup(&wstate);
      }
    }

    PL_close_foreign_frame(cid);
  }
}


#define frameFinished(fr, reason) LDFUNC(frameFinished, fr, reason)
static int
frameFinished(DECL_LD LocalFrame fr, enum finished reason)
{ if ( true(fr, FR_CLEANUP) )
  { size_t fref = consTermRef(fr);
    callCleanupHandler(fr, reason);
    fr = (LocalFrame)valTermRef(fref);
  }

  if ( true(fr, FR_DEBUG) )
    return callEventHook(PLEV_FRAMEFINISHED, fr);

  return TRUE;
}


#define mustBeCallable(call) LDFUNC(mustBeCallable, call)
static int
mustBeCallable(DECL_LD term_t call)
{ Word p = valTermRef(call);
  Word ap;

  deRef(p);
  if ( isVar(*p) )
  { instantiation_error:
    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
  }
  if ( !hasFunctor(*p, FUNCTOR_colon2) )
  { type_error:
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, call);
  }
  ap = argTermP(*p, 0);
  deRef(ap);
  if ( isVar(*ap) )
    goto instantiation_error;
  if ( !isAtom(*ap) )
    goto type_error;
  ap = argTermP(*p, 1);
  deRef(ap);
  if ( isVar(*ap) )
    goto instantiation_error;
  if ( !(isTerm(*ap) || isTextAtom(*ap)) )
    goto type_error;

  succeed;
}


		 /*******************************
		 *	   BREAKPOINTS		*
		 *******************************/

typedef enum
{ BRK_ERROR = 0,			/* Exception */
  BRK_CONTINUE,				/* continue execution */
  BRK_TRACE,				/* trace from here */
  BRK_DEBUG,				/* debug from here */
  BRK_CALL				/* Call returned term */
} break_action;

#define SAVE_PTRS() \
	frameref = consTermRef(frame); \
	chref    = consTermRef(bfr); \
	pcref    = (onStack(local, PC) ? consTermRef(PC) : 0);
#define RESTORE_PTRS() \
	frame = (LocalFrame)valTermRef(frameref); \
	bfr   = (Choice)valTermRef(chref); \
	PC    = (pcref ? (Code)valTermRef(pcref) : PC);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify a pointer into a new  global  term   (g)  with  a  pointer into an
environment as obtained from some instruction VAR argument. This assumes
we have allocated enough trail stack.

See (*) with callBreakHook() for why we  need protect_var(). As the term
reference is only used for  GC,  we   can  use  a  local stack reference
(makeRefLok()).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define protect_var(v) LDFUNC(protect_var, v)
static void
protect_var(DECL_LD Word v)
{ term_t t = PL_new_term_ref_noshift();

  if ( t )
    *valTermRef(t) = makeRefLok(v);
  else
    assert(0);		/* cannot happen due to MINFOREIGNSIZE */
}


#define unify_gl(g, l, has_firstvar) LDFUNC(unify_gl, g, l, has_firstvar)
static void
unify_gl(DECL_LD Word g, Word l, int has_firstvar)
{ if ( has_firstvar )
    protect_var(l);

  deRef(l);
  if ( isVar(*l) )
  { setVar(*g);
    *l = makeRefG(g);
    assert(tTop+1 < tMax);
    LTrail(l);
  } else if ( needsRef(*l) )
  { *g = makeRefG(l);
  } else
  { *g = *l;
  }
}


#define put_call_goal(t, proc) LDFUNC(put_call_goal, t, proc)
static int
put_call_goal(DECL_LD term_t t, Procedure proc)
{ FunctorDef fd  = proc->definition->functor;

  if ( fd->arity > 0 )
  { Word        gt = allocGlobal(fd->arity+1);
    LocalFrame NFR = LD->query->next_environment;
    Word ap        = argFrameP(NFR, 0);
    Word gp	   = gt;
    int i;

    if ( !gt )
      return FALSE;			/* could not allocate */

    DEBUG(MSG_TRACE,
	  Sdprintf("Copy %d call args from %p\n", fd->arity, ap));

    *gp++ = fd->functor;
    for(i=0; i<fd->arity; i++)
      unify_gl(gp++, ap++, FALSE);
    *valTermRef(t) = consPtr(gt, STG_GLOBAL|TAG_COMPOUND);
  } else
  { *valTermRef(t) = fd->name;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_vm_call() creates a description of  the   instruction  to  which the
break applied.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define put_vm_call(t, frref, PC, op, has_firstvar, pop) LDFUNC(put_vm_call, t, frref, PC, op, has_firstvar, pop)
static int
put_vm_call(DECL_LD term_t t, term_t frref, Code PC, code op, int has_firstvar,
	    int *pop)
{ atom_t simple_goal;
  functor_t ftor;
  int clean;

  switch(op)
  { case I_CALL:			/* procedure */
    case I_DEPART:
    { return ( put_call_goal(t, (Procedure) PC[1]) &&
	       PL_cons_functor_v(t, FUNCTOR_call1, t) );
    }
    case I_CALLM:			/* module, procedure */
    case I_DEPARTM:
    { Module m = (Module)PC[1];
      term_t av;

      return ( (av = PL_new_term_refs(2)) &&
	       PL_put_atom(av+0, m->name) &&
	       put_call_goal(av+1, (Procedure) PC[2]) &&
	       PL_cons_functor_v(t, FUNCTOR_colon2, av) &&
	       PL_cons_functor_v(t, FUNCTOR_call1, t) );
    }
    case I_CALLATM:			/* procm, contextm, proc */
    case I_DEPARTATM:			/* call(@(procm:g, contextm)) */
    { Module procm    = (Module)PC[1];
      Module contextm = (Module)PC[2];
      term_t av;

      return ( (av = PL_new_term_refs(2)) &&
	       PL_put_atom(av+0, procm->name) &&
	       put_call_goal(av+1, (Procedure) PC[3]) &&
	       PL_cons_functor_v(av+0, FUNCTOR_colon2, av) &&
	       PL_put_atom(av+1, contextm->name) &&
	       PL_cons_functor_v(t, FUNCTOR_xpceref2, av) &&
	       PL_cons_functor_v(t, FUNCTOR_call1, t) );
    }
    case I_CALLATMV:			/* procm, contextm, proc */
    case I_DEPARTATMV:			/* call(@(procm:g, contextm)) */
    { Module procm    = (Module)PC[1];
      LocalFrame   fr = (LocalFrame)valTermRef(frref);
      term_t      cmv = consTermRef(varFrameP(fr, (int)PC[2]));
      term_t av;

      return ( (av = PL_new_term_refs(2)) &&
	       PL_put_atom(av+0, procm->name) &&
	       put_call_goal(av+1, (Procedure) PC[3]) &&
	       PL_cons_functor_v(av+0, FUNCTOR_colon2, av) &&
	       PL_put_term(av+1, cmv) &&
	       PL_cons_functor_v(t, FUNCTOR_xpceref2, av) &&
	       PL_cons_functor_v(t, FUNCTOR_call1, t) );
    }
    case I_USERCALL0:
    { LocalFrame NFR = LD->query->next_environment;
      term_t       g = consTermRef(argFrameP(NFR, 0));

      return PL_cons_functor_v(t, FUNCTOR_call1, g);
    }
    case I_USERCALLN:			/* call(call(G, ...)) */
    { int      extra = (int)PC[1];
      functor_t   cf = PL_new_functor(ATOM_call, 1+extra);
      LocalFrame NFR = LD->query->next_environment;
      term_t       g = consTermRef(argFrameP(NFR, 0));

      return ( PL_cons_functor_v(t, cf, g) &&
	       PL_cons_functor_v(t, FUNCTOR_call1, t) );
    }
    case I_FAIL:	simple_goal = ATOM_fail; goto simple;
    case I_TRUE:	simple_goal = ATOM_true; goto simple;
    simple:
    { Word gt = allocGlobal(2);

      gt[0] = FUNCTOR_call1;
      gt[1] = simple_goal;
      *valTermRef(t) = consPtr(gt, STG_GLOBAL|TAG_COMPOUND);

      return TRUE;
    }
    case B_EQ_VC:    clean = 0x0; ftor = FUNCTOR_strict_equal2;     goto vc_2;
    case B_NEQ_VC:   clean = 0x0; ftor = FUNCTOR_not_strict_equal2; goto vc_2;
    case B_UNIFY_VC: clean = 0x0; ftor = FUNCTOR_equals2;           goto vc_2;
    case B_UNIFY_FC: clean = 0x1; ftor = FUNCTOR_equals2;           goto vc_2;
    vc_2:
    { Word gt       = allocGlobal(2+1+2);	/* call(f(V,C)) */
      LocalFrame fr = (LocalFrame)valTermRef(frref);
      Word       v1 = varFrameP(fr, (int)PC[1]);

      if ( !gt )
	return FALSE;

      if ( clean&0x1 ) setVar(*v1);

      gt[0] = ftor;
      unify_gl(&gt[1], v1, has_firstvar);
      gt[2] = (word)PC[2];
      gt[3] = FUNCTOR_call1;
      gt[4] = consPtr(gt, STG_GLOBAL|TAG_COMPOUND);
      *valTermRef(t) = consPtr(&gt[3], STG_GLOBAL|TAG_COMPOUND);

      return TRUE;
    }
    case B_EQ_VV:    clean = 0x0; ftor = FUNCTOR_strict_equal2;     goto fa_2;
    case B_NEQ_VV:   clean = 0x0; ftor = FUNCTOR_not_strict_equal2; goto fa_2;
    case B_UNIFY_FF: clean = 0x3; ftor = FUNCTOR_equals2;	    goto fa_2;
    case B_UNIFY_VF:
    case B_UNIFY_FV: clean = 0x1; ftor = FUNCTOR_equals2;           goto fa_2;
    case B_UNIFY_VV: clean = 0x0; ftor = FUNCTOR_equals2;           goto fa_2;
    fa_2:
    { Word gt       = allocGlobal(2+1+2);	/* call(A=B) */
      LocalFrame fr = (LocalFrame)valTermRef(frref);
      Word v1       = varFrameP(fr, (int)PC[1]);
      Word v2       = varFrameP(fr, (int)PC[2]);

      if ( !gt )
	return FALSE;

      if ( clean&0x1 ) setVar(*v1);
      if ( clean&0x2 ) setVar(*v2);

      gt[0] = ftor;
      unify_gl(&gt[1], v1, has_firstvar);
      unify_gl(&gt[2], v2, has_firstvar);
      gt[3] = FUNCTOR_call1;
      gt[4] = consPtr(gt, STG_GLOBAL|TAG_COMPOUND);
      *valTermRef(t) = consPtr(&gt[3], STG_GLOBAL|TAG_COMPOUND);

      return TRUE;
    }
    case B_UNIFY_EXIT:
    { if ( debugstatus.debugging )
      { return ( put_call_goal(t, GD->procedures.equals2) &&
		 PL_cons_functor_v(t, FUNCTOR_call1, t) );
      } else
      { return PL_put_atom_chars(t, "unify_exit");
      }
    }
    case I_VAR:		ftor = FUNCTOR_var1;      goto fa_1;
    case I_NONVAR:	ftor = FUNCTOR_nonvar1;   goto fa_1;
    case I_INTEGER:	ftor = FUNCTOR_integer1;  goto fa_1;
    case I_FLOAT:	ftor = FUNCTOR_float1;    goto fa_1;
    case I_NUMBER:	ftor = FUNCTOR_number1;   goto fa_1;
    case I_ATOMIC:	ftor = FUNCTOR_atomic1;   goto fa_1;
    case I_ATOM:	ftor = FUNCTOR_atom1;     goto fa_1;
    case I_STRING:	ftor = FUNCTOR_string1;   goto fa_1;
    case I_COMPOUND:	ftor = FUNCTOR_compound1; goto fa_1;
    case I_CALLABLE:	ftor = FUNCTOR_callable1; goto fa_1;
    fa_1:
    { Word gt       = allocGlobal(1+1+2);	/* call(f(A)) */
      LocalFrame fr = (LocalFrame)valTermRef(frref);
      Word       v1 = varFrameP(fr, (int)PC[1]);

      if ( !gt )
	return FALSE;

      gt[0] = ftor;
      unify_gl(&gt[1], v1, has_firstvar);
      gt[2] = FUNCTOR_call1;
      gt[3] = consPtr(gt, STG_GLOBAL|TAG_COMPOUND);
      *valTermRef(t) = consPtr(&gt[2], STG_GLOBAL|TAG_COMPOUND);

      return TRUE;
    }
    case A_LT:	ftor = FUNCTOR_smaller2;       goto ar_2;
    case A_LE:	ftor = FUNCTOR_smaller_equal2; goto ar_2;
    case A_GT:	ftor = FUNCTOR_larger2;        goto ar_2;
    case A_GE:	ftor = FUNCTOR_larger_equal2;  goto ar_2;
    case A_EQ:	ftor = FUNCTOR_ar_equals2;     goto ar_2;
    case A_NE:	ftor = FUNCTOR_ar_not_equal2;  goto ar_2;
    ar_2:
    { Number n1, n2;
      term_t av;
      int rc;

      n1 = argvArithStack(2);
      n2 = n1+1;

      rc = ((av = PL_new_term_refs(2)) &&
	    PL_put_number(av+0, n1) &&
	    PL_put_number(av+1, n2) &&
	    PL_cons_functor_v(t, ftor, av) &&
	    PL_cons_functor_v(t, FUNCTOR_call1, t));

      *pop = 2;
      return rc;
    }
    case A_IS:
    { Number     val = argvArithStack(1);
      LocalFrame NFR = LD->query->next_environment;
      term_t       r = consTermRef(argFrameP(NFR, 0));
      term_t av;
      int rc;

      rc = ((av = PL_new_term_refs(2)) &&
	    PL_put_term(av+0, r) &&
	    PL_put_number(av+1, val) &&
	    PL_cons_functor_v(t, FUNCTOR_is2, av) &&
	    PL_cons_functor_v(t, FUNCTOR_call1, t));

      *pop = 1;
      return rc;
    }
    case A_FIRSTVAR_IS:			/* call(A is B) */
    { Number     val = argvArithStack(1);
      LocalFrame  fr = (LocalFrame)valTermRef(frref);
      Word A         = varFrameP(fr, (int)PC[1]);
      term_t       r = consTermRef(A);
      term_t av;
      int rc;

      setVar(*A);
      rc = ((av = PL_new_term_refs(2)) &&
	    PL_put_term(av+0, r) &&
	    PL_put_number(av+1, val) &&
	    PL_cons_functor_v(t, FUNCTOR_is2, av) &&
	    PL_cons_functor_v(t, FUNCTOR_call1, t));

      *pop = 1;
      return rc;
    }
    case A_ADD_FC:
    { Word gt       = allocGlobal(2+1+2+1+2);	/* call(A is B+Int) */
      LocalFrame fr = (LocalFrame)valTermRef(frref);
      Word A        = varFrameP(fr, (int)PC[1]);
      Word B        = varFrameP(fr, (int)PC[2]);
      intptr_t add  = (intptr_t)PC[3];

      if ( !gt )
	return FALSE;

      setVar(*A);
      gt[0] = FUNCTOR_plus2;
      unify_gl(&gt[1], B, has_firstvar);
      gt[2] = consInt(add);
      gt[3] = FUNCTOR_is2;
      unify_gl(&gt[4], A, has_firstvar);
      gt[5] = consPtr(&gt[0], STG_GLOBAL|TAG_COMPOUND);
      gt[6] = FUNCTOR_call1;
      gt[7] = consPtr(&gt[3], STG_GLOBAL|TAG_COMPOUND);
      *valTermRef(t) = consPtr(&gt[6], STG_GLOBAL|TAG_COMPOUND);

      return TRUE;
    }
    case I_CUT:
      return PL_put_atom(t, ATOM_cut);
    case I_ENTER:
      return PL_put_atom(t, ATOM_prove);
    case I_EXIT:
      return PL_put_atom(t, ATOM_exit);
    default:
      assert(0);
      return PL_put_atom_chars(t, codeTable[op].name);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
callBreakHook() calls prolog:break_hook/6 as

    prolog:break_hook(+Clause, +PC, +Frame, +Choice, +Goal, -Action) is semidet.

(*) If put_vm_call() addresses  `F`  (first   var)  variables,  it  will
initialise these to bind to the  goal.   However,  if GC comes along, it
will reset these variables.  Therefore,  we   fake  GC  that  we already
executed this instruction. The price is   that  V (normal var) arguments
are not marked as used, and GC might   thus clean them. We fix that with
protect_var(), which creates a  term-reference   to  the local variable,
such that it is marked from the foreign environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define callBreakHook(frame, bfr, PC, op, pop) LDFUNC(callBreakHook, frame, bfr, PC, op, pop)
static break_action
callBreakHook(DECL_LD LocalFrame frame, Choice bfr,
	      Code PC, code op, int *pop)
{ predicate_t proc;
  fid_t cid;
  term_t frameref, chref, pcref;
  wakeup_state wstate;
  size_t pc_offset;

  *pop = 0;
  if (op == B_UNIFY_VAR || op == B_UNIFY_FIRSTVAR)
  { LD->slow_unify = TRUE;
    goto default_action;
  }
  proc = _PL_predicate("break_hook", 6, "prolog",
		       &GD->procedures.prolog_break_hook6);
  if ( !getProcDefinition(proc)->impl.any.defined )
    goto default_action;

  if ( strchr(codeTable[op].argtype, CA1_FVAR) )
    pc_offset = stepPC(PC)-PC;
  else
    pc_offset = 0;

  SAVE_PTRS();

  /* make enough space to avoid GC/shift in the	critical region*/
  if ( !hasGlobalSpace(10) )
  { int rc;

    if ( (rc=ensureGlobalSpace(10, ALLOW_GC)) != TRUE )
    { raiseStackOverflow(rc);
      return BRK_ERROR;
    }
  }

  if ( saveWakeup(&wstate, FALSE) )
  { if ( (cid=PL_open_foreign_frame()) )
    { term_t argv = PL_new_term_refs(6);
      Clause clause = frame->clause->value.clause;
      qid_t qid;

      RESTORE_PTRS();
      PL_put_clref(argv+0, clause);
      PL_put_intptr(argv+1, PC - clause->codes);
      PL_put_frame(argv+2, frame);
      PL_put_choice(argv+3, bfr);
      if ( ( op == B_UNIFY_EXIT &&
             put_call_goal(argv+4, GD->procedures.equals2) &&
             PL_cons_functor_v(argv+4, FUNCTOR_call1, argv+4) ) ||
           put_vm_call(argv+4, frameref, PC, op, pc_offset != 0, pop) )
      { DEBUG(CHK_SECURE, checkStacks(NULL));
	if ( (qid = PL_open_query(MODULE_user,
				  PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION, proc, argv)) )
	{ int rc;

	  LD->query->parent->registers.pc += pc_offset; /* see (*) */
	  rc = PL_next_solution(qid);
	  LD->query->parent->registers.pc -= pc_offset;
	  PL_cut_query(qid);

	  if ( rc )
	  { atom_t a_action;
	    break_action action;

	    if ( PL_get_atom(argv+5, &a_action) )
	    { if ( a_action == ATOM_continue )
	      { action = BRK_CONTINUE;
	      } else if ( a_action == ATOM_trace )
	      { action = BRK_TRACE;
	      } else if ( a_action == ATOM_debug )
	      { action = BRK_DEBUG;
	      } else
		goto invalid_action;

	      PL_close_foreign_frame(cid);
	      restoreWakeup(&wstate);

	      return action;
	    } else if ( PL_is_functor(argv+5, FUNCTOR_call1) )
	    { LocalFrame NFR = LD->query->next_environment;
	      Word p = valTermRef(argv+5);

	      deRef(p);
	      assert(hasFunctor(p[0], FUNCTOR_call1));
	      p = argTermP(*p, 0);
	      deRef(p);
	      argFrame(NFR, 0) = *p;

	      PL_close_foreign_frame(cid);
	      restoreWakeup(&wstate);

	      return BRK_CALL;
	    } else
	    { invalid_action:
	      PL_warning("prolog:break_hook/6: invalid action");
	    }
	  }
	}
      }

      PL_discard_foreign_frame(cid);
    }
    restoreWakeup(&wstate);
  }

default_action:
  if ( exception_term )
    return BRK_ERROR;
  if ( debugstatus.debugging )
    return BRK_TRACE;

  return BRK_CONTINUE;
}

#undef SAVE_PTRS
#undef RESTORE_PTRS


		 /*******************************
		 *    DESTRUCTIVE ASSIGNMENT	*
		 *******************************/

#ifdef O_DESTRUCTIVE_ASSIGNMENT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trailing of destructive assignments. This feature   is  used by setarg/3
and put_attr/2.

Such an assignment is trailed by first  pushing the assigned address (as
normal) and then pushing a marked pointer to  a cell on the global stack
holding the old (overwritten) value.

Undo is slightly more complicated as it has to check for these special
cells on the trailstack.

The garbage collector has to take care in  a number of places: it has to
pass through the trail-stack, marking   the  global-stack references for
assigned data and the sweep_trail() must be   careful about this type of
marks.

Typically, this is only called to modify   values in terms in the global
stack. Unfortunately it is also called  for   the  head  and tail of the
wakeup list which is allocated on the local stack.

TBD: allocate the head and tail of the  wakeup list on the global stack.
Possibly this should also hold for the other `special term references'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
TrailAssignment(DECL_LD Word p)
{ assert(gTop+1 <= gMax && tTop+2 <= tMax);
  assert(!(*p & (MARK_MASK|FIRST_MASK)));

  if ( p < LD->mark_bar || p >= (Word)lBase )
  { Word old = gTop;

    gTop++;
    *old = *p;				/* save the old value on the global */
    (tTop++)->address = p;
    (tTop++)->address = tagTrailPtr(old);
  }
}


#ifdef O_ATTVAR
#define reclaim_attvars(after) LDFUNC(reclaim_attvars, after)
static void
reclaim_attvars(DECL_LD Word after)
{ while ( LD->attvar.attvars >= after )
  { word w = *LD->attvar.attvars;

    if ( isVar(w) )
      LD->attvar.attvars = NULL;
    else
      LD->attvar.attvars = unRef(w);
  }
}
#endif


#define __do_undo(m) LDFUNC(__do_undo, m)
static inline void
__do_undo(DECL_LD mark *m)
{ TrailEntry tt = tTop;
  TrailEntry mt = m->trailtop;

  while(--tt >= mt)
  { Word p = tt->address;

    if ( isTrailVal(p) )
    { DEBUG(2, Sdprintf("Undoing a trailed assignment\n"));
      tt--;
      if ( tt->address == gBase )
	push_undo(tt->address);
      *tt->address = trailVal(p);
      DEBUG(CHK_SECURE,
	    if ( isAttVar(*tt->address) )
	      assert(on_attvar_chain(tt->address)));
      assert(!(*tt->address & (MARK_MASK|FIRST_MASK)));
    } else
      setVar(*p);
  }

  tTop = mt;
  if ( LD->frozen_bar > m->globaltop )
  { DEBUG(CHK_SECURE, assert(gTop >= LD->frozen_bar));
    reclaim_attvars(LD->frozen_bar);
    gTop = LD->frozen_bar;
  } else
  { reclaim_attvars(m->globaltop);
    gTop = m->globaltop;
  }
}


void
do_undo(mark *m)
{ GET_LD
  __do_undo(m);
}

#undef Undo
#define Undo(m) __do_undo(&m)
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/


		 /*******************************
		 *	    PROCEDURES		*
		 *******************************/

/* Note that we use PL_malloc_uncollectable() here because the pointer in
   our block is not the real memory pointer.
*/

#ifdef O_PLMT
#define localDefinition(def) LDFUNC(localDefinition, def)
static Definition
localDefinition(DECL_LD Definition def)
{ unsigned int tid = LD->thread.info->pl_tid;
  size_t idx = MSB(tid);
  LocalDefinitions v = def->impl.local.local;

  if ( !v->blocks[idx] )
  { size_t bs = (size_t)1<<idx;
    Definition *newblock;

    if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(Definition))) )
      outOfCore();

    memset(newblock, 0, bs*sizeof(Definition));
    if ( !COMPARE_AND_SWAP_PTR(&v->blocks[idx], NULL, newblock-bs) )
      PL_free(newblock);
  }

  if ( !v->blocks[idx][tid] )
    v->blocks[idx][tid] = localiseDefinition(def);

  return v->blocks[idx][tid];
}

void
destroyLocalDefinition(Definition def, unsigned int tid)
{ size_t idx = MSB(tid);
  LocalDefinitions v = def->impl.local.local;
  Definition local;

  DEBUG(MSG_PRED_COUNT,
	Sdprintf("Free local def[%d] for %s at %p\n",
		 tid, predicateName(def), def));

  local = v->blocks[idx][tid];
  v->blocks[idx][tid] = NULL;
  destroyDefinition(local);
}
#endif

Definition
getLocalProcDefinition(DECL_LD Definition def)
{
#ifdef O_PLMT
  if ( true(def, P_THREAD_LOCAL) )
  { MEMORY_ACQUIRE();
    return localDefinition(def);
  }
#endif

  return def;
}


Definition
getProcDefinitionForThread(Definition def, unsigned int tid)
{ size_t idx = MSB(tid);
  LocalDefinitions v = def->impl.local.local;

  if ( !v->blocks[idx] )
    return NULL;

  return v->blocks[idx][tid];
}


#define getProcDefinedDefinition(def) LDFUNC(getProcDefinedDefinition, def)
static inline Definition
getProcDefinedDefinition(DECL_LD Definition def)
{ if ( !def->impl.any.defined && false(def, PROC_DEFINED) )
    def = trapUndefined(def);

#ifdef O_PLMT
  if ( true(def, P_THREAD_LOCAL) )
    return getLocalProcDefinition(def);
#endif

  return def;
}


Module
contextModule(LocalFrame fr)
{ for(; fr; fr = fr->parent)
  { if ( true(fr, FR_CONTEXT) )
      return fr->context;
    if ( false(fr->predicate, P_TRANSPARENT) )
      return fr->predicate->module;
  }

  return MODULE_user;
}


static inline void
setContextModule__(LocalFrame fr, Module context)
{ fr->context = context;
  set(fr, FR_CONTEXT);
}


void
setContextModule(LocalFrame fr, Module context)
{ setContextModule__(fr, context);
}
#define setContextModule(fr, ctx) setContextModule__(fr, ctx)


/* Earlier versions tested for <atom>:X, but this makes it very hard
   to write predicates such as current_resource/3. This is also
   compatible to at least SICStus and YAP.
*/

#define is_qualified(p) LDFUNC(is_qualified, p)
static inline int
is_qualified(DECL_LD Word p)
{ return hasFunctor(*p, FUNCTOR_colon2);
}


#define m_qualify_argument(fr, arg) LDFUNC(m_qualify_argument, fr, arg)
static int
m_qualify_argument(DECL_LD LocalFrame fr, int arg)
{ Word k = varFrameP(fr, arg);
  Word p;

  deRef2(k, p);
  if ( !is_qualified(p) )
  { Word p2;

    if ( !hasGlobalSpace(3) )
    { int rc;
      term_t fref = consTermRef(fr);

      lTop = (LocalFrame)argFrameP(fr, fr->predicate->functor->arity);
      if ( (rc=ensureGlobalSpace(3, ALLOW_GC)) != TRUE )
	return rc;

      fr = (LocalFrame)valTermRef(fref);
      k = varFrameP(fr, arg);
      deRef2(k, p);
    }

    p2 = gTop;
    gTop += 3;
    p2[0] = FUNCTOR_colon2;
    p2[1] = contextModule(fr)->name;
    if ( isVar(*p) && p > (Word)lBase )
    { setVar(p2[2]);
      LTrail(p);
      *p = makeRefG(&p2[2]);
    } else
    { p2[2] = (needsRef(*p) ? makeRefG(p) : *p);
    }
    *k = consPtr(p2, STG_GLOBAL|TAG_COMPOUND);
  } else
  { int depth = 100;

    for(;;)
    { Word p2 = argTermP(*p, 1);
      Word ap;

      deRef2(p2, ap);
      if ( is_qualified(ap) )
      { Word a1 = argTermP(*p, 0);
	deRef(a1);
	if (! isAtom(*a1))
	  break;
	p = ap;
	if ( --depth == 0 && !is_acyclic(p) )
	{ term_t t = pushWordAsTermRef(p);
	  PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_acyclic_term, t);
	  popTermRef();
	  return FALSE;
	}
      } else
      { break;
      }
    }

    *k = *p;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Verify that p is  of  the   shape  @(Goal,Module)  that  is sufficiently
instantiated to avoid teh compiler to generate a meta-call for it. Other
errors will find their way to the user in other ways.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define checkCallAtContextInstantiation(p) LDFUNC(checkCallAtContextInstantiation, p)
static int
checkCallAtContextInstantiation(DECL_LD Word p)
{ Word g, m;
  atom_t pm;

  deRef(p);

  m = argTermP(*p, 1);
  deRef(m);
  if ( canBind(*m) )
    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
  g = argTermP(*p, 0);
  deRef(g);
  if ( !(g=stripModuleName(g, &pm)) )
    return FALSE;
  if ( hasFunctor(*g, FUNCTOR_colon2) )
  { m = argTermP(*g, 0);
    deRef(m);
    if ( canBind(*m) )
      return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    g = argTermP(*g, 1);
    deRef(g);
  }
  if ( canBind(*g) )
    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
foreignWakeup() calls delayed goals while executing a foreign procedure.
Note that the  choicepoints  of  the   awoken  code  are  destroyed  and
therefore this code can only be used in places introducing an (implicit)
cut such as \=/2 (implemented as A \= B :- ( A = B -> fail ; true )).

Can perform GC/shift and may leave overflow exceptions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
foreignWakeup(DECL_LD term_t ex)
{ if ( unlikely(LD->alerted & ALERT_WAKEUP) )
  { LD->alerted &= ~ALERT_WAKEUP;

    if ( *valTermRef(LD->attvar.head) )
    { fid_t fid;

      if ( (fid=PL_open_foreign_frame()) )
      { term_t a0 = PL_new_term_ref();
	int rval = FALSE;
	qid_t qid;

	PL_put_term(a0, LD->attvar.head);
	if ( (qid = PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, PROCEDURE_dwakeup1, a0)) )
	{ setVar(*valTermRef(LD->attvar.head));
	  setVar(*valTermRef(LD->attvar.tail));
	  rval = PL_next_solution(qid);
	  if ( rval == FALSE )
	  { term_t t = PL_exception(qid);

	    if ( t )
	      PL_put_term(ex, t);
	  }
	  PL_cut_query(qid);
	}

	PL_close_foreign_frame(fid);

	return rval;
      }

      PL_put_term(ex, exception_term);
      return FALSE;
    }
  }

  return TRUE;
}


		 /*******************************
		 *	     EXCEPTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called at the end of handling an exception. We cannot do GC, however, we
can request it, after it will be executed   at the start of the recovery
handler. If no GC is needed the is enoush space so, we call trimStacks()
to re-enable the spare stack-space if applicable.

TBD: In these modern days we can  probably   do  GC. Still, if it is not
needed why would we?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
resumeAfterException(int clear, Stack outofstack)
{ GET_LD

  if ( clear )
  { exception_term = 0;
    LD->exception.fr_rewritten = 0;
    setVar(*valTermRef(LD->exception.bin));
    setVar(*valTermRef(LD->exception.printed));
    setVar(*valTermRef(LD->exception.pending));
  }

  if ( outofstack && outofstack->gc )
  { LD->stacks.global.gced_size = 0;
    LD->stacks.trail.gced_size  = 0;
  }

  if ( !considerGarbageCollect((Stack)NULL) )
  { trimStacks((outofstack != NULL));
  } else
  { trimStacks(FALSE);		/* just re-enable the spare stacks */
    if ( outofstack != NULL )
      LD->trim_stack_requested = TRUE;	/* next time with resize */
  }

  LD->exception.processing = FALSE;
  LD->outofstack = NULL;

#ifdef O_PLMT
  updatePendingThreadSignals();
#endif
}


static void
exceptionUnwindGC(void)
{ GET_LD

  LD->stacks.global.gced_size = 0;
  LD->stacks.trail.gced_size = 0;
  LD->trim_stack_requested = TRUE;
  if ( considerGarbageCollect(NULL) )
  { garbageCollect(GC_EXCEPTION);
    enableSpareStacks();
  }
}


		 /*******************************
		 *   FOREIGN-LANGUAGE INTERFACE *
		 *******************************/

#ifdef O_DEBUGGER
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
findStartChoice(LocalFrame fr, Choice ch)
    Within the same query, find the choice-point that was created at the
    start of this frame.  This is used for the debugger at the fail-port
    as well as for realising retry.

    Note that older versions also considered the initial choicepoint a
    choicepoint for the initial frame, but this is not correct as the
    frame may be replaced due to last-call optimisation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Choice
findStartChoice(LocalFrame fr, Choice ch)
{ for( ; (void *)ch > (void *)fr; ch = ch->parent )
  { if ( ch->frame == fr )
    { switch ( ch->type )
      { case CHP_JUMP:
	  continue;			/* might not be at start */
	default:
	  return ch;
      }
    }
  }

  return NULL;
}


static Choice
findChoiceBeforeFrame(LocalFrame fr, Choice ch)
{ while ( (void*)ch > (void*)fr )
    ch = ch->parent;

  return ch;
}

#endif /*O_DEBUGGER*/


#if O_CATCHTHROW
		/********************************
		*        EXCEPTION SUPPORT      *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the I_EXIT of catch/3. We use this as the return address of catch/3
when running the handler. Maybe we can remove the catch/3 in the future?
This would also fix the problem that  we   need  to be sure not to catch
exceptions from the handler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Code
findCatchExit()
{ if ( !GD->exceptions.catch_exit_address )
  { Definition catch3 = PROCEDURE_catch3->definition;
    Clause cl = catch3->impl.clauses.first_clause->value.clause;
    Code Exit = &cl->codes[cl->code_size-1];
    assert(*Exit == encode(I_EXIT));

    GD->exceptions.catch_exit_address = Exit;
  }

  return GD->exceptions.catch_exit_address;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the frame running catch/3. If we found  it, we will mark this frame
and not find it again, as a catcher   can  only catch once from the 1-st
argument goal. Exceptions from the  recover   goal  should be passed (to
avoid a loop and allow for re-throwing).   With  thanks from Gertjan van
Noord.

findCatcher() can do  GC/shift!  The  return   value  is  a  local-frame
reference, so we can deal with relocation of the local stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define findCatcher(fr, ch, ex) LDFUNC(findCatcher, fr, ch, ex)
static term_t
findCatcher(DECL_LD LocalFrame fr, Choice ch, term_t ex)
{ Definition catch3  = PROCEDURE_catch3->definition;

  for(; fr; fr = fr->parent)
  { int rc;
    term_t tref, catcher;

    if ( fr->predicate != catch3 )
      continue;
    if ( true(fr, FR_CATCHED) )
      continue;				/* thrown from recover */
    if ( (void*)fr > (void*)ch )
      continue;				/* call-port of catch/3 */

    tref = consTermRef(fr);
    catcher = consTermRef(argFrameP(fr, 1));
    DEBUG(MSG_THROW, Sdprintf("Unify ball for frame %ld\n", (long)tref));
    rc = PL_unify(catcher, ex);
    fr = (LocalFrame)valTermRef(tref);

    if ( rc )
    { DEBUG(MSG_THROW, Sdprintf("Unified for frame %ld\n", (long)tref));
      set(fr, FR_CATCHED);
      return consTermRef(fr);
    }
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See whether some outer  environment  will   catch  this  exception. I.e.
catch(Goal, ...), where Goal calls C, calls   Prolog  and then raises an
exception somewhere.

Note that when throwing from a catch/3,   the  catcher is subject to GC.
Hence, we should not call can_unify() if  it has been garbage collected.
Doing so generally does no harm as the unification will fail, but is not
elegant and traps an assert() in do_unify().

Returns:

  - term-reference to catch/3 frame
  - (term_t)0 if not caught
  - (term_t)-1 if caught in C
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef offset
#define offset(s, f) ((size_t)(&((struct s *)NULL)->f))
#endif

#ifdef O_DEBUGGER
#define isCaughtInOuterQuery(qid, ball) LDFUNC(isCaughtInOuterQuery, qid, ball)
static term_t
isCaughtInOuterQuery(DECL_LD qid_t qid, term_t ball)
{ Definition catch3 = PROCEDURE_catch3->definition;
  QueryFrame qf = QueryFromQid(qid);

  while( qf && true(qf, PL_Q_PASS_EXCEPTION) )
  { LocalFrame fr = qf->saved_environment;

    if ( !fr )
      break;

    while( fr )
    { if ( fr->predicate == catch3 )
      { term_t fref  = consTermRef(fr);
	Word catcher = argFrameP(fr, 1);

	deRef(catcher);

	if ( *catcher != ATOM_garbage_collected &&
	     can_unify(catcher,		/* may shift */
		       valTermRef(ball),
		       0) )
	  return fref;
	fr = (LocalFrame)valTermRef(fref);
      }

      if ( fr->parent )
      { fr = fr->parent;
      } else
      { qf = queryOfFrame(fr);
	break;
      }
    }
  }

  if ( qf && true(qf, PL_Q_CATCH_EXCEPTION|PL_Q_PASS_EXCEPTION) )
    return (term_t)-1;

  return 0;
}


#define uncachableException(t) LDFUNC(uncachableException, t)
static word
uncachableException(DECL_LD term_t t)
{ Word p = valTermRef(t);

  deRef(p);
  if ( *p == ATOM_aborted )
    return *p;

  return 0;
}

#define resourceException(t) LDFUNC(resourceException, t)
static word
resourceException(DECL_LD term_t t)
{ Word p = valTermRef(t);

  deRef(p);
  if ( hasFunctor(*p, FUNCTOR_error2) )
  { p = argTermP(*p, 0);
    deRef(p);
    if ( hasFunctor(*p, FUNCTOR_resource_error1) )
    { p = argTermP(*p, 0);
      deRef(p);
      return *p;
    }
  }

  return 0;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dbgRedoFrame(LocalFrame fr)

Find the frame we report  for  a  retry.   If  the  current  frame  is a
debugable frame, we only debug if it is a user predicate.

If the current frame  is  not  debuggable   we  have  a  choicepoint  in
non-debug code. We walk up the stack to   find  a debug frame. If we are
already in the box of this frame, we   have  an internal retry of called
system predicate, which we should not trace. If we are outside the `box'
however, we must trace the toplevel visible predicate.

FR_INBOX is maintained when the debugger is   active.  It is set on CALL
and REDO and reset on EXIT. So, if we find this set, we are dealing with
an internal retry and otherwise we have an external retry.

The cht argument is one of CHP_CLAUSE   or CHP_JUMP, indicating the type
of redo.  We always show redo for an external redo.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define dbgRedoFrame(fr, cht) LDFUNC(dbgRedoFrame, fr, cht)
static LocalFrame
dbgRedoFrame(DECL_LD LocalFrame fr, choice_type cht)
{ DEBUG(MSG_TRACE, Sdprintf("REDO on [%d] %s\n",
			    (int)levelFrame(fr), predicateName(fr->predicate)));

  if ( SYSTEM_MODE )
    return fr;				/* system mode; debug everything */
  if ( isDebugFrame(fr) && false(fr->predicate, HIDE_CHILDS) )
    return fr;				/* normal user code */
  for( ; fr && fr->parent && true(fr->parent->predicate, HIDE_CHILDS);
       fr = fr->parent)
    ;					/* find top of hidden children */
  DEBUG(MSG_TRACE, if ( fr )
	Sdprintf("REDO user frame of [%d] %s%s\n",
		 (int)levelFrame(fr),
		 predicateName(fr->predicate),
		 true(fr, FR_INBOX) ? " (inbox)" : ""));
  if ( fr && false(fr, FR_INBOX) )
  { set(fr, FR_INBOX);			/* External retry */
    return fr;
  }

  return NULL;
}

#endif /*O_DEBUGGER*/

#define exception_hook(pqid, fr, catchfr_ref) LDFUNC(exception_hook, pqid, fr, catchfr_ref)
static int
exception_hook(DECL_LD qid_t pqid, term_t fr, term_t catchfr_ref)
{ if ( PROCEDURE_exception_hook4->definition->impl.clauses.first_clause )
  { if ( !LD->exception.in_hook )
    { wakeup_state wstate;
      qid_t qid;
      term_t av, ex = 0;
      int debug, trace, rc;

      LD->exception.in_hook++;
      if ( !saveWakeup(&wstate, TRUE) )
	return FALSE;

      av = PL_new_term_refs(4);
      PL_put_term(av+0, exception_bin);
      PL_put_frame(av+2, (LocalFrame)valTermRef(fr));

      if ( !catchfr_ref )
	catchfr_ref = isCaughtInOuterQuery(pqid, exception_bin);
      if ( catchfr_ref == (term_t)-1 )
      { PL_put_atom_chars(av+3, "C");
      } else if ( catchfr_ref && catchfr_ref == LD->exception.fr_rewritten )
      { DEBUG(MSG_THROW,
	    Sdprintf("Already rewritting exception for frame %d\n",
		     catchfr_ref));
	rc = FALSE;
	goto done;
      } else if ( catchfr_ref )
      { LocalFrame cfr = (LocalFrame)valTermRef(catchfr_ref);
	cfr = parentFrame(cfr);
	PL_put_frame(av+3, cfr);
	LD->exception.fr_rewritten = catchfr_ref;
      } else
      { PL_put_frame(av+3, NULL);	/* puts 'none' */
      }

      startCritical();
      qid = PL_open_query(MODULE_user, PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION,
			  PROCEDURE_exception_hook4, av);
      rc = PL_next_solution(qid);
      rc = endCritical() && rc;
      debug = debugstatus.debugging;
      trace = debugstatus.tracing;
      if ( rc )				/* pass user setting trace/debug */
      { PL_cut_query(qid);
	if ( debug ) debugstatus.debugging = TRUE;
	if ( trace ) debugstatus.tracing = TRUE;
	if ( !PL_is_variable(av+1) )
	  ex = av+1;
      } else
      { ex = PL_exception(qid);
	if ( ex )
	{ PL_put_term(av+1, ex);
	  ex = av+1;
	}
	PL_cut_query(qid);
      }

      if ( ex && !PL_same_term(ex, exception_term) )
      {	PL_raise_exception(ex);	/* copy term again */
	wstate.flags |= WAKEUP_STATE_SKIP_EXCEPTION;
	rc = TRUE;			/* handled */
      } else
      { rc = FALSE;
      }

    done:
      restoreWakeup(&wstate);
      LD->exception.in_hook--;

      return rc;
    } else
    { PL_warning("Recursive exception in prolog_exception_hook/4");
    }
  }

  return FALSE;
}


#endif /*O_CATCHTHROW*/


		 /*******************************
		 *	  TAIL-RECURSION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Tail recursion copy of the arguments of the new frame back into the  old
one.   This  should  be  optimised  by the compiler someday, but for the
moment this will do.

The new arguments block can contain the following types:
  - Instantiated data (atoms, ints, reals, strings, terms
    These can just be copied.
  - Plain variables
    These can just be copied.
  - References to frames older than the `to' frame
    These can just be copied.
  - 1-deep references into the `to' frame.
    This is hard as there might be two of  them  pointing  to  the  same
    location  in  the  `to' frame, indicating sharing variables.  In the
    first pass we will fill the  variable  in  the  `to'  frame  with  a
    reference  to the new variable.  If we get another reference to this
    field we will copy the reference saved in the `to'  field.   Because
    on  entry  references into this frame are always 1 deep we KNOW this
    is a saved reference.  The critical program for this is:

	a :- b(X, X).
	b(X, Y) :- X == Y.
	b(X, Y) :- write(bug), nl.

					This one costed me 1/10 bottle of
					brandy to Huub Knops, SWI
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define copyFrameArguments(from, to, argc) LDFUNC(copyFrameArguments, from, to, argc)
static void
copyFrameArguments(DECL_LD LocalFrame from, LocalFrame to, size_t argc)
{ Word ARGD, ARGS, ARGE;

  if ( argc == 0 )
    return;

  ARGS = argFrameP(from, 0);
  ARGE = ARGS+argc;
  ARGD = argFrameP(to, 0);
  while( ARGS < ARGE )			/* now copy them */
    *ARGD++ = *ARGS++;
}

		/********************************
		*          INTERPRETER          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			 MACHINE REGISTERS

  - DEF
    Definition structure of current procedure.
  - PC
    Virtual machine `program counter': pointer to the next byte code  to
    interpret.
  - ARGP
    Argument pointer.  Pointer to the next argument to be matched  (when
    in the clause head) or next argument to be instantiated (when in the
    clause  body).   Saved  and  restored  via  the  argument  stack for
    functors.
  - FR
    Current environment frame
  - BFR
    Frame where execution should continue if  the  current  goal  fails.
    Used by I_CALL and deviates to fill the backtrackFrame slot of a new
    frame and set by various instructions.
  - deterministic
    Last clause has been found deterministically
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG_BACKTRACK
int backtrack_from_line;
choice_type last_choice;
#define GO(helper) do { backtrack_from_line = __LINE__; VMH_GOTO(helper); } while(0)
#else
#define GO(helper) VMH_GOTO(helper)
#endif

#define FRAME_FAILED		GO(deep_backtrack)
#define CLAUSE_FAILED		GO(shallow_backtrack)
#define BODY_FAILED		GO(shallow_backtrack)
#ifdef O_DEBUGGER
#define TRACE_RETRY		VMH_GOTO(retry)
#endif
#ifdef O_ATTVAR
#define ATTVAR_WAKEUP		VMH_GOTO(wakeup)
#endif
#ifdef O_DEBUG
#define THROW_EXCEPTION		do { THROWED_FROM_LINE = __LINE__; \
				     _THROW_EXCEPTION; } while(0)
#else
#define THROW_EXCEPTION		do { _THROW_EXCEPTION; } while(0)
#endif
#define _THROW_EXCEPTION	VMH_GOTO(b_throw)

#ifdef O_PROFILE
#define Profile(g) if ( unlikely(LD->profile.active) ) g
#else
#define Profile(g) (void)0
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{leave,discard}Frame()
     Exit from a frame.  leaveFrame() is used for normal leaving due to
     failure.  discardFrame() is used for frames that have
     been cut.  If such frames are running a foreign predicate, the
     functions should be called again using FRG_CUTTED context.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
leaveFrame(LocalFrame fr)
{ //Definition def = fr->predicate;

  fr->clause = NULL;
  leaveDefinition(def);
}


#define discardFrame(fr) LDFUNC(discardFrame, fr)
static void
discardFrame(DECL_LD LocalFrame fr)
{ Definition def = fr->predicate;

  DEBUG(2, Sdprintf("discard #%d running %s\n",
		    loffset(fr),
		    predicateName(fr->predicate)));

  if ( true(def, P_FOREIGN) )
  { if ( fr->clause )
    { discardForeignFrame(fr);
      fr->clause = NULL;
    }
  } else
  { fr->clause = NULL;	/* leaveDefinition() may destroy clauses (no more) */
    leaveDefinition(def);
  }
}


/* TRUE if fr is in the continuation of frame or the frame of ch or one
 * of its parents.
 */
static int
in_continuation(LocalFrame fr, LocalFrame frame, Choice ch)
{ for(;;)
  { while(frame > fr)
      frame = frame->parent;
    if ( frame == fr )
      return TRUE;

    if ( (void*)ch > (void*)fr )
    { frame = ch->frame;
      ch = ch->parent;
    } else
    { return FALSE;
    }
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Discard all choice-points created after  the   creation  of the argument
environment. See also discardFrame().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(O_DEBUG) || defined(SECURE_GC) || defined(O_MAINTENANCE)
char *
chp_chars(Choice ch)
{ GET_LD
  static char buf[256];

  Ssnprintf(buf, sizeof(buf), "Choice at #%ld for frame #%ld (%s), type %s",
	    loffset(ch), loffset(ch->frame),
	    predicateName(ch->frame->predicate),
	    ch->type == CHP_JUMP ? "JUMP" :
	    ch->type == CHP_CLAUSE ? "CLAUSE" :
	    ch->type == CHP_TOP ? "TOP" :
	    ch->type == CHP_DEBUG ? "DEBUG" :
	    ch->type == CHP_CATCH ? "CATCH" : "NONE");

  return buf;
}
#endif


int
existingChoice(DECL_LD Choice ch)
{ if ( onStack(local, ch) && onStack(local, ch->frame) &&
       (int)ch->type >= 0 && (int)ch->type <= CHP_DEBUG )
  { Choice ch2;

    for(ch2 = BFR; ch2 > ch; ch2 = ch2->parent)
      ;
    if ( ch2 == ch )
      return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
discardChoicesAfter() discards all choicepoints created  after fr, while
calling  possible  hooks  on   the   frames.    If   the   `reason`   is
FINISH_EXTERNAL_EXCEPT_UNDO, Undo() is called  on   the  oldest  removed
choicepoint (before it is  actually   removed).  Older versions returned
this choicepoint, but as it is already removed, this is not safe.

GC interaction is tricky here. See also   C_CUT.  The loop needs to call
the  cleanup  handlers  and  call   discardFrame().  The  latter  resets
LocalFrame->clause to NULL. This means  that   these  frames  may not be
visible to GC. In older versions, this was achieved using two loops: one
for the cleanup and one to discard the  frames, but if Undo() is called,
the resets may corrupt the datastructures,   which makes the second loop
fail. We now moved discardFrame() back into the primary loop and set BFR
before calling frameFinished() such that the discarded frames are really
invisible to GC.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define discardChoicesAfter(fr, reason) LDFUNC(discardChoicesAfter, fr, reason)
static void
discardChoicesAfter(DECL_LD LocalFrame fr, enum finished reason)
{ if ( (LocalFrame)BFR > fr )
  { Choice me;

    for(me = BFR; ; me=me->parent)
    { LocalFrame fr2;
      LocalFrame delto;
      int me_undone = FALSE;

      if ( me->parent && me->parent->frame > fr )
	delto = me->parent->frame;
      else
	delto = fr;

      DEBUG(3, Sdprintf("Discarding %s\n", chp_chars(me)));

      for(fr2 = me->frame;
	  fr2 > delto;
	  fr2 = fr2->parent)
      { assert(onStack(local, me));
	assert(onStack(local, fr2));
	assert(fr2->clause || true(fr2->predicate, P_FOREIGN));

	if ( true(fr2, FR_WATCHED) )
	{ char *lSave = (char*)lBase;

	  if ( !me_undone && is_exception_finish(reason) )
	  { me_undone = TRUE;
	    Undo(me->mark);
	    DiscardMark(me->mark);
	  }
	  BFR = me;
	  frameFinished(fr2, reason);
	  BFR = BFR->parent;
	  if ( lSave != (char*)lBase )	/* shifted */
	  { intptr_t offset = (char*)lBase - lSave;

	    me  = addPointer(me, offset);
	    fr  = addPointer(fr, offset);
	    fr2 = addPointer(fr2, offset);
	    delto = addPointer(delto, offset);
	  }
#if 0					/* What to do if we have multiple */
	  if ( exception_term )		/* handlers and multiple exceptions? */
	    break;
#endif
	}

	discardFrame(fr2);
      }

      if ( (LocalFrame)me->parent <= fr )
      { if ( !me_undone )
	{ if ( reason == FINISH_EXTERNAL_EXCEPT_UNDO )
	    Undo(me->mark);
	  DiscardMark(me->mark);
	}
	BFR = me->parent;
	return;
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Discard choicepoints in debugging mode.  As we might be doing callbacks
on behalf of the debugger we need to preserve the pending exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define dbg_discardChoicesAfter(fr, reason) LDFUNC(dbg_discardChoicesAfter, fr, reason)
static void
dbg_discardChoicesAfter(DECL_LD LocalFrame fr, enum finished reason)
{ if ( exception_term )
  { Word p = valTermRef(exception_term);
    word w;

    DEBUG(3, Sdprintf("dbg_discardChoicesAfter(): saving exception: ");
	     pl_writeln(exception_term));
    deRef(p);
    w = *p;
    assert(!isVar(w));
    PushVal(w);
    exception_term = 0;
    discardChoicesAfter(fr, reason);
    PopVal(w);
    *valTermRef(exception_bin) = w;
    exception_term = exception_bin;
  } else
  { discardChoicesAfter(fr, reason);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
newChoice(CH_*, FR) Creates a new  choicepoint.   After  creation of the
choice-point, the user has to fill the choice-points mark as well as the
required context value.

Note that a frame has only one choicepoint associated, except for choice
points created from C_OR. Therefore, C_OR  ensures there is space; space
for the one other choicepoint is ensured of a local frame is created.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Choice
newChoice(DECL_LD choice_type type, LocalFrame fr)
{ Choice ch = (Choice)lTop;

  DEBUG(0, assert(ch+1 <= (Choice)lMax));
  DEBUG(0, assert(BFR < ch));
  lTop = (LocalFrame)(ch+1);

  ch->type = type;
  ch->frame = fr;
  ch->parent = BFR;
  Mark(ch->mark);
#ifdef O_PROFILE
  ch->prof_node = LD->profile.current;
#endif
  BFR = ch;
  DEBUG(3, Sdprintf("NEW %s\n", chp_chars(ch)));

  return ch;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Op top of the query frame there are two   local frames. The top one is a
dummy one, just enough to satisfy stack-walking   and GC. The first real
one has a programPointer pointing to  I_EXITQUERY, doing the return from
a query.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define NDEBUG_SAVE_FLAGS RUN_MODE_NORMAL

qid_t
PL_open_query(Module ctx, int flags, Procedure proc, term_t args)
{ GET_LD
  QueryFrame qf;
  LocalFrame fr, top;
  Definition def;
  size_t i, arity;
  Word ap;
  size_t lneeded;
  static int top_initialized = FALSE;
  static struct clause clause;
  static struct clause_ref cref;

  if ( !top_initialized )
  { clause.predicate = PROCEDURE_dc_call_prolog->definition;
    clause.generation.erased = ~(gen_t)0;
    clause.code_size = 1;
    clause.codes[0] = encode(I_EXITQUERY);
    cref.value.clause = &clause;

    top_initialized = TRUE;
  }

  DEBUG(2, { FunctorDef f = proc->definition->functor;
	     size_t n;

	     Sdprintf("PL_open_query: %s(", stringAtom(f->name));
	     for(n=0; n < f->arity; n++)
	     { if ( n > 0 )
		 Sdprintf(", ");
	       PL_write_term(Serror, args+n, 999, 0);
	     }
	     Sdprintf(")\n");
	   });
  DEBUG(CHK_SECURE, checkStacks(NULL));
  assert((void*)fli_context > (void*)environment_frame);
  assert((Word)lTop >= refFliP(fli_context, fli_context->size));

					/* resolve can call-back */
  def = getProcDefinedDefinition(proc->definition);
  arity = def->functor->arity;

#ifdef JMPBUF_ALIGNMENT
  lneeded = JMPBUF_ALIGNMENT + sizeof(struct queryFrame)+MAXARITY*sizeof(word);
#else
  lneeded = sizeof(struct queryFrame)+MAXARITY*sizeof(word);
#endif

  if ( !ensureLocalSpace(lneeded) )
    return (qid_t)0;
  for(i=0; i<arity; i++)
  { if ( !globalizeTermRef(args+i) )
      return (qid_t)0;
  }
					/* should be struct alignment, */
					/* but for now, I think this */
					/* is always the same */
  qf = (QueryFrame)lTop;
#ifdef JMPBUF_ALIGNMENT
  while ( (uintptr_t)qf % JMPBUF_ALIGNMENT )
    qf = addPointer(qf, sizeof(word));
#endif
  qf->saved_ltop = lTop;

					/* fill top-frame */
  top	             = &qf->top_frame;
  top->parent        = NULL;
  top->predicate     = PROCEDURE_dc_call_prolog->definition;
  top->programPointer= NULL;
  top->clause        = &cref;
#ifdef O_PROFILE
  if ( LD->profile.active )
    top->prof_node = profCall(top->predicate);
  else
    top->prof_node = NULL;
#endif
  if ( environment_frame )
  { setNextFrameFlags(top, environment_frame);
    clear(top, FR_INRESET);		/* shift/1 can't pass callbacks */
  } else
  { top->flags	     = FR_MAGIC;
    top->level	     = 0;
  }
  fr                 = &qf->frame;
  fr->parent         = top;
  setNextFrameFlags(fr, top);
  set(top, FR_HIDE_CHILDS);
  fr->programPointer = clause.codes;

  DEBUG(CHK_SECURE, checkStacks(NULL));
  assert((uintptr_t)fli_context > (uintptr_t)environment_frame);
  assert((uintptr_t)lTop >= (uintptr_t)(fli_context+1));

  if ( flags == TRUE )			/* compatibility */
    flags = PL_Q_NORMAL;
  else if ( flags == FALSE )
    flags = PL_Q_NODEBUG;
  flags &= 0xff;			/* mask reserved flags */

  qf->magic		= QID_MAGIC;
  qf->foreign_frame	= 0;
  qf->flags		= flags;
  qf->saved_environment = environment_frame;
  qf->saved_bfr		= LD->choicepoints;
  qf->aSave             = aTop;
  qf->solutions         = 0;
  qf->exception		= 0;
  qf->yield.term        = 0;
  qf->registers.fr      = NULL;		/* invalid */
  qf->next_environment  = NULL;		/* see D_BREAK */
					/* fill frame arguments */
  ap = argFrameP(fr, 0);
  { size_t n;
    Word p = valTermRef(args);

    for( n = arity; n-- > 0; p++ )
      *ap++ = linkValI(p);
  }
					/* lTop above the arguments */
  lTop = (LocalFrame)ap;

  DEBUG(3, Sdprintf("Level = %d\n", levelFrame(fr)));
  if ( true(qf, PL_Q_NODEBUG) )
  { set(fr, FR_HIDE_CHILDS);
    suspendTrace(TRUE);
    qf->debugSave = debugstatus.debugging;
    debugstatus.debugging = DBG_OFF;
    qf->flags_saved = (LD->prolog_flag.mask.flags[0] & NDEBUG_SAVE_FLAGS);
    setPrologRunMode(RUN_MODE_NORMAL);
#ifdef O_LIMIT_DEPTH
    qf->saved_depth_limit   = LD->depth_info.limit;
    qf->saved_depth_reached = LD->depth_info.reached;
    LD->depth_info.limit    = DEPTH_NO_LIMIT;
#endif
  }
  fr->predicate      = def;
  fr->clause         = NULL;
					/* create initial choicepoint */
  qf->choice.type   = CHP_TOP;
  qf->choice.parent = NULL;
  qf->choice.frame  = top;
#ifdef O_PROFILE
  qf->choice.prof_node = NULL;
  fr->prof_node = NULL;			/* true? */
#endif
  Mark(qf->choice.mark);
  setGenerationFrame(fr);
					/* context module */
  if ( true(def, P_TRANSPARENT) )
  { if ( ctx )
      setContextModule(fr, ctx);
    else if ( qf->saved_environment )
      setContextModule(fr, contextModule(qf->saved_environment));
    else
      setContextModule(fr, MODULE_user);
  }

					/* publish environment */
  LD->choicepoints  = &qf->choice;
  environment_frame = fr;
  qf->parent = LD->query;
  LD->query = qf;
  qf->qid = allocHeapOrHalt(sizeof(*qf->qid));
  qf->qid->engine = LD;
  qf->qid->offset = consTermRef(qf);

  DEBUG(2, Sdprintf("QID=%p\n", QidFromQuery(qf)));
  updateAlerted(LD);

  return QidFromQuery(qf);
}


static void
discard_query(qid_t qid)
{ QueryFrame qf = QueryFromQid(qid);

  WITH_LD(qid->engine)
  { discardChoicesAfter(&qf->frame, FINISH_CUT);
    qf = QueryFromQid(qid);		/* may be shifted */
    discardFrame(&qf->frame);
    if ( true(&qf->frame, FR_WATCHED) )
    { lTop = (LocalFrame)argFrameP(&qf->frame,
				   qf->frame.predicate->functor->arity);
      frameFinished(&qf->frame, FINISH_CUT);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Restore the environment. If an exception was raised by the query, and no
new  exception  has  been  thrown,  consider    it  handled.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
restore_after_query(QueryFrame qf)
{ GET_LD
  if ( qf->exception && !exception_term )
    *valTermRef(exception_printed) = 0;

  DiscardMark(qf->choice.mark);

  LD->query         = qf->parent;
  LD->choicepoints  = qf->saved_bfr;
  environment_frame = qf->saved_environment;
  aTop		    = qf->aSave;
  lTop		    = qf->saved_ltop;
  if ( true(qf, PL_Q_NODEBUG) )
  { suspendTrace(FALSE);
    debugstatus.debugging = qf->debugSave;
    LD->prolog_flag.mask.flags[0] &= (~NDEBUG_SAVE_FLAGS);
    LD->prolog_flag.mask.flags[0] |= qf->flags_saved;
#ifdef O_LIMIT_DEPTH
    LD->depth_info.limit   = qf->saved_depth_limit;
    LD->depth_info.reached = qf->saved_depth_reached;
#endif /*O_LIMIT_DEPTH*/
  }
  updateAlerted(LD);
  DEBUG(CHK_SECURE, checkStacks(NULL));
}


int
PL_cut_query(qid_t qid)
{ int rc = TRUE;

  if ( qid )
  { WITH_LD(qid->engine)
    { QueryFrame qf = QueryFromQid(qid);

      DEBUG(0, assert(qf->magic == QID_MAGIC));
      if ( qf->foreign_frame )
	PL_close_foreign_frame(qf->foreign_frame);

      if ( false(qf, PL_Q_DETERMINISTIC) )
      { int exbefore = (exception_term != 0);

	discard_query(qid);
	qf = QueryFromQid(qid);
	if ( !exbefore && exception_term != 0 )
	  rc = FALSE;
      }

      restore_after_query(qf);
      qf->magic = QID_CMAGIC;		/* disqualify the frame */

      freeHeap(qid, sizeof(*qid));
    }
  }

  return rc;
}


int
PL_close_query(qid_t qid)
{ int rc = TRUE;

  if ( qid )
  { WITH_LD(qid->engine)
    { QueryFrame qf = QueryFromQid(qid);

      DEBUG(0, assert(qf->magic == QID_MAGIC));
      if ( qf->foreign_frame )
	PL_close_foreign_frame(qf->foreign_frame);

      if ( false(qf, PL_Q_DETERMINISTIC) )
      { int exbefore = (exception_term != 0);

	discard_query(qid);
	qf = QueryFromQid(qid);
	if ( !exbefore && exception_term != 0 )
	  rc = FALSE;
      }

      if ( !(qf->exception && true(qf, PL_Q_PASS_EXCEPTION)) )
	Undo(qf->choice.mark);

      restore_after_query(qf);
      qf->magic = QID_CMAGIC;		/* disqualify the frame */
      freeHeap(qid, sizeof(*qid));
    }
  }

  return rc;
}


qid_t
PL_current_query(void)
{ GET_LD

  if ( HAS_LD )
  { if ( LD->query && LD->query->magic == QID_MAGIC )
      return QidFromQuery(LD->query);
  }

  return 0;
}

int
PL_can_yield(void)
{ GET_LD

  return ( HAS_LD &&
	   LD->query &&
	   LD->query->magic == QID_MAGIC &&
	   true(LD->query, PL_Q_ALLOW_YIELD) );
}

PL_engine_t
PL_query_engine(qid_t qid)
{ return qid->engine;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_exception(qid) is used to extract exceptions   from an query executed
using  PL_next_solution().  The  term-reference  itself   is  no  longer
referenced and therefore we must create a new one and copy the term.

If qid == 0, we return the currently pending exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

term_t
PL_exception(qid_t qid)
{ GET_LD

  if ( qid )
  { QueryFrame qf = QueryFromQid(qid);

    if ( qf->exception )
    { term_t ex;

      if ( (void*)fli_context <= (void*)environment_frame )
	fatalError("PL_exception(): No foreign environment");

      ex = PL_new_term_ref();
      PL_put_term(ex, qf->exception);
      return ex;
    }

    return 0;
  } else
    return exception_term;
}


term_t
PL_yielded(qid_t qid)
{ QueryFrame qf = QueryFromQid(qid);

  return qf->yield.term;
}


#define SAVE_REGISTERS(qid) \
	{ QueryFrame qf = QueryFromQid(qid); \
	  qf->registers.fr   = FR; \
	  qf->registers.argp = ARGP; \
	  qf->registers.pc   = PC; \
	}
#define LOAD_REGISTERS(qid) \
	{ QueryFrame qf = QueryFromQid(qid); \
	  FR   = qf->registers.fr; \
	  ARGP = qf->registers.argp; \
	  PC   = qf->registers.pc; \
	  qf->registers.fr = NULL; \
	}

typedef enum
{ uread = 0,				/* Unification in read-mode */
  uwrite				/* Unification in write mode */
} unify_mode;

/* HACK: uncomment the following line to enable function mode */
/* #define O_VMI_FUNCTIONS 1 */
/* #define VMI_REGISTER_VARIABLES 1 */
/* #define VMI_USE_REGISTER_VARIABLES 1 */
/* registers here MUST be in the list of callee-saved registers! */
#define LD_REGISTER "rbx"
#define REGFILE_REGISTER "r12"

#include "pentium.h"

#if O_VMI_FUNCTIONS
struct register_file;
#define VMI_RETTYPE Code
# if VMI_USE_REGISTER_VARIABLES
#  define VMI_ARG_DECL Code PC
#  define VMI_ARG_PASS PC
# else
/* Avoiding the LDFUNC builtins entirely here, because they don't play
 * nicely with indirect usage, and the VMI-function/non-VMI-function
 * switch is already complicated enough. Luckily, the VMI functions
 * are entirely self-contained and they are only ever called from our
 * macros (and from functions with local __PL_ld defined), so replicating
 * the pl-builtin.h logic here isn't such a hazard.
 */
#  if (defined(O_PLMT) || defined(O_MULTIPLE_ENGINES)) && USE_LD_MACROS
#   define VMI_ARG_DECL PL_local_data_t *__PL_ld, Code PC, struct register_file *registers
#   define VMI_ARG_PASS __PL_ld, PC, registers
#  else
#   define VMI_ARG_DECL Code PC, struct register_file *registers
#   define VMI_ARG_PASS PC, registers
#  endif
# endif
typedef VMI_RETTYPE (*vmi_instr)(VMI_ARG_DECL);
# if VMI_REGISTER_VARIABLES
register struct register_file *__reg_registers asm(REGFILE_REGISTER);
register PL_local_data_t *__reg_ld asm(LD_REGISTER);
# endif
#endif

/* All the registers used in PL_next_solution et al; there will always be a
 * variable or macro called REGISTERS with the current active registers */
typedef struct register_file
{ unify_mode umode;			/* Unification mode */
# define     UMODE	(REGISTERS.umode)
  int        slow_unify;		/* B_UNIFY_FIRSTVAR */
# define     SLOW_UNIFY	(REGISTERS.slow_unify)
  qid_t      qid;			/* External query ID (argument to PL_next_solution()) */
# define     QID	(REGISTERS.qid)
  QueryFrame qf;			/* Query frame */
# define     QF		(REGISTERS.qf)
  LocalFrame fr;			/* current frame */
# define     FR		(REGISTERS.fr)
# define     CL		(FR->clause)	/* clause of current frame */
  LocalFrame nfr;			/* Next frame */
# define     NFR	(REGISTERS.nfr)
  Word       argp;			/* current argument pointer */
# define     ARGP	(REGISTERS.argp)
  Definition def;			/* definition of current procedure */
# define     DEF	(REGISTERS.def)
  fid_t      ffr_id;			/* foreign function id */
# define     FFR_ID	(REGISTERS.ffr_id)
  ar_context pl_ar_ctx;
# define     __PL_ar_ctx (REGISTERS.pl_ar_ctx)
  struct foreign_context fndet_context;	/* foreign function non-deterministic context */
# define     FNDET_CONTEXT (REGISTERS.fndet_context)
#if VMCODE_IS_ADDRESS
  int	     nop1;			/* See SEPARATE_VMI1 */
  int	     nop2;			/* See SEPARATE_VMI2 */
#endif
#ifdef O_DEBUG
  int	     throwed_from_line;		/* Debugging: line we came from */
# define     THROWED_FROM_LINE	(REGISTERS.throwed_from_line)
#endif
#if O_VMI_FUNCTIONS
  int        solution_ret;		/* return value for PL_next_solution, when exit_vm_buf is used */
# define     SOLUTION_RET		(REGISTERS.solution_ret)
  jmp_buf    exit_vm_buf;		/* jump target for exiting PL_next_solution */
# define     EXIT_VM_BUF		(REGISTERS.exit_vm_buf)
#endif
} register_file;


/* Imperative code that gets executed just after entry to an instruction,
 * and just before exit from an instruction (i.e. before goto/return/etc).
 * For profiling/tracing purposes only, and not applied to VMH's.
 */
#define VMI_ENTER(n)		count(n, PC); START_PROF(n, #n);
#define VMI_EXIT		END_PROF();

/* Components of VMI/VMH macro expansion. The underscore-prefix macros
 * get defined per-implementation.
 */

/* If the compiler complains about undeclared __is_vmi or __is_vmh, it means
 * a mismatch of VMI..END_VMH or VMH..END_VMI.
 */
#define assert_exists(var, message) (void)(var)
#define VMI(Name,f,na,a)	_VMI_DECLARATION(Name,f,na,a) \
				{ int __is_vmi = 1; \
				  { _VMI_PROLOGUE(Name,f,na,a); VMI_ENTER(Name)
#define END_VMI			    _VMI_EPILOGUE \
				  } \
				  assert_exists(__is_vmi, "END_VMI used without VMI!"); \
				}
#define VMH(Name,na,at,an)	_VMH_DECLARATION(Name,na,at,an) \
				{ int __is_vmh = 1; \
				  { _VMH_PROLOGUE(Name,na,at,an)
#define END_VMH			    _VMH_EPILOGUE \
				  } \
				  assert_exists(__is_vmh, "END_VMH used without VMH!"); \
				}
#define NEXT_INSTRUCTION	do { VMI_EXIT; _NEXT_INSTRUCTION; } while(0)
#define VMI_GOTO(n)		do { VMI_EXIT; _VMI_GOTO(n); } while(0)
#define VMH_GOTO(...)		do { _VMH_GOTO(__VA_ARGS__); } while(0)
#define SOLUTION_RETURN(val)	do { VMI_EXIT; _SOLUTION_RETURN(val); } while(0)
#define VMI_GOTO_CODE(c)	do { VMI_EXIT; _VMI_GOTO_CODE(c); } while(0)
#define SEPARATE_VMI1		(void)0
#define SEPARATE_VMI2		(void)0

/* By default, instruction and helper prologue/epilogue are empty */
#define _VMI_PROLOGUE(Name,f,na,a)	;
#define _VMI_EPILOGUE			;
#define _VMH_PROLOGUE(Name,na,at,an)	;
#define _VMH_EPILOGUE			;

/* Same syntax as VMH_GOTO, but handles profiling as if it were an instruction */
#define VMH_GOTO_AS_VMI(n,args...) do { VMI_EXIT; \
					VMI_ENTER(n); \
					VMH_GOTO(n, args); \
				      } while(0)


/* Helper macros for rendering VMH arguments */
#define HEAD(h, ...) h
#define TAIL(_, t...) (t)
#define VMH_ARGS0(n,at,an,f,asep...)
#define VMH_ARGS1(n,at,an,f,asep...) f(n, HEAD at, HEAD an)
#define VMH_ARGS2(n,at,an,f,asep...) f(n, HEAD at, HEAD an) asep VMH_ARGS1(n, TAIL at, TAIL an, f, asep)
#define VMH_ARGS3(n,at,an,f,asep...) f(n, HEAD at, HEAD an) asep VMH_ARGS2(n, TAIL at, TAIL an, f, asep)
#define VMH_ARGS4(n,at,an,f,asep...) f(n, HEAD at, HEAD an) asep VMH_ARGS3(n, TAIL at, TAIL an, f, asep)
#define COMMA_TYPE_ARG(n,at,an) , at an
#define TYPE_ARG_SEMI(n,at,an)	at an ;
#define VMH_ARGS(n) A_PASTE(VMH_ARGS, VMH_ARGCOUNT(n))(n, (VMH_ARGTYPES(n)), (VMH_ARGNAMES(n)), TYPE_ARG_SEMI)

/* Define struct types for all the helper argument lists */
#define VMH_ARGSTRUCT(Name)		struct helper_args_ ## Name

FOREACH_VMH(T_EMPTY,
  ,VMH_ARGSTRUCT, { ,VMH_ARGS, };
)

#define ASSIGN_ARG(n,at,an)		at an = HELPER_ARGS(n).an;
#undef _VMH_PROLOGUE
#define _VMH_PROLOGUE(Name,na,at,an)	VMH_ARGS ## na(Name, at, an, ASSIGN_ARG)
#if O_VMI_FUNCTIONS

#define HELPER_ARGS(n)			__args
#define _VMI_DECLARATION(Name,na,at,an)	static VMI_RETTYPE instr_ ## Name(VMI_ARG_DECL)
#define _VMH_DECLARATION(Name,na,at,an)	static VMI_RETTYPE helper_ ## Name(VMI_ARG_DECL, VMH_ARGSTRUCT(Name) __args)
#define _NEXT_INSTRUCTION		return PC
#define _SOLUTION_RETURN(val)		SOLUTION_RET = (val); longjmp(EXIT_VM_BUF, 1)
#define _VMI_GOTO(n)			PC--; return instr_ ## n(VMI_ARG_PASS)
#define _VMH_GOTO(n,...)		VMH_ARGSTRUCT(n) __args = {__VA_ARGS__}; (void)__args; \
					return helper_ ## n(VMI_ARG_PASS, __args)
#undef _VMI_PROLOGUE
#define _VMI_PROLOGUE(Ident,f,na,a)	PC++;
#if VMCODE_IS_ADDRESS
#define VMI_ADDR(c)		((vmi_instr)(c))
#else
#define VMI_ADDR(c)		jmp_table[c]
#endif
#define _VMI_GOTO_CODE(c)	PC--; return VMI_ADDR(c)(VMI_ARG_PASS)

/* Declare prototypes for all VMI/VMH functions */
FOREACH_VMIDECL_CALL(T_SEMICOLON, _VMI_DECLARATION);
FOREACH_VMHDECL_CALL(T_SEMICOLON, _VMH_DECLARATION);

/* Define the jump table with all the function addresses */
static vmi_instr jmp_table[] =
{ FOREACH_VMI(T_COMMA,
    &,VMI_IDENT,
  ),
  NULL
};

/* Define implementations */
#if VMI_USE_REGISTER_VARIABLES
# define REGISTERS (*__reg_registers)
# undef LD
# define LD (__reg_ld)
# define _GET_LD /* empty */
#else
# define REGISTERS (*registers)
#endif
#include "pl-vmi.c"
#undef REGISTERS

/* Redefine NEXT_INSTRUCTION and VMH_GOTO for PL_next_solution() */
#undef NEXT_INSTRUCTION
#undef VMH_GOTO
#define NEXT_INSTRUCTION (void)0
#define VMH_GOTO(n) PC = helper_##n(VMI_ARG_PASS, (VMH_ARGSTRUCT(n)){})

#if VMI_USE_REGISTER_VARIABLES
# undef LD
# define LD LOCAL_LD
#endif

#else /* O_VMI_FUNCTIONS */

#define HELPER_ARGS(n)			helper_args.n
#define _VMH_DECLARATION(Name,na,at,an)	helper_ ## Name:
#define _VMH_GOTO(n,args...)		VMH_ARGSTRUCT(n) __args = {args}; \
					HELPER_ARGS(n) = __args; \
					goto helper_ ## n;
#define _SOLUTION_RETURN		return

#if VMCODE_IS_ADDRESS

#define _VMI_DECLARATION(Name,f,na,a)	Name ## _LBL:
#define _NEXT_INSTRUCTION		DbgPrintInstruction(FR, PC); _VMI_GOTO_CODE(*PC++)
#define _VMI_GOTO(n)			goto n ## _LBL
#define _VMI_GOTO_CODE(c)		goto *(void *)(c)
#undef SEPARATE_VMI1
#undef SEPARATE_VMI2
/* This macro must ensure that two identical VMI instructions do not get
 * merged onto the same address by the compiler, causing decompilation
 * which translates the addresses back into the VMI number to fail.
 * initWamTable() verfies this does not happen.  We have two versions
 * to deal with jumps from to VMI to the same implementation.
 */
#define SEPARATE_VMI1 \
	{ if ( ++REGISTERS.nop1 == 0 ) separate_vmi(REGISTERS.nop1); }
#define SEPARATE_VMI2 \
	{ if ( ++REGISTERS.nop2 == 0 ) separate_vmi(REGISTERS.nop2); }

#else /* VMCODE_IS_ADDRESS */

#if __GNUC__
#define UNUSED_LABEL __attribute__ ((unused))
#else
#define UNUSED_LABEL
#endif

#define _VMI_DECLARATION(Name,f,na,a)	case Name: case_ ## Name: UNUSED_LABEL
#define _NEXT_INSTRUCTION		goto next_instruction
#define _VMI_GOTO(n)			goto case_ ## n
#define _VMI_GOTO_CODE(c)		thiscode = (c); goto resumebreak;

#endif /* VMCODE_IS_ADDRESS */
#endif /* O_VMI_FUNCTIONS */

API_STUB(int)
(PL_next_solution)(qid_t qid)
( return PL_next_solution(qid); )

int
PL_next_solution(DECL_LD qid_t qid)
{ register_file REGISTERS =
  { .qid = qid
#if VMCODE_IS_ADDRESS
    , .nop1 = 0, .nop2 = 0
#endif
  };					/* Active registers */
  Code PC;				/* program counter */
  exception_frame THROW_ENV;		/* PL_thow() environment */

#if O_VMI_FUNCTIONS
  register_file *registers = &REGISTERS;

#else /* O_VMI_FUNCTIONS */
  /* define local union with all "helper arguments" (formerly SHAREDVARS) */
  union
  { FOREACH_VMH(T_EMPTY,
      ,VMH_ARGSTRUCT, ,VMH_NAME,;
    )
  } helper_args;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the labels of the various  virtual-machine instructions in an array.
This is for exploiting GCC's `goto   var' language extension. This array
can only be allocated insite this   function. The initialisation process
calls PL_next_solution() with qid =  QID_EXPORT_WAM_TABLE. This function
will export jmp_table as the compiler  needs   to  know  this table. See
pl-comp.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#if VMCODE_IS_ADDRESS
#undef VMI_IDENT
#define VMI_IDENT(n) n ## _LBL
  static void *jmp_table[] =
  { FOREACH_VMI(T_COMMA,
      &&,VMI_IDENT,
    ),
    NULL
  };
#else /* VMCODE_IS_ADDRESS */
code thiscode;
#endif /* VMCODE_IS_ADDRESS */

#endif /* O_VMI_FUNCTIONS */

#if VMCODE_IS_ADDRESS
  if ( qid == QID_EXPORT_WAM_TABLE )
  { interpreter_jmp_table = (void**) jmp_table;	/* make it globally known */
    succeed;
  }
#endif /* VMCODE_IS_ADDRESS */

  if ( qid == 0 )			/* PL_open_query() failed */
    return FALSE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the real start point  of   this  function.  Simply loads the VMI
registers from the frame filled by   PL_open_query()  and either jump to
depart_continue() to do the normal thing or to the backtrack point.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  QF  = QueryFromQid(qid);
  if ( QF->magic == QID_CMAGIC )
    return FALSE;
  DEBUG(CHK_SECURE, assert(QF->magic == QID_MAGIC));
  if ( true(QF, PL_Q_DETERMINISTIC) )	/* last one succeeded */
  { fid_t fid = QF->foreign_frame;
    QF->foreign_frame = 0;
    PL_close_foreign_frame(fid);
    Undo(QF->choice.mark);
    fail;
  }
  FR  = &QF->frame;
  ARGP = argFrameP(FR, 0);
  DEBUG(9, Sdprintf("QF=%p, FR=%p\n", QF, FR));

#if O_VMI_FUNCTIONS
  if (setjmp(EXIT_VM_BUF) != 0)
  { // LD->vmi_registers = old_registers;
    assert(LD->exception.throw_environment == &THROW_ENV);
    LD->exception.throw_environment = THROW_ENV.parent;
    return SOLUTION_RET;
  }

# if VMI_REGISTER_VARIABLES
  /* Now that we've executed our exit setjmp, we can set register vars with impunity */
  __reg_registers = registers;
  __reg_ld = LD;
# endif
#endif /*O_VMI_FUNCTIONS*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check for exceptions raised by foreign code.  PL_throw() uses longjmp()
to get back here.  Our task is to restore the environment and throw the
Prolog exception.

setjmp()/longjmp clobbers register variables. FR   is  restored from the
environment. BFR is volatile, and qid is an argument. These are the only
variables used in the B_THROW instruction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  DEBUG(9, Sdprintf("Setjmp env at %p\n", &LD->exception.throw_environment));
  THROW_ENV.parent = LD->exception.throw_environment;
  if ( setjmp(THROW_ENV.exception_jmp_env) != 0 )
  { FliFrame ffr;
    GET_LD		/* might be clobbered */

    ffr = fli_context;

    FR = environment_frame;
    DEF = FR->predicate;
    while(ffr && (void *)ffr > (void *)FR) /* discard foreign contexts */
      ffr = ffr->parent;
    fli_context = ffr;

    AR_CLEANUP();

    if ( LD->signal.current )
    { unblockSignal(LD->signal.current);
      LD->signal.current = 0;	/* TBD: saved? */
    }

    THROW_EXCEPTION;
  } else				/* installation */
  { THROW_ENV.magic = THROW_MAGIC;
    LD->exception.throw_environment = &THROW_ENV;
  }

  DEF = FR->predicate;
  if ( QF->solutions || QF->yield.term ) /* retry or resume */
  { fid_t fid = QF->foreign_frame;
    QF->foreign_frame = 0;
    PL_close_foreign_frame(fid);
    if ( QF->yield.term )
    { LOAD_REGISTERS(qid);
      DEBUG(CHK_SECURE, checkStacks(NULL));
      if ( exception_term )
	THROW_EXCEPTION;
      if ( QF->yield.term == -1 )		/* foreign yield */
      { DEF = FR->predicate;
	QF->yield.term = 0;
      }
      NEXT_INSTRUCTION;
    } else
      BODY_FAILED;
  } else
    VMH_GOTO(depart_or_retry_continue);		/* first call */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Main entry of the virtual machine cycle.  A branch to `next instruction'
will  cause  the  next  instruction  to  be  interpreted.   All  machine
registers  should  hold  valid  data  and  the  machine stacks should be
initialised properly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#if O_VMI_FUNCTIONS
  for (;;)
  { DbgPrintInstruction(FR, PC);
#if VMI_REGISTER_VARIABLES && !VMI_USE_REGISTER_VARIABLES
    DEBUG(0,
      assert(__reg_registers == &REGISTERS);
      assert(__reg_ld == LD);
    );
#endif
    PC = VMI_ADDR(*PC)(VMI_ARG_PASS);
  }
#else /* O_VMI_FUNCTIONS */
#if !VMCODE_IS_ADDRESS			/* no goto *ptr; use a switch */
next_instruction:
  DbgPrintInstruction(FR, PC);
  thiscode = *PC++;
#ifdef O_DEBUGGER
resumebreak:
#endif
  switch( thiscode )
#endif
  {
#include "pl-vmi.c"
  }

#endif /* O_VMI_FUNCTIONS */

  assert(0);
  return FALSE;
} /* end of PL_next_solution() */
