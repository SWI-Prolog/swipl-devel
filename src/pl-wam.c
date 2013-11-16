/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-inline.h"
#include "pl-dbref.h"
#include "pl-prof.h"
#ifdef _MSC_VER
#pragma warning(disable: 4102)		/* unreferenced labels */
#endif

#define	     BFR (LD->choicepoints)	/* choicepoint registration */

#if sun
#include <prof.h>			/* in-function profiling */
#else
#define MARK(label)
#endif

static Choice	newChoice(choice_type type, LocalFrame fr ARG_LD);

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
#define loffset(p) loffset__LD(p PASS_LD)
static intptr_t
loffset__LD(void *p ARG_LD)
{ if ( p == NULL )
    return 0;

  assert((intptr_t)p % sizeof(word) == 0);
  return (Word)p-(Word)lBase;
}
#endif

#ifdef O_DEBUG

static void
DbgPrintInstruction(LocalFrame FR, Code PC)
{ GET_LD
  static LocalFrame ofr = NULL;		/* not thread-safe */

  if ( DEBUGGING(MSG_VMI) )
  { if ( ofr != FR )
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




#include "pl-alloc.c"
#include "pl-index.c"


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

  if ( ld->signal.pending )			mask |= ALERT_SIGNAL;
#ifdef O_PROFILE
  if ( ld->profile.active )			mask |= ALERT_PROFILE;
#endif
#ifdef O_PLMT
  if ( ld->exit_requested )			mask |= ALERT_EXITREQ;
#endif
#ifdef O_LIMIT_DEPTH
  if ( ld->depth_info.limit != DEPTH_NO_LIMIT ) mask |= ALERT_DEPTHLIMIT;
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

  ld->alerted = mask;
}


int
raiseSignal(PL_local_data_t *ld, int sig)
{ if ( sig > 0 && sig <= MAXSIGNAL && ld )
  { simpleMutexLock(&ld->signal.sig_lock);
    ld->signal.pending |= ((int64_t)1 << (sig-1));
    simpleMutexUnlock(&ld->signal.sig_lock);
    updateAlerted(ld);
    return TRUE;
  }

  return FALSE;
}


static inline int
is_signalled(ARG1_LD)
{ return unlikely(LD->signal.pending != 0);
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
		 *	 LOCAL ALLOCATION	*
		 *******************************/

/* Note that lTop can be >= lMax when calling ENSURE_LOCAL_SPACE() */

#define ENSURE_LOCAL_SPACE(bytes, ifnot) \
	if ( addPointer(lTop, (bytes)) > (void*)lMax ) \
        { int rc; \
	  SAVE_REGISTERS(qid); \
	  rc = ensureLocalSpace(bytes, ALLOW_SHIFT); \
	  LOAD_REGISTERS(qid); \
	  if ( rc != TRUE ) \
	  { rc = raiseStackOverflow(rc); \
	    ifnot; \
	  } \
	}



		 /*******************************
		 *	    FOREIGN FRAME	*
		 *******************************/

static fid_t
open_foreign_frame(ARG1_LD)
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
PL_close_foreign_frame__LD(fid_t id ARG_LD)
{ FliFrame fr = (FliFrame) valTermRef(id);

  if ( !id || fr->magic != FLI_MAGIC )
    sysError("PL_close_foreign_frame(): illegal frame: %d", id);
  DiscardMark(fr->mark);
  fr->magic = FLI_MAGIC_CLOSED;
  fli_context = fr->parent;
  lTop = (LocalFrame) fr;
}


fid_t
PL_open_foreign_frame__LD(ARG1_LD)
{ size_t lneeded = sizeof(struct fliFrame) + MINFOREIGNSIZE*sizeof(word);

  if ( (char*)lTop + lneeded > (char*)lMax )
  { int rc;

    if ( (rc=ensureLocalSpace(lneeded, ALLOW_SHIFT)) != TRUE )
    { raiseStackOverflow(rc);
      return 0;
    }
  }

  return open_foreign_frame(PASS_LD1);
}


#undef PL_open_foreign_frame
fid_t
PL_open_foreign_frame(void)
{ GET_LD

  return PL_open_foreign_frame__LD(PASS_LD1);
}
				/* This local definition was here before */
#define PL_open_foreign_frame() open_foreign_frame(PASS_LD1)


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

      if ( (rc=ensureLocalSpace(minspace, ALLOW_SHIFT)) != TRUE )
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


#undef PL_close_foreign_frame
void
PL_close_foreign_frame(fid_t id)
{ GET_LD

  PL_close_foreign_frame__LD(id PASS_LD);
}
#define PL_close_foreign_frame(id) PL_close_foreign_frame__LD(id PASS_LD)


void
PL_rewind_foreign_frame(fid_t id)
{ GET_LD
  FliFrame fr = (FliFrame) valTermRef(id);

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

static void
discardForeignFrame(LocalFrame fr ARG_LD)
{ Definition def = fr->predicate;
  int argc       = def->functor->arity;
  Func function  = def->impl.function;
  struct foreign_context context;
  fid_t fid;

  DEBUG(5, Sdprintf("\tCut %s, context = %p\n",
		    predicateName(def), fr->clause));

  context.context = (word)fr->clause;
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
frameFinished() is used for two reasons:   providing hooks for the (GUI)
debugger  for  updating   the   stack-view    and   for   dealing   with
call_cleanup/3.  Both may call-back the Prolog engine.

Note that the cleanup handler is called while protected against signals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline int
isCleanupFrame(LocalFrame fr)
{ return (fr->predicate == PROCEDURE_setup_call_catcher_cleanup4->definition &&
	  false(fr, FR_CATCHED));	/* from handler */
}


static void
callCleanupHandler(LocalFrame fr, enum finished reason ARG_LD)
{ if ( isCleanupFrame(fr) )
  { size_t fref = consTermRef(fr);
    fid_t cid;
    term_t catcher;

    if ( !(cid=PL_open_foreign_frame()) )
      return;

    fr = (LocalFrame)valTermRef(fref);
    catcher = consTermRef(argFrameP(fr, 2));

    set(fr, FR_CATCHED);
    if ( unify_finished(catcher, reason) )
    { term_t clean;
      term_t ex = 0;
      int rval;
      wakeup_state wstate;

      fr = (LocalFrame)valTermRef(fref);
      clean = consTermRef(argFrameP(fr, 3));
      if ( saveWakeup(&wstate, FALSE PASS_LD) )
      { rval = callProlog(contextModule(fr), clean, PL_Q_CATCH_EXCEPTION, &ex);
	restoreWakeup(&wstate PASS_LD);
      } else
      { rval = FALSE;
      }

      if ( !rval && ex && !exception_term )
	PL_raise_exception(ex);
    }

    PL_close_foreign_frame(cid);
  }
}


static void
frameFinished(LocalFrame fr, enum finished reason ARG_LD)
{ if ( isCleanupFrame(fr) )
  { size_t fref = consTermRef(fr);
    callCleanupHandler(fr, reason PASS_LD);
    fr = (LocalFrame)valTermRef(fref);
  }

#ifdef O_DEBUGGER
  callEventHook(PLEV_FRAMEFINISHED, fr);
#endif
}


static int
mustBeCallable(term_t call ARG_LD)
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
protect_var(Word v ARG_LD)
{ term_t t = PL_new_term_ref_noshift();

  if ( t )
    *valTermRef(t) = makeRefL(v);
  else
    assert(0);		/* cannot happen due to MINFOREIGNSIZE */
}


static void
unify_gl(Word g, Word l, int has_firstvar ARG_LD)
{ if ( has_firstvar )
    protect_var(l PASS_LD);

  deRef(l);
  if ( isVar(*l) )
  { setVar(*g);
    *l = makeRefG(g);
    assert(tTop+1 < tMax);
    LTrail(l);
  } else if ( needsRef(*l) )
  { *g = makeRef(l);
  } else
  { *g = *l;
  }
}


static int
put_call_goal(term_t t, Procedure proc ARG_LD)
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
      unify_gl(gp++, ap++, FALSE PASS_LD);
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

static int
put_vm_call(term_t t, term_t frref, Code PC, code op, int has_firstvar,
	    int *pop ARG_LD)
{ atom_t simple_goal;
  functor_t ftor;
  int clean;

  switch(op)
  { case I_CALL:			/* procedure */
    case I_DEPART:
    { return ( put_call_goal(t, (Procedure) PC[1] PASS_LD) &&
	       PL_cons_functor_v(t, FUNCTOR_call1, t) );
    }
    case I_CALLM:			/* module, procedure */
    case I_DEPARTM:
    { Module m = (Module)PC[1];
      term_t av;

      return ( (av = PL_new_term_refs(2)) &&
	       PL_put_atom(av+0, m->name) &&
	       put_call_goal(av+1, (Procedure) PC[2] PASS_LD) &&
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
	       put_call_goal(av+1, (Procedure) PC[3] PASS_LD) &&
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
	       put_call_goal(av+1, (Procedure) PC[3] PASS_LD) &&
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
      unify_gl(&gt[1], v1, has_firstvar PASS_LD);
      gt[2] = (word)PC[2];
      gt[3] = FUNCTOR_call1;
      gt[4] = consPtr(gt, STG_GLOBAL|TAG_COMPOUND);
      *valTermRef(t) = consPtr(&gt[3], STG_GLOBAL|TAG_COMPOUND);

      return TRUE;
    }
    case B_EQ_VV:    clean = 0x0; ftor = FUNCTOR_strict_equal2;     goto fa_2;
    case B_NEQ_VV:   clean = 0x0; ftor = FUNCTOR_not_strict_equal2; goto fa_2;
    case B_UNIFY_FF: clean = 0x3; ftor = FUNCTOR_equals2;	    goto fa_2;
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
      unify_gl(&gt[1], v1, has_firstvar PASS_LD);
      unify_gl(&gt[2], v2, has_firstvar PASS_LD);
      gt[3] = FUNCTOR_call1;
      gt[4] = consPtr(gt, STG_GLOBAL|TAG_COMPOUND);
      *valTermRef(t) = consPtr(&gt[3], STG_GLOBAL|TAG_COMPOUND);

      return TRUE;
    }
    case B_UNIFY_EXIT:
    { if ( debugstatus.debugging )
      { return ( put_call_goal(t, GD->procedures.equals2 PASS_LD) &&
		 PL_cons_functor_v(t, FUNCTOR_call1, t) );
      } else
      { return PL_put_atom_chars(t, "unify_exit");
      }
    }
    case I_VAR:		ftor = FUNCTOR_var1;    goto fa_1;
    case I_NONVAR:	ftor = FUNCTOR_nonvar1; goto fa_1;
    fa_1:
    { Word gt       = allocGlobal(1+1+2);	/* call(f(A)) */
      LocalFrame fr = (LocalFrame)valTermRef(frref);
      Word       v1 = varFrameP(fr, (int)PC[1]);

      if ( !gt )
	return FALSE;

      gt[0] = ftor;
      unify_gl(&gt[1], v1, has_firstvar PASS_LD);
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

      n1 = argvArithStack(2 PASS_LD);
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
    { Number     val = argvArithStack(1 PASS_LD);
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
    { Number     val = argvArithStack(1 PASS_LD);
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
      unify_gl(&gt[1], B, has_firstvar PASS_LD);
      gt[2] = consInt(add);
      gt[3] = FUNCTOR_is2;
      unify_gl(&gt[4], A, has_firstvar PASS_LD);
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
are not marked as used, and GC migh   thus  clean them. We fix that with
protect_var(), which creates a  term-reference   to  the local variable,
such that it is marked from the foreign environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static break_action
callBreakHook(LocalFrame frame, Choice bfr,
	      Code PC, code op, int *pop ARG_LD)
{ predicate_t proc;
  fid_t cid;
  term_t frameref, chref, pcref;
  wakeup_state wstate;
  size_t pc_offset;

  *pop = 0;

  proc = _PL_predicate("break_hook", 6, "prolog",
		       &GD->procedures.prolog_break_hook6);
  if ( !getProcDefinition(proc)->impl.any )
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

  if ( saveWakeup(&wstate, FALSE PASS_LD) )
  { if ( (cid=PL_open_foreign_frame()) )
    { term_t argv = PL_new_term_refs(6);
      Clause clause = frame->clause->value.clause;
      qid_t qid;

      RESTORE_PTRS();
      PL_put_clref(argv+0, clause);
      PL_put_intptr(argv+1, PC - clause->codes);
      PL_put_frame(argv+2, frame);
      PL_put_choice(argv+3, bfr);
      if ( put_vm_call(argv+4, frameref, PC, op, pc_offset != 0, pop PASS_LD) )
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
	      restoreWakeup(&wstate PASS_LD);

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
	      restoreWakeup(&wstate PASS_LD);

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
    restoreWakeup(&wstate PASS_LD);
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
TrailAssignment__LD(Word p ARG_LD)
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


static inline void
__do_undo(mark *m ARG_LD)
{ TrailEntry tt = tTop;
  TrailEntry mt = m->trailtop;

  while(--tt >= mt)
  { Word p = tt->address;

    if ( isTrailVal(p) )
    { DEBUG(2, Sdprintf("Undoing a trailed assignment\n"));
      tt--;
      *tt->address = trailVal(p);
      assert(!(*tt->address & (MARK_MASK|FIRST_MASK)));
    } else
      setVar(*p);
  }

  tTop = mt;
  if ( LD->frozen_bar > m->globaltop )
  { DEBUG(CHK_SECURE, assert(gTop >= LD->frozen_bar));
    gTop = LD->frozen_bar;
  } else
  { gTop = m->globaltop;
  }
}


void
do_undo(mark *m)
{ GET_LD
  __do_undo(m PASS_LD);
}

#undef Undo
#define Undo(m) __do_undo(&m PASS_LD)
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/


		 /*******************************
		 *	    PROCEDURES		*
		 *******************************/

/* Note that we use PL_malloc_uncollectable() here because the pointer in
   our block is not the real memory pointer.
*/

static Definition
localDefinition(Definition def ARG_LD)
{ unsigned int tid = LD->thread.info->pl_tid;
  size_t idx = MSB(tid);
  LocalDefinitions v = def->impl.local;

  if ( !v->blocks[idx] )
  { LOCKDYNDEF(def);
    if ( !v->blocks[idx] )
    { size_t bs = (size_t)1<<idx;
      Definition *newblock;

      if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(Definition))) )
	outOfCore();

      memset(newblock, 0, bs*sizeof(Definition));
      v->blocks[idx] = newblock-bs;
    }
    UNLOCKDYNDEF(def);
  }

  if ( !v->blocks[idx][tid] )
    v->blocks[idx][tid] = localiseDefinition(def);

  return v->blocks[idx][tid];
}


void
destroyLocalDefinition(Definition def, unsigned int tid)
{ size_t idx = MSB(tid);
  LocalDefinitions v = def->impl.local;
  Definition local;

  local = v->blocks[idx][tid];
  v->blocks[idx][tid] = NULL;
  destroyDefinition(local);
}


Definition
getProcDefinition__LD(Definition def ARG_LD)
{
#ifdef O_PLMT
  if ( true(def, P_THREAD_LOCAL) )
  { return localDefinition(def PASS_LD);
  }
#endif

  return def;
}


Definition
getProcDefinitionForThread(Definition def, unsigned int tid)
{ size_t idx = MSB(tid);
  LocalDefinitions v = def->impl.local;

  if ( !v->blocks[idx] )
    return NULL;

  return v->blocks[idx][tid];
}


static inline Definition
getProcDefinedDefinition(Definition def ARG_LD)
{ if ( !def->impl.any && false(def, PROC_DEFINED) )
    def = trapUndefined(def PASS_LD);

#ifdef O_PLMT
  if ( true(def, P_THREAD_LOCAL) )
    return getProcDefinition__LD(def PASS_LD);
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

static inline int
is_qualified(Word p ARG_LD)
{ return hasFunctor(*p, FUNCTOR_colon2);
}


static int
m_qualify_argument(LocalFrame fr, int arg ARG_LD)
{ Word k = varFrameP(fr, arg);
  Word p;

  deRef2(k, p);
  if ( !is_qualified(p PASS_LD) )
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
      *p = makeRefG(&p2[2]);
    } else
    { p2[2] = (needsRef(*p) ? makeRef(p) : *p);
    }
    *k = consPtr(p2, STG_GLOBAL|TAG_COMPOUND);
  } else
  { for(;;)
    { Word p2 = argTermP(*p, 1);
      Word ap;

      deRef2(p2, ap);
      if ( is_qualified(ap PASS_LD) )
      { Word a1 = argTermP(*p, 0);
	deRef(a1);
	if (! isAtom(*a1))
	  break;
	p = ap;
      }
      else
	break;
    }

    *k = *p;
  }

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
foreignWakeup(term_t *ex ARG_LD)
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
	    *ex = PL_exception(qid);
	  else
	    *ex = 0;
	  PL_cut_query(qid);
	}

	PL_close_foreign_frame(fid);

	return rval;
      }

      *ex = exception_term;
      return FALSE;
    }
  }

  return TRUE;
}


		 /*******************************
		 *   FOREIGN-LANGUAGE INTERFACE *
		 *******************************/

#include "pl-fli.c"

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

findCatchExit() can do GC/shift!  The  return   value  is  a local-frame
reference, so we can deal with relocation of the local stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static term_t
findCatcher(LocalFrame fr, Choice ch, term_t ex ARG_LD)
{ Definition catch3  = PROCEDURE_catch3->definition;

  for(; fr; fr = fr->parent)
  { int rc;
    term_t tref;

    if ( fr->predicate != catch3 )
      continue;
    if ( true(fr, FR_CATCHED) )
      continue;				/* thrown from recover */
    if ( (void*)fr > (void*)ch )
      continue;				/* call-port of catch/3 */

    tref = consTermRef(fr);
    rc = PL_unify(consTermRef(argFrameP(fr, 1)), ex);
    fr = (LocalFrame)valTermRef(tref);

    if ( rc )
    { set(fr, FR_CATCHED);
      return consTermRef(fr);
    }
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See whether some outer  environment  will   catch  this  exception. I.e.
catch(Goal, ...), where Goal calls C, calls   Prolog  and then raises an
exception somewhere.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef offset
#define offset(s, f) ((size_t)(&((struct s *)NULL)->f))
#endif

#ifdef O_DEBUGGER
static int
isCaughtInOuterQuery(qid_t qid, term_t ball ARG_LD)
{ Definition catch3 = PROCEDURE_catch3->definition;
  QueryFrame qf = QueryFromQid(qid);

  while( qf && true(qf, PL_Q_PASS_EXCEPTION) )
  { LocalFrame fr = qf->saved_environment;
    term_t ex;

    while( fr )
    { if ( fr->predicate == catch3 )
      { term_t fref = consTermRef(fr);

	if ( can_unify(argFrameP(fr, 1), /* may shift */
		       valTermRef(ball),
		       &ex) )
	  return TRUE;
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

  return FALSE;
}


static word
uncachableException(term_t t ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);
  if ( *p == ATOM_aborted )
    return *p;

  return 0;
}



static inline int
slotsInFrame(LocalFrame fr, Code PC)
{ Definition def = fr->predicate;

  if ( !PC || true(def, P_FOREIGN) || !fr->clause )
    return def->functor->arity;

  return fr->clause->value.clause->prolog_vars;
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

static LocalFrame
dbgRedoFrame(LocalFrame fr, choice_type cht ARG_LD)
{ DEBUG(MSG_TRACE, Sdprintf("REDO on [%d] %s\n",
			    (int)levelFrame(fr), predicateName(fr->predicate)));

  if ( SYSTEM_MODE )
    return fr;				/* system mode; debug everything */
  if ( isDebugFrame(fr) && false(fr->predicate, HIDE_CHILDS) )
    return fr;				/* normal user code */
  for( ; fr && !isDebugFrame(fr); fr = fr->parent)
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

static int
exception_hook(LocalFrame fr, term_t catchfr_ref ARG_LD)
{ if ( PROCEDURE_exception_hook4->definition->impl.clauses.first_clause )
  { if ( !LD->exception.in_hook )
    { wakeup_state wstate;
      qid_t qid;
      term_t av;
      int debug, trace, rc;

      LD->exception.in_hook++;
      if ( !saveWakeup(&wstate, TRUE PASS_LD) )
	return FALSE;

      av = PL_new_term_refs(4);
      PL_put_term(av+0, exception_bin);
      PL_put_frame(av+2, fr);
      if ( catchfr_ref )
      { LocalFrame cfr = (LocalFrame)valTermRef(catchfr_ref);
	cfr = parentFrame(cfr);
	PL_put_frame(av+3, cfr);
      } else
	PL_put_frame(av+3, NULL);	/* puts 'none' */

      qid = PL_open_query(MODULE_user, PL_Q_NODEBUG,
			  PROCEDURE_exception_hook4, av);
      rc = PL_next_solution(qid);
      debug = debugstatus.debugging;
      trace = debugstatus.tracing;
      PL_cut_query(qid);
      if ( rc )				/* pass user setting trace/debug */
      { if ( debug ) debugstatus.debugging = TRUE;
	if ( trace ) debugstatus.tracing = TRUE;
      }

      if ( rc )
	rc = !PL_is_variable(av+1);
      if ( rc )
      {	PL_raise_exception(av+1);	/* copy term again */
	wstate.flags |= WAKEUP_STATE_SKIP_EXCEPTION;
      }
      restoreWakeup(&wstate PASS_LD);

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

static void
copyFrameArguments(LocalFrame from, LocalFrame to, int argc ARG_LD)
{ Word ARGD, ARGS, ARGE;

  if ( argc == 0 )
    return;

  ARGS = argFrameP(from, 0);
  ARGE = ARGS+argc;
  ARGD = argFrameP(to, 0);
  for( ; ARGS < ARGE; ARGS++, ARGD++) /* dereference the block */
  { word k = *ARGS;

    if ( isRefL(k) )
    { Word p = unRefL(k);

      if ( p > (Word)to )
      { if ( isVar(*p) )
	{ *p = makeRefL(ARGD);
	  setVar(*ARGS);
	} else
	  *ARGS = *p;
      }
    }
  }
  ARGS = argFrameP(from, 0);
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
#define GO(label) do { backtrack_from_line = __LINE__; goto label; } while(0)
#else
#define GO(label) goto label
#endif

#define FRAME_FAILED		GO(frame_failed)
#define CLAUSE_FAILED		GO(clause_failed)
#define BODY_FAILED		GO(body_failed)
#ifdef O_DEBUG
#define THROW_EXCEPTION		do { throwed_from_line = __LINE__; \
				     goto b_throw; } while(0)
#else
#define THROW_EXCEPTION		goto b_throw
#endif

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
{ Definition def = fr->predicate;

  fr->clause = NULL;
  leaveDefinition(def);
}


static void
discardFrame(LocalFrame fr ARG_LD)
{ Definition def = fr->predicate;

  DEBUG(2, Sdprintf("discard #%d running %s\n",
		    loffset(fr),
		    predicateName(fr->predicate)));

  if ( true(def, P_FOREIGN) )
  { if ( fr->clause )
    { discardForeignFrame(fr PASS_LD);
      fr->clause = NULL;
    }
  } else
  { fr->clause = NULL;		/* leaveDefinition() may destroy clauses */
    leaveDefinition(def);
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

  Ssprintf(buf, "Choice at #%ld for frame #%ld, type %s",
	   loffset(ch), loffset(ch->frame),
	   ch->type == CHP_JUMP ? "JUMP" :
	   ch->type == CHP_CLAUSE ? "CLAUSE" :
	   ch->type == CHP_TOP ? "TOP" :
	   ch->type == CHP_DEBUG ? "DEBUG" :
	   ch->type == CHP_CATCH ? "CATCH" : "NONE");

  return buf;
}
#endif


static int
existingChoice(Choice ch ARG_LD)
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

static void
discardChoicesAfter(LocalFrame fr, enum finished reason ARG_LD)
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
      { assert(fr2->clause || true(fr2->predicate, P_FOREIGN));

	if ( true(fr2, FR_WATCHED) )
	{ char *lSave = (char*)lBase;

	  if ( !me_undone && is_exception_finish(reason) )
	  { me_undone = TRUE;
	    Undo(me->mark);
	  }
	  BFR = me->parent;
	  frameFinished(fr2, reason PASS_LD);
	  if ( lSave != (char*)lBase )	/* shifted */
	  { intptr_t offset = (char*)lBase - lSave;

	    me  = addPointer(me, offset);
	    me->parent = BFR;		/* not updated because BFR=me->parent */
	    fr  = addPointer(fr, offset);
	    fr2 = addPointer(fr2, offset);
	    delto = addPointer(delto, offset);
	    fr2->parent = addPointer(fr2->parent, offset);
	  }
#if 0					/* What to do if we have multiple */
	  if ( exception_term )		/* handlers and multiple exceptions? */
	    break;
#endif
	}

	discardFrame(fr2 PASS_LD);
      }

      if ( (LocalFrame)me->parent <= fr )
      { if ( !me_undone )
	{ if ( reason == FINISH_EXTERNAL_EXCEPT_UNDO )
	    Undo(me->mark);
	  else
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

static void
dbg_discardChoicesAfter(LocalFrame fr, enum finished reason ARG_LD)
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
    discardChoicesAfter(fr, reason PASS_LD);
    PopVal(w);
    *valTermRef(exception_bin) = w;
    exception_term = exception_bin;
  } else
  { discardChoicesAfter(fr, reason PASS_LD);
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
newChoice(choice_type type, LocalFrame fr ARG_LD)
{ Choice ch = (Choice)lTop;

  DEBUG(CHK_SECURE, assert(ch+1 <= (Choice)lMax));
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

#define NDEBUG_SAVE_FLAGS (PLFLAG_LASTCALL)

qid_t
PL_open_query(Module ctx, int flags, Procedure proc, term_t args)
{ GET_LD
  QueryFrame qf;
  LocalFrame fr, top;
  Definition def;
  int arity;
  Word ap;
  size_t lneeded;
  static int top_initialized = FALSE;
  static struct clause clause;
  static struct clause_ref cref;

  if ( !top_initialized )
  { clause.procedure = PROCEDURE_dc_call_prolog;
    clause.generation.erased = ~(gen_t)0;
    clause.code_size = 1;
    clause.codes[0] = encode(I_EXITQUERY);
    cref.value.clause = &clause;

    top_initialized = TRUE;
  }

  DEBUG(2, { FunctorDef f = proc->definition->functor;
	     unsigned int n;

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
  def = getProcDefinedDefinition(proc->definition PASS_LD);

#ifdef JMPBUF_ALIGNMENT
  lneeded = JMPBUF_ALIGNMENT + sizeof(struct queryFrame)+MAXARITY*sizeof(word);
#else
  lneeded = sizeof(struct queryFrame)+MAXARITY*sizeof(word);
#endif

  if ( (char*)lTop + lneeded > (char*)lMax )
  { int rc;

    if ( (rc=ensureLocalSpace(lneeded, ALLOW_SHIFT)) != TRUE )
    { raiseStackOverflow(rc);
      return (qid_t)0;
    }
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
    top->prof_node = profCall(top->predicate PASS_LD);
  else
    top->prof_node = NULL;
#endif
  if ( environment_frame )
  { setNextFrameFlags(top, environment_frame);
  } else
  { top->flags	     = 0;
    top->level	     = 0;
  }
  fr                 = &qf->frame;
  fr->parent         = top;
  setNextFrameFlags(fr, top);
  set(top, FR_HIDE_CHILDS);
  fr->programPointer = clause.codes;
  arity		     = def->functor->arity;

  DEBUG(CHK_SECURE, checkStacks(NULL));
  assert((uintptr_t)fli_context > (uintptr_t)environment_frame);
  assert((uintptr_t)lTop >= (uintptr_t)(fli_context+1));

  if ( flags == TRUE )			/* compatibility */
    flags = PL_Q_NORMAL;
  else if ( flags == FALSE )
    flags = PL_Q_NODEBUG;
  flags &= 0x1f;			/* mask reserved flags */

  qf->magic		= QID_MAGIC;
  qf->foreign_frame	= 0;
  qf->flags		= flags;
  qf->saved_environment = environment_frame;
  qf->saved_bfr		= LD->choicepoints;
  qf->aSave             = aTop;
  qf->solutions         = 0;
  qf->exception		= 0;
  qf->registers.fr      = NULL;		/* invalid */
  qf->next_environment  = NULL;		/* see D_BREAK */

					/* fill frame arguments */
  ap = argFrameP(fr, 0);
  { int n;
    Word p = valTermRef(args);

    for( n = arity; n-- > 0; p++ )
      *ap++ = linkVal(p);
  }
					/* lTop above the arguments */
  lTop = (LocalFrame)ap;

  DEBUG(3, Sdprintf("Level = %d\n", levelFrame(fr)));
  if ( true(qf, PL_Q_NODEBUG) )
  { set(fr, FR_HIDE_CHILDS);
    suspendTrace(TRUE);
    qf->debugSave = debugstatus.debugging;
    debugstatus.debugging = DBG_OFF;
    qf->flags_saved = (LD->prolog_flag.mask.flags & NDEBUG_SAVE_FLAGS);
    setPrologFlagMask(PLFLAG_LASTCALL);
#ifdef O_LIMIT_DEPTH
    qf->saved_depth_limit   = depth_limit;
    qf->saved_depth_reached = depth_reached;
    depth_limit = DEPTH_NO_LIMIT;
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
  setGenerationFrame(fr, GD->generation);
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
  PL_LOCK(L_STOPTHEWORLD);		/* see restore_after_query() */
  LD->choicepoints  = &qf->choice;
  environment_frame = fr;
  qf->parent = LD->query;
  LD->query = qf;
  PL_UNLOCK(L_STOPTHEWORLD);

  DEBUG(2, Sdprintf("QID=%d\n", QidFromQuery(qf)));
  updateAlerted(LD);

  return QidFromQuery(qf);
}


static void
discard_query(qid_t qid ARG_LD)
{ QueryFrame qf = QueryFromQid(qid);

  discardChoicesAfter(&qf->frame, FINISH_CUT PASS_LD);
  qf = QueryFromQid(qid);		/* may be shifted */
  discardFrame(&qf->frame PASS_LD);
  if ( true(&qf->frame, FR_WATCHED) )
  { lTop = (LocalFrame)argFrameP(&qf->frame,
				 qf->frame.predicate->functor->arity);
    frameFinished(&qf->frame, FINISH_CUT PASS_LD);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Restore the environment. If an exception was raised by the query, and no
new  exception  has  been  thrown,  consider    it  handled.  Note  that
LD->choicepoints must be restored *before*   environment_frame to ensure
async safeness for markAtomsInEnvironments().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
restore_after_query(QueryFrame qf)
{ GET_LD
  if ( qf->exception && !exception_term )
    *valTermRef(exception_printed) = 0;

  DiscardMark(qf->choice.mark);

  PL_LOCK(L_STOPTHEWORLD);	/* see Tests/thread/test_agc_callback.pl */
  LD->query         = qf->parent;
  LD->choicepoints  = qf->saved_bfr;
  environment_frame = qf->saved_environment;
  PL_UNLOCK(L_STOPTHEWORLD);
  aTop		    = qf->aSave;
  lTop		    = qf->saved_ltop;
  if ( true(qf, PL_Q_NODEBUG) )
  { suspendTrace(FALSE);
    debugstatus.debugging = qf->debugSave;
    LD->prolog_flag.mask.flags &= (~NDEBUG_SAVE_FLAGS);
    LD->prolog_flag.mask.flags |= qf->flags_saved;
#ifdef O_LIMIT_DEPTH
    depth_limit   = qf->saved_depth_limit;
    depth_reached = qf->saved_depth_reached;
#endif /*O_LIMIT_DEPTH*/
  }
  updateAlerted(LD);
  DEBUG(CHK_SECURE, checkStacks(NULL));
}


void
PL_cut_query(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);

  DEBUG(CHK_SECURE, assert(qf->magic == QID_MAGIC));
  if ( qf->foreign_frame )
    PL_close_foreign_frame(qf->foreign_frame);

  if ( false(qf, PL_Q_DETERMINISTIC) )
  { discard_query(qid PASS_LD);
    qf = QueryFromQid(qid);
  }

  restore_after_query(qf);
  qf->magic = 0;			/* disqualify the frame */
}


void
PL_close_query(qid_t qid)
{ if ( qid != 0 )
  { GET_LD
    QueryFrame qf = QueryFromQid(qid);

    DEBUG(CHK_SECURE, assert(qf->magic == QID_MAGIC));
    if ( qf->foreign_frame )
      PL_close_foreign_frame(qf->foreign_frame);

    if ( false(qf, PL_Q_DETERMINISTIC) )
    { discard_query(qid PASS_LD);
      qf = QueryFromQid(qid);
    }

    if ( !(qf->exception && true(qf, PL_Q_PASS_EXCEPTION)) )
      Undo(qf->choice.mark);

    restore_after_query(qf);
    qf->magic = 0;			/* disqualify the frame */
  }
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

#ifndef ASM_NOP
int _PL_nop_counter;

#define ASM_NOP _PL_nop_counter++
#endif

typedef enum
{ uread = 0,				/* Unification in read-mode */
  uwrite				/* Unification in write mode */
} unify_mode;

#define IF_WRITE_MODE_GOTO(label) \
	if ( umode == uwrite ) VMI_GOTO(label)

#define TRUST_CLAUSE(cref) \
	umode = uread; \
	CL    = cref; \
	lTop  = (LocalFrame)(ARGP + cref->value.clause->variables); \
	ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION); \
	if ( debugstatus.debugging ) \
	  newChoice(CHP_DEBUG, FR PASS_LD); \
	PC    = cref->value.clause->codes; \
	NEXT_INSTRUCTION;
#define TRY_CLAUSE(cref, cond, altpc) \
	umode = uread; \
	CL    = cref; \
	lTop  = (LocalFrame)(ARGP + cref->value.clause->variables); \
	ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION); \
	if ( cond ) \
	{ Choice ch = newChoice(CHP_JUMP, FR PASS_LD); \
	  ch->value.PC = altpc; \
	} else if ( debugstatus.debugging ) \
	{ newChoice(CHP_DEBUG, FR PASS_LD); \
	} \
	PC    = cref->value.clause->codes; \
	NEXT_INSTRUCTION;


int
PL_next_solution(qid_t qid)
{ GET_LD
  AR_CTX
  QueryFrame QF;			/* Query frame */
  LocalFrame FR;			/* current frame */
  LocalFrame NFR;			/* Next frame */
  Word	     ARGP;			/* current argument pointer */
  Code	     PC = NULL;			/* program counter */
  Definition DEF = NULL;		/* definition of current procedure */
  unify_mode umode = uread;		/* Unification mode */
  exception_frame throw_env;		/* PL_thow() environment */
#ifdef O_DEBUG
  int	     throwed_from_line=0;	/* Debugging: line we came from */
#endif
#define	     CL (FR->clause)		/* clause of current frame */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the labels of the various  virtual-machine instructions in an array.
This is for exploiting GCC's `goto   var' language extension. This array
can only be allocated insite this   function. The initialisation process
calls PL_next_solution() with qid =  QID_EXPORT_WAM_TABLE. This function
will export jmp_table as the compiler  needs   to  know  this table. See
pl-comp.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pentium.h"

#if VMCODE_IS_ADDRESS
#include <pl-jumptable.ic>

#define VMI(Name,f,na,a)	Name ## _LBL: \
				  count(Name, PC); \
				  START_PROF(Name, #Name);
#define VMI_GOTO(n)		do { END_PROF(); \
				       goto n ## _LBL; \
				   } while(0)
#define NEXT_INSTRUCTION	do { END_PROF(); \
				     DbgPrintInstruction(FR, PC); \
				     goto *(void *)((intptr_t)(*PC++)); \
				   } while(0)
#ifndef ASM_NOP
#define ASM_NOP asm("nop")
#endif
#define SEPERATE_VMI ASM_NOP

#else /* VMCODE_IS_ADDRESS */

code thiscode;

#if __GNUC__
#define UNUSED_LABEL __attribute__ ((unused))
#else
#define UNUSED_LABEL
#endif

#define VMI(Name,f,na,a)	case Name: \
				  case_ ## Name: UNUSED_LABEL \
				  count(Name, PC); \
				  START_PROF(Name, #Name);
#define VMI_GOTO(n)		{ END_PROF(); \
				  goto case_ ## n; \
				}
#define NEXT_INSTRUCTION	{ DbgPrintInstruction(FR, PC); \
				  END_PROF(); \
                                  goto next_instruction; \
				}
#define SEPERATE_VMI		(void)0

#endif /* VMCODE_IS_ADDRESS */

#if VMCODE_IS_ADDRESS
  if ( qid == QID_EXPORT_WAM_TABLE )
  { interpreter_jmp_table = jmp_table;	/* make it globally known */
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check for exceptions raised by foreign code.  PL_throw() uses longjmp()
to get back here.  Our task is to restore the environment and throw the
Prolog exception.

setjmp()/longjmp clobbers register variables. FR   is  restored from the
environment. BFR is volatile, and qid is an argument. These are the only
variables used in the B_THROW instruction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  DEBUG(9, Sdprintf("Setjmp env at %p\n", &LD->exception.throw_environment));
  throw_env.parent = LD->exception.throw_environment;
  if ( setjmp(throw_env.exception_jmp_env) != 0 )
  { FliFrame ffr;
#ifdef O_PLMT
    __PL_ld = GLOBAL_LD;		/* might be clobbered */
#endif
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
  { throw_env.magic = THROW_MAGIC;
    LD->exception.throw_environment = &throw_env;
  }

  DEF = FR->predicate;
  if ( QF->solutions )			/* retry */
  { fid_t fid = QF->foreign_frame;
    QF->foreign_frame = 0;
    PL_close_foreign_frame(fid);
    BODY_FAILED;
  } else
    goto retry_continue;		/* first call */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Main entry of the virtual machine cycle.  A branch to `next instruction'
will  cause  the  next  instruction  to  be  interpreted.   All  machine
registers  should  hold  valid  data  and  the  machine stacks should be
initialised properly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !VMCODE_IS_ADDRESS			/* no goto *ptr; use a switch */
next_instruction:
  thiscode = *PC++;
#ifdef O_DEBUGGER
resumebreak:
#endif
  switch( thiscode )
#endif
  {
#include "pl-vmi.c"
  }

#ifdef O_ATTVAR
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Attributed variable handling
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
wakeup:
  DEBUG(1, Sdprintf("Activating wakeup\n"));
  NFR = lTop;
  setNextFrameFlags(NFR, FR);
  SAVE_REGISTERS(qid);
  DEF = GD->procedures.dwakeup1->definition;
  LOAD_REGISTERS(qid);
  ARGP = argFrameP(NFR, 0);
  ARGP[0] = *valTermRef(LD->attvar.head);
  setVar(*valTermRef(LD->attvar.head));
  setVar(*valTermRef(LD->attvar.tail));

  goto normal_call;
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			TRACER RETRY ACTION

By default, retries the  current  frame.  If   another  frame  is  to be
retried, place the frame-reference, which  should   be  a  parent of the
current frame, in debugstatus.retryFrame and jump to this label. This is
implemented by returning retry(Frame) of the prolog_trace_interception/3
hook.

First, the system will leave any parent  frames. Next, it will undo back
to the call-port and finally, restart the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_DEBUGGER
retry:					MARK(RETRY);
{ LocalFrame rframe0, rframe = debugstatus.retryFrame;
  mark m;
  Choice ch;

  if ( !rframe )
    rframe = FR;
  debugstatus.retryFrame = NULL;
  rframe0 = rframe;

  m.trailtop = tTop;
  m.globaltop = gTop;
  for( ; rframe; rframe = rframe->parent )
  { if ( (ch = findStartChoice(rframe, BFR)) )
    { m = ch->mark;
      goto do_retry;
    }
  }
  Sdprintf("[Could not find retry-point]\n");
  SAVE_REGISTERS(qid);
  abortProlog();				/* What else? */
  LOAD_REGISTERS(qid);
  THROW_EXCEPTION;

do_retry:
  if ( rframe0 != rframe )
    Sdprintf("[No retry-information for requested frame]\n");

  Sdprintf("[Retrying frame %d running %s]\n",
	   (Word)rframe - (Word)lBase,
	   predicateName(rframe->predicate));

  discardChoicesAfter(rframe, FINISH_CUT PASS_LD);
  rframe->clause = NULL;
  environment_frame = FR = rframe;
  DEF = FR->predicate;
  clear(FR, FR_SKIPPED);
  Undo(m);
  exception_term = 0;

  goto retry_continue;
}
#endif /*O_DEBUGGER*/

		 /*******************************
		 *	   BACKTRACKING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The rest of this giant procedure handles   backtracking. This used to be
very complicated, but as of pl-3.3.6, choice-points are explicit objects
and life is a lot easier. In the old days we distinquished between three
cases to get here. We leave that   it for documentation purposes as well
as to investigate optimization in the future.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

				MARK(BKTRK);
clause_failed:				/* shallow backtracking */
body_failed:
END_PROF();
START_PROF(P_SHALLOW_BACKTRACK, "P_SHALLOW_BACKTRACK");
{ Choice ch = BFR;

  if ( FR == ch->frame )
  { Undo(ch->mark);
    QF = QueryFromQid(qid);
    aTop = QF->aSave;

    if ( ch->type == CHP_JUMP )
    { DiscardMark(ch->mark);
      PC   = ch->value.PC;
      BFR  = ch->parent;
      lTop = (LocalFrame)ch;
      ARGP = argFrameP(lTop, 0);

      NEXT_INSTRUCTION;
    } else if ( ch->type == CHP_CLAUSE )
    { ARGP = argFrameP(FR, 0);
      if ( !(CL = nextClause(&ch->value.clause, ARGP, FR, DEF)) )
	FRAME_FAILED;		/* can happen if scan-ahead was too short */
      PC = CL->value.clause->codes;
      umode = uread;

      if ( ch == (Choice)argFrameP(FR, CL->value.clause->variables) )
      { DiscardMark(ch->mark);		/* is this needed? */
	if ( ch->value.clause.cref )
	{ Mark(ch->mark);
	  lTop = (LocalFrame)(ch+1);
	  NEXT_INSTRUCTION;
	} else if ( unlikely(debugstatus.debugging) )
	{ ch->type = CHP_DEBUG;
	  Mark(ch->mark);
	  lTop = (LocalFrame)(ch+1);
	  NEXT_INSTRUCTION;
	}

	BFR = ch->parent;
	lTop = (LocalFrame)ch;
	NEXT_INSTRUCTION;
      } else				/* Choice point needs to move */
      { struct clause_choice chp;

        DiscardMark(ch->mark);
	BFR = ch->parent;
	chp = ch->value.clause;
	lTop = (LocalFrame)argFrameP(FR, CL->value.clause->variables);
	ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION);

	if ( chp.cref )
	{ ch = newChoice(CHP_CLAUSE, FR PASS_LD);
	  ch->value.clause = chp;
	} else if ( unlikely(debugstatus.debugging) )
	{ ch = newChoice(CHP_DEBUG, FR PASS_LD);
	}
	NEXT_INSTRUCTION;
      }
    }
  }
}


frame_failed:
END_PROF();
START_PROF(P_DEEP_BACKTRACK, "P_DEEP_BACKTRACK");
{
#ifdef O_DEBUGGER
  Choice ch0 = BFR;
#endif
  Choice ch;

  DEBUG(3, Sdprintf("BACKTRACKING\n"));

next_choice:
  ch = BFR;
					/* leave older frames */
  for(; (void *)FR > (void *)ch; FR = FR->parent)
  {
#ifdef O_DEBUGGER
    if ( debugstatus.debugging && isDebugFrame(FR) )
    { Choice sch = findStartChoice(FR, ch0);

      DEBUG(1, Sdprintf("FAIL on %s\n", predicateName(FR->predicate)));

      if ( sch )
      { int rc;

	Undo(sch->mark);
	environment_frame = FR;
	FR->clause = NULL;
	lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);
	SAVE_REGISTERS(qid);
	rc = tracePort(FR, BFR, FAIL_PORT, NULL PASS_LD);
	LOAD_REGISTERS(qid);
	ch = BFR;			/* can be shifted */

	switch( rc )
	{ case ACTION_RETRY:
	    environment_frame = FR;
	    DEF = FR->predicate;
	    clear(FR, FR_CATCHED|FR_SKIPPED);
	    goto retry_continue;
	    case ACTION_ABORT:
	      THROW_EXCEPTION;
	}
      } else
      { DEBUG(2, Sdprintf("Cannot trace FAIL [%d] %s\n",
			  levelFrame(FR), predicateName(FR->predicate)));
      }
    }
#endif

    leaveFrame(FR);
    if ( true(FR, FR_WATCHED) )
    { environment_frame = FR;
      lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);
      FR->clause = NULL;
      SAVE_REGISTERS(qid);
      frameFinished(FR, FINISH_FAIL PASS_LD);
      LOAD_REGISTERS(qid);
      ch = BFR;			/* can be shifted */
      if ( exception_term )
	THROW_EXCEPTION;
    }
  }

  environment_frame = FR = ch->frame;
  Undo(ch->mark);
  QF = QueryFromQid(qid);
  aTop = QF->aSave;
  DEF  = FR->predicate;
#ifdef O_DEBUG_BACKTRACK
  last_choice = ch->type;
#endif

  switch(ch->type)
  { case CHP_JUMP:
      DEBUG(3, Sdprintf("    REDO #%ld: Jump in %s\n",
			loffset(FR),
			predicateName(DEF)));
#ifdef O_DEBUGGER
      if ( debugstatus.debugging && !debugstatus.suspendTrace  )
      { LocalFrame fr = dbgRedoFrame(FR, CHP_JUMP PASS_LD);

	if ( fr )
	{ int action;

	  SAVE_REGISTERS(qid);
	  action = tracePort(fr, BFR, REDO_PORT, ch->value.PC PASS_LD);
	  LOAD_REGISTERS(qid);
	  ch = BFR;			/* can be shifted */

	  switch( action )
	  { case ACTION_FAIL:
	      FRAME_FAILED;
	    case ACTION_IGNORE:
	      VMI_GOTO(I_EXIT);
	    case ACTION_RETRY:
	      goto retry_continue;
	    case ACTION_ABORT:
	      THROW_EXCEPTION;
	  }
	}
      }
#endif
      PC   = ch->value.PC;
      DiscardMark(ch->mark);
      BFR  = ch->parent;
      Profile(profRedo(ch->prof_node PASS_LD));
      lTop = (LocalFrame)ch;
      ARGP = argFrameP(lTop, 0);
      NEXT_INSTRUCTION;
    case CHP_CLAUSE:			/* try next clause */
    { Clause clause;
      struct clause_choice chp;

      DEBUG(3, Sdprintf("    REDO #%ld: Clause in %s\n",
			loffset(FR),
			predicateName(DEF)));
      ARGP = argFrameP(FR, 0);
      DiscardMark(ch->mark);
      BFR = ch->parent;
      if ( !(CL = nextClause(&ch->value.clause, ARGP, FR, DEF)) )
	goto next_choice;	/* Can happen of look-ahead was too short */
      chp = ch->value.clause;

#ifdef O_DEBUGGER
      if ( debugstatus.debugging && !debugstatus.suspendTrace  )
      { LocalFrame fr = dbgRedoFrame(FR, CHP_CLAUSE PASS_LD);

	if ( fr )
	{ int action;

	  SAVE_REGISTERS(qid);
	  action = tracePort(fr, BFR, REDO_PORT, NULL PASS_LD);
	  LOAD_REGISTERS(qid);
	  ch = BFR;			/* can be shifted */

	  switch( action )
	  { case ACTION_FAIL:
	      FRAME_FAILED;
	    case ACTION_IGNORE:
	      VMI_GOTO(I_EXIT);
	    case ACTION_RETRY:
	      goto retry_continue;
	    case ACTION_ABORT:
	      THROW_EXCEPTION;
	  }
	}
      }
#endif

      umode  = uread;
      clause = CL->value.clause;
      PC     = clause->codes;
      Profile(profRedo(ch->prof_node PASS_LD));
      lTop   = (LocalFrame)argFrameP(FR, clause->variables);
      ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION);

      if ( chp.cref )
      { ch = newChoice(CHP_CLAUSE, FR PASS_LD);
	ch->value.clause = chp;
      } else if ( unlikely(debugstatus.debugging) )
      { newChoice(CHP_DEBUG, FR PASS_LD);
      }

      if ( is_signalled(PASS_LD1) )
      { SAVE_REGISTERS(qid);
	handleSignals(PASS_LD1);
	LOAD_REGISTERS(qid);
	if ( exception_term )
	  THROW_EXCEPTION;
      }

			/* require space for the args of the next frame */
      ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION);
      NEXT_INSTRUCTION;
    }
    case CHP_TOP:			/* Query toplevel */
    { DiscardMark(ch->mark);
      Profile(profRedo(ch->prof_node PASS_LD));
      QF = QueryFromQid(qid);
      set(QF, PL_Q_DETERMINISTIC);
      QF->foreign_frame = PL_open_foreign_frame();
      assert(LD->exception.throw_environment == &throw_env);
      LD->exception.throw_environment = throw_env.parent;
      fail;
    }
    case CHP_CATCH:			/* catch/3 & setup_call_cleanup/3 */
      if ( true(ch->frame, FR_WATCHED) )
      { DiscardMark(ch->mark);
	environment_frame = FR = ch->frame;
	lTop = (LocalFrame)(ch+1);
	FR->clause = NULL;
	if ( isCleanupFrame(ch->frame) )
	{ SAVE_REGISTERS(qid);
	  callCleanupHandler(ch->frame, FINISH_FAIL PASS_LD);
	  LOAD_REGISTERS(qid);
	} else
	{ set(ch->frame, FR_CATCHED);
	}
	ch = BFR;			/* can be shifted */
	if ( exception_term )
	  THROW_EXCEPTION;
      } else
      { set(ch->frame, FR_CATCHED);
      }
      /*FALLTHROUGH*/
    case CHP_DEBUG:			/* Just for debugging purposes */
#ifdef O_DEBUGGER
      ch0 = ch;
#endif
      BFR = ch->parent;
      DiscardMark(ch->mark);
      goto next_choice;
  }
}
  assert(0);
  return FALSE;
} /* end of PL_next_solution() */
