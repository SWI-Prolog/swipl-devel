/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

/*#define O_SECURE 1*/
/*#define O_DEBUG 1*/
#include "pl-incl.h"
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
    case CA1_CHP:
    { int v = (int)*PC;
      
      v -= ARGOFFSET/sizeof(word);
      assert(v>=0);
      if ( v >= MAXVAR )
	v = MAXVAR-1;

      if ( !counting[c].vartimesptr )
      { int bytes = sizeof(int)*MAXVAR;

	counting[c].vartimesptr = allocHeap(bytes);
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
static inline intptr_t
loffset(void *p)
{ if ( p == NULL )
    return 0;

  assert((intptr_t)p % sizeof(word) == 0);
  return (Word)p-(Word)lBase;
}
#endif

#ifdef O_DEBUG

static void
DbgPrintInstruction(LocalFrame FR, Code PC)
{ static LocalFrame ofr = NULL;

  DEBUG(3,
	if ( ofr != FR )
	{ Sfprintf(Serror, "#%ld at [%ld] predicate %s\n",
		   loffset(FR),
		   levelFrame(FR),
		   predicateName(FR->predicate));
	  ofr = FR;
	});

  DEBUG(3,
	{ Code relto = NULL;

	  if ( FR->predicate->codes )
	  { int offset = PC - FR->predicate->codes;
	    
	    if ( offset >= 0 && offset < (int)FR->predicate->codes[-1] )
	      relto = FR->predicate->codes;
	  } else if ( FR->clause )
	  { relto = FR->clause->clause->codes;
	  }

	  Sdprintf("\t%s\n", codeTable[decode(*PC)].name);
	});
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

  if ( ld->pending_signals )			mask |= ALERT_SIGNAL;
  if ( ld->outofstack )				mask |= ALERT_OUTOFSTACK;
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
  { ld->pending_signals |= ((int64_t)1 << (sig-1));
    updateAlerted(ld);
    return TRUE;
  }

  return FALSE;
}


static inline int
is_signalled(ARG1_LD)
{
#ifdef O_PLMT
  if ( LD->exit_requested )
    pthread_testcancel();
#endif

  return (LD->pending_signals != 0);
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

#undef LD
#define LD LOCAL_LD

static fid_t
open_foreign_frame(ARG1_LD)
{ FliFrame fr = (FliFrame) lTop;

  requireStack(local, sizeof(struct fliFrame));
  lTop = addPointer(lTop, sizeof(struct fliFrame));
  fr->size = 0;
  Mark(fr->mark);
  SECURE(assert(fr>fli_context));
  fr->parent = fli_context;
  fr->magic = FLI_MAGIC;
  fli_context = fr;

  return consTermRef(fr);
}


static void
close_foreign_frame(fid_t id ARG_LD)
{ FliFrame fr = (FliFrame) valTermRef(id);

  assert(fr->magic == FLI_MAGIC);
  fr->magic = FLI_MAGIC_CLOSED;
  fli_context = fr->parent;
  lTop = (LocalFrame) fr;
}


fid_t
PL_open_foreign_frame()
{ GET_LD

  return open_foreign_frame(PASS_LD1);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open a foreign frame to handle a signal.  We must skip MAXARITY words to
deal with the fact that the WAM write-mode   writes above the top of the
stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fid_t
PL_open_signal_foreign_frame()
{ GET_LD
  FliFrame fr;
  size_t margin = sizeof(struct localFrame) + MAXARITY*sizeof(word);

  requireStack(local, sizeof(struct fliFrame)+margin);
  lTop = addPointer(lTop, margin);
  fr = (FliFrame) lTop;

  fr->magic = FLI_MAGIC;
  fr->size = 0;
  Mark(fr->mark);
  fr->parent = fli_context;
  lTop = (LocalFrame)(fr+1);
  fli_context = fr;

  return consTermRef(fr);
}


void
PL_close_foreign_frame(fid_t id)
{ GET_LD
  
  close_foreign_frame(id PASS_LD);
}

#define PL_open_foreign_frame()    open_foreign_frame(PASS_LD1)
#define PL_close_foreign_frame(id) close_foreign_frame((id) PASS_LD)

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
  Func function  = def->definition.function;
  struct foreign_context context;
  fid_t fid;

  DEBUG(5, Sdprintf("\tCut %s, context = 0x%lx\n",
		    predicateName(def), context));

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


enum finished
{ FINISH_EXIT = 0,
  FINISH_FAIL,
  FINISH_CUT,
  FINISH_EXCEPT,
  FINISH_EXITCLEANUP
};


static int
unify_finished(term_t catcher, enum finished reason)
{ GET_LD

  static atom_t reasons[] = 
  { ATOM_exit,
    ATOM_fail,
    ATOM_cut,
    ATOM_exception,
    ATOM_exit
  };

  if ( reason == FINISH_EXCEPT )
  { SECURE(checkData(valTermRef(exception_bin)));

    return PL_unify_term(catcher,
			 PL_FUNCTOR, FUNCTOR_exception1,
			   PL_TERM, exception_bin);
  } else if ( reason == FINISH_EXIT )
  { fail;
  } else
  { return PL_unify_atom(catcher, reasons[reason]);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
frameFinished() is used for two reasons:   providing hooks for the (GUI)
debugger  for  updating   the   stack-view    and   for   dealing   with
call_cleanup/3. Both may call-back the Prolog engine, but in general the
system is not in a state where we can do garbage collection.

As a consequence the cleanup-handler  of   call_cleanup()  runs  with GC
disables and so do the callEventHook()  hooks.   The  latter is merely a
developers issue. Cleanup seems reasonable too.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
callCleanupHandler(LocalFrame fr, enum finished reason ARG_LD)
{ if ( fr->predicate == PROCEDURE_setup_and_call_cleanup4->definition &&
       false(fr, FR_CATCHED) )		/* from handler */
  { fid_t cid  = PL_open_foreign_frame();
    term_t catcher = argFrameP(fr, 2) - (Word)lBase;

    set(fr, FR_CATCHED);
    if ( unify_finished(catcher, reason) )
    { term_t clean = argFrameP(fr, 3) - (Word)lBase;
      term_t ex;
      int rval;
      
      blockGC(PASS_LD1);
      if ( reason == FINISH_EXCEPT )
      {	term_t pending = PL_new_term_ref();

	*valTermRef(pending) = *valTermRef(exception_bin);

	exception_term = 0;
	*valTermRef(exception_bin) = 0;
	rval = callProlog(contextModule(fr), clean, PL_Q_CATCH_EXCEPTION, &ex);
	if ( rval || !ex )
	{ *valTermRef(exception_bin) = *valTermRef(pending);
	  exception_term = exception_bin;
	}
      } else
      { rval = callProlog(contextModule(fr), clean, PL_Q_CATCH_EXCEPTION, &ex);
      }
      unblockGC(PASS_LD1);

      if ( !rval && ex )
	PL_raise_exception(ex);
    }
    
    PL_close_foreign_frame(cid);
  }
}


static void
frameFinished(LocalFrame fr, enum finished reason ARG_LD)
{ callCleanupHandler(fr, reason PASS_LD);

#ifdef O_DEBUGGER
  callEventHook(PLEV_FRAMEFINISHED, fr);
#endif
}

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

Note this function doesn't call Trail() for   the address as it can only
be called from setarg/3 and the argument  is thus always a term-argument
on the global stack.

(*) Enabling this test triggers an asserion error in unifiable/3. In any
case, we need tighter assignment of  LD->mark_bar as foreign frames that
surround each foreign predicate currently creates a mark.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
TrailAssignment__LD(Word p ARG_LD)
{ /*if ( p < LD->mark_bar )  see (*) */
  { Word old = allocGlobal(1);

    assert(!(*p & (MARK_MASK|FIRST_MASK)));
    *old = *p;				/* save the old value on the global */
    requireStack(trail, 2*sizeof(struct trail_entry));
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
  { SECURE(assert(gTop >= LD->frozen_bar));
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

static inline Definition
pl__getProcDefinition(Procedure proc ARG_LD)
{
#ifdef O_PLMT
  Definition def = proc->definition;

  if ( true(def, P_THREAD_LOCAL) )
  { int i = LD->thread.info->pl_tid;
    Definition local;

    LOCKDEF(def);
    if ( !def->definition.local ||
	 i >= def->definition.local->size ||
	 !(local=def->definition.local->thread[i]) )
      local = localiseDefinition(def);
    UNLOCKDEF(def);

    return local;
  }

  return def;
#else
  return proc->definition;
#endif
}


Definition
getProcDefinition(Procedure proc)
{ GET_LD

  return pl__getProcDefinition(proc PASS_LD);
}

#define getProcDefinition(proc) pl__getProcDefinition(proc PASS_LD)


static inline Definition
getProcDefinedDefinition(LocalFrame *frp, Code PC, Procedure proc ARG_LD)
{ Definition def = proc->definition;

  if ( !def->definition.clauses && false(def, PROC_DEFINED) )
    def = trapUndefined(frp, PC, proc PASS_LD);

#ifdef O_PLMT
  if ( true(def, P_THREAD_LOCAL) )
  { int i = LD->thread.info->pl_tid;
    Definition local;

    LOCKDEF(def);
    if ( !def->definition.local ||
	 i >= def->definition.local->size ||
	 !(local=def->definition.local->thread[i]) )
      local = localiseDefinition(def);
    UNLOCKDEF(def);

    return local;
  }

  return def;
#else
  return def;
#endif
}


Module
contextModule(LocalFrame fr)
{ for(; fr; fr = fr->parent)
  { if ( true(fr, FR_CONTEXT) )
      return fr->context;
    if ( false(fr->predicate, METAPRED) )
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
foreignWakeup() calls delayed goals while executing a foreign procedure.
Note that the  choicepoints  of  the   awoken  code  are  destroyed  and
therefore this code can only be used in places introducing an (implicit)
cut such as \=/2 (implemented as A \= B :- ( A = B -> fail ; true )).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
foreignWakeup(ARG1_LD)
{ if ( LD->alerted & ALERT_WAKEUP )
  { LD->alerted &= ~ALERT_WAKEUP;

    if ( *valTermRef(LD->attvar.head) )
    { fid_t fid = PL_open_foreign_frame();
      int rval;
      term_t a0 = PL_new_term_ref();
  
      PL_put_term(a0, LD->attvar.head);
      setVar(*valTermRef(LD->attvar.head));
      setVar(*valTermRef(LD->attvar.tail));
  
      rval = PL_call_predicate(NULL, PL_Q_NORMAL, PROCEDURE_dwakeup1,
			       a0);
  
      PL_close_foreign_frame(fid);
  
      return rval;
    }
  }

  succeed;
}


		 /*******************************
		 *   FOREIGN-LANGUAGE INTERFACE *
		 *******************************/

#include "pl-fli.c"

#if O_BLOCK
		/********************************
		*         BLOCK SUPPORT         *
		*********************************/

static LocalFrame
findBlock(LocalFrame fr, Word block)
{ GET_LD
  for(; fr; fr = fr->parent)
  { if ( fr->predicate == PROCEDURE_block3->definition &&
	 unify_ptrs(argFrameP(fr, 0), block PASS_LD) )
      return fr;
  }

  PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_block, wordToTermRef(block));

  return NULL;
}

#endif /*O_BLOCK*/

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
    Clause cl = catch3->definition.clauses->clause;
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static LocalFrame
findCatcher(LocalFrame fr, Word ex ARG_LD)
{ Definition catch3  = PROCEDURE_catch3->definition;

  for(; fr; fr = fr->parent)
  { if ( fr->predicate != catch3 )
      continue;
    if ( true(fr, FR_CATCHED) )
      continue;
    if ( unify_ptrs(argFrameP(fr, 1), ex PASS_LD) )
    { set(fr, FR_CATCHED);
      return fr;
    }
  }

  return NULL;
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
isCatchedInOuterQuery(QueryFrame qf, Word catcher)
{ Definition catch3 = PROCEDURE_catch3->definition;

  while( qf && true(qf, PL_Q_PASS_EXCEPTION) )
  { LocalFrame fr = qf->saved_environment;

    while( fr )
    { if ( fr->predicate == catch3 && can_unify(argFrameP(fr, 1), catcher) )
	succeed;

      if ( fr->parent )
      { fr = fr->parent;
      } else
      { qf = queryOfFrame(fr);
	break;
      }
    }

  }

  fail;
}


static inline int
slotsInFrame(LocalFrame fr, Code PC)
{ Definition def = fr->predicate;

  if ( !PC || true(def, FOREIGN) || !fr->clause )
    return def->functor->arity;

  return fr->clause->clause->prolog_vars;
}


static void
updateMovedTerm(LocalFrame fr, word old, word new)
{ Code pc = NULL;

  for(; fr; fr=fr->parent)
  { int slots = slotsInFrame(fr, pc);
    Word p = argFrameP(fr, 0);
    
    for(; slots-- > 0; p++)
    { if ( *p == old )
	*p = new;
    }
  }
}


#endif /*O_DEBUGGER*/

static int
exception_hook(LocalFrame fr, LocalFrame catcher ARG_LD)
{ if ( PROCEDURE_exception_hook4->definition->definition.clauses )
  { if ( !LD->exception.in_hook )
    { fid_t fid, wake;
      qid_t qid;
      term_t av;
      int debug, trace, rc;
  
      LD->exception.in_hook++;
      blockGC(PASS_LD1);
      wake = saveWakeup(PASS_LD1);
      fid = PL_open_foreign_frame();
      av = PL_new_term_refs(4);
  
      PL_put_term(av+0, exception_bin);
      PL_put_frame(av+2, fr);
      if ( catcher )
	catcher = parentFrame(catcher);
      PL_put_frame(av+3, catcher);
  
      exception_term = 0;
      setVar(*valTermRef(exception_bin));
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

      PL_put_term(exception_bin, rc ? av+1 : av+0);
      exception_term = exception_bin;
      
      PL_close_foreign_frame(fid);
      restoreWakeup(wake PASS_LD);
      unblockGC(PASS_LD1);
      LD->exception.in_hook--;

      return rc;
    } else
    { PL_warning("Recursive exception in prolog_exception_hook/4");
    }
  }

  return FALSE;
}


#endif /*O_CATCHTHROW*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
isSimpleGoal(Word g)
    Determines whether we need to compile a call (as call/1) to the
    specified term (see I_USERCALL0) or we can call it directly.  The
    choice is based on optimisation.  Compilation is slower, but almost
    required to deal with really complicated cases.
 
    TBD: use CONTROL_F
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
isSimpleGoal(Word a ARG_LD)		/* a is dereferenced and compound */
{ functor_t f = functorTerm(*a);

  if ( f == FUNCTOR_comma2 ||
       f == FUNCTOR_semicolon2 ||
       f == FUNCTOR_bar2 )
    fail;

  succeed;
}

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
#define GO(label) do { backtrack_from_line = __LINE__; goto label; } while(0);
#else
#define GO(label) goto label
#endif

#define FRAME_FAILED		GO(frame_failed)
#define CLAUSE_FAILED		GO(clause_failed)
#define BODY_FAILED		GO(body_failed)

#ifdef O_PROFILE
#define Profile(g) if ( LD->profile.active ) g
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
leaveFrame(LocalFrame fr ARG_LD)
{ Definition def = fr->predicate;

  fr->clause = NULL;
  leaveDefinition(def);

  if ( true(fr, FR_WATCHED) )
    frameFinished(fr, FINISH_FAIL PASS_LD);
}


static void
discardFrame(LocalFrame fr, enum finished reason ARG_LD)
{ Definition def = fr->predicate;

  DEBUG(2, Sdprintf("discard #%d running %s\n",
		    loffset(fr),
		    predicateName(fr->predicate)));

  if ( true(def, FOREIGN) )
  { if ( fr->clause )
    { discardForeignFrame(fr PASS_LD);
      fr->clause = NULL;
    }
  } else
  { fr->clause = NULL;		/* leaveDefinition() may destroy clauses */
    leaveDefinition(def);
  }

  if ( true(fr, FR_WATCHED) )
    frameFinished(fr, reason PASS_LD);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Discard all choice-points created after  the   creation  of the argument
environment. See also discardFrame().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(O_DEBUG) || defined(SECURE_GC) || defined(O_MAINTENANCE)
char *
chp_chars(Choice ch)
{ static char buf[256];

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


static void
discardChoicesAfter(LocalFrame fr ARG_LD)
{ for(; BFR && (LocalFrame)BFR > fr; BFR = BFR->parent)
  { LocalFrame fr2;

    DEBUG(3, Sdprintf("Discarding %s\n", chp_chars(BFR)));
    for(fr2 = BFR->frame;    
	fr2 && fr2->clause && fr2 > fr;
	fr2 = fr2->parent)
    { discardFrame(fr2, FINISH_CUT PASS_LD);
      if ( exception_term )
	break;
    }
  }

  DEBUG(3, Sdprintf(" --> BFR = #%ld\n", loffset(BFR)));
  LD->mark_bar = BFR->mark.globaltop;
} 


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Discard choicepoints in debugging mode.  As we might be doing callbacks
on behalf of the debugger we need to preserve the pending exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
dbg_discardChoicesAfter(LocalFrame fr ARG_LD)
{ blockGC(PASS_LD1);

  if ( exception_term )
  { Word p = valTermRef(exception_term);
    DEBUG(3, Sdprintf("dbg_discardChoicesAfter(): saving exception: ");
	     pl_writeln(exception_term));
    exception_term = 0;
    discardChoicesAfter(fr PASS_LD);
    *valTermRef(exception_bin) = *p;
    exception_term = exception_bin;
  } else
    discardChoicesAfter(fr PASS_LD);

  unblockGC(PASS_LD1);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
newChoice(CH_*, FR) Creates a new  choicepoint.   After  creation of the
choice-point, the user has to fill the choice-points mark as well as the
required context value.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Choice
newChoice(choice_type type, LocalFrame fr ARG_LD)
{ Choice ch = (Choice)lTop;

  requireStack(local, sizeof(*ch));
  lTop = addPointer(lTop, sizeof(*ch));
  ch->type = type;
  ch->frame = fr;
  ch->parent = BFR;
  Mark(ch->mark);
  BFR = ch;
#ifdef O_PROFILE
  ch->prof_node = LD->profile.current;
#endif
  DEBUG(3, Sdprintf("NEW %s\n", chp_chars(ch)));

  return ch;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Op top of the query frame there are two   local frames. The top one is a
dummy one, just enough to satisfy stack-walking   and GC. The first real
one has a programPointer pointing to  I_EXITQUERY, doing the return from
a query.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

qid_t
PL_open_query(Module ctx, int flags, Procedure proc, term_t args)
{ GET_LD
  QueryFrame qf;
  LocalFrame fr, top;
  Definition def;
  int arity;
  Word ap;
  static int top_initialized = FALSE;
  static struct clause clause;
  static struct clause_ref cref = {&clause};

  if ( !top_initialized )
  { clause.procedure = PROCEDURE_dc_call_prolog;
    clause.generation.erased = ~0L;
    clause.code_size = 1;
    clause.codes[0] = encode(I_EXITQUERY);

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
  SECURE(checkStacks(environment_frame, NULL));
  assert((void*)fli_context > (void*)environment_frame);
  assert((void*)lTop >= (void*)(fli_context+1));


					/* should be struct alignment, */
					/* but for now, I think this */
					/* is always the same */
#ifdef JMPBUF_ALIGNMENT
  while ( (uintptr_t)lTop % JMPBUF_ALIGNMENT )
    lTop = addPointer(lTop, sizeof(word));
#endif

  requireStack(local, sizeof(struct queryFrame)+MAXARITY*sizeof(word));

  qf	             = (QueryFrame) lTop;
					/* fill top-frame */
  top	             = &qf->top_frame;
  top->parent        = NULL;
  top->predicate     = PROCEDURE_dc_call_prolog->definition;
  top->clause        = &cref;
#ifdef O_PROFILE
  if ( LD->profile.active )
    top->prof_node = profCall(top->predicate PASS_LD);
  else
    top->prof_node = NULL;
#endif
  if ( environment_frame )
    setNextFrameFlags(top, environment_frame);
  else
    top->flags	     = 0;
  fr                 = &qf->frame;
  fr->parent         = top;
  setNextFrameFlags(fr, top);
  set(top, FR_NODEBUG);
  fr->programPointer = clause.codes;
  def                = getProcDefinedDefinition(&fr, NULL, proc PASS_LD);
#ifdef O_SHIFT_STACKS
  qf	             = (QueryFrame) lTop;
#endif
  arity		     = def->functor->arity;

  SECURE(checkStacks(environment_frame, NULL));
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
  qf->saved_PC		= NULL;
  qf->aSave             = aTop;
  qf->solutions         = 0;
  qf->exception		= 0;
  qf->exception_env.parent = NULL;
  qf->saved_throw_env   = LD->exception.throw_environment;

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
  { set(fr, FR_NODEBUG);
    debugstatus.suspendTrace++;
    qf->debugSave = debugstatus.debugging;
    debugstatus.debugging = DBG_OFF;
#ifdef O_LIMIT_DEPTH
    qf->saved_depth_limit   = depth_limit;
    qf->saved_depth_reached = depth_reached;
    depth_limit = (uintptr_t)DEPTH_NO_LIMIT;
#endif
  }
  fr->predicate      = def;
  fr->clause         = NULL;
					/* create initial choicepoint */
  qf->choice.type   = CHP_TOP;
  qf->choice.parent = NULL;
  qf->choice.frame  = fr;
#ifdef O_PROFILE
  qf->choice.prof_node = NULL;
  fr->prof_node = NULL;			/* true? */
#endif
  Mark(qf->choice.mark);
					/* publish environment */
  LD->choicepoints  = &qf->choice;

  if ( true(def, FOREIGN) )
  { fr->clause = NULL;			/* initial context */
  } else
  { fr->clause = def->definition.clauses;
  }
#ifdef O_LOGICAL_UPDATE
  fr->generation = GD->generation;
#endif
					/* context module */
  if ( true(def, METAPRED) )
  { if ( ctx )
      setContextModule(fr, ctx);
    else if ( qf->saved_environment )
      setContextModule(fr, contextModule(qf->saved_environment));
    else
      setContextModule(fr, MODULE_user);
  }

  environment_frame = fr;
  DEBUG(2, Sdprintf("QID=%d\n", QidFromQuery(qf)));
  updateAlerted(LD);

  return QidFromQuery(qf);
}


static void
discard_query(QueryFrame qf)
{ GET_LD
  LocalFrame FR  = &qf->frame;

  discardChoicesAfter(FR PASS_LD);
  discardFrame(FR, FINISH_CUT PASS_LD);
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

  LD->choicepoints  = qf->saved_bfr;
  environment_frame = qf->saved_environment;
  LD->exception.throw_environment = qf->saved_throw_env;
  aTop		    = qf->aSave;
  lTop		    = (LocalFrame)qf;
  if ( true(qf, PL_Q_NODEBUG) )
  { debugstatus.suspendTrace--;
    debugstatus.debugging = qf->debugSave;
#ifdef O_LIMIT_DEPTH
    depth_limit   = qf->saved_depth_limit;
    depth_reached = qf->saved_depth_reached;
#endif /*O_LIMIT_DEPTH*/
  }
  updateAlerted(LD);
  SECURE(checkStacks(environment_frame, NULL));
}


void
PL_cut_query(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);

  SECURE(assert(qf->magic == QID_MAGIC));
  if ( qf->foreign_frame )
    PL_close_foreign_frame(qf->foreign_frame);

  if ( false(qf, PL_Q_DETERMINISTIC) )
    discard_query(qf);

  restore_after_query(qf);
  qf->magic = 0;			/* disqualify the frame */
}


void
PL_close_query(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);

  SECURE(assert(qf->magic == QID_MAGIC));
  if ( qf->foreign_frame )
    PL_close_foreign_frame(qf->foreign_frame);

  if ( false(qf, PL_Q_DETERMINISTIC) )
    discard_query(qf);

  if ( !(qf->exception && true(qf, PL_Q_PASS_EXCEPTION)) )
    Undo(qf->choice.mark);

  restore_after_query(qf);
  qf->magic = 0;			/* disqualify the frame */
}


term_t
PL_exception(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);

  return qf->exception;
}


#if O_SHIFT_STACKS
#define SAVE_REGISTERS(qid) \
	{ QueryFrame qf = QueryFromQid(qid); \
	  qf->registers.fr  = FR; \
	}
#define LOAD_REGISTERS(qid) \
	{ QueryFrame qf = QueryFromQid(qid); \
	  FR = qf->registers.fr; \
	}
#else /*O_SHIFT_STACKS*/
#define SAVE_REGISTERS(qid)
#define LOAD_REGISTERS(qid)
#endif /*O_SHIFT_STACKS*/

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
	lTop  = (LocalFrame)(ARGP + cref->clause->variables); \
	PC    = cref->clause->codes; \
	NEXT_INSTRUCTION;
#define TRY_CLAUSE(cref, cond, altpc) \
	umode = uread; \
	CL    = cref; \
	lTop  = (LocalFrame)(ARGP + cref->clause->variables); \
	if ( cond ) \
	{ Choice ch = newChoice(CHP_JUMP, FR PASS_LD); \
 	  ch->value.PC = altpc; \
	} \
	PC    = cref->clause->codes; \
	NEXT_INSTRUCTION;

int
PL_next_solution(qid_t qid)
{ GET_LD
  AR_CTX
  QueryFrame QF;			/* Query frame */
  LocalFrame FR;			/* current frame */
  LocalFrame NFR;			/* Next frame */
  Word	     ARGP = NULL;		/* current argument pointer */
  Code	     PC = NULL;			/* program counter */
  Definition DEF = NULL;		/* definition of current procedure */
  unify_mode umode = uread;		/* Unification mode */
  Word *     aFloor = aTop;		/* don't overwrite old arguments */
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

#define VMI(Name,na,a)		Name ## _LBL: \
				  count(Name, PC); \
				  START_PROF(Name, #Name);
#define VMI_GOTO(n)		{ END_PROF(); \
				  goto n ## _LBL; \
				}
#define NEXT_INSTRUCTION	{ END_PROF(); \
				  DbgPrintInstruction(FR, PC); \
				  goto *(void *)((intptr_t)(*PC++)); \
				}
#ifndef ASM_NOP
#define ASM_NOP asm("nop")
#endif
#define SEPERATE_VMI ASM_NOP

#else /* VMCODE_IS_ADDRESS */

code thiscode;

#define VMI(Name,na,a)		case Name: \
				  case_ ## Name:
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the real start point  of   this  function.  Simply loads the VMI
registers from the frame filled by   PL_open_query()  and either jump to
depart_continue() to do the normal thing or to the backtrack point.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  QF  = QueryFromQid(qid);
  SECURE(assert(QF->magic == QID_MAGIC));
  if ( true(QF, PL_Q_DETERMINISTIC) )	/* last one succeeded */
  { fid_t fid = QF->foreign_frame;
    QF->foreign_frame = 0;
    PL_close_foreign_frame(fid);
    Undo(QF->choice.mark);
    fail;
  }
  FR  = &QF->frame;
  DEBUG(9, Sdprintf("QF=%p, FR=%p\n", QF, FR));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check for exceptions raised by foreign code.  PL_throw() uses longjmp()
to get back here.  Our task is to restore the environment and throw the
Prolog exception.

setjmp()/longjmp clobbers register variables. FR   is  restored from the
environment. BFR is volatile, and qid is an argument. These are the only
variables used in the B_THROW instruction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  DEBUG(9, Sdprintf("Setjmp env at %p\n", &QF->exception_env.exception_jmp_env));
  if ( setjmp(QF->exception_env.exception_jmp_env) != 0 )
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

    if ( LD->current_signal ) 
    { unblockSignal(LD->current_signal);
      LD->current_signal = 0;	/* TBD: saved? */
    }

    goto b_throw;
  }

  LD->exception.throw_environment = &QF->exception_env;
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

#if VMCODE_IS_ADDRESS
  NEXT_INSTRUCTION;
#else
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
  DEF = getProcDefinedDefinition(&NFR, PC,
				 PROCEDURE_dwakeup1
				 PASS_LD);
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
  pl_abort(ABORT_NORMAL);		/* dubious */

do_retry:
  if ( rframe0 != rframe )
    Sdprintf("[No retry-information for requested frame]\n");

  Sdprintf("[Retrying frame %d running %s]\n",
	   (Word)rframe - (Word)lBase,
	   predicateName(rframe->predicate));

  discardChoicesAfter(rframe PASS_LD);
  environment_frame = FR = rframe;
  DEF = FR->predicate;
  Undo(m);
  exception_term = 0;
#ifdef O_LOGICAL_UPDATE
  if ( false(DEF, DYNAMIC) )
    FR->generation = GD->generation;
#endif

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
as to investigate optimisation in the future.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

				MARK(BKTRK);
clause_failed:				/* shallow backtracking */
END_PROF();
START_PROF(P_SHALLOW_BACKTRACK, "P_SHALLOW_BACKTRACK");
{ Choice ch = BFR;

  if ( FR == ch->frame && ch->type == CHP_CLAUSE )
  { ClauseRef next;
    Undo(ch->mark);
    aTop = aFloor;

    ARGP = argFrameP(FR, 0);
    if ( !(CL = findClause(ch->value.clause, ARGP, FR, DEF, &next PASS_LD)) )
      FRAME_FAILED;			/* should not happen */
    PC = CL->clause->codes;
    umode = uread;

    if ( ch == (Choice)argFrameP(FR, CL->clause->variables) )
    { if ( next )
      { ch->value.clause = next;
	lTop = addPointer(ch, sizeof(*ch));
	NEXT_INSTRUCTION;
      } else if ( debugstatus.debugging )
      { ch->type = CHP_DEBUG;
	lTop = addPointer(ch, sizeof(*ch));
	NEXT_INSTRUCTION;
      }

      BFR = ch->parent;
      lTop = (LocalFrame)ch;
      NEXT_INSTRUCTION;
    } else
    { BFR = ch->parent;
      lTop = (LocalFrame)argFrameP(FR, CL->clause->variables);
      
      if ( next )
      { ch = newChoice(CHP_CLAUSE, FR PASS_LD);
	ch->value.clause = next;
      } else if ( debugstatus.debugging )
      { ch = newChoice(CHP_DEBUG, FR PASS_LD);
      }

      requireStack(local, (size_t)argFrameP((LocalFrame)NULL, MAXARITY));
      NEXT_INSTRUCTION;
    }
  }  
}

body_failed:
frame_failed:
END_PROF();
START_PROF(P_DEEP_BACKTRACK, "P_DEEP_BACKTRACK");
{
#ifdef O_DEBUGGER
  Choice ch0 = BFR;
#endif
  Choice ch;
  LocalFrame fr0;

  DEBUG(3, Sdprintf("BACKTRACKING\n"));

next_choice:
  ch = BFR;
  fr0 = FR;
					/* leave older frames */
#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { for(; (void *)FR > (void *)ch; FR = FR->parent)
    { if ( false(FR, FR_NODEBUG) )
      { Choice sch = findStartChoice(FR, ch0);

	if ( sch )
	{ Undo(sch->mark);

	  switch( tracePort(FR, BFR, FAIL_PORT, NULL PASS_LD) )
	  { case ACTION_RETRY:
	      environment_frame = FR;
	      DEF = FR->predicate;
#ifdef O_LOGICAL_UPDATE
	      if ( false(DEF, DYNAMIC) )
		FR->generation = GD->generation;
#endif
	      clear(FR, FR_CATCHED);
	      goto retry_continue;
	  }
	} else
	{ DEBUG(2, Sdprintf("Cannot trace FAIL [%d] %s\n",
			    levelFrame(FR), predicateName(FR->predicate)));
	}
      }

      /*Profile(FR->predicate->profile_fails++);*/
      leaveFrame(FR PASS_LD);
      if ( exception_term )
	goto b_throw;
    }
  } else
#endif /*O_DEBUGGER*/
  { for(; (void *)FR > (void *)ch; FR = FR->parent)
    { /*Profile(FR->predicate->profile_fails++);*/
      leaveFrame(FR PASS_LD);
      if ( exception_term )
	goto b_throw;
    }
  }

  environment_frame = FR = ch->frame;
  Undo(ch->mark);
  aTop = aFloor;			/* reset to start, for interrupts */
  DEF  = FR->predicate;
#ifdef O_DEBUG_BACKTRACK
  last_choice = ch->type;
#endif

  switch(ch->type)
  { case CHP_JUMP:
      DEBUG(3, Sdprintf("    REDO #%ld: Jump in %s\n",
			loffset(FR),
			predicateName(DEF)));
      PC   = ch->value.PC;
      BFR  = ch->parent;
      Profile(profRedo(ch->prof_node PASS_LD));
      lTop = (LocalFrame)ch;
      ARGP = argFrameP(lTop, 0);
      NEXT_INSTRUCTION;
    case CHP_CLAUSE:			/* try next clause */
    { ClauseRef next;
      Clause clause;

      DEBUG(3, Sdprintf("    REDO #%ld: Clause in %s\n",
			loffset(FR),
			predicateName(DEF)));
      ARGP = argFrameP(FR, 0);
      BFR = ch->parent;
      if ( !(CL = findClause(ch->value.clause, ARGP, FR, DEF, &next PASS_LD)) )
	goto next_choice;		/* should not happen */

#ifdef O_DEBUGGER
      if ( debugstatus.debugging && !debugstatus.suspendTrace  )
      { LocalFrame fr;

	if ( !SYSTEM_MODE )		/* find user-level goal to retry */
	{ for(fr = FR; fr && true(fr, FR_NODEBUG); fr = fr->parent)
	    ;
	} else
	  fr = FR;

	if ( fr &&
	     (false(fr->predicate, HIDE_CHILDS) ||
	      false(fr, FR_INBOX)) )
	{ switch( tracePort(fr, BFR, REDO_PORT, NULL PASS_LD) )
	  { case ACTION_FAIL:
	      FRAME_FAILED;
	    case ACTION_IGNORE:
	      VMI_GOTO(I_EXIT);
	    case ACTION_RETRY:
#ifdef O_LOGICAL_UPDATE
	      if ( false(DEF, DYNAMIC) )
		FR->generation = GD->generation;
#endif
	      goto retry_continue;
	  }
	  set(fr, FR_INBOX);
	}
      }
#endif

      clause = CL->clause;
      PC     = clause->codes;
      Profile(profRedo(ch->prof_node PASS_LD));
      lTop   = (LocalFrame)argFrameP(FR, clause->variables);
      umode  = uread;

      if ( next )
      { ch = newChoice(CHP_CLAUSE, FR PASS_LD);
	ch->value.clause = next;
      } else if ( debugstatus.debugging )
      { if ( false(FR, FR_NODEBUG) && true(FR->predicate, HIDE_CHILDS) )
	  ch = newChoice(CHP_DEBUG, FR PASS_LD);
      }

      if ( is_signalled(PASS_LD1) )
      { SAVE_REGISTERS(qid);
	handleSignals(NULL);
	LOAD_REGISTERS(qid);
	if ( exception_term )
	  goto b_throw;
      }

			/* require space for the args of the next frame */
      requireStack(local, (size_t)argFrameP((LocalFrame)NULL, MAXARITY));
      NEXT_INSTRUCTION;
    }
    case CHP_TOP:			/* Query toplevel */
    { Profile(profRedo(ch->prof_node PASS_LD));
      QF = QueryFromQid(qid);
      set(QF, PL_Q_DETERMINISTIC);
      QF->foreign_frame = PL_open_foreign_frame();
      fail;
    }
    case CHP_CATCH:			/* catch/3 */
      Undo(ch->mark);
      callCleanupHandler(ch->frame, FINISH_FAIL PASS_LD);
    case CHP_DEBUG:			/* Just for debugging purposes */
      BFR  = ch->parent;
      goto next_choice;
  }
}
  assert(0);
  return FALSE;
} /* end of PL_next_solution() */
