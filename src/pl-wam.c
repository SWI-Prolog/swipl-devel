/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

/*#define O_SECURE 1*/
/*#define O_DEBUG 1*/
#include "pl-incl.h"

#define	     BFR (LD->choicepoints)	/* choicepoint registration */

#if sun
#include <prof.h>			/* in-function profiling */
#else
#define MARK(label)
#endif

static bool	callForeign(LocalFrame, control_t ctx ARG_LD);
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

#if defined(O_DEBUG) || defined(SECURE_GC) /* use counting for debugging */

static long
loffset(void *p)
{ if ( p == NULL )
    return 0;

  assert((long)p % sizeof(word) == 0);
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

  DEBUG(3, wamListInstruction(Serror, FR->clause->clause, PC));
}

#else

#define DbgPrintInstruction(fr, pc)

#endif




#include "pl-alloc.c"
#include "pl-index.c"

		 /*******************************
		 *	    ASYNC HOOKS		*
		 *******************************/

#if O_ASYNC_HOOK

static struct
{ PL_async_hook_t	hook;		/* the hook function */
  unsigned int		mask;		/* the mask */
} async;


PL_async_hook_t
PL_async_hook(unsigned int count, PL_async_hook_t hook)
{ PL_async_hook_t old = async.hook;

  async.hook = hook;
  async.mask = 1;
  while(async.mask < count)
    async.mask <<= 1;
  async.mask--;

  return old;
}


#endif /*O_ASYNC_HOOK*/

		 /*******************************
		 *	     SIGNALS		*
		 *******************************/

#if 0 /*def O_SAFE_SIGNALS*/

static inline int
is_signalled()
{ sigset_t set;

  sigpending(&set);

  return set != 0;			/* non-portable! */
}

#else

#define is_signalled() (LD->pending_signals != 0)

#endif


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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If a foreign frame is at the top   of the stack and something else needs
to be placed on top of it, the   frame  needs to be `closed': the ->size
must be filled with the number of term-references in the frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
finish_foreign_frame(ARG1_LD)
{ if ( fli_context )
  { FliFrame fr = fli_context;

    if ( (void *)environment_frame < (void *) fr &&
	 (void *)BFR < (void *)fr )
    { fr->size = (Word) lTop - (Word)addPointer(fr, sizeof(struct fliFrame));
      DEBUG(9, Sdprintf("Pushed fli context with %d term-refs\n", fr->size));
    }
  }
}


static fid_t
open_foreign_frame(ARG1_LD)
{ FliFrame fr = (FliFrame) lTop;

  finish_foreign_frame(PASS_LD1);
  requireStack(local, sizeof(struct fliFrame));
  lTop = addPointer(lTop, sizeof(struct fliFrame));
  fr->size = 0;
  Mark(fr->mark);
  fr->parent = fli_context;
  fli_context = fr;

  return consTermRef(fr);
}


static void
close_foreign_frame(fid_t id ARG_LD)
{ FliFrame fr = (FliFrame) valTermRef(id);

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

  finish_foreign_frame(PASS_LD1);
  lTop = addPointer(lTop, sizeof(struct localFrame) + MAXARITY*sizeof(word));
  fr = (FliFrame) lTop;

  requireStack(local, sizeof(struct fliFrame));
  lTop = addPointer(lTop, sizeof(struct fliFrame));
  fr->size = 0;
  Mark(fr->mark);
  fr->parent = fli_context;
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

  fli_context = fr->parent;
  Undo(fr->mark);
  lTop = (LocalFrame) fr;
}

		/********************************
		*         FOREIGN CALLS         *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Calling foreign predicates.  We will have to  set  `lTop',  compose  the
argument  vector  for  the  foreign  function,  call  it and analyse the
result.  The arguments of the frame are derefenced  here  to  avoid  the
need for explicit dereferencing in most foreign predicates themselves.

A non-deterministic foreign predicate  can   return  either the constant
FALSE  to  start  backtracking,  TRUE    to   indicate  success  without
alternatives or anything  else.  The  return   value  is  saved  in  the
choice-point that is  created  after   return  of  the non-deterministic
foreign function. On `redo', the  foreign   predicate  is  called with a
control_t argument that indicates the context   value and the reason for
the call-back.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_FLI_ARGS 10			/* extend switches on change */

#define CALLDETFN(r, argc) \
  { switch(argc) \
    { case 0: \
	r = F(); \
        break; \
      case 1: \
	r = F(A(0)); \
	break; \
      case 2: \
	r = F(A(0),A(1)); \
        break; \
      case 3: \
	r = F(A(0),A(1),A(2)); \
        break; \
      case 4: \
	r = F(A(0),A(1),A(2),A(3)); \
        break; \
      case 5: \
	r = F(A(0),A(1),A(2),A(3),A(4)); \
        break; \
      case 6: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5)); \
        break; \
      case 7: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6)); \
        break; \
      case 8: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7)); \
        break; \
      case 9: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8)); \
        break; \
      case 10: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9)); \
        break; \
      default: \
	r = sysError("Too many arguments to foreign function (>%d)", \
		     MAX_FLI_ARGS); \
    } \
  }

#define CALLNDETFN(r, argc, c) \
  { switch(argc) \
    { case 0: \
	r = F(c); \
        break; \
      case 1: \
	r = F(A(0),(c)); \
	break; \
      case 2: \
	r = F(A(0),A(1),(c)); \
        break; \
      case 3: \
	r = F(A(0),A(1),A(2),(c)); \
        break; \
      case 4: \
	r = F(A(0),A(1),A(2),A(3),(c)); \
        break; \
      case 5: \
	r = F(A(0),A(1),A(2),A(3),A(4),(c)); \
        break; \
      case 6: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),(c)); \
        break; \
      case 7: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),(c)); \
        break; \
      case 8: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),(c)); \
        break; \
      case 9: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),(c)); \
        break; \
      case 10: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),(c)); \
        break; \
      default: \
	r = sysError("Too many arguments to foreign function (>%d)", \
		     MAX_FLI_ARGS); \
    } \
  }


static bool
callForeign(LocalFrame frame, control_t ctx ARG_LD)
{ Definition def = frame->predicate;
  Func function = def->definition.function;
  int argc = def->functor->arity;
  word context = (word)frame->clause;
  word result;
  term_t h0 = argFrameP(frame, 0) - (Word)lBase;
  fid_t cid;
  SaveLocalPtr(s1, frame);
  
#ifdef O_DEBUGGER
retry:
#endif
  lTop = (LocalFrame) argFrameP(frame, argc);
  exception_term = 0;
  frame->clause = NULL;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { int port = (ForeignControl(ctx) == FIRST_CALL ? CALL_PORT : REDO_PORT);

    switch( tracePort(frame, LD->choicepoints, port, NULL) )
    { case ACTION_FAIL:
	fail;
      case ACTION_IGNORE:
	succeed;
      case ACTION_RETRY:
	ctx = FIRST_CALL;
    }
  }
#endif /*O_DEBUGGER*/

  cid = PL_open_foreign_frame();

  SECURE({ int n;
	   Word p0 = argFrameP(frame, 0);
	   
	   for(n=0; n<argc; n++)
	     checkData(p0+n);
	 });

#define F (*function)    
  if ( true(def, P_VARARG) )
  { result = F(h0, argc, context);
    if ( false(def, NONDETERMINISTIC) )
      goto ret_det;
    else
      goto ret_ndet;
  } else
#define A(n) (h0+n)
  { if ( false(def, NONDETERMINISTIC) )	/* deterministic */
    { CALLDETFN(result, argc);

    ret_det:
      RestoreLocalPtr(s1, frame);

      if ( exception_term )
      { if ( result )			/* False alarm */
	{ exception_term = 0;
	  setVar(*valTermRef(exception_bin));
	}
#ifdef O_DEBUGGER
	else
	  goto except;			/* force debugging */
#endif
      }

#ifdef O_DEBUGGER
      if ( debugstatus.debugging )
      { int port;

	switch( (int)result )
	{ case TRUE:
	    port = EXIT_PORT;
	    break;
	  case FALSE:
	  except:
	  { FliFrame ffr = (FliFrame)valTermRef(cid);

	    if ( exception_term )
	    { mark m;
	      Choice ch;

	      m = ffr->mark;
	      PL_close_foreign_frame(cid);
	      ch = newChoice(CHP_DEBUG, frame PASS_LD);
	      ch->mark = m;

	      return FALSE;
	    } else
	    { port = FAIL_PORT;
	      Undo(ffr->mark);
	      break;
	    }
	  }
	  default:
	    goto err_domain;
	}

	switch( tracePort(frame, LD->choicepoints, port, NULL) )
	{ case ACTION_FAIL:
	    fail;
	  case ACTION_IGNORE:
	    succeed;
	  case ACTION_RETRY:
	    ctx = FIRST_CALL;
	    PL_close_foreign_frame(cid);
	    goto retry;
	}
      }
#endif /*O_DEBUGGER*/

      PL_close_foreign_frame(cid);

      switch((int)result)
      { case TRUE:
	  succeed;
	case FALSE:
	  fail;
	default:
#ifdef O_DEBUGGER
	err_domain:
#endif
	{ FunctorDef fd = def->functor;
	  term_t ex = PL_new_term_ref();

	  PL_put_integer(ex, result);

	  return PL_error(stringAtom(fd->name), fd->arity, NULL, ERR_DOMAIN,
			  ATOM_foreign_return_value, ex);
	}
      }
    } else				/* non-deterministic */
    { FliFrame ffr;
      mark m;
      Choice ch;

      CALLNDETFN(result, argc, context);

    ret_ndet:
      RestoreLocalPtr(s1, frame);
      if ( exception_term )
      { if ( result )			/* False alarm */
	{ exception_term = 0;
	  setVar(*valTermRef(exception_bin));
	}
#ifdef O_DEBUGGER
	else
	  goto except;			/* force debugging */
#endif
      }
      
#ifdef O_DEBUGGER
      if ( debugstatus.debugging )
      { int port;

	if ( result )
	{ port = EXIT_PORT;
	} else
	{ if ( exception_term )
	    goto except;
	  else
	  { FliFrame ffr = (FliFrame)valTermRef(cid);
	    port = FAIL_PORT;
	    Undo(ffr->mark);
	  }
	}

	switch( tracePort(frame, LD->choicepoints, port, NULL) )
	{ case ACTION_FAIL:
	    fail;
	  case ACTION_IGNORE:
	    succeed;
	  case ACTION_RETRY:
	    ctx = FIRST_CALL;
	    PL_close_foreign_frame(cid);
	    goto retry;
	}
      }
#endif /*O_DEBUGGER*/

      if ( result == FALSE || result == TRUE )
      { PL_close_foreign_frame(cid);
	return result;
      }

      assert(result & FRG_CONTROL_MASK);
      ffr = (FliFrame) valTermRef(cid);
      m = ffr->mark;
      PL_close_foreign_frame(cid);
      ch = newChoice(CHP_FOREIGN, frame PASS_LD);
      ch->mark = m;
      ch->value.foreign = result;
      frame->clause = (ClauseRef)result; /* for discardFrame() */

      return TRUE;
    }
#undef A
  }
#undef F
}


static int
discardForeignFrame(LocalFrame fr)
{ Definition def = fr->predicate;
  int argc       = def->functor->arity;
  Func function  = def->definition.function;
  word context   = ((word) fr->clause & ~FRG_CONTROL_MASK) | FRG_CUTTED;
  int  result;

#define F	(*function)
#define A(n)	0

  DEBUG(5, Sdprintf("\tCut %s, context = 0x%lx\n",
		    predicateName(def), context));

  CALLNDETFN(result, argc, context);
#undef A
#undef F

  return result;
}


enum finished
{ FINISH_EXIT = 0,
  FINISH_FAIL,
  FINISH_CUT,
  FINISH_EXCEPT
};


static int
unify_finished(term_t catcher, enum finished reason)
{ GET_LD

  static atom_t reasons[] = 
  { ATOM_exit,
    ATOM_fail,
    ATOM_cut,
    ATOM_exception
  };

  if ( reason == FINISH_EXCEPT )
  { SECURE(checkData(valTermRef(exception_bin)));

    return PL_unify_term(catcher,
			 PL_FUNCTOR, FUNCTOR_exception1,
			   PL_TERM, exception_bin);
  } else
  { return PL_unify_atom(catcher, reasons[reason]);
  }
}


static void
frameFinished(LocalFrame fr, enum finished reason)
{ GET_LD
  fid_t cid = PL_open_foreign_frame();
    
  if ( fr->predicate == PROCEDURE_call_cleanup3->definition )
  { term_t catcher = argFrameP(fr, 1) - (Word)lBase;

    if ( unify_finished(catcher, reason) )
    { term_t clean = argFrameP(fr, 2) - (Word)lBase;
      term_t ex = 0;
      int rval;
      
      rval = callProlog(fr->context, clean, PL_Q_CATCH_EXCEPTION, &ex);
      if ( !rval && ex )
	PL_throw(ex);
    }

  }

#ifdef O_DEBUGGER
  callEventHook(PLEV_FRAMEFINISHED, fr);
#endif

  PL_discard_foreign_frame(cid);
}

		 /*******************************
		 *	     TRAILING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trail  an  assignment.  Note  that  -when  using  dynamic  stacks-,  the
assignment should be made *before* calling Trail()!

p is a pointer into the local or   global stack. We trail any assignment
made in the local stack. If the current   mark is from a choice-point we
could improve here. Some marks however are created in foreign code, both
using PL_open_foreign_frame() and directly by   calling Mark(). It would
be a good idea to remove the latter.

Mark() sets LD->mark_bar, indicating  that   any  assignment  above this
value need not be trailed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define Trail(p) \
  if ( p >= (Word)lBase || p < LD->mark_bar ) \
  { requireStack(trail, sizeof(struct trail_entry)); \
    (tTop++)->address = p; \
  }

void
DoTrail(Word p)
{ GET_LD
  Trail(p);
}


#ifdef O_DESTRUCTIVE_ASSIGNMENT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trailing of destructive assignments.  This feature is used by setarg/3.

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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
TrailAssignment(Word p)
{ GET_LD
  Word old = allocGlobal(1);

  *old = *p;				/* save the old value on the global */
  requireStack(trail, 2*sizeof(struct trail_entry));
  (tTop++)->address = p;
  (tTop++)->address = tagTrailPtr(old);
}


static inline void
__do_undo(mark *m ARG_LD)
{ TrailEntry tt = tTop;
  TrailEntry mt = m->trailtop;

  while(--tt >= mt)
  { Word p = tt->address;

    if ( isTrailVal(p) )
    { DEBUG(2, Sdprintf("Undoing a trailed assignment\n"));
      *(--tt)->address = trailVal(p);
    } else
      setVar(*p);
  }

  tTop = mt;
  gTop = m->globaltop;
}


void
do_undo(mark *m)
{ GET_LD
  __do_undo(m PASS_LD);
}

#undef Undo
#define Undo(m) __do_undo(&m PASS_LD)
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/

		/********************************
		*          UNIFICATION          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify is the general unification procedure. This raw routine should only
be called by interpret as it  does   not  undo  bindings made during the
unification in case the unification fails. pl_unify() (implementing =/2)
does undo bindings and should be used   by  foreign predicates. See also
unify_ptrs().

Unification depends on the datatypes available in the system and will in
general need updating if new types are added.  It should be  noted  that
unify()  is  not  the only place were unification happens.  Other points
are:

  - various of the virtual machine instructions
  - various macros, for example APPENDLIST and CLOSELIST
  - unifyAtomic(): unification of atomic data.
  - various builtin predicates. They should be flagged some way.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
unify(Word t1, Word t2 ARG_LD)
{ 
  word w1;
  word w2;

right_recursion:
  w1 = *t1;
  w2 = *t2;

  while(isRef(w1))			/* this is deRef() */
  { t1 = unRef(w1);
    w1 = *t1;
  }
  while(isRef(w2))
  { t2 = unRef(w2);
    w2 = *t2;
  }

  if ( isVar(w1) )
  { if ( isVar(w2) )
    { if ( t1 < t2 )			/* always point downwards */
      { *t2 = makeRef(t1);
	Trail(t2);
	succeed;
      }
      if ( t1 == t2 )
	succeed;
      *t1 = makeRef(t2);
      Trail(t1);
      succeed;
    }
    *t1 = w2;
    Trail(t1);
    succeed;
  }
  if ( isVar(w2) )
  { *t2 = w1;
    Trail(t2);
    succeed;
  }

  if ( w1 == w2 )
    succeed;
  if ( tag(w1) != tag(w2) )
    fail;

  switch(tag(w1))
  { case TAG_ATOM:
      fail;
    case TAG_INTEGER:
      if ( storage(w1) == STG_INLINE ||
	   storage(w2) == STG_INLINE )
	fail;
    case TAG_STRING:
    case TAG_FLOAT:
      return equalIndirect(w1, w2);
    case TAG_COMPOUND:
    { Functor f1 = valueTerm(w1);
      Functor f2 = valueTerm(w2);
      Word e;

      if ( f1->definition != f2->definition )
	fail;

      t1 = f1->arguments;
      t2 = f2->arguments;
      e  = t1+arityFunctor(f1->definition)-1; /* right-recurse on last */

      for(; t1 < e; t1++, t2++)
      { if ( !unify(t1, t2 PASS_LD) )
	  fail;
      }
      goto right_recursion;
    }
  }

  succeed;
}


word
pl_unify(term_t t1, term_t t2)		/* =/2 */
{ GET_LD
  Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);
  mark m;
  int rval;

  Mark(m);
  if ( !(rval = unify(p1, p2 PASS_LD)) )
    Undo(m);

  return rval;  
}


word
pl_notunify(term_t t1, term_t t2)	/* A \= B */
{ GET_LD
  Word p1    = valTermRef(t1);
  Word p2    = valTermRef(t2);

  return can_unify(p1, p2) ? FALSE : TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Public unification procedure for  `raw'  data.   See  also  unify()  and
PL_unify().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
unify_ptrs(Word t1, Word t2)
{ GET_LD
  mark m;
  bool rval;

  Mark(m);
  if ( !(rval = unify(t1, t2 PASS_LD)) )
    Undo(m);

  return rval;  
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
can_unify(t1, t2) succeeds if  two  terms   *can*  be  unified,  without
actually doing so. This  is  basically   a  stripped  version of unify()
above. See this function for comments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
can_unify(Word t1, Word t2)
{ GET_LD
  mark m;
  bool rval;

  Mark(m);
  rval = unify(t1, t2 PASS_LD);
  Undo(m);

  return rval;  
}

		 /*******************************
		 *	   OCCURS-CHECK		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int var_occurs_in(Word v, Word t)
    Succeeds of the term `v' occurs in `t'.  v must be dereferenced on
    entry.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
var_occurs_in(Word v, Word t)
{ GET_LD

right_recursion:
  deRef(t);
  if ( v == t )
    succeed;

  if ( isTerm(*t) )
  { Functor f = valueTerm(*t);
    int arity = arityFunctor(f->definition);

    t = f->arguments;
    for( ; --arity > 0; t++)
    { if ( var_occurs_in(v, t) )
	succeed;
    }
    goto right_recursion;
  }

  fail;
}


static bool
unify_with_occurs_check(Word t1, Word t2)
{ GET_LD
  word w1;
  word w2;

right_recursion:
  w1 = *t1;
  w2 = *t2;

  while(isRef(w1))			/* this is deRef() */
  { t1 = unRef(w1);
    w1 = *t1;
  }
  while(isRef(w2))
  { t2 = unRef(w2);
    w2 = *t2;
  }

  if ( isVar(w1) )
  { if ( isVar(w2) )
    { if ( t1 < t2 )			/* always point downwards */
      { *t2 = makeRef(t1);
	Trail(t2);
	succeed;
      }
      if ( t1 == t2 )
	succeed;
      *t1 = makeRef(t2);
      Trail(t1);
      succeed;
    }
    if ( var_occurs_in(t1, t2) )
      fail;
    *t1 = w2;
    Trail(t1);
    succeed;
  }
  if ( isVar(w2) )
  { if ( var_occurs_in(t2, t1) )
      fail;

    *t2 = w1;
    Trail(t2);
    succeed;
  }

  if ( w1 == w2 )
    succeed;
  if ( tag(w1) != tag(w2) )
    fail;

  switch(tag(w1))
  { case TAG_ATOM:
      fail;
    case TAG_INTEGER:
      if ( storage(w1) == STG_INLINE ||
	   storage(w2) == STG_INLINE )
	fail;
    case TAG_STRING:
    case TAG_FLOAT:
      return equalIndirect(w1, w2);
    case TAG_COMPOUND:
    { int arity;
      Functor f1 = valueTerm(w1);
      Functor f2 = valueTerm(w2);

      if ( f1->definition != f2->definition )
	fail;

      arity = arityFunctor(f1->definition);
      t1 = f1->arguments;
      t2 = f2->arguments;

      for(; --arity > 0; t1++, t2++)
      { if ( !unify_with_occurs_check(t1, t2) )
	  fail;
      }
      goto right_recursion;
    }
  }

  succeed;
}


word
pl_unify_with_occurs_check(term_t t1, term_t t2)
{ GET_LD
  mark m;
  Word p1, p2;
  word rval;

  Mark(m);
  p1 = valTermRef(t1);
  p2 = valTermRef(t2);
  rval = unify_with_occurs_check(p1, p2);
  if ( !rval )
    Undo(m);

  return rval;
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
{ for(; fr; fr = fr->parent)
  { if ( fr->predicate == PROCEDURE_block3->definition &&
	 unify_ptrs(argFrameP(fr, 0), block) )
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Choice
findStartChoice(LocalFrame fr, Choice ch)
{ for( ;
       (void *)ch > (void *)fr || (ch && ch->type == CHP_TOP);
       ch = ch->parent )
  { if ( ch->frame == fr )
    { switch ( ch->type )
      { case CHP_JUMP:
	case CHP_NONE:
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
Find the frame running catch/3. If we found  it, we will mark this frame
and not find it again, as a catcher   can  only catch once from the 1-st
argument goal. Exceptions from the  recover   goal  should be passed (to
avoid a loop and allow for re-throwing).   With  thanks from Gertjan van
Noord.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static LocalFrame
findCatcher(LocalFrame fr, Word ex)
{ Definition catch3  = PROCEDURE_catch3->definition;

  for(; fr; fr = fr->parent)
  { if ( fr->predicate == catch3 &&
	 false(fr, FR_CATCHED) &&
	 unify_ptrs(argFrameP(fr, 1), ex) )
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
#define offset(s, f) ((int)(&((struct s *)NULL)->f))
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
	fr = fr->parent;
      else
      { qf = (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
	break;
      }
    }

  }

  fail;
}
#endif /*O_DEBUGGER*/


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

#define FRAME_FAILED		goto frame_failed
#define CLAUSE_FAILED		goto clause_failed
#define BODY_FAILED		goto body_failed

#ifndef ulong
#define ulong unsigned long
#endif

#ifdef O_PROFILE
#define Profile(g) if ( LD->statistics.profiling ) g
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

  if ( false(def, FOREIGN) )
    leaveDefinition(def);

  if ( true(fr, FR_WATCHED) )
    frameFinished(fr, FINISH_FAIL);
}


static void
discardFrame(LocalFrame fr, enum finished reason)
{ Definition def = fr->predicate;

  DEBUG(3, Sdprintf("discard #%d running %s\n",
		    loffset(fr),
		    predicateName(fr->predicate)));

  if ( true(def, FOREIGN) )
  { if ( fr->clause )
      discardForeignFrame(fr);
  } else
    leaveDefinition(def);

  if ( true(fr, FR_WATCHED) )
    frameFinished(fr, reason);

  fr->clause = NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Discard all choice-points created after  the   creation  of the argument
environment. See also discardFrame().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(O_DEBUG) || defined(SECURE_GC)
char *
chp_chars(Choice ch)
{ static char buf[256];

  Ssprintf(buf, "Choice at #%ld for frame #%ld, type %s",
	   loffset(ch), loffset(ch->frame),
	   ch->type == CHP_JUMP ? "JUMP" :
	   ch->type == CHP_CLAUSE ? "CLAUSE" :
	   ch->type == CHP_FOREIGN ? "FOREIGN" : 
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
      discardFrame(fr2, FINISH_CUT);
  }

  DEBUG(3, Sdprintf(" --> BFR = #%ld\n", loffset(BFR)));
  LD->mark_bar = BFR->mark.globaltop;
} 


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
newChoice(CH_*, FR) Creates a new  choicepoint.   After  creation of the
choice-point, the user has to fill the choice-points mark as well as the
required context value. We do not need finish_foreign_frame() here as in
none of the applicable cases we can be in a foreign environment.
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
  DEBUG(3, Sdprintf("NEW %s\n", chp_chars(ch)));

  return ch;
}


qid_t
PL_open_query(Module ctx, int flags, Procedure proc, term_t args)
{ GET_LD
  QueryFrame qf;
  LocalFrame fr;
  Definition def;
  int arity;
  Word ap;
  ClauseRef clause;

  DEBUG(2, { FunctorDef f = proc->definition->functor;
	     int n;

	     Sdprintf("PL_open_query: %s(", stringAtom(f->name));
	     for(n=0; n < f->arity; n++)
	     { if ( n > 0 )
		 Sdprintf(", ");
	       PL_write_term(Serror, args+n, 999, 0);
	     }
	     Sdprintf(")\n");
	   });

  finish_foreign_frame(PASS_LD1);	/* adjust the size of the context */

					/* should be struct alignment, */
					/* but for now, I think this */
					/* is always the same */
#ifdef JMPBUF_ALIGNMENT
  while ( (ulong)lTop % JMPBUF_ALIGNMENT )
    lTop = addPointer(lTop, sizeof(word));
#endif

  qf	= (QueryFrame) lTop;
  fr    = &qf->frame;
  def   = proc->definition;
  arity	= def->functor->arity;

  requireStack(local, sizeof(struct queryFrame)+arity*sizeof(word));

  SECURE(checkStacks(environment_frame, NULL));
  assert((ulong)fli_context > (ulong)environment_frame);
  assert((ulong)lTop >= (ulong)(fli_context+1));

  if ( flags == TRUE )			/* compatibility */
    flags = PL_Q_NORMAL;
  else if ( flags == FALSE )
    flags = PL_Q_NODEBUG;
  flags &= 0x1f;			/* mask reserved flags */

  qf->magic		= QID_MAGIC;
  qf->flags		= flags;
  qf->saved_environment = environment_frame;
  qf->saved_bfr		= LD->choicepoints;
  qf->aSave             = aTop;
  qf->solutions         = 0;
  qf->exception		= 0;

  fr->parent = NULL;
					/* fill frame arguments */
  ap = argFrameP(fr, 0);
  { int n;
    Word p = valTermRef(args);

    for( n = arity; n-- > 0; p++ )
      *ap++ = linkVal(p);
  }
  lTop = (LocalFrame)ap;

					/* find definition and clause */
  if ( !(clause = def->definition.clauses) && false(def, PROC_DEFINED) )
  { def = trapUndefined(def);
    clause = def->definition.clauses;
  }
  if ( true(def, FOREIGN) )
  { fr->clause = FIRST_CALL;
  } else
  { fr->clause = clause;
  }
					/* context module */
  if ( true(def, METAPRED) )
  { if ( ctx )
      fr->context = ctx;
    else if ( environment_frame )
      fr->context = environment_frame->context;
    else
      fr->context = MODULE_user;
  } else
    fr->context = def->module;

  clearFlags(fr);
{ LocalFrame parent;
  long plevel;

  if ( (parent = parentFrame(fr)) )
    plevel = levelFrame(parent);
  else
    plevel = 0L;

  setLevelFrame(fr, plevel+1);
}
			
  DEBUG(3, Sdprintf("Level = %d\n", levelFrame(fr)));
  if ( true(qf, PL_Q_NODEBUG) )
  { set(fr, FR_NODEBUG);
    debugstatus.suspendTrace++;
    qf->debugSave = debugstatus.debugging;
    debugstatus.debugging = DBG_OFF;
#ifdef O_LIMIT_DEPTH
    qf->saved_depth_limit   = depth_limit;
    qf->saved_depth_reached = depth_reached;
    depth_limit = (unsigned long)DEPTH_NO_LIMIT;
#endif
  }
  fr->predicate      = def;
#ifdef O_LOGICAL_UPDATE
  fr->generation = GD->generation;
#endif

  qf->choice.type   = CHP_TOP;
  qf->choice.parent = NULL;
  qf->choice.frame  = fr;
  Mark(qf->choice.mark);
  LD->choicepoints  = &qf->choice;
  environment_frame = fr;

  DEBUG(2, Sdprintf("QID=%d\n", QidFromQuery(qf)));

  return QidFromQuery(qf);
}


static void
discard_query(QueryFrame qf)
{ GET_LD
  LocalFrame FR  = &qf->frame;

  discardChoicesAfter(FR PASS_LD);
  discardFrame(FR, FINISH_CUT);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Restore the environment.  If an exception was raised by the query, and no
new exception has been thrown, consider it handled.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
restore_after_query(QueryFrame qf)
{ GET_LD
  if ( qf->exception && !exception_term )
    *valTermRef(exception_printed) = 0;

  environment_frame = qf->saved_environment;
  LD->choicepoints  = qf->saved_bfr;
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
  SECURE(checkStacks(environment_frame, NULL));
}


void
PL_cut_query(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);

  SECURE(assert(qf->magic == QID_MAGIC));
  qf->magic = 0;			/* disqualify the frame */

  if ( false(qf, PL_Q_DETERMINISTIC) )
    discard_query(qf);

  restore_after_query(qf);
}


void
PL_close_query(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);

  SECURE(assert(qf->magic == QID_MAGIC));
  qf->magic = 0;			/* disqualify the frame */

  if ( false(qf, PL_Q_DETERMINISTIC) )
    discard_query(qf);

  if ( !(qf->exception && true(qf, PL_Q_PASS_EXCEPTION)) )
    Undo(qf->choice.mark);

  restore_after_query(qf);
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
#define ASM_NOP _PL_nop_counter++
#endif

#ifdef ASM_NOP
int _PL_nop_counter;
#endif

int
PL_next_solution(qid_t qid)
{ GET_LD
  QueryFrame QF;			/* Query frame */
  LocalFrame FR;			/* current frame */
  Word	     ARGP = NULL;		/* current argument pointer */
  Code	     PC;			/* program counter */
  Definition DEF = NULL;		/* definition of current procedure */
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

#if VMCODE_IS_ADDRESS
  static void *jmp_table[] =
  { &&I_NOP_LBL,
    &&I_ENTER_LBL,
    &&I_CALL_LBL,
    &&I_DEPART_LBL,
    &&I_EXIT_LBL,
    &&B_FUNCTOR_LBL,
    &&B_RFUNCTOR_LBL,
    &&H_FUNCTOR_LBL,
    &&H_RFUNCTOR_LBL,
    &&I_POPF_LBL,
    &&B_VAR_LBL,
    &&H_VAR_LBL,
    &&B_CONST_LBL,
    &&H_CONST_LBL,
    &&H_INDIRECT_LBL,
    &&B_INTEGER_LBL,
    &&H_INTEGER_LBL,
    &&B_FLOAT_LBL,
    &&H_FLOAT_LBL,

    &&B_FIRSTVAR_LBL,
    &&H_FIRSTVAR_LBL,
    &&B_VOID_LBL,
    &&H_VOID_LBL,
    &&B_ARGFIRSTVAR_LBL,
    &&B_ARGVAR_LBL,

    &&H_NIL_LBL,
    &&B_NIL_LBL,
    &&H_LIST_LBL,
    &&H_RLIST_LBL,
    &&B_LIST_LBL,
    &&B_RLIST_LBL,

    &&B_VAR0_LBL,
    &&B_VAR1_LBL,
    &&B_VAR2_LBL,

    &&I_USERCALL0_LBL,
    &&I_USERCALLN_LBL,
    &&I_CUT_LBL,
    &&I_APPLY_LBL,

#if O_COMPILE_ARITH
    &&A_ENTER_LBL,
    &&A_INTEGER_LBL,
    &&A_DOUBLE_LBL,
    &&A_VAR0_LBL,
    &&A_VAR1_LBL,
    &&A_VAR2_LBL,
    &&A_VAR_LBL,
    &&A_FUNC0_LBL,
    &&A_FUNC1_LBL,
    &&A_FUNC2_LBL,
    &&A_FUNC_LBL,
    &&A_LT_LBL,
    &&A_GT_LBL,
    &&A_LE_LBL,
    &&A_GE_LBL,
    &&A_EQ_LBL,
    &&A_NE_LBL,
    &&A_IS_LBL,
#endif /* O_COMPILE_ARITH */

#if O_COMPILE_OR
    &&C_OR_LBL,
    &&C_JMP_LBL,
    &&C_MARK_LBL,
    &&C_CUT_LBL,
    &&C_IFTHENELSE_LBL,
    &&C_VAR_LBL,
    &&C_END_LBL,
    &&C_NOT_LBL,
    &&C_FAIL_LBL,
#endif /* O_COMPILE_OR */

    &&B_INDIRECT_LBL,
#if O_BLOCK
    &&I_CUT_BLOCK_LBL,
    &&B_EXIT_LBL,
#endif /*O_BLOCK*/
#if O_INLINE_FOREIGNS
    &&I_CALL_FV0_LBL,
    &&I_CALL_FV1_LBL,
    &&I_CALL_FV2_LBL,
#endif /*O_INLINE_FOREIGNS*/
    &&I_FAIL_LBL,
    &&I_TRUE_LBL,
#ifdef O_SOFTCUT
    &&C_SOFTIF_LBL,
    &&C_SOFTCUT_LBL,
#endif
    &&I_EXITFACT_LBL,
    &&D_BREAK_LBL,
#if O_CATCHTHROW
    &&I_CATCH_LBL,
    &&B_THROW_LBL,
#endif
    &&I_CONTEXT_LBL,
    &&C_LCUT_LBL,
    &&I_CALLCLEANUP_LBL,
    &&I_EXITCLEANUP_LBL,
    NULL
  };

#define VMI(Name)	Name ## _LBL: count(Name, PC);
#if VMCODE_IS_ADDRESS
#define NEXT_INSTRUCTION	{ DbgPrintInstruction(FR, PC); \
				  goto *(void *)((long)(*PC++)); \
				}
#else
#define NEXT_INSTRUCTION	{ DbgPrintInstruction(FR, PC); \
				  goto *jmp_table[*PC++]; \
				}
#endif

#else /* VMCODE_IS_ADDRESS */

code thiscode;

#define VMI(Name)		case Name: count(Name, PC);
#define NEXT_INSTRUCTION	{ DbgPrintInstruction(FR, PC); \
                                  goto next_instruction; \
				}

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
  { Undo(QF->choice.mark);
    fail;
  }
  FR  = &QF->frame;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check for exceptions raised by foreign code.  PL_throw() uses longjmp()
to get back here.  Our task is to restore the environment and throw the
Prolog exception.

setjmp()/longjmp clobbers register variables. FR   is  restored from the
environment. BFR is volatile, and qid is an argument. These are the only
variables used in the B_THROW instruction.

Is there a way to make the compiler keep its mouth shut!?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( setjmp(QF->exception_jmp_env) != 0 )
  { FliFrame ffr;
#ifdef O_PLMT
    __PL_ld = GLOBAL_LD;		/* might be clobbered */
#endif
    ffr = fli_context;

    FR = environment_frame;
    while(ffr && (void *)ffr > (void *)FR) /* discard foreign contexts */
      ffr = ffr->parent;
    fli_context = ffr;

    if ( LD->current_signal ) 
    { unblockSignal(LD->current_signal);
      LD->current_signal = 0;	/* TBD: saved? */
    }

    goto b_throw;
  }

  DEF = FR->predicate;
  if ( QF->solutions )
  { BODY_FAILED;
  } else
    goto retry_continue;

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
D_BREAK implements break-points in the  code.   A  break-point is set by
replacing  an  instruction  by  a   D_BREAK  instruction.  The  orininal
instruction is saved in a table. replacedBreak() fetches it.

Note that we must  be  careful  that   the  user  may  have  removed the
break-point in the debugger, so we must check for it.

We might be in a state where  we   are  writing  the arguments above the
current lTop, and therefore with higher this  with the maximum number of
arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(D_BREAK)
#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int action;
      LocalFrame lSave = lTop;

      lTop = (LocalFrame)argFrameP(lTop, MAXARITY);
      clearUninitialisedVarsFrame(FR, PC-1);
      action = tracePort(FR, BFR, BREAK_PORT, PC-1);
      lTop = lSave;

      switch(action)
      { case ACTION_RETRY:
	  goto retry;
      }

      if ( PC[-1] != encode(D_BREAK) )
      { PC--;
	NEXT_INSTRUCTION;
      }
    }
#if VMCODE_IS_ADDRESS
    { void *c = (void *)replacedBreak(PC-1);
      
      goto *c;
    }
#else
    thiscode = replacedBreak(PC-1);
    goto resumebreak;
#endif      
#endif /*O_DEBUGGER*/

    VMI(I_NOP)
	NEXT_INSTRUCTION;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An atomic constant in the head  of  the  clause.   ARGP  points  to  the
current  argument  to be matched.  ARGP is derefenced and unified with a
constant argument.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { word c;
    Word k;						MARK(HCONST);

    VMI(H_CONST)
	c = (word)*PC++;
	goto common_hconst;
    VMI(H_NIL)
        c = ATOM_nil;

  common_hconst:
        deRef2(ARGP++, k);
        if (isVar(*k))
	{ *k = c;
	  Trail(k);
	  NEXT_INSTRUCTION;
	}
        if (*k == c)
	  NEXT_INSTRUCTION;
        CLAUSE_FAILED;
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
32-bit integer in the head. Copy to the  global stack if the argument is
variable, compare the numbers otherwise.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(H_INTEGER) MARK(HINT)
      { register Word k;

	deRef2(ARGP++, k);
	if (isVar(*k))
	{ Word p = allocGlobal(3);

	  *k   = consPtr(p, TAG_INTEGER|STG_GLOBAL);
	  Trail(k);
	  *p++ = mkIndHdr(1, TAG_INTEGER);
	  *p++ = (long)*PC++;
	  *p++ = mkIndHdr(1, TAG_INTEGER);
	  NEXT_INSTRUCTION;
	} else if ( isBignum(*k) && valBignum(*k) == (long)*PC++ )
	  NEXT_INSTRUCTION;

      	CLAUSE_FAILED;
      }  

    VMI(H_FLOAT) MARK(HFLOAT)
      { Word k;

	deRef2(ARGP++, k);
	if (isVar(*k))
	{ Word p = allocGlobal(4);

	  *k   = consPtr(p, TAG_FLOAT|STG_GLOBAL);
	  Trail(k);
	  *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
	  cpDoubleData(p, PC);
	  *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
	  NEXT_INSTRUCTION;
	} else if ( isReal(*k) )
	{ Word p = valIndirectP(*k);

	  switch(WORDS_PER_DOUBLE) /* depend on compiler to clean up */
	  { case 2:
	      if ( *p++ != *PC++ )
	        CLAUSE_FAILED;
	    case 1:
	      if ( *p++ == *PC++ )
	        NEXT_INSTRUCTION;
	      CLAUSE_FAILED;
	    default:
	      assert(0);
	  }
	}

      	CLAUSE_FAILED;
      }  

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General indirect in the head.  Used for strings only at the moment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(H_INDIRECT) MARK(HINDIR);
      { register Word k;

	deRef2(ARGP++, k);
	if (isVar(*k))
	{ *k = globalIndirectFromCode(&PC);
	  Trail(k);
	  NEXT_INSTRUCTION;
	}
	if ( isIndirect(*k) && equalIndirectFromCode(*k, &PC) )
	  NEXT_INSTRUCTION;
	CLAUSE_FAILED;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An atomic constant in the body of  a  clause.   We  know  that  ARGP  is
pointing  to  a  not  yet  instantiated  argument  of the next frame and
therefore can just fill the argument.  Trailing is not needed as this is
above the stack anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_CONST) MARK(BCONST);
      { *ARGP++ = (word)*PC++;
	NEXT_INSTRUCTION;
      }
    VMI(B_NIL) MARK(BNIL);
      { *ARGP++ = ATOM_nil;
        NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
32-bit integer in write-mode (body).  Simply   create  the bignum on the
global stack and assign the pointer to *ARGP.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(B_INTEGER) MARK(BINT)
      { Word p = allocGlobal(3);

	*ARGP++ = consPtr(p, TAG_INTEGER|STG_GLOBAL);
	*p++ = mkIndHdr(1, TAG_INTEGER);
	*p++ = (long)*PC++;
	*p++ = mkIndHdr(1, TAG_INTEGER);
	NEXT_INSTRUCTION;
      }  

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Double  in  the  body.  Simply  copy  to  the  global  stack.  See  also
globalReal().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(B_FLOAT) MARK(BINT)
      { Word p = allocGlobal(4);

	*ARGP++ = consPtr(p, TAG_FLOAT|STG_GLOBAL);
	*p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
	cpDoubleData(p, PC);
	*p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
	NEXT_INSTRUCTION;
      }  


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_INDIRECT need to copy the  value  on   the  global  stack  because the
XR-table might be freed due to a retract.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_INDIRECT) MARK(BIDT);
      { *ARGP++ = globalIndirectFromCode(&PC);
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the head which is not an anonymous one and is not used for
the first time.  Invoke general unification between the argument pointer
and the variable, whose offset is given relative to  the  frame.

Its doubtfull whether inlining (the simple   cases)  is worthwhile. I've
tested this on various platforms, and   the  results vary. Simplicity is
probably worth more than the 0.001% performance to gain.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(H_VAR) MARK(HVAR);
      { Word p1 = varFrameP(FR, *PC++);
	Word p2 = ARGP++;

	if ( unify(p1, p2 PASS_LD) )
	  NEXT_INSTRUCTION;
	CLAUSE_FAILED;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body which is not an anonymous one, is  not  used  for
the  first  time  and is nested in a term (with B_FUNCTOR).  We now know
that *ARGP is a variable,  so  we  either  copy  the  value  or  make  a
reference.   The  difference between this one and B_VAR is the direction
of the reference link in case *k turns out to be variable.

ARGP is pointing into the term on the global stack we are creating.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_ARGVAR) MARK(BAVAR);
      { Word k;

	deRef2(varFrameP(FR, *PC++), k);	
	if ( isVar(*k) )
	{ if ( ARGP < k )
	  { setVar(*ARGP);
	    *k = makeRefG(ARGP++);
	    Trail(k);
	    NEXT_INSTRUCTION;
	  }
	  *ARGP++ = makeRefG(k);	/* both on global stack! */
	  NEXT_INSTRUCTION;	  
	}
	*ARGP++ = *k;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body which is not an anonymous one and is not used for
the first time.  We now know that *ARGP is a variable, so we either copy
the value or make a reference.  Trailing is not needed as we are writing
above the stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { int n;

    VMI(B_VAR0)						MARK(BVAR0);
      n = VAROFFSET(0);
      goto common_bvar;
    VMI(B_VAR1)						MARK(BVAR1);
      n = VAROFFSET(1);
      goto common_bvar;
    VMI(B_VAR2)						MARK(BVAR2);
      n = VAROFFSET(2);
      goto common_bvar;
    VMI(B_VAR)					MARK(BVARN);
      n = (int)*PC++;
    common_bvar:
    { Word k = varFrameP(FR, n);

      *ARGP++ = linkVal(k);

      NEXT_INSTRUCTION;
    }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the head, which is  not anonymous, but encountered for the
first time. So we know that the variable   is  still a variable. Copy or
make a reference. Trailing is  not  needed   as  we  are writing in this
frame. As ARGP is pointing in the  argument   list,  it  is on the local
stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(H_FIRSTVAR)
      MARK(HFVAR);
      { varFrame(FR, *PC++) = (isVar(*ARGP) ? makeRef(ARGP) : *ARGP);
	ARGP++;
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body nested in a term, encountered for the first time.
We now know both *ARGP and the variable are variables.  ARGP  points  to
the  argument  of  a  term  on  the  global stack.  The reference should
therefore go from k to ARGP.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_ARGFIRSTVAR)
      MARK(BAFVAR);
      { setVar(*ARGP);
	varFrame(FR, *PC++) = makeRefG(ARGP++);
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body, encountered for the first  time.   We  now  know
both  *ARGP and the variable are variables.  We set the variable to be a
variable (it is uninitialised memory) and make a reference.  No trailing
needed as we are writing in this and the next frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_FIRSTVAR)
      MARK(BFVAR);
      { Word k = varFrameP(FR, *PC++);

	setVar(*k);
	*ARGP++ = makeRefL(k);
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A singleton variable in the head.  Just increment the argument  pointer.
Also generated for non-singleton variables appearing on their own in the
head  and  encountered  for  the  first  time.   Note  that the compiler
suppresses H_VOID when there are no other instructions before I_ENTER or
I_EXIT.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(H_VOID) MARK(HVOID);
      { ARGP++;
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A singleton variable in the body. Ensure the argument is a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_VOID) MARK(BVOID);
      { setVar(*ARGP++);
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A functor in the head.  If the current argument is a  variable  we  will
instantiate  it  with  a  new  term,  all  whose  arguments  are  set to
variables.  Otherwise we check the functor  definition.   In  both  case
ARGP  is  pushed  on the argument stack and set to point to the leftmost
argument of the  term.   Note  that  the  instantiation  is  trailed  as
dereferencing might have caused we are now pointing in a parent frame or
the global stack (should we check?  Saves trail! How often?).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(H_FUNCTOR)
      MARK(HFUNC);
      { functor_t f;

	requireStack(argument, sizeof(Word));
	*aTop++ = ARGP + 1;
    VMI(H_RFUNCTOR)
	f = (functor_t) *PC++;
        deRef(ARGP);
	if ( isVar(*ARGP) )
	{ int arity = arityFunctor(f);
	  Word ap;

#ifdef O_SHIFT_STACKS
	  if ( gTop + 1 + arity > gMax )
	    growStacks(FR, BFR, PC, FALSE, TRUE, FALSE);
#else
	  requireStack(global, sizeof(word)*(1+arity));
#endif

	  ap = gTop;
	  *ARGP = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
	  Trail(ARGP);
	  *ap++ = f;
	  ARGP = ap;
	  while(arity-- > 0)
	  { setVar(*ap++);
	  }
	  gTop = ap;
	  NEXT_INSTRUCTION;
	}
	if ( hasFunctor(*ARGP, f) )
	{ ARGP = argTermP(*ARGP, 0);
	  NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;	    

    VMI(H_LIST) MARK(HLIST);
        requireStack(argument, sizeof(Word));
	*aTop++ = ARGP + 1;
    VMI(H_RLIST) MARK(HRLIST);
	deRef(ARGP);
	if ( isVar(*ARGP) )
	{ 
#if O_SHIFT_STACKS
  	  if ( gTop + 3 > gMax )
	    growStacks(FR, BFR, PC, FALSE, TRUE, FALSE);
#else
	  requireStack(global, 3*sizeof(word));
#endif
	  *ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
	  Trail(ARGP);
	  *gTop++ = FUNCTOR_dot2;
	  ARGP = gTop;
	  setVar(*gTop++);
	  setVar(*gTop++);
	  NEXT_INSTRUCTION;
	}
	if ( isList(*ARGP) )
	{ ARGP = argTermP(*ARGP, 0);
	  NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A functor in the body.  As we don't expect ARGP to point to  initialised
memory  while  in  body  mode  we  just  allocate  the  term,  but don't
initialise the arguments to variables.  Allocation is done in  place  to
avoid a function call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_FUNCTOR) MARK(BFUNC);
      { functor_t f;
	int arity;

	requireStack(argument, sizeof(Word));
	*aTop++ = ARGP+1;
    VMI(B_RFUNCTOR) MARK(BRFUNC);
	f = (functor_t) *PC++;
	arity = arityFunctor(f);
	requireStack(global, sizeof(word) * (1+arity));
	*ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
	*gTop++ = f;
	ARGP = gTop;
	gTop += arity;

	NEXT_INSTRUCTION;
      }

    VMI(B_LIST) MARK(BLIST);
      { requireStack(argument, sizeof(Word));
	*aTop++ = ARGP+1;
    VMI(B_RLIST) MARK(BRLIST);
	requireStack(global, sizeof(word) * 3);
	*ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
	*gTop++ = FUNCTOR_dot2;
	ARGP = gTop;
	gTop += 2;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pop the saved argument pointer (see H_FUNCTOR and B_FUNCTOR).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_POPF) MARK(POP);
      { ARGP = *--aTop;
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Enter the body of the clause.  This  instruction  is  left  out  if  the
clause  has no body.  The basic task of this instruction is to move ARGP
from the argument part of this frame into the argument part of the child
frame to be built.  `BFR' (the last frame with alternatives) is  set  to
this   frame   if   this   frame  has  alternatives,  otherwise  to  the
backtrackFrame of this frame.

If this frame has no alternatives it is possible to  put  the  backtrack
frame  immediately  on  the backtrack frame of this frame.  This however
makes debugging much more  difficult  as  the  system  will  do  a  deep
backtrack without showing the fail ports explicitely.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(I_ENTER) MARK(ENTER);
      { 
#if O_DEBUGGER
	if ( debugstatus.debugging )
	{ clearUninitialisedVarsFrame(FR, PC);
	  switch(tracePort(FR, BFR, UNIFY_PORT, PC))
	  { case ACTION_RETRY:
	      goto retry;
	    case ACTION_FAIL:
	      FRAME_FAILED;
	  }
	}
#endif /*O_DEBUGGER*/

	ARGP = argFrameP(lTop, 0);
        NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CONTEXT is used by  non-meta  predicates   that  are  compiled  into a
different  module  using  <module>:<head>  :-    <body>.  The  I_CONTEXT
instruction immediately follows the I_ENTER. The argument is the module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_CONTEXT) MARK(CONTEXT);
      { Module m = (Module)*PC++;

	FR->context = m;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Part of call_cleanup(:Goal, :Cleanup).  Simply set a flag on the frame and
call the 1-st argument.  See also I_CATCH.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_CALLCLEANUP)
      { if ( BFR->frame == FR && BFR == (Choice)argFrameP(FR, 3) )
	{ assert(BFR->type == CHP_DEBUG);
	  BFR->type = CHP_CATCH;
	} else
	  newChoice(CHP_CATCH, FR PASS_LD);

	set(FR, FR_WATCHED);
				/* = B_VAR0 */
	*argFrameP(lTop, 0) = linkVal(argFrameP(FR, 0));

	goto i_usercall0;
      }
      
    VMI(I_EXITCLEANUP)
      { if ( BFR->frame == FR && BFR == (Choice)argFrameP(FR, 3) )
	{ assert(BFR->type == CHP_CATCH);

	  DEBUG(3, Sdprintf(" --> BFR = #%ld\n", loffset(BFR->parent)));
	  BFR = BFR->parent;

	  frameFinished(FR, FINISH_EXIT);
	}

	NEXT_INSTRUCTION;		/* goto i_exit? */
      }

#if O_CATCHTHROW
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CATCH has to fake a choice-point to   make  it possible to undo before
starting the recover action. Otherwise it simply   has to call the first
argument.  Catch is defined as:

catch(Goal, Pattern, Recover) :-
	$catch.

which is translated to:
	I_ENTER
	I_CATCH
	I_EXIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_CATCH)
      { if ( BFR->frame == FR && BFR == (Choice)argFrameP(FR, 3) )
	{ assert(BFR->type == CHP_DEBUG);
	  BFR->type = CHP_CATCH;
	} else
	  newChoice(CHP_CATCH, FR PASS_LD);

					/* = B_VAR0 */
	*argFrameP(lTop, 0) = linkVal(argFrameP(FR, 0));

	goto i_usercall0;
      }
      
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The B_THROW code is the implementation for   throw/1.  The call walks up
the stack, looking for a frame running catch/3 on which it can unify the
exception code. It then cuts all  choicepoints created since throw/3. If
throw/3 is not found, it sets  the   query  exception  field and returns
failure. Otherwise, it will simulate an I_USERCALL0 instruction: it sets
the FR and lTop as it it  was   running  the  throw/3 predicate. Then it
pushes the recovery goal from throw/3 and jumps to I_USERCALL0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_THROW) MARK(B_THROW);
      { Word catcher;
	word except;
	LocalFrame catchfr;

	PL_raise_exception(argFrameP(lTop, 0) - (Word)lBase);
    b_throw:
        assert(exception_term);
        catcher = valTermRef(exception_term);

	SECURE(checkData(catcher));
	DEBUG(0, { Sdprintf("Throwing ");
		   PL_write_term(Serror, wordToTermRef(catcher), 1200, 0);
		   Sdprintf("\n");
		 });

	deRef(catcher);
	except = *catcher;
        catchfr = findCatcher(FR, catcher);

	SECURE(checkData(catcher));	/* verify all data on stacks stack */

#if O_DEBUGGER
	if ( !catchfr &&
	     hasFunctor(except, FUNCTOR_error2) &&
	     *valTermRef(exception_printed) != except )
	{ QF = QueryFromQid(qid);	/* reload for relocation */

	  if ( trueFeature(DEBUG_ON_ERROR_FEATURE) &&
	       false(QF, PL_Q_CATCH_EXCEPTION) &&
	       !isCatchedInOuterQuery(QF, catcher) )
	  { fid_t fid = PL_open_foreign_frame();
	    term_t t0 = PL_new_term_refs(2);
	    
	    PL_put_atom(t0+0, ATOM_error);
	    *valTermRef(t0+1) = except;
	    PL_call_predicate(NULL, FALSE, PROCEDURE_print_message2, t0);
	    PL_close_foreign_frame(fid);
	    *valTermRef(exception_printed) = except;

	    pl_trace();
	  }
	}

        if ( debugstatus.debugging )
	{ for( ; FR && FR > catchfr; FR = FR->parent )
	  { Choice ch = findStartChoice(FR, LD->choicepoints);

	    if ( ch )
	    { int printed = (*valTermRef(exception_printed) == except);

					/* needed to avoid destruction */
					/* in the undo */
	      discardChoicesAfter((LocalFrame)ch PASS_LD);
	      undo_while_saving_term(&ch->mark, catcher);
	      except = *catcher;
	      *valTermRef(LD->exception.pending) = except;
	      if ( printed )
		*valTermRef(exception_printed) = except;

	      environment_frame = FR;
	      switch(tracePort(FR, ch, EXCEPTION_PORT, PC))
	      { case ACTION_RETRY:
		  *valTermRef(exception_printed) = 0;
		  Undo(ch->mark);
		  discardChoicesAfter(FR PASS_LD);
		  DEF = FR->predicate;
#ifdef O_LOGICAL_UPDATE
		  if ( false(DEF, DYNAMIC) )
		    FR->generation = GD->generation;
#endif
		  goto retry_continue;
	      }

	      *valTermRef(LD->exception.pending) = 0;
	    }

	    discardChoicesAfter(FR PASS_LD);
	    discardFrame(FR, FINISH_EXCEPT);
	  }
	} else
#endif /*O_DEBUGGER*/
	{ for( ; FR && FR > catchfr; FR = FR->parent )
	  { SECURE(checkData(catcher));
	    discardChoicesAfter(FR PASS_LD);
	    discardFrame(FR, FINISH_EXCEPT);
	  }
	}


	if ( catchfr )
	{ static code exit_instruction;		/* may be gone otherwise */
	  Word p = argFrameP(FR, 1);
	  Choice ch = (Choice)argFrameP(FR, 3); /* Aligned above */

	  assert(ch->type == CHP_CATCH);

	  deRef(p);

	  assert(catchfr == FR);
	  discardChoicesAfter(FR PASS_LD);
	  environment_frame = FR;
	  undo_while_saving_term(&ch->mark, catcher);
	  unify_ptrs(p, catcher);	/* undo_while_saving_term() also */
					/* undoes unify of findCatcher() */
	  lTop = (LocalFrame) argFrameP(FR, 3); /* above the catch/3 */
	  if ( LD->trim_stack_requested )
	    trimStacks();
	  argFrame(lTop, 0) = argFrame(FR, 2);  /* copy recover goal */
	  *valTermRef(exception_printed) = 0;   /* consider it handled */
	  *valTermRef(exception_bin)     = 0;
	  exception_term		 = 0;

	  exit_instruction = encode(I_EXIT);    /* we must continue with */
	  PC = &exit_instruction;		/* an I_EXIT. Use catch? */

	  goto i_usercall0;
	} else
	{ Word p;

	  QF = QueryFromQid(qid);	/* may be shifted: recompute */
	  set(QF, PL_Q_DETERMINISTIC);
	  FR = environment_frame = &QF->frame;
	  lTop = (LocalFrame) argFrameP(FR, FR->predicate->functor->arity);

					/* needs a foreign frame? */
	  QF->exception = PL_new_term_ref();
	  p = valTermRef(QF->exception);
	  *p = except;
	  deRef(p);

	  undo_while_saving_term(&QF->choice.mark, p);
	  if ( false(QF, PL_Q_PASS_EXCEPTION) )
	  { *valTermRef(exception_bin)     = 0;
	    exception_term		   = 0;
	    *valTermRef(exception_printed) = 0; /* consider it handled */
	  } else
	  { *valTermRef(exception_bin)     = *p;
	    exception_term		   = exception_bin;
	  }

	  if ( LD->trim_stack_requested )
	    trimStacks();

	  fail;
	}
      }
#endif /*O_CATCHTHROW*/

#if O_BLOCK
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
exit(Block, RVal).  First does !(Block).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_EXIT) MARK(B_EXIT);
      { Word name, rval;
	LocalFrame blockfr;

	name = argFrameP(lTop, 0); deRef(name);
	rval = argFrameP(lTop, 1); deRef(rval);

        if ( !(blockfr = findBlock(FR, name)) )
	{ if ( exception_term )
	    goto b_throw;

	  BODY_FAILED;
	}
	
	if ( unify(argFrameP(blockfr, 2), rval PASS_LD) )
	{ for( ; ; FR = FR->parent )
	  { SECURE(assert(FR > blockfr));
	    discardChoicesAfter(FR PASS_LD);
	    discardFrame(FR, FINISH_CUT);
	    if ( FR->parent == blockfr )
	    { PC = FR->programPointer;
	      break;
	    }
	  }
					/* TBD: tracing? */
          environment_frame = FR = blockfr;
	  discardChoicesAfter(FR PASS_LD); /* delete possible CHP_DEBUG */
	  DEF = FR->predicate;
	  lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	  ARGP = argFrameP(lTop, 0);

	  NEXT_INSTRUCTION;
	} else
	{ lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	  ARGP = argFrameP(lTop, 0);

	  BODY_FAILED;
	}
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!(Block).  Cuts all alternatives created after entering the named block.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(I_CUT_BLOCK) MARK(CUT_BLOCK);
      { LocalFrame cutfr;
        Choice ch;
        Word name;

        name = argFrameP(lTop, 0); deRef(name);

        if ( !(cutfr = findBlock(FR, name)) )
	{ if ( exception_term )
            goto b_throw;
	  BODY_FAILED;
	}

	for(ch=BFR; (void *)ch > (void *)cutfr; ch = ch->parent)
	{ LocalFrame fr2;

          DEBUG(3, Sdprintf("Discarding %s\n", chp_chars(ch)));
          for(fr2 = ch->frame;
              fr2 && fr2->clause && fr2 > FR;
	      fr2 = fr2->parent)
	      discardFrame(fr2, FINISH_CUT);
	}
        BFR = ch;

	lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	ARGP = argFrameP(lTop, 0);

	NEXT_INSTRUCTION;
      }
#endif /*O_BLOCK*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!. Task is to detroy all choicepoints   newer then the current frame. If
we are in debug-mode we create a   new CHP_DEBUG frame to provide proper
debugger output.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    /*i_cut:*/			/* from I_USERCALL0 */
    VMI(I_CUT)						MARK(CUT);
      { 
#ifdef O_DEBUGGER
	if ( debugstatus.debugging )
	{ Choice ch;

	  switch(tracePort(FR, BFR, CUT_CALL_PORT, PC))
	  { case ACTION_RETRY:
	      goto retry;
	    case ACTION_FAIL:
	      FRAME_FAILED;
	  }

	  discardChoicesAfter(FR PASS_LD);
	  lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	  ch = newChoice(CHP_DEBUG, FR PASS_LD);
	  ARGP = argFrameP(lTop, 0);

	  switch(tracePort(FR, BFR, CUT_EXIT_PORT, PC))
	  { case ACTION_RETRY:
	      goto retry;
	    case ACTION_FAIL:
	      FRAME_FAILED;
	  }
	} else
#endif
	{ discardChoicesAfter(FR PASS_LD);
	  lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	  ARGP = argFrameP(lTop, 0);
	}

	NEXT_INSTRUCTION;
      }

#if O_COMPILE_OR
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
WAM support for ``A ; B'', ``A -> B'' and ``A -> B ; C'' constructs.  As
these functions introduce control within the WAM instructions  they  are
tagged `C_'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_JMP skips the amount stated in the pointed argument.   The  PC++
could be compiled out, but this is a bit more neath.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_JMP) MARK(C_JMP);
      { PC += *PC;
	PC++;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_MARK saves the value of BFR  (current   backtrack  frame) into a local
frame slot reserved by the compiler.  Note that the variable to hold the
local-frame pointer is  *not*  reserved   in  clause->variables,  so the
garbage collector won't see it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   VMI(C_MARK) MARK(C_MARK);
      { varFrame(FR, *PC++) = (word) BFR;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_VAR is generated by the compiler to ensure the  instantiation  pattern
of  the  variables  is  the  same after finishing both paths of the `or'
wired in the clause.  Its task is to make the n-th variable slot of  the
current frame to be a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   VMI(C_VAR) MARK(C_VAR);
      { setVar(varFrame(FR, *PC++));

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_CUT will  destroy  all  backtrack  points  created  after  the  C_MARK
instruction in this clause.  It assumes the value of BFR has been stored
in the nth-variable slot of the current local frame.

We can dereference all frames that are older that the old backtrackframe
and older than this frame.

All frames created since what becomes now the  backtrack  point  can  be
discarded.

C_LCUT  results  from  !'s  encountered  in    the   condition  part  of
if->then;else and \+ (which  is  (g->fail;true)).   It  should  cut  all
choices created since the mark, but not   the mark itself. The test-case
is  a  :-  \+  (b,  !,  fail),    which   should  succeed.  The  current
implementation  walks  twice  over  the    choice-points,  but  cuts  in
conditions should be rare (I hope :-).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
      { Choice och;
	LocalFrame fr;
	Choice ch;
    VMI(C_LCUT)						MARK(C_LCUT);
	och = (Choice) varFrame(FR, *PC);
	PC++;

	for(ch=BFR; ch; ch = ch->parent)
	{ if ( ch->parent == och )
	  { och = ch;
	    goto c_cut;
	  }
	}
	assert(BFR == och);		/* no choicepoint yet */
	NEXT_INSTRUCTION;
    VMI(C_CUT) 						MARK(C_CUT);
	och = (Choice) varFrame(FR, *PC);
	PC++;				/* cannot be in macro! */
      c_cut:
	if ( !och || FR > och->frame )	/* most recent frame to keep */
	  fr = FR;
	else
	  fr = och->frame;

	for(ch=BFR; ch && ch > och; ch = ch->parent)
	{ LocalFrame fr2;

	  DEBUG(3, Sdprintf("Discarding %s\n", chp_chars(ch)));
	  for(fr2 = ch->frame;    
	      fr2 && fr2->clause && fr2 > fr;
	      fr2 = fr2->parent)
	    discardFrame(fr2, FINISH_CUT);
	}
	assert(och == ch);
	BFR = och;

	if ( (void *)och > (void *)fr )
	{ lTop = addPointer(och, sizeof(*och));
	} else
	{ int nvar = (true(fr->predicate, FOREIGN)
				? fr->predicate->functor->arity
				: fr->clause->clause->variables);
	  lTop = (LocalFrame) argFrameP(fr, nvar);
	}

	ARGP = argFrameP(lTop, 0);

	DEBUG(3, Sdprintf(" --> BFR = #%ld, lTop = #%ld\n",
			  loffset(BFR), loffset(lTop)));
        NEXT_INSTRUCTION;
      }

#ifdef O_SOFTCUT
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handle the commit-to of A *-> B; C.  Simply mark the $alt/1 frame as cutted,
and control will not reach C again.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_SOFTCUT) MARK(CSOFTCUT);
      { Choice ch = (Choice) varFrame(FR, *PC);

	PC++;
	ch->type = CHP_NONE;
	NEXT_INSTRUCTION;
      }
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_END is a dummy instruction to help the decompiler to find the end of A
->  B.  (Note  that  a  :-  (b  ->  c),  d == a :- (b -> c, d) as far as
semantics.  They are different terms however.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   VMI(C_END) MARK(C_END);
      {	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_FAIL is equivalent to fail/0. Used to implement \+/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   VMI(C_FAIL) MARK(C_FAIL);
      {	BODY_FAILED;
      }
#endif /* O_COMPILE_OR */

#if O_COMPILE_ARITH
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Arithmic is compiled using a  stack  machine.    ARGP  is  used as stack
pointer and the arithmic stack is allocated   on top of the local stack,
starting at the argument field of the next slot of the stack (where ARGP
points to when processing the body anyway).

Arguments to functions are pushed on the stack  starting  at  the  left,
thus `add1(X, Y) :- Y is X + 1' translates to:

    I_ENTER	% enter body
    B_VAR1	% push Y via ARGP
    A_ENTER	% align the stack to prepare for writing doubles
    A_VAR0	% evaluate X and push numeric result
    A_INTEGER 1	% Push 1 as numeric value
    A_FUNC2 0	% Add top-two of the stack and push result
    A_IS 	% unify Y with numeric result
    I_EXIT	% leave the clause

a_func0:	% executes arithmic function without arguments, pushing
		% its value on the stack
a_func1:	% unary function. Changes the top of the stack.
a_func2:	% binary function. Pops two values and pushes one.

Note that we do not call `ar_func0(*PC++, &ARGP)' as ARGP is a register
variable.  Also, for compilers that do register allocation it is unwise
to give the compiler a hint to put ARGP not into a register.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_ENTER) MARK(AENTER)
      { 
#ifdef DOUBLE_ALIGNMENT
	ARGP = (Word) (((unsigned long)ARGP + (DOUBLE_ALIGNMENT-1)) &
		       ~(DOUBLE_ALIGNMENT-1));
#endif
        NEXT_INSTRUCTION;
      }

    VMI(A_INTEGER) MARK(AINT);
      {	Number n = (Number)ARGP;

	n->value.i = (long) *PC++;
	n->type    = V_INTEGER;
	ARGP       = (Word)(n+1);
	NEXT_INSTRUCTION;
      }

    VMI(A_DOUBLE) MARK(ADOUBLE);
      {	Number n = (Number)ARGP;
	Word p = &n->value.w[0];

	cpDoubleData(p, PC);
	n->type       = V_REAL;
	ARGP          = (Word)(n+1);
	NEXT_INSTRUCTION;
      }

    VMI(A_VAR) MARK(AVARN);
    { int offset;
      term_t v;
      Number n;
      offset = *PC++;

    a_var_n:
      v = consTermRef(varFrameP(FR, offset));
      n = (Number)ARGP;

      if ( valueExpression(v, n PASS_LD) )
      { ARGP = (Word)(n+1);
	NEXT_INSTRUCTION;
      } else
      {
#if O_CATCHTHROW
	if ( exception_term )
	  goto b_throw;
#endif
	BODY_FAILED;			/* check this */
      }

    VMI(A_VAR0) MARK(AVAR0);
      offset = ARGOFFSET / sizeof(word);
      goto a_var_n;
    VMI(A_VAR1) MARK(AVAR1);
      offset = ARGOFFSET / sizeof(word) + 1;
      goto a_var_n;
    VMI(A_VAR2) MARK(AVAR2);
      offset = ARGOFFSET / sizeof(word) + 2;
      goto a_var_n;
    }

  { int an;
    code fn;

    VMI(A_FUNC0) MARK(A_FUNC0);
      {	an = 0;
	fn = *PC++;
	goto common_an;
      }

    VMI(A_FUNC1) MARK(A_FUNC1);
      {	an = 1;
	fn = *PC++;
	goto common_an;
      }

    VMI(A_FUNC2) MARK(A_FUNC2);
      {	an = 2;
	fn = *PC++;
	goto common_an;
      }

    VMI(A_FUNC) MARK(A_FUNC);
      {	Number n;

	fn = *PC++;
	an = (int) *PC++;

      common_an:
	n = (Number) ARGP;

	if ( !ar_func_n(fn, an, &n) )
	{ if ( exception_term )
	    goto b_throw;
	  BODY_FAILED;
	}

	ARGP = (Word) n;
	NEXT_INSTRUCTION;
      }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of the arithmic comparison predicates (<, >, =<,  >=,  =:=).
Both sides are pushed on the stack, so we just compare the two values on
the  top  of  this  stack  and  backtrack  if  they  do  not suffice the
condition.  Example translation: `a(Y) :- b(X), X > Y'

    ENTER
    B_FIRSTVAR 1	% Link X from B's frame to a new var in A's frame
    CALL 0		% call b/1
    A_VAR 1		% Push X
    A_VAR 0		% Push Y
    A_GT		% compare
    EXIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_LT) MARK(A_LT);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, LT) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_LE) MARK(A_LE);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, LE) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_GT) MARK(A_GT);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, GT) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_GE) MARK(A_GE);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, GE) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_EQ) MARK(A_EQ);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, EQ) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_NE) MARK(A_NE);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, NE) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of is/2.  The stack has two pushed values: the variable for
the result (a word) and the number holding the result.  For example:

	 a(X) :- X is sin(3).

	I_ENTER
	B_VAR 0			push left argument of is/2
	A_INTEGER 3		push integer as number
	A_FUNC <sin>		run function on it
	A_IS			bind value
	I_EXIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_IS) MARK(A_IS);
      { Number n = (Number)ARGP;
	Word k;

	n--;				/* pop the number */
	ARGP = argFrameP(lTop, 0);	/* 1-st argument */
	deRef2(ARGP, k);
	canoniseNumber(n);		/* whole real --> long */

	if ( isVar(*k) )
	{ Trail(k);
	  if ( intNumber(n) )
	  { if ( inTaggedNumRange(n->value.i) )
	      *k = consInt(n->value.i);
	    else
	      *k = globalLong(n->value.i);
	  } else
	    *k = globalReal(n->value.f);
	  NEXT_INSTRUCTION;
	} else
	{ if ( isInteger(*k) && intNumber(n) && valInteger(*k) == n->value.i )
	    NEXT_INSTRUCTION;
	  if ( isReal(*k) && floatNumber(n) && valReal(*k) == n->value.f )
	    NEXT_INSTRUCTION;
	}

	BODY_FAILED;
      }
#endif /* O_COMPILE_ARITH */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_USERCALL0 is generated by the compiler if a variable is encountered as
a subclause. Note that the compount   statement  opened here is encloses
also I_APPLY and I_CALL. This allows us to use local register variables,
but still jump to the `normal_call' label to   do the common part of all
these three virtual machine instructions.

I_USERCALL0 has the task of  analysing  the   goal:  it  should fill the
->procedure slot of the new frame and  save the current program counter.
It also is responsible of filling the   argument part of the environment
frame with the arguments of the term.

BUG: have to find out how to proceed in case of failure (I am afraid the
`goto frame_failed' is a bit dangerous here).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#if O_CATCHTHROW
    i_usercall0:			/* from B_THROW */
#endif
    VMI(I_USERCALL0) MARK(USRCL0);
      { word goal;
	int arity;
	Word args, a;
	int n;
	LocalFrame next;
	Module module;
	functor_t functor;
	int callargs;

	next = lTop;
	a = argFrameP(next, 0);		/* get the (now) instantiated */
	deRef(a);			/* variable */

	module = NULL;
	a = stripModule(a, &module);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the functor definition associated with the goal as well as the
arity and a pointer to the argument vector of the goal.

If the goal is not a  simple  goal,   the  body  is  compiled to `local'
clause. The compilation mode is presented to compileClause() by the NULL
`head'. This compilation-mode  constructs  a   stack-frame  whose  first
argument is the  goal-term,  followed   by  large  structures (compound,
string) from the arguments, followed  by   the  normal  local variables,
followed by the VM codes and, the   clause structure and finally a dummy
list for the clause-chain  (ClauseRef)  used   for  the  frames ->clause
field.

The clause data is discarded automatically  if the frame is invalidated.
Note that compilation does not give contained   atoms a reference as the
atom is referenced by the goal-term anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	if ( isAtom(goal = *a) )
	{ /*if ( *a == ATOM_cut )		NOT ISO
	    goto i_cut; */
	  functor = lookupFunctorDef(goal, 0);
	  arity   = 0;
	  args    = NULL;
	} else if ( isTerm(goal) )
	{ if ( isSimpleGoal(a PASS_LD)
#if O_DEBUG
	       || GD->bootsession || !GD->initialised
#endif
	     )
	  { args    = argTermP(goal, 0);
	    functor = functorTerm(goal);
	    arity   = arityFunctor(functor);
	  } else
	  { Clause cl;
	    
	    a = &goal;			/* we're going to overwrite */
	    deRef(a);
	    DEBUG(1, { term_t g = a - (Word)lBase;
		       LocalFrame ot = lTop;
		       lTop += 100;
		       pl_write(g); pl_nl();
		       lTop = ot;
		     });
	    lTop = next;
	    if ( !(cl = compileClause(NULL, a, PROCEDURE_dcall1, module)) )
	      goto b_throw;

	    DEF			 = next->predicate;
	    SECURE(assert(DEF == PROCEDURE_dcall1->definition));
	    next->flags	         = FR->flags;
	    next->parent	 = FR;
	    next->programPointer = PC;
#ifdef O_LOGICAL_UPDATE
	    cl->generation.erased = ~0L;
	    cl->generation.created = next->generation = GD->generation;
#endif
	    incLevel(next);
	    PC = cl->codes;
  
	    enterDefinition(DEF);
	    environment_frame = FR = next;
	    ARGP = argFrameP(lTop, 0);

	    NEXT_INSTRUCTION;
	  }
	} else
	{ PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, wordToTermRef(a));
	  goto b_throw;
	}
	goto i_usercall_common;

    VMI(I_USERCALLN) MARK(USRCLN);
        callargs = *PC++;
	next = lTop;
	a = argFrameP(next, 0);		/* get the (now) instantiated */
	deRef(a);			/* variable */

	module = NULL;
	a = stripModule(a, &module);

	if ( isAtom(goal = *a) )
	{ arity   = 0;
	  functor = lookupFunctorDef(goal, callargs);
	  args    = NULL;
	} else if ( isTerm(goal) )
	{ FunctorDef fdef = valueFunctor(functorTerm(goal));

	  arity   = fdef->arity;
	  functor = lookupFunctorDef(fdef->name, arity + callargs);
	  args    = argTermP(goal, 0);
	} else
	{ PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, wordToTermRef(a));
	  goto b_throw;
	}

	if ( arity != 1 )
	{ int i, shift = arity - 1;

	  a = argFrameP(next, 1);	/* pointer to 1-st arg */
	  
	  if ( shift > 0 )
	  { for(i=callargs-1; i>=0; i--)
	    { if ( isRef(a[i]) )
	      { Word a1 = unRef(a[i]);
	    
		if ( a1 >= a && a1 < a+arity )
		  a[i+shift] = makeRef(a1+shift);
		else
		  a[i+shift] = a[i];
	      } else
		a[i+shift] = a[i];
	    }
	  } else
	  { for(i=0; i < callargs; i++)
	    { if ( isRef(a[i]) )
	      { Word a1 = unRef(a[i]);
		
		if ( a1 >= a && a1 < a+arity )
		  a[i+shift] = makeRef(a1+shift);
		else
		  a[i+shift] = a[i];
	      } else
		a[i+shift] = a[i];
	    }
	  }
	}

    i_usercall_common:
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) )
	  set(next, FR_NODEBUG);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now scan the argument vector of the goal and fill the arguments  of  the
frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	if ( arity > 0 )
	{ ARGP = argFrameP(next, 0);

	  for(; arity-- > 0; ARGP++, args++)
	    *ARGP = linkVal(args);
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the associated procedure.  First look in the specified module.   If
the function is not there then look in the user module.  Finally specify
the context module environment for the goal. This is not necessary if it
will  be  specified  correctly  by  the goal started.  Otherwise tag the
frame and write  the  module  name  just  below  the  frame.   See  also
contextModule().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	DEF = resolveProcedure(functor, module)->definition;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Save the program counter (note  that   I_USERCALL0  has no argument) and
continue as with a normal call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	next->context = module;
	goto normal_call;
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fast control functions. Should  set-up  normal   call  if  the  function
doesn't exist.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_FAIL) MARK(I_FAIL);
#ifdef O_DEBUGGER
      if ( debugstatus.debugging )
      { next = lTop;
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) ) /* parent has hide_childs */
	  set(next, FR_NODEBUG);
	DEF = lookupProcedure(FUNCTOR_fail0, MODULE_system)->definition;
	next->context = FR->context;

	goto normal_call;
      }
#endif
      BODY_FAILED;

    VMI(I_TRUE) MARK(I_TRUE);
#ifdef O_DEBUGGER
      if ( debugstatus.debugging )
      { next = lTop;
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) ) /* parent has hide_childs */
	  set(next, FR_NODEBUG);
	DEF = lookupProcedure(FUNCTOR_true0, MODULE_system)->definition;
	next->context = FR->context;

	goto normal_call;
      }
#endif
      NEXT_INSTRUCTION;

#if O_COMPILE_OR
#ifdef O_SOFTCUT
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A *-> B ; C is translated to C_SOFIF <A> C_SOFTCUT <B> C_JMP end <C>.  See
pl-comp.c and C_SOFTCUT implementation for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_SOFTIF) MARK(C_SOFTIF);
      { varFrame(FR, *PC++) = (word) lTop; /* see C_SOFTCUT */

	goto c_or;
      }

#endif
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If-then-else is a contraction of C_MARK and C_OR.  This contraction  has
been  made  to help the decompiler distinguis between (a ; b) -> c and a
-> b ; c, which would otherwise only be  possible  to  distinguis  using
look-ahead.

The asm("nop") is a tricky. The problem   is that C_NOT and C_IFTHENELSE
are the same instructions. The one is generated on \+/1 and the other on
(Cond -> True ; False). Their different   virtual-machine  id is used by
the decompiler. Now, as the VMCODE_IS_ADDRESS   is  in effect, these two
instruction would become the same. The  asm("nop") ensures they have the
same *functionality*, but a *different* address.  If your machine does't
like nop, define the macro ASM_NOP in  your md-file to do something that
1) has *no effect* and 2) is *not optimised* away by the compiler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_NOT)						MARK(C_NOT)
#if VMCODE_IS_ADDRESS
#ifdef ASM_NOP
      ASM_NOP;
#else
      asm("nop");
#endif
#endif
    VMI(C_IFTHENELSE)
      MARK(C_ITE);
      { varFrame(FR, *PC++) = (word) BFR; /* == C_MARK */

	/*FALL-THROUGH to C_OR*/
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_OR introduces a backtrack point within the clause.   The  argument  is
how  many  entries  of  the  code  array  to skip should backtracking be
necessary.  It is implemented by calling a foreign  functions  predicate
with as argument the amount of bytes to skip.  The foreign function will
on  first  call  succeed,  leaving  a  backtrack  point.   It does so by
returning the amount to skip as backtracking  argument.   On  return  it
will increment PC in its frame with this amount (which will be popped on
its exit) and succeed deterministically.

Note that this one is enclosed in the compound statement of I_USERCALL0,
I_APPLY, I_CALL and I_DEPART to allow   sharing of the register variable
`next' with them and thus make the `goto common_call' valid.

NOTE: as of SWI-Prolog 2.0.2, the  call   to  $alt/1  is `inlined'. As a
consequence it has lost its argument and   is  now $alt/0. We just build
the frame for $alt/1 and then  continue   execution.  This  is ok as the
first call of $alt/1 simply succeeds.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_OR) MARK(C_OR);
    c_or:
      { int skip = *PC++;
	Choice ch = newChoice(CHP_JUMP, FR PASS_LD);
	ch->value.PC = PC+skip;
	ARGP = argFrameP(lTop, 0);

	NEXT_INSTRUCTION;
      }
#endif /* O_COMPILE_OR */

#ifdef O_INLINE_FOREIGNS
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALL_FV[012] Call a deterministic foreign procedures with a 0, 1, or 2
arguments that appear as variables  in   the  clause.  This covers true,
fail, var(X) and other type-checking  predicates,   =/2  in  a number of
cases (i.e. X = Y, not X = 5).

The VMI for these calls are ICALL_FVN, proc, var-index ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    { int nvars;
      Procedure fproc;
      Word v;

      VMI(I_CALL_FV0) MARK(CFV0);
      { fproc = (Procedure) *PC++;
	nvars = 0;

	goto common_call_fv;
      }

      VMI(I_CALL_FV1) MARK(CFV1);
      { fproc = (Procedure) *PC++;
	nvars = 1;
	v = varFrameP(FR, *PC++);
	*ARGP++ = (isVar(*v) ? makeRefL(v) : *v);
	goto common_call_fv;
      }

      VMI(I_CALL_FV2) MARK(CFV2);
      { fproc = (Procedure) *PC++;
	nvars = 2;
	v = varFrameP(FR, *PC++);
	*ARGP++ = (isVar(*v) ? makeRefL(v) : *v);
	v = varFrameP(FR, *PC++);
	*ARGP++ = (isVar(*v) ? makeRefL(v) : *v);

      common_call_fv:
	{ Definition def = fproc->definition;
	  Func f = def->definition.function;
	  int rval;

	  if ( !f )
	  { def = trapUndefined(def);
	    f = def->definition.function;
	  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If we are debugging, just build a normal  frame and do the normal thing,
so the inline call is expanded to a normal call and may be traced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	  if ( !f ||
#ifdef O_DEBUGGER
	       debugstatus.debugging ||
#endif
	       false(def, FOREIGN) )
	  { next = lTop;
	    next->flags = FR->flags;
	    if ( true(DEF, HIDE_CHILDS) ) /* parent has hide_childs */
	      set(next, FR_NODEBUG);
	    DEF = def;
	    next->context = FR->context;

	    goto normal_call;
	  } else
	  { LocalFrame oldtop = lTop;
	    term_t h0;
	    fid_t fid;
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We must create a frame and mark the  stacks for two reasons: undo if the
foreign call fails *AND*  make  sure   Trail()  functions  properly.  We
increase lTop too to prepare for asynchronous interrupts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	    LD->statistics.inferences++;
	    next = lTop;
	    h0 = argFrameP(next, 0) - (Word)lBase;
	    lTop = (LocalFrame) argFrameP(next, nvars);
	    if ( true(def, METAPRED) )
	      next->context = FR->context;
	    else
	      next->context = def->module;
	    next->predicate      = def;
	    next->programPointer = PC;
	    next->parent         = FR;
	    next->flags		 = FR->flags;
  	    next->clause	 = NULL; /* for handling exceptions */
#ifdef O_LOGICAL_UPDATE
	    next->generation     = GD->generation;
#endif
	    incLevel(next);
	    Profile(def->profile_calls++);
	    environment_frame = next;

	    exception_term = 0;
	    SAVE_REGISTERS(qid);
	    fid = PL_open_foreign_frame();
	    if ( is_signalled() )
	    { PL_handle_signals();
	      if ( exception_term )
		goto b_throw;
	    }
	    switch(nvars)
	    { case 0:
		rval = (*f)();
	        break;
	      case 1:
		rval = (*f)(h0);
	        break;
	      case 2:
	      default:
		rval = (*f)(h0, h0+1);
	        break;
	    }
	    PL_close_foreign_frame(fid);
	    LOAD_REGISTERS(qid);

	    ARGP -= nvars;
	    environment_frame = FR;
	    lTop = oldtop;

	    if ( exception_term )
	    { if ( rval )
	      { exception_term = 0;
		setVar(*valTermRef(exception_bin));
	      } else
		goto b_throw;
	    }

	    if ( rval )
	    { assert(rval == TRUE);
	      NEXT_INSTRUCTION;
	    }

	    LD->statistics.inferences++;	/* is a redo! */
	    BODY_FAILED;
	  }
	}
      }
    }
#endif /*O_INLINE_FOREIGNS*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_APPLY is the code generated by the Prolog goal $apply/2 (see reference
manual for the definition of apply/2).  We   expect  a term in the first
argument of the frame and a  list   in  the second, comtaining aditional
arguments. Most comments of I_USERCALL0 apply   to I_APPLY as well. Note
that the two arguments are copied in  local variables as they will later
be overwritten by the arguments for the actual call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
      VMI(I_APPLY) MARK(APPLY);
      { atom_t functor;
	Word lp;
	Module module = (Module) NULL;
	Word gp;
	word a1 = 0;

	next = lTop;
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) )
	  set(next, FR_NODEBUG);

	ARGP = argFrameP(next, 0); deRef(ARGP); gp = ARGP;
	ARGP = argFrameP(next, 1); deRef(ARGP); lp = ARGP;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Obtain the functor of the actual goal from the first argument  and  copy
the arguments of this term in the frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	
	gp = stripModule(gp, &module);
	next->context = module;
	goal = *gp;

	ARGP = argFrameP(next, 0);

	if ( isAtom(goal) )
	{ functor = goal;
	  arity = 0;
	} else if ( isTerm(goal) )
	{ Functor     f = valueTerm(goal);
	  FunctorDef fd = valueFunctor(f->definition);

	  functor = fd->name;
	  arity   = fd->arity;
	  args    = f->arguments;
	  for(n=0; n<arity; n++, ARGP++, args++)
	  { if ( n == 1 )
	      a1 = linkVal(args);
	    else
	      *ARGP = linkVal(args);
	  }
	} else
	{ PL_error("apply", 2, NULL, ERR_TYPE,
		   ATOM_callable, wordToTermRef(a));
	  goto b_throw;
	}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the list and add the elements to the argument vector of the frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	for( ;isList(*lp); ARGP++ )
	{ args = argTermP(*lp, 0);	/* i.e. the head */
	  if ( arity++ == 1 )
	    a1 = linkVal(args);
	  else
	    *ARGP = linkVal(args);
	  if ( arity > MAXARITY )
	  { PL_error("apply", 2, NULL, ERR_REPRESENTATION, ATOM_max_arity);
	    goto b_throw;
	  }
	  lp = argTermP(*lp, 1);	/* i.e. the tail */
	  deRef(lp);
	}
	if ( !isNil(*lp) )
	{ PL_error("apply", 2, NULL, ERR_TYPE,
		   ATOM_list, wordToTermRef(lp));
	  goto b_throw;
	}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Finally do the delayed assignment of a1 (delayed to avoid overwriting the
argument list).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	if ( a1 )
	  argFrame(next, 1) = a1;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the associated procedure (see I_CALL for module handling), save the
program pointer and jump to the common part.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	{ functor_t fdef;

	  fdef = lookupFunctorDef(functor, arity);
	  DEF = resolveProcedure(fdef, module)->definition;
	  next->context = module;
	}

	goto normal_call;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALL and I_DEPART form the normal code generated by the  compiler  for
calling  predicates.   The  arguments  are  already written in the frame
starting at `lTop'.  I_DEPART implies it is the last  subclause  of  the
clause.  This is be the entry point for tail recursion optimisation.

The task of I_CALL is to  save  necessary  information  in  the  current
frame,  fill  the next frame and initialise the machine registers.  Then
execution can continue at `next_instruction'
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#define TAILRECURSION 1
      VMI(I_DEPART)
							 MARK(DEPART);
#if TAILRECURSION
	if ( (void *)BFR <= (void *)FR
#if O_DEBUGGER
	     && trueFeature(TAILRECURSION_FEATURE)
#endif
	   )
	{ Definition ndef = ((Procedure) *PC++)->definition;
	  arity = ndef->functor->arity;

	  if ( true(FR, FR_WATCHED) )
	  { LocalFrame lSave = lTop;
	    lTop = (LocalFrame)argFrameP(lTop, arity);
	    frameFinished(FR, FINISH_EXIT);
	    lTop = lSave;
	  }

	  if ( DEF )
	  { if ( true(DEF, HIDE_CHILDS) )
	      set(FR, FR_NODEBUG);
	    leaveDefinition(DEF);
	  }

	  copyFrameArguments(lTop, FR, arity PASS_LD);
	  FR->predicate = DEF = ndef;

	  goto depart_continue;
	}
#endif /*TAILRECURSION*/
	/*FALLTHROUGH*/
      VMI(I_CALL)					MARK(CALL);
        next = lTop;
        next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) ) /* parent has hide_childs */
	  set(next, FR_NODEBUG);
	DEF = ((Procedure) *PC++)->definition;
	next->context = FR->context;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the common part of the call variations.  By now the following is
true:

  - arguments, nodebug		filled
  - context			filled with context for
				transparent predicate
  - DEF				filled
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

      normal_call:
	requireStack(local, (int)argFrameP((LocalFrame)NULL, MAXARITY));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise those slots of the frame that are common to Prolog predicates
and foreign ones.  There might be some possibilities for optimisation by
delaying these initialisations till they are really  needed  or  because
the information they are calculated from is destroyed.  This probably is
not worthwile.

Note: we are working above `lTop' here!!   We restore this as quickly as
possible to be able to call-back to Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	next->parent         = FR;
	next->predicate	     = DEF;		/* TBD */
	next->programPointer = PC;		/* save PC in child */
	next->clause         = NULL;		/* for save atom-gc */
	environment_frame = FR = next;		/* open the frame */

      depart_continue:
	incLevel(FR);
#ifdef O_LOGICAL_UPDATE
	FR->generation     = GD->generation;
#endif
      retry_continue:
        lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);

#ifdef O_DEBUGLOCAL
      {	Word ap = argFrameP(FR, DEF->functor->arity);
	int n;
	
	for(n=50; --n; )
	  *ap++ = (word)(((char*)ATOM_nil) + 1);
      }
#endif

	clear(FR, FR_SKIPPED|FR_WATCHED|FR_CATCHED);

	LD->statistics.inferences++;

	if ( is_signalled() )
	{ PL_handle_signals();
	  if ( exception_term )
	    goto b_throw;
	}

#if O_ASYNC_HOOK			/* Asynchronous hooks */
	{ if ( async.hook &&
	       !((++LD->statistics.inferences & async.mask)) )
	    (*async.hook)();		/* check the hook */
	}
#endif

	Profile(DEF->profile_calls++);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Undefined predicate detection and handling.   trapUndefined() takes care
of linking from the public modules or calling the exception handler.

Note that DEF->definition is  a  union   of  the  clause  or C-function.
Testing is suffices to find out that the predicate is defined.

Logical-update: note that trapUndefined() may add  clauses and we should
be able to access these!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	if ( !DEF->definition.clauses && false(DEF, PROC_DEFINED) )	
	{ FR->predicate = DEF = trapUndefined(DEF);
#ifdef O_LOGICAL_UPDATE
	  FR->generation = GD->generation;
#endif

	  if ( !DEF->definition.clauses &&
	       false(DEF, PROC_DEFINED) &&
	       true(DEF->module, UNKNOWN_ERROR) )
	  { FR->clause = NULL;
	    enterDefinition(DEF);	/* will be left in exception code */
	    if ( exception_term )
	      goto b_throw;
	  }
	}

	if ( false(DEF, METAPRED) )
	  FR->context = DEF->module;
	if ( false(DEF, SYSTEM) )
	  clear(FR, FR_NODEBUG);

#if O_DYNAMIC_STACKS
	if ( gc_status.requested )
	{ garbageCollect(FR, BFR);
	}
#else /*O_DYNAMIC_STACKS*/
#if O_SHIFT_STACKS
      { int gshift = narrowStack(global);
	int lshift = narrowStack(local);
	int tshift = narrowStack(trail);

	if ( gshift || lshift || tshift )
	{ if ( gshift || tshift )
	  { long gused = usedStack(global);
	    long tused = usedStack(trail);

	    garbageCollect(FR, BFR);
	    DEBUG(1, Sdprintf("\tgshift = %d; tshift = %d", gshift, tshift));
	    if ( gshift )
	      gshift = ((2 * usedStack(global)) > gused);
	    if ( tshift )
	      tshift = ((2 * usedStack(trail)) > tused);
	    DEBUG(1, Sdprintf(" --> gshift = %d; tshift = %d\n",
			    gshift, tshift));
	  }

	  if ( gshift || tshift || lshift )
	  { SAVE_REGISTERS(qid);
	    growStacks(FR, BFR, NULL, lshift, gshift, tshift);
	    LOAD_REGISTERS(qid);
	  }
	}
      }
#else /*O_SHIFT_STACKS*/
	if ( narrowStack(global) || narrowStack(trail) )
	  garbageCollect(FR);
#endif /*O_SHIFT_STACKS*/
#endif /*O_DYNAMIC_STACKS*/

	if ( LD->outofstack )
	{ enterDefinition(DEF);		/* exception will lower! */
	  outOfStack(LD->outofstack, STACK_OVERFLOW_RAISE);
	  goto b_throw;
	}

	if ( true(DEF, FOREIGN) )
	{ int rval;

	  SAVE_REGISTERS(qid);
	  rval = callForeign(FR, FIRST_CALL PASS_LD);
	  LOAD_REGISTERS(qid);

	  if ( rval )
	    goto exit_builtin;

#if O_CATCHTHROW
	  if ( exception_term )
	  { goto b_throw;
	  }
#endif

	  goto frame_failed;
	} 

#if O_DEBUGGER
	if ( debugstatus.debugging )
	{ CL = DEF->definition.clauses;
	  switch(tracePort(FR, BFR, CALL_PORT, NULL))
	  { case ACTION_FAIL:	goto frame_failed;
	    case ACTION_IGNORE: goto exit_builtin;
	  }
	}
#endif /*O_DEBUGGER*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a normal Prolog predicate.  Just   load  the machine registers with
values found in the clause,  give  a   reference  to  the clause and set
`lTop' to point to the first location after the current frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	ARGP = argFrameP(FR, 0);
	enterDefinition(DEF);

#ifdef O_LIMIT_DEPTH
      { unsigned long depth = levelFrame(FR);

	if ( depth > depth_reached )
	  depth_reached = depth;
	if ( depth > depth_limit )
	   FRAME_FAILED;
      }
#endif
	DEBUG(9, Sdprintf("Searching clause ... "));

      { ClauseRef next;
	Clause clause;

	lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
	if ( !(CL = firstClause(ARGP, FR, DEF, &next PASS_LD)) )
	{ DEBUG(9, Sdprintf("No clause matching index.\n"));
	  if ( debugstatus.debugging )
	    newChoice(CHP_DEBUG, FR PASS_LD);

	  FRAME_FAILED;
	}
	DEBUG(9, Sdprintf("Clauses found.\n"));

	clause = CL->clause;
	PC = clause->codes;
	lTop = (LocalFrame)(ARGP + clause->variables);

	if ( next )
	{ Choice ch = newChoice(CHP_CLAUSE, FR PASS_LD);
	  ch->value.clause = next;
	} else if ( debugstatus.debugging )
	  newChoice(CHP_DEBUG, FR PASS_LD);

			/* require space for the args of the next frame */
	requireStack(local, (int)argFrameP((LocalFrame)NULL, MAXARITY));
      }

	SECURE(
	int argc; int n;
	argc = DEF->functor->arity;
	for(n=0; n<argc; n++)
	  checkData(argFrameP(FR, n));
	);

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Leave the clause:

  - update reference of current clause
    If there are no alternatives left and BFR  <=  frame  we  will
    never  return  at  this clause and can decrease the reference count.
    If BFR > frame the backtrack frame is a child of  this  frame, 
    so  this frame can become active again and we might need to continue
    this clause.

  - update BFR
    `BFR' will become the backtrack frame of other childs  of  the
    parent  frame  in which we are going to continue.  If this frame has
    alternatives and is newer than the old backFrame `BFR'  should
    become this frame.

    If there are no alternatives and  the  BFR  is  this  one  the
    BFR can become this frame's backtrackframe.

  - Update `lTop'.
    lTop can be set to this frame if there are no alternatives  in  this
    frame  and  BFR  is  older  than this frame (e.g. there are no
    frames with alternatives that are newer).

  - restore machine registers from parent frame
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
      {				MARK(I_EXIT);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
i_exitfact is generated to close a fact. The reason for not generating a
plain I_EXIT is first of all that the actual sequence should be I_ENTER,
I_EXIT,  and  just  optimising   to    I_EXIT   looses   the  unify-port
interception. Second, there should be some room for optimisation here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(I_EXITFACT) MARK(EXITFACT);
#if O_DEBUGGER
	if ( debugstatus.debugging )
	{ switch(tracePort(FR, BFR, UNIFY_PORT, PC))
	  { case ACTION_RETRY:
	      goto retry;
	  }
	}
#endif /*O_DEBUGGER*/
        /* FALLTHROUGH*/
    VMI(I_EXIT) MARK(EXIT);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First, call the tracer. Basically,  the   current  frame is garbage, but
given that the tracer might need to print the variables, we have to be a
bit more careful.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#if O_DEBUGGER
	if ( debugstatus.debugging )
        { int action = tracePort(FR, BFR, EXIT_PORT, PC);

	  switch( action )
	  { case ACTION_RETRY:
	      goto retry;
	    case ACTION_FAIL:
	      discardChoicesAfter(FR PASS_LD);
	      FRAME_FAILED;
	  }
	}
#endif /*O_DEBUGGER*/

    exit_builtin:			/* tracer already by callForeign() */
	if ( (void *)BFR <= (void *)FR ) /* deterministic */
	{ if ( false(DEF, FOREIGN) )
	    leaveDefinition(DEF);
	  lTop = FR;
	  DEBUG(3, Sdprintf("Deterministic exit of %s, lTop = #%ld\n",
			    predicateName(FR->predicate), loffset(lTop)));
	}

	if ( !FR->parent )		/* query exit */
	{ QF = QueryFromQid(qid);	/* may be shifted: recompute */
	  QF->solutions++;

	  assert(FR == &QF->frame);

	  if ( BFR == &QF->choice )	/* No alternatives */
	  { set(QF, PL_Q_DETERMINISTIC);
	    lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);

	    if ( true(FR, FR_WATCHED) )
	      frameFinished(FR, FINISH_EXIT);
	  }

	  succeed;
	}

      {
#if O_DEBUGGER
	LocalFrame leave;

	leave = (true(FR, FR_WATCHED) && FR == lTop) ? FR : NULL;
#endif

        SECURE(assert(onStackArea(local, FR->parent)));

	PC = FR->programPointer;
	environment_frame = FR = FR->parent;
	DEF = FR->predicate;
	ARGP = argFrameP(lTop, 0);

#if O_DEBUGGER
	if ( leave )
	  frameFinished(leave, FINISH_EXIT);
#endif
      }
	NEXT_INSTRUCTION;
      }	  
  }

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


body_failed:				MARK(BKTRK);
clause_failed:
frame_failed:

{
#ifdef O_DEBUGGER
  Choice ch0 = BFR;
#endif
  Choice ch;
  LocalFrame fr0;

  DEBUG(3, Sdprintf("BACKTRACKING\n"));

  if ( is_signalled() )
  { PL_handle_signals();
    if ( exception_term )
      goto b_throw;
  }

next_choice:
  ch = BFR;
  fr0 = FR;
					/* leave older frames */
#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { for(; (void *)FR > (void *)ch; FR = FR->parent)
    { if ( false(FR->predicate, FOREIGN) ) /* done by callForeign() */
      { Choice sch = findStartChoice(FR, ch0);

	if ( sch )
	{ Undo(sch->mark);

	  switch( tracePort(FR, BFR, FAIL_PORT, NULL) )
	  { case ACTION_RETRY:
	      environment_frame = FR;
	      DEF = FR->predicate;
#ifdef O_LOGICAL_UPDATE
	      if ( false(DEF, DYNAMIC) )
		FR->generation = GD->generation;
#endif
	      goto retry_continue;
	  }
	}
      }

      Profile(FR->predicate->profile_fails++);
      leaveFrame(FR);
    }
  } else
#endif /*O_DEBUGGER*/
  { for(; (void *)FR > (void *)ch; FR = FR->parent)
    { Profile(FR->predicate->profile_fails++);
      leaveFrame(FR);
    }
  }

  environment_frame = FR = ch->frame;
  Undo(ch->mark);
  aTop = aFloor;			/* reset to start, for interrupts */
  DEF  = FR->predicate;

  switch(ch->type)
  { case CHP_JUMP:
      DEBUG(3, Sdprintf("    REDO #%ld: Jump in %s\n",
			loffset(FR),
			predicateName(DEF)));
      PC   = ch->value.PC;
      BFR  = ch->parent;
      lTop = (LocalFrame)ch;
      ARGP = argFrameP(lTop, 0);
      NEXT_INSTRUCTION;
    case CHP_CLAUSE:
    { ClauseRef next;
      Clause clause;

      DEBUG(3, Sdprintf("    REDO #%ld: Clause in %s\n",
			loffset(FR),
			predicateName(DEF)));
      ARGP = argFrameP(FR, 0);
      BFR = ch->parent;
      if ( !(CL = findClause(ch->value.clause, ARGP, FR, DEF, &next)) )
	goto next_choice;		/* should not happen */

#ifdef O_DEBUGGER
      if ( debugstatus.debugging && fr0 != FR )
      { switch( tracePort(FR, BFR, REDO_PORT, NULL) )
	{ case ACTION_FAIL:
	    FRAME_FAILED;
	  case ACTION_IGNORE:
	    goto exit_builtin;
	  case ACTION_RETRY:
#ifdef O_LOGICAL_UPDATE
	    if ( false(DEF, DYNAMIC) )
	      FR->generation = GD->generation;
#endif
	    goto retry_continue;
	}
      }
#endif

      clause = CL->clause;
      PC     = clause->codes;
      lTop   = (LocalFrame)argFrameP(FR, clause->variables);

      if ( next )
      { ch = newChoice(CHP_CLAUSE, FR PASS_LD);
	ch->value.clause = next;
      } else if ( debugstatus.debugging )
      { ch = newChoice(CHP_DEBUG, FR PASS_LD);
      }

			/* require space for the args of the next frame */
      requireStack(local, (int)argFrameP((LocalFrame)NULL, MAXARITY));
      Profile(DEF->profile_redos++);
      NEXT_INSTRUCTION;
    }
    case CHP_FOREIGN:
    { int rval;

      DEBUG(3, Sdprintf("    REDO #%ld: Foreign %s, ctx = 0x%x\n",
			loffset(FR),
			predicateName(DEF),
		        ch->value.foreign));
      BFR  = ch->parent;
      lTop = (LocalFrame)ch;
      Profile(DEF->profile_redos++);

      SAVE_REGISTERS(qid);
      rval = callForeign(FR, ch->value.foreign PASS_LD);
      LOAD_REGISTERS(qid);

      if ( rval )
	goto exit_builtin;
      if ( exception_term )
	goto b_throw;

      FRAME_FAILED;
    }
    case CHP_TOP:			/* Query toplevel */
    { QF = QueryFromQid(qid);
      set(QF, PL_Q_DETERMINISTIC);
      fail;
    }
    case CHP_CATCH:			/* catch/3 */
    case CHP_DEBUG:			/* Just for debugging purposes */
    case CHP_NONE:			/* used for C_SOFTCUT */
      BFR  = ch->parent;
      goto next_choice;
  }
}
  assert(0);
  return FALSE;
} /* end of PL_next_solution() */






