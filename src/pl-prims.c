/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2024, University of Amsterdam
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
#include "pl-prims.h"
#include "pl-arith.h"
#include "pl-attvar.h"
#include "pl-copyterm.h"
#include "pl-fli.h"
#include "pl-gc.h"
#include "pl-wam.h"
#include "pl-write.h"
#include "pl-gvar.h"
#include "pl-read.h"
#include "pl-util.h"
#include "pl-funct.h"
#include "os/pl-ctype.h"
#include "os/pl-utf8.h"
#include "os/pl-cstack.h"
#include "pl-inline.h"
#include <math.h>

#undef LD
#define LD LOCAL_LD

#define unify_with_occurs_check(t1, t2, mode) LDFUNC(unify_with_occurs_check, t1, t2, mode)
static int	unify_with_occurs_check(DECL_LD Word t1, Word t2,
					occurs_check_t mode);


		 /*******************************
		 *	   CYCLIC TERMS		*
		 *******************************/

#if O_CYCLIC

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cyclic term unification. The algorithm has been  described to me by Bart
Demoen. Here it is (translated from dutch):

I created my own variation. You only need it during general unification.
Here is a short description:  suppose  you   unify  2  terms  f(...) and
f(...), which are represented on the heap (=global stack) as:

     +-----+          and     +-----+
     | f/3 |                  | f/3 |
     +-----+                  +-----+
      args                     args'

Before working on args and args', change  this into the structure below,
using a reference pointer pointing from functor  of the one to the other
term.

     +-----+          and      +-----+
     | ----+----------------->| f/3 |
     +-----+                  +-----+
      args                     args'

If, during this unification you  find  a   compound  whose  functor is a
reference to the term at the right hand you know you hit a cycle and the
terms are the same.

Of course functor_t must be different from ref. Overwritten functors are
collected in a stack and  reset   regardless  of whether the unification
succeeded or failed.

Note that we need to  dereference  the   functors  both  left and right.
References at the right are rare, but possible. The trick is to use both
sharing and cycles, where the cycles at the left are shorter:

t :-
	X = s(X),       Y = y(X,X),
	A = s(s(s(A))), B = y(A,A),
	Y = B.

While unifying the first argument of y/2, the left-walker crosses to the
right after the first cycle  and  creates   references  in  A, which are
processed by the right-walker when entering the second argument of y/2.

Initial measurements show a performance degradation for deep unification
of approx. 30%. On the other hand,  if subterms appear multiple times in
a term unification can be much faster. As only a small percentage of the
unifications of a realistic program are   covered by unify() and involve
deep unification the overall impact of performance is small (< 3%).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define initvisited(_) LDFUNC(initvisited, _)
static void
initvisited(DECL_LD)
{ LD->cycle.vstack.unit_size = sizeof(Word);
}


#ifdef O_DEBUG
#define empty_visited(_) LDFUNC(empty_visited, _)
static int
empty_visited(DECL_LD)
{ return emptySegStack(&LD->cycle.vstack);
}
#endif


#define visitedWord(p) LDFUNC(visitedWord, p)
static inline int
visitedWord(DECL_LD Word p)
{ if ( is_marked(p) )
    succeed;
  set_marked(p);
  if ( !pushSegStack(&LD->cycle.vstack, p, Word) )
    outOfCore();
  fail;
}


#define visited(f) LDFUNC(visited, f)
static inline int
visited(DECL_LD Functor f)
{ Word p = &f->definition;

  return visitedWord(p);
}


#define unvisit(_) LDFUNC(unvisit, _)
static void
unvisit(DECL_LD)
{ Word p;

  while( popSegStack(&LD->cycle.vstack, &p, Word) )
  { clear_marked(p);
  }
}

#define unvisit_and_unfirst(_) LDFUNC(unvisit_and_unfirst, _)
static void
unvisit_and_unfirst(DECL_LD)
{ Word p;

  while( popSegStack(&LD->cycle.vstack, &p, Word) )
  { clear_both(p);
  }
}


#define popVisited(_) LDFUNC(popVisited, _)
static void
popVisited(DECL_LD)
{ Word p = NULL;

  popSegStack(&LD->cycle.vstack, &p, Word);
  clear_marked(p);
}


#define initCyclic(_) LDFUNC(initCyclic, _)
static inline void
initCyclic(DECL_LD)
{ LD->cycle.lstack.unit_size = sizeof(Word);
}


#define linkTermsCyclic(f1, f2) LDFUNC(linkTermsCyclic, f1, f2)
static inline void
linkTermsCyclic(DECL_LD Functor f1, Functor f2)
{ Word p1 = (Word)&f1->definition;
  Word p2 = (Word)&f2->definition;

  *p1 = makeRefG(p2);
  if ( !pushSegStack(&LD->cycle.lstack, p1, Word) )
    outOfCore();
}


#define exitCyclic(_) LDFUNC(exitCyclic, _)
static inline void
exitCyclic(DECL_LD)
{ Word p;

  while( popSegStack(&LD->cycle.lstack, &p, Word) )
  { *p = *unRef(*p);
  }
}

#else /*O_CYCLIC*/

static inline visited(DECL_LD Functor f) { fail; }
static inline unvisit(DECL_LD Word *base) { }
static inline void initCyclic(DECL_LD) {}
static inline void exitCyclic(DECL_LD) {}
static inline void linkTermsCyclic(DECL_LD Functor f1, Functor f2) {}

#endif /*O_CYCLIC*/

#define HAVE_VISITED
#define AC_TERM_WALK_LR 1
#include "pl-termwalk.c"

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

Returns one of:

  - false:		terms cannot unify.  Note that this routine does not
			rollback changes it made!
  - true:		Unification has completed successfully
  - GLOBAL_OVERFLOW:	Unification cannot be completed due to lack
			of global-space.
  - TRAIL_OVERFLOW:	Unification cannot be completed due to lack
			of trail-space.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SWAPW(p,q) do { Word _tmp = p; p=q; q=_tmp; } while(0)
#define unify_simple_ptrs(t1, t2) LDFUNC(unify_simple_ptrs, t1, t2)

int
unify_simple_ptrs(DECL_LD Word t1, Word t2)
{ word w1, w2;

  if ( t1 == t2 )
    return true;

  w1 = *t1;
  w2 = *t2;

  DEBUG(CHK_ATOM_GARBAGE_COLLECTED,
	{ assert(w1 != ATOM_garbage_collected);
	  assert(w2 != ATOM_garbage_collected);
	});

  if ( isVar(w1) )
  { if ( unlikely(tTop+2 >= tMax) )
      return TRAIL_OVERFLOW;

    if ( isVar(w2) )
    { if ( t1 > t2 )
	SWAPW(t1, t2);
      if ( t1 < t2 )			/* always point downwards */
      { if ( t1 > (Word)lBase )
	{ Word v;

	  if ( unlikely(gTop+1 >= gMax) )
	    return GLOBAL_OVERFLOW;

	  v = gTop++;
	  setVar(*v);
	  Trail(t1, makeRefG(v));
	  Trail(t2, makeRefG(v));
	} else
	{ Trail(t2, makeRefG(t1));
	}
	return true;
      }
    }
#ifdef O_ATTVAR
    if ( isAttVar(w2 ) )
      w2 = makeRefG(t2);
#endif
    Trail(t1, w2);
    return true;
  }

  if ( isVar(w2) )
  { if ( unlikely(tTop+1 >= tMax) )
      return TRAIL_OVERFLOW;
#ifdef O_ATTVAR
    if ( isAttVar(w1) )
      w1 = makeRefG(t1);
#endif
    Trail(t2, w1);
    return true;
  }

#ifdef O_ATTVAR
  if ( isAttVar(w1) )
  { if ( !hasGlobalSpace(0) )
      return overflowCode(0);

    assignAttVar(t1, t2);
    return true;
  }
  if ( isAttVar(w2) )
  { if ( !hasGlobalSpace(0) )
      return overflowCode(0);

    assignAttVar(t2, t1);
    return true;
  }
#endif

  if ( w1 == w2 )
    return true;
  if ( tagex(w1) != tagex(w2) )
    return false;

  if ( isIndirect(w1) )
    return equalIndirect(w1, w2);

  if ( tag(w1) == TAG_COMPOUND )
    return DO_COMPOUND;

  return false;
}


#define do_unify(t1, t2) LDFUNC(do_unify, t1, t2)
int
do_unify(DECL_LD Word t1, Word t2)
{ deRef(t1);
  deRef(t2);

  int rc = unify_simple_ptrs(t1, t2);
  if ( rc >= 0 )
    return rc;
  if ( rc != DO_COMPOUND )
    return rc;
  rc = true;

  Functor f1 = valueTerm(*t1);
  Functor f2 = valueTerm(*t2);
  if ( f1->definition != f2->definition )
    return false;

  term_agendaLR agenda;
  size_t arity = arityFunctor(f1->definition);
  initTermAgendaLR(&agenda, arity, f1->arguments, f2->arguments);
  initCyclic();
  linkTermsCyclic(f1, f2);

  while( nextTermAgendaLR(&agenda, &t1, &t2) )
  { deRef(t1);
    deRef(t2);
    rc = unify_simple_ptrs(t1, t2);
    if ( rc == true )
      continue;
    if ( rc == false )
      break;
    if ( rc != DO_COMPOUND )
      break;
    rc = true;

    f1 = valueTerm(*t1);
    f2 = valueTerm(*t2);

#if O_CYCLIC
    while ( isRef(f1->definition) )
      f1 = (Functor)unRef(f1->definition);
    while ( isRef(f2->definition) )
      f2 = (Functor)unRef(f2->definition);
    if ( f1 == f2 )
      continue;
#endif

    if ( f1->definition != f2->definition )
    { rc = false;
      break;
    }

    size_t arity = arityFunctor(f1->definition);
    if ( !pushWorkAgendaLR(&agenda, arity, f1->arguments, f2->arguments) )
    { rc = MEMORY_OVERFLOW;
      break;
    }
    linkTermsCyclic(f1, f2);
  }

  clearTermAgendaLR(&agenda);
  exitCyclic();

  return rc;
}


#define raw_unify_ptrs(t1, t2) LDFUNC(raw_unify_ptrs, t1, t2)
static int
raw_unify_ptrs(DECL_LD Word t1, Word t2)
{ switch(LD->prolog_flag.occurs_check)
  { case OCCURS_CHECK_FALSE:
      return do_unify(t1, t2);
    case OCCURS_CHECK_TRUE:
      return unify_with_occurs_check(t1, t2, OCCURS_CHECK_TRUE);
    case OCCURS_CHECK_ERROR:
      return unify_with_occurs_check(t1, t2, OCCURS_CHECK_ERROR);
    default:
      assert(0);
      fail;
  }
}


static
PRED_IMPL("=", 2, unify, 0)
{ PRED_LD

  return PL_unify(A1, A2);
}


static
PRED_IMPL("\\=", 2, not_unify, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;
  word w1, w2;
  term_t ex;

  deRef(p1); w1 = *p1;
  deRef(p2); w2 = *p2;

  if ( isVar(w1) || isVar(w2) )
  { if ( LD->prolog_flag.occurs_check == OCCURS_CHECK_FALSE )
      return false;			/* can unify */
    goto full_check;
  }
  if ( w1 == w2 )
    return false;
  if ( isAttVar(w1) || isAttVar(w2) )
    goto full_check;
  if ( tag(w1) != tag(w2) )
    return true;

  switch(tag(w1))
  { case TAG_ATOM:
      return true;
    case TAG_INTEGER:
      if ( storage(w1) == STG_INLINE ||
	   storage(w2) == STG_INLINE )
	return true;
    case TAG_STRING:
    case TAG_FLOAT:
      return !equalIndirect(w1, w2);
    case TAG_COMPOUND:
      break;
  }

full_check:
  ex = PL_new_term_ref();

  if ( can_unify(p1, p2, ex) )
    return false;
  if ( !PL_is_variable(ex) )
    return PL_raise_exception(ex);
  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Public unification procedure for `raw' data.   See also PL_unify().

Return:

  - true: success
  - If (flags&ALLOW_RETCODE), one of
      - false: unification failure
      - *_OVERFLOW: stack or memory overflow
    Else
      - false: unification failure or raised exception
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
unify_ptrs(DECL_LD Word t1, Word t2, int flags)
{ for(;;)
  { int rc;

    rc = raw_unify_ptrs(t1, t2);
    if ( rc >= 0 )
      return rc;

    if ( !(flags&ALLOW_RETCODE) )
    { if ( rc == MEMORY_OVERFLOW )
      { return PL_no_memory();
      } else				/* Stack overflow */
      { int rc2;

	PushPtr(t1); PushPtr(t2);
	rc2 = makeMoreStackSpace(rc, flags);
	PopPtr(t2); PopPtr(t1);
	if ( !rc2 )
	  return false;
      }
    } else
      return rc;			/* return error code */
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
can_unify(t1, t2, ex) succeeds if two   terms  *can* be unified, without
actually doing so. This  is  basically   a  stripped  version of unify()
above. See this function for comments.  Note   that  we  have to execute
delayed goals and these may raise an   exception. If this happens, ex is
set to the exception term.

If ex = 0, a possible exception is ignored and cleared.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
can_unify(Word t1, Word t2, term_t ex)
{ GET_LD
  fid_t fid;

  if ( (fid = PL_open_foreign_frame()) )
  { int handle_exception = !ex;

    if ( !ex )
      ex = PL_new_term_ref();

    if ( unify_ptrs(t1, t2, ALLOW_GC|ALLOW_SHIFT) &&
	 foreignWakeup(ex) )
    { PL_discard_foreign_frame(fid);
      return true;
    }

    if ( exception_term && isVar(*valTermRef(ex)) )
      PL_put_term(ex, exception_term);
    if ( !handle_exception && !isVar(*valTermRef(ex)) )
      PL_clear_exception();

    PL_discard_foreign_frame(fid);
  }

  return false;
}


		 /*******************************
		 *	   OCCURS-CHECK		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int var_occurs_in(Word v, Word t)

Succeeds of the term `v' occurs in `t'.  v must be dereferenced on
entry.  Returns one of

	- false if v does not occur in t
	- true if v occurs in t
	- MEMORY_OVERFLOW if the malloc() fails.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define var_occurs_in(v, t) LDFUNC(var_occurs_in, v, t)
static int
var_occurs_in(DECL_LD Word v, Word t)
{ segstack visited;
  Functor tmp[256];
  term_agenda agenda;
  int compound = false;
  int rc = false;

  deRef(t);
  if ( v == t )
  { if ( isTerm(*t) )
      goto unified;

    return false;
  }

  do
  { if ( v == t )
    { rc = true;
      break;
    }

  unified:
    if ( isTerm(*t) )
    { Functor f = valueTerm(*t);
      int arity = arityFunctor(f->definition);

      if ( !compound )
      { compound = true;
	initSegStack(&visited, sizeof(Functor), sizeof(tmp), tmp);
	f->definition |= FIRST_MASK;
	if ( !pushSegStack(&visited, f, Functor) )
	  outOfCore();
	initTermAgenda(&agenda, arity, f->arguments);
      } else if ( !(f->definition & FIRST_MASK) )
      { f->definition |= FIRST_MASK;
	if ( !pushSegStack(&visited, f, Functor) ||
	     !pushWorkAgenda(&agenda, arity, f->arguments) )
	  return MEMORY_OVERFLOW;
      }
    }
  } while( compound && (t=nextTermAgenda(&agenda)) );

  if ( compound )
  { Functor f;

    while( popSegStack(&visited, &f, Functor) )
      f->definition &= ~FIRST_MASK;
    clearTermAgenda(&agenda);
  }

  return rc;
}


int
PL_var_occurs_in(term_t var, term_t value)
{ GET_LD
  Word v = valTermRef(var);

  deRef(v);

  return var_occurs_in(v, valTermRef(value));
}


#define failed_unify_with_occurs_check(t1, t2, mode) LDFUNC(failed_unify_with_occurs_check, t1, t2, mode)
static int
failed_unify_with_occurs_check(DECL_LD Word t1, Word t2, occurs_check_t mode)
{ int rc;

  if ( mode == OCCURS_CHECK_TRUE )
    fail;

  deRef(t1);
  deRef(t2);
  if ( isVar(*t2) )			/* try to make Var = Term */
  { Word tmp = t1;

    t1 = t2;
    t2 = tmp;
  }

  blockGC(0);
  rc = PL_error(NULL, 0, NULL, ERR_OCCURS_CHECK, t1, t2);
  unblockGC(0);

  return rc;
}


static int
unify_with_occurs_check(DECL_LD Word t1, Word t2, occurs_check_t mode)
{ mark m;
  int rc;

  deRef(t1);
  deRef(t2);
  if ( canBind(*t1) )
  { if ( onStack(global, t1) && var_occurs_in(t1, t2) )
      return failed_unify_with_occurs_check(t1, t2, mode);
    return do_unify(t1, t2);
  }
  if ( canBind(*t2) )
  { if ( onStack(global, t2) && var_occurs_in(t2, t1) )
      return failed_unify_with_occurs_check(t1, t2, mode);
    return do_unify(t1, t2);
  }

  Mark(m);
  LD->mark_bar = NO_MARK_BAR;		/* see also unify_all_trail_ptrs() */
  rc = do_unify(t1, t2);
  DiscardMark(m);

  if ( rc == true )
  { TrailEntry tt = tTop;
    TrailEntry mt = m.trailtop.as_ptr;

    while(--tt >= mt)
    { Word p = tt->address;
      Word p2;

      if ( isTrailVal(p) )		/* assignment of an attvars */
      { p = (--tt)->address;

	if ( isTrailVal((--tt)->address) ) /* tail of wakeup list */
	  tt--;
	if ( isTrailVal((--tt)->address) ) /* head of wakeup list */
	  tt--;
      }

      deRef2(p, p2);
      if ( var_occurs_in(p2, p2) )
      { if ( mode == OCCURS_CHECK_ERROR )
	{ Word t = allocGlobalNoShift(1);

	  if ( !t )
	    return GLOBAL_OVERFLOW;
	  *t = *p2;
	  Undo(m);
	  rc = failed_unify_with_occurs_check(p, t, mode);
	}
	rc = false;
	break;
      }
    }
  }

  return rc;
}


static
PRED_IMPL("unify_with_occurs_check", 2, unify_with_occurs_check, 0)
{ PRED_LD
  occurs_check_t old = LD->prolog_flag.occurs_check;
  int rc;

  LD->prolog_flag.occurs_check = OCCURS_CHECK_TRUE;
  rc = PL_unify(A1, A2);
  LD->prolog_flag.occurs_check = old;

  return rc;
}


		/********************************
		*         TYPE CHECKING         *
		*********************************/

static
PRED_IMPL("nonvar", 1, nonvar, 0)
{ PRED_LD
  return PL_is_variable(A1) ? false : true;
}

static
PRED_IMPL("var", 1, var, 0)
{ PRED_LD
  return PL_is_variable(A1);
}

static
PRED_IMPL("integer", 1, integer, 0)
{ return PL_is_integer(A1);
}

static
PRED_IMPL("float", 1, float, 0)
{ return PL_is_float(A1);
}

#if O_BIGNUM
static
PRED_IMPL("rational", 1, rational, 0)
{ return PL_is_rational(A1);
}
#endif

#if O_STRING
static
PRED_IMPL("string", 1, string, 0)
{ return PL_is_string(A1);
}
#endif /* O_STRING */

static
PRED_IMPL("number", 1, number, 0)
{ return PL_is_number(A1);
}

static
PRED_IMPL("atom", 1, atom, 0)
{ PRED_LD
  return PL_is_atom(A1);
}

static
PRED_IMPL("atomic", 1, atomic, 0)
{ PRED_LD
  return PL_is_atomic(A1);
}


		 /*******************************
		 *	       GROUND		*
		 *******************************/

typedef enum
{ ph_mark,
  ph_unmark
} phase;

static inline int
ph_visitedWord(Word p, phase ph)
{ switch(ph)
  { case ph_mark:
      if ( is_marked(p) )
	succeed;
      set_marked(p);
      break;
    case ph_unmark:
      if ( !is_marked(p) )
	succeed;
      clear_marked(p);
  }
  fail;
}

static inline int
ph_visited(Functor f, phase ph)
{ Word p = &f->definition;

  return ph_visitedWord(p, ph);
}


#define ph_ground(p, ph) LDFUNC(ph_ground, p, ph)
static Word
ph_ground(DECL_LD Word p, phase ph) /* Phase 1 marking */
{ term_agenda agenda;

  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  { if ( canBind(*p) )
    { clearTermAgenda(&agenda);
      return p;
    }
    if ( isTerm(*p) )
    { Functor f = valueTerm(*p);

      if ( !ph_visited(f, ph) )
      { pushWorkAgenda(&agenda, arityFunctor(f->definition), f->arguments);
      }
    }
  }

  return NULL;
}


Word
ground(DECL_LD Word p)
{ Word rc1, rc2;

  deRef(p);
  if ( canBind(*p) )
    return p;
  if ( !isTerm(*p) )
    return NULL;

  rc1 = ph_ground(p, ph_mark);  /* mark functors */
  rc2 = ph_ground(p, ph_unmark);  /* unmark the very same functors */
  assert(rc1 == rc2);
  (void)rc2;
  return rc1;
}


bool
PL_is_ground(term_t t)
{ GET_LD

  return ground(valTermRef(t)) == NULL;
}


static
PRED_IMPL("ground", 1, ground, PL_FA_ISO)
{ PRED_LD

  return ground(valTermRef(A1)) == NULL;
}

static
PRED_IMPL("nonground", 2, nonground, 0)
{ PRED_LD
  Word p;

  if ( (p=ground(valTermRef(A1))) )
    return unify_ptrs(valTermRef(A2), p, ALLOW_GC|ALLOW_SHIFT);

  return false;
}


static
PRED_IMPL("compound", 1, compound, 0)
{ return PL_is_compound(A1);
}


static
PRED_IMPL("callable", 1, callable, PL_FA_ISO)
{ return PL_is_callable(A1);
}


		 /*******************************
		 *	     COMPLEXITY		*
		 *******************************/

#define term_size(p, max) LDFUNC(term_size, p, max)
static size_t
term_size(DECL_LD Word p, size_t max)
{ size_t count = 0;
  term_agenda agenda;
  Word t;

  initvisited();
  initTermAgenda(&agenda, 1, p);

  while((t=nextTermAgenda(&agenda)))
  { if ( isAttVar(*t) )
    { Word p = valPAttVar(*t);

      if ( ++count > max )
	break;

      assert(onGlobalArea(p));
      pushWorkAgenda(&agenda, 1, p);
    } else if ( isIndirect(*t) )
    { Word p = addressIndirect(*t);

      count += wsizeofInd(*p)+2;
      if ( count > max )
	break;
    } else if ( isTerm(*t) )
    { Functor f = valueTerm(*t);
      size_t arity = arityFunctor(f->definition);

      if ( visited(f) )
	continue;

      count += arity+1;
      if ( count > max )
	break;

      pushWorkAgenda(&agenda, arity, f->arguments);
    }
  }

  clearTermAgenda(&agenda);
  unvisit();

  return count;
}


/** $term_size(+Term, +Max, -Size)

Size represents the total size of Term on the stack, counted in cells.
*/

static
PRED_IMPL("$term_size", 3, term_size, 0)
{ PRED_LD
  size_t c, m;
  term_t t = A1;
  term_t mx = A2;
  term_t count = A3;

  if ( PL_is_variable(mx) )
    m = (size_t)-1;
  else if ( !PL_get_size_ex(mx, &m) )
    return false;

  c = term_size(valTermRef(t), m);
  if ( c > m )
    return false;

  return PL_unify_integer(count, c);
}


		 /*******************************
		 *	     CYCLIC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A DFS search is used to iterate over all 'term chains' in  a term graph.
A term chain is a list of terms  where term N+1 is  the last argument of
term N.  Terms are 'TEMP' marked as we find them. When the end of a term
chain has been reached  all terms in the chain  are 'PERM' marked  as we
know that all terms  in that chain are acyclic.  If we reach a term that
was already TEMP marked then we terminate the search as a cycle has been
detected.  If we reach a term  that has already been PERM marked then we
backtrack as a shared term that we know to be acyclic has been reached.

Two strategies are used  to avoid repeated  pop+push cycles  of the same
term chain:

1. aggressively cache new term chains for all args of the tail term.
2. only cache the current term chain  if we know at least one arg of the
   tail term is itself a term.

Author: Keri Harris
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ACYCLIC_TEMP_MASK	FIRST_MASK
#define ACYCLIC_PERM_MASK	MARK_MASK

#define set_acyclic_temp(p)	do { *(p) |= ACYCLIC_TEMP_MASK; } while(0)
#define set_acyclic_perm(p)	do { *(p) |= ACYCLIC_PERM_MASK; } while(0)

#define clear_acyclic_temp(p)	do { *(p) &= ~ACYCLIC_TEMP_MASK; } while(0)
#define clear_acyclic_perm(p)	do { *(p) &= ~ACYCLIC_PERM_MASK; } while(0)
#define clear_acyclic_both(p) \
	do { *(p) &= ~(ACYCLIC_TEMP_MASK|ACYCLIC_PERM_MASK); } while(0)

#define is_acyclic_temp(p)	(*(p) & ACYCLIC_TEMP_MASK)
#define is_acyclic_perm(p)	(*(p) & ACYCLIC_PERM_MASK)
#define is_acyclic_or_temp(p)	(*(p) & (ACYCLIC_TEMP_MASK|ACYCLIC_PERM_MASK))

typedef struct termChain
{ Functor	head;
  Functor	tail;
  Word          p;
} termChain;

typedef struct term_chain_agenda
{ termChain	work;
  segstack	stack;
} term_chain_agenda;


#define ph_acyclic_mark(p) LDFUNC(ph_acyclic_mark, p)
static int
ph_acyclic_mark(DECL_LD Word p)
{ term_chain_agenda agenda;
  termChain chains[32];
  Functor top = valueTerm(*p);
  Functor head = top;
  Functor tail = top;
  Functor iter;
  Word pdef;
  int arity;

  initSegStack(&agenda.stack, sizeof(termChain), sizeof(chains), chains);

  while ( true )
  { if ( is_acyclic_temp(&tail->definition) )
    { if ( is_acyclic_perm(&tail->definition) )
      { goto end_of_chain;
      } else
      { clearSegStack(&agenda.stack);
	return false;
      }
    }

    set_acyclic_temp(&tail->definition);

    arity = arityFunctor(tail->definition);

    if ( arity > 1 )
    { int i;
      int new_workspace = false;

      iter = tail;
      for( i = arity-2; i >= 0; i-- )
      { p = iter->arguments + i;
	deRef(p);

	if ( isTerm(*p) )
	{ if ( !new_workspace )
	  { if ( !pushSegStack(&agenda.stack, agenda.work, termChain) )
	      outOfCore();
	    agenda.work.head = head;
	    agenda.work.tail = tail;
	    agenda.work.p = iter->arguments + arity-1;

	    head = tail = valueTerm(*p);
	    new_workspace = true;
	  } else
	  { if ( !pushSegStack(&agenda.stack, agenda.work, termChain) )
	      outOfCore();
	    agenda.work.head = agenda.work.tail = valueTerm(*p);
	    agenda.work.p = NULL;
	  }
	}
      }

      if ( new_workspace )
	continue;
    }

    p = tail->arguments + arity-1;
    deRef(p);

  process_p:
    if ( isTerm(*p) )
    { tail = valueTerm(*p);
    } else
    {
    end_of_chain:

      if ( head == top )
	return true;

      iter = head;
      pdef = &iter->definition;
      while ( iter != tail )
      { set_acyclic_perm(pdef);

	p = iter->arguments + arityFunctor(*pdef) - 1;
	deRef(p);
	iter = valueTerm(*p);
	pdef = &iter->definition;
      }
      set_acyclic_perm(pdef);

      head = agenda.work.head;
      tail = agenda.work.tail;
      p = agenda.work.p;

      if ( !popSegStack(&agenda.stack, &agenda.work, termChain) )
      { assert(0);
      }

      if ( p )
      { deRef(p);
	goto process_p;
      }
    }
  }

  return true;
}


#define ph_acyclic_unmark(p) LDFUNC(ph_acyclic_unmark, p)
static int
ph_acyclic_unmark(DECL_LD Word p)
{ term_agenda agenda;

  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  { if ( isTerm(*p) )
    { Functor f = valueTerm(*p);
      Word p = &f->definition;

      if ( is_acyclic_temp(p) )
      { clear_acyclic_both(p);
      } else
      { continue;
      }

      pushWorkAgenda(&agenda, arityFunctor(f->definition), f->arguments);
    }
  }

  return true;
}


int
is_acyclic(DECL_LD Word p)
{ int rc1;

  deRef(p);
  if ( isTerm(*p) )
  { rc1 = ph_acyclic_mark(p);
    ph_acyclic_unmark(p);

    return rc1;
  }

  return true;
}


#define PL_is_acyclic(t) LDFUNC(PL_is_acyclic, t)
static int
PL_is_acyclic(DECL_LD term_t t)
{ int rc;

  if ( (rc=is_acyclic(valTermRef(t))) == true )
    return true;

  if ( rc == MEMORY_OVERFLOW )
    rc = PL_error(NULL, 0, NULL, ERR_NOMEM);

  return rc;
}


API_STUB(bool)
(PL_is_acyclic)(term_t t)
( return PL_is_acyclic(t); )


static
PRED_IMPL("acyclic_term", 1, acyclic_term, PL_FA_ISO)
{ PRED_LD

  return PL_is_acyclic(A1);
}


static
PRED_IMPL("cyclic_term", 1, cyclic_term, 0)
{ PRED_LD
  int rc;

  if ( (rc=is_acyclic(valTermRef(A1))) == true )
    return false;
  if ( rc == false )
    return true;

  return PL_error(NULL, 0, NULL, ERR_NOMEM);
}


		 /*******************************
		 *	     FACTORIZE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Factorizing a term  based  on  the   internal  sharing.  This  takes the
following steps:

  1. scan_shared() walks the term and
     a. Set MARK_MASK on all visited terms and FIRST_MASK on those find
	twice.
     b. Create a list Var=Term for all terms found twice.
     c. Returns the count of places that must be shared.
  2. reverse_factor_pointers() walks through the created list, placing
     the functor in Var and creating a reference from the location of
     the original functor.
  3. link_shared() walks the term and
     a. If the functor is a reference, follow the reference-chain to
	find the functor.  Link the term into the reference-chain.
     b. If the functor is marked, unmark it.
  4. restore_shared_functors() finishes the job by following the
     variable-list and putting all functors from the variable back
     into the term and setting the variable to be a real variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define scan_shared(t, vart, count) LDFUNC(scan_shared, t, vart, count)
static int
scan_shared(DECL_LD Word t, Word vart, size_t *count)
{ term_agenda agenda;
  size_t shared = 0;
  Word p;

  initTermAgenda(&agenda, 1, t);
  while( (p=nextTermAgenda(&agenda)) )
  { if ( isTerm(*p) )
    { Functor f = valueTerm(*p);
      Word d = &f->definition;

      if ( is_marked(d) )
      { if ( !is_first(d) )
	{ Word v;

	  if ( !(v=allocGlobalNoShift(6)) )
	    return GLOBAL_OVERFLOW;

	  v[0] = FUNCTOR_dot2;
	  v[1] = consPtr(&v[3], TAG_COMPOUND|STG_GLOBAL);
	  v[2] = ATOM_nil;
	  v[3] = FUNCTOR_equals2;
	  v[4] = 0;			/* For now */
	  v[5] = consPtr(d, TAG_COMPOUND|STG_GLOBAL);

	  *vart = consPtr(&v[0], TAG_COMPOUND|STG_GLOBAL);
	  vart = &v[2];

	  set_first(d);
	  shared++;			/* this is already the second one */
	}
	shared++;
      } else
      { int arity = arityFunctor(f->definition);

	pushWorkAgenda(&agenda, arity, f->arguments);
	set_marked(d);
      }
    }
  }
  clearTermAgenda(&agenda);
  *count = shared;

  return true;
}


/* Needed to restore if we run out of stack
*/

#define unscan_shared(t) LDFUNC(unscan_shared, t)
static int
unscan_shared(DECL_LD Word t)
{ term_agenda agenda;
  Word p;

  initTermAgenda(&agenda, 1, t);
  while( (p=nextTermAgenda(&agenda)) )
  { if ( isTerm(*p) )
    { Functor f = valueTerm(*p);
      Word d = &f->definition;

      if ( is_marked(d) )
      { int arity;

	clear_marked(d);
	clear_first(d);
	arity = arityFunctor(f->definition);
	pushWorkAgenda(&agenda, arity, f->arguments);
      }
    }
  }
  clearTermAgenda(&agenda);

  return true;
}


#define reverse_factor_pointers(vars) LDFUNC(reverse_factor_pointers, vars)
static void
reverse_factor_pointers(DECL_LD Word vars)
{ while(*vars != ATOM_nil)
  { Word v = (Word)valueTerm(*vars);
    Functor t = valueTerm(v[5]);

    v[4] = t->definition & ~(MARK_MASK|FIRST_MASK); /* v[4] is the variable */
    t->definition = makeRefG(&v[4])|MARK_MASK|FIRST_MASK;

    vars = &v[2];
  }
}


#define restore_shared_functors(vars) LDFUNC(restore_shared_functors, vars)
static void
restore_shared_functors(DECL_LD Word vars)
{ while(*vars != ATOM_nil)
  { Word v = (Word)valueTerm(*vars);
    Functor t = valueTerm(v[5]);
    Word p = &v[4];

    deRef(p);
    t->definition = *p;
    setVar(*p);

    vars = &v[2];
  }
}


#define link_shared(t) LDFUNC(link_shared, t)
static int
link_shared(DECL_LD Word t)
{ term_agenda agenda;
  Word p;

  initTermAgenda(&agenda, 1, t);
  while( (p=nextTermAgenda(&agenda)) )
  { if ( isTerm(*p) )
    { Functor f = valueTerm(*p);
      Word d = &f->definition;

      if ( isRef(*d) )			/* shared term */
      { Word v;

	v = unRef(*d & ~(FIRST_MASK|MARK_MASK));
	deRef(v);

	if ( is_marked(d) )
	{ int arity = arityFunctor(*v);
	  pushWorkAgenda(&agenda, arity, f->arguments);
	}

	if ( v < p )
	{ TrailAssignment(p);
	  *p = makeRefG(v);
	} else
	{ TrailAssignment(p);
	  *p = *v;
	  *v = makeRefG(p);
	}
      } else if ( is_marked(d) )
      { int arity;
	word fun = f->definition & ~(FIRST_MASK|MARK_MASK);

	clear_marked(d);
	arity = arityFunctor(fun);
	pushWorkAgenda(&agenda, arity, f->arguments);
      }
    }
  }
  clearTermAgenda(&agenda);

  return true;
}


int
PL_factorize_term(term_t term, term_t template, term_t factors)
{ GET_LD
  fid_t fid;
  term_t vars, wrapped;
  Word t;
  size_t count;
  int rc;

  for(;;)
  { if ( !(fid = PL_open_foreign_frame()) ||
	 !(wrapped = PL_new_term_ref()) ||
	 !(vars = PL_new_term_ref()) ||
	 !PL_unify_term(wrapped, PL_FUNCTOR, FUNCTOR_var1, PL_TERM, term) )
      return false;

    PL_put_nil(vars);
    t = valTermRef(wrapped);

    DEBUG(CHK_SECURE, checkStacks(NULL));
    switch( (rc=scan_shared(t, valTermRef(vars), &count)) )
    { case true:
	if ( tTop + 2*count > tMax )
	  rc = TRAIL_OVERFLOW;
	else if ( gTop + count > gMax )
	  rc = GLOBAL_OVERFLOW;
	else
	  break;
	/*FALLTHROUGH*/
      default:
	unscan_shared(t);
	PL_discard_foreign_frame(fid);
	if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	  return false;
	continue;
    }

    break;
  }

  reverse_factor_pointers(valTermRef(vars));
  link_shared(t);
  restore_shared_functors(valTermRef(vars));
  PL_close_foreign_frame(fid);
  DEBUG(CHK_SECURE, checkStacks(NULL));

  _PL_get_arg(1, wrapped, wrapped);
  return ( PL_unify(template, wrapped) &&
	   PL_unify(factors, vars) );
}

static
PRED_IMPL("$factorize_term", 3, factorize_term, 0)
{ return PL_factorize_term(A1, A2, A3);
}

		 /*******************************
		 *	 META-CALL SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
deterministic(-Bool)

Bool = true if no choicepoint has been created in the current clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("deterministic", 1, deterministic, 0)
{ PRED_LD
  LocalFrame FR  = environment_frame->parent;
  Choice     BFR = LD->choicepoints;

  for( ; BFR; BFR = BFR->parent)
  { switch(BFR->type)
    { case CHP_CLAUSE:
	if ( BFR->frame == FR )
	  return PL_unify_atom(A1, ATOM_true);
      case CHP_JUMP:
	if ( (void *)BFR > (void *)FR )
	  return PL_unify_atom(A1, ATOM_false);
	else
	  return PL_unify_atom(A1, ATOM_true);
      default:
	continue;
    }
  }

  return PL_unify_atom(A1, ATOM_true);
}


#ifdef O_TERMHASH
		 /*******************************
		 *	    TERM-HASH		*
		 *******************************/

#define termHashValue(term, depth, hval) LDFUNC(termHashValue, term, depth, hval)
static bool
termHashValue(DECL_LD word term, long depth, unsigned int *hval)
{ for(;;)
  { switch(tag(term))
    { case TAG_VAR:
      case TAG_ATTVAR:
	fail;
      case TAG_ATOM:
      { *hval = MurmurHashAligned2(&atomValue(term)->hash_value,
				   sizeof(unsigned int), *hval);
	succeed;
      }
      case TAG_STRING:
      { size_t len;
	char *s;

	s = getCharsString(term, &len);
	*hval = MurmurHashAligned2(s, len, *hval);

	succeed;
      }
      case TAG_INTEGER:
	if ( storage(term) == STG_INLINE )
	{ int64_t v = valInt(term);

	  *hval = MurmurHashAligned2(&v, sizeof(v), *hval);

	  succeed;
	}
      /*FALLTHROUGH*/
      case TAG_FLOAT:
	{ Word p = addressIndirect(term);
	  size_t n = wsizeofInd(*p);

	  *hval = MurmurHashAligned2(p+1, n*sizeof(word), *hval);

	  succeed;
	}
      case TAG_COMPOUND:
      { Functor t = valueTerm(term);
	FunctorDef fd;
	int arity;
	Word p;
	unsigned int atom_hashvalue;

	if ( visited(t) )
	{ *hval = MurmurHashAligned2(hval, sizeof(*hval), *hval);
	  succeed;
	}

	fd = valueFunctor(t->definition);
	arity = fd->arity;

	atom_hashvalue = atomValue(fd->name)->hash_value + arity;
	*hval = MurmurHashAligned2(&atom_hashvalue,
				   sizeof(atom_hashvalue),
				   *hval);

	if ( --depth != 0 )
	{ for(p = t->arguments; arity-- > 0; p++)
	  { if ( !termHashValue(*p, depth, hval) )
	    { popVisited();
	      fail;
	    }
	  }
	}

	popVisited();
	succeed;
      }
      case TAG_REFERENCE:
      { term = *unRef(term);
	continue;
      }
      default:
	assert(0);
    }
  }
}


/* term_hash(+Term, +Depth, +Range, -HashKey) */

static
PRED_IMPL("term_hash", 4, term_hash4, 0)
{ PRED_LD
  Word p = valTermRef(A1);
  unsigned int hraw = MURMUR_SEED;
  long depth;
  int range;
  int rc = true;

  if ( !PL_get_long_ex(A2, &depth) )
    fail;
  if ( depth < -1 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_not_less_than_zero, A2);

  if ( !PL_get_integer_ex(A3, &range) )
    fail;
  if ( range < 1 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_not_less_than_one, A2);

  if ( depth != 0 )
  { initvisited();
    rc = termHashValue(*p, depth, &hraw);
    DEBUG(CHK_SECURE, assert(empty_visited()));
  }

  if ( rc )
  { hraw = hraw % range;

    return PL_unify_integer(A4, hraw);
  }

  succeed;
}

#endif /*O_TERMHASH*/


		/********************************
		*        STANDARD ORDER         *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There are atoms of different  type.   We  only define comparison between
atoms of the same type, except for mixed ISO Latin-1 and UCS atoms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
compareAtoms(atom_t w1, atom_t w2)
{ if ( w1 == w2 )
    return CMP_EQUAL;

  Atom a1 = atomValue(w1);
  Atom a2 = atomValue(w2);

  if ( a1->type == a2->type )
  { if ( a1->type->compare )
    { return (*a1->type->compare)(w1, w2);
    } else
    { size_t l = (a1->length <= a2->length ? a1->length : a2->length);
      int v;

      if ( (v=memcmp(a1->name, a2->name, l)) != 0 )
	return SCALAR_TO_CMP(v, 0);
      else
	return SCALAR_TO_CMP(a1->length, a2->length);
    }
  } else if ( ison(a1->type, PL_BLOB_TEXT) &&
	      ison(a2->type, PL_BLOB_TEXT) )
  { PL_chars_t t1, t2;
    size_t len;

    get_atom_text(w1, &t1);
    get_atom_text(w2, &t2);
    len = t1.length > t2.length ? t1.length : t2.length;

    return PL_cmp_text(&t1, 0, &t2, 0, len);
  } else
  { return SCALAR_TO_CMP(a1->type->rank, a2->type->rank);
  }
}


#define compareStrings(w1, w2, eq) LDFUNC(compareStrings, w1, w2, eq)
static int
compareStrings(DECL_LD word w1, word w2, bool eq)
{ PL_chars_t t1, t2;
  size_t len;

  get_string_text(w1, &t1);
  get_string_text(w2, &t2);
  if ( eq && ( t1.encoding != t2.encoding ||
	       t1.length   != t2.length ) )
    return CMP_NOTEQ;

  size_t len1 = PL_text_length(&t1);
  size_t len2 = PL_text_length(&t2);
  len = (len1 > len2 ? len1 : len2);

  return PL_cmp_text(&t1, 0, &t2, 0, len);
}

/* Compare two floats know to be non-equal.  As we do standard
   order comparison, we may not return equal and must decide on
   one to be the greatest.
*/

static int
compare_neq_floats(double f1, double f2)
{ if ( isnan(f1) )
  { if ( isnan(f2) )
    { double nf1 = NaN_value(f1);
      double nf2 = NaN_value(f2);

      if ( nf1 < nf2 )
      { return CMP_LESS;
      } else if ( nf1 > nf2 )
      { return CMP_GREATER;
      } else if ( signbit(nf1) != signbit(nf2) )
      { return signbit(nf1) ? CMP_LESS : CMP_GREATER;
      } else
      { return CMP_EQUAL;
      }
    }
    return CMP_LESS;
  } else if ( isnan(f2) )
  { return CMP_GREATER;
  }

  if ( f1 < f2 )
  { return CMP_LESS;
  } else if ( f1 > f2 )
  { return CMP_GREATER;
  } else
  { assert(signbit(f1) != signbit(f2));
    return signbit(f1) ? CMP_LESS : CMP_GREATER;
  }
}

#define compare_mixed_float_rational(w1, w2) \
	LDFUNC(compare_mixed_float_rational, w1, w2)

static int
compare_mixed_float_rational(DECL_LD word w1, word w2)
{ number left, right;
  int rc;

  get_number(w1, &left);
  get_number(w2, &right);
  if ( left.type == V_FLOAT && isnan(left.value.f) )
    rc = CMP_LESS;
  else if ( right.type == V_FLOAT && isnan(right.value.f) )
    rc = CMP_GREATER;
  else
  { rc = cmpReals(&left, &right);
    assert(rc != CMP_NOTEQ);
  }
  clearNumber(&left);
  clearNumber(&right);

  if ( rc == CMP_EQUAL )
    rc = (tag(w1) == TAG_FLOAT) ? CMP_LESS : CMP_GREATER;

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
compareStandard(Word p1, Word p2, int eq)

    Rules:

    Var @< AttVar @< Number @< String @< Atom < Term

    OldVar < NewVar	(not relyable)
    Atom:	alphabetically
    Strings:	alphabetically
    number:	value
    Term:	arity / alphabetically / recursive

If eq == true, only test for equality. In this case expensive inequality
tests (alphabetical order) are skipped and the call returns NOTEQ.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define compare_primitives(p1, p2, eq) LDFUNC(compare_primitives, p1, p2, eq)
static int
compare_primitives(DECL_LD Word p1, Word p2, bool eq)
{ word t1, t2;
  word w1, w2;

  w1 = *p1;
  w2 = *p2;

  if ( w1 == w2 )
  { if ( isVar(w1) )
      return SCALAR_TO_CMP(p1, p2);
    return CMP_EQUAL;
  }

  t1 = tag(w1);
  t2 = tag(w2);

  if ( t1 != t2 )
  { if ( eq )
      return CMP_NOTEQ;

    if ( (t1|t2) == (TAG_INTEGER|TAG_FLOAT) && /* quick test first */
	 !truePrologFlag(PLFLAG_ISO) &&
	 ( (t1 == TAG_INTEGER && t2 == TAG_FLOAT) ||
	   (t1 == TAG_FLOAT && t2 == TAG_INTEGER)
	 )
      )
      return compare_mixed_float_rational(w1, w2);

    static_assert(TAG_VAR == 0 && TAG_ATTVAR==1,
		  "Think twice before reordering the tags");
    if ( (t1|t2) > TAG_ATTVAR )			/* actually `t1 > TAG_ATTVAR || t2 > TAG_ATTVAR` */
      return t1 < t2 ? CMP_LESS : CMP_GREATER;
  }

  switch(t1)
  { case TAG_VAR:
    case TAG_ATTVAR:
      return SCALAR_TO_CMP(p1, p2);
    case TAG_INTEGER:
    { number n1, n2;
      int rc;

      if ( storage(w1) == STG_INLINE &&
	   storage(w2) == STG_INLINE )
	return SCALAR_TO_CMP(valInt(w1), valInt(w2));

      get_rational(w1, &n1);
      get_rational(w2, &n2);
      if ( eq && (n1.type != n2.type) )
	return CMP_NOTEQ;
      rc = cmpNumbers(&n1, &n2);
      clearNumber(&n1);
      clearNumber(&n2);

      return rc;
    }
    case TAG_FLOAT:
    { if ( equalIndirect(w1,w2) )
	return CMP_EQUAL;
      else if ( eq )
	return CMP_NOTEQ;
      else
	return compare_neq_floats(valFloat(w1), valFloat(w2));
    }
    case TAG_ATOM:
      return eq ? CMP_NOTEQ : compareAtoms(word2atom(w1), word2atom(w2));
    case TAG_STRING:
      return compareStrings(w1, w2, eq);
    case TAG_COMPOUND:
      return CMP_COMPOUND;
    default:
      assert(0);
      return CMP_ERROR;
  }
}

static int
compare_functors(word f1, word f2, int eq)
{ if ( eq )
  { return CMP_NOTEQ;
  } else
  { FunctorDef fd1 = valueFunctor(f1);
    FunctorDef fd2 = valueFunctor(f2);

    if ( fd1->arity != fd2->arity )
      return fd1->arity > fd2->arity ? CMP_GREATER : CMP_LESS;

    return compareAtoms(fd1->name, fd2->name);
  }
}

#define do_compare(agenda, f1, f2, eq) LDFUNC(do_compare, agenda, f1, f2, eq)
static int
do_compare(DECL_LD term_agendaLR *agenda, Functor f1, Functor f2, int eq)
{ Word p1, p2;

  goto compound;

  while( nextTermAgendaLR(agenda, &p1, &p2) )
  { int rc;

    deRef(p1);
    deRef(p2);

    if ( (rc=compare_primitives(p1, p2, eq)) != CMP_COMPOUND )
    { if ( rc == CMP_EQUAL )
	continue;
      return rc;
    } else
    { f1 = (Functor)valPtr(*p1);
      f2 = (Functor)valPtr(*p2);

#if O_CYCLIC
      while ( isRef(f1->definition) )
	f1 = (Functor)unRef(f1->definition);
      while ( isRef(f2->definition) )
	f2 = (Functor)unRef(f2->definition);
      if ( f1 == f2 )
	continue;
#endif

      if ( f1->definition != f2->definition )
      { return compare_functors(f1->definition, f2->definition, eq);
      } else
      { int arity;

      compound:
	arity = arityFunctor(f1->definition);

	linkTermsCyclic(f1, f2);
	if ( !pushWorkAgendaLR(agenda, arity, f1->arguments, f2->arguments) )
	{ PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_memory);
	  return CMP_ERROR;
	}
	continue;
      }
    }
  }

  return CMP_EQUAL;
}


int
compareStandard(DECL_LD Word p1, Word p2, int eq)
{ int rc;

  deRef(p1);
  deRef(p2);

  if ( (rc=compare_primitives(p1, p2, eq)) != CMP_COMPOUND )
  { return rc;
  } else
  { Functor f1 = (Functor)valPtr(*p1);
    Functor f2 = (Functor)valPtr(*p2);

    if ( f1->definition != f2->definition )
    { return compare_functors(f1->definition, f2->definition, eq);
    } else
    { term_agendaLR agenda;

      initCyclic();
      initTermAgendaLR0(&agenda);
      rc = do_compare(&agenda, f1, f2, eq);
      clearTermAgendaLR(&agenda);
      exitCyclic();

      return rc;
    }
  }
}


/* compare(-Diff, +T1, +T2) */

static
PRED_IMPL("compare", 3, compare, PL_FA_ISO)
{ PRED_LD
  Word d  = valTermRef(A1);
  Word p1 = valTermRef(A2);
  Word p2 = p1+1;
  int val;
  atom_t a;

  deRef(d);
  if ( canBind(*d) )
  { a = 0;
  } else
  { if ( isAtom(*d) )
    { a = word2atom(*d);

      if ( a == ATOM_equals )
	return compareStandard(p1, p2, true) == CMP_EQUAL ? true : false;

      if ( a != ATOM_smaller && a != ATOM_larger )
	return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_order, A1);
    } else
      return PL_type_error("atom", A1);
  }

  if ( (val = compareStandard(p1, p2, false)) == CMP_ERROR )
    return false;

  if ( a )
  { if ( a == ATOM_smaller )
      return val < 0;
    else
      return val > 0;
  } else
  { a = val < 0 ? ATOM_smaller :
	val > 0 ? ATOM_larger :
		  ATOM_equals;

    return PL_unify_atom(A1, a);
  }
}


static
PRED_IMPL("@<", 2, std_lt, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;
  int rc;

  if ( (rc=compareStandard(p1, p2, false)) == CMP_ERROR )
    return false;

  return rc < 0 ? true : false;
}


static
PRED_IMPL("@=<", 2, std_leq, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;
  int rc;

  if ( (rc=compareStandard(p1, p2, false)) == CMP_ERROR )
    return false;

  return rc <= 0 ? true : false;
}


static
PRED_IMPL("@>", 2, std_gt, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;
  int rc;

  if ( (rc=compareStandard(p1, p2, false)) == CMP_ERROR )
    return false;

  return rc > 0 ? true : false;
}


static
PRED_IMPL("@>=", 2, std_geq, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;
  int rc;

  if ( (rc=compareStandard(p1, p2, false)) == CMP_ERROR )
    return false;

  return rc >= 0 ? true : false;
}

		/********************************
		*           EQUALITY            *
		*********************************/

static
PRED_IMPL("==", 2, equal, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;
  int rc;

  if ( (rc=compareStandard(p1, p2, true)) == CMP_ERROR )
    return false;

  return rc == CMP_EQUAL ? true : false;
}


static
PRED_IMPL("\\==", 2, nonequal, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;
  int rc;

  if ( (rc=compareStandard(p1, p2, true)) == CMP_ERROR )
    return false;

  return rc == CMP_EQUAL ? false : true;
}


/** ?=(@X, @Y) is semidet.

True if we can decide for now and forever  that X and Y are either equal
or non-equal. I.e. X and Y are equal or they cannot unify.
*/

static
PRED_IMPL("?=", 2, can_compare, 0)
{ PRED_LD
  fid_t fid = PL_open_foreign_frame();
  int rc;

  rc = PL_unify(A1, A2);
  if ( rc )
  { FliFrame fr = (FliFrame) valTermRef(fid);

    FLI_ASSERT_VALID(fr);
    if ( fr->mark.trailtop.as_ptr != tTop )
      rc = false;
  } else if ( exception_term )
  { PL_close_foreign_frame(fid);	/* keep exception */
    return false;
  } else
  { rc = true;				/* could not unify */
  }

  PL_discard_foreign_frame(fid);
  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
same_term(@T1, @T2) is semidet.

True if T1 and T2 is really  the   same  term,  so setarg/3 affects both
terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_same_term(DECL_LD term_t T1, term_t T2)
{ Word t1 = valTermRef(T1);
  Word t2 = valTermRef(T2);

  deRef(t1);
  deRef(t2);

  if ( isVar(*t1) )
    return t1 == t2;
  if ( *t1 == *t2 )
    succeed;
  if ( isIndirect(*t1) && isIndirect(*t2) )
    return equalIndirect(*t1, *t2);

  fail;
}

static
PRED_IMPL("same_term", 2, same_term, 0)
{ PRED_LD

  return PL_same_term(A1, A2);
}

static
PRED_IMPL("$term_id", 2, term_id, 0)
{ PRED_LD
  Word t1 = valTermRef(A1);

  deRef(t1);
  if ( isTerm(*t1) )
  { Functor f = valueTerm(*t1);
    return PL_unify_int64(A2, (Word)f-gBase);
  }

  return PL_type_error("compound", A1);
}

		/********************************
		*         TERM HACKING          *
		*********************************/

/* functor(+Term, -Name, -Arity) */
/* functor(-Term, +Name, +Arity) */

static
PRED_IMPL("functor", 3, functor, 0)
{ PRED_LD
  size_t arity;
  atom_t name;
  Word p = valTermRef(A1);

  deRef(p);

  if ( isTerm(*p) )
  { FunctorDef fd = valueFunctor(functorTerm(*p));

    if ( fd->arity == 0 )
      return PL_domain_error("compound_non_zero_arity", A1);

    return ( PL_unify_atom(A2, fd->name) &&
	     PL_unify_integer(A3, fd->arity) );
  }
  if ( isAtomic(*p) )
  { return ( PL_unify(A2, A1) &&
	     PL_unify_integer(A3, 0) );
  }
  if ( !PL_is_atomic(A2) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atomic, A2);

  if ( !PL_get_size_ex(A3, &arity) )
    fail;
  if ( arity == 0 )
    return PL_unify(A1, A2);
  if ( PL_get_atom_ex(A2, &name) )
    return PL_unify_functor(A1, PL_new_functor(name, arity));

  fail;
}


/* compound_name_arity(+Compound, -Name, -Arity) */
/* compound_name_arity(-Compound, +Name, +Arity) */

static
PRED_IMPL("compound_name_arity", 3, compound_name_arity, 0)
{ PRED_LD
  size_t arity;
  atom_t name;
  Word p = valTermRef(A1);

  deRef(p);

  if ( isTerm(*p) )
  { FunctorDef fd = valueFunctor(functorTerm(*p));
    if ( !PL_unify_atom(A2, fd->name) ||
	 !PL_unify_integer(A3, fd->arity) )
      fail;

    succeed;
  }
  if ( !canBind(*p) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_compound, A1);

  if ( !PL_get_atom_ex(A2, &name) ||
       !PL_get_size_ex(A3, &arity) )
    fail;

  return PL_unify_compound(A1, PL_new_functor(name, arity));
}


/* functor(+Callable, -Name, -Arity, -Type) */
/* functor(-Callable, +Name, +Arity, +Type) */

#define match_functor_type(Type, atype, type) LDFUNC(match_functor_type, Type, atype, type)
static int
match_functor_type(DECL_LD term_t Type, atom_t atype, atom_t type)
{ if ( !atype )
    return PL_unify_atom(Type, type);
  if ( atype == type )
    return true;
  if ( atype == ATOM_callable &&
       ( type == ATOM_atom || type == ATOM_compound ) )
    return true;

  return false;
}

static
PRED_IMPL("functor", 4, functor, 0)
{ PRED_LD
  size_t arity;
  atom_t name;
  atom_t type;
  Word p;

  if ( !PL_get_atom(A4, &type) )
  { if ( PL_is_variable(A4) )
      type = 0;
    else
      return PL_type_error("atom", A4);
  }

  p = valTermRef(A1);
  deRef(p);

  if ( isTerm(*p) )
  { FunctorDef fd = valueFunctor(functorTerm(*p));

    return ( PL_unify_atom(A2, fd->name) &&
	     PL_unify_integer(A3, fd->arity) &&
	     match_functor_type(A4, type, ATOM_compound) );
  } else if ( isAtom(*p) )
  { return ( PL_unify_atom(A2, word2atom(*p)) &&
	     PL_unify_integer(A3, 0) &&
	     match_functor_type(A4, type, ATOM_atom) );
  } else if ( !canBind(*p) )
  { return ( PL_unify(A2, A1) &&
	     PL_unify_integer(A3, 0) &&
	     match_functor_type(A4, type, ATOM_atomic) );
  }

  if ( !PL_get_size_ex(A3, &arity) )
    return false;

  if ( arity > 0 )
  { return ( PL_get_atom_ex(A2, &name) &&
	     match_functor_type(A4, type, ATOM_compound) &&
	     PL_unify_compound(A1, PL_new_functor(name, arity)) );
  } else if ( PL_get_atom(A2, &name) )
  { if ( type == ATOM_compound )
      return PL_unify_compound(A1, PL_new_functor(name, 0));
    else if ( type == ATOM_callable || type == ATOM_atom )
      return PL_unify_atom(A1, name);
    else if ( type == ATOM_atomic )
      return false;
    else
      return PL_domain_error("functor_type", A4);
  } else if ( PL_is_atomic(A2) )
  { return ( PL_unify(A2, A1) &&
	     match_functor_type(A4, type, ATOM_atomic) );
  } else
  { return PL_type_error("atomic", A2);
  }
}


/** '$filled_array'(-Compound, +Name, +Arity, +Value) is det.
 * Created an array (compound) with all arguments set to Value.
 */

static
PRED_IMPL("$filled_array", 4, filled_array, 0)
{ PRED_LD
  size_t arity;
  atom_t name;
  functor_t f;
  Word p, v;
  term_t compound = PL_new_term_ref();
  size_t i;

  if ( !PL_get_atom_ex(A2, &name) ||
       !PL_get_size_ex(A3, &arity) )
    return false;

  f = PL_new_functor(name, arity);
  if ( !(p = allocGlobal(arity+1)) )
    return false;
  v = valTermRef(A4);
  deRef(v);

  p[0] = f;
  if ( arity > 0 )
  { word w;
    bArgVar(&p[1], v);
    w = isVar(p[1]) ? makeRefG(&p[1]) : p[1];
    for(i=2; i<=arity; i++)
      p[i] = w;
  }

  *valTermRef(compound) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
  return PL_unify(A1, compound);
}




/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int get_arg_integer_ex(term_t t, intptr_t *n)

Get argument position from t.  Returns:

   true  if t is a small non-negative integer
   -1    if t is unbound
   false
     - with exception if t is not an integer or negative
     - without exception if t is 0 or a large positive integer
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define get_arg_integer_ex(t, n) LDFUNC(get_arg_integer_ex, t, n)
static int
get_arg_integer_ex(DECL_LD term_t t, size_t *n)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTaggedInt(*p) )
  { sword v = valInt(*p);

    if ( v > 0 )
    {
#if SIZEOF_VOIDP < SIZEOF_WORD
      if ( v > INT_MAX )
	return false;
#endif
      *n = (size_t)v;
      return true;
    }
    if ( v == 0 )
      return false;
  }

  if ( isInteger(*p) )
  { number n;

    get_integer(*p, &n);
    if ( ar_sign_i(&n) < 0 )
      PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_not_less_than_zero, t);

    return false;
  }

  if ( canBind(*p) )
    return -1;

  PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
  return false;
}


static
PRED_IMPL("arg", 3, arg, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  size_t arity;
  size_t argn;

  term_t n    = A1;
  term_t term = A2;
  term_t arg  = A3;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { size_t idx;
      int rc;
      Word p = valTermRef(term);

      deRef(p);
      if ( isTerm(*p) )
	arity = arityTerm(*p);
      else
	return PL_error("arg", 3, NULL, ERR_TYPE, ATOM_compound, term);

      if ( (rc=get_arg_integer_ex(n, &idx)) == true )
      { if ( idx <= arity )
	{ Word ap = argTermP(*p, idx-1);

	  return unify_ptrs(valTermRef(arg), ap, ALLOW_GC|ALLOW_SHIFT);
	}
	fail;
      }
      if ( rc == -1 )			/* variable */
      { argn = 1;

	goto genarg;
      }
      return false;			/* bigint, negative or type error */
    }
    case FRG_REDO:
    { term_t a;
      fid_t fid;
      int rc;
      Word p = valTermRef(term);

      deRef(p);
      arity = arityTerm(*p);
      argn = (unsigned)CTX_INT + 1;

    genarg:
      rc = false;
      if ( !(fid=PL_open_foreign_frame()) ||
	   !(a = PL_new_term_ref()) )
	return false;
      for(; argn <= arity; argn++)
      { _PL_get_arg(argn, term, a);
	if ( PL_unify(arg, a) )
	{ if ( !PL_unify_integer(n, argn) )
	    break;
	  if ( argn == arity )
	  { rc = true;
	    break;
	  }
	  PL_close_foreign_frame(fid);
	  ForeignRedoInt(argn);
	}
	if ( exception_term )
	  break;
	PL_rewind_foreign_frame(fid);
      }

      PL_close_foreign_frame(fid);
      return rc;
    }
    default:
      succeed;
  }
}


/* unify_vp() assumes *vp is a variable and binds it to val.
   The assignment is *not* trailed. As no allocation takes
   place, there are no error conditions.

   It is *not* allowed for *both* vp and val to be local stack
   pointers.
*/

void
unify_vp(DECL_LD Word vp, Word val)
{ deRef(val);

  if ( isVar(*val) )
  { if ( val < vp )
    { DEBUG(0, assert(val < (Word)lBase));
      *vp = makeRefG(val);
    } else if ( vp < val )
    { setVar(*vp);
      DEBUG(0, assert(vp < (Word)lBase));
      *val = makeRefG(vp);
    } else
      setVar(*vp);
  } else if ( isAttVar(*val) )
  { *vp = makeRefG(val);
  } else
    *vp = *val;
}


#define may_share_in_duplicate(p) LDFUNC(may_share_in_duplicate, p)

static term_t
may_share_in_duplicate(DECL_LD Word p)
{ deRef(p);
  if ( isTerm(*p) )
  { term_t share = PL_new_term_ref();
    *valTermRef(share) = *p;
    return share;
  }

  return 0;
}

bool
setarg(DECL_LD size_t argn, term_t term, term_t value, unsigned int flags)
{ size_t arity;
  Word a, v;

  a = valTermRef(term);
  deRef(a);
  if ( !isTerm(*a) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_compound, term);
  arity = arityTerm(*a);

  if ( argn == 0 || argn > arity )
    return false;

  if ( (flags & SETARG_BACKTRACKABLE) )
  { Word a2;

    a = valTermRef(term);
    deRef(a);
    a = argTermP(*a, argn-1);
    deRef2(a, a2);

    if ( isVar(*a2) )
    { return unify_ptrs(valTermRef(value), a, ALLOW_GC|ALLOW_SHIFT);
    } else
    { if ( !hasGlobalSpace(0) )
      { int rc;

	if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != true )
	  return raiseStackOverflow(rc);
	a = valTermRef(term);
	deRef(a);
	a = argTermP(*a, argn-1);
      }

      TrailAssignment(a);
    }
  } else
  { v = valTermRef(value);
    deRef(v);

    if ( storage(*v) == STG_GLOBAL )
    { if ( isTerm(*v) && !(flags & SETARG_LINK) )
      { term_t copy = PL_new_term_ref();
	term_t keep = may_share_in_duplicate(argTermP(*a, argn-1));
	size_t nshare = keep ? 1 : 0;

	if ( !duplicate_term(value, copy, nshare, keep) )
	  return false;
	value = copy;
      }

      freezeGlobal();
    }

    a = valTermRef(term);		/* duplicate may shift stacks */
    deRef(a);
    a = argTermP(*a, argn-1);
  }
					/* this is unify(), but the */
					/* assignment must *not* be trailed */
  v = valTermRef(value);
  unify_vp(a, v);

  return true;
}

#define setarg_t(arg, term, value, flags) \
	LDFUNC(setarg_t, arg, term, value, flags)

static inline bool
setarg_t(DECL_LD term_t arg, term_t term, term_t value, unsigned int flags)
{ size_t argn;

  if ( !PL_get_size_ex(arg, &argn) || argn == 0 )
    return false;

  return setarg(argn, term, value, flags);
}

static
PRED_IMPL("setarg", 3, setarg, 0)
{ PRED_LD

  return setarg_t(A1, A2, A3, SETARG_BACKTRACKABLE);
}


static
PRED_IMPL("nb_setarg", 3, nb_setarg, 0)
{ PRED_LD

  return setarg_t(A1, A2, A3, 0);
}


static
PRED_IMPL("nb_linkarg", 3, nb_linkarg, 0)
{ PRED_LD

  return setarg_t(A1, A2, A3, SETARG_LINK);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cycle detection for lists using Brent's algorithm.

skip_list() was originally added to SWI-Prolog by Ulrich Neumerkel.  The
code below is a clean-room re-implementation by Keri Harris.

See http://en.wikipedia.org/wiki/Cycle_detection#Brent.27s_algorithm
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

intptr_t
skip_list(DECL_LD Word l, Word *tailp)
{ deRef(l);

  if ( !isList(*l) )
  { *tailp = l;
    return 0;
  } else
  { Word checkCell, currentCell;
    intptr_t length = 0;
    int power, lam;

    checkCell = currentCell = l;
    lam       = 0;
    power     = 1;

    while ( true )
    { currentCell = TailList(currentCell);
      deRef(currentCell);
      length++;

      if ( !isList(*currentCell) || (*checkCell == *currentCell) )
	break;

      lam++;
      if ( power == lam )
      { checkCell = currentCell;
	power *= 2;
	lam = 0;
      }
    }

    *tailp = currentCell;

    return length;
  }
}


/** '$skip_list'(-Length, +Xs0, -Xs) is det.

Xs0, Xs is a pair of list differences. Xs0   is the input list and Xs is
the minimal remaining list. Examination of   Xs  permits to classify the
list Xs0:

	Xs        | list type of Xs0   | Length
	[]    ... | well formed        | length
	Var   ... | partial            | elements skipped
	[_|_] ... | infinite           | upper bound for cycle
	Term  ... | malformed          | elements skipped
*/

static
PRED_IMPL("$skip_list", 3, skip_list, 0)
{ PRED_LD
  Word tail;
  intptr_t len;

  len = skip_list(valTermRef(A2), &tail);
  if ( unify_ptrs(valTermRef(A3), tail, ALLOW_GC|ALLOW_SHIFT) &&
       PL_unify_integer(A1, len) )
    return true;

  return false;
}

/** '$seek_list'(+Count, +List, -RestCount, -Rest)
*/

static
PRED_IMPL("$seek_list", 4, seek_list, 0)
{ PRED_LD
  int64_t size;
  Word tail;

  if ( !PL_get_int64_ex(A1, &size) )
    return false;
  tail = valTermRef(A2);
  while(size > 0)
  { deRef(tail);
    if ( isList(*tail) )
    { tail = TailList(tail);
      size--;
      if ( size%1024 == 0 &&
	   PL_handle_signals() < 0 )
	return false;
    } else
      break;
  }

  return ( unify_ptrs(valTermRef(A4), tail, ALLOW_GC|ALLOW_SHIFT) &&
	   PL_unify_int64(A3, size)
	 );
}


/*  Determine the length of a list.  Returns:

	len >=  0 if list is proper
	len == -1 if list is not a list
	len == -2 if list is incomplete (i.e. tail is unbound)

 ** Mon Apr 18 16:29:01 1988  jan@swivax.UUCP (Jan Wielemaker)  */

intptr_t
lengthList(term_t list, int errors)
{ GET_LD
  intptr_t length = 0;
  Word l = valTermRef(list);
  Word tail;

  length = skip_list(l, &tail);

  if ( isNil(*tail) )
    return length;

  if ( errors )
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, pushWordAsTermRef(l));
    popTermRef();
  }

  return isVar(*tail) ? -2 : -1;
}


static
PRED_IMPL("=..", 2, univ, PL_FA_ISO)
{ GET_LD
  term_t t = A1;
  term_t list = A2;
  Word p;
  int n;

  if ( PL_is_variable(t) )
  { atom_t name;
    intptr_t arity;
    term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();

    if ( !PL_get_list(tail, head, tail) )
    { if ( PL_get_nil(tail) )
	return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			ATOM_non_empty_list, tail);
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, tail);
    }

    if ( PL_get_nil(tail) )		/* A =.. [H] */
    { if ( !PL_is_atomic(head) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atomic, head);
      return PL_unify(t, head);
    }
    if ( !PL_get_atom_ex(head, &name) )
      fail;

    if ( (arity = lengthList(tail, false)) < 0 )
    { if ( arity == -1 )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, list);
      else
	return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    }

    if ( (p = allocGlobal(arity+1)) )
    { Word l = valTermRef(tail);

      *valTermRef(head) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
      *p++ = PL_new_functor(name, arity);
      deRef(l);
      while(isList(*l))
      { Word h = HeadList(l);

	deRef(h);
	*p++ = needsRef(*h) ? makeRefG(h) : *h;
	l = TailList(l);
	deRef(l);
      }

      return PL_unify(t, head);
    }

    return false;
  }

  p = valTermRef(t);
  deRef(p);

  if ( isTerm(*p) )
  { FunctorDef fd = valueFunctor(functorTerm(*p));
    term_t head, l;

    if ( fd->arity == 0 )
      return PL_domain_error("compound_non_zero_arity", A1);

    head = PL_new_term_ref();
    l    = PL_new_term_ref();

    if ( !PL_unify_list_ex(list, head, l) ||
	 !PL_unify_atom(head, fd->name) )
      return false;

    for(n = 1; n <= fd->arity; n++)
    { if ( !PL_unify_list_ex(l, head, l) ||
	   !PL_unify_arg(n, t, head) )
	return false;
    }

    return PL_unify_nil_ex(l);
  }

  if ( PL_is_atomic(t) )		/* 3 =.. X, 3.4 =.. X, "foo" =.. X */
  { term_t head = PL_new_term_ref();
    term_t l = PL_new_term_ref();

    if ( PL_unify_list_ex(list, head, l) &&
	 PL_unify(head, t) &&
	 PL_unify_nil_ex(l) )
      succeed;
  }

  fail;
}


/** compound_name_arguments(-Term, +Name, +Arguments)
    compound_name_arguments(+Term, -Name, -Arguments)
*/

static
PRED_IMPL("compound_name_arguments", 3, compound_name_arguments, 0)
{ GET_LD
  term_t t = A1;
  term_t list = A3;
  intptr_t len;
  size_t arity;
  atom_t name;
  size_t n;

  if ( PL_is_variable(t) )
  { term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();

    if ( !PL_get_atom_ex(A2, &name) )
      return false;

    if ( (len = lengthList(tail, false)) < 0 )
    { if ( len == -1 )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, list);
      else
	return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    }

    if ( !PL_unify_compound(t, PL_new_functor(name, (size_t)len)) )
      fail;

    for(n=1; PL_get_list(tail, head, tail); n++)
    { if ( !PL_unify_arg(n, t, head) )
	fail;
    }

    succeed;
  }

					/* 1st arg is term */
  if ( PL_get_compound_name_arity(t, &name, &arity) )
  { term_t head = PL_new_term_ref();
    term_t l = PL_copy_term_ref(list);

    if ( !PL_unify_atom(A2, name) )
      fail;

    for(n = 1; n <= arity; n++)
    { if ( !PL_unify_list_ex(l, head, l) ||
	   !PL_unify_arg(n, t, head) )
	fail;
    }

    return PL_unify_nil_ex(l);
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_compound, A1);
}


		 /*******************************
		 *	     NUMBERVARS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns	>= 0: Number for next variable variable
	  -1: Error
	< -1: Out of stack error or ALREADY_NUMBERED or CONTAINS_ATTVAR

TBD: when using the `singletons' mode, the   predicate is not cycle safe
(this is an error) and does not exploit sharing. We could fix this using
both flags:

    - Not marked: go in there
    - Marked, but not alt-mark: map vars in there _-->n
    - both-marked: done
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ALREADY_NUMBERED     (-10)
#define CONTAINS_ATTVAR      (-11)
#define REPRESENTATION_ERROR (-12)
#define NV_EINTR	     (-13)

#define do_number_vars(p, options, n, m) LDFUNC(do_number_vars, p, options, n, m)
static intptr_t
do_number_vars(DECL_LD Word p, nv_options *options, intptr_t n, mark *m)
{ term_agenda agenda;
  intptr_t start = n;
  size_t progress = 0;

  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  { if ( canBind(*p) )
    { Word a;
      word v;

      if ( isAttVar(*p) )
      { switch(options->on_attvar)
	{ case AV_SKIP:
	    continue;
	  case AV_ERROR:
	    n = CONTAINS_ATTVAR;
	    goto out;
	  case AV_BIND:
	    break;
	}
      }

      if ( !hasGlobalSpace(2) )
      { n = overflowCode(2);
	goto out;
      }

      a = gTop;
      a[0] = options->functor;
      if ( options->singletons )
      { a[1] = ATOM_anonvar;
      } else
      { intptr_t v = n+options->offset;
	a[1] = consInt(v);
	if ( valInt(a[1]) != v )
	{ n = REPRESENTATION_ERROR;
	  goto out;
	}
	n++;
      }
      gTop += 2;

      v = consPtr(a, TAG_COMPOUND|STG_GLOBAL);
      bindConst(p, v);
    } else if ( isTerm(*p) )
    { Functor f = valueTerm(*p);

      if ( options->singletons && (++progress%32768) == 0 && is_signalled() && !LD->critical )
      { n = NV_EINTR;
	goto out;
      }

      if ( f->definition == options->functor )
      { if ( (Word)f >= m->globaltop.as_ptr )	/* new one we created ourselves */
	{ if ( options->singletons )
	  { Word p = &f->arguments[0];

	    if ( options->singletons == 1 )
	    { if ( *p == ATOM_anonvar )
	      { intptr_t v = n+options->offset;
		word w = consInt(v);
		if ( valInt(w) != v )
		{ n = REPRESENTATION_ERROR;
		  goto out;
		}
		*p = ATOM_var;
		n++;
	      }
	    } else
	    { if ( *p == ATOM_var )
	      { intptr_t v = n+options->offset;

		*p = consInt(v);
		n++;
	      }
	    }
	  }
	} else
	{ Word p = &f->arguments[0];

	  deRef(p);
	  if ( options->numbered_check && isInteger(*p) )
	  { intptr_t i = (intptr_t)valInt(*p); /* cannot be bigger */

	    if ( i >= (intptr_t)start )
	    { n = ALREADY_NUMBERED;
	      goto out;
	    }
	  }
	  if ( isVar(*p) || isTerm(*p) )
	    goto do_number;		/* number '$VAR'(_) */
	}
	continue;
      }

    do_number:
      if ( !options->singletons && visited(f) )
	continue;

      if ( !pushWorkAgenda(&agenda, arityFunctor(f->definition), f->arguments) )
      { n = MEMORY_OVERFLOW;
	goto out;
      }
    }
  }

out:
  clearTermAgenda(&agenda);

  return n;				/* anything else */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns	>= 0: Number for next variable variable
	  -1: Error.  Exception is left in the environment
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

intptr_t
numberVars(DECL_LD term_t t, nv_options *options, intptr_t n)
{ if ( !inTaggedNumRange(n) )
  { PL_representation_error("tagged_integer");
    return NV_ERROR;
  }

  options->offset = n;

  for(;;)
  { mark m;
    intptr_t rc;

    Mark(m);
    initvisited();
    rc = do_number_vars(valTermRef(t), options, 0, &m);
    unvisit();
    if ( rc >= 0 )			/* all ok */
    { DiscardMark(m);
      if ( options->singletons )
      { intptr_t rc2;

	options->singletons = 2;
	initvisited();
	rc2 = do_number_vars(valTermRef(t), options, 0, &m);
	unvisit();
	if ( rc2 == NV_EINTR )
	{ Undo(m);
	  DiscardMark(m);
	  if ( PL_handle_signals() < 0 )
	    return NV_ERROR;
	  assert(!is_signalled());
	  continue;
	}
	assert(rc == rc2);
	(void)rc2;
      }
      return rc + options->offset;
    } else
    { switch( rc )
      { case CONTAINS_ATTVAR:
	  DiscardMark(m);
	  PL_error(NULL, 0, NULL,
		   ERR_TYPE, ATOM_free_of_attvar, t);
	  return NV_ERROR;
	case ALREADY_NUMBERED:
	  DiscardMark(m);
	  PL_error(NULL, 0, "already numbered",
		   ERR_PERMISSION, ATOM_numbervars, ATOM_term, t);
	  return NV_ERROR;
	case REPRESENTATION_ERROR:
	  DiscardMark(m);
	  PL_representation_error("tagged_integer");
	  return NV_ERROR;
	case NV_EINTR:
	  Undo(m);
	  DiscardMark(m);
	  if ( PL_handle_signals() < 0 )
	    return NV_ERROR;
	  assert(!is_signalled());
	  continue;
	default:
	  Undo(m);
	  DiscardMark(m);
	  if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	    return NV_ERROR;
      }
    }
  }
}


static const PL_option_t numbervar_options[] =
{ { ATOM_attvar,	    OPT_ATOM },
  { ATOM_functor_name,	    OPT_ATOM },
  { ATOM_singletons,	    OPT_BOOL },
  { NULL_ATOM,		    0 }
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
numbervars(+Term, +Start, -End, +Options)
numbervars(+Term, +Functor, +Start, -End)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("numbervars", 4, numbervars, 0)
{ GET_LD
  intptr_t n;
  atom_t name = ATOM_isovar;		/* '$VAR' */
  atom_t av = ATOM_error;
  term_t t, end, options;
  nv_options opts;

  opts.singletons = false;
  opts.numbered_check = false;

  t = PL_copy_term_ref(A1);

  if ( PL_get_intptr_ex(A2, &n) )
  { end = A3;
    options = A4;
  } else
    return false;

  if ( options &&
       !PL_scan_options(options, 0, "numbervar_option", numbervar_options,
			&av, &name, &opts.singletons) )
    fail;

  if ( av == ATOM_error )
    opts.on_attvar = AV_ERROR;
  else if ( av == ATOM_skip )
    opts.on_attvar = AV_SKIP;
  else if ( av == ATOM_bind )
    opts.on_attvar = AV_BIND;
  else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_numbervar_option, options);

  if ( opts.singletons )		/* Hack */
  { if ( !is_acyclic(valTermRef(A1)) )
      opts.singletons = false;
  }

  opts.functor = PL_new_functor(name, 1);
  n = numberVars(t, &opts, n);
  if ( n != NV_ERROR )
    return PL_unify_int64(end, n);

  return false;
}


static
PRED_IMPL("var_number", 2, var_number, 0)
{ PRED_LD
  term_t t = A1;
  Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);

    if ( f->definition == FUNCTOR_isovar1 )
    { Word a = &f->arguments[0];

      deRef(a);
      if ( isAtom(*a) || isInteger(*a) )
	return PL_unify_atomic(A2, *a);
    }
  }

  return false;
}

		 /*******************************
		 *	      TEMPLATE		*
		 *******************************/

/** '$unbind_template'(+Template) is det.
 *
 * Assuming Template is a  term  v(A1,  A2,   ...)  where  A1,  ...  are
 * initially references into another  term   for  example  created using
 * term_variables/2 followed by =../2.  This   predicate  is designed to
 * unify the template with concrete values   and subsequently unbind the
 * template such that we can unify it with  a new set of concrete values
 * without copying the entire term.
 *
 * @see used by foreach/2.
 */

static
PRED_IMPL("$unbind_template", 1, unbind_template, 0)
{ PRED_LD
  Word p = valTermRef(A1);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    size_t i, arity = arityFunctor(f->definition);

    for(i=0; i<arity; i++)
    { word w = f->arguments[i];

      if ( isRef(w) )
      { Word tp = unRef(w);

	setVar(*tp);
      } else
      { return PL_type_error("template", A1);
      }
    }
  } else if ( !isAtom(*p) )
  { return PL_type_error("template", A1);
  }

  return true;
}



		 /*******************************
		 *	   TERM-VARIABLES	*
		 *******************************/

#define TV_ATTVAR    0x1		/* attributed vars only */
#define TV_SINGLETON 0x2		/* singletons */
#define TV_SHARED    0x4		/* non-singletons */
#define TV_ATTS      0x8		/* recurse into attributes */
#define TV_EXCEPTION ((size_t)-1)
#define TV_NOSPACE   ((size_t)-2)
#define TV_NOMEM     ((size_t)-3)
#define TV_EINTR     ((size_t)-4)

#define term_variables_loop(agenda, maxcount, flags) \
	LDFUNC(term_variables_loop, agenda, maxcount, flags)

static size_t
term_variables_loop(DECL_LD term_agenda *agenda, size_t maxcount, int flags)
{ Word p;
  size_t count = 0;
  size_t progress = 0;

  while( (p=nextTermAgenda(agenda)) )
  { word w;

  again:
    w = *p;

    if ( canBind(w) )
    { term_t v;

      if ( visitedWord(p) )
      { if ( (flags&(TV_SINGLETON|TV_SHARED)) )
	  (*p) |= FIRST_MASK;
	continue;
      }

      if ( (flags&TV_ATTVAR) && !isAttVar(w) )
	continue;

      if ( ++count > maxcount )
	return count;
      if ( !(v = PL_new_term_ref_noshift()) )
	return TV_NOSPACE;
      *valTermRef(v) = makeRefG(p);
      if ( (flags&TV_ATTS) && isAttVar(w) )
      { Word p2 = valPAttVar(w);
	deRef2(p2, p);
	goto again;
      }
    } else if ( isTerm(w) )
    { Functor f = valueTerm(w);

      if ( visited(f) && !(flags&(TV_SINGLETON|TV_SHARED)) )
	continue;
      if ( (++progress % 32768) == 0 && is_signalled() && !LD->critical )
	return TV_EINTR;
      if ( !pushWorkAgenda(agenda, arityFunctor(f->definition), f->arguments) )
	return TV_NOMEM;
    }
  }

  return count;
}


#define term_variables_to_termv(t, vp, maxcount, flags) LDFUNC(term_variables_to_termv, t, vp, maxcount, flags)
static size_t
term_variables_to_termv(DECL_LD term_t t, term_t *vp, size_t maxcount, int flags)
{ term_agenda agenda;
  term_t v0   = PL_new_term_refs(0);
  size_t count;

  initvisited();
  initTermAgenda(&agenda, 1, valTermRef(t));
  count = term_variables_loop(&agenda, maxcount, flags);
  clearTermAgenda(&agenda);
  if ( (flags&(TV_SINGLETON|TV_SHARED)) && (ssize_t)count >= 0 )
  { size_t o = 0;
    size_t i;

    for(i=0; i<count; i++)
    { Word p = valTermRef(v0+i);

      assert(isRef(*p));
      p = unRef(*p);
      if ( (!((*p)&FIRST_MASK) && (flags&TV_SINGLETON)) ||
	   (((*p)&FIRST_MASK)  && (flags&TV_SHARED)) )
      { if ( o != i )
	  *valTermRef(v0+o) = *valTermRef(v0+i);
	o++;
      }
    }
    if ( o < i )
      PL_reset_term_refs(v0+o);

    count = o;
  }
  unvisit_and_unfirst();

  *vp = v0;
  return count;
}



#define term_variables(t, vars, tail, flags) LDFUNC(term_variables, t, vars, tail, flags)
static int
term_variables(DECL_LD term_t t, term_t vars, term_t tail, int flags)
{ term_t list = PL_copy_term_ref(vars);
  term_t head = PL_new_term_ref();
  term_t v0;
  size_t i, maxcount, count;

  if ( !(!tail && PL_skip_list(vars, 0, &maxcount) == PL_LIST) ||
       (flags&TV_SINGLETON) )
    maxcount = ~0;

  for(;;)
  { count = term_variables_to_termv(t, &v0, maxcount, flags);
    if ( count == TV_EXCEPTION )
      return false;
    if ( count == TV_NOSPACE )
    { PL_reset_term_refs(v0);
      if ( !makeMoreStackSpace(LOCAL_OVERFLOW, ALLOW_SHIFT) )
	return false;			/* GC doesn't help */
      continue;
    }
    if ( count == TV_EINTR )
    { PL_reset_term_refs(v0);
      if ( PL_handle_signals() < 0 )
	return false;
      assert(!is_signalled());
      continue;
    }
    if ( count == TV_NOMEM )
      return PL_error(NULL, 0, NULL, ERR_NOMEM);
    if ( count > maxcount )
      return false;
    break;
  }

  for(i=0; i<count; i++)
  { if ( !PL_unify_list(list, head, list) ||
	 !PL_unify(head, v0+i) )
      return false;
  }
  PL_reset_term_refs(head);

  if ( tail )
    return PL_unify(list, tail);
  else
    return PL_unify_nil(list);
}


static
PRED_IMPL("term_variables", 2, term_variables2, PL_FA_ISO)
{ PRED_LD

  return term_variables(A1, A2, 0, 0);
}


static
PRED_IMPL("term_variables", 3, term_variables3, 0)
{ PRED_LD

  return term_variables(A1, A2, A3, 0);
}

static
PRED_IMPL("$term_attvar_variables", 2, term_attvar_variables, 0)
{ PRED_LD

  return term_variables(A1, A2, 0, TV_ATTS);
}


static
PRED_IMPL("term_singletons", 2, term_singletons, 0)
{ PRED_LD

  if ( PL_is_acyclic(A1) )
    return term_variables(A1, A2, 0, TV_SINGLETON);
  else
    return PL_representation_error("acyclic_term");
}


static
PRED_IMPL("$term_multitons", 2, term_multitons, 0)
{ PRED_LD

  if ( PL_is_acyclic(A1) )
    return term_variables(A1, A2, 0, TV_SHARED);
  else
    return PL_representation_error("acyclic_term");
}


static
PRED_IMPL("term_attvars", 2, term_attvars, 0)
{ PRED_LD

  return term_variables(A1, A2, 0, TV_ATTVAR|TV_ATTS);
}


#define is_most_general_term(p) LDFUNC(is_most_general_term, p)
static int
is_most_general_term(DECL_LD Word p)
{ deRef(p);

  if ( isAtom(*p) )
    return true;

  if ( isTerm(*p) )
  { Functor t = valueTerm(*p);

    if ( t->definition == FUNCTOR_dot2 )
    { Word tail;

      (void)skip_list(p, &tail);

      if ( isNil(*tail) )
      { Word l = p;
	int rc = true;

	while( isList(*l) )
	{ Word h = HeadList(l);

	  deRef(h);
	  if ( !isVar(*h) )
	  { rc = false;
	    break;
	  }
	  set_marked(h);
	  l = TailList(l);
	  deRef(l);
	}

	l = p;
	while( isList(*l) )
	{ Word h = HeadList(l);

	  deRef(h);
	  if ( is_marked(h) )
	  { clear_marked(h);
	    l = TailList(l);
	    deRef(l);
	  } else
	  { break;
	  }
	}

	return rc;
      }
    } else
    { size_t arity = arityFunctor(t->definition);
      size_t i, j;
      int rc = true;

      for(i=0; i<arity; i++)
      { Word a = &t->arguments[i];

	deRef(a);
	if ( !isVar(*a) )
	{ rc = false;
	  break;
	}
	set_marked(a);
      }
      for(j=0; j<i; j++)
      { Word a = &t->arguments[j];

	deRef(a);
	clear_marked(a);
      }

      return rc;
    }
  }

  return false;
}


static
PRED_IMPL("is_most_general_term", 1, is_most_general_term, 0)
{ PRED_LD

  return is_most_general_term(valTermRef(A1));
}


		 /*******************************
		 *	      SUBSUMES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
subsumes is defined as

subsumes(General, Specific) :-
	term_variables(Specific, SVars),
	General = Specific,
	term_variables(SVars, SVars).

Below is the implementation, but we keep  the array of variables instead
of creating an array and we check whether these are all unique variables
by scanning the array.  This saves both time and space.

We tried to do this using   a one-sided unification implementation. Most
of this is fairly trivial, but  we  must   make  sure  we know when left
argument (general) becomes a pointer  in   the  specific term. There are
three cases for this to happen. One  is following a cycle-reference, two
is following a previously bound term and  three is following a reference
pointer from a variable that  was   shared  between general and specific
before the entry of subsumes/2. The first  two are easily fixed. I don't
know how to fix the latter without a   complete  scan on specific. If we
need to do that anyway,  we  can  just   as  well  use  the below simple
algorithm.

We can enhance on this by combining this with the one-sided unification.
We could delay scanning specific until we  bind the first variable. This
will not have any significant  inpact   on  performance for a succeeding
subsumes check, but can result in early failure and avoiding the scan of
specific. This works because  the   one-sided  unification algorithm can
only succeed in places where it should fail.

The latest version of the old algorithm is in the GIT commit

	f68eb71a9d5d0b9b6055483842d9654c30e29550
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define subsumes(general, specific) LDFUNC(subsumes, general, specific)
static int
subsumes(DECL_LD term_t general, term_t specific)
{ term_t v0;
  size_t i, n;
  term_t ex = 0;
  int rc;
  int omode;

  for(;;)
  { n = term_variables_to_termv(specific, &v0, ~0, 0);
    if ( n == TV_EXCEPTION )
      return false;
    if ( n == TV_NOSPACE )
    { PL_reset_term_refs(v0);
      if ( !makeMoreStackSpace(LOCAL_OVERFLOW, ALLOW_SHIFT) )
	return false;			/* GC does not help */
      continue;
    }
    if ( n == TV_NOMEM )
      return PL_error(NULL, 0, NULL, ERR_NOMEM);
    break;
  }

  omode = LD->prolog_flag.occurs_check;
  LD->prolog_flag.occurs_check = OCCURS_CHECK_FALSE;
  rc = PL_unify(general, specific);
  LD->prolog_flag.occurs_check = omode;

  if ( rc &&
       (ex = PL_new_term_ref()) &&
       foreignWakeup(ex) )
  { int rc = true;

    initvisited();
    for(i=0; i<n; i++)
    { Word p = valTermRef(v0+i);
      deRef(p);

      if ( !canBind(*p) || visitedWord(p) )
      { rc = false;
	break;
      }
    }
    unvisit();
    return rc;
  }

  if ( ex && !PL_is_variable(ex) )
    return PL_raise_exception(ex);

  fail;
}


static
PRED_IMPL("subsumes_term", 2, subsumes_term, PL_FA_ISO)
{ PRED_LD
  int rc;
  fid_t fid;

  fid = PL_open_foreign_frame();
  rc = subsumes(A1, A2);
  PL_discard_foreign_frame(fid);

  return rc;
}


/** free_variable_set(+Template^GoalIn, -GoalOut, -VarTemplate)

This implements _|free variable set|_ as   defined the ISO core standard
(sec. 7.1.1.4) for setof/3 and  bagof/3. This demands ^/2-quantification
to be on the outside (except for M:) and removes ^/2 from the goal-term.
The latter implies that we no longer need ^/2 as a predicate.
*/

#define free_variables_loop(t, mname, goal) LDFUNC(free_variables_loop, t, mname, goal)
static size_t
free_variables_loop(DECL_LD Word t, atom_t *mname, term_t goal)
{ term_agenda agenda;
  int in_goal = false;
  int existential = false;		/* true when processing left of ^ */
  size_t n = 0;
  word mark = 0;			/* mark that tells us we completed vars */

  initTermAgenda(&agenda, 1, t);
  while((t=nextTermAgenda(&agenda)))
  { if ( t == &mark )
    { existential = false;
      continue;
    }

  again:
    if ( canBind(*t) )
    { term_t v;

      if ( !visitedWord(t) && !existential )
      { if ( !(v = PL_new_term_ref_noshift()) )
	{ n = TV_NOSPACE;
	  goto out;
	}
	*valTermRef(v) = makeRefG(t);

	n++;
      }

      continue;
    }

    if ( isTerm(*t) )
    { Functor f = valueTerm(*t);
      functor_t fd = word2functor(f->definition); /* modified by visited */

      if ( visited(f) )
      { if ( !in_goal && !existential )
	{ *valTermRef(goal) = *t;
	  in_goal = true;
	}
	continue;
      }

      if ( !in_goal )
      { if ( fd == FUNCTOR_hat2 && existential == false )
	{ if ( !pushWorkAgenda(&agenda, 1, &f->arguments[1]) ||
	       !pushWorkAgenda(&agenda, 1, &mark) ||
	       !pushWorkAgenda(&agenda, 1, &f->arguments[0]) )
	    return TV_NOMEM;
	  existential = true;
	  continue;
	}
	if ( fd == FUNCTOR_colon2 && !existential )
	{ Word a1;

	  deRef2(&f->arguments[0], a1);
	  if ( isAtom(*a1) )
	    *mname = word2atom(*a1);
	  t = &f->arguments[1];
	  goto again;
	} else if ( !existential )
	{ *valTermRef(goal) = *t;
	  in_goal = true;
	}
      }

      if ( !pushWorkAgenda(&agenda, arityFunctor(fd), f->arguments) )
	return TV_NOMEM;

      continue;
    } else if ( !in_goal && !existential) /* non-term goal (atom or invalid) */
    { *valTermRef(goal) = needsRef(*t) ? makeRefG(t) : *t;
      in_goal = true;
    }
  }

out:
  clearTermAgenda(&agenda);

  return n;
}


static
PRED_IMPL("$free_variable_set", 3, free_variable_set, 0)
{ GET_LD

  for(;;)
  { term_t goal = PL_new_term_ref();
    term_t v0 = PL_new_term_refs(0);
    size_t n;
    atom_t mname = (atom_t)0;

    initvisited();
    n = free_variables_loop(valTermRef(A1), &mname, goal);
    unvisit();
    if ( n == TV_NOSPACE )
    { PL_reset_term_refs(goal);
      if ( !makeMoreStackSpace(LOCAL_OVERFLOW, ALLOW_SHIFT) )
	return false;
      continue;
    }
    if ( n == TV_NOMEM )
      return PL_error(NULL, 0, NULL, ERR_NOMEM);

    if ( PL_unify_functor(A3, PL_new_functor(ATOM_v, (int)n)) )
    { int i, m = (int)n;

      for(i=0; i<m; i++)
      { if ( !PL_unify_arg(i+1, A3, v0+i) )
	  return false;
      }

      if ( mname )
      { term_t m = PL_new_term_ref();

	PL_put_atom(m, mname);
	if ( !PL_cons_functor(goal, FUNCTOR_colon2, m, goal) )
	  return false;
      }

      return PL_unify(A2, goal);
    }
    return false;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unifiable(@X, @Y, -Substitution)

If X can be unified to Y, unify   Substitution with a list of Variable =
value for the substitutions that must be made to make X and Y identical.

The implementation extracts the substitutions  from the trail, rewinding
the trail at the same  time.  This   is  fairly  trivial, except for the
assignments of attributed variables (assignAttVar()). The last operation
of assignAttVar() is a trailed assignment  replacing the attvar with its
value. Before that it performs two trailed  actions to update the wakeup
list. These two must be skipped.

Unfortunately, if a value is unified to   a  local stack variable (which
can only be the case if one of the arguments is a plain variable) things
get very complicated. Therefore we test   these  cases before going into
the trouble. Note that unifying attributed   variables  is no problem as
these always live on the global stack.

(*) Unfortunately, we cannot handle  shift/GC   during  this process. In
particular, if we  need  space  for   the  result-list,  we  cannot call
allocGlobal(), because the resulting  GC  will   do  early-reset  on the
trailed variables and thus invalidate our nice   and clean trail. So, if
there is no space we rewind and retry the whole process.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define unifiable_occurs_check(t1, t2) LDFUNC(unifiable_occurs_check, t1, t2)
static int
unifiable_occurs_check(DECL_LD term_t t1, term_t t2)
{ switch(LD->prolog_flag.occurs_check)
  { case OCCURS_CHECK_FALSE:
      return true;
    case OCCURS_CHECK_TRUE:
    case OCCURS_CHECK_ERROR:
    { Word p1 = valTermRef(t1);
      Word p2 = valTermRef(t2);

      deRef(p1);
      if ( !var_occurs_in(p1, p2) )
	return true;

      return failed_unify_with_occurs_check(p1, p2,
					    LD->prolog_flag.occurs_check);
    }
    default:
      assert(0);
      fail;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Same as unify_ptrs(), but ensures that   all  assignments are trailed by
setting LD->mark_bar to the top  of   the  memory. Note that NO_MARK_BAR
also needs support in garbageCollect() and growStacks().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define unify_all_trail_ptrs(t1, t2, m) LDFUNC(unify_all_trail_ptrs, t1, t2, m)
static bool
unify_all_trail_ptrs(DECL_LD term_t t1, term_t t2, mark *m)
{ for(;;)
  { int rc;

    Mark(*m);
    LD->mark_bar = NO_MARK_BAR;
    rc = raw_unify_ptrs(valTermRef(t1), valTermRef(t2));
    if ( rc == true )			/* Terms unified */
    { return rc;
    } else if ( rc == false )		/* Terms did not unify */
    { if ( !exception_term )		/* Check for occurs error */
	Undo(*m);
      DiscardMark(*m);
      return rc;
    } else				/* Stack overflow */
    { int rc2;

      Undo(*m);
      DiscardMark(*m);
      rc2 = makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT);
      if ( !rc2 )
	return false;
    }
  }
}


#define unifiable(t1, t2, subst) LDFUNC(unifiable, t1, t2, subst)
static ssize_t
unifiable(DECL_LD term_t t1, term_t t2, term_t subst)
{ mark m;
  int rc;

  if ( PL_is_variable(t1) )
  { if ( PL_compare(t1, t2) == 0 )
    { return PL_unify_atom(subst, ATOM_nil);
    } else
    { if ( !unifiable_occurs_check(t1, t2) )
	fail;

      return PL_unify_term(subst,
			   PL_FUNCTOR, FUNCTOR_dot2,
			     PL_FUNCTOR, FUNCTOR_equals2,
			       PL_TERM, t1,
			       PL_TERM, t2,
			     PL_ATOM, ATOM_nil);
    }
  }
  if ( PL_is_variable(t2) )
  { if ( !unifiable_occurs_check(t2, t1) )
      fail;

    return PL_unify_term(subst,
			 PL_FUNCTOR, FUNCTOR_dot2,
			   PL_FUNCTOR, FUNCTOR_equals2,
			     PL_TERM, t2,
			     PL_TERM, t1,
			   PL_ATOM, ATOM_nil);
  }

retry:
  if ( unify_all_trail_ptrs(t1, t2, &m) )
  { TrailEntry tt = tTop;
    TrailEntry mt = m.trailtop.as_ptr;

    if ( tt > mt )
    { ssize_t needed = (tt-mt)*6+1;
      Word list, gp, tail;

      if ( !hasGlobalSpace(needed) )	/* See (*) */
      { int rc = overflowCode(needed);

	Undo(m);
	DiscardMark(m);
	rc = makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT);
	if ( rc )
	  goto retry;
	return false;
      }

      DiscardMark(m);
      tail = list = gTop;
      gp = list+1;

      *list = ATOM_nil;
      while(--tt >= mt)
      { Word p = tt->address;

	*tail = consPtr(&gp[0], TAG_COMPOUND|STG_GLOBAL);
	gp[0] = FUNCTOR_dot2;
	gp[1] = consPtr(&gp[3], TAG_COMPOUND|STG_GLOBAL);
	gp[2] = ATOM_nil;
	tail = &gp[2];
	gp[3] = FUNCTOR_equals2;
	if ( isTrailVal(p) )
	{ Word p2 = tt[-1].address;
	  gp[4] = makeRefG(p2);
	  gp[5] = *p2;
	} else
	{ gp[5] = *p;
	  assert(onGlobalArea(p));
	  gp[4] = makeRefG(p);
	  setVar(*p);
	}
	gp += 6;

	if ( isTrailVal(p) )
	{ assert(isAttVar(trailVal(p)));

	  tt--;				/* re-insert the attvar */
	  *tt->address = trailVal(p);

	  tt--;				/* restore tail of wakeup list */
	  p = tt->address;
	  if ( isTrailVal(p) )
	  { tt--;
	    *tt->address = trailVal(p);
	  } else
	  { setVar(*p);
	  }

	  tt--;				/* restore head of wakeup list */
	  p = tt->address;
	  if ( isTrailVal(p) )
	  { tt--;
	    *tt->address = trailVal(p);
	  } else
	  { setVar(*p);
	  }

	  assert(tt>=mt);
	}
      }
      gTop = gp;			/* may not have used all space */
      tTop = m.trailtop.as_ptr;

      rc = PL_unify(pushWordAsTermRef(list), subst);
      popTermRef();

      return rc;
    } else
    { DiscardMark(m);
      return PL_unify_atom(subst, ATOM_nil);
    }
  } else
  { return false;
  }
}


static
PRED_IMPL("unifiable", 3, unifiable, 0)
{ PRED_LD

  return unifiable(A1, A2, A3);
}



		 /*******************************
		 *	       ATOMS		*
		 *******************************/

#define text_length(s, len, flags) \
	LDFUNC(text_length, s, len, flags)

static foreign_t
text_length(DECL_LD term_t s, term_t len, int flags)
{ PL_chars_t txt;

  flags |= CVT_EXCEPTION|BUF_ALLOW_STACK;

  if ( PL_get_text(s, &txt, flags) )
  { int rc = PL_unify_int64_ex(len, PL_text_length(&txt));

    PL_free_text(&txt);

    return rc;
  }

  return false;
}

static
PRED_IMPL("atom_length", 2, atom_length, PL_FA_ISO)
{ PRED_LD
  int flags;

  if ( truePrologFlag(PLFLAG_ISO) )
    flags = CVT_ATOM|CVT_STRING;
  else
    flags = CVT_ALL;

  return text_length(A1, A2, flags);
}


#define	X_AUTO		   0x00
#define	X_ATOM		   0x01
#define	X_NUMBER	   0x02
#define	X_MASK		   0x0f
#define	X_CHARS		   0x10
#define	X_STRING	   0x20
#define	X_NO_SYNTAX_ERROR  0x40
#define X_NO_LEADING_WHITE 0x80

#define x_chars(pred, atom, string, how) \
	LDFUNC(x_chars, pred, atom, string, how)

static int
x_chars(DECL_LD const char *pred, term_t atom, term_t string, int how)
{ PL_chars_t atext, stext;
  int arg1;
  int flags2 = CVT_STRING|CVT_LIST|CVT_EXCEPTION;

  arg1 = PL_get_text(atom, &atext,
		     (how & X_NUMBER) ? CVT_NUMBER : CVT_ATOMIC);

  if ( arg1 )					/* +,? */
  { int ok;
    int out_type;
    fid_t fid = PL_open_foreign_frame();

    out_type = (how&X_CHARS ? PL_CHAR_LIST :
		how&X_STRING ? PL_STRING : PL_CODE_LIST);

    ok = PL_unify_text(string, 0, &atext, out_type);

    if ( ok )
    { PL_close_foreign_frame(fid);
      return ok;
    }
    flags2 |= CVT_VARNOFAIL;
    PL_discard_foreign_frame(fid);
  } else if ( !PL_is_variable(atom) )
  { atom_t type;

    how &= X_MASK;
    type = (how == X_ATOM   ? ATOM_atom :
	    how == X_NUMBER ? ATOM_number :
			      ATOM_atomic);

    return PL_error(pred, 2, NULL, ERR_TYPE, type, atom);
  }

  if ( PL_get_text(string, &stext, flags2) != true )
    return false;

  switch(how&X_MASK)
  { case X_ATOM:
    case_atom:
    { int rc = PL_unify_text(atom, 0, &stext, PL_ATOM);
      PL_free_text(&stext);
      return rc;
    }
    case X_NUMBER:
    case X_AUTO:
    { strnumstat nrc = NUM_ERROR;

      if ( stext.encoding == ENC_ISO_LATIN_1 ||
	   stext.encoding == ENC_UTF8 )
      { const char *s;
	unsigned char *q;
	number n;

      utf8:
	s = stext.text.t;

	/* ISO: number_codes(X, "  42") */
	if ( (how&X_MASK) == X_NUMBER && !(how&X_NO_LEADING_WHITE) )
	  s = utf8_skip_blanks(s);

	if ( (nrc=str_number((const unsigned char*)s, &q, &n, 0)) == NUM_OK )
	{ if ( (char*)q == stext.text.t + stext.length )
	  { int rc = PL_unify_number(atom, &n);
	    clearNumber(&n);
	    PL_free_text(&stext);
	    return rc;
	  }
	  clearNumber(&n);
	  nrc = NUM_ERROR;
	}
      } else if ( stext.encoding == ENC_WCHAR )
      { const wchar_t *ws = stext.text.w;
	const wchar_t *we = ws + stext.length;
	int c;

	if ( (how&X_MASK) == X_NUMBER && !(how&X_NO_LEADING_WHITE) )
	{ while(ws < we)
	  { const wchar_t *ce = get_wchar(ws, &c);
	    if ( isBlankW(c) )
	      ws = ce;
	    else
	      break;
	  }
	}

	if ( ws < we )
	{ const wchar_t *ce = get_wchar(ws, &c);
	  if ( c == '-' || c == '+' )
	    ws = ce;
	}

	if ( ws < we )
	{ get_wchar(ws, &c);

	  if ( f_is_decimal(c) )
	  { if ( !PL_mb_text(&stext, REP_UTF8) )
	    { PL_free_text(&stext);
	      return false;
	    }
	    goto utf8;
	  }
	}
      }

      if ( (how&X_MASK) == X_AUTO )
      { goto case_atom;
      } else
      { PL_free_text(&stext);

	if ( !(how & X_NO_SYNTAX_ERROR) )
	  return PL_error(pred, 2, NULL, ERR_SYNTAX, str_number_error(nrc));
	else
	  return false;
      }
    }
    default:
      assert(0);
      return false;
  }
}


static
PRED_IMPL("name", 2, name, 0)
{ PRED_LD
  return x_chars("name", A1, A2, X_AUTO);
}


static
PRED_IMPL("atom_chars", 2, atom_chars, PL_FA_ISO)
{ PRED_LD
  return x_chars("atom_chars", A1, A2, X_ATOM|X_CHARS);
}


static
PRED_IMPL("atom_codes", 2, atom_codes, PL_FA_ISO)
{ PRED_LD
  return x_chars("atom_codes", A1, A2, X_ATOM);
}


static
PRED_IMPL("number_chars", 2, number_chars, PL_FA_ISO)
{ PRED_LD
  return x_chars("number_chars", A1, A2, X_NUMBER|X_CHARS);
}


static
PRED_IMPL("number_codes", 2, number_codes, PL_FA_ISO)
{ PRED_LD
  return x_chars("number_codes", A1, A2, X_NUMBER);
}


static
PRED_IMPL("number_string", 2, number_string, 0)
{ PRED_LD
  return x_chars("number_string", A1, A2,
		 X_NUMBER|X_STRING|X_NO_SYNTAX_ERROR|X_NO_LEADING_WHITE);
}


static
PRED_IMPL("char_code", 2, char_code, PL_FA_ISO)
{ PRED_LD
  int n;
  term_t atom = A1;
  term_t chr  = A2;
  int vatom = PL_is_variable(atom);
  int vchr  = PL_is_variable(chr);
  int achr = -1;
  int cchr = -1;

  if ( vatom && vchr )
    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);

  if ( !vatom )
  { atom_t a;

    if ( !PL_get_atom(atom, &a) || (achr = charCode(a)) == -1 )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_character, atom);
  }

  if ( !vchr )
  { if ( !PL_get_integer_ex(chr, &n) )
      return false;

    if ( !VALID_CODE_POINT(n) )
      return PL_type_error("character_code", chr);

    cchr = n;
  }

  if ( achr == cchr )
    return true;
  if ( vatom )
    return PL_unify_atom(atom, codeToAtom(cchr));
  else
    return PL_unify_integer(chr, achr);
}


static int
is_code(word w)
{ if ( isTaggedInt(w) )
  { sword code = valInt(w);

    return VALID_CODE_POINT(code);
  }

  return false;
}

static int
is_char(word w)
{ PL_chars_t text;

  return ( isAtom(w) &&
	   get_atom_text(word2atom(w), &text) &&
	   PL_text_length(&text) == 1
	 );
}

static
PRED_IMPL("$is_char_code", 1, is_char_code, 0)
{ PRED_LD;
  Word p = valTermRef(A1);

  deRef(p);
  return is_code(*p);

  return false;
}

static
PRED_IMPL("$is_char", 1, is_char, 0)
{ PRED_LD;
  Word p = valTermRef(A1);

  deRef(p);
  return is_char(*p);
}


#define is_text_list(text, lent, test) LDFUNC(is_text_list, text, lent, test)
static int
is_text_list(DECL_LD term_t text, term_t lent, int (*test)(word))
{ Word p = valTermRef(text);
  intptr_t len = 0;

  deRef(p);
  while(isList(*p))
  { Word av = HeadList(p);
    Word h;

    deRef2(av, h);
    if ( !(*test)(*h) )
      return false;
    deRef2(av+1, p);

    if ( ++len == 1000 )
    { Word tail;
      skip_list(p, &tail);
      if ( !isNil(*tail) )
	return false;
    }
  }
  return ( isNil(*p) &&
	   PL_unify_int64(lent, len) );
}

static
PRED_IMPL("$is_code_list", 2, is_code_list, 0)
{ PRED_LD

  return is_text_list(A1, A2, is_code);
}

static
PRED_IMPL("$is_char_list", 2, is_char_list, 0)
{ PRED_LD

  return is_text_list(A1, A2, is_char);
}


static
PRED_IMPL("atom_number", 2, atom_number, 0)
{ PRED_LD
  char *s;
  size_t len;

  if ( PL_get_nchars(A1, &len, &s, REP_UTF8|CVT_ATOM|CVT_STRING) )
  { number n;
    unsigned char *q;
    strnumstat rc;

    if ( (rc=str_number((unsigned char *)s, &q, &n, 0) == NUM_OK) )
    { if ( *q == EOS )
      { int rc = PL_unify_number(A2, &n);
	clearNumber(&n);

	return rc;
      } else
      { clearNumber(&n);
	return false;
      }
    } else
    { return false;
    }
  } else if ( PL_get_nchars(A2, &len, &s, CVT_NUMBER) )
  { return PL_unify_atom_nchars(A1, len, s);
  }

  if ( !PL_is_variable(A2) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_number, A2);
  else if ( !PL_is_atom(A1) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, A1);
  else
    return false;
}

/* MacOS X Mavericks and Yosemite write a char (nul) too many if the
 * buffer is short.  Thanks to Samer Abdallah for sorting this out.
 *
 * (*) On failure, wcsxfrm() normally returns the required size.
 * The official docs are not completely explicit about this though
 * and it appears MacOS 15 (beta) does not, requiring another
 * iteration.  We now allow for 5 iterations ...
 */
#ifdef __APPLE__
#define WCSXFRM_BUFFER_OVERRUN 1
#else
#define WCSXFRM_BUFFER_OVERRUN 0
#endif

static
PRED_IMPL("collation_key", 2, collation_key, 0)
{
#ifdef HAVE_WCSXFRM
  wchar_t *s;
  size_t len;
  wchar_t buf[256];
  size_t buflen = sizeof(buf)/sizeof(wchar_t) - WCSXFRM_BUFFER_OVERRUN;
  wchar_t *o = buf;
  size_t n;

  if ( !PL_get_wchars(A1, &len, &s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
    fail;
  for(;;)
  { if ( (n=wcsxfrm(o, s, buflen)) < buflen )
    { int rc = PL_unify_wchars(A2, PL_STRING, n, o);

      if ( o != buf )
	free(o);

      return rc;
    } else
    { if ( o == buf )			/* see (*) */
      { buflen = n+1;
      } else
      {	free(o);			/* we get a nonsense return */
	buflen *= 2;			/* so, double */
      }
      if ( !(o = malloc(buflen*sizeof(wchar_t))) )
	return PL_no_memory();
    }
  }
#else
  GET_LD
  return PL_unify(A1, A2);
#endif
}

#define SIZE_NOT_SET  ((size_t)-1)
#define SIZE_GIVEN(v) ((v) != SIZE_NOT_SET)

#define concat(a1, a2, a3, bidirectional, ctx, accept, otype) \
	LDFUNC(concat, a1, a2, a3, bidirectional, ctx, accept, otype)

static foreign_t
concat(DECL_LD term_t a1, term_t a2, term_t a3,
       int bidirectional,		/* false: only mode +,+,- */
       control_t ctx,
       int accept,			/* CVT_* */
       int otype)			/* PL_ATOM or PL_STRING */
{ PL_chars_t t1, t2, t3;
  int rc;
  int inmode = bidirectional ? CVT_VARNOFAIL : 0;
  size_t l1 = SIZE_NOT_SET;
  size_t l2 = SIZE_NOT_SET;
  size_t l3 = SIZE_NOT_SET;

#define L1 (SIZE_GIVEN(l1) ? l1 : (l1=PL_text_length(&t1)))
#define L2 (SIZE_GIVEN(l2) ? l2 : (l2=PL_text_length(&t2)))
#define L3 (SIZE_GIVEN(l3) ? l3 : (l3=PL_text_length(&t3)))

  if ( ForeignControl(ctx) == FRG_CUTTED )
    succeed;

  t1.text.t = t2.text.t = t3.text.t = NULL;

  if ( !PL_get_text(a1, &t1, accept|inmode|CVT_EXCEPTION) ||
       !PL_get_text(a2, &t2, accept|inmode|CVT_EXCEPTION) ||
       !PL_get_text(a3, &t3, accept|CVT_EXCEPTION|CVT_VARNOFAIL) )
    fail;

  if ( t1.text.t && t2.text.t )
  { if ( t3.text.t )
    { rc = ( L1 + L2 == L3 &&
	     PL_cmp_text(&t1, 0, &t3,  0, L1) == 0 &&
	     PL_cmp_text(&t2, 0, &t3, L1, L2) == 0 );
      goto out;
    } else
    { PL_chars_t c;
      PL_chars_t *v[2];

      v[0] = &t1;
      v[1] = &t2;

      PL_concat_text(2, v, &c);

      rc = PL_unify_text(a3, 0, &c, otype);
      PL_free_text(&c);
      goto out;
    }
  }

  if ( !t3.text.t )
    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);

  if ( t1.text.t )			/* +, -, + */
  { (void)L1;
    if ( l1 <= L3 &&
	 PL_cmp_text(&t1, 0, &t3, 0, l1) == 0 )
      return PL_unify_text_range(a2, &t3, l1, L3-l1, otype);
    fail;
  } else if ( t2.text.t )		/* -, +, + */
  { (void)L2;
    if ( l2 <= L3 &&
	 PL_cmp_text(&t2, 0, &t3, L3-l2, l2) == 0 )
      return PL_unify_text_range(a1, &t3, 0, L3-l2, otype);
    fail;
  } else				/* -, -, + */
  { size_t at_n;

    switch ( ForeignControl(ctx) )
    { case FRG_FIRST_CALL:
	if ( PL_same_term(a1, a2) )	/* sharing variables */
	{ if ( L3 % 2 )
	  { rc = false;
	    goto out;
	  } else
	  { at_n = L3/2;
	    if ( PL_cmp_text(&t3, 0, &t3, at_n, at_n) == 0 )
	    { PL_unify_text_range(a1, &t3, 0, at_n, otype);
	      rc = true;
	    } else
	    { rc = false;
	    }
	    goto out;
	  }
	}
	at_n = 0;
	break;
      case FRG_REDO:
	at_n = ForeignContextInt(ctx);
	break;
      default:
	succeed;
    }

    PL_unify_text_range(a2, &t3, at_n, L3-at_n, otype);
    PL_unify_text_range(a1, &t3, 0,    at_n, otype);
    if ( at_n < L3 )
      ForeignRedoInt(at_n+1);

    rc = true;
  }

out:
  if ( t1.text.t ) PL_free_text(&t1);
  if ( t2.text.t ) PL_free_text(&t2);
  if ( t3.text.t ) PL_free_text(&t3);

#undef L1
#undef L2
#undef L3

  return rc;
}


static
PRED_IMPL("atom_concat", 3, atom_concat, PL_FA_NONDETERMINISTIC|PL_FA_ISO)
{ PRED_LD

  return concat(A1, A2, A3, true, PL__ctx, CVT_ATOMIC, PL_ATOM);
}


static
PRED_IMPL("atomic_concat", 3, atomic_concat, PL_FA_ISO)
{ PRED_LD

  return concat(A1, A2, A3, false, PL__ctx, CVT_ATOMIC, PL_ATOM);
}


#define split_atom(list, st, atom) LDFUNC(split_atom, list, st, atom)
static int
split_atom(DECL_LD term_t list, PL_chars_t *st, term_t atom)
{ PL_chars_t at;
  size_t i, last;
  term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  size_t sep_len = st->length;

  if ( !PL_get_text(atom, &at, CVT_ATOMIC|CVT_EXCEPTION) )
    return false;

  for(last=i=0; (ssize_t)i<=(ssize_t)(at.length-sep_len); )
  { if ( PL_cmp_text(st, 0, &at, i, sep_len) == 0 )
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_text_range(head, &at, last, i-last, PL_ATOM) )
	fail;
      i += sep_len;
      last = i;
    } else
      i++;
  }

  if ( !PL_unify_list(tail, head, tail) ||
       !PL_unify_text_range(head, &at, last, at.length-last, PL_ATOM) )
    fail;

  return PL_unify_nil(tail);
}


static void
append_text_to_buffer(Buffer b, PL_chars_t *txt, IOENC *enc)
{ if ( txt->encoding == *enc )
  { if ( txt->encoding == ENC_ISO_LATIN_1 )
    { addMultipleBuffer(b, txt->text.t, txt->length, char);
    } else
    { addMultipleBuffer(b, txt->text.w, txt->length, pl_wchar_t);
    }
  } else if ( txt->encoding == ENC_ISO_LATIN_1 )
  { const unsigned char *s = (const unsigned char*)txt->text.t;
    const unsigned char *e = &s[txt->length];

    for( ;s<e; s++)
    { pl_wchar_t chr = *s;

      addBuffer(b, chr, pl_wchar_t);
    }
  } else				/* promote our buffer */
  { size_t len = entriesBuffer(b, char);
    unsigned char *tmp = PL_malloc(len);
    const unsigned char *s = tmp;
    const unsigned char *e = &s[len];

    memcpy(tmp, baseBuffer(b, char), len);
    discardBuffer(b);
    initBuffer(b);

    for( ;s<e; s++)
    { pl_wchar_t chr = *s;

      addBuffer(b, chr, pl_wchar_t);
    }

    PL_free(tmp);
    *enc = ENC_WCHAR;
					/* and add new text */
    addMultipleBuffer(b, txt->text.w, txt->length, pl_wchar_t);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
atomic_list_concat()     implements     atomic_list_concat/2,3       and
atomics_to_string/2,3.

(*) Note that the atom-version for  historical reasons supports the mode
(-,+,+)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define atomic_list_concat(list, sep, atom, ret_type) LDFUNC(atomic_list_concat, list, sep, atom, ret_type)
static foreign_t
atomic_list_concat(DECL_LD term_t list, term_t sep, term_t atom, int ret_type)
{ term_t l = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  IOENC enc = ENC_ISO_LATIN_1;
  tmp_buffer b;
  PL_chars_t st;			/* separator text */
  int ntxt = 0;

  if ( sep && !PL_get_text(sep, &st, CVT_ATOMIC) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_text, sep);

  initBuffer(&b);
  while( PL_get_list(l, head, l) )
  { PL_chars_t txt;
    int ok = 1;

    PL_STRINGS_MARK();
    if ( PL_get_text(head, &txt, CVT_ATOMIC) )
    { if ( ntxt > 0 && sep )
	append_text_to_buffer((Buffer)&b, &st, &enc);

      append_text_to_buffer((Buffer)&b, &txt, &enc);
      PL_free_text(&txt);
    } else
    { if ( PL_is_variable(head) && sep && ret_type == PL_ATOM ) /* see (*) */
	ok = -1;
      else
	ok = PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_text, head);
    }
    PL_STRINGS_RELEASE();

    if ( ok == 1 && ++ntxt == 100 &&
	 lengthList(l, true) < 0 )
      ok = 0;

    if ( ok != 1 )
    { discardBuffer(&b);

      if ( ok == -1 )
	goto split;
      if ( ok == 0 )
	return false;
    }
  }

  if ( PL_get_nil(l) )
  { PL_chars_t sum;
    int rc;

    sum.encoding  = enc;
    sum.storage   = PL_CHARS_HEAP;
    sum.canonical = true;

    if ( enc == ENC_ISO_LATIN_1 )
    { sum.text.t = baseBuffer(&b, char);
      sum.length = entriesBuffer(&b, char);
    } else
    { sum.text.w = baseBuffer(&b, pl_wchar_t);
      sum.length = entriesBuffer(&b, pl_wchar_t);
    }

    rc = PL_unify_text(atom, 0, &sum, ret_type);
    discardBuffer(&b);

    return rc;
  } else if ( !PL_is_variable(l) )
  { return PL_type_error("list", l);
  }

  discardBuffer(&b);

split:
  if ( !sep || st.length == 0 )
  { if ( !sep )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_text, l);

    return PL_domain_error("non_empty_atom", sep);
  }

  return split_atom(list, &st, atom);
}


static
PRED_IMPL("atomic_list_concat", 3, atomic_list_concat, 0)
{ PRED_LD
  return atomic_list_concat(A1, A2, A3, PL_ATOM);
}


static
PRED_IMPL("atomic_list_concat", 2, atomic_list_concat, 0)
{ PRED_LD
  return atomic_list_concat(A1, 0, A2, PL_ATOM);
}


static
PRED_IMPL("atomics_to_string", 3, atomics_to_string, 0)
{ PRED_LD
  return atomic_list_concat(A1, A2, A3, PL_STRING);
}


static
PRED_IMPL("atomics_to_string", 2, atomics_to_string, 0)
{ PRED_LD
  return atomic_list_concat(A1, 0, A2, PL_STRING);
}


/** sub_atom_icasechk(+Haystack, ?Start, +Needle) is semidet.
*/

static
PRED_IMPL("sub_atom_icasechk", 3, sub_atom_icasechk, 0)
{ PRED_LD
  char       *needleA=NULL, *haystackA=NULL;
  pl_wchar_t *needleW=NULL, *haystackW=NULL;
  size_t l1, l2, offset;
  int has_offset;

  term_t haystack = A1;
  term_t start	  = A2;
  term_t needle   = A3;

  if ( PL_is_variable(start) )
    has_offset = false, offset = 0;
  else if ( PL_get_size_ex(start, &offset) )
    has_offset = true;
  else
    return false;

  if ( PL_get_nchars(needle,   &l1, &needleA, CVT_ALL|BUF_STACK) &&
       PL_get_nchars(haystack, &l2, &haystackA, CVT_ALL) )
  { char *s, *q, *s2 = haystackA + offset;
    const char *eq = (const char *)&needleA[l1];
    const char *es = (const char *)&haystackA[l2];

    for (; s2<=es-l1; s2++)
    { for(q=needleA, s=s2; q<eq && s<es; q++, s++)
      { if ( *q != *s && *q != toLower(*s) )
	  break;
      }
      if ( q == eq )
      { offset = s2-haystackA;
	goto found;
      }
      if ( has_offset )
	break;
    }
    fail;
  }

  if ( PL_get_wchars(needle,   &l1, &needleW, CVT_ALL|CVT_EXCEPTION|BUF_STACK) &&
       PL_get_wchars(haystack, &l2, &haystackW, CVT_ALL|CVT_EXCEPTION) )
  { pl_wchar_t *s, *q, *s2 = haystackW + offset;
    pl_wchar_t *eq = &needleW[l1];
    pl_wchar_t *es = &haystackW[l2];

    for (; s2<=es-l1; s2++)
    { for(q=needleW, s=s2; q<eq && s<es; q++, s++)
      { if ( *q != *s && *q != (pl_wchar_t)towlower(*s) )
	  break;
      }
      if ( q == eq )
      { offset = s2-haystackW;
	goto found;
      }
      if ( has_offset )
	break;
    }
    fail;
  }

  return false;

found:
  if ( !has_offset )
    return PL_unify_integer(start, offset);

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ISO compliant hacking  into  atoms.  The   state  is  represented  by  a
`redo-int', of which we use the first 15   bits for the `before' and the
second 15 bits for the `after'.

There are many possibilities (think the semantics are a bit overloaded).

    * sub is given
	+ if len conflicts: fail
	+ if before or after given: test deterministically
	+ otherwise: search (non-deterministic)
    * two of the integers are given
	+ generate (deterministic)
    * before is given:
	+ split the remainder (non-deterministic)
    * len is given:
	+ enumerate breaks (non-deterministic)
    * after is given:
	+ split the remainder (non-deterministic)
    * non given:
	+ enumerate using before and len (non-deterministic)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

enum sub_type
{ SUB_SEARCH,				/* sub given, but no position */
  SUB_SPLIT_TAIL,			/* before given, split tail */
  SUB_SPLIT_HEAD,			/* after given, split head */
  SUB_SPLIT_LEN,			/* len given, move it */
  SUB_ENUM				/* all free */
};

typedef struct
{ enum sub_type type;			/* Type of enumeration */
  size_t n1;				/* 1-st state id */
  size_t n2;				/* 2-nd state id */
  size_t n3;
} sub_state;


#define get_positive_integer_or_unbound(t, v) \
	LDFUNC(get_positive_integer_or_unbound, t, v)

static int
get_positive_integer_or_unbound(DECL_LD term_t t, size_t *v)
{ if ( PL_is_variable(t) )
  { *v = SIZE_NOT_SET;
    return true;
  }

  return PL_get_size_ex(t, v);
}

#define sub_text(atom, before, len, after, sub, h, type) \
	LDFUNC(sub_text, atom, before, len, after, sub, h, type)

static foreign_t
sub_text(DECL_LD term_t atom,
	 term_t before, term_t len, term_t after,
	 term_t sub,
	 control_t h,
	 int type			/* PL_ATOM or PL_STRING */)
{ PL_chars_t ta, ts;			/* the strings */
  sub_state *state;			/* non-deterministic state */
  atom_t expected = (type == PL_STRING ? ATOM_string : ATOM_atom);
  int match;
  fid_t fid;
  size_t b,l,a;				/* before,length,after match */
  size_t lab = SIZE_NOT_SET;		/* length of haystack */
  size_t lsb = SIZE_NOT_SET;		/* length of needle */

#define la (SIZE_GIVEN(lab) ? lab : (lab=PL_text_length(&ta)))
#define ls (SIZE_GIVEN(lsb) ? lsb : (lsb=PL_text_length(&ts)))

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { if ( !PL_get_text(atom, &ta, CVT_ATOMIC|BUF_ALLOW_STACK) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, expected, atom);

      if ( !get_positive_integer_or_unbound(before, &b) ||
	   !get_positive_integer_or_unbound(len, &l) ||
	   !get_positive_integer_or_unbound(after, &a) )
	return false;

      if ( !PL_get_text(sub, &ts, CVT_ATOMIC|BUF_ALLOW_STACK) )
      { if ( !PL_is_variable(sub) )
	  return PL_error(NULL, 0, NULL, ERR_TYPE, expected, sub);
	ts.text.t = NULL;
      }

      if ( ts.text.t )			/* `sub' given */
      { if ( SIZE_GIVEN(l) && ls != l ) /* len conflict */
	  return false;
	if ( SIZE_GIVEN(b) )		/* before given: test */
	{ if ( PL_cmp_text(&ta, b, &ts, 0, ls) == CMP_EQUAL )
	  { return (PL_unify_integer(len, ls) &&
		    PL_unify_integer(after, la-ls-b)) ? true : false;
	  }
	  return false;
	}
	if ( SIZE_GIVEN(a) )		/* after given: test */
	{ if ( la >= a+ls )
	  { size_t off = la-a-ls;

	    if ( PL_cmp_text(&ta, off, &ts, 0, ls) == CMP_EQUAL )
	    { return ( PL_unify_integer(len, ls) &&
		       PL_unify_integer(before, off) );
	    }
	  }
	  return false;
	}
	state = allocForeignState(sizeof(*state));
	state->type = SUB_SEARCH;
	state->n1   = 0;
	state->n2   = la;
	state->n3   = ls;
	break;
      }

      if ( SIZE_GIVEN(b) )		/* before given */
      { if ( b > la )
	  return false;

	if ( SIZE_GIVEN(l) )		/* len given */
	{ if ( b+l <= la )		/* deterministic fit */
	  { if ( PL_unify_text_range(sub, &ta, b, l, type) &&
		 PL_unify_integer(after, la-b-l) )
	      return true;
	  }
	  return false;
	}
	if ( SIZE_GIVEN(a) )		/* after given */
	{ if ( la >= a+b )
	  { size_t l2 = la-a-b;

	    if ( PL_unify_text_range(sub, &ta, b, l2, type) &&
		 PL_unify_integer(len, l2) )
	      return true;
	  }

	  return false;
	}
	state = allocForeignState(sizeof(*state));
	state->type = SUB_SPLIT_TAIL;
	state->n1   = 0;		/* len of the split */
	state->n2   = la;		/* length of the atom */
	state->n3   = b;		/* length before */
	break;
      }

      if ( SIZE_GIVEN(l) )		/* no before, len given */
      { if ( l > la )
	  return false;

	if ( SIZE_GIVEN(a) )		/* len and after */
	{ if ( la >= a+l )
	  { size_t b2 = la-a-l;

	    if ( PL_unify_text_range(sub, &ta, b2, l, type) &&
		 PL_unify_integer(before, b2) )
	      return true;
	  }

	  return false;
	}
	state = allocForeignState(sizeof(*state));
	state->type = SUB_SPLIT_LEN;
	state->n1   = 0;		/* before */
	state->n2   = l;		/* length */
	state->n3   = la;
	break;
      }

      if ( SIZE_GIVEN(a) )		/* only after given */
      { if ( a > la )
	  return false;

	state = allocForeignState(sizeof(*state));
	state->type = SUB_SPLIT_HEAD;
	state->n1   = 0;		/* before */
	state->n2   = la;
	state->n3   = a;
	break;
      }

      state = allocForeignState(sizeof(*state));
      state->type = SUB_ENUM;
      state->n1	= 0;			/* before */
      state->n2 = 0;			/* len */
      state->n3 = la;			/* total length */
      break;
    }
    case FRG_REDO:
      state = ForeignContextPtr(h);
      PL_get_text(atom, &ta, CVT_ATOMIC|BUF_ALLOW_STACK);
      break;
    case FRG_CUTTED:
      state = ForeignContextPtr(h);
      if ( state )
	freeForeignState(state, sizeof(*state));
      return true;
    default:
      assert(0);
      return false;
  }

  fid = PL_open_foreign_frame();
again:
  switch(state->type)
  { case SUB_SEARCH:
    { PL_get_text(sub, &ts, CVT_ATOMIC|BUF_ALLOW_STACK);
      lab = state->n2;
      lsb = state->n3;

      for( ; state->n1+ls <= la; state->n1++ )
      { if ( PL_cmp_text(&ta, state->n1, &ts, 0, ls) == 0 )
	{ match = (PL_unify_integer(before, state->n1) &&
		   PL_unify_integer(len,    ls) &&
		   PL_unify_integer(after,  la-ls-state->n1));

	  if ( ++state->n1 + ls > la )
	    goto exit_succeed;
	  else
	    goto next;
	}
      }
      goto exit_fail;
    }
    case SUB_SPLIT_TAIL:		/* before given, rest unbound */
    { lab = state->n2;
      b   = state->n3;
      l   = state->n1++;

      match = (PL_unify_text_range(sub, &ta, b, l, type) &&
	       PL_unify_integer(len, l) &&
	       PL_unify_integer(after, la-b-l));
    out:
      if ( b+l < (int)la )
	goto next;
      else if ( match )
	goto exit_succeed;
      else
	goto exit_fail;
    }
    case SUB_SPLIT_LEN:
    { b   = state->n1++;
      l   = state->n2;
      lab = state->n3;

      match = (PL_unify_text_range(sub, &ta, b, l, type) &&
	       PL_unify_integer(before, b) &&
	       PL_unify_integer(after, la-b-l));
      goto out;
    }
    case SUB_SPLIT_HEAD:
    { b   = state->n1++;
      lab = state->n2;
      a   = state->n3;
      l   = la - a - b;

      match = (PL_unify_text_range(sub, &ta, b, l, type) &&
	       PL_unify_integer(before, b) &&
	       PL_unify_integer(len, l));
      if ( l > 0 )
	goto next;
      else if ( match )
	goto exit_succeed;
      else
	goto exit_fail;
    }
    case SUB_ENUM:
    { b   = state->n1;
      l   = state->n2++;
      lab = state->n3;
      a   = la-b-l;

      match = (PL_unify_text_range(sub, &ta, b, l, type) &&
	       PL_unify_integer(before, b) &&
	       PL_unify_integer(len, l) &&
	       PL_unify_integer(after, a));
      if ( a == 0 )
      { if ( b == (int)la )
	{ if ( match )
	    goto exit_succeed;
	  else
	    goto exit_fail;
	}
	state->n2 = 0;
	state->n1++;
      }
      goto next;
    }
  }

exit_fail:
  freeForeignState(state, sizeof(*state));
  return false;

exit_succeed:
  freeForeignState(state, sizeof(*state));
  return true;

next:
  if ( match )
  { ForeignRedoPtr(state);
  } else
  { if ( !PL_exception(0) )
    { PL_rewind_foreign_frame(fid);
      goto again;
    } else
    { goto exit_fail;
    }
  }

#undef la
#undef ls
}


foreign_t
pl_sub_atom(term_t atom,
	    term_t before, term_t len, term_t after,
	    term_t sub,
	    control_t h)
{ GET_LD
  return sub_text(atom, before, len, after, sub, h, PL_ATOM);
}


#if O_STRING
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provisional String manipulation functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("string_length", 2, string_length, 0)
{ PRED_LD

  return text_length(A1, A2, CVT_ALL);
}


static
PRED_IMPL("string_concat", 3, string_concat, PL_FA_NONDETERMINISTIC)
{ PRED_LD

  return concat(A1, A2, A3, true, PL__ctx, CVT_ATOMIC, PL_STRING);
}


static
PRED_IMPL("sub_string", 5, sub_string, PL_FA_NONDETERMINISTIC)
{ PRED_LD

  return sub_text(A1, A2, A3, A4, A5, PL__ctx, PL_STRING);
}

#endif /* O_STRING */


		/********************************
		*            CONTROL            *
		*********************************/

foreign_t
pl_repeat(control_t h)
{ switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    case FRG_REDO:
      ForeignRedoInt(2L);
    case FRG_CUTTED:
    default:
      succeed;
  }
}

foreign_t
pl_fail()		/* just to define it */
{ fail;
}

foreign_t
pl_true()		/* just to define it */
{ succeed;
}

static
PRED_IMPL("halt", 1, halt, 0)
{ PRED_LD
  int status;
  atom_t a;
  term_t code = A1;

  if ( PL_get_atom(code, &a) )
  { if ( a == ATOM_abort )
    { PL_abort_process();
      return false;				/* not reached */
    } else if ( PL_get_signum_ex(code, &status) )
    { status += 128;
      status |= PL_CLEANUP_NO_CANCEL;
    } else
      return false;
  } else if ( !PL_get_integer_ex(code, &status) )
  { return false;
  }

  return PL_halt(status|PL_HALT_WITH_EXCEPTION);
}

#if defined(O_LIMIT_DEPTH) || defined(O_INFERENCE_LIMIT)
#define unify_det(t) LDFUNC(unify_det, t)
static foreign_t
unify_det(DECL_LD term_t t)
{ Choice ch;

  for(ch=LD->choicepoints; ch; ch = ch->parent)
  { if ( ch->frame == environment_frame )
      continue;			/* choice from I_FOPENNDET */
    switch(ch->type)
    { case CHP_CATCH:
      case CHP_DEBUG:
	continue;
      default:
	break;
    }
    break;
  }

  if ( ch && ch->frame == environment_frame->parent )
  { return PL_unify_atom(t, ATOM_cut);
  } else
  { if ( PL_unify_atom(t, ATOM_true) )
      ForeignRedoInt(1);
    return false;
  }
}

#endif

#ifdef O_LIMIT_DEPTH

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The    predicates    below    provide      the     infrastructure    for
call_with_depth_limit/3. This predicate was included on request by Steve
Moyle, for improving the implementation of a theorem prover.

The implementation of call_with_depth_limit/3 in pl-prims.pl is

```
call_with_depth_limit(G, Limit, Result) :-
	$depth_limit(Limit, OLimit, OReached),
	(   catch(G, E, depth_limit_except(OLimit, OReached, E)),
	    $depth_limit_true(Limit, OLimit, OReached, Result, Cut),
	    Cut
	;   $depth_limit_false(OLimit, OReached, Result)
	).
```

$depth_limit/3 sets the new limit and fetches the old values so they can
be restored by the other calls.   '$depth_limit_true'/5 restores the old
limits, and unifies Result with  the   maximum  depth reached during the
proof. Cut is unified  with  !   if  G  succeeded deterministically, and
`true' otherwise and  ensures  the   wrapper  maintains  the determistic
properties of G. It can be debated whether this is worthwhile ...

Finally, '$depth_limit_false'/4 checks for a depth-overflow, and unifies
result with `depth_limit_exceeded' if an overflow  has occurred and just
fails otherwise. Of course it always restores the outer environment.

Note that call_with_depth_limit/3 cannot be written  as a simple foreign
call using PL_open_query(), etc, as   the non-deterministic predicate is
not allowed to return to  the   parent  environment  without closing the
query.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* $depth_limit(+Limit, -OldLimit, -DepthReached)
*/

#define unify_depth_LD(t, depth) LDFUNC(unify_depth_LD, t, depth)
static int
unify_depth_LD(DECL_LD term_t t, size_t depth)
{ if ( depth == DEPTH_NO_LIMIT )
    return PL_unify_atom(t, ATOM_inf);
  else
    return PL_unify_uint64(t, depth);
}

#define get_depth_LD(t, depth) LDFUNC(get_depth_LD, t, depth)
static int
get_depth_LD(DECL_LD term_t t, size_t *depth)
{ atom_t a;

  if ( PL_get_atom(t, &a) && a == ATOM_inf )
  { *depth = DEPTH_NO_LIMIT;
    return true;
  }

  return PL_get_size_ex(t, depth);
}

#define unify_depth(t, d) unify_depth_LD(t, d)
#define get_depth(t, d)   get_depth_LD(t, d)

static
PRED_IMPL("$depth_limit", 3, pl_depth_limit, 0)
{ GET_LD
  size_t levels;
  size_t clevel = levelFrame(environment_frame) - 1;

  if ( PL_get_size_ex(A1, &levels) )
  { if ( unify_depth(A2, LD->depth_info.limit) &&
	 unify_depth(A3, LD->depth_info.reached) )
    { size_t newlimit = clevel + levels + 1; /* 1 for the catch/3 */

      if ( newlimit < clevel )
	return PL_representation_error("depth_limit");

      LD->depth_info.limit   = newlimit;
      LD->depth_info.reached = clevel;

      updateAlerted(LD);
      return true;
    }
  }

  return false;
}


static
PRED_IMPL("$depth_limit_true", 5, pl_depth_limit_true, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  term_t limit = A1;
  term_t olimit = A2;
  term_t oreached = A3;
  term_t res = A4;
  term_t cut = A5;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { size_t l, ol, or;

      if ( get_depth(limit, &l) &&
	   get_depth(olimit, &ol) &&
	   get_depth(oreached, &or) )
      { intptr_t clevel = levelFrame(environment_frame) - 1;
	intptr_t used = LD->depth_info.reached - clevel - 1;

	LD->depth_info.limit   = ol;
	LD->depth_info.reached = or;
	updateAlerted(LD);

	if ( used < 1 )
	  used = 1;
	if ( !PL_unify_integer(res, used) )
	  return false;

	return unify_det(cut);
      }

      break;
    }
    case FRG_REDO:
    { size_t levels;
      size_t clevel = levelFrame(environment_frame) - 1;

      if ( !get_depth(limit, &levels) )
	return false;
      LD->depth_info.limit   = clevel + levels + 1; /* 1 for catch/3 */
      LD->depth_info.reached = clevel;
      updateAlerted(LD);

      return false;				    /* backtrack to goal */
    }
    case FRG_CUTTED:
      return true;
    case FRG_RESUME:
      assert(0);
  }

  return false;
}


static
PRED_IMPL("$depth_limit_false", 3, depth_limit_false, 0)
{ PRED_LD
  size_t ol, or;

  if ( get_depth(A1, &ol) &&
       get_depth(A2, &or) )
  { int exceeded = (LD->depth_info.reached > LD->depth_info.limit);

    LD->depth_info.limit   = ol;
    LD->depth_info.reached = or;
    updateAlerted(LD);

    if ( exceeded )
      return PL_unify_atom(A3, ATOM_depth_limit_exceeded);
  }

  return false;
}


static
PRED_IMPL("$depth_limit_except", 3, depth_limit_except, 0)
{ PRED_LD
  size_t ol, or;

  if ( get_depth(A1, &ol) &&
       get_depth(A2, &or) )
  { LD->depth_info.limit   = ol;
    LD->depth_info.reached = or;
    updateAlerted(LD);

    return PL_raise_exception(A3);
  }

  fail;
}

#endif /*O_LIMIT_DEPTH*/

#ifdef O_INFERENCE_LIMIT

#define INFERENCE_LIMIT_OVERHEAD 2

static
PRED_IMPL("$inference_limit", 2, pl_inference_limit, 0)
{ PRED_LD
  int64_t limit;

  if ( PL_get_int64_ex(A1, &limit) &&
       PL_unify_int64(A2, LD->inference_limit.limit) )
  { int64_t nlimit = LD->statistics.inferences + limit + INFERENCE_LIMIT_OVERHEAD;

    if ( limit < 0 )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		      ATOM_not_less_than_zero, A1);

    DEBUG(MSG_INFERENCE_LIMIT,
	  Sdprintf("Install %lld --> %lld\n",
		   LD->inference_limit.limit, nlimit));

    if ( nlimit < LD->inference_limit.limit )
      LD->inference_limit.limit = nlimit;

    updateAlerted(LD);
    return true;
  }

  return false;
}


/** '$inference_limit_true'(+Limit, +OldLimit, ?Result)

On first call:

  1. If Result is nonvar, there was the inference limit is exceeded.
     The limit is already reset by '$inference_limit_except'/3, so we
     just indicate that our result is deterministic.
  2. Else, restore the limit and indicate determinism in Det.

On redo, use Limit to set a new  limit and fail to continue retrying the
guarded goal.
*/

static
PRED_IMPL("$inference_limit_true", 3, pl_inference_limit_true,
	  PL_FA_NONDETERMINISTIC)
{ PRED_LD

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { int64_t olimit;

      if ( !PL_is_variable(A3) )
	return true;

      if ( PL_get_int64_ex(A2, &olimit) )
      { DEBUG(MSG_INFERENCE_LIMIT, Sdprintf("true (det) --> %lld\n", olimit));
	LD->inference_limit.limit = olimit;
	updateAlerted(LD);

	return unify_det(A3);
      }

      return false;
    }
    case FRG_REDO:
    { int64_t limit;

      if ( PL_get_int64_ex(A1, &limit) )
      { LD->inference_limit.limit =
		LD->statistics.inferences + limit + INFERENCE_LIMIT_OVERHEAD;
	DEBUG(MSG_INFERENCE_LIMIT,
	      Sdprintf("true (ndet) --> %lld\n", LD->inference_limit.limit));

	updateAlerted(LD);
      }

      return false;
    }
    case FRG_CUTTED:
      return true;
    case FRG_RESUME:
      assert(0);
  }

  return false;
}


static
PRED_IMPL("$inference_limit_false", 1, inference_limit_false, 0)
{ PRED_LD
  int64_t olimit;

  if ( PL_get_int64_ex(A1, &olimit) )
  { LD->inference_limit.limit = olimit;
    DEBUG(MSG_INFERENCE_LIMIT, Sdprintf("false --> %lld\n", olimit));
    updateAlerted(LD);
  }

  return false;
}


/** '$inference_limit_except'(+OldLimit, +Exception, -Result)

Restore the limit. If  exception   is  =inference_limit_exceeded=, unify
Result with this, otherwise re-throw the exception.
*/

static
PRED_IMPL("$inference_limit_except", 3, inference_limit_except, 0)
{ PRED_LD
  int64_t olimit;

  if ( PL_get_int64_ex(A1, &olimit) )
  { atom_t a;

    DEBUG(MSG_INFERENCE_LIMIT, Sdprintf("except --> %lld\n", olimit));

    LD->inference_limit.limit = olimit;
    updateAlerted(LD);

    if ( PL_get_atom(A2, &a) && a == ATOM_inference_limit_exceeded )
    { return PL_unify_atom(A3, a);
    } else
    { return PL_raise_exception(A2);
    }
  }

  return false;
}

void
raiseInferenceLimitException(void)
{ GET_LD
  fid_t fid;
  Definition def = environment_frame->predicate;
  int64_t olimit;
  int i;

  if ( LD->exception.processing )
    return;
					/* Do not throw here */
  olimit = LD->inference_limit.limit;
  LD->inference_limit.limit = INFERENCE_NO_LIMIT;

  DEBUG(MSG_INFERENCE_LIMIT,
	Sdprintf("Got inference limit in %s\n", predicateName(def)));

#define not_exceed GD->inference_limit.not_exceed_
  if ( !GD->inference_limit.initialized )
  { not_exceed[0] = PL_predicate("$inference_limit_true",     3, "system");
    not_exceed[1] = PL_predicate("$inference_limit_false",    1, "system");
    not_exceed[2] = PL_predicate("$inference_limit_except",   3, "system");
    not_exceed[3] = PL_predicate("$inference_limit",          2, "system");
    not_exceed[4] = PL_predicate("call_with_inference_limit", 3, "system");
    not_exceed[5] = GD->procedures.catch3;
  }

  for(i=0; i<6; i++)
  { if ( not_exceed[i]->definition == def )
    { LD->inference_limit.limit = olimit;
      DEBUG(MSG_INFERENCE_LIMIT, Sdprintf("--> Ignored\n"));
      return;
    }
  }

  if ( (fid = PL_open_foreign_frame()) )
  { term_t t;

    LD->exception.processing = true;
    t = PL_new_term_ref();
    PL_put_atom(t, ATOM_inference_limit_exceeded);
    PL_raise_exception(t);
    PL_close_foreign_frame(fid);
  }
}
#endif /*O_INFERENCE_LIMIT*/

		/********************************
		*          STATISTICS           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fetch runtime statistics. There are two standards  here. One is based on
old C-Prolog compatibility, exended as required   by  SWI-Prolog and the
other  is  defined  by  Quintus/SICStus.  The   latter  is  included  if
QP_STATISTICS is defined. The compatibility   is pretty complete, except
the `atoms' key that is defined by both and this ambiguous.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
programSpace(void)
{ size_t allocated = heapUsed();

  if ( allocated )
    return allocated - GD->statistics.stack_space;

  return 0;
}


#define QP_STATISTICS 1

#ifdef QP_STATISTICS

#define qp_statistics(key, v) LDFUNC(qp_statistics, key, v)
static int
qp_statistics(DECL_LD atom_t key, int64_t v[])
{ int vn;

  if ( key == ATOM_runtime )		/* compat: exclude gc-time */
  { v[0] = (int64_t)((LD->statistics.user_cputime -
		      LD->gc.stats.totals.time -
		      GD->atoms.gc_time) * 1000.0);
    v[1] = v[0] - LD->statistics.last_cputime;
    LD->statistics.last_cputime = v[0];
    vn = 2;
  } else if ( key == ATOM_system_time )
  { v[0] = (int64_t)(LD->statistics.system_cputime * 1000.0);
    v[1] = v[0] - LD->statistics.last_systime;
    LD->statistics.last_systime = v[0];
    vn = 2;
  } else if ( key == ATOM_real_time )
  { v[0] = (int64_t)WallTime();
    v[1] = v[0] - LD->statistics.last_real_time;
    LD->statistics.last_real_time = v[0];
    vn = 2;
  } else if ( key == ATOM_walltime )
  { double wt = WallTime();
    if ( !LD->statistics.last_walltime )
      LD->statistics.last_walltime = LD->statistics.start_time;
    v[0] = (int64_t)((wt - LD->statistics.start_time) * 1000.0);
    v[1] = (int64_t)((wt - LD->statistics.last_walltime) * 1000.0);
    LD->statistics.last_walltime = wt;
    vn = 2;
  } else if ( key == ATOM_memory || key == ATOM_core )
  { v[0] = UsedMemory();
    v[1] = FreeMemory();
    vn = 2;
  } else if ( key == ATOM_stacks )
  { v[0] = usedStack(global);
    v[1] = usedStack(local);
    vn = 2;
  } else if ( key == ATOM_global_stack )
  { v[0] = usedStack(global);
    v[1] = roomStack(global);
    vn = 2;
  } else if ( key == ATOM_local_stack )
  { v[0] = usedStack(local);
    v[1] = roomStack(local);
    vn = 2;
  } else if ( key == ATOM_trail )
  { v[0] = usedStack(trail);
    v[1] = roomStack(trail);
    vn = 2;
  } else if ( key == ATOM_program )
  { v[0] = programSpace();
    v[1] = 0;
    vn = 2;
  } else if ( key == ATOM_garbage_collection )
  { vn=0;
    gc_stats *stats = &LD->gc.stats;
    gc_stat  *last  = last_gc_stats(stats);

    v[vn++] = stats->totals.collections;
    v[vn++] = stats->totals.trail_gained + stats->totals.global_gained;
    v[vn++] = (int64_t)(stats->totals.time * 1000.0);
    v[vn++] = last->trail_after + last->global_after;

  } else if ( key == ATOM_stack_shifts )
  {
    v[0] = LD->shift_status.global_shifts;
    v[1] = LD->shift_status.local_shifts;
    v[2] = (int64_t)(LD->shift_status.time * 1000.0);
    vn = 3;
  } else if ( key == ATOM_atoms )
  { v[0] = GD->statistics.atoms;
    v[1] = GD->statistics.atom_string_space;
    v[2] = 0;
    vn = 3;
  } else if ( key == ATOM_atom_garbage_collection )
  {
#ifdef O_ATOMGC
    v[0] = GD->atoms.gc;
    v[1] = GD->statistics.atom_string_space_freed;
    v[2] = (int64_t)(GD->atoms.gc_time * 1000.0);
    vn = 3;
#else
    vn = 0;				/* no values */
#endif
  } else if ( key == ATOM_clause_garbage_collection )
  {
#ifdef O_CLAUSEGC
    v[0] = GD->clauses.cgc_count;
    v[1] = GD->clauses.cgc_reclaimed;
    v[2] = (int64_t)(GD->clauses.cgc_time * 1000.0);
    vn = 3;
#else
    vn = 0;				/* no values */
#endif
  } else
    vn = -1;				/* unknown key */

  return vn;
}

#endif /*QP_STATISTICS*/

#ifdef O_PLMT
#define SUBTHREAD_TIME()	(LD->thread.child_cputime)
#define SUBTHREAD_INFERENCES()	(LD->thread.child_inferences)
#else
#define SUBTHREAD_TIME()	(0)
#define SUBTHREAD_INFERENCES()	(0)
#endif

#define swi_statistics(key, v) LDFUNC(swi_statistics, key, v)

static int
swi_statistics(DECL_LD atom_t key, Number v)
{ v->type = V_INTEGER;			/* most of them */

  if      (key == ATOM_self_cputime)			/* time */
  { v->type = V_FLOAT;
    v->value.f = LD->statistics.user_cputime;
  } else if (key == ATOM_cputime)			/* time */
  { v->type = V_FLOAT;
    v->value.f = LD->statistics.user_cputime + SUBTHREAD_TIME();
  } else if (key == ATOM_process_cputime)		/* time */
  { v->type = V_FLOAT;
    v->value.f = GD->statistics.user_cputime;
  } else if (key == ATOM_self_inferences)		/* inferences */
    v->value.i = LD->statistics.inferences;
  else if (key == ATOM_inferences)			/* inferences */
    v->value.i = LD->statistics.inferences + SUBTHREAD_INFERENCES();
  else if (key == ATOM_stack)
    v->value.i = GD->statistics.stack_space;
  else if (key == ATOM_stack_limit)
    v->value.i = LD->stacks.limit;
  else if (key == ATOM_local)				/* local stack */
    v->value.i = sizeStack(local);
  else if (key == ATOM_localused)
    v->value.i = usedStack(local);
  else if (key == ATOM_trail)				/* trail */
    v->value.i = sizeStack(trail);
  else if (key == ATOM_trailused)
    v->value.i = usedStack(trail);
  else if (key == ATOM_global)				/* global */
    v->value.i = sizeStack(global);
  else if (key == ATOM_globalused )
    v->value.i = usedStack(global);
  else if (key == ATOM_c_stack)
  { c_stack_info *cinfo = CStackSize();
    v->value.i = cinfo ? cinfo->size : 0;
  } else if (key == ATOM_atoms)				/* atoms */
    v->value.i = GD->statistics.atoms;
  else if (key == ATOM_atom_space)			/* atom_space */
    v->value.i = atom_space();
  else if (key == ATOM_functors)			/* functors */
    v->value.i = GD->statistics.functors;
  else if (key == ATOM_functor_space)			/* functor_space */
    v->value.i = functor_space();
  else if (key == ATOM_predicates)			/* predicates */
    v->value.i = GD->statistics.predicates;
  else if (key == ATOM_clauses)				/* clauses */
    v->value.i = GD->statistics.clauses;
  else if (key == ATOM_modules)				/* modules */
    v->value.i = GD->statistics.modules;
  else if (key == ATOM_codes)				/* codes */
    v->value.i = GD->statistics.codes;
  else if (key == ATOM_epoch)
  { v->type = V_FLOAT;
    v->value.f = LD->statistics.start_time;
  } else if (key == ATOM_process_epoch)
  { v->type = V_FLOAT;
    v->value.f = PL_local_data.statistics.start_time;
  } else if (key == ATOM_gctime)
  { v->type = V_FLOAT;
    v->value.f = LD->gc.stats.totals.time;
  } else if (key == ATOM_collections)
    v->value.i = LD->gc.stats.totals.collections;
  else if (key == ATOM_collected)
    v->value.i = LD->gc.stats.totals.trail_gained +
		 LD->gc.stats.totals.global_gained;
  else if (key == ATOM_heapused)			/* heap usage */
    v->value.i = programSpace();
#ifdef O_ATOMGC
  else if (key == ATOM_agc)
    v->value.i = GD->atoms.gc;
  else if (key == ATOM_agc_gained)
    v->value.i = GD->atoms.collected;
  else if (key == ATOM_agc_time)
  { v->type = V_FLOAT;
    v->value.f = GD->atoms.gc_time;
  }
#endif
#ifdef O_CLAUSEGC
  else if (key == ATOM_cgc)
    v->value.i = GD->clauses.cgc_count;
  else if (key == ATOM_cgc_gained)
    v->value.i = GD->clauses.cgc_reclaimed;
  else if (key == ATOM_cgc_time)
  { v->type = V_FLOAT;
    v->value.f = GD->clauses.cgc_time;
  }
#endif
  else if (key == ATOM_global_shifts)
    v->value.i = LD->shift_status.global_shifts;
  else if (key == ATOM_local_shifts)
    v->value.i = LD->shift_status.local_shifts;
  else if (key == ATOM_trail_shifts)
    v->value.i = LD->shift_status.trail_shifts;
  else if (key == ATOM_shift_time)
  { v->type = V_FLOAT;
    v->value.f = LD->shift_status.time;
  }
#ifdef O_PLMT
  else if ( key == ATOM_threads )
    v->value.i = GD->statistics.threads_created -
		 GD->statistics.engines_created -
		 GD->statistics.threads_finished +
		 GD->statistics.engines_finished;
  else if ( key == ATOM_engines )
    v->value.i = GD->statistics.engines_created -
		 GD->statistics.engines_finished;
  else if ( key == ATOM_threads_created )
    v->value.i = GD->statistics.threads_created -
		 GD->statistics.engines_created;
  else if ( key == ATOM_engines_created )
    v->value.i = GD->statistics.engines_created;
  else if ( key == ATOM_thread_cputime )
  { v->type = V_FLOAT;
    v->value.f = GD->statistics.thread_cputime;
  } else if ( key == ATOM_threads_peak )
    v->value.i = GD->thread.peak_id;
#endif
  else if (key == ATOM_table_space_used)
  { alloc_pool *pool;
    if ( (pool=LD->tabling.node_pool) )
      v->value.i = pool->size;
    else
      v->value.i = 0;
  } else if (key == ATOM_indexes_created)
    v->value.i = GD->statistics.indexes.created;
  else if (key == ATOM_indexes_destroyed)
    v->value.i = GD->statistics.indexes.destroyed;
  else if (key == ATOM_warnings)
    v->value.i = GD->statistics.warnings;
  else if (key == ATOM_errors)
    v->value.i = GD->statistics.errors;

  else
    return -1;				/* unknown key */

  succeed;
}


int
pl_statistics_ld(DECL_LD term_t k, term_t value, PL_local_data_t *ld)
{ number result;			/* make compiler happy */
  atom_t key;
  int rc;
#ifdef QP_STATISTICS
  int64_t v[4];
#endif

  if ( !PL_get_atom_ex(k, &key) )
    fail;

  if ( !PL_is_list(value) )
  { switch(swi_statistics(PASS_AS_LD(ld) key, &result))
    { case true:
	return PL_unify_number(value, &result);
      case false:
	fail;
      case -1:
	break;
    }
  }

#ifdef QP_STATISTICS
  if ( (rc=qp_statistics(PASS_AS_LD(ld) key, v)) >= 0 )
  { int64_t *p;
    term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    for(p = v; rc-- > 0; p++)
    { if ( !PL_unify_list(tail, head, tail) )
      { if ( PL_unify_nil(tail) )
	  succeed;
	fail;
      }
      if ( !PL_unify_int64(head, *p) )
	fail;
    }

    return PL_unify_nil(tail);
  }
#endif /*QP_STATISTICS*/

  return PL_error("statistics", 2, NULL, ERR_DOMAIN,
		  PL_new_atom("statistics_key"), k);
}


static
PRED_IMPL("statistics", 2, statistics, 0)
{ GET_LD
  atom_t k;

  if ( PL_get_atom(A1, &k) )
  { if ( k == ATOM_process_cputime )
      GD->statistics.user_cputime = CpuTime(CPU_USER);
    if ( k == ATOM_cputime || k == ATOM_runtime )
      LD->statistics.user_cputime = ThreadCPUTime(CPU_USER);
    else if ( k == ATOM_system_time )
      LD->statistics.system_cputime = ThreadCPUTime(CPU_SYSTEM);
  }

  return pl_statistics_ld(A1, A2, LD);
}


		/********************************
		*            OPTIONS            *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$cmd_option_val/3, provides access to the  option structure from Prolog.
This is halfway a generic structure  package   ...  Anyway, it is better
then direct coded  access,  as  the   indirect  approach  allows  us  to
enumerate the options and generalise  the   option  processing  from the
saved-states.

See also pl-init.c, which exploits set_pl_option()  to parse the options
resource  member.  Please  note  this   code    doesn't   use  atoms  as
set_pl_option() is called before the Prolog system is initialised.

This code should be moved into another file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ const char   *name;
  int		type;
  void	       *address;
} optdef, *OptDef;

#define CMDOPT_BOOL   0
#define CMDOPT_SIZE_T 1
#define CMDOPT_STRING 2
#define CMDOPT_LIST   3

static const optdef optdefs[] =
{ { "stack_limit",	CMDOPT_SIZE_T,	&GD->options.stackLimit },

  { "goals",		CMDOPT_LIST,	&GD->options.goals },
  { "toplevel",		CMDOPT_STRING,	&GD->options.topLevel },
  { "init_file",	CMDOPT_STRING,	&GD->options.initFile },
  { "system_init_file",	CMDOPT_STRING,	&GD->options.systemInitFile },
  { "script_file",	CMDOPT_LIST,	&GD->options.scriptFiles },
  { "config",		CMDOPT_STRING,	&GD->options.config },
  { "compileout",	CMDOPT_STRING,	&GD->options.compileOut },
  { "class",		CMDOPT_STRING,  &GD->options.saveclass },
  { "search_paths",	CMDOPT_LIST,	&GD->options.search_paths },
  { "defines",		CMDOPT_LIST,	&GD->options.defines },
  { "pldoc_server",	CMDOPT_STRING,	&GD->options.pldoc_server },
  { "nosignals",	CMDOPT_BOOL,	&GD->options.nosignals },
#ifdef __WINDOWS__
  { "win_app",		CMDOPT_BOOL,	&GD->options.win_app },
#endif
  { "home",		CMDOPT_STRING,	&GD->defaults.home },

  { NULL,		0,		NULL }
};


static
PRED_IMPL("$cmd_option_val", 2, cmd_option_val, 0)
{ PRED_LD
  char *k;

  term_t key = A1;
  term_t val = A2;

  if ( PL_get_atom_chars(key, &k) )
  { OptDef d = (OptDef)optdefs;

    for( ; d->name; d++ )
    { if ( streq(k, d->name) )
      { switch(d->type)
	{ case CMDOPT_BOOL:
	  { bool *lp = d->address;

	    return PL_unify_bool(val, *lp);
	  }
	  case CMDOPT_SIZE_T:
	  { size_t *lp = d->address;

	    return PL_unify_int64(val, *lp);
	  }
	  case CMDOPT_STRING:
	  { char **sp = d->address;

	    if ( *sp )
	      return PL_unify_chars(val, PL_ATOM|REP_FN, (size_t)-1, *sp);
	    return false;
	  }
	  case CMDOPT_LIST:
	  { opt_list **list = d->address;
	    opt_list *l;
	    term_t tail = PL_copy_term_ref(val);
	    term_t head = PL_new_term_ref();

	    for( l=*list; l; l = l->next)
	    { if ( !PL_unify_list(tail, head, tail) ||
		   !PL_unify_chars(head, PL_ATOM|REP_FN, (size_t)-1, l->opt_val) )
		return false;
	    }

	    return PL_unify_nil(tail);
	  }
	}
      }
    }
  }

  return PL_existence_error("cmd_option", key);
}


static
PRED_IMPL("$cmd_option_set", 2, cmd_option_set, 0)
{ char *k, *v;

  term_t key = A1;
  term_t val = A2;

  if ( PL_get_chars(key, &k, CVT_ALL|CVT_EXCEPTION|REP_FN) &&
       PL_get_chars(val, &v, CVT_ALL|CVT_EXCEPTION|REP_FN) )
  { return set_pl_option(k, v);
  }

  return false;
}


int
set_pl_option(const char *name, const char *value)
{ OptDef d = (OptDef)optdefs;

  if ( streq(name, "goal") )
    name = "goals";			/* HACK */

  for( ; d->name; d++ )
  { if ( streq(name, d->name) )
    { switch(d->type)
      { case CMDOPT_BOOL:
	{ bool *val =  d->address;
	  if ( streq(value, "true") )
	    *val = true;
	  else if ( streq(value, "false") )
	    *val = false;
	  else
	    assert(0);
	  return true;
	}
	case CMDOPT_SIZE_T:
	{ size_t *val = d->address;
	  number n;
	  unsigned char *q;

	  if ( str_number((unsigned char *)value, &q, &n, 0) == NUM_OK &&
	       *q == EOS &&
	       intNumber(&n) )
	  { *val = (size_t)n.value.i;
	    succeed;
	  }
	  fail;
	}
	case CMDOPT_STRING:
	{ char **val = d->address;

	  *val = store_string(value);
	  succeed;
	}
	case CMDOPT_LIST:
	{ opt_list **l = d->address;

	  opt_append(l, value);
	  succeed;
	}
	default:
	  assert(0);
      }
    }
  }

  fail;
}


		/********************************
		*         STYLE CHECK           *
		*********************************/

static
PRED_IMPL("$style_check", 2, style_check, 0)
{ PRED_LD
  int n;

  term_t old = A1;
  term_t new = A2;

  if ( PL_unify_integer(old, debugstatus.styleCheck) &&
       PL_get_integer(new, &n) )
  { debugstatus.styleCheck = n;

    succeed;
  }

  fail;
}


		 /*******************************
		 *	       THROW		*
		 *******************************/

static
PRED_IMPL("throw", 1, throw, 0)
{ PRED_LD

  if ( PL_is_variable(A1) )
    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);

  return PL_raise_exception(A1);
}

static
PRED_IMPL("$urgent_exception", 3, urgent_exception, 0)
{ PRED_LD
  except_class c1 = classify_exception(A1);
  except_class c2 = classify_exception(A2);

  return PL_unify(A3, c2 > c1 ? A2 : A1);
}



		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(prims)
  PRED_DEF("=", 2, unify, PL_FA_ISO)
  PRED_DEF("\\=", 2, not_unify, PL_FA_ISO)
  PRED_DEF("unify_with_occurs_check", 2, unify_with_occurs_check, PL_FA_ISO)
  PRED_DEF("subsumes_term", 2, subsumes_term, PL_FA_ISO)
  PRED_DEF("nonvar", 1, nonvar, PL_FA_ISO)
  PRED_DEF("var", 1, var, PL_FA_ISO)
  PRED_DEF("integer", 1, integer, PL_FA_ISO)
  PRED_DEF("float", 1, float, PL_FA_ISO)
#ifdef O_BIGNUM
  PRED_DEF("rational", 1, rational, 0)
#endif
  PRED_DEF("number", 1, number, PL_FA_ISO)
  PRED_DEF("arg", 3, arg, PL_FA_NONDETERMINISTIC|PL_FA_ISO)
  PRED_DEF("atomic", 1, atomic, PL_FA_ISO)
  PRED_DEF("atom", 1, atom, PL_FA_ISO)
  PRED_DEF("string", 1, string, 0)
  PRED_DEF("ground", 1, ground, PL_FA_ISO)
  PRED_DEF("nonground", 2, nonground, 0)
  PRED_DEF("$term_size", 3, term_size, 0)
  PRED_DEF("acyclic_term", 1, acyclic_term, PL_FA_ISO)
  PRED_DEF("cyclic_term", 1, cyclic_term, 0)
  PRED_DEF("$factorize_term", 3, factorize_term, 0)
  PRED_DEF("compound", 1, compound, PL_FA_ISO)
  PRED_DEF("callable", 1, callable, PL_FA_ISO)
  PRED_DEF("==", 2, equal, PL_FA_ISO)
  PRED_DEF("\\==", 2, nonequal, PL_FA_ISO)
  PRED_DEF("compare", 3, compare, PL_FA_ISO)
  PRED_DEF("@<", 2, std_lt, PL_FA_ISO)
  PRED_DEF("@=<", 2, std_leq, PL_FA_ISO)
  PRED_DEF("@>", 2, std_gt, PL_FA_ISO)
  PRED_DEF("@>=", 2, std_geq, PL_FA_ISO)
  PRED_DEF("?=", 2, can_compare, 0)
  PRED_DEF("same_term", 2, same_term, 0)
  PRED_DEF("$term_id", 2, term_id, 0)
  PRED_DEF("functor", 3, functor, PL_FA_ISO)
  PRED_DEF("functor", 4, functor, 0)
  PRED_DEF("=..", 2, univ, PL_FA_ISO)
  PRED_DEF("compound_name_arity", 3, compound_name_arity, 0)
  PRED_DEF("compound_name_arguments", 3, compound_name_arguments, 0)
  PRED_DEF("$filled_array", 4, filled_array, 0)
  PRED_DEF("$unbind_template", 1, unbind_template, 0)
  PRED_DEF("numbervars", 4, numbervars, 0)
  PRED_DEF("var_number", 2, var_number, 0)
  PRED_DEF("term_variables", 2, term_variables2, PL_FA_ISO)
  PRED_DEF("$term_attvar_variables", 2, term_attvar_variables, 0)
  PRED_DEF("term_variables", 3, term_variables3, 0)
  PRED_DEF("term_singletons", 2, term_singletons, 0)
  PRED_DEF("$term_multitons", 2, term_multitons, 0)
  PRED_DEF("term_attvars", 2, term_attvars, 0)
  PRED_DEF("is_most_general_term", 1, is_most_general_term, 0)
  PRED_DEF("$free_variable_set", 3, free_variable_set, 0)
  PRED_DEF("unifiable", 3, unifiable, 0)
#ifdef O_TERMHASH
  PRED_DEF("term_hash", 4, term_hash4, 0)
#endif
#ifdef O_LIMIT_DEPTH
  PRED_DEF("$depth_limit_except", 3, depth_limit_except, 0)
  PRED_DEF("$depth_limit_false",  3, depth_limit_false, 0)
  PRED_DEF("$depth_limit", 3, pl_depth_limit, 0)
  PRED_DEF("$depth_limit_true", 5, pl_depth_limit_true, PL_FA_NONDETERMINISTIC)
#endif
#ifdef O_INFERENCE_LIMIT
  PRED_DEF("$inference_limit", 2, pl_inference_limit, 0)
  PRED_DEF("$inference_limit_true", 3, pl_inference_limit_true,
	   PL_FA_NONDETERMINISTIC)
  PRED_DEF("$inference_limit_false", 1, inference_limit_false, 0)
  PRED_DEF("$inference_limit_except", 3, inference_limit_except, 0)
#endif
  PRED_DEF("atom_length", 2, atom_length, PL_FA_ISO)
  PRED_DEF("name", 2, name, 0)
  PRED_DEF("atom_chars", 2, atom_chars, PL_FA_ISO)
  PRED_DEF("atom_codes", 2, atom_codes, PL_FA_ISO)
  PRED_DEF("atom_concat", 3, atom_concat, PL_FA_NONDETERMINISTIC|PL_FA_ISO)
  PRED_DEF("atomic_concat", 3, atomic_concat, 0)
  PRED_DEF("number_chars", 2, number_chars, PL_FA_ISO)
  PRED_DEF("number_codes", 2, number_codes, PL_FA_ISO)
  PRED_DEF("number_string", 2, number_string, 0)
  PRED_DEF("char_code", 2, char_code, PL_FA_ISO)
  PRED_DEF("$is_char_code", 1, is_char_code, 0)
  PRED_DEF("$is_char", 1, is_char, 0)
  PRED_DEF("$is_code_list", 2, is_code_list, 0)
  PRED_DEF("$is_char_list", 2, is_char_list, 0)
  PRED_DEF("atom_number", 2, atom_number, 0)
  PRED_DEF("collation_key", 2, collation_key, 0)
  PRED_DEF("atomic_list_concat", 3, atomic_list_concat, 0)
  PRED_DEF("atomic_list_concat", 2, atomic_list_concat, 0)
  PRED_DEF("string_concat", 3, string_concat, PL_FA_NONDETERMINISTIC)
  PRED_DEF("sub_string", 5, sub_string, PL_FA_NONDETERMINISTIC)
  PRED_DEF("string_length", 2, string_length, 0)
  PRED_DEF("atomics_to_string", 3, atomics_to_string, 0)
  PRED_DEF("atomics_to_string", 2, atomics_to_string, 0)
  PRED_DEF("sub_atom_icasechk", 3, sub_atom_icasechk, 0)
  PRED_DEF("statistics", 2, statistics, 0)
  PRED_DEF("$cmd_option_val", 2, cmd_option_val, 0)
  PRED_DEF("$cmd_option_set", 2, cmd_option_set, 0)
  PRED_DEF("$style_check", 2, style_check, 0)
  PRED_DEF("deterministic", 1, deterministic, 0)
  PRED_DEF("setarg", 3, setarg, 0)
  PRED_DEF("nb_setarg", 3, nb_setarg, 0)
  PRED_DEF("nb_linkarg", 3, nb_linkarg, 0)
  PRED_DEF("$skip_list", 3, skip_list, 0)
  PRED_DEF("$seek_list", 4, seek_list, 0)
  PRED_DEF("throw", 1, throw, PL_FA_ISO)
  PRED_DEF("$urgent_exception", 3, urgent_exception, 0)
  PRED_DEF("halt", 1, halt, 0)
EndPredDefs
