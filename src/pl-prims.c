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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-ctype.h"
#undef ulong
#define ulong unsigned long

#undef LD
#define LD LOCAL_LD

static char 	*prependBase(int, char *);
static void	duplicate_term(term_t in, term_t copy ARG_LD);


		/********************************
		*         TYPE CHECKING         *
		*********************************/


static
PRED_IMPL("nonvar", 1, nonvar, 0)
{ PRED_LD
  return PL_is_variable(A1) ? FALSE : TRUE;
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


#if O_CYCLIC

static inline int
visitedWord(Word p ARG_LD)
{ if ( is_marked(p) )
    succeed;
  set_marked(p);
  requireStack(argument, sizeof(Word));
  *aTop++ = p;
  fail;
}


static int
visited(Functor f ARG_LD)
{ Word p = &f->definition;

  return visitedWord(p PASS_LD);
}


static void
unvisit(Word *base ARG_LD)
{ Word *p = aTop;

  while(p>base)
  { p--;
    clear_marked(*p);
  }

  aTop = base;
}


					/* see also pl-wam.c, unify() */
static inline void
linkTermsCyclic(Functor f1, Functor f2 ARG_LD)
{ Word p1 = (Word)&f1->definition;
  Word p2 = (Word)&f2->definition;

  *p1 = makeRefG(p2);
  requireStack(argument, sizeof(Word));
  *aTop++ = p1;
}


static inline void
exitCyclic(Word *base ARG_LD)
{ Word *sp = aTop;

  while(sp>base)
  { Word p;

    sp--;
    p = *sp;
    *p = *unRef(*p);
  }

  aTop = base;
}

#else

static inline visited(Functor f ARG_LD) { fail; }
static inline unvisit(Word *base ARG_LD) { }
static inline void exitCyclic(ARG1_LD) {}
static inline void linkTermsCyclic(Functor f1, Functor f2 ARG_LD) {}

#endif /*O_CYCLIC*/

static int
ground(Word p ARG_LD)
{ int arity;
  Functor f;

last:
  deRef(p);

  if ( canBind(*p) )		/* attributed variables are not ground */
    fail;
  if ( !isTerm(*p) )
    succeed;

  f = valueTerm(*p);
  arity = arityFunctor(f->definition);
  p = f->arguments;
  if ( visited(f PASS_LD) )	/* already been here, so it must be ground */
    succeed;

  for(; --arity > 0; p++)
  { if ( !ground(p PASS_LD) )
      fail;
  }

  goto last;
}


int
PL_is_ground(term_t t)
{ GET_LD

  Word *m = aTop;
  int rc;

  rc = ground(valTermRef(t) PASS_LD);
  unvisit(m PASS_LD);

  return rc;
}


static
PRED_IMPL("ground", 1, ground, 0)
{ PRED_LD
  Word *m = aTop;
  int rc;

  rc = ground(valTermRef(A1) PASS_LD);
  unvisit(m PASS_LD);

  return rc;
}


static
PRED_IMPL("compound", 1, compound, 0)
{ return PL_is_compound(A1);
}


static
PRED_IMPL("callable", 1, callable, 0)
{ PRED_LD
  if ( PL_is_atom(A1) || PL_is_compound(A1) )
    succeed;

  fail;
}


static int
is_acyclic(Word p ARG_LD)
{ int arity;
  Functor f;

last:
  deRef(p);

  if ( !isTerm(*p) )
    succeed;

  f = valueTerm(*p);
  arity = arityFunctor(f->definition);
  p = f->arguments;
  if ( visited(f PASS_LD) )	/* Got a cycle! */
    fail;

  for(; --arity > 0; p++)
  { if ( !is_acyclic(p PASS_LD) )
      fail;
  }

  goto last;
}


static
PRED_IMPL("acyclic_term", 1, acyclic_term, 0)
{ PRED_LD
  Word *m = aTop;
  int rc;

  rc = is_acyclic(valTermRef(A1) PASS_LD);
  unvisit(m PASS_LD);

  return rc;
}


static
PRED_IMPL("cyclic_term", 1, cyclic_term, 0)
{ PRED_LD
  Word *m = aTop;
  int rc;

  rc = is_acyclic(valTermRef(A1) PASS_LD);
  unvisit(m PASS_LD);

  return rc ? FALSE : TRUE;
}


		 /*******************************
		 *	 META-CALL SUPPORT	*
		 *******************************/

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
      case CHP_FOREIGN:
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


#ifdef O_HASHTERM
		 /*******************************
		 *	    HASH-TERM		*
		 *******************************/

static bool
termHashValue(word term, long *hval ARG_LD)
{ for(;;)
  { switch(tag(term))
    { case TAG_VAR:
      case TAG_ATTVAR:
	fail;
      case TAG_ATOM:
	*hval = atomValue(term)->hash_value;
        succeed;
      case TAG_STRING:
      { unsigned len;
	char *s;

	s = getCharsString(term, &len);
	*hval = unboundStringHashValue(s, len);

        succeed;
      }
      case TAG_INTEGER:
	*hval = valInteger(term);
        succeed;
      case TAG_FLOAT:
      { int i;
	long *p = (long *)valIndirectP(term);
	
	*hval = *p;
	for(p++, i=WORDS_PER_DOUBLE-1; --i >= 0; )
	  *hval ^= *p++;

	succeed;
      }
      case TAG_COMPOUND:
      { Functor f = valueTerm(term);
	int arity = arityFunctor(f->definition);
	Word a = f->arguments;

	if ( visited(f PASS_LD) )
	  succeed;

	*hval = atomValue(nameFunctor(f->definition))->hash_value + arity;
	for(; arity; arity--, a++)
	{ long av;
	  Word a2;

	  deRef2(a, a2);
	  if ( termHashValue(*a2, &av PASS_LD) )
	    *hval += av << (arity % 8);
	  else
	    fail;
	}
        succeed;
      }
      case TAG_REFERENCE:
	term = *unRef(term);
        continue;
    }
  }
}


/* hash_term(+Term, -HashKey */

static
PRED_IMPL("hash_term", 2, hash_term, 0)
{ PRED_LD
  Word p = valTermRef(A1);
  long hraw;
  Word *m = aTop;
  int rc;

  deRef(p);

  rc = termHashValue(*p, &hraw PASS_LD);
  unvisit(m PASS_LD);

  if ( rc )
  { hraw = hraw & PLMAXTAGGEDINT;	/* ensure tagged */

    return PL_unify_integer(A2, hraw);
  }

  succeed;
}

#endif /*O_HASHTERM*/


		/********************************
		*        STANDARD ORDER         *
		*********************************/

static int
compareAtoms(atom_t w1, atom_t w2)
{ Atom a1 = atomValue(w1);
  Atom a2 = atomValue(w2);

  if ( a1->type == a2->type )
  { if ( a1->type->compare )
    { return (*a1->type->compare)(w1, w2);
    } else
    { int l   = (a1->length <= a2->length ? a1->length : a2->length);
      int v;

      if ( (v=memcmp(a1->name, a2->name, l)) != 0 )
	return v;
      return (int)a1->length - (int)a2->length;
    }
  } else
  { return a1->type->rank - a2->type->rank;
  }
}


static int
compareStrings(word w1, word w2 ARG_LD)
{ char *s1, *s2;
  int l1, l2, l, v;

  s1 = getCharsString(w1, &l1);
  s2 = getCharsString(w2, &l2);
  l = (l1 < l2 ? l1 : l2);

  if ( (v=memcmp(s1, s2, l)) != 0 )
    return v;

  return l1-l2;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
compareStandard(Word p1, Word p2, int eq)

    Rules:

    Var @< AttVar @< Number @< Atom @< String < Term
    
    OldVar < NewVar	(not relyable)
    Atom:	alphabetically
    Strings:	alphabetically
    number:	value
    Term:	arity / alphabetically / recursive

If eq == TRUE, only test for equality. In this case expensive inequality
tests (alphabetical order) are skipped and the call returns NOTEQ.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LESS    -1
#define EQUAL    0
#define GREATER  1
#define NOTEQ    2

static int
do_compare(Word p1, Word p2, int eq ARG_LD)
{ word w1, w2;
  int t1, t2;

tail_recursion:
  deRef(p1);
  deRef(p2);
  w1 = *p1;
  w2 = *p2;
  
  if ( w1 == w2 )
  { if ( isVar(w1) )
      goto cmpvars;
    return EQUAL;
  }

  t1 = tag(w1);
  t2 = tag(w2);

  if ( t1 != t2 )
  { if ( !trueFeature(ISO_FEATURE) && !eq )
    { if ( t1 == TAG_INTEGER && t2 == TAG_FLOAT )
      { real f1 = (real)valInteger(w1);
	real f2 = valReal(w2);
  
	return f1 < f2 ? LESS : f1 == f2 ? EQUAL : GREATER;
      } else if ( t1 == TAG_FLOAT && t2 == TAG_INTEGER )
      { real f1 = valReal(w1);
	real f2 = (real)valInteger(w2);
  
	return f1 < f2 ? LESS : f1 == f2 ? EQUAL : GREATER;
      }
    }

    if ( t1 > TAG_ATTVAR || t2 > TAG_ATTVAR )
      return t1 < t2 ? LESS : GREATER;
  }

  switch(t1)
  { case TAG_VAR:
    case TAG_ATTVAR:
    cmpvars:
      return p1 < p2 ? LESS : p1 == p2 ? EQUAL : GREATER;
    case TAG_INTEGER:
    { long l1 = valInteger(w1);
      long l2 = valInteger(w2);

      return l1 < l2 ? LESS : l1 == l2 ? EQUAL : GREATER;
    }
    case TAG_FLOAT:
    { real f1 = valReal(w1);
      real f2 = valReal(w2);

      return f1 < f2 ? LESS : f1 == f2 ? EQUAL : GREATER;
    }
    case TAG_ATOM:
      return eq ? NOTEQ : compareAtoms(w1, w2);
    case TAG_STRING:
      return compareStrings(w1, w2 PASS_LD);
    case TAG_COMPOUND:
    { Functor f1 = (Functor)valPtr(w1);
      Functor f2 = (Functor)valPtr(w2);

#if O_CYCLIC
      while ( isRef(f1->definition) )
	f1 = (Functor)unRef(f1->definition);
      while ( isRef(f2->definition) )
	f2 = (Functor)unRef(f2->definition);
      if ( f1 == f2 )
	return EQUAL;
#endif

      if ( f1->definition != f2->definition )
      { FunctorDef fd1 = valueFunctor(f1->definition);
	FunctorDef fd2 = valueFunctor(f2->definition);

	if ( fd1->arity != fd2->arity )
	  return fd1->arity > fd2->arity ? GREATER : LESS;

	return eq ? NOTEQ : compareAtoms(fd1->name, fd2->name);
      } else
      { int arity = arityFunctor(f1->definition);
	int rval;
	
	p1 = f1->arguments;
	p2 = f2->arguments;
	linkTermsCyclic(f1, f2 PASS_LD);
	for( ; --arity > 0; p1++, p2++ )
	{ if ((rval = do_compare(p1, p2, eq PASS_LD)) != EQUAL)
	    return rval;
	}
        goto tail_recursion;
      }
    }
    default:
      assert(0);
      return EQUAL;
  }
}


int
compareStandard(Word p1, Word p2, int eq ARG_LD)
{ Word *m = aTop;
  int rc;

  rc = do_compare(p1, p2, eq PASS_LD);
  exitCyclic(m PASS_LD);

  return rc;
}


/* compare(-Diff, +T1, +T2) */

static
PRED_IMPL("compare", 3, compare, 0)
{ PRED_LD
  Word p1 = valTermRef(A2);
  Word p2 = p1+1;

  int val = compareStandard(p1, p2, FALSE PASS_LD);

  return PL_unify_atom(A1, val < 0 ? ATOM_smaller :
		           val > 0 ? ATOM_larger :
		                     ATOM_equals);
}


static
PRED_IMPL("@<", 2, std_lt, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;

  return compareStandard(p1, p2, FALSE PASS_LD) < 0 ? TRUE : FALSE;
}


static
PRED_IMPL("@=<", 2, std_leq, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;

  return compareStandard(p1, p2, FALSE PASS_LD) <= 0 ? TRUE : FALSE;
}


static
PRED_IMPL("@>", 2, std_gt, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;

  return compareStandard(p1, p2, FALSE PASS_LD) > 0 ? TRUE : FALSE;
}


static
PRED_IMPL("@>=", 2, std_geq, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;

  return compareStandard(p1, p2, FALSE PASS_LD) >= 0 ? TRUE : FALSE;
}

		/********************************
		*           EQUALITY            *
		*********************************/

static
PRED_IMPL("==", 2, equal, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;

  return compareStandard(p1, p2, TRUE PASS_LD) == EQUAL ? TRUE : FALSE;
}


static
PRED_IMPL("\\==", 2, nonequal, 0)
{ PRED_LD
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;

  return compareStandard(p1, p2, TRUE PASS_LD) == EQUAL ? FALSE : TRUE;
}


		/********************************
		*     STRUCTURAL EQUIVALENCE    *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  idea  for  this  predicate  is  taken  from  the  usenet   network.
Unfortunately I can't recall the author of the note.

Structural equivalency is stronger then unifyable (=), but  weaker  then
pure equivalence (==). Two terms are structural equivalent if their tree
representation is equivalent. Examples:

  a =@= A			--> false
  A =@= B			--> true
  foo(A, B) =@= foo(C, D)	--> true
  foo(A, A) =@= foo(B, C)	--> false
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct 
{ Word	v1;
  Word  v2;
} reset, *Reset;

typedef struct uchoice *UChoice;

struct uchoice
{ Word		alist1;
  Word		alist2;
  int		size;
  UChoice	next;
};

static bool
structeql(Word t1, Word t2, TmpBuffer buf ARG_LD)
{ int todo = 1;
  UChoice nextch = NULL, tailch = NULL;
  int arity;

  for(;;)
  { Word p1, p2;
    word w1, w2;

    if ( !todo )
    { if ( nextch )
      { t1 = nextch->alist1;
	t2 = nextch->alist2;
	todo = nextch->size;
	nextch = nextch->next;
      } else
	succeed;
    }

    deRef2(t1, p1);
    deRef2(t2, p2);
    w1 = *p1;
    w2 = *p2;

    todo--;
    t1++; t2++;

    if ( w1 == w2 )
    { if ( isVar(w1) )
      { word id = consInt(sizeOfBuffer(buf))|MARK_MASK;
	reset r;
  
	r.v1 = p1;
	r.v2 = p2;
	addBuffer(buf, r, reset);
	*p1 = *p2 = id;
      }
      continue;
    }
  
    if ( ((w1|w2)&MARK_MASK) || tag(w1) != tag(w2) )
      fail;

    switch(tag(w1))
    { case TAG_VAR:
      case TAG_ATOM:
	fail;
      case TAG_ATTVAR:
      { p1 = valPAttVar(w1);
	p2 = valPAttVar(w2);
	arity = 1;

	goto compound;
      }
      case TAG_INTEGER:
	if ( storage(w1) == STG_INLINE ||
	     storage(w2) == STG_INLINE )
	  fail;
      case TAG_STRING:
      case TAG_FLOAT:
	if ( equalIndirect(w1, w2) )
	  continue;
        fail;
      case TAG_COMPOUND:
      { Functor f1 = (Functor)valPtr(w1);
	Functor f2 = (Functor)valPtr(w2);

#if O_CYCLIC
        while ( isRef(f1->definition) )
	  f1 = (Functor)unRef(f1->definition);
	while ( isRef(f2->definition) )
	  f2 = (Functor)unRef(f2->definition);
	if ( f1 == f2 )
	  continue;
#endif
	
	if ( f1->definition == f2->definition )
	{ arity = arityFunctor(f1->definition);
	  
	  p1 = f1->arguments;
	  p2 = f2->arguments;
	  linkTermsCyclic(f1, f2 PASS_LD);
	  
	compound:
	  if ( todo == 0 )		/* right-most argument recursion */
	  { todo = arity;
	    t1 = p1;
	    t2 = p2;
	  } else if ( arity > 0 )
	  { UChoice next = alloca(sizeof(*next));
  
	    next->size   = arity;
	    next->alist1 = p1;
	    next->alist2 = p2;
	    next->next   = NULL;
	    if ( !nextch )
	      nextch = tailch = next;
	    else
	    { tailch->next = next;
	      tailch = next;
	    }
	  }
	} else
	{ fail;
	}
      }
    }
  }
}


static
PRED_IMPL("=@=", 2, structural_eq, 0)
{ GET_LD
  bool rval;
  tmp_buffer buf;
  Reset r;
  Word p1 = valTermRef(A1);
  Word p2 = p1+1;
  Word *m = aTop;

  deRef(p1);
  deRef(p2);

  if ( *p1 == *p2 )
    succeed;

  initBuffer(&buf);			/* can be faster! */
  rval = structeql(p1, p2, &buf PASS_LD);
  for(r = baseBuffer(&buf, reset); r < topBuffer(&buf, reset); r++)
  { setVar(*r->v1);
    setVar(*r->v2);
  }
  discardBuffer(&buf);
  exitCyclic(m PASS_LD);

  return rval;
}


static
PRED_IMPL("\\=@=", 2, structural_neq, 0)
{ return pl_structural_eq_va(PL__t0, PL__ac, PL__ctx) ? FALSE : TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
?=(X, Y) is true if we can decide for   now and forever that X and Y are
either equal or non-equal. I.e. X and Y are equal or they cannot unify.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("?=", 2, can_compare, 0)
{ PRED_LD
  mark m;
  bool rval;

  Mark(m);
  rval = PL_unify(A1, A2);
  if ( rval )
  { if ( m.trailtop != tTop )
      rval = FALSE;			/* can be equal after substitution */
  } else
    rval = TRUE;			/* cannot unify */
  Undo(m);

  return rval;
}


		/********************************
		*         TERM HACKING          *
		*********************************/

/* functor(+Term, -Name, -Arity) */
/* functor(-Term, +Name, +Arity) */

PRED_IMPL("functor", 3, functor, 0)
{ PRED_LD
  int arity;
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
  if ( isAtomic(*p) )
  { if ( !PL_unify(A2, A1) ||
	 !PL_unify_integer(A3, 0) )
      fail;

    succeed;
  }
  if ( !PL_is_atomic(A2) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atomic, A2);

  if ( !PL_get_integer_ex(A3, &arity) )
    fail;
  if ( arity == 0 )
    return PL_unify(A1, A2);
  if ( arity < 0 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_not_less_than_zero, A3);
  if ( PL_get_atom_ex(A2, &name) )
    return PL_unify_functor(A1, PL_new_functor(name, arity));

  fail;
}


static
PRED_IMPL("arg", 3, arg, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  atom_t name;
  int arity;

  term_t n    = A1;
  term_t term = A2;
  term_t arg  = A3;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { int idx;
      Word p = valTermRef(term);

      deRef(p);
      if ( isTerm(*p) )
	arity = arityTerm(*p);
      else if ( isTextAtom(*p) && !trueFeature(ISO_FEATURE) )
	arity = 0;
      else
	return PL_error("arg", 3, NULL, ERR_TYPE, ATOM_compound, term);
  
      if ( PL_get_integer(n, &idx) )
      { if ( idx > 0 && idx <= arity )
	{ Word ap = argTermP(*p, idx-1);
	
	  return unify_ptrs(valTermRef(arg), ap PASS_LD);
	}
	if ( idx < 0 )
	  return PL_error("arg", 3, NULL, ERR_DOMAIN,
			  ATOM_not_less_than_zero, n);
	fail;
      } 
      if ( PL_is_variable(n) )
      { int argn = 1;
	term_t a = PL_new_term_ref();

	for(argn=1; argn <= arity; argn++)
	{ PL_get_arg(argn, term, a);
	  if ( PL_unify(arg, a) )
	  { PL_unify_integer(n, argn);
	    if ( argn == arity )
	      succeed;
	    ForeignRedoInt(argn);
	  }
	}
	fail;
      }
      return PL_error("arg", 3, NULL, ERR_TYPE, ATOM_integer, n);
    }
    case FRG_REDO:
    { int argn = CTX_INT + 1;
      term_t a = PL_new_term_ref();

      PL_get_name_arity(term, &name, &arity);

      for(; argn <= arity; argn++)
      { PL_get_arg(argn, term, a);
	if ( PL_unify(arg, a) )
	{ PL_unify_integer(n, argn);
	  if ( argn == arity )
	    succeed;
	  ForeignRedoInt(argn);
	}
      }

      fail;
    }
    default:
      succeed;
  }
}
	

#define SETARG_BACKTRACKABLE    0x1
#define SETARG_LINK		0x2


static word
setarg(term_t n, term_t term, term_t value, int flags)
{ GET_LD
  int arity, argn;
  atom_t name;
  Word a, v;

  if ( !PL_get_integer_ex(n, &argn) )
    fail;
  if ( argn < 0 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_not_less_than_zero, n);
  if ( !PL_get_name_arity(term, &name, &arity) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_compound, term);
  
  if ( argn > arity )
    fail;

  if ( (flags & SETARG_BACKTRACKABLE) )
  { a = valTermRef(term);
    deRef(a);
    a = argTermP(*a, argn-1);

    TrailAssignment(a);
  } else
  { v = valTermRef(value);
    deRef(v);

    if ( !isAtomic(*v) )
    { if ( !(flags & SETARG_LINK) )
      { term_t copy = PL_new_term_ref();

	duplicate_term(value, copy PASS_LD);
	value = copy;
      }

      freezeGlobal(PASS_LD1);
    }

    a = valTermRef(term);		/* duplicate may shift stacks */
    deRef(a);
    a = argTermP(*a, argn-1);
  }
					/* this is unify(), but the */
					/* assignment must *not* be trailed */
  v = valTermRef(value);
  deRef(v);

  if ( isVar(*v) )
  { if ( v < a )
    { *a = makeRef(v);
    } else if ( a < v )
    { setVar(*a);
      *v = makeRef(a);
    } else
      setVar(*a);
  } else if ( isAttVar(*v) )
  { *a = makeRef(v);
  } else
    *a = *v;

  succeed;
}


static
PRED_IMPL("setarg", 3, setarg, 0)
{ return setarg(A1, A2, A3, SETARG_BACKTRACKABLE);
}


static
PRED_IMPL("nb_setarg", 3, nb_setarg, 0)
{ return setarg(A1, A2, A3, 0);
}


static
PRED_IMPL("nb_linkarg", 3, nb_linkarg, 0)
{ return setarg(A1, A2, A3, SETARG_LINK);
}



/*  Determine the length of a list. If the list is not proper (or not
    a list at all) -1 is returned.

 ** Mon Apr 18 16:29:01 1988  jan@swivax.UUCP (Jan Wielemaker)  */

int
lengthList(term_t list, int errors)
{ GET_LD
  int length = 0;
  Word l = valTermRef(list);

  deRef(l);

  while(isList(*l) )
  { length++;
    l = TailList(l);
    deRef(l);
  }

  if ( isNil(*l) )
    return length;

  if ( errors )
    PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, wordToTermRef(l));

  return isVar(*l) ? -2 : -1;
}

word
pl_univ(term_t t, term_t list)
{ GET_LD
  int arity;
  atom_t name;
  int n;

  if ( PL_is_variable(t) )
  { term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();

    if ( !PL_get_list(tail, head, tail) )
    { if ( PL_get_nil(tail) )
	return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			ATOM_not_empty_list, tail);
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, tail);
    }

    if ( PL_get_nil(tail) )		/* A =.. [H] */
      return PL_unify(t, head);
    if ( !PL_get_atom_ex(head, &name) )
      fail;
    
    if ( (arity = lengthList(tail, FALSE)) < 0 )
    { if ( arity == -1 )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, list);
      else
	return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    }

    if ( !PL_unify_functor(t, PL_new_functor(name, arity)) )
      fail;

    for(n=1; PL_get_list(tail, head, tail); n++)
    { if ( !PL_unify_arg(n, t, head) )
	fail;
    }

    succeed;
  }

					/* 1st arg is term or atom */
  if ( PL_get_name_arity(t, &name, &arity) )
  { term_t head = PL_new_term_ref();
    term_t l = PL_new_term_ref();

    if ( !PL_unify_list_ex(list, head, l) )
      fail;
    if ( !PL_unify_atom(head, name) )
      fail;

    for(n = 1; n <= arity; n++)
    { if ( !PL_unify_list_ex(l, head, l) ||
	   !PL_unify_arg(n, t, head) )
	fail;
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


static int
do_number_vars(term_t t, functor_t functor, av_action on_av, int n ARG_LD)
{ Word p;

start:
  if ( n < 0 )
    return n;				/* error */

  p = valTermRef(t);
  deRef(p);

  if ( canBind(*p) )
  { Word a;
    word v;

    if ( isAttVar(*p) )
    { switch(on_av)
      { case AV_SKIP:
	  return n;
	case AV_ERROR:
	  return -1;
	case AV_BIND:
	  break;
      }
    }

#ifdef O_SHIFT_STACKS
    if ( roomStack(global) < 2 * (long)sizeof(word) )
    { growStacks(environment_frame, NULL, NULL, FALSE, TRUE, FALSE);
      p = valTermRef(t);
      deRef(p);
    }
#else
    requireStack(global, sizeof(word)*(2));
#endif
    
    a = gTop;
    a[0] = functor;
    a[1] = makeNum(n);
    gTop += 2;
    

    v = consPtr(a, TAG_COMPOUND|STG_GLOBAL);
    if ( isAttVar(*p) )
    { assignAttVar(p, &v PASS_LD);
    } else
    { *p = v;
      Trail(p);
    }
    
    n++;
  } else if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    int arity;

    if ( visited(f PASS_LD) )
      return n;

    arity = arityFunctor(f->definition);
    if ( arity == 1 )
    { _PL_get_arg(1, t, t);
      goto start;
    } else
    { term_t a = PL_new_term_ref();
      int i;

      for(i=1; ; i++)
      { if ( i == arity )
	{ PL_reset_term_refs(a);
	  _PL_get_arg(i, t, t);
	  goto start;			/* right-recursion optimisation */
	} else
	{ _PL_get_arg(i, t, a);
	  n = do_number_vars(a, functor, on_av, n PASS_LD);
	}
      }
    }
  }

  return n;			/* anything else */
}


int
numberVars(term_t t, functor_t functor, av_action on_av, int n ARG_LD)
{ term_t h2 = PL_copy_term_ref(t);
  Word *m = aTop;
  int rval = do_number_vars(h2, functor, on_av, n PASS_LD);
  unvisit(m PASS_LD);

  PL_reset_term_refs(h2);

  return rval;
}


static const opt_spec numbervar_options[] = 
{ { ATOM_attvar,	    OPT_ATOM },
  { ATOM_functor_name,	    OPT_ATOM },
  { NULL_ATOM,	     	    0 }
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
numbervars(+Term, +Start, -End, +Options)
numbervars(+Term, +Functor, +Start, -End)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("numbervars", 4, numbervars, 0)
{ GET_LD
  int n;
  functor_t functor;
  atom_t name = ATOM_isovar;		/* '$VAR' */
  atom_t av = ATOM_error;
  term_t t, end, options;
  av_action on_av;

  t = PL_copy_term_ref(A1);

  if ( !PL_get_integer(A2, &n) )
  { if ( PL_get_atom(A2, &name) &&
	 PL_get_integer(A3, &n)	)	/* old calling conventions */
    { end = A4;
      options = 0;
    } else
    { return PL_get_integer_ex(A2, &n);
    }
  } else
  { end = A3;
    options = A4;
  }

  if ( options &&
       !scan_options(options, 0, ATOM_numbervar_option, numbervar_options,
		     &av, &name) )
    fail;

  if ( av == ATOM_error )
    on_av = AV_ERROR;
  else if ( av == ATOM_skip )
    on_av = AV_SKIP;
  else if ( av == ATOM_bind )
    on_av = AV_BIND;
  else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_numbervar_option, options);

  functor = PL_new_functor(name, 1);
  n = numberVars(t, functor, on_av, n PASS_LD);
  if ( n == -1 )
    return PL_error(NULL, 0, NULL,
		    ERR_TYPE, ATOM_free_of_attvar, A1);

  return PL_unify_integer(end, n);
}


static int
term_variables_loop(Word t, term_t l, int n ARG_LD)
{ 
right_recursion:
  deRef(t);

  if ( canBind(*t) )
  { term_t v;

    if ( visitedWord(t PASS_LD) )
      return n;

    v = PL_new_term_ref();
    *valTermRef(v) = makeRef(t);

    return n+1;
  }
  if ( isTerm(*t) )
  { int arity;
    Functor f = valueTerm(*t);

    if ( visited(f PASS_LD) )
      return n;
    
    arity = arityFunctor(f->definition);
    for(t = f->arguments; --arity > 0; t++)
      n = term_variables_loop(t, l, n PASS_LD);

    goto right_recursion;
  }
    
  return n;
}


static int
term_variables(term_t t, term_t vars, term_t tail ARG_LD)
{ term_t head = PL_new_term_ref();
  term_t v0   = PL_new_term_refs(0);
  Word *m     = aTop;
  int i, n;

  startCritical;
  n = term_variables_loop(valTermRef(t), v0, 0 PASS_LD);
  unvisit(m PASS_LD);
  endCritical;

  for(i=0; i<n; i++)
  { if ( !PL_unify_list(vars, head, vars) ||
	 !PL_unify(head, v0+i) )
      fail;
  }
      
  if ( tail )
    return PL_unify(vars, tail);
  else
    return PL_unify_nil(vars);
}



static
PRED_IMPL("term_variables", 2, term_variables2, 0)
{ PRED_LD

  return term_variables(A1, A2, 0 PASS_LD);
}


static
PRED_IMPL("term_variables", 3, term_variables3, 0)
{ PRED_LD

  return term_variables(A1, A2, A3 PASS_LD);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_e_free_variables(V0^V1^t, vars) is used  by   setof/3  and bagof/3 to
determine  the  free  variables  in  the    goal   that  have  not  been
existentially   bound.   The   implementation   is     very   close   to
term_variables/2, but while traversing the lefthand   of ^, the variable
is marked but not added to the list.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
free_variables_loop(Word t, term_t l, int n, int existential ARG_LD)
{ 
right_recursion:
  deRef(t);

  if ( canBind(*t) )
  { term_t v;

    if ( !visitedWord(t PASS_LD) && !existential )
    { v = PL_new_term_ref();
      *valTermRef(v) = makeRef(t);

      n++;
    }

    return n;
  }
  if ( isTerm(*t) )
  { int arity;
    Functor f = valueTerm(*t);
    word fd = f->definition;

    if ( visited(f PASS_LD) )
      return n;
    
    t = f->arguments;
    if ( fd == FUNCTOR_hat2 )
    { n = free_variables_loop(t, l, n, TRUE PASS_LD);
      t++;
    } else
    { arity = arityFunctor(f->definition);
      for(; --arity > 0; t++)
	n = free_variables_loop(t, l, n, existential PASS_LD);
    }

    goto right_recursion;
  }
    
  return n;
}


word
pl_e_free_variables(term_t t, term_t vars)
{ GET_LD
  Word *vm = aTop;
  Word t2 = valTermRef(t);
  term_t v0 = PL_new_term_refs(0);
  int i, n;

  startCritical;
  n = free_variables_loop(t2, v0, 0, FALSE PASS_LD);
  unvisit(vm PASS_LD);
  endCritical;

  if ( PL_unify_functor(vars, PL_new_functor(ATOM_v, n)) )
  { for(i=0; i<n; i++)
    { TRY(PL_unify_arg(i+1, vars, v0+i));
    }

    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unifyable(@X, @Y, -Substitution)

If X can be unified to Y, unify   Substitution with a list of Variable =
value for the substitutions that must be made to make X and Y identical.

The implementation extracts the substitutions  from the trail, rewinding
the trail at the same  time.  This   is  fairly  trivial, except for the
assignments of attributed variables (assignAttVar()). The last operation
of assignAttVar() is a trailed assignment  replacing the attvar with its
value. Before that it performs two trailed  actions to update the wakeup
list. These two must be skipped.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("unifyable", 3, unifyable, 0)
{ PRED_LD
  mark m;

  Mark(m);
  if ( PL_unify(A1, A2) )
  { TrailEntry tt = tTop;
    TrailEntry mt = m.trailtop;

    if ( tt > mt )
    { Word list = allocGlobalNoShift((tt-mt)*6+1);
      Word gp = list+1;
      Word tail = list;

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
	  gp[4] = makeRef(p2);
	  gp[5] = *p2;
	} else
	{ gp[4] = makeRef(p);
	  gp[5] = *p;
	}
	gp += 6;

	if ( !isTrailVal(p) )
	{ setVar(*p);
	} else
	{ assert(isAttVar(trailVal(p)));

	  tt--;				/* re-insert the attvar */
	  *tt->address = trailVal(p);

	  tt--;				/* restore tail of wakeup list */
	  p = tt->address;
	  assert(isTrailVal(p));
	  tt--;
	  *tt->address = trailVal(p);

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
      tTop = m.trailtop;

      return PL_unify(wordToTermRef(list), A3);
    } else
      return PL_unify_atom(A3, ATOM_nil);
  } else
    fail;
}

#if O_CYCLIC

		 /*******************************
		 *	       CYCLES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
term_cycles(+Term, -Cycles)
is_cycle(+SubTerm, +Cycles)

term_cycles(+Term, -Cycles) returns a binary tree  holding the cycles of
Term sorted by address. The tree   is  represented as cycle(Cycle, Left,
Right) where [] represents terminals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


  
#endif /*O_CYCLIC*/

		 /*******************************
		 *	    COPY TERM		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
copy_term(+Term, -Copy)

Copy a term, renaming its  variables.   Attributed  variables have their
attributed copied, so futher modification of the attribute list does not
affect the copy  and  visa  versa.   The  algorithm  deals  with  shared
variables as well as cyclic  terms.  It   works,  like  unify for cyclic
terms, by creating references from  the   original  to the reference and
restoring the references using the argument stack.

There are three types of references between the original and the copy:

	* For variables we set the new variable to VAR_MARK and
	  make a reference to it.  This means that if we find a
	  reference to a variable VAR_MARK we must create a reference
	  to the same address.
	* For attributed variables we create an TAG_ATTVAR link to the
	  copy.  If we find a TAG_ATTVAR pointing to a TAG_ATTVAR we
	  no we found a copy.  Unfortunately just trailing the old
	  location doesn't suffice as we must recreate the link to
	  the old address, so we push this one first.
	* Compounds use the old trick to make the functor a reference
	  to the copy.

do_copy_term() returns TRUE if the term can   be shared and FALSE if not
(i.e. it is a variable or attributed variable). If, in sharing mode, the
copying routine copied a shareable term it   discards the copy and links
the original.

NOTE: when using the stack-shifter, we cannot   affort  the stacks to be
shifted during the execution of copy_term/2. I think the proper solution
is to try and copy. If the copy   fails  we use exitCyclic() to undo the
damage, reset gTop, get more space   and try again. allocGlobalNoShift()
is a version of allocGlobal() that returns NULL if the stack needs to be
shifted rather than doing it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define VAR_MARK (0x1<<LMASK_BITS|TAG_VAR)

static inline void
TrailCyclic(Word p ARG_LD)
{ requireStack(argument, sizeof(Word));
  *aTop++ = p;
}


static inline void
exitCyclicCopy(Word *m ARG_LD)
{ Word *sp;

  for(sp = aTop; sp > m; )
  { Word p = *--sp;

    if ( isRef(*p) )
    { Word p2 = unRef(*p);

      if ( *p2 == VAR_MARK )		/* sharing variables */
      { setVar(*p2);
	setVar(*p);
      } else
      { *p = *p2;			/* cyclic terms */
      }
    } else				/* attributed variable */
    { Word old = *--sp;
      *p = consPtr(old, STG_GLOBAL|TAG_ATTVAR);
    }
  }
  aTop = sp;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FALSE: term cannot be shared
TRUE:  term can be shared (ground)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
do_copy_term(Word from, Word to, int share ARG_LD)
{ 

again:
  switch(tag(*from))
  { case TAG_REFERENCE:
    { Word p2 = unRef(*from);

      if ( *p2 == VAR_MARK )
      { *to = makeRef(p2);
        return FALSE;
      } else
      { from = p2;
	goto again;
      }
    }
    case TAG_VAR:
      *to = VAR_MARK;
      *from = makeRef(to);
      TrailCyclic(from PASS_LD);
      return FALSE;
    case TAG_ATTVAR:
    { Word p = valPAttVar(*from);
      
      if ( isAttVar(*p) )		/* already copied */
      { *to = makeRefG(p);
        return FALSE;
      } else
      { Word attr;			/* the new attributes */

	if ( !onStackArea(global, to) )
	{ Word t = allocGlobalNoShift(1);
	  
	  *to = makeRefG(t);
	  to = t;
	}
	attr = allocGlobalNoShift(1);
	TrailCyclic(p PASS_LD);
	TrailCyclic(from PASS_LD);
	*from = consPtr(to, STG_GLOBAL|TAG_ATTVAR);
	*to = consPtr(attr, STG_GLOBAL|TAG_ATTVAR);

	do_copy_term(p, attr, FALSE PASS_LD);	/* copy attribute value */
	return FALSE;
      }
    }
    case TAG_ATOM:
    case TAG_FLOAT:
    case TAG_INTEGER:
    case TAG_STRING:
      *to = *from;
      return TRUE;
    case TAG_COMPOUND:
    { Functor f1 = valueTerm(*from);

      if ( isRef(f1->definition) )
      { *to = consPtr(unRef(f1->definition), TAG_COMPOUND|STG_GLOBAL);
        return FALSE;			/* Cyclic */
      } else
      { int arity = arityFunctor(f1->definition);
	Word oldtop = gTop;
	Word to0 = to;
	Word from0 = from;
	Functor f2 = (Functor)allocGlobalNoShift(arity+1);
	int ground = TRUE;
	Word *am = aTop;

	f2->definition = f1->definition;
	f1->definition = makeRefG((Word)f2);
	TrailCyclic(&f1->definition PASS_LD);
	*to = consPtr(unRef(f1->definition), TAG_COMPOUND|STG_GLOBAL);
	
	from = &f1->arguments[0];
	to   = &f2->arguments[0];
	while(--arity > 0)
	  ground &= do_copy_term(from++, to++, share PASS_LD);
	if ( share )
	{ ground &= do_copy_term(from, to, share PASS_LD);
	  if ( ground )
	  { exitCyclicCopy(am PASS_LD);
	    gTop = oldtop;
	    *to0 = *from0;
	    DEBUG(2, Sdprintf("Shared\n"));
	    return TRUE;
	  } else
	    return FALSE;
	} else
	  goto again;
      }
    }
    default:
      assert(0);
      return FALSE;
  }
}


static
PRED_IMPL("copy_term", 2, copy_term, 0)
{ PRED_LD
  term_t copy = PL_new_term_ref();
  Word *m = aTop;

  do_copy_term(valTermRef(A1), valTermRef(copy), TRUE PASS_LD);
  exitCyclicCopy(m PASS_LD);
  
  return PL_unify(copy, A2);
}


static void
duplicate_term(term_t in, term_t copy ARG_LD)
{ Word *m = aTop;

  do_copy_term(valTermRef(in), valTermRef(copy), FALSE PASS_LD);
  exitCyclicCopy(m PASS_LD);
}


static
PRED_IMPL("duplicate_term", 2, duplicate_term, 0)
{ PRED_LD
  term_t copy = PL_new_term_ref();

  duplicate_term(A1, copy PASS_LD);
  
  return PL_unify(copy, A2);
}


#undef LD
#define LD GLOBAL_LD

		 /*******************************
		 *	       ATOMS		*
		 *******************************/

word
pl_atom_length(term_t w, term_t n)
{ char *s;
  unsigned int len;
  int flags;

  if ( trueFeature(ISO_FEATURE) )
    flags = CVT_ATOM|CVT_STRING;	/* strings are not known to ISO */
  else
    flags = CVT_ALL;

  if ( PL_get_nchars_ex(w, &len, &s, flags) )
  { int nval;

    if ( PL_is_variable(n) )
      return PL_unify_integer(n, len);
    else if ( PL_get_integer(n, &nval) )
      return nval == (int)len ? TRUE	: FALSE;
    else
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, n);
  }

  fail;
}


static char *
prependBase(int b, char *s)
{ *s-- = '\'';
  while(b > 0)
  { *s-- = digitName(b % 10, TRUE);
    b /= 10;
  }

  return s;
}

word
pl_int_to_atom(term_t number, term_t base, term_t atom)
{ int n, b;
  char result[100];
  char *s = &result[99];

  *s-- = EOS;
  if ( !PL_get_integer(number, &n) ||
       !PL_get_integer(base, &b) )
    return warning("int_to_atom/3: instantiation fault");

  if ( b == 0 && n > 0 && n < 256 )
  { *s-- = (char) n;
    *s-- = '\'';
    *s = '0';
    return PL_unify_atom_chars(atom, s);
  }

  if ( b > 36 || b < 2 )
    return warning("int_to_atom/3: Illegal base: %d", b);

  if ( n == 0 )
  { *s-- = '0';
  } else
  { while( n > 0 )
    { *s-- = digitName((int)(n % b), TRUE);
      n /= b;
    }
  }
  if ( b != 10 )
    s = prependBase(b, s);

  return PL_unify_atom_chars(atom, s+1);
}

/*  format an integer according to  a  number  of  modifiers  at various
    radius.   `split'  is a boolean asking to put ',' between each group
    of three digits (e.g. 67,567,288).  `div' askes to divide the number
    by radix^`div' before printing.   `radix'  is  the  radix  used  for
    conversion.  `n' is the number to be converted.

 ** Fri Aug 19 22:26:41 1988  jan@swivax.UUCP (Jan Wielemaker)  */

char *
formatInteger(bool split, int div, int radix, bool small, long int n,
	      char *out)
{ char tmp[100];
  char *s = &tmp[sizeof(tmp)-1];	/* i.e. start at the end */
  int before = (div == 0);
  int digits = 0;
  bool negative = FALSE;

  *s = EOS;
  if ( n < 0 )
  { n = -n;
    negative = TRUE;
  }
  if ( n == 0 && div == 0 )
  { out[0] = '0';
    out[1] = EOS;
    return out;
  }
  while( n > 0 || div >= 0 )
  { if ( div-- == 0 && !before )
    { *--s = '.';
      before = 1;
    }
    if ( split && before && (digits++ % 3) == 0 && digits != 1 )
      *--s = ',';
    *--s = digitName((int)(n % radix), small);
    n /= radix;
  }
  if ( negative )
    *--s = '-';  

  return strcpy(out, s);
}	  


#define X_AUTO   0x00
#define X_ATOM   0x01
#define X_NUMBER 0x02
#define X_MASK   0x0f
#define X_CHARS  0x10

static word
x_chars(const char *pred, term_t atom, term_t string, int how)
{ char *s;
  pl_wchar_t *ws;
  unsigned int len;
  int arg1;

  if ( (how & X_NUMBER) )
  { arg1 = PL_get_nchars(atom, &len, &s, CVT_NUMBER);
  } else
  { if ( !(arg1 = PL_get_nchars(atom, &len, &s, CVT_ATOMIC)) )
    { s = NULL;
      arg1 = PL_get_wchars(atom, &len, &ws, CVT_ATOM|CVT_STRING);
    }
  }

  if ( arg1 )
  { int ok;

    if ( s )
    { if ( how & X_CHARS )
	ok = PL_unify_list_nchars(string, len, s);
      else
	ok = PL_unify_list_ncodes(string, len, s);
    } else
    { int flags = (how & X_CHARS) ? PL_CHAR_LIST : PL_CODE_LIST;
      ok = PL_unify_wchars(string, flags, len, ws);
    }

    if ( ok || !(how & X_NUMBER) )
      return ok;
  } else if ( !PL_is_variable(atom) )
  { return PL_error(pred, 2, NULL, ERR_TYPE,
		    (how & X_NUMBER) ? ATOM_number : ATOM_atom,
		    atom);
  }

  if ( !PL_get_list_nchars(string, &len, &s, 0) )
  { if ( !PL_is_list(string) )
      return PL_error(pred, 2, NULL,
		      ERR_TYPE, ATOM_list, string);
    s = NULL;
    if ( !PL_get_wchars(string, &len, &ws, CVT_LIST) )
      return PL_error(pred, 2, NULL,
		      ERR_REPRESENTATION,
		      ATOM_character_code);
  }

  how &= X_MASK;

  switch(how)
  { case X_ATOM:
    case_atom:
      if ( s )
	return PL_unify_atom_nchars(atom, len, s);
      else
	return PL_unify_wchars(atom, PL_ATOM, len, ws);
    case X_AUTO:
    case X_NUMBER:
    default:
    { number n;
      unsigned char *q;

      if ( s && get_number((unsigned char *)s, &q, &n, FALSE) && *q == EOS )
      { if ( intNumber(&n) )
	  return PL_unify_integer(atom, n.value.i);
	else
	  return PL_unify_float(atom, n.value.f);
      }
      if ( how == X_AUTO )
	goto case_atom;
      else
	return PL_error(pred, 2, NULL, ERR_SYNTAX, "illegal_number");
    }
  }
}


word
pl_name(term_t atom, term_t string)
{ return x_chars("name", atom, string, X_AUTO);
}


word
pl_atom_chars(term_t atom, term_t string)
{ return x_chars("atom_chars", atom, string, X_ATOM|X_CHARS);
}


word
pl_atom_codes(term_t atom, term_t string)
{ return x_chars("atom_codes", atom, string, X_ATOM);
}


word
pl_number_chars(term_t atom, term_t string)
{ return x_chars("number_chars", atom, string, X_NUMBER|X_CHARS);
}


word
pl_number_codes(term_t atom, term_t string)
{ return x_chars("number_chars", atom, string, X_NUMBER);
}


word
pl_char_code(term_t atom, term_t chr)
{ char *s;
  int n;

  if ( PL_get_atom_chars(atom, &s) && strlen(s) == 1 )
  { int i = s[0] & 0xff;

    return PL_unify_integer(chr, i);
  } else if ( PL_get_integer(chr, &n) )
  { if ( n >= 0 )
      return PL_unify_atom(atom, codeToAtom(n));

    return PL_error("char_code", 2, NULL, ERR_REPRESENTATION,
		    ATOM_character_code);
  }

  return PL_error("char_code", 2, NULL, ERR_TYPE, ATOM_character, atom);
}


static
PRED_IMPL("atom_number", 2, atom_number, 0)
{ char *s;
  unsigned len;

  if ( PL_get_nchars(A1, &len, &s, CVT_ATOM|CVT_STRING) )
  { number n;
    unsigned char *q;

    if ( get_number((unsigned char *)s, &q, &n, FALSE) && *q == EOS )
    { if ( intNumber(&n) )
	return PL_unify_integer(A2, n.value.i);
      else
	return PL_unify_float(A2, n.value.f);
    } else
      return PL_error(NULL, 0, NULL, ERR_SYNTAX, "illegal_number");
  } else if ( PL_get_nchars(A2, &len, &s, CVT_NUMBER) )
    return PL_unify_atom_nchars(A1, len, s);
  else if ( !PL_is_variable(A2) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_number, A2);
  else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, A1);
}



static bool
isPrefix(char *s, char *q)		/* s is prefix of q */
{ while(*s && *s == *q)
    s++, q++;

  return *s == EOS ? TRUE : FALSE;
}


word
pl_atom_prefix(term_t atom, term_t prefix)
{ char *a, *p;

  if ( PL_get_chars_ex(atom,   &a, CVT_ATOMIC|BUF_RING) &&
       PL_get_chars_ex(prefix, &p, CVT_ATOMIC|BUF_RING) )
    return isPrefix(p, a);

  return FALSE;
}


static word
concat(const char *pred,
       term_t a1, term_t a2, term_t a3, 
       control_t ctx,
       int otype)			/* PL_ATOM or PL_STRING */
{ PL_chars_t t1, t2, t3;
  int rc;

#define L1 t1.length
#define L2 t2.length
#define L3 t3.length

  if ( ForeignControl(ctx) == FRG_CUTTED )
    succeed;

  t1.text.t = t2.text.t = t3.text.t = NULL;
  t1.flags  = t2.flags  = t3.flags = 0;

  PL_get_text(a1, &t1, CVT_ATOMIC);
  PL_get_text(a2, &t2, CVT_ATOMIC);
  PL_get_text(a3, &t3, CVT_ATOMIC);

  if ( !t1.text.t && !PL_is_variable(a1) )
  { rc = PL_error(pred, 3, NULL, ERR_TYPE, ATOM_atomic, a1);
    goto out;
  }
  if ( !t2.text.t && !PL_is_variable(a2) )
  { rc = PL_error(pred, 3, NULL, ERR_TYPE, ATOM_atomic, a2);
    goto out;
  }
  if ( !t3.text.t && !PL_is_variable(a3) )
  { err3:
    rc = PL_error(pred, 3, NULL, ERR_TYPE, ATOM_atomic, a3);
    goto out;
  }

  if (t1.text.t && t2.text.t)
  { PL_chars_t c;
    PL_chars_t *v[2];

    v[0] = &t1;
    v[1] = &t2;

    PL_concat_text(2, v, &c);

    rc = PL_unify_text(a3, &c, otype);
    goto out;
  }

  if ( !t3.flags ) 
    goto err3;

  if ( t1.flags )			/* +, -, + */
  { if ( L1 <= L3 &&
	 PL_cmp_text(&t1, 0, &t3, 0, L1) == 0 )
      return PL_unify_text_range(a2, &t3, L1, L3-L1, otype);
    fail;
  } else if ( t2.flags )		/* -, +, + */
  { if ( L2 < L3 &&
	 PL_cmp_text(&t2, 0, &t3, L3-L2, L2) == 0 )
      return PL_unify_text_range(a1, &t3, 0, L3-L2, otype);
    fail;
  } else				/* -, -, + */
  { unsigned int at_n;
    mark m;

    switch ( ForeignControl(ctx) )
    { case FRG_FIRST_CALL:
        if ( L3 == 0 )
	{ rc = FALSE;
	  goto out;
	}
	at_n = 0;
        break;
      case FRG_REDO:
	at_n = ForeignContextInt(ctx);
        break;
      default:
	succeed;
    }

    Mark(m);
    for(; at_n <= L3; at_n++)
    { if ( PL_unify_text_range(a2, &t3, at_n, L3-at_n, otype) &&
	   PL_unify_text_range(a1, &t3, 0,    at_n, otype) )
      { if ( at_n < L3 )
	  ForeignRedoInt(at_n+1);
	else
	{ rc = TRUE;
	  goto out;
	}
      }
    }
    rc = FALSE;
    goto out;
  }    

out:
  PL_free_text(&t1);
  PL_free_text(&t2);
  PL_free_text(&t3);

#undef L1
#undef L2
#undef L3

  return rc;
}


word
pl_atom_concat(term_t a1, term_t a2, term_t a3, control_t ctx)
{ return concat("atom_concat", a1, a2, a3, ctx, PL_ATOM);
}


static int
split_atom(term_t list, term_t sep, term_t atom)
{ char *sp, *text;
  unsigned int splen, tlen;
  int i, last;
  term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  if ( !sep )
    return -1;
  if ( !PL_get_nchars(atom, &tlen,  &text, CVT_ATOMIC|BUF_RING) ||
       !PL_get_nchars(sep, &splen, &sp, CVT_ATOMIC|BUF_RING) )
    return -1;

  for(last=i=0; i<=(int)tlen-(int)splen; )
  { if ( memcmp(sp, text+i, splen) == 0 )
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_atom_nchars(head, i-last, text+last) )
	fail;
      i += splen;
      last = i;
    } else
      i++;
  }

  if ( !PL_unify_list(tail, head, tail) ||
       !PL_unify_atom_nchars(head, tlen-last, text+last) )
    fail;

  return PL_unify_nil(tail);
}


word
pl_concat_atom3(term_t list, term_t sep, term_t atom)
{ term_t l = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  int first = TRUE;
  char *sp;
  unsigned int splen;
  tmp_buffer b;
  
  if ( sep )
  { if ( !PL_get_nchars(sep, &splen, &sp, CVT_ATOMIC|BUF_RING) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_text, sep);
  } else
  { sp = NULL;
    splen = 0;
  }

  initBuffer(&b);
  while( PL_get_list(l, head, l) )
  { char *s;
    unsigned int slen;

    if ( !PL_get_nchars(head, &slen, &s, CVT_ATOMIC) )
    { discardBuffer(&b);
      switch(split_atom(list, sep, atom))
      { case -1:
	  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_text, head);
	case 0:
	  fail;
	default:
	  succeed;
      }
    }

    if ( first )
      first = FALSE;
    else if ( splen )
      addMultipleBuffer(&b, sp, splen, char);

    addMultipleBuffer(&b, s, slen, char);
  }

  if ( PL_get_nil(l) )
  { int rval;
    unsigned int len = entriesBuffer(&b, char);
    char *s = baseBuffer(&b, char);

    rval = PL_unify_atom_nchars(atom, len, s);
    discardBuffer(&b);
    
    return rval;
  }

  discardBuffer(&b);
  switch(split_atom(list, sep, atom))
  { case -1:
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
    case 0:
      fail;
    default:
      succeed;
  }
}


word
pl_concat_atom(term_t list, term_t atom)
{ return pl_concat_atom3(list, 0, atom);
}


word
pl_apropos_match(term_t a1, term_t a2)
{ char *s1=NULL, *s2=NULL;

  if ( PL_get_chars(a1, &s1, CVT_ALL|BUF_RING) &&
       PL_get_chars(a2, &s2, CVT_ALL) )
  { char *s, *q;

    for (; *s2; s2++)
    { for(q=s1, s=s2; *q && *s; q++, s++)
      { if ( *q != *s && *q != toLower(*s) )
	  break;
      }
      if ( *q == EOS )
	succeed;
    }
    fail;
  }
  
  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_text, s1 ? a2 : a1);
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
  int n1;				/* 1-st state id */
  int n2;				/* 2-nd state id */
  int n3;
} sub_state;


static int
get_positive_integer_or_unbound(term_t t, int *v)
{ int i;

  if ( PL_get_integer(t, &i) )
  { if ( i < 0 )
      PL_error(NULL, 0, NULL, ERR_DOMAIN,
	       ATOM_not_less_than_zero, t);
    *v = i;

    return TRUE;
  }

  if ( PL_is_variable(t) )
    return TRUE;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}



static foreign_t
sub_text(term_t atom,
	 term_t before, term_t len, term_t after,
	 term_t sub,
	 control_t h,
	 int (*out)(term_t h, unsigned int len, const char *s))
{ char *aa, *s = NULL;			/* the string */
  int b = -1, l = -1, a = -1;		/* the integers */
  unsigned int la;			/* length of `atom' */
  unsigned int ls;			/* length of `sub' */
  sub_state *state;			/* non-deterministic state */
  atom_t expected = (out == PL_unify_string_nchars ? ATOM_string : ATOM_atom);
  int match;
  mark mrk;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { if ( !PL_get_nchars(atom, &la, &aa, CVT_ATOMIC) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, expected, atom);

      if ( !get_positive_integer_or_unbound(before, &b) ||
	   !get_positive_integer_or_unbound(len, &l) ||
	   !get_positive_integer_or_unbound(after, &a) )
	fail;

      if ( !PL_get_nchars(sub, &ls, &s, CVT_ATOMIC) )
      { if ( !PL_is_variable(sub) )
	  return PL_error(NULL, 0, NULL, ERR_TYPE, expected, sub);
      }

      if ( s )				/* `sub' given */
      { if ( l >= 0 && (int)ls != l )	/* len conflict */
	  fail;
	if ( b >= 0 )			/* before given: test */
	{ if ( memcmp(aa+b, s, ls) == 0 )
	  { return (PL_unify_integer(len, ls) &&
		    PL_unify_integer(after, la-ls-b)) ? TRUE : FALSE;
	  }
	  fail;
	}
	if ( a >= 0 )			/* after given: test */
	{ if ( memcmp(aa+la-a-ls, s, ls) == 0 )
	  { return (PL_unify_integer(len, ls) &&
		    PL_unify_integer(before, la-ls-a)) ? TRUE : FALSE;
	  }
	  fail;
	}
	state = allocHeap(sizeof(*state));
	state->type = SUB_SEARCH;
	state->n1   = 0;
	state->n2   = la;
	state->n3   = ls;
	break;
      }

      if ( b >= 0 )			/* before given */
      { if ( b > (int)la )
	  fail;

	if ( l >= 0 )			/* len given */
	{ if ( b+l <= (int)la )		/* deterministic fit */
	  { if ( PL_unify_integer(after, la-b-l) &&
		 (*out)(sub, l, aa+b) )
	      succeed;
	  }
	  fail;
	}
	if ( a >= 0 )			/* after given */
	{ if ( (l = la-a-b) >= 0 )
	  { if ( PL_unify_integer(len, l) &&
		 (*out)(sub, l, aa+b) )
	      succeed;
	  }

	  fail;
	}
	state = allocHeap(sizeof(*state));
	state->type = SUB_SPLIT_TAIL;
	state->n1   = 0;		/* len of the split */
	state->n2   = la;		/* length of the atom */
	state->n3   = b;		/* length before */
	break;
      }

      if ( l >= 0 )			/* no before, len given */
      { if ( l > (int)la )
	  fail;

	if ( a >= 0 )			/* len and after */
	{ if ( (b = la-a-l) >= 0 )
	  { if ( PL_unify_integer(before, b) &&
		 (*out)(sub, l, aa+b) )
	      succeed;
	  }

	  fail;
	}
	state = allocHeap(sizeof(*state));
	state->type = SUB_SPLIT_LEN;
	state->n1   = 0;		/* before */
	state->n2   = l;		/* length */
	state->n3   = la;
	break;
      }

      if ( a >= 0 )			/* only after given */
      { if ( a > (int)la )
	  fail;

	state = allocHeap(sizeof(*state));
	state->type = SUB_SPLIT_HEAD;
	state->n1   = 0;		/* before */
	state->n2   = la;
	state->n3   = a;
	break;
      }

      state = allocHeap(sizeof(*state));
      state->type = SUB_ENUM;
      state->n1	= 0;			/* before */
      state->n2 = 0;			/* len */
      state->n3 = la;			/* total length */
      break;
    }
    case FRG_REDO:
      state = ForeignContextPtr(h);
      PL_get_chars(atom, &aa, CVT_ATOMIC);
      break;
    case FRG_CUTTED:
    exit_succeed:
      state = ForeignContextPtr(h);
      if ( state )
	freeHeap(state, sizeof(*state));
      succeed;
    default:
      assert(0);
      fail;
  }

  Mark(mrk);
again:
  switch(state->type)
  { case SUB_SEARCH:
    { PL_get_chars(sub,  &s,  CVT_ATOMIC);
      la = state->n2;
      ls = state->n3;

      for( ; state->n1+ls <= la; state->n1++ )
      { if ( memcmp(aa+state->n1, s, ls) == 0 )
	{ match = (PL_unify_integer(before, state->n1) &&
		   PL_unify_integer(len,    ls) &&
		   PL_unify_integer(after,  la-ls-state->n1));
	  
	  state->n1++;
	  goto next;
	}
      }
      goto exit_fail;
    }
    case SUB_SPLIT_TAIL:		/* before given, rest unbound */
    { la = state->n2;
      b  = state->n3;
      l  = state->n1++;

      match = (PL_unify_integer(len, l) &&
	       PL_unify_integer(after, la-b-l));
    out:
      match = (match && (*out)(sub, l, aa+b));
      if ( b+l < (int)la )
	goto next;
      else if ( match )
	goto exit_succeed;
      else
	goto exit_fail;
    }
    case SUB_SPLIT_LEN:
    { b  = state->n1++;
      l  = state->n2;
      la = state->n3;

      match = (PL_unify_integer(before, b) &&
	       PL_unify_integer(after, la-b-l));
      goto out;
    }
    case SUB_SPLIT_HEAD:
    { b  = state->n1++;
      la = state->n2;
      a  = state->n3;
      l  = la - a - b;

      match = (PL_unify_integer(before, b) &&
	       PL_unify_integer(len, l) &&
	       (*out)(sub, l, aa+b));
      if ( l > 0 )
	goto next;
      else if ( match )
	goto exit_succeed;
      else
	goto exit_fail;
    }
    case SUB_ENUM:
    { b  = state->n1;
      l  = state->n2++;
      la = state->n3;
      a  = la-b-l;

      match = (PL_unify_integer(before, b) &&
	       PL_unify_integer(len, l) &&
	       PL_unify_integer(after, a) &&
	       (*out)(sub, l, aa+b));
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
  freeHeap(state, sizeof(*state));
  fail;

next:
  if ( match )
  { ForeignRedoPtr(state);
  } else
  { Undo(mrk);
    goto again;
  }
}


foreign_t
pl_sub_atom(term_t atom,
	    term_t before, term_t len, term_t after,
	    term_t sub,
	    control_t h)
{ return sub_text(atom, before, len, after, sub, h, PL_unify_atom_nchars);
}


#if O_STRING
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provisional String manipulation functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_string_length(term_t str, term_t l)
{ char *s;
  unsigned int len;

  if ( PL_get_string(str, &s, &len) ||
       PL_get_nchars(str, &len, &s, CVT_ALL) )
    return PL_unify_integer(l, len);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_string, str);
}


word
pl_string_concat(term_t a1, term_t a2, term_t a3, control_t h)
{ return concat("string_concat", a1, a2, a3, h, PL_STRING);
}


word
pl_string_to_atom(term_t str, term_t a)
{ char *s;
  unsigned int len;

  if ( PL_get_nchars(str, &len, &s, CVT_ALL) )
    return PL_unify_atom_nchars(a, len, s);
  if ( PL_get_nchars(a, &len, &s, CVT_ALL) )
    return PL_unify_string_nchars(str, len, s);

  return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
}


word
pl_string_to_list(term_t str, term_t list)
{ char *s;
  unsigned int len;

  if ( PL_get_nchars(str, &len, &s, CVT_ALL) )
    return PL_unify_list_ncodes(list, len, s);
  if ( PL_get_list_nchars(list, &len, &s, 0) )	/* string_to_list(S, []). */
    return PL_unify_string_nchars(str, len, s);
  if ( PL_get_nchars(list, &len, &s, CVT_ALL) )
    return PL_unify_string_nchars(str, len, s);

  return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
}


foreign_t
pl_sub_string(term_t atom,
	      term_t before, term_t len, term_t after,
	      term_t sub,
	      control_t h)
{ return sub_text(atom, before, len, after, sub, h, PL_unify_string_nchars);
}

#endif /* O_STRING */


word
pl_write_on_string(term_t goal, term_t target)
{ char buf[1024];
  char *str = buf;
  int size = sizeof(buf);
  IOSTREAM *fd = Sopenmem(&str, &size, "w");
  term_t tmp = PL_new_term_ref();
  int rval;
  term_t ex = 0;

  pushOutputContext();
  Scurout = fd;
  rval = callProlog(MODULE_user, goal,
		    PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION, &ex);
  if ( rval )
  { Sflush(fd);
    PL_put_string_nchars(tmp, size, str);
  }
  Sclose(fd);
  if ( str != buf )
    free(str);
  popOutputContext();

  if ( rval )
    return PL_unify(target, tmp);
  if ( ex )
    return PL_raise_exception(ex);
  fail;
}


		/********************************
		*            CONTROL            *
		*********************************/

word
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

word
pl_fail()		/* just to define it */
{ fail;
}

word
pl_true()		/* just to define it */
{ succeed;
}

word
pl_halt(term_t code)
{ int status;

#ifdef O_PLMT
  if ( PL_thread_self() != 1 )
  { term_t t = PL_new_term_ref();

    pl_thread_self(t);
    return PL_error("halt", 1, "Only from thread `main'",
		    ERR_PERMISSION,
		    ATOM_halt, ATOM_thread, t);
  }
#endif

  if ( !PL_get_integer(code, &status) )
    status = 1;

  PL_halt(status);
  /*NOTREACHED*/
  fail;
}

#ifdef O_LIMIT_DEPTH

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The    predicates    below    provide      the     infrastructure    for
call_with_depth_limit/3. This predicate was included on request by Steve
Moyle, for improving the implementation of a theorem prover.

The implementation of call_with_depth_limit/3 in pl-prims.pl is

================================================================
call_with_depth_limit(G, Limit, Result) :-
	$depth_limit(Limit, OLimit, OReached),
	(   catch(G, E, depth_limit_except(OLimit, OReached, E)),
	    $depth_limit_true(Limit, OLimit, OReached, Result, Cut),
	    Cut
	;   $depth_limit_false(OLimit, OReached, Result)
	).
================================================================

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

word
pl_depth_limit(term_t limit, term_t olimit, term_t oreached)
{ long levels;
  long clevel = levelFrame(environment_frame) - 1;

  if ( PL_get_long_ex(limit, &levels) )
  { if ( PL_unify_integer(olimit, depth_limit) &&
	 PL_unify_integer(oreached, depth_reached) )
    { depth_limit   = clevel + levels + 1; /* 1 for the catch/3 */
      depth_reached = clevel;
    
      succeed;
    }
  }

  fail;
}


word
pl_depth_limit_true(term_t limit, term_t olimit, term_t oreached,
		    term_t res, term_t cut, control_t b)
{ switch(ForeignControl(b))
  { case FRG_FIRST_CALL:
    { long l, ol, or;

      if ( PL_get_long_ex(limit, &l) &&
	   PL_get_long_ex(olimit, &ol) &&
	   PL_get_long_ex(oreached, &or) )
      { long clevel = levelFrame(environment_frame) - 1;
	long used = depth_reached - clevel - 1;
	Choice ch;

	depth_limit   = ol;
	depth_reached = or;

	if ( used < 1 )
	  used = 1;
	if ( !PL_unify_integer(res, used) )
	  fail;
    
	for(ch=LD->choicepoints; ch; ch = ch->parent)
	{ switch(ch->type)
	  { case CHP_CATCH:
	    case CHP_DEBUG:
	    case CHP_NONE:
	      continue;
	    default:
	      break;
	  }
	  break;
	}

	if ( ch && ch->frame == environment_frame->parent )
	{ DEBUG(1, Sdprintf("CUT\n"));
	  return PL_unify_atom(cut, ATOM_cut);
	} else
	{ if ( PL_unify_atom(cut, ATOM_true) )
	    ForeignRedoInt(1);
	}
      }

      break;
    }
    case FRG_REDO:
    { long levels;
      long clevel = levelFrame(environment_frame) - 1;

      PL_get_long_ex(limit, &levels);
      depth_limit   = clevel + levels + 1; /* 1 for catch/3 */
      depth_reached = clevel;

      fail;				/* backtrack to goal */
    }
    case FRG_CUTTED:
      succeed;
  }

  fail;
}


static
PRED_IMPL("$depth_limit_false", 3, depth_limit_false, 0)
{ long ol, or;

  if ( PL_get_long_ex(A1, &ol) &&
       PL_get_long_ex(A2, &or) )
  { int exceeded = (depth_reached > depth_limit);

    depth_limit   = ol;
    depth_reached = or;

    if ( exceeded )
      return PL_unify_atom(A3, ATOM_depth_limit_exceeded);
  }

  fail;
}


static
PRED_IMPL("$depth_limit_except", 3, depth_limit_except, 0)
{ long ol, or;

  if ( PL_get_long_ex(A1, &ol) &&
       PL_get_long_ex(A2, &or) )
  { depth_limit   = ol;
    depth_reached = or;

    return PL_raise_exception(A3);
  }

  fail;
}


#endif /*O_LIMIT_DEPTH*/

		/********************************
		*          STATISTICS           *
		*********************************/

#undef LD
#define LD LOCAL_LD			/* must be an argument! */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fetch runtime statistics. There are two standards  here. One is based on
old C-Prolog compatibility, exended as required   by  SWI-Prolog and the
other  is  defined  by  Quintus/SICStus.  The   latter  is  included  if
QP_STATISTICS is defined. The compatibility   is pretty complete, except
the `atoms' key that is defined by both and this ambiguous.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long
heapUsed(void)
{ long heap = GD->statistics.heap;	/* Big allocations */
  
  heap += GD->alloc_pool.allocated;	/* global small allocations */
#ifdef O_PLMT
  heap += threadLocalHeapUsed();	/* thread-local small allocations */
#endif

  return heap;
}

#define QP_STATISTICS 1

#ifdef QP_STATISTICS

static int
#ifdef O_PLMT
qp_statistics__LD(atom_t key, long v[], PL_local_data_t *LD)
#else
qp_statistics__LD(atom_t key, long v[], PL_local_data_t *ld)
#endif
{ int vn;

  if ( key == ATOM_runtime )
  { v[0] = (long)(LD->statistics.user_cputime * 1000.0);
    v[1] = v[0] - LD->statistics.last_cputime;
    LD->statistics.last_cputime = v[0];
    vn = 2;
  } else if ( key == ATOM_system_time )
  { v[0] = (long)(LD->statistics.system_cputime * 1000.0);
    v[1] = v[0] - LD->statistics.last_systime;
    LD->statistics.last_systime = v[0];
    vn = 2;
  } else if ( key == ATOM_real_time )
  { v[0] = (long)WallTime();
    v[1] = v[0] - LD->statistics.last_walltime;
    LD->statistics.last_walltime = v[0];
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
    v[1] = limitStack(global) - v[0];
    vn = 2;
  } else if ( key == ATOM_local_stack )
  { v[0] = usedStack(local);
    v[1] = limitStack(local) - v[0];
    vn = 2;
  } else if ( key == ATOM_trail )
  { v[0] = usedStack(trail);
    v[1] = 0;
    vn = 2;
  } else if ( key == ATOM_program )
  { v[0] = GD->statistics.heap;
    v[1] = 0;
    vn = 2;
  } else if ( key == ATOM_garbage_collection )
  { v[0] = gc_status.collections;
    v[1] = gc_status.trail_gained + gc_status.global_gained;
    v[2] = (long)(gc_status.time * 1000.0);
    vn = 3;
  } else if ( key == ATOM_stack_shifts )
  {
#ifdef O_SHIFT_STACKS
    v[0] = shift_status.global_shifts;
    v[1] = shift_status.local_shifts;
    v[2] = (long)(shift_status.time * 1000.0);
    vn = 3;
#else
    fail;
#endif
  } else if ( key == ATOM_atoms )
  { v[0] = GD->statistics.atoms;
    v[1] = GD->statistics.atomspace;
    v[2] = 0;
    vn = 3;
  } else if ( key == ATOM_atom_garbage_collection )
  {
#ifdef O_ATOMGC
    v[0] = GD->atoms.gc;
    v[1] = GD->statistics.atomspacefreed;
    v[2] = (long)(GD->atoms.gc_time * 1000.0);
    vn = 3;
#else
    vn = 0;				/* no values */
#endif
  } else
    vn = -1;				/* unknown key */

  return vn;
}

#endif /*QP_STATISTICS*/

static int
#ifdef O_PLMT
swi_statistics__LD(atom_t key, Number v, PL_local_data_t *LD)
#else
swi_statistics__LD(atom_t key, Number v, PL_local_data_t *ld)
#endif
{ v->type = V_INTEGER;			/* most of them */

  if      (key == ATOM_cputime)				/* time */
  { v->type = V_REAL;
    v->value.f    = LD->statistics.user_cputime;
  } else if (key == ATOM_inferences)			/* inferences */
    v->value.i = LD->statistics.inferences;
  else if (key == ATOM_local)				/* local stack */
    v->value.i = sizeStack(local);
  else if (key == ATOM_localused)
    v->value.i = usedStack(local);
  else if (key == ATOM_locallimit)
    v->value.i = limitStack(local);
  else if (key == ATOM_heaplimit)			/* heap */
    fail;
  else if (key == ATOM_heap)
    fail;
  else if (key == ATOM_heapused)			/* heap usage */
    v->value.i = heapUsed();
  else if (key == ATOM_trail)				/* trail */
    v->value.i = sizeStack(trail);
  else if (key == ATOM_trailused)	
    v->value.i = usedStack(trail);
  else if (key == ATOM_traillimit)
    v->value.i = limitStack(trail);
  else if (key == ATOM_global)				/* global */
    v->value.i = sizeStack(global);
  else if (key == ATOM_globalused )
    v->value.i = usedStack(global);
  else if (key == ATOM_globallimit)
    v->value.i = limitStack(global);
  else if (key == ATOM_atoms)				/* atoms */
    v->value.i = GD->statistics.atoms;
  else if (key == ATOM_functors)			/* functors */
    v->value.i = GD->statistics.functors;
  else if (key == ATOM_predicates)			/* predicates */
    v->value.i = GD->statistics.predicates;
  else if (key == ATOM_modules)				/* modules */
    v->value.i = GD->statistics.modules;
  else if (key == ATOM_codes)				/* codes */
    v->value.i = GD->statistics.codes;
  else if (key == ATOM_gctime)
  { v->type = V_REAL;
    v->value.f = gc_status.time;
  } else if (key == ATOM_collections)
    v->value.i = gc_status.collections;
  else if (key == ATOM_collected)
    v->value.i = gc_status.trail_gained + gc_status.global_gained;
#ifdef O_ATOMGC
  else if (key == ATOM_agc)
    v->value.i = GD->atoms.gc;
  else if (key == ATOM_agc_gained)
    v->value.i = GD->atoms.collected;
  else if (key == ATOM_agc_time)
  { v->type = V_REAL;
    v->value.f = GD->atoms.gc_time;
  }
#endif
#if O_SHIFT_STACKS
  else if (key == ATOM_global_shifts)
    v->value.i = shift_status.global_shifts;
  else if (key == ATOM_local_shifts)
    v->value.i = shift_status.local_shifts;
  else if (key == ATOM_trail_shifts)
    v->value.i = shift_status.trail_shifts;
#else
  else if ( key == ATOM_global_shifts ||
	    key == ATOM_local_shifts ||
	    key == ATOM_trail_shifts )
    fail;
#endif
#ifdef O_PLMT
  else if ( key == ATOM_threads )
    v->value.i = GD->statistics.threads_created -
      		 GD->statistics.threads_finished;
  else if ( key == ATOM_threads_created )
    v->value.i = GD->statistics.threads_created;
  else if ( key == ATOM_thread_cputime )
  { v->type = V_REAL;
    v->value.f = GD->statistics.thread_cputime;
  }
#endif
  else
    return -1;				/* unknown key */

  succeed;
}


int
pl_statistics_ld(term_t k, term_t value, PL_local_data_t *ld ARG_LD)
{ number result;			/* make compiler happy */
  atom_t key;
  int rc;
#ifdef QP_STATISTICS
  long v[3];
#endif

  if ( !PL_get_atom_ex(k, &key) )
    fail;

  if ( !PL_is_list(value) )
  { switch(swi_statistics__LD(key, &result, ld))
    { case TRUE:
	if ( intNumber(&result) )
	  return PL_unify_integer(value, result.value.i);
	else
	  return PL_unify_float(value, result.value.f);
      case FALSE:
	fail;
      case -1:
	break;
    }
  }

#ifdef QP_STATISTICS
  if ( (rc=qp_statistics__LD(key, v, ld)) >= 0 )
  { long *p;
    term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    for(p = v; rc-- > 0; p++)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_integer(head, *p) )
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
  { if ( k == ATOM_cputime || k == ATOM_runtime )
      LD->statistics.user_cputime = CpuTime(CPU_USER);
    else if ( k == ATOM_system_time )
      LD->statistics.system_cputime = CpuTime(CPU_SYSTEM);
  }

  return pl_statistics_ld(A1, A2, LD PASS_LD);
}



		/********************************
		*            OPTIONS            *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$option/3, provides access to the option  structure from Prolog. This is
halfway a generic structure package ... Anyway, it is better then direct
coded access, as the  indirect  approach   allows  us  to  enumerate the
options and generalise the option processing from the saved-states.

See also pl-main.c, which exploits set_pl_option()  to parse the options
resource  member.  Please  note  this   code   doesn't   use   atoms  as
set_pl_option() is called before the Prolog system is initialised.

This code should be moved into another file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ const char   *name;
  int   	type;
  void	       *address;
} optdef, *OptDef;

#define CMDOPT_LONG   0
#define CMDOPT_STRING 1

static const optdef optdefs[] =
{ { "local",		CMDOPT_LONG,	&GD->options.localSize },
  { "global",		CMDOPT_LONG,	&GD->options.globalSize },
  { "trail",		CMDOPT_LONG,	&GD->options.trailSize },
  { "argument",		CMDOPT_LONG,	&GD->options.argumentSize },
  { "heap",		CMDOPT_LONG,	&GD->options.heapSize },

  { "goal",		CMDOPT_STRING,	&GD->options.goal },
  { "toplevel",		CMDOPT_STRING,	&GD->options.topLevel },
  { "init_file",	CMDOPT_STRING,	&GD->options.initFile },
  { "system_init_file",	CMDOPT_STRING,	&GD->options.systemInitFile },
  { "script_file",	CMDOPT_STRING,	&GD->options.scriptFile },
  { "compileout",	CMDOPT_STRING,	&GD->options.compileOut },
  { "class",		CMDOPT_STRING,  &GD->options.saveclass },
  { "home",		CMDOPT_STRING,	&GD->defaults.home },

  { NULL,		0,		NULL }
};


static
PRED_IMPL("$option", 3, option, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  char *k;

  term_t key = A1;
  term_t old = A2;
  term_t new = A3;

  switch( CTX_CNTRL )
  { int index;

    case FRG_FIRST_CALL:
      if ( PL_is_variable(key) )
      { index = 0;
	
      next:
	for( ; optdefs[index].name; index++ )
	{ switch( optdefs[index].type )
	  { case CMDOPT_LONG:
	    { long *val = optdefs[index].address;

	      if ( !PL_unify_integer(old, *val) )
		continue;
	      break;
	    }
	    case CMDOPT_STRING:
	    { char **val = optdefs[index].address;
	      
	      if ( !PL_unify_atom_chars(old, *val) )
		continue;
	      break;
	    }
	  }
	  PL_unify_atom_chars(key, optdefs[index].name);
	  ForeignRedoInt(index+1);
	}

	fail;
      }
      break;
    case FRG_REDO:
      index = CTX_INT;
      goto next;
    case FRG_CUTTED:
      succeed;
  }

  if ( PL_get_atom_chars(key, &k) )
  { OptDef d = (OptDef)optdefs;

    for( ; d->name; d++ )
    { if ( streq(k, d->name) )
      { switch(d->type)
	{ case CMDOPT_LONG:
	  { long *val = d->address;
	    long newval;

	    if ( !PL_unify_integer(old, *val) ||
		 !PL_get_long(new, &newval) )
	      fail;
	    *val = newval;

	    succeed;
	  }
	  case CMDOPT_STRING:
	  { char **val = d->address;
	    char *newval;

	    if ( !PL_unify_atom_chars(old, *val) ||
		 !PL_get_atom_chars(new, &newval) )
	      fail;

	    if ( !streq(*val, newval) )
	    { remove_string(*val);
	      *val = store_string(newval);
	    }

	    succeed;
	  }
	}
      }
    }
  }

  fail;
}


int
set_pl_option(const char *name, const char *value)
{ OptDef d = (OptDef)optdefs;

  for( ; d->name; d++ )
  { if ( streq(name, d->name) )
    { switch(d->type)
      { case CMDOPT_LONG:
	{ long *val = d->address;
	  number n;
	  unsigned char *q;

	  if ( get_number((unsigned char *)value, &q, &n, FALSE) &&
	       *q == EOS &&
	       intNumber(&n) )
	  { *val = n.value.i;
	    succeed;
	  }
	  fail;
	}
	case CMDOPT_STRING:
	{ char **val = d->address;
	  
	  *val = store_string(value);
	  succeed;
	}
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
    systemMode(n & DOLLAR_STYLE);

    succeed;
  }

  fail;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(prims)
  PRED_DEF("nonvar", 1, nonvar, 0)
  PRED_DEF("var", 1, var, 0)
  PRED_DEF("integer", 1, integer, 0)
  PRED_DEF("float", 1, float, 0)
  PRED_DEF("number", 1, number, 0)
  PRED_DEF("arg", 3, arg, PL_FA_NONDETERMINISTIC)
  PRED_DEF("atomic", 1, atomic, 0)
  PRED_DEF("atom", 1, atom, 0)
  PRED_DEF("string", 1, string, 0)
  PRED_DEF("ground", 1, ground, 0)
  PRED_DEF("acyclic_term", 1, acyclic_term, 0)
  PRED_DEF("cyclic_term", 1, cyclic_term, 0)
  PRED_DEF("compound", 1, compound, 0)
  PRED_DEF("callable", 1, callable, 0)
  PRED_DEF("==", 2, equal, 0)
  PRED_DEF("\\==", 2, nonequal, 0)
  PRED_DEF("compare", 3, compare, 0)
  PRED_DEF("@<", 2, std_lt, 0)
  PRED_DEF("@=<", 2, std_leq, 0)
  PRED_DEF("@>", 2, std_gt, 0)
  PRED_DEF("@>=", 2, std_geq, 0)
  PRED_DEF("=@=", 2, structural_eq, 0)
  PRED_DEF("\\=@=", 2, structural_neq, 0)
  PRED_DEF("?=", 2, can_compare, 0)
  PRED_DEF("functor", 3, functor, 0)
  PRED_DEF("numbervars", 4, numbervars, 0)
  PRED_DEF("term_variables", 2, term_variables2, 0)
  PRED_DEF("term_variables", 3, term_variables3, 0)
  PRED_DEF("unifyable", 3, unifyable, 0)
#ifdef O_HASHTERM
  PRED_DEF("hash_term", 2, hash_term, 0)
#endif
  PRED_DEF("copy_term", 2, copy_term, 0)
  PRED_DEF("duplicate_term", 2, duplicate_term, 0)
#ifdef O_LIMIT_DEPTH
  PRED_DEF("$depth_limit_except", 3, depth_limit_except, 0)
  PRED_DEF("$depth_limit_false",  3, depth_limit_false, 0)
#endif
  PRED_DEF("atom_number", 2, atom_number, 0)
  PRED_DEF("statistics", 2, statistics, 0)
  PRED_DEF("$option", 3, option, PL_FA_NONDETERMINISTIC)
  PRED_DEF("$style_check", 2, style_check, 0)
  PRED_DEF("deterministic", 1, deterministic, 0)
  PRED_DEF("setarg", 3, setarg, 0)
  PRED_DEF("nb_setarg", 3, nb_setarg, 0)
  PRED_DEF("nb_linkarg", 3, nb_linkarg, 0)
EndPredDefs
