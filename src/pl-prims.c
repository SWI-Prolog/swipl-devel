/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: primitive built in
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-ctype.h"
#undef ulong
#define ulong unsigned long

forwards char 	*prependBase(int, char *);


		/********************************
		*         TYPE CHECKING         *
		*********************************/


word
pl_nonvar(term_t k)
{ return PL_is_variable(k) ? FALSE : TRUE;
}

word
pl_var(term_t k)
{ return PL_is_variable(k);
}

word
pl_integer(term_t k)
{ return PL_is_integer(k);
}

word
pl_float(term_t k)
{ return PL_is_float(k);
}

#if O_STRING
word
pl_string(term_t k)
{ return PL_is_string(k);
}
#endif /* O_STRING */

word
pl_number(term_t k)
{ return PL_is_number(k);
}

word
pl_atom(term_t k)
{ return PL_is_atom(k);
}

word
pl_atomic(term_t k)
{ return PL_is_atomic(k);
}

static int
_pl_ground(Word p)
{ int arity;

  deRef(p);

  if (isVar(*p) )
    fail;
  if (!isTerm(*p) )
    succeed;
  arity = arityFunctor(functorTerm(*p));
  for(p = argTermP(*p, 0); arity > 0; arity--, p++)
    TRY( _pl_ground(p) );

  succeed;
}

word
pl_ground(term_t k)
{ return _pl_ground(valTermRef(k));
}


word
pl_compound(term_t k)
{ return PL_is_compound(k);
}

#ifdef O_HASHTERM
		 /*******************************
		 *	    HASH-TERM		*
		 *******************************/

static bool
termHashValue(word term, long *hval)
{ for(;;)
  { switch(tag(term))
    { case TAG_VAR:
	fail;
      case TAG_ATOM:
	*hval = atomValue(term)->hash_value;
        succeed;
      case TAG_STRING:
	*hval = unboundStringHashValue(valString(term));
        succeed;
      case TAG_INTEGER:
	*hval = valInteger(term);
        succeed;
      case TAG_FLOAT:
      { union { real f;
		long l[2];
	      } v;

        v.f = valReal(term);
	*hval = v.l[0] ^ v.l[1];

	succeed;
      }
      case TAG_COMPOUND:
      { functor_t fd = functorTerm(term);
	int arity = arityFunctor(fd);
	Word a, a2;

	*hval = atomValue(nameFunctor(fd))->hash_value + arity;
	for(a = argTermP(term, 0); arity; arity--, a++)
	{ long av;

	  deRef2(a, a2);
	  if ( termHashValue(*a2, &av) )
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


word
pl_hash_term(term_t term, term_t hval)
{ Word p = valTermRef(term);
  long hraw;

  deRef(p);

  if ( termHashValue(*p, &hraw) )
  { hraw = hraw & PLMAXTAGGEDINT;	/* ensure tagged */

    return PL_unify_integer(hval, hraw);
  }

  succeed;
}

#endif /*O_HASHTERM*/


		/********************************
		*           EQUALITY            *
		*********************************/

word
pl_unify(term_t t1, term_t t2)		/* =/2 */
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);
  mark m;
  int rval;

  Mark(m);
  if ( !(rval = unify(p1, p2, environment_frame)) )
    Undo(m);

  return rval;  
}


word
pl_notunify(term_t t1, term_t t2) /* A \= B */
{ Word p1    = valTermRef(t1);
  Word p2    = valTermRef(t2);

  return can_unify(p1, p2) ? FALSE : TRUE;
}


static word
_pl_equal(register Word t1, register Word t2)
{ int arity, n;

  deRef(t1);
  deRef(t2);

  if ( isVar(*t1) )
  { if (t1 == t2)
      succeed;
    fail;
  }

  if (*t1 == *t2)
    succeed;

  if ( isIndirect(*t1) )
  { if ( isIndirect(*t2) )
      return equalIndirect(*t1, *t2);
    fail;
  }

  if (!isTerm(*t1) || !isTerm(*t2) ||
       functorTerm(*t1) != functorTerm(*t2) )
    fail;

  arity = arityFunctor(functorTerm(*t1));
  t1 = argTermP(*t1, 0);
  t2 = argTermP(*t2, 0);
  for(n=0; n<arity; n++, t1++, t2++)
    TRY(_pl_equal(t1, t2) );

  succeed;
}


word
pl_equal(term_t t1, term_t t2) /* == */
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  return _pl_equal(p1, p2);
}


word
pl_nonequal(term_t t1, term_t t2) /* \== */
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  return _pl_equal(p1, p2) ? FALSE : TRUE;
}


		/********************************
		*        STANDARD ORDER         *
		*********************************/

/*  Rules:

    Var @< Number @< Atom @< String < Term
    
    OldVar < NewVar	(not relyable)
    Atom:	alphabetically
    Strings:	alphabetically
    number:	value
    Term:	arity / alphabetically / recursive

 ** Tue Apr 26 16:25:50 1988  jan@swivax.UUCP (Jan Wielemaker)  */

#define LESS -1
#define EQUAL  0
#define GREATER  1

int
compareStandard(Word p1, Word p2)
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
  { if ( !trueFeature(ISO_FEATURE) )
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

    return t1 < t2 ? LESS : GREATER;
  }

  switch(t1)
  { case TAG_VAR:
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
      return strcmp(stringAtom(w1), stringAtom(w2));
    case TAG_STRING:
      return strcmp(valString(w1), valString(w2));
    case TAG_COMPOUND:
    { Functor f1 = (Functor)valPtr(w1);
      Functor f2 = (Functor)valPtr(w2);

      if ( f1->definition != f2->definition )
      { FunctorDef fd1 = valueFunctor(f1->definition);
	FunctorDef fd2 = valueFunctor(f2->definition);

	if ( fd1->arity != fd2->arity )
	  return fd1->arity > fd2->arity ? GREATER : LESS;

	return strcmp(stringAtom(fd1->name),
		      stringAtom(fd2->name));
      } else
      { int arity = arityFunctor(f1->definition);
	int rval;
	
	p1 = f1->arguments;
	p2 = f2->arguments;
	for( ; --arity > 0; p1++, p2++ )
	{ if ((rval = compareStandard(p1, p2)) != EQUAL)
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


word
pl_compare(term_t rel, term_t t1, term_t t2)
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  int val = compareStandard(p1, p2);

  return PL_unify_atom(rel, val < 0 ? ATOM_smaller :
		            val > 0 ? ATOM_larger :
		                      ATOM_equals);
}


word
pl_lessStandard(term_t t1, term_t t2) /* @</2 */
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  return compareStandard(p1, p2) < 0 ? TRUE : FALSE;
}

word
pl_lessEqualStandard(term_t t1, term_t t2) /* @=</2 */
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  return compareStandard(p1, p2) <= 0 ? TRUE : FALSE;
}


word
pl_greaterStandard(term_t t1, term_t t2) /* @>/2 */
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  return compareStandard(p1, p2) > 0 ? TRUE : FALSE;
}


word
pl_greaterEqualStandard(term_t t1, term_t t2)	/* @>=/2 */
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  return compareStandard(p1, p2) >= 0 ? TRUE : FALSE;
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
structeql(Word t1, Word t2, TmpBuffer buf)
{ int todo = 1;
  UChoice nextch = NULL, tailch = NULL;

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
      case TAG_INTEGER:
	if ( storage(w1) == STG_INLINE )
	  fail;
      case TAG_STRING:
      case TAG_FLOAT:
	if ( equalIndirect(w1, w2) )
	  continue;
        fail;
      case TAG_COMPOUND:
      { functor_t fd = functorTerm(w1);
	int arity;

	if ( !hasFunctor(w2, fd) )
	  fail;

	arity = arityFunctor(fd);
	p1 = argTermP(w1, 0);
	p2 = argTermP(w2, 0);
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
      }
    }
  }
}


word
pl_structural_equal(term_t t1, term_t t2)
{ bool rval;
  tmp_buffer buf;
  Reset r;
  Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  deRef(p1);
  deRef(p2);

  if ( *p1 == *p2 )
    succeed;

  initBuffer(&buf);			/* can be faster! */
  rval = structeql(p1, p2, &buf);
  for(r = baseBuffer(&buf, reset); r < topBuffer(&buf, reset); r++)
  { setVar(*r->v1);
    setVar(*r->v2);
  }
  discardBuffer(&buf);

  return rval;
}


word
pl_structural_nonequal(term_t t1, term_t t2)
{ return pl_structural_equal(t1, t2) == FALSE ? TRUE : FALSE;
}


		/********************************
		*         TERM HACKING          *
		*********************************/

word
pl_functor(term_t t, term_t f, term_t a)
{ int arity;
  atom_t name;
  
  if ( PL_get_name_arity(t, &name, &arity) )
  { if ( !PL_unify_atom(f, name) ||
	 !PL_unify_integer(a, arity) )
      fail;

    succeed;
  }
  if ( PL_is_atomic(t) )
  { if ( !PL_unify(f, t) ||
	 !PL_unify_integer(a, 0) )
      fail;

    succeed;
  }

  if ( !PL_get_integer(a, &arity) )
    fail;
  if ( arity == 0 && PL_is_atomic(f) )
    return PL_unify(t, f);
  if ( arity < 0 )
    return PL_warning("functor/3: illegal arity");
  if ( PL_get_atom(f, &name) )
    return PL_unify_functor(t, PL_new_functor(name, arity));

  fail;
}


word
pl_arg(term_t n, term_t term, term_t arg, word b)
{ atom_t name;
  int arity;

  switch( ForeignControl(b) )
  { case FRG_FIRST_CALL:
    { int idx;

      if ( !PL_get_name_arity(term, &name, &arity) )
      { if ( PL_is_variable(term) )
	  return PL_error("arg", 3, NULL, ERR_INSTANTIATION);
	else
	  return PL_error("arg", 3, NULL, ERR_TYPE, ATOM_compound, term);
      }
  
      if ( PL_get_integer(n, &idx) )
      { if ( idx > 0 && idx <= arity )
	{ term_t a = PL_new_term_ref();
	
	  PL_get_arg(idx, term, a);
	  return PL_unify(arg, a);
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
    { int argn = ForeignContextInt(b) + 1;
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
	

word
pl_setarg(term_t n, term_t term, term_t value)
{ int arity, argn;
  atom_t name;
  Word a, v;

  if ( !PL_get_integer(n, &argn) ||
       !PL_get_name_arity(term, &name, &arity) )
    return warning("$setarg/3: instantiation fault");
  
  if ( argn < 1 || argn > arity )
    fail;

  a = valTermRef(term);
  v = valTermRef(value);
  deRef(a);
  deRef(v);

  a = argTermP(*a, argn-1);

#ifdef O_DESTRUCTIVE_ASSIGNMENT
  TrailAssignment(a);
#endif
					/* this is unify(), but the */
					/* assignment must *not* be trailed */
  if ( isVar(*v) )
  { if ( v < a )
    { *a = makeRef(v);
    } else if ( a < v )
    { setVar(*a);
      *v = makeRef(a);
    } else
      setVar(*a);
  } else
    *a = *v;

  succeed;
}


/*  Determine the length of a list. If the list is not proper (or not
    a list at all) -1 is returned.

 ** Mon Apr 18 16:29:01 1988  jan@swivax.UUCP (Jan Wielemaker)  */

int
lengthList(term_t list)
{ GET_LD
#undef LD
#define LD LOCAL_LD
  int length = 0;
  Word l = valTermRef(list);

  deRef(l);

  while(!isNil(*l) )
  { if (!isList(*l) )
      return -1;			/* not a proper list */
    length++;
    l = TailList(l);
    deRef(l);
  }
  if (isNil(*l) )
    return length;

  return -1;
#undef LD
#define LD GLOBAL_LD
}

word
pl_univ(term_t t, term_t list)
{ int arity;
  atom_t name;
  term_t l = PL_new_term_ref();

  arity = lengthList(list) - 1;

  if ( arity >= 0 )			/* 2nd argument is a proper list */
  { term_t head = PL_new_term_ref();
    int n;

    PL_get_list(list, head, l);
    if ( arity == 0 )			/* X =.. [Head] */
      return PL_unify(t, head);

    if ( PL_get_atom(head, &name) )
    { if ( !PL_unify_functor(t, PL_new_functor(name, arity)) )
	fail;

      for(n=1; PL_get_list(l, head, l); n++)
      { if ( !PL_unify_arg(n, t, head) )
	  fail;
      }

      succeed;
    }
  }
  
					/* 1st arg is term or atom */
  if ( PL_get_name_arity(t, &name, &arity) )
  { term_t head = PL_new_term_ref();
    int n;

    if ( !PL_unify_list(list, head, l) ||
	 !PL_unify_atom(head, name) )
      fail;

    for(n = 1; n <= arity; n++)
    { if ( !PL_unify_list(l, head, l) ||
	   !PL_unify_arg(n, t, head) )
	fail;
    }

    return PL_unify_nil(l);
  }

  if ( PL_is_atomic(t) )		/* 3 =.. X, 3.4 =.. X, "foo" =.. X */
  { term_t head = PL_new_term_ref();

    if ( PL_unify_list(list, head, l) &&
	 PL_unify(head, t) &&
	 PL_unify_nil(l) )
      succeed;
  }

  fail;
}


static int
do_number_vars(term_t t, functor_t functor, int n)
{ atom_t name;
  int arity;

start:
  if ( PL_is_variable(t) )
  { term_t tmp = PL_new_term_ref();

    PL_unify_functor(t, functor);
    PL_put_integer(tmp, n);
    PL_unify_arg(1, t, tmp);

    n++;
  } else if ( _PL_get_name_arity(t, &name, &arity) )
  { if ( arity == 1 )
    { PL_get_arg(1, t, t);
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
	  n = do_number_vars(a, functor, n);
	}
      }
    }
  }

  return n;			/* anything else */
}


int
numberVars(term_t t, functor_t functor, int n)
{ term_t h2 = PL_copy_term_ref(t);
  int rval = do_number_vars(h2, functor, n);

  PL_reset_term_refs(h2);

  return rval;
}


word
pl_numbervars(term_t t, term_t f,
	      term_t start, term_t end)
{ int n;
  functor_t functor;
  atom_t name;
  
  if ( !PL_get_integer(start, &n) ||
       !PL_get_atom(f, &name) )
    return warning("numbervars/4: instantiation fault");

  functor = PL_new_functor(name, 1);
  n = numberVars(t, functor, n);

  return PL_unify_integer(end, n);
}


static int
free_variables(Word t, term_t l, int n)
{ deRef(t);

  if ( isVar(*t) )
  { int i;
    term_t v;

    for(i=0; i<n; i++)
    { Word p2 = valTermRef(l+i);	/* see whether we got this one! */

      deRef(p2);
      if ( p2 == t )
	return n;
    }
    v = PL_new_term_ref();
    *valTermRef(v) = makeRef(t);

    return n+1;
  }
  if ( isTerm(*t) )
  { int arity = arityFunctor(functorTerm(*t));

    for(t = argTermP(*t, 0); arity > 0; arity--, t++)
      n = free_variables(t, l, n);
  }
    
  return n;
}


word
pl_free_variables(term_t t, term_t variables)
{ term_t head = PL_new_term_ref();
  term_t vars = PL_copy_term_ref(variables);
  term_t v0   = PL_new_term_refs(0);
  int i, n    = free_variables(valTermRef(t), v0, 0);

  for(i=0; i<n; i++)
  { if ( !PL_unify_list(vars, head, vars) ||
	 !PL_unify(head, v0+i) )
      fail;
  }
      
  return PL_unify_nil(vars);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_e_free_variables(V0^V1^t, vars) is used  by   setof/3  and bagof/3 to
determine  the  free  variables  in  the    goal   that  have  not  been
existentially bound.  The implementation is rather tricky:

A backtract mark is pushed. Then  bind_existential_vars(t) will bind all
variables in terms at the left-side  of   the  ^/2 operator to []. Next,
free_variables() is used to  make  PL_term_refs   for  all  of  the free
variables. The Undo() is used to free all []-bound variables and finally
the list is constructed.  All  this  works   thanks  to  the  fact  that
free_variables() doesn't use unification and its   bindings are thus not
undone by the Undo().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
dobind_vars(Word t, atom_t constant)
{ deRef(t);

  if ( isVar(*t) )
  { *t = constant;
    DoTrail(t);
    return;
  }
  if ( isTerm(*t) )
  { int arity = arityFunctor(functorTerm(*t));

    for(t = argTermP(*t, 0); arity > 0; arity--, t++)
      dobind_vars(t, constant);
  }
}


static Word
bind_existential_vars(Word t)
{ deRef(t);

  if ( isTerm(*t) )
  { Functor f = valueTerm(*t);
    int arity;
    Word a;

    if ( f->definition == FUNCTOR_hat2 )
    { dobind_vars(&f->arguments[0], ATOM_nil);
      return bind_existential_vars(&f->arguments[1]);
    }
    
    arity = arityFunctor(f->definition);
    for(a = f->arguments; arity > 0; arity--, a++)
      bind_existential_vars(a);
  }

  return t;
}


word
pl_e_free_variables(term_t t, term_t vars)
{ mark m;

  Mark(m);
  { Word t2          = bind_existential_vars(valTermRef(t));
    term_t v0   = PL_new_term_refs(0);
    int i, n	     = free_variables(t2, v0, 0);
    Undo(m);

    if ( PL_unify_functor(vars, PL_new_functor(ATOM_v, n)) )
    { for(i=0; i<n; i++)
      { TRY(PL_unify_arg(i+1, vars, v0+i));
      }

      succeed;
    }

    fail;
  }  
}
  

		 /*******************************
		 *	      COPY-TERM		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Term copying is used to create a  term with `fresh' variables. The ideal
algorithm should take care of sharing  variables   in  the term and copy
ground parts of the term by  sharing   them  with the original term. The
implementation below satisfies these requirements,  passes the term only
twice, is safe to stack-shifting and garbage-collection while in progres
and is efficient for both large and small terms. Here is how it works.

Phase *1* analyses the term. It will   make  a foreign term-reference to
any variable found in the term. It  will add a foreign-reference holding
the index-number of a ground term encountered. While numbering the tree,
only compound terms are numbered and a ground term counts as one.

Next, the array of foreign references   is  sorted. Variables are placed
first, ordered on their address and  ground-term indices after them. The
variable array is then  scanned  and   for  each  shared  variable (i.e.
reference to the same address), two  term-references are made. The first
points to the old term's shared variable and   the  other is set to NULL
(var). This field will be  used  to   store  a  reference  to the copied
variable.

Finally, the term is copied. If a variable  is found, it is looked up in
the shared variable database.  When  present   and  already  copied, the
reference is copied. When present, but not   copied, a reference is made
from  the  free  cell  to  the  copy.  Otherwise  no  action  is  needed
(singleton). If a term is found and it  is in the ground-list, just copy
the term-reference, otherwise, recurse into the term.  Finally, copy all
other (atomic) data by reference.

NOTE: the variable detection could be  more efficient by introducing two
special constants. Finding a variable,  assign   the  first, finding the
first, assign the second and make a  reference in the variable array. To
be considered.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
pre_copy_analysis(Word t, int *index)
{ deRef(t);

  if ( isVar(*t) )
  { term_t h = PL_new_term_ref();
    Word p = valTermRef(h);

    *p = makeRef(t);
    return 1;				/* 1 variable */
  }

  if ( isTerm(*t) )
  { int arity = arityFunctor(functorTerm(*t));
    int subvars = 0;
    int thisindex = (*index)++;
    term_t thisterm = PL_new_term_refs(0);
    
    t = argTermP(*t, 0);
    for( ; --arity >= 0; t++ )
      subvars += pre_copy_analysis(t, index);
    
    if ( subvars == 0 )			/* ground term */
    { term_t h;

      PL_reset_term_refs(thisterm);
      h = PL_new_term_ref();
      PL_put_integer(h, thisindex);
      *index = thisindex+1;		/* don't number in ground! */
    }

    return subvars;
  }
    
  return 0;
}


static int
cmp_copy_refs(const void *h1, const void *h2)
{ word w1 = *((Word) h1);
  word w2 = *((Word) h2);

  if ( isRef(w1) )
  { if ( isRef(w2) )
      return unRef(w1) - unRef(w2);
    return -1;
  }
  if ( isRef(w2) )
    return 1;

  return valInt(w1) - valInt(w2);
}


typedef struct
{ term_t shared_variables;		/* handle of first shared var */
  int    nshared;			/* # shared variables */
  term_t ground_terms;			/* index of first ground term */
  int	 nground;			/* # ground terms */
  int    index;				/* index of current compound */
} copy_info, *CopyInfo;


static Word
lookup_shared_var(CopyInfo info, Word v)
{ if ( info->nshared )
  { Word v0 = valTermRef(info->shared_variables);
    int n;

    for(n = info->nshared; n > 0; n--, v0 += 2)
    { if ( unRef(*v0) == v )
	return v0;
    }
  }

  return NULL;
}


static int
lookup_ground(CopyInfo info)
{ if ( info->nground )
  { Word g0 = valTermRef(info->ground_terms);

    if ( valInt(*g0) == info->index )
    { info->nground--;
      info->ground_terms++;
      succeed;
    }
  }

  fail;
}


static void
do_copy(term_t from, term_t to, CopyInfo info)
{ Word p = valTermRef(from);

  deRef(p);
  if ( isVar(*p) )
  { Word p2 = lookup_shared_var(info, p);

    if ( p2 )
    { Word t = valTermRef(to);

      deRef(t);
      if ( p2[1] )
	*t = p2[1];
      else
      { setVar(*t);
	p2[1] = makeRef(t);
      }
    }
  } else if ( isTerm(*p) )
  { if ( lookup_ground(info) )
    { info->index++;
      PL_unify(to, from);
    } else
    { functor_t fd = functorTerm(*p);
      int n, arity = arityFunctor(fd);
      term_t af = PL_new_term_ref();
      term_t at = PL_new_term_ref();

      info->index++;
      PL_unify_functor(to, fd);
      for(n=0; n<arity; n++)
      { PL_get_arg(n+1, from, af);
	PL_get_arg(n+1, to, at);
	do_copy(af, at, info);
      }
    }
  } else
    PL_unify(to, from);
}


word
pl_copy_term(term_t from, term_t to)
{ Word f = valTermRef(from);
  term_t copy = PL_new_term_ref();
  term_t ha = copy+1;			/* next free one */
  int hn;
  Word p, q;
  copy_info info;
  int n, index = 1;

  pre_copy_analysis(f, &index);
  hn = PL_new_term_refs(0) - ha;
  info.shared_variables = ha;
  info.index = 1;
  if ( hn > 0 )
  { q = p = valTermRef(ha);

    qsort(p, hn, sizeof(word), cmp_copy_refs);
    for( n = hn; n > 0; n--)
    { if ( isRef(*p) )
      { Word v = unRef(*p);
	int shared = 1;

	while(n > 1 && isRef(p[shared]) && unRef(p[shared]) == v )
	{ shared++;
	  n--;
	}

	if ( shared > 1 )
	{ *q++ = *p;
	  *q++ = 0;			/* reserved for new one */
	}
	p += shared;
      } else				/* hit ground terms */
      { info.nshared = (q-valTermRef(ha))/2;
	info.nground = n;
	info.ground_terms = consTermRef(q);

	while(n-- > 0)
	  *q++ = *p++;
	goto end_analysis;
      }
    }
    info.nshared = (q-valTermRef(ha))/2;
    info.nground = 0;
  } else
  { info.nshared = 0;
    info.nground = 0;
  }
end_analysis:

  DEBUG(5, Sdprintf("%d shared variables and %d ground terms:\n",
		    info.nshared, info.nground);
	for(n=0; n<info.nground; n++)
	{ Sdprintf("\t");
	  pl_write(info.ground_terms+n);
	  Sdprintf("\n");
	});

  do_copy(from, copy, &info);

  return PL_unify(to, copy);
}


word
pl_atom_length(term_t w, term_t n)
{ char *s;

  if ( PL_get_chars(w, &s, CVT_ALL) )
    return PL_unify_integer(n, strlen(s));

  return warning("atom_length/2: instantiation fault");
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


word
pl_format_number(term_t format, term_t number, term_t string)
{ char *fmt;
  int arg;
  char conv;

  if ( !PL_get_chars(format, &fmt, CVT_ALL) )
    return warning("$format_number/2: instantiation fault");
  if ( *fmt == EOS )
    return warning("$format_number/3: illegal format");
  arg = atoi(fmt);
  conv = fmt[strlen(fmt)-1];

  switch(conv)
  { case 'D':
    case 'd':
    case 'r':
    case 'R':
      { long i;
	char result[50];

	if ( !PL_get_long(number, &i) )
	  return warning("format_number/3: 2nd argument is not an integer");
	if (conv == 'd' || conv == 'D')
	  formatInteger(conv == 'D', arg, 10, TRUE, i, result);
	else
	  formatInteger(FALSE, 0, arg, conv == 'r', i, result);

	return PL_unify_list_codes(string, result);
      }
    case 'e':
    case 'E':
    case 'f':
    case 'g':
    case 'G':
      { double f;
	char tmp[100];
	char form2[10];

	if ( fmt[1] == EOS )
	  arg = 6;
	if ( !PL_get_float(number, &f) )
	  return warning("$format_number/3: 2nd argument is not a float");
	Ssprintf(form2, "%%.%d%c", arg, conv);
	Ssprintf(tmp, form2, f);

	return PL_unify_list_codes(string, tmp);
      }
    default:
      return warning("$format_number/3: illegal conversion code");
  }
}


#define X_AUTO   0x00
#define X_ATOM   0x01
#define X_NUMBER 0x02
#define X_MASK   0x0f
#define X_CHARS  0x10

static word
x_chars(const char *pred, term_t atom, term_t string, int how)
{ char *s;

  if ( PL_get_chars(atom, &s, CVT_ATOMIC) ) /* atomic --> "list" */
  { if ( how & X_CHARS )
      return PL_unify_list_chars(string, s);
    else
      return PL_unify_list_codes(string, s);
  }

  if ( PL_is_variable(atom) )
  { if ( !PL_get_list_chars(string, &s, 0) )
    { if ( !PL_is_list(string) )
	return PL_error(pred, 2, NULL,
			ERR_TYPE, ATOM_list, string);
      else
	return PL_error(pred, 2, NULL,
			ERR_REPRESENTATION,
			lookupAtom("character_code"));
    }

    how &= X_MASK;

    switch(how)
    { case X_ATOM:
	return PL_unify_atom_chars(atom, s);
      case X_AUTO:
      case X_NUMBER:
      default:
      { number n;
	unsigned char *q;

	if ( get_number((unsigned char *)s, &q, &n) && *q == EOS )
	{ if ( intNumber(&n) )
	    return PL_unify_integer(atom, n.value.i);
	  else
	    return PL_unify_float(atom, n.value.f);
	}
	if ( how == X_AUTO )
	  return PL_unify_atom_chars(atom, s);
	else
	  return PL_error(pred, 2, NULL,
			  ERR_REPRESENTATION,
			  lookupAtom("number"));
      }
    }
  }

  return PL_error(pred, 2, NULL, ERR_TYPE, ATOM_atom, atom);
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
  { if ( n >= 0 && n < 256 )
      return PL_unify_atom(atom, codeToAtom(n));

    return PL_error("char_code", 2, NULL,
		    ERR_REPRESENTATION,
		    lookupAtom("character_code"));
  }

  return PL_error("char_code", 2, NULL, ERR_INSTANTIATION);
}


static bool
isPrefix(char *s, char *q)		/* s is prefix of q */
{ while(*s && *s == *q)
    s++, q++;

  return *s == EOS;
}


word
pl_atom_prefix(term_t atom, term_t prefix)
{ char *a, *p;

  PL_get_chars(atom,   &a, CVT_ATOMIC|BUF_RING);
  PL_get_chars(prefix, &p, CVT_ATOMIC|BUF_RING);

  if ( a && p )
    return isPrefix(p, a) ? TRUE : FALSE;

  return warning("atom_prefix/2: instantiation fault");
}


static word
concat(const char *pred,
       term_t a1, term_t a2, term_t a3, 
       word ctx,
       int (*out)(term_t, const char *))
{ char *s1 = NULL, *s2 = NULL, *s3 = NULL;
  long l1, l2, l3;
  char *tmp;

  PL_get_chars(a1, &s1, CVT_ATOMIC|BUF_RING);
  PL_get_chars(a2, &s2, CVT_ATOMIC|BUF_RING);
  PL_get_chars(a3, &s3, CVT_ATOMIC|BUF_RING);

  if ( !s1 && !PL_is_variable(a1) )
    return PL_error(pred, 3, NULL, ERR_INSTANTIATION, ATOM_atomic, a1);
  if ( !s2 && !PL_is_variable(a2) )
    return PL_error(pred, 3, NULL, ERR_INSTANTIATION, ATOM_atomic, a1);
  if ( !s3 && !PL_is_variable(a3) )
  { err3:
    return PL_error(pred, 3, NULL, ERR_INSTANTIATION, ATOM_atomic, a1);
  }

  if (s1 && s2)
  { l1 = strlen(s1);
    l2 = strlen(s2);
    tmp = alloca(l1 + l2 + 1);
    strcpy(tmp, s1);
    strcpy(tmp+l1, s2);
    return (*out)(a3, tmp);
  }

  if ( !s3 ) 
    goto err3;

  if ( s1 )
  { if (isPrefix(s1, s3) )
      return (*out)(a2, s3+strlen(s1));
    fail;
  } else if ( s2 )
  { int ld;				/* fixed 13/09/93 for: */
    char *q;				/* concat(X, ' ', 'xxx  ') */

    l2 = strlen(s2);
    l3 = strlen(s3);
    ld = l3 - l2;
    if (l2 > l3 || !streq(s3+ld, s2) )
      fail;
    q = alloca(ld+1);
    strncpy(q, s3, ld);
    q[ld] = EOS;
    return (*out)(a1, q);
  } else				/* -, -, + */
  { int at_n;
    mark m;

    switch ( ForeignControl(ctx) )
    { case FRG_FIRST_CALL:
        if ( !s3[0] )
	  fail;				/* empty string */
	at_n = 0;
        break;
      case FRG_REDO:
	at_n = ForeignContextInt(ctx);
        break;
      default:
	succeed;
    }

    Mark(m);
    for(; s3[at_n]; at_n++)
    { if ( (*out)(a2, s3+at_n) )
      { char *q = alloca(at_n+1);
	strncpy(q, s3, at_n);
	q[at_n] = EOS;
	if ( (*out)(a1, q) )
	{ ForeignRedoInt(at_n+1);
	} 
      }

      Undo(m);
    }
    if ( (*out)(a1, s3) && (*out)(a2, "") )
      succeed;
    fail;
  }    
}


word
pl_atom_concat(term_t a1, term_t a2, term_t a3, control_t ctx)
{ return concat("atom_concat", a1, a2, a3, ctx, PL_unify_atom_chars);
}


word
pl_concat_atom3(term_t list, term_t sep, term_t atom)
{ char *s;
  term_t l = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  int first = TRUE;
  char *sp;
  int splen;
  tmp_buffer b;
  
  if ( sep )
  { if ( !PL_get_chars(sep, &sp, CVT_ATOMIC|BUF_RING) )
      return warning("concat_atom/3: illegal separator");
    splen = strlen(sp);
  } else
  { sp = NULL;
    splen = 0;
  }

  initBuffer(&b);
  while( PL_get_list(l, head, l) &&
	 PL_get_chars(head, &s, CVT_ATOMIC) )
  { if ( first )
      first = FALSE;
    else if ( splen )
      addMultipleBuffer(&b, sp, splen, char);

    addMultipleBuffer(&b, s, strlen(s), char);
  }

  if ( PL_get_nil(l) )
  { atom_t a;

    addBuffer(&b, EOS, char);
    a = lookupAtom(baseBuffer(&b, char));
    discardBuffer(&b);

    return PL_unify_atom(atom, a);
  }

  discardBuffer(&b);
  return warning("concat_atom/2: instantiation fault");
}


word
pl_concat_atom(term_t list, term_t atom)
{ return pl_concat_atom3(list, 0, atom);
}


word
pl_apropos_match(term_t a1, term_t a2)
{ char *s1, *s2;

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
  
  return warning("$apropos_match/2: instantiation fault");
}


#if O_STRING
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provisional String manipulation functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_string_length(term_t str, term_t l)
{ char *s;
  int i;

  if ( PL_get_string(str, &s, &i) )
    return PL_unify_integer(l, i);
  if ( PL_get_chars(str, &s, CVT_ALL) )
    return PL_unify_integer(l, strlen(s));

  return warning("string_length/2: instantiation fault");
}


word
pl_string_concat(term_t a1, term_t a2, term_t a3, control_t h)
{ return concat("string_concat", a1, a2, a3, h, PL_unify_string_chars);
}


word
pl_string_to_atom(term_t str, term_t a)
{ char *s;

  if ( PL_get_chars(str, &s, CVT_ALL) )
    return PL_unify_atom_chars(a, s);
  if ( PL_get_chars(a, &s, CVT_ALL) )
    return PL_unify_string_chars(str, s);

  return warning("string_to_atom/2: instantiation fault");
}


word
pl_string_to_list(term_t str, term_t list)
{ char *s;

  if ( PL_get_chars(str, &s, CVT_ALL) )
    return PL_unify_list_codes(list, s);
  if ( PL_get_list_chars(list, &s, 0) )	/* string_to_list(S, []). */
    return PL_unify_string_chars(str, s);
  if ( PL_get_chars(list, &s, CVT_ALL) )
    return PL_unify_string_chars(str, s);

  return warning("string_to_list/2 instantiation fault");
}


word
pl_substring(term_t str, term_t offset,
	     term_t length, term_t sub)
{ long off, l, size, end;
  char *s, c;
  word ss;

  if ( !PL_get_chars(str, &s, CVT_ALL) ||
       !PL_get_long(offset, &off) ||
       !PL_get_long(length, &l) )
    return warning("substring/4: instantiation fault");

  size = strlen(s);
  end = off + l - 1;
  if ( off < 1 || off > size || l < 0 || end > size )
    return warning("substring/4: index out of range");

  c = s[end];
  s[end] = EOS;
  ss = globalString(&s[off-1]);
  s[end] = c;

  return _PL_unify_atomic(sub, ss);
}
#endif /* O_STRING */


word
pl_write_on_string(term_t goal, term_t target)
{ char buf[1024];
  int bufsize = sizeof(buf);
  char *string = buf;
  bool rval;

  tellString(&string, &bufsize);
  rval = callProlog(MODULE_user, goal, PL_Q_NODEBUG, NULL);
  toldString();

  if ( rval )
#if O_STRING
    rval = PL_unify_string_chars(target, string);
#else
    rval = PL_unify_list_codes(target, string);
#endif

  if ( string != buf )
    free(string);

  return rval;
}


		/********************************
		*            CONTROL            *
		*********************************/

word
pl_repeat(word h)
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

  if ( !PL_get_integer(code, &status) )
    status = 1;

  Halt(status);
  /*NOTREACHED*/
  fail;
}

#ifdef O_LIMIT_DEPTH

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The    predicates    below    provide      the     infrastructure    for
call_with_depth_limit/3. This predicate was included  on request by ...,
for improving the implementation of a theorem prover.

The implementation of call_with_depth_limit/3 in pl-prims.pl is

================================================================
call_with_depth_limit(G, Limit, Result) :-
	'$depth_limit'(Limit, OLimit, OReached),
	(   G,
	    '$depth_limit_true'(Limit, OLimit, OReached, Result, Cut),
	    Cut
	;   '$depth_limit_false'(Limit, OLimit, OReached, Result)
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

  if ( PL_get_long(limit, &levels) )
  { if ( PL_unify_integer(olimit, depth_limit) &&
	 PL_unify_integer(oreached, depth_reached) )
    { depth_limit   = clevel + levels;
      depth_reached = 0;
    
      succeed;
    }

    fail;
  }

  return PL_warning("call_with_depth_limit/3: instantiation fault");
}


word
pl_depth_limit_true(term_t limit, term_t olimit, term_t oreached,
		    term_t res, term_t cut, word b)
{ switch(ForeignControl(b))
  { case FRG_FIRST_CALL:
    { long l, ol, or;

      if ( PL_get_long(limit, &l) &&
	   PL_get_long(olimit, &ol) &&
	   PL_get_long(oreached, &or) )
      { long clevel = levelFrame(environment_frame) - 1;
	long used = depth_reached - clevel;

	depth_limit   = olimit;
	depth_reached = oreached;

	if ( used < 1 )
	  used = 1;
	if ( !PL_unify_integer(res, used) )
	  fail;
    
	if ( environment_frame->backtrackFrame->predicate ==
	     PROCEDURE_alt0->definition &&
	     environment_frame->parent ==
	     environment_frame->backtrackFrame->parent )
	{ return PL_unify_atom(cut, ATOM_cut);
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

      PL_get_long(limit, &levels);
      depth_limit   = clevel + levels;
      depth_reached = 0;

      fail;				/* backtrack to goal */
    }
  }

  fail;
}



word
pl_depth_limit_false(term_t limit, term_t olimit, term_t oreached, term_t res)
{ long l, ol, or;

  if ( PL_get_long(limit, &l) &&
       PL_get_long(olimit, &ol) &&
       PL_get_long(oreached, &or) )
  { unsigned long clevel = (unsigned long)levelFrame(environment_frame)	- 1;
    int exceeded = (depth_reached > clevel + l);

    depth_limit   = olimit;
    depth_reached = oreached;

    if ( exceeded )
      return PL_unify_atom(res, ATOM_depth_limit_exceeded);
  }

  fail;
}

#endif /*O_LIMIT_DEPTH*/

		/********************************
		*          STATISTICS           *
		*********************************/

/*  Return various runtime statistics.

 ** Sun Apr 17 15:38:46 1988  jan@swivax.UUCP (Jan Wielemaker)  */

#define makeNum(n)	((n) < PLMAXTAGGEDINT ? consInt(n) : globalLong(n))
#define limitStack(n)	diffPointers(LD->stacks.n.limit, LD->stacks.n.base)

word
pl_statistics(term_t k, term_t value)
{ word result;
  atom_t key;

  if ( !PL_get_atom(k, &key) )
    return PL_error("statistics", 2, NULL, ERR_TYPE, ATOM_atom, k);

  if      (key == ATOM_cputime)				/* time */
    result = globalReal(CpuTime());
  else if (key == ATOM_inferences)			/* inferences */
    result = makeNum(LD->statistics.inferences);
  else if (key == ATOM_local)				/* local stack */
    result = makeNum((long)lMax - (long)lBase);
  else if (key == ATOM_localused)
    result = makeNum((long)lTop - (long)lBase);
  else if (key == ATOM_locallimit)
    result = makeNum(limitStack(local));
  else if (key == ATOM_heaplimit)			/* heap */
    fail;
  else if (key == ATOM_heap)
    fail;
  else if (key == ATOM_heapused)			/* heap usage */
    result = makeNum(GD->statistics.heap);
  else if (key == ATOM_trail)				/* trail */
    result = makeNum((long)tMax - (long)tBase);
  else if (key == ATOM_trailused)
    result = makeNum((long)tTop - (long)tBase);
  else if (key == ATOM_traillimit)
    result = makeNum(limitStack(trail));
  else if (key == ATOM_global)				/* global */
    result = makeNum((long)gMax - (long)gBase);
  else if (key == ATOM_globalused)
    result = makeNum((long)gTop - (long)gBase);
  else if (key == ATOM_globallimit)
    result = makeNum(limitStack(global));
  else if (key == ATOM_atoms)				/* atoms */
    result = consInt(GD->statistics.atoms);
  else if (key == ATOM_functors)			/* functors */
    result = consInt(GD->statistics.functors);
  else if (key == ATOM_predicates)			/* predicates */
    result = consInt(GD->statistics.predicates);
  else if (key == ATOM_modules)				/* modules */
    result = consInt(GD->statistics.modules);
  else if (key == ATOM_codes)				/* codes */
    result = consInt(GD->statistics.codes);
  else if (key == ATOM_gctime)
    result = globalReal(gc_status.time);
  else if (key == ATOM_collections)
    result = consInt(gc_status.collections);
  else if (key == ATOM_collected)
    result = makeNum(gc_status.trail_gained + gc_status.global_gained);
  else if (key == ATOM_core_left)			/* core left */
#if tos
    result = consInt((long)coreleft());
#else
    fail;
#endif
#if O_SHIFT_STACKS
  else if (key == ATOM_global_shifts)
    result = consInt(shift_status.global_shifts);
  else if (key == ATOM_local_shifts)
    result = consInt(shift_status.local_shifts);
  else if (key == ATOM_trail_shifts)
    result = consInt(shift_status.trail_shifts);
#else
  else if ( key == ATOM_global_shifts ||
	    key == ATOM_local_shifts ||
	    key == ATOM_trail_shifts )
    fail;
#endif
#ifdef O_PLMT
  else if ( key == ATOM_threads )
    result = consInt(GD->statistics.threads_created -
		     GD->statistics.threads_finished);
  else if ( key == ATOM_threads_created )
    result = consInt(GD->statistics.threads_created);
  else if ( key == ATOM_thread_cputime )
    result = globalReal(GD->statistics.thread_cputime);
#endif
  else
    return PL_error("statistics", 2, NULL, ERR_DOMAIN,
		    PL_new_atom("statistics_key"), k);

  return _PL_unify_atomic(value, result);
}


		/********************************
		*            OPTIONS            *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_option() realises $option/3, providing access to the option structure
from Prolog. This is halfway a generic  structure package ... Anyway, it
is better then direct coded access, as   the indirect approach allows us
to enumerate the options and generalise   the option processing from the
saved-states.

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
  { "compileout",	CMDOPT_STRING,	&GD->options.compileOut },
  { "class",		CMDOPT_STRING,  &GD->options.saveclass },

  { NULL,		0,		NULL }
};

word
pl_option(term_t key, term_t old, term_t new, control_t h)
{ char *k;

  switch( ForeignControl(h) )
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
      index = ForeignContextInt(h);
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
	    *val = newval;

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

	  if ( get_number((unsigned char *)value, &q, &n) &&
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

word
pl_style_check(term_t old, term_t new)
{ int n;

  if ( PL_unify_integer(old, debugstatus.styleCheck) &&
       PL_get_integer(new, &n) )
  { debugstatus.styleCheck = n;
    systemMode(n & DOLLAR_STYLE);

    succeed;
  }

  fail;
}
