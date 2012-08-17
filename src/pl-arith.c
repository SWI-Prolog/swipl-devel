/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The arithmetic module defines a small set of logical integer  predicates
as   well   as  the  evaluation  of  arbitrary  arithmetic  expressions.
Arithmetic can be interpreted or compiled (see  -O  flag).   Interpreted
arithmetic  is  supported  by  the  built-in  predicates is/2, >/2, etc.
These functions call valueExpression() to evaluate a Prolog term holding
an arithmetic expression.

For compiled arithmetic, the compiler generates WAM codes that execute a
stack machine.  This module maintains an array of arithmetic  functions.
These  functions are addressed by the WAM instructions using their index
in this array.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#undef abs			/* avoid abs() problem with MSVC++ */
#include <math.h>
#undef abs
#define abs(a) ((a) < 0 ? -(a) : (a))
#include <limits.h>
#ifdef HAVE_FLOAT_H
#include <float.h>
#ifdef _MSC_VER
#define isnan(x) _isnan(x)
#define copysign(x,y) _copysign(x,y)
#endif
#endif
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

#ifdef fpclassify
#define HAVE_FPCLASSIFY 1
#endif

#ifdef __WINDOWS__
#include <wincrypt.h>
#endif

#undef LD
#define LD LOCAL_LD

#ifndef M_PI
#define M_PI (3.14159265358979323846)
#endif
#ifndef M_E
#define M_E (2.7182818284590452354)
#endif

#ifdef _MSC_VER
#define LL(x) x ## i64
#else
#define LL(x) x ## LL
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On some machines, notably  FreeBSD  upto   version  3.x,  floating point
operations raise signals rather then leaving an error condition and this
behaviour can be changed to be   IEEE754  using fpsetmask() and friends.
Here  we  test  whether  this  interface  is   present  and  set  it  up
accordingly.

With many thanks to NIDE  Naoyuki  for   the  clear  explanation  of the
problem.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(HAVE_FLOATINGPOINT_H) && defined(HAVE_FPSETMASK) && defined(HAVE_FPRESETSTICKY)
#define O_INHIBIT_FP_SIGNALS
#include <floatingpoint.h>
#ifndef FP_X_DZ
#define FP_X_DZ 0
#endif
#ifndef FP_X_INV
#define FP_X_INV 0
#endif
#ifndef FP_X_OFL
#define FP_X_OFL 0
#endif
#endif

static int		getCharExpression(Word p, Number r ARG_LD);
static int		ar_minus(Number n1, Number n2, Number r);


		/********************************
		*   LOGICAL INTEGER FUNCTIONS   *
		*********************************/

static inline void
clearInteger(Number n)
{
#ifdef O_GMP
  if ( n->type == V_MPZ && n->value.mpz->_mp_alloc )
    mpz_clear(n->value.mpz);
#endif
}


typedef struct between_state
{ number low;
  number high;
  int hinf;
} between_state;


static
PRED_IMPL("between", 3, between, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  between_state *state;
  term_t low = A1;
  term_t high = A2;
  term_t n = A3;
  int rc = TRUE;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
      { number l, h, i;
	int hinf = FALSE;

	if ( !PL_get_number(low, &l) || !intNumber(&l) )
	  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, low);
	if ( !PL_get_number(high, &h) || !intNumber(&h) )
	{ if ( PL_is_inf(high) )
	  { h.type = V_INTEGER;		/* make clearInteger() safe */
	    hinf = TRUE;
	  } else
	  { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, high);
	  }
	}

					/* between(+,+,+) */
	if ( PL_get_number(n, &i) && intNumber(&i) )
	{ int rc;

	  if ( hinf )
	  { rc = cmpNumbers(&i, &l) >= 0;
	  } else
	  { rc = cmpNumbers(&i, &l) >= 0 && cmpNumbers(&i, &h) <= 0;
	  }

	  clearInteger(&l);
	  clearInteger(&i);
	  if ( !hinf )
	    clearInteger(&h);

	  return rc;
	}

					/* between(+,+,-) */
	if ( !PL_is_variable(n) )
	  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, n);
	if ( hinf == FALSE && cmpNumbers(&h, &l) < 0 )
	{ clearInteger(&l);
	  clearInteger(&h);
	  fail;
	}

	if ( !PL_unify(n, low) )
	  fail;
	if ( hinf == FALSE && cmpNumbers(&l, &h) == 0 )
	{ clearInteger(&l);
	  clearInteger(&h);
	  succeed;
	}

	state = allocForeignState(sizeof(*state));
	cpNumber(&state->low, &l);
	cpNumber(&state->high, &h);
	state->hinf = hinf;
	clearInteger(&l);
	clearInteger(&h);
	ForeignRedoPtr(state);
	/*NOTREACHED*/
      }
    case FRG_REDO:
      { state = CTX_PTR;

	ar_add_ui(&state->low, 1);
	if ( !PL_unify_number(n, &state->low) )
	{ rc = FALSE;
	  goto cleanup;
	}
	if ( !state->hinf &&
	     cmpNumbers(&state->low, &state->high) == 0 )
	  goto cleanup;
	ForeignRedoPtr(state);
	/*NOTREACHED*/
      }
    case FRG_CUTTED:
      { state = CTX_PTR;
      cleanup:
	clearInteger(&state->low);
	clearInteger(&state->high);
	freeForeignState(state, sizeof(*state));
	/*FALLTHROUGH*/
      }
    default:;
      return rc;
  }
}

static
PRED_IMPL("succ", 2, succ, 0)
{ GET_LD
  Word p1, p2;
  number i1, i2, one;
  int rc;

  p1 = valTermRef(A1); deRef(p1);

  one.type = V_INTEGER;
  one.value.i = 1;

  if ( isInteger(*p1) )
  { get_integer(*p1, &i1);
    if ( ar_sign_i(&i1) < 0 )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		      ATOM_not_less_than_zero, A1);
    pl_ar_add(&i1, &one, &i2);
    rc = PL_unify_number(A2, &i2);
  } else if ( !canBind(*p1) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, A1);

  p2 = valTermRef(A2); deRef(p2);

  if ( isInteger(*p2) )
  { get_integer(*p2, &i2);
    switch( ar_sign_i(&i2) )
    { case 1:
	ar_minus(&i2, &one, &i1);
        rc = PL_unify_number(A1, &i1);
	break;
      case 0:
	fail;
      case -1:
      default:
	return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			ATOM_not_less_than_zero, A2);
    }
  } else if ( !canBind(*p2) )
  { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, A2);
  } else
    return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);

  clearInteger(&i1);
  clearInteger(&i2);
  clearInteger(&one);

  return rc;
}


static int
var_or_integer(term_t t, number *n, int which, int *mask ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isInteger(*p) )
  { get_integer(*p, n);
    *mask |= which;
    succeed;
  }
  if ( isVar(*p) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


static
PRED_IMPL("plus", 3, plus, 0)
{ GET_LD
  number m, n, o;
  int mask = 0;
  int rc;

  if ( !var_or_integer(A1, &m, 0x1, &mask PASS_LD) ||
       !var_or_integer(A2, &n, 0x2, &mask PASS_LD) ||
       !var_or_integer(A3, &o, 0x4, &mask PASS_LD) )
    fail;

  switch(mask)
  { case 0x7:				/* +, +, + */
    case 0x3:				/* +, +, - */
      pl_ar_add(&m, &n, &o);
      rc = PL_unify_number(A3, &o);
      break;
    case 0x5:				/* +, -, + */
      ar_minus(&o, &m, &n);
      rc = PL_unify_number(A2, &n);
      break;
    case 0x6:				/* -, +, + */
      ar_minus(&o, &n, &m);
      rc = PL_unify_number(A1, &m);
      break;
    default:
      return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
  }

  clearInteger(&m);
  clearInteger(&n);
  clearInteger(&o);

  return rc;
}


		/********************************
		*           COMPARISON          *
		*********************************/

#ifdef O_GMP

#define COMPARE_FUNC(name, op, n1, n2) \
int \
name(Number n1, Number n2) \
{ switch(n1->type) \
  { case V_INTEGER: \
      return n1->value.i op n2->value.i; \
    case V_MPZ: \
      return mpz_cmp(n1->value.mpz, n2->value.mpz) op 0; \
    case V_MPQ: \
      return mpq_cmp(n1->value.mpq, n2->value.mpq) op 0; \
    case V_FLOAT: \
      return n1->value.f op n2->value.f; \
    default: \
      assert(0); \
      fail; \
  } \
}

#else /*O_GMP*/

#define COMPARE_FUNC(name, op, n1, n2) \
int \
name(Number n1, Number n2) \
{ switch(n1->type) \
  { case V_INTEGER: \
      return n1->value.i op n2->value.i; \
    case V_FLOAT: \
      return n1->value.f op n2->value.f; \
    default: \
      assert(0); \
      fail; \
  } \
}

#endif /*O_GMP*/

static COMPARE_FUNC(ar_compare_lt, <,  n1, n2)
static COMPARE_FUNC(ar_compare_gt, >,  n1, n2)
static COMPARE_FUNC(ar_compare_le, <=, n1, n2)
static COMPARE_FUNC(ar_compare_ge, >=, n1, n2)
static COMPARE_FUNC(ar_compare_ne, !=, n1, n2)
       COMPARE_FUNC(ar_compare_eq, ==, n1, n2)

int
ar_compare(Number n1, Number n2, int what)
{ same_type_numbers(n1, n2);

  switch(what)
  { case LT: return ar_compare_lt(n1, n2);
    case GT: return ar_compare_gt(n1, n2);
    case LE: return ar_compare_le(n1, n2);
    case GE: return ar_compare_ge(n1, n2);
    case NE: return ar_compare_ne(n1, n2);
    case EQ: return ar_compare_eq(n1, n2);
    default:
      assert(0);
      fail;
  }
}


static word
compareNumbers(term_t n1, term_t n2, int what ARG_LD)
{ AR_CTX
  number left, right;
  int rc;

  AR_BEGIN();

  if ( valueExpression(n1, &left PASS_LD) &&
       valueExpression(n2, &right PASS_LD) )
  { rc = ar_compare(&left, &right, what);

    clearNumber(&left);
    clearNumber(&right);
  } else
    rc = FALSE;

  AR_END();

  return rc;
}

static
PRED_IMPL("<", 2, lt, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, LT PASS_LD);
}

static
PRED_IMPL(">", 2, gt, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, GT PASS_LD);
}

static
PRED_IMPL("=<", 2, leq, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, LE PASS_LD);
}

static
PRED_IMPL(">=", 2, geq, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, GE PASS_LD);
}

static
PRED_IMPL("=\\=", 2, neq, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, NE PASS_LD);
}

static
PRED_IMPL("=:=", 2, eq, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, EQ PASS_LD);
}


		 /*******************************
		 *	 ARITHMETIC STACK	*
		 *******************************/

Number
allocArithStack(ARG1_LD)
{ Number n;

  if ( LD->arith.stack.top == LD->arith.stack.max )
  { size_t size;

    if ( LD->arith.stack.base )
    { size = (size_t)(LD->arith.stack.max - LD->arith.stack.base);
      LD->arith.stack.base = PL_realloc(LD->arith.stack.base, size*sizeof(number)*2);
      LD->arith.stack.top  = LD->arith.stack.base+size;
      size *= 2;
    } else
    { size = 16;
      LD->arith.stack.base = PL_malloc(size*sizeof(number));
      LD->arith.stack.top  = LD->arith.stack.base;
    }

    LD->arith.stack.max = LD->arith.stack.base+size;
  }

  n = LD->arith.stack.top;
  LD->arith.stack.top++;

  return n;
}


void
pushArithStack(Number n ARG_LD)
{ Number np = allocArithStack(PASS_LD1);

  *np = *n;				/* structure copy */
}


void
resetArithStack(ARG1_LD)
{ LD->arith.stack.top = LD->arith.stack.base;
}


Number
argvArithStack(int n ARG_LD)
{ assert(LD->arith.stack.top - n >= LD->arith.stack.base);

  return LD->arith.stack.top - n;
}


void
popArgvArithStack(int n ARG_LD)
{ assert(LD->arith.stack.top - n >= LD->arith.stack.base);

  for(; n>0; n--)
  { LD->arith.stack.top--;
    clearNumber(LD->arith.stack.top);
  }
}


void
freeArithLocalData(PL_local_data_t *ld)
{ if ( ld->arith.stack.base )
    PL_free(ld->arith.stack.base);
#ifdef O_GMP
  if ( ld->arith.random.initialised )
  { DEBUG(0, { GET_LD
	       assert(ld == LD);
	     });

    ld->gmp.persistent++;
    gmp_randclear(ld->arith.random.state);
    ld->gmp.persistent--;
    ld->arith.random.initialised = FALSE;
  }
#endif
}


		/********************************
		*           FUNCTIONS           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
isCurrentArithFunction(functor_t f)
    Find existing arithmetic function definition for f.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline ArithF
isCurrentArithFunction(functor_t f)
{ size_t index = indexFunctor(f);

  if ( index < GD->arith.functions_allocated )
  { return GD->arith.functions[index];
  }

  return NULL;
}


int
check_float(double f)
{
#ifdef HAVE_FPCLASSIFY
  switch(fpclassify(f))
  { case FP_NAN:
      return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
      break;
    case FP_INFINITE:
      return PL_error(NULL, 0, NULL, ERR_AR_OVERFLOW);
      break;
  }
#else
#ifdef HAVE_FPCLASS
  switch(fpclass(f))
  { case FP_SNAN:
    case FP_QNAN:
      return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
      break;
    case FP_NINF:
    case FP_PINF:
      return PL_error(NULL, 0, NULL, ERR_AR_OVERFLOW);
      break;
    case FP_NDENORM:			/* pos/neg denormalized non-zero */
    case FP_PDENORM:
    case FP_NNORM:			/* pos/neg normalized non-zero */
    case FP_PNORM:
    case FP_NZERO:			/* pos/neg zero */
    case FP_PZERO:
      break;
  }
#else
#ifdef HAVE__FPCLASS
  switch(_fpclass(f))
  { case _FPCLASS_SNAN:
    case _FPCLASS_QNAN:
      return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
      break;
    case _FPCLASS_NINF:
    case _FPCLASS_PINF:
      return PL_error(NULL, 0, NULL, ERR_AR_OVERFLOW);
      break;
  }
#else
#ifdef HAVE_ISNAN
  if ( isnan(f) )
    return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
#endif
#ifdef HAVE_ISINF
  if ( isinf(f) )
    return PL_error(NULL, 0, NULL, ERR_AR_OVERFLOW);
#endif
#endif /*HAVE__FPCLASS*/
#endif /*HAVE_FPCLASS*/
#endif /*HAVE_FPCLASSIFY*/
  return TRUE;
}


		 /*******************************
		 *	     EVALULATE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
valueExpression() evaluates an `evaluable term'.

This new implementation avoids using the C-stack   to be able to process
more deeply nested terms and to be able  to recover in the unlikely case
that terms are still too deeply nested.

If finds a term, it starts processing at the last argument, working back
to the start. It it finds  the   functor  itself it evaluates the pushed
arguments. Using this technique we push as  few as possible arguments on
terms that are nested on the left (as   in (1+2)+3, while we only push a
single pointer for each recursion level in the evaluable term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
pushForMark(segstack *stack, Word p, int wr)
{ word w = ((word)p)|wr;

  return pushSegStack(stack, w, word);
}

static void
popForMark(segstack *stack, Word *pp, int *wr)
{ word w;

  popSegStack(stack, &w, word);
  *wr = w & (word)0x1;
  *pp = (Word)(w & ~(word)0x1);
}


int
valueExpression(term_t expr, number *result ARG_LD)
{ segstack term_stack;
  segstack arg_stack;
  Word term_buf[16];
  number arg_buf[16];
  number *n = result;
  number n_tmp;
  int walk_ref = FALSE;
  Word p = valTermRef(expr);
  Word start;
  int known_acyclic = FALSE;
  int pushed = 0;

  deRef(p);
  start = p;
  LD->in_arithmetic++;

  for(;;)
  { switch(tag(*p))
    { case TAG_INTEGER:
	get_integer(*p, n);
        break;
      case TAG_FLOAT:
	n->value.f = valFloat(*p);
        n->type = V_FLOAT;
	break;
      case TAG_VAR:
	PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
        goto error;
      case TAG_REFERENCE:
      { if ( !pushForMark(&term_stack, p, walk_ref) )
	{ PL_no_memory();
	  goto error;
	}
	walk_ref = TRUE;
	deRef(p);
	continue;
      }
      case TAG_ATOM:
      { functor_t functor = lookupFunctorDef(*p, 0);
	ArithF f;

        if ( (f = isCurrentArithFunction(functor)) )
	{ if ( (*f)(n) != TRUE )
	    goto error;
	} else
	{ PL_error(NULL, 0, NULL, ERR_NOT_EVALUABLE, functor);
	  goto error;
	}
	break;
      }
      case TAG_COMPOUND:
      { Functor term = valueTerm(*p);
	int arity;

	if ( term->definition == FUNCTOR_dot2 )
	{ if ( getCharExpression(p, n PASS_LD) != TRUE )
	    goto error;
	  break;
	}

	if ( p == start )
	{ initSegStack(&term_stack, sizeof(Word), sizeof(term_buf), term_buf);
	  initSegStack(&arg_stack, sizeof(number), sizeof(arg_buf), arg_buf);
	}

	if ( !pushForMark(&term_stack, p, walk_ref) )
	{ PL_no_memory();
	  goto error;
	}
	if ( ++pushed > 100 && !known_acyclic )
	{ int rc;

	  if ( (rc=is_acyclic(start PASS_LD)) == TRUE )
	  { known_acyclic = TRUE;
	  } else
	  { if ( rc == MEMORY_OVERFLOW )
	      PL_error(NULL, 0, NULL, ERR_NOMEM);
	    else
	      PL_error(NULL, 0, "cyclic term", ERR_TYPE, ATOM_expression, expr);
	    goto error;
	  }
	}
	walk_ref = FALSE;
	n = &n_tmp;
	arity = arityFunctor(term->definition);
	p = &term->arguments[arity-1];
	continue;
      }
      default:
	PL_error(NULL, 0, NULL, ERR_PTR_TYPE, ATOM_number, p);
        goto error;
    }

    if ( p == start )
    { LD->in_arithmetic--;
      assert(n == result);

      return TRUE;
    }

    if ( walk_ref )
      popForMark(&term_stack, &p, &walk_ref);

    if ( !pushSegStack(&arg_stack, n_tmp, number) )
    { PL_no_memory();
      goto error;
    }

    while ( tagex(*--p) == (TAG_ATOM|STG_GLOBAL) )
    { functor_t functor = *p;
      ArithF f;

      DEBUG(1, Sdprintf("Eval %s/%d\n",
			stringAtom(nameFunctor(functor)),
			arityFunctor(functor)));

      if ( (f = isCurrentArithFunction(functor)) )
      { int arity = arityFunctor(functor);

	switch(arity)
	{ case 1:
	  { int rc;
	    number *a0 = topOfSegStack(&arg_stack);

	    rc = (*f)(a0, n);
	    clearNumber(a0);
	    if ( rc == TRUE )
	    { *a0 = *n;
	    } else
	    { popTopOfSegStack(&arg_stack);
	      goto error;
	    }

	    break;
	  }
	  case 2:
	  { int rc;
	    void *a[2];

	    topsOfSegStack(&arg_stack, 2, a);
	    rc = (*f)((number*)a[0], (number*)a[1], n);
	    clearNumber((number*)a[0]);
	    clearNumber((number*)a[1]);
	    popTopOfSegStack(&arg_stack);

	    if ( rc == TRUE )
	    { number *n1 = a[1];
	      *n1 = *n;
	    } else
	    { popTopOfSegStack(&arg_stack);
	      goto error;
	    }

	    break;
	  }
	  case 3:
	  { int rc;
	    void *a[3];

	    topsOfSegStack(&arg_stack, 3, a);
	    rc = (*f)((number*)a[0], (number*)a[1], (number*)a[2], n);
	    clearNumber((number*)a[0]);
	    clearNumber((number*)a[1]);
	    clearNumber((number*)a[2]);
	    popTopOfSegStack(&arg_stack);
	    popTopOfSegStack(&arg_stack);

	    if ( rc == TRUE )
	    { number *n2 = a[2];
	      *n2 = *n;
	    } else
	    { popTopOfSegStack(&arg_stack);
	      goto error;
	    }

	    break;
	  }
	  default:
	    assert(0);
	}

	popForMark(&term_stack, &p, &walk_ref);
	if ( p == start )
	{ LD->in_arithmetic--;
	  *result = *n;

	  return TRUE;
	}
	if ( walk_ref )
	  popForMark(&term_stack, &p, &walk_ref);
      } else
      { PL_error(NULL, 0, NULL, ERR_NOT_EVALUABLE, functor);
	goto error;
      }
    }
  }

error:
  if ( p != start )
  { number n;

    clearSegStack(&term_stack);
    while( popSegStack(&arg_stack, &n, number) )
      clearNumber(&n);
  }
  LD->in_arithmetic--;

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int arithChar(Word p)
    Handle arithmetic argument "x", normally appearing as [X], where X
    is an integer or one-character atom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
arithChar(Word p ARG_LD)
{ deRef(p);

  if ( isInteger(*p) )
  { intptr_t chr = valInt(*p);

    if ( chr >= 0 && chr <= 0x10ffff )	/* UNICODE_MAX */
      return (int)chr;
  } else if ( isAtom(*p) )
  { PL_chars_t txt;

    if ( get_atom_text(*p, &txt) && txt.length == 1 )
    { if ( txt.encoding == ENC_WCHAR )
	return txt.text.w[0];
      else
	return txt.text.t[0]&0xff;
    }
  }

  PL_error(NULL, 0, NULL, ERR_TYPE,
	   ATOM_character, pushWordAsTermRef(p));
  popTermRef();

  return EOF;
}


static int
getCharExpression(Word p, Number r ARG_LD)
{ Word a;
  int chr;

  deRef(p);

  a = argTermP(*p, 0);
  if ( (chr = arithChar(a PASS_LD)) == EOF )
    fail;

  a = argTermP(*p, 1);
  if ( !isNil(*a) )
  { PL_error(".", 2, "\"x\" must hold one character", ERR_TYPE,
	     ATOM_nil, pushWordAsTermRef(a));
    popTermRef();
    return FALSE;
  }

  r->value.i = chr;
  r->type = V_INTEGER;

  succeed;
}




		 /*******************************
		 *	     CONVERSION		*
		 *******************************/

static int
double_in_int64_range(double x)
{ int k;
  double y = frexp(x, &k);

  if ( k < 8*(int)sizeof(int64_t) ||
       (y == -0.5 && k == 8*(int)sizeof(int64_t)) )
    return TRUE;

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
toIntegerNumber(Number n, int flags)

Convert a number to an integer. Default,   only rationals that happen to
be integer are converted. If   TOINT_CONVERT_FLOAT  is present, floating
point  numbers  are  converted  if  they  represent  integers.  If  also
TOINT_TRUNCATE is provided non-integer floats are truncated to integers.

Note that if a double is  out  of   range  for  int64_t,  it never has a
fractional part.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
toIntegerNumber(Number n, int flags)
{ switch(n->type)
  { case V_INTEGER:
      succeed;
#ifdef O_GMP
    case V_MPZ:
      succeed;
    case V_MPQ:				/* never from stacks iff integer */
      if ( mpz_cmp_ui(mpq_denref(n->value.mpq), 1L) == 0 )
      { mpz_clear(mpq_denref(n->value.mpq));
	n->value.mpz[0] = mpq_numref(n->value.mpq)[0];
	n->type = V_MPZ;
	succeed;
      }
      fail;
#endif
    case V_FLOAT:
      if ( (flags & TOINT_CONVERT_FLOAT) )
      { if ( double_in_int64_range(n->value.f) )
	{ int64_t l = (int64_t)n->value.f;

	  if ( (flags & TOINT_TRUNCATE) ||
	       (double)l == n->value.f )
	  { n->value.i = l;
	    n->type = V_INTEGER;

	    return TRUE;
	  }
	  return FALSE;
#ifdef O_GMP
	} else
	{ mpz_init_set_d(n->value.mpz, n->value.f);
	  n->type = V_MPZ;

	  return TRUE;
#endif
	}
      }
      return FALSE;
  }

  assert(0);
  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
promoteIntNumber() promotes a number of type V_INTEGER to a number with
larger capacity.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
promoteIntNumber(Number n)
{
#ifdef O_GMP
  promoteToMPZNumber(n);
#else
  GET_LD

    if ( truePrologFlag(PLFLAG_ISO) )
      return PL_error("+", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);

  return promoteToFloatNumber(n);
#endif

  succeed;
}



		/********************************
		*     ARITHMETIC FUNCTIONS      *
		*********************************/

static int ar_u_minus(Number n1, Number r);

int
ar_add_ui(Number n, intptr_t add)
{ switch(n->type)
  { case V_INTEGER:
    { int64_t r = n->value.i + add;

      if ( (r < 0 && add > 0 && n->value.i > 0) ||
	   (r > 0 && add < 0 && n->value.i < 0) )
      { if ( !promoteIntNumber(n) )
	  fail;
      } else
      { n->value.i = r;
	succeed;
      }
    }
#ifdef O_GMP
    case V_MPZ:
    { if ( add > 0 )
	mpz_add_ui(n->value.mpz, n->value.mpz, (unsigned long)add);
      else
	mpz_sub_ui(n->value.mpz, n->value.mpz, (unsigned long)-add);

      succeed;
    }
    case V_MPQ:
    { if ( add > 0 )
	mpz_addmul_ui(mpq_numref(n->value.mpq), mpq_denref(n->value.mpq),
		      (unsigned long)add);
      else
	mpz_submul_ui(mpq_numref(n->value.mpq), mpq_denref(n->value.mpq),
		      (unsigned long)-add);

      succeed;
    }
#endif
    case V_FLOAT:
    { n->value.f += (double)add;

      return check_float(n->value.f);
    }
    default:
      ;
  }

  assert(0);
  fail;
}

#define SAME_SIGN(i1, i2) (((i1) ^ (i2)) >= 0)

int
pl_ar_add(Number n1, Number n2, Number r)
{ same_type_numbers(n1, n2);

  switch(n1->type)
  { case V_INTEGER:
    { if ( SAME_SIGN(n1->value.i, n2->value.i) )
      { if ( n2->value.i < 0 )		/* both negative */
	{ if ( n1->value.i < PLMININT - n2->value.i )
	    goto overflow;
	} else				/* both positive */
	{ if ( PLMAXINT - n1->value.i < n2->value.i )
	    goto overflow;
	}
      }
      r->value.i = n1->value.i + n2->value.i;
      r->type = V_INTEGER;
      succeed;
    overflow:
      if ( !promoteIntNumber(n1) ||
	   !promoteIntNumber(n2) )
	fail;
    }
    /*FALLTHROUGH*/
#ifdef O_GMP
    case V_MPZ:
    { r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_add(r->value.mpz, n1->value.mpz, n2->value.mpz);
      succeed;
    }
    case V_MPQ:
    { r->type = V_MPQ;
      mpq_init(r->value.mpq);
      mpq_add(r->value.mpq, n1->value.mpq, n2->value.mpq);
      succeed;
    }
#endif
    case V_FLOAT:
    { r->value.f = n1->value.f + n2->value.f;
      r->type = V_FLOAT;

      return check_float(r->value.f);
    }
  }

  assert(0);
  fail;
}


static int
ar_minus(Number n1, Number n2, Number r)
{ same_type_numbers(n1, n2);

  switch(n1->type)
  { case V_INTEGER:
    { r->value.i = n1->value.i - n2->value.i;

      if ( (n1->value.i >= 0 && n2->value.i < 0 && r->value.i <= 0) ||
	   (n1->value.i < 0  && n2->value.i > 0 && r->value.i >= 0) )
      {					/* overflow */
	if ( !promoteIntNumber(n1) ||
	     !promoteIntNumber(n2) )
	  fail;
      } else
      { r->type = V_INTEGER;
	succeed;
      }
    }
#ifdef O_GMP
    case V_MPZ:
    { r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_sub(r->value.mpz, n1->value.mpz, n2->value.mpz);
      succeed;
    }
    case V_MPQ:
    { r->type = V_MPQ;
      mpq_init(r->value.mpq);
      mpq_sub(r->value.mpq, n1->value.mpq, n2->value.mpq);
      succeed;
    }
#endif
    case V_FLOAT:
    { r->value.f = n1->value.f - n2->value.f;
      r->type = V_FLOAT;

      return check_float(r->value.f);
    }
  }

  assert(0);
  fail;
}


#ifdef O_GMP
static int
ar_even(Number i)
{ switch(i->type)
  { case V_INTEGER:
      return i->value.i % 2 == 0;
    case V_MPZ:
      return mpz_fdiv_ui(i->value.mpz, 2) == 0;
    default:
      assert(0);
      fail;
  }
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mod(X, Y) = X - (floor(X/Y) * Y)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline int64_t
mod(int64_t x, int64_t y)
{ int64_t r = x % y;

  if ( r != 0 && (r<0) != (y<0) )
    r += y;

  return r;
}


static int
ar_mod(Number n1, Number n2, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("mod", 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error("mod", 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

  same_type_numbers(n1, n2);

  switch(n1->type)
  { case V_INTEGER:
      if ( n2->value.i == 0 )
	return PL_error("mod", 2, NULL, ERR_DIV_BY_ZERO);

      r->value.i = mod(n1->value.i, n2->value.i);
      r->type = V_INTEGER;
      break;
#ifdef O_GMP
    case V_MPZ:
      if ( mpz_sgn(n2->value.mpz) == 0 )
	return PL_error("mod", 2, NULL, ERR_DIV_BY_ZERO);

      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_fdiv_r(r->value.mpz, n1->value.mpz, n2->value.mpz);
      break;
#endif
    default:
      assert(0);
  }

  succeed;
}


static int
msb64(int64_t i)
{ int j = 0;

  if (i >= LL(0x100000000)) {i >>= 32; j += 32;}
  if (i >=     LL(0x10000)) {i >>= 16; j += 16;}
  if (i >=       LL(0x100)) {i >>=  8; j +=  8;}
  if (i >=	  LL(0x10)) {i >>=  4; j +=  4;}
  if (i >=         LL(0x4)) {i >>=  2; j +=  2;}
  if (i >=         LL(0x2)) j++;

  return j;
}


static int
int_too_big()
{ GET_LD
  return (int)outOfStack((Stack)&LD->stacks.global, STACK_OVERFLOW_RAISE);
}


static int
shift_to_far(Number shift, Number r, int dir)
{ if ( ar_sign_i(shift) * dir < 0 )	/* << */
  { return int_too_big();
  } else
  { r->value.i = 0;
    r->type = V_INTEGER;

    return TRUE;
  }
}


static int
ar_shift(Number n1, Number n2, Number r, int dir)
{ long shift;
  const char *plop = (dir < 0 ? "<<" : ">>");

  if ( !toIntegerNumber(n1, 0) )
    return PL_error(plop, 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error(plop, 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

  if ( ar_sign_i(n1) == 0 )		/* shift of 0 is always 0 */
  { r->value.i = 0;
    r->type = V_INTEGER;
  }

  switch(n2->type)			/* amount to shift */
  { case V_INTEGER:
      if ( n2->value.i < LONG_MIN  ||
	   n2->value.i > LONG_MAX )
	return shift_to_far(n2, r, dir);
      else
	shift = (long)n2->value.i;
      break;
#ifdef O_GMP
    case V_MPZ:
      if ( mpz_cmp_si(n2->value.mpz, LONG_MIN) < 0 ||
	   mpz_cmp_si(n2->value.mpz, LONG_MAX) > 0 )
	return shift_to_far(n2, r, dir);
      else
	shift = mpz_get_si(n2->value.mpz);
      break;
#endif
    default:
      assert(0);
      fail;
  }

  if ( shift < 0 )
  { shift = -shift;
    dir = -dir;
  }

  switch(n1->type)
  { case V_INTEGER:
      if ( dir < 0 )			/* shift left (<<) */
      {
#ifdef O_GMP				/* msb() is 0..63 */
        int bits = shift;

	if ( n1->value.i >= 0 )
	  bits += msb64(n1->value.i);
	else if ( n1->value.i == PLMININT )
	  bits += sizeof(int64_t)*8;
	else
	  bits += msb64(-n1->value.i);

	if ( bits >= (int)(sizeof(int64_t)*8-1) )
	{ promoteToMPZNumber(n1);
	  goto mpz;
	} else
#endif
	{ r->value.i = n1->value.i << shift;
	}
      } else
      { if ( shift >= (long)sizeof(int64_t)*8 )
	  r->value.i = (r->value.i >= 0 ? 0 : -1);
	else
	  r->value.i = n1->value.i >> shift;
      }
      r->type = V_INTEGER;
      succeed;
#ifdef O_GMP
    case V_MPZ:
    mpz:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      if ( dir < 0 )
      {
#ifdef O_GMP_PRECHECK_ALLOCATIONS
	GET_LD
	uint64_t msb = mpz_sizeinbase(n1->value.mpz, 2)+shift;

	if ( (msb/sizeof(char)) > (uint64_t)limitStack(global) )
	{ mpz_clear(r->value.mpz);
	  return (int)outOfStack(&LD->stacks.global, STACK_OVERFLOW_RAISE);
	}
#endif /*O_GMP_PRECHECK_ALLOCATIONS*/
	mpz_mul_2exp(r->value.mpz, n1->value.mpz, shift);
      } else
      { mpz_fdiv_q_2exp(r->value.mpz, n1->value.mpz, shift);
      }
      succeed;
#endif
    default:
      assert(0);
      fail;
  }
}


static int
ar_shift_left(Number n1, Number n2, Number r)
{ return ar_shift(n1, n2, r, -1);
}


static int
ar_shift_right(Number n1, Number n2, Number r)
{ return ar_shift(n1, n2, r, 1);
}


static int
ar_gcd(Number n1, Number n2, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("gcd", 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error("gcd", 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

  same_type_numbers(n1, n2);
  switch(n1->type)
  { case V_INTEGER:
    { int64_t a = n1->value.i;
      int64_t b = n2->value.i;
      int64_t t;

      if ( a < 0 )
      { a = -a;
	if ( a < 0 )
	{ promote:
#ifdef O_GMP
	  promoteToMPZNumber(n1);
	  promoteToMPZNumber(n2);
	  goto case_gmp;
#else
	  return PL_error("gcd", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
	}
      }
      if ( b < 0 )
      { b = -b;
	if ( b < 0 )
	  goto promote;
      }
      while(b != 0)
      { t = b;
	b = a % b;
	a = t;
      }
      r->type = V_INTEGER;
      r->value.i = a;
      break;
    }
#ifdef O_GMP
    case V_MPZ:
    case_gmp:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_gcd(r->value.mpz, n1->value.mpz, n2->value.mpz);
      break;
#endif
    default:
      assert(0);
  }

  succeed;
}


/* Unary functions requiring double argument */

#define UNAIRY_FLOAT_FUNCTION(name, op) \
  static int \
  name(Number n1, Number r) \
  { if ( !promoteToFloatNumber(n1) ) return FALSE; \
    r->value.f = op(n1->value.f); \
    r->type    = V_FLOAT; \
    return check_float(r->value.f); \
  }

/* Binary functions requiring integer argument */

#ifdef O_GMP
#define BINAIRY_INT_FUNCTION(name, plop, op, mpop) \
  static int \
  name(Number n1, Number n2, Number r) \
  { if ( !toIntegerNumber(n1, 0) ) \
      return PL_error(plop, 2, NULL, ERR_AR_TYPE, ATOM_integer, n1); \
    if ( !toIntegerNumber(n2, 0) ) \
      return PL_error(plop, 2, NULL, ERR_AR_TYPE, ATOM_integer, n2); \
    same_type_numbers(n1, n2); \
    switch(n1->type) \
    { case V_INTEGER: \
	r->value.i = n1->value.i op n2->value.i; \
	r->type = V_INTEGER; \
	succeed; \
      case V_MPZ: \
	r->type = V_MPZ; \
	mpz_init(r->value.mpz); \
	mpop(r->value.mpz, n1->value.mpz, n2->value.mpz); \
        succeed; \
      default: \
	assert(0); \
        fail; \
    } \
  }

#else /*O_GMP*/

#define BINAIRY_INT_FUNCTION(name, plop, op, mpop) \
  static int \
  name(Number n1, Number n2, Number r) \
  { if ( !toIntegerNumber(n1, 0) ) \
      return PL_error(plop, 2, NULL, ERR_AR_TYPE, ATOM_integer, n1); \
    if ( !toIntegerNumber(n2, 0) ) \
      return PL_error(plop, 2, NULL, ERR_AR_TYPE, ATOM_integer, n2); \
    same_type_numbers(n1, n2); \
    switch(n1->type) \
    { case V_INTEGER: \
	r->value.i = n1->value.i op n2->value.i; \
	r->type = V_INTEGER; \
	succeed; \
      default: \
	assert(0); \
        fail; \
    } \
  }
#endif /*O_GMP*/

#define BINAIRY_FLOAT_FUNCTION(name, func) \
  static int \
  name(Number n1, Number n2, Number r) \
  { if ( !promoteToFloatNumber(n1) || \
	 !promoteToFloatNumber(n2) ) return FALSE; \
    r->value.f = func(n1->value.f, n2->value.f); \
    r->type = V_FLOAT; \
    return check_float(r->value.f); \
  }

UNAIRY_FLOAT_FUNCTION(ar_sin, sin)
UNAIRY_FLOAT_FUNCTION(ar_cos, cos)
UNAIRY_FLOAT_FUNCTION(ar_tan, tan)
UNAIRY_FLOAT_FUNCTION(ar_atan, atan)
UNAIRY_FLOAT_FUNCTION(ar_exp, exp)

BINAIRY_FLOAT_FUNCTION(ar_atan2, atan2)

BINAIRY_INT_FUNCTION(ar_disjunct,    "\\/", |, mpz_ior)
BINAIRY_INT_FUNCTION(ar_conjunct,    "/\\", &, mpz_and)
BINAIRY_INT_FUNCTION(ar_xor,         "xor", ^, mpz_xor)

static int
ar_pow(Number n1, Number n2, Number r)
{
#ifdef O_GMP
  if ( intNumber(n1) && intNumber(n2) )
  { unsigned long exp;

    switch(n1->type)			/* test for 0**X and 1**X */
    { case V_INTEGER:
	if ( n1->value.i == 0 )
	{ ret0:
	  r->type = V_INTEGER;
	  if ( ar_sign_i(n2) == 0 )	/* 0**0 --> 1 */
	    r->value.i = 1;
	  else
	    r->value.i = 0;
	  succeed;
	}
        if ( n1->value.i == 1 )
	{ ret1:
	  r->type = V_INTEGER;
	  r->value.i = 1;
	  succeed;
	}
	if ( n1->value.i == -1 )
	{ ret_1:
	  r->type = V_INTEGER;
	  if ( ar_even(n2) )
	    r->value.i = 1;
	  else
	    r->value.i = -1;
	  succeed;
	}
        break;
      case V_MPZ:
	if ( mpz_cmp_si(n1->value.mpz, 0) == 0 )
	  goto ret0;
        if ( mpz_cmp_si(n1->value.mpz, 1) == 0 )
	  goto ret1;
        if ( mpz_cmp_si(n1->value.mpz, -1) == 0 )
	  goto ret_1;
	break;
      default:
	assert(0);
    }

					/* get the exponent */
    switch(n2->type)
    { case V_INTEGER:
	if ( n2->value.i < 0 )
	  goto doreal;
        if ( n2->value.i > LONG_MAX )
	  return int_too_big();
	exp = (long)n2->value.i;
	break;
      case V_MPZ:
	if ( mpz_sgn(n2->value.mpz) < 0 )
	  goto doreal;
        if ( mpz_cmp_si(n2->value.mpz, LONG_MAX) > 0 )
	  return int_too_big();
        exp = mpz_get_ui(n2->value.mpz);
	break;
      default:
	assert(0);
        fail;
    }

    r->type = V_MPZ;
    mpz_init(r->value.mpz);

    switch(n1->type)
    { case V_INTEGER:
	if ( n1->value.i >= 0L && n1->value.i <= LONG_MAX )
	{ mpz_ui_pow_ui(r->value.mpz, (unsigned long)n1->value.i, exp);
	  succeed;
	} else
	{ promoteToMPZNumber(n1);
	  /*FALLTHROUGH*/
	}
      case V_MPZ:
	mpz_pow_ui(r->value.mpz, n1->value.mpz, exp);
        succeed;
      default:
	assert(0);
        fail;
    }
  }

doreal:
#endif /*O_GMP*/
  if ( !promoteToFloatNumber(n1) ||
       !promoteToFloatNumber(n2) )
    return FALSE;
  r->value.f = pow(n1->value.f, n2->value.f);
  r->type = V_FLOAT;

  return check_float(r->value.f);
}


static int
ar_powm(Number base, Number exp, Number mod, Number r)
{
  if ( !intNumber(base) )
    PL_error("powm", 3, NULL, ERR_AR_TYPE, ATOM_integer, base);
  if ( !intNumber(exp) )
    PL_error("powm", 3, NULL, ERR_AR_TYPE, ATOM_integer, exp);
  if ( !intNumber(exp) )
    PL_error("powm", 3, NULL, ERR_AR_TYPE, ATOM_integer, exp);

#ifdef O_GMP
  promoteToMPZNumber(base);
  promoteToMPZNumber(exp);
  promoteToMPZNumber(mod);

  r->type = V_MPZ;
  mpz_init(r->value.mpz);
  mpz_powm(r->value.mpz, base->value.mpz, exp->value.mpz, mod->value.mpz);

  succeed;
#else
  return PL_error("powm", 3, "requires unbounded arithmetic (GMP) support",
		  ERR_NOT_IMPLEMENTED, "powm/3");
#endif
}


static int
ar_sqrt(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  if ( n1->value.f < 0 )
    return PL_error("sqrt", 1, NULL, ERR_AR_UNDEF);
  r->value.f = sqrt(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r->value.f);
}


static int
ar_asin(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  if ( n1->value.f < -1.0 || n1->value.f > 1.0 )
    return PL_error("asin", 1, NULL, ERR_AR_UNDEF);
  r->value.f = asin(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r->value.f);
}


static int
ar_acos(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  if ( n1->value.f < -1.0 || n1->value.f > 1.0 )
    return PL_error("acos", 1, NULL, ERR_AR_UNDEF);
  r->value.f = acos(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r->value.f);
}


static int
ar_log(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  if ( n1->value.f <= 0.0 )
    return PL_error("log", 1, NULL, ERR_AR_UNDEF);
  r->value.f = log(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r->value.f);
}


static int
ar_log10(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  if ( n1->value.f <= 0.0 )
    return PL_error("log10", 1, NULL, ERR_AR_UNDEF);
  r->value.f = log10(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r->value.f);
}


/* IntExpr1 // IntExpr2

Integer division. Defined by ISO core   standard  as rnd(X,Y), where the
direction of the rounding is conform the flag integer_rounding_function,
which is one of =toward_zero= or =down=.

The implementation below rounds according to the C-compiler. This is not
desirable, but I understand that as  of   C99,  this is towards zero and
this is precisely what we want to make  this different from div/2. As we
need C99 for the wide-character  support   anyway,  we  should be fairly
safe.
*/

static int
ar_tdiv(Number n1, Number n2, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("//", 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error("//", 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

#ifdef O_GMP
  if ( n1->type == V_INTEGER && n2->type == V_INTEGER )
#endif
  { if ( n2->value.i == 0 )
      return PL_error("//", 2, NULL, ERR_DIV_BY_ZERO);

    if ( !(n2->value.i == -1 && n1->value.i == PLMININT) )
    { r->value.i = n1->value.i / n2->value.i;
      r->type = V_INTEGER;

      succeed;
    }
  }

#ifdef O_GMP
  promoteToMPZNumber(n1);
  promoteToMPZNumber(n2);

  if ( mpz_sgn(n2->value.mpz) == 0 )
    return PL_error("//", 2, NULL, ERR_DIV_BY_ZERO);

  r->type = V_MPZ;
  mpz_init(r->value.mpz);
  if ( (-3 / 2) == -1 )
    mpz_tdiv_q(r->value.mpz, n1->value.mpz, n2->value.mpz);
  else
    mpz_fdiv_q(r->value.mpz, n1->value.mpz, n2->value.mpz);

  succeed;
#else
  return PL_error("//", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
}


/** div(IntExpr1, IntExpr2)

Result is rnd_i(IntExpr1/IntExpr2), rounded towards -infinity
*/

static int
ar_div(Number n1, Number n2, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("//", 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error("//", 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

#ifdef O_GMP
  if ( n1->type == V_INTEGER && n2->type == V_INTEGER )
#endif
  { if ( n2->value.i == 0 )
      return PL_error("div", 2, NULL, ERR_DIV_BY_ZERO);

    if ( !(n2->value.i == -1 && n1->value.i == PLMININT) )
    { r->value.i = (n1->value.i - mod(n1->value.i, n2->value.i)) / n2->value.i;
      r->type = V_INTEGER;

      succeed;
    }
  }

#ifdef O_GMP
  promoteToMPZNumber(n1);
  promoteToMPZNumber(n2);

  if ( mpz_sgn(n2->value.mpz) == 0 )
    return PL_error("//", 2, NULL, ERR_DIV_BY_ZERO);

  r->type = V_MPZ;
  mpz_init(r->value.mpz);
  mpz_fdiv_q(r->value.mpz, n1->value.mpz, n2->value.mpz);

  succeed;
#else
  return PL_error("div", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
}


#ifndef HAVE_SIGNBIT				/* differs for -0.0 */
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
signbit() and copysign() are part of C99.   These  should be provided by
most C compilers, but Microsoft decided  not   to  adopt  C99 (it is now
2012).

Note that there is no autoconf support  to verify that floats conform to
the IEE754 representation,  but  they  typically   do  these  days.  See
http://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Floating-Point-Portability.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#define IEEE754 1

#ifdef IEEE754
static inline int
signbit(double f)
{ union
  { double f;
    int64_t i;
  } v;

  v.f = f;
  return v.i < 0;
}

#ifndef copysign
double
copysign(double x, double y)
{ union { double f; uint64_t i; } ux, uy;
  const uint64_t smask = (uint64_t)1<<(sizeof(uint64_t)*8-1);

  ux.f = x;
  uy.f = y;
  ux.i &= ~smask;
  ux.i |= (uy.i & smask);

  return ux.f;
}
#endif
#else
#error "Don't know how to support signbit() and copysign()"
#endif
#endif

int
ar_sign_i(Number n1)
{ switch(n1->type)
  { case V_INTEGER:
      return (n1->value.i < 0 ? -1 : n1->value.i > 0 ? 1 : 0);
#ifdef O_GMP
    case V_MPZ:
      return mpz_sgn(n1->value.mpz);
    case V_MPQ:
      return mpq_sgn(n1->value.mpq);
#endif
    default:
      assert(0);
      fail;
  }
}

static int
ar_sign(Number n1, Number r)
{ if ( n1->type == V_FLOAT )
  { r->value.f = n1->value.f < 0 ? -1.0 : n1->value.f > 0.0 ? 1.0 : 0.0;
    r->type = V_FLOAT;
  } else
  { r->value.i = ar_sign_i(n1);
    r->type = V_INTEGER;
  }

  succeed;
}


static int
ar_signbit(Number n)
{ switch(n->type)
  { case V_INTEGER:
      return n->value.i	< 0 ? -1 : 1;
#ifdef O_GMP
    case V_MPZ:
    { int i = mpz_sgn(n->value.mpz);
      return i < 0 ? -1 : 1;
    }
    case V_MPQ:
    { int i = mpq_sgn(n->value.mpq);
      return i < 0 ? -1 : 1;
    }
#endif
    case V_FLOAT:
      return signbit(n->value.f) ? -1 : 1;
    default:
      assert(0);
      return 0;
  }
}


static int
ar_copysign(Number n1, Number n2, Number r)
{
  if ( n1->type == V_FLOAT && n2->type == V_FLOAT )
  { r->value.f = copysign(n1->value.f, n2->value.f);
    r->type = V_FLOAT;
  } else
  { if ( ar_signbit(n1) != ar_signbit(n2) )
      return ar_u_minus(n1, r);
    else
      cpNumber(r, n1);
  }

  return TRUE;
}


static int
ar_rem(Number n1, Number n2, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("rem", 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error("rem", 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

  same_type_numbers(n1, n2);
  switch(n1->type)
  { case V_INTEGER:
      if ( n2->value.i == 0 )
	return PL_error("rem", 2, NULL, ERR_DIV_BY_ZERO);

      r->value.i = n1->value.i % n2->value.i;
      r->type = V_INTEGER;

      break;
#ifdef O_GMP
    case V_MPZ:
    { if ( mpz_sgn(n2->value.mpz) == 0 )
	return PL_error("rem", 2, NULL, ERR_DIV_BY_ZERO);

      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_tdiv_r(r->value.mpz, n1->value.mpz, n2->value.mpz);
      break;
    }
#endif

    default:
      assert(0);
      fail;
  }

  succeed;
}


#ifdef O_GMP
static int
ar_rational(Number n1, Number r)
{ cpNumber(r, n1);
  promoteToMPQNumber(r);

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A is rationalize(Float)

Introduced on the suggestion of Richard   O'Keefe  after the Common Lisp
standard. The algorithm is taken from figure  3 in ``A Rational Rotation
Method for Robust Geometric Algorithms'' by John Canny, Bruce Donald and
Eugene K. Ressler.  Found at

http://www.cs.dartmouth.edu/~brd/papers/rotations-scg92.pdf
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef DBL_EPSILON			/* normal for IEEE 64-bit double */
#define DBL_EPSILON 0.00000000000000022204
#endif

static int
ar_rationalize(Number n1, Number r)
{ switch(n1->type)
  { case V_INTEGER:
    case V_MPZ:
    case V_MPQ:
      cpNumber(r, n1);
      promoteToMPQNumber(r);
      succeed;
    case V_FLOAT:
    { double e0 = n1->value.f, p0 = 0.0, q0 = 1.0;
      double e1 =	 -1.0, p1 = 1.0, q1 = 0.0;
      double d;

      do
      { double r = floor(e0/e1);
	double e00 = e0, p00 = p0, q00 = q0;

	e0 = e1;
	p0 = p1;
	q0 = q1;
	e1 = e00 - r*e1;
	p1 = p00 - r*p1;
	q1 = q00 - r*q1;

	DEBUG(2, Sdprintf("e = %.20f, r = %f, p1/q1 = %f/%f\n",
			  DBL_EPSILON, r, p1, q1));

	d = p1/q1 - n1->value.f;
      } while(abs(d) > DBL_EPSILON);

      r->type = V_MPQ;
      mpz_init_set_d(mpq_numref(r->value.mpq), p1);
      mpz_init_set_d(mpq_denref(r->value.mpq), q1);
      mpq_canonicalize(r->value.mpq);	/* is this needed? */

      succeed;
    }
  }

  assert(0);
  fail;
}


static int
ar_rdiv(Number n1, Number n2, Number r)
{ if ( toIntegerNumber(n1, 0) &&
       toIntegerNumber(n2, 0) )
  { promoteToMPZNumber(n1);
    promoteToMPZNumber(n2);

    if ( mpz_sgn(n2->value.mpz) == 0 )
      return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);
    if ( mpz_divisible_p(n1->value.mpz, n2->value.mpz) )
    { mpz_init(r->value.mpz);
      r->type = V_MPZ;
      mpz_divexact(r->value.mpz, n1->value.mpz, n2->value.mpz);
      succeed;
    }
    r->type = V_MPQ;
    mpq_init(r->value.mpq);
    mpz_set(mpq_numref(r->value.mpq), n1->value.mpz);
    mpz_set(mpq_denref(r->value.mpq), n2->value.mpz);
    mpq_canonicalize(r->value.mpq);
  } else
  { promoteToMPQNumber(n1);
    promoteToMPQNumber(n2);

    if ( mpz_sgn(mpq_numref(n2->value.mpq)) == 0 )
      return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);

    r->type = V_MPQ;
    mpq_init(r->value.mpq);
    mpq_div(r->value.mpq, n1->value.mpq, n2->value.mpq);
  }

  succeed;
}
#endif /*O_GMP*/


static int
ar_divide(Number n1, Number n2, Number r)
{ GET_LD

  if ( !truePrologFlag(PLFLAG_ISO) )
  { same_type_numbers(n1, n2);

    switch(n1->type)
    { case V_INTEGER:
	if ( n2->value.i == LL(0) )
	  return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);
        if ( n1->value.i % n2->value.i == 0 )
	{ r->value.i = n1->value.i / n2->value.i;
	  r->type = V_INTEGER;
	  succeed;
	}
	break;
#ifdef O_GMP
      case V_MPZ:
	if ( mpz_sgn(n2->value.mpz) == 0 )
	  return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);
	if ( mpz_divisible_p(n1->value.mpz, n2->value.mpz) )
	{ mpz_init(r->value.mpz);
	  r->type = V_MPZ;
	  mpz_divexact(r->value.mpz, n1->value.mpz, n2->value.mpz);
	  succeed;
	}
        break;
      case V_MPQ:
	if ( mpq_sgn(n2->value.mpq) == 0 )
	  return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);
        mpq_init(r->value.mpq);
	r->type = V_MPQ;
	mpq_div(r->value.mpq, n1->value.mpq, n2->value.mpq);
	succeed;
#endif
      case V_FLOAT:
	break;
    }
  }

					/* TBD: How to handle Q? */
  if ( !promoteToFloatNumber(n1) ||
       !promoteToFloatNumber(n2) )
    return FALSE;
  if ( n2->value.f == 0.0 )
    return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);
  r->value.f = n1->value.f / n2->value.f;
  r->type = V_FLOAT;

  return check_float(r->value.f);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mul64(int64_t x, int64_t y, int64_t *r)
    *r = x*y.  Returns TRUE if there is no overflow, FALSE on overflow.
    This is pretty complicated.  Bart Demoen pointed me at "Revisiting
    Overflow in Integer Multiplication" by Ayeas Qawasmeh and Ahmed
    Dalalah.  They prove nor claim their simple tests are complete
    (notably it is not clear whether they may falsily signal overflow).
    Their Multiply_using_splitting() looks promising, but is flawed
    as the results r2 and r3 must be shifted and split.

    They do suggest to multiply and then divide to check the result.
    They claim this is not correct as the behaviour of C is undefined
    on overflow, but as far as I can tell, it is defined as the truncated
    result for the multiplication of _unsigned_ integers.  Hence, we do
    unsigned multiplication, change back to signed and check using
    division.

    As division is pretty expensive, we make a quick test to see whether
    we are in the danger zone.

    Finally, we must avoid INT64_MIN/-1 :-(
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MU64_SAFE_MAX (LL(1)<<30)
#ifndef INT64_MIN
#define INT64_MIN (LL(1)<<63)
#endif

static int
mul64(int64_t x, int64_t y, int64_t *r)
{ if ( x == LL(0) || y == LL(0) )
  { *r = LL(0);
    return TRUE;
  } else
  { int sign;
    uint64_t ax, ay;
    int64_t prod;

    if ( x > LL(0) )
    { ax = x;
      if ( y > LL(0) )
      { ay = y;
	sign = 1;
      } else
      { ay = -y;
	sign = -1;
      }
    } else
    { ax = -x;
      if ( y > LL(0) )
      { ay = y;
	sign = -1;
      } else
      { ay = -y;
	sign = 1;
      }
    }

    prod = (int64_t)(ax*ay);
    if ( sign < 0 )
      prod = -prod;
    if ( (ax < MU64_SAFE_MAX && ay < MU64_SAFE_MAX) )
    { *r = prod;
      return TRUE;
    }
    if ( !(y==LL(-1) && prod == INT64_MIN) && prod/y == x )
    { *r = prod;
      return TRUE;
    }

    return FALSE;
  }
}


int
ar_mul(Number n1, Number n2, Number r)
{ same_type_numbers(n1, n2);

  switch(n1->type)
  { case V_INTEGER:
      if ( mul64(n1->value.i, n2->value.i, &r->value.i) )
      { r->type = V_INTEGER;
	succeed;
      }
      /*FALLTHROUGH*/
#ifdef O_GMP
      promoteToMPZNumber(n1);
      promoteToMPZNumber(n2);

    case V_MPZ:
      mpz_init(r->value.mpz);
      r->type = V_MPZ;
      mpz_mul(r->value.mpz, n1->value.mpz, n2->value.mpz);
      succeed;
    case V_MPQ:
      r->type = V_MPQ;
      mpq_init(r->value.mpq);
      mpq_mul(r->value.mpq, n1->value.mpq, n2->value.mpq);
      succeed;
#else
      return PL_error("*", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
    case V_FLOAT:
      r->value.f = n1->value.f * n2->value.f;
      r->type = V_FLOAT;

      return check_float(r->value.f);
  }

  assert(0);
  fail;
}


static int
ar_minmax(Number n1, Number n2, Number r, int ismax)
{ int which;
  number cp1, cp2;
  Number c1 = n1;
  Number c2 = n2;

  if ( c1->type != c2->type )
  { if ( c1->type > c2->type )
    { cpNumber(&cp2, c2);
      promoteNumber(&cp2, c1->type);
      c2 = &cp2;
    } else
    { cpNumber(&cp1, c1);
      promoteNumber(&cp1, c2->type);
      c1 = &cp1;
    }
  }

  switch(c1->type)
  { case V_INTEGER:
      which = c1->value.i >= c2->value.i;
      break;
#ifdef O_GMP
    case V_MPZ:
      which = (mpz_cmp(c1->value.mpz, c2->value.mpz) > 0);
      break;
    case V_MPQ:
      which = (mpq_cmp(c1->value.mpq, c2->value.mpq) > 0);
      break;
#endif
    case V_FLOAT:
      which = c1->value.f >= c2->value.f;
      break;
    default:
      assert(0);
      fail;
  }

  if ( c1 == &cp1 )
    clearNumber(c1);
  else if ( c2 == &cp2 )
    clearNumber(c2);

  if ( !ismax )
    which = !which;

  if ( which )
    cpNumber(r, n1);
  else
    cpNumber(r, n2);

  succeed;
}


static int
ar_max(Number n1, Number n2, Number r)
{ return ar_minmax(n1, n2, r, TRUE);
}


static int
ar_min(Number n1, Number n2, Number r)
{ return ar_minmax(n1, n2, r, FALSE);
}


static int
ar_negation(Number n1, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("\\", 1, NULL, ERR_AR_TYPE, ATOM_integer, n1);

  switch(n1->type)
  { case V_INTEGER:
      r->value.i = ~n1->value.i;
      r->type = V_INTEGER;
      break;
#ifdef O_GMP
    case V_MPZ:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_com(r->value.mpz, n1->value.mpz);
      break;
#endif
    default:
      assert(0);
      fail;
  }
  succeed;
}


static int
notLessThanZero(const char *f, int a, Number n)
{ return PL_error(f, a, NULL, ERR_AR_DOMAIN, ATOM_not_less_than_zero, n);
}


static int
mustBePositive(const char *f, int a, Number n)
{ return PL_error(f, a, NULL, ERR_AR_DOMAIN, ATOM_not_less_than_one, n);
}


static int
ar_msb(Number n1, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("msb", 1, NULL, ERR_AR_TYPE, ATOM_integer, n1);

  switch(n1->type)
  { case V_INTEGER:
      if (  n1->value.i <= 0 )
	return mustBePositive("msb", 1, n1);

      r->value.i = msb64(n1->value.i);
      r->type = V_INTEGER;
      succeed;
#ifdef O_GMP
    case V_MPZ:
      if ( mpz_sgn(n1->value.mpz) <= 0 )
	return mustBePositive("msb", 1, n1);
      if ( mpz_sgn(n1->value.mpz) == 0 )
      { r->value.i = 0;
      } else		/* is binary print-size the best we can do?? */
      { r->value.i = mpz_sizeinbase(n1->value.mpz, 2)-1;
      }
      r->type = V_INTEGER;
      succeed;
#endif
    default:
      assert(0);
      fail;
  }
}


static int
lsb64(int64_t i)
{ int j = 0;

  if ( i == 0 )
    return 0;

  if (!(i & LL(0xffffffff))) {i >>= 32; j += 32;}
  if (!(i &     LL(0xffff))) {i >>= 16; j += 16;}
  if (!(i &       LL(0xff))) {i >>=  8; j +=  8;}
  if (!(i &	   LL(0xf))) {i >>=  4; j +=  4;}
  if (!(i &        LL(0x3))) {i >>=  2; j +=  2;}
  if (!(i &        LL(0x1))) j++;

  return j;
}


static int
ar_lsb(Number n1, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("lsb", 1, NULL, ERR_AR_TYPE, ATOM_integer, n1);

  switch(n1->type)
  { case V_INTEGER:
      if (  n1->value.i <= 0 )
	return mustBePositive("lsb", 1, n1);

      r->value.i = lsb64(n1->value.i);
      r->type = V_INTEGER;
      succeed;
#ifdef O_GMP
    case V_MPZ:
      if ( mpz_sgn(n1->value.mpz) <= 0 )
	return mustBePositive("lsb", 1, n1);
      r->value.i = mpz_scan1(n1->value.mpz, 0);
      r->type = V_INTEGER;
      succeed;
#endif
    default:
      assert(0);
      fail;
  }
}


static int
my_popcount64(int64_t i)		/* my_: avoid NetBSD name conflict */
{ int c;
  size_t j;
  int64_t m = LL(1);

  for(j=0,c=0; j<sizeof(i)*8; j++, m<<=1)
  { if ( i&m )
      c++;
  }

  return c;
}


static int
ar_popcount(Number n1, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("popcount", 1, NULL, ERR_AR_TYPE, ATOM_integer, n1);

  switch(n1->type)
  { case V_INTEGER:
      if (  n1->value.i < 0 )
	return notLessThanZero("popcount", 1, n1);

      r->value.i = my_popcount64(n1->value.i);
      r->type = V_INTEGER;
      succeed;
#ifdef O_GMP
    case V_MPZ:
      if ( mpz_sgn(n1->value.mpz) < 0 )
	return notLessThanZero("popcount", 1, n1);
      r->value.i = mpz_popcount(n1->value.mpz);
      r->type = V_INTEGER;
      succeed;
#endif
    default:
      assert(0);
      fail;
  }
}



static int
ar_u_minus(Number n1, Number r)
{ r->type = n1->type;

  switch(n1->type)
  { case V_INTEGER:
      if ( n1->value.i == PLMININT )
      {
#ifdef O_GMP
	promoteToMPZNumber(n1);
	r->type = V_MPZ;
#else
	if ( !promoteToFloatNumber(n1) )
	  return FALSE;
	r->type = V_FLOAT;
#endif
	/*FALLTHROUGH*/
      } else
      { r->value.i = -n1->value.i;
	break;
      }
#ifdef O_GMP
    case V_MPZ:
      mpz_init(r->value.mpz);
      mpz_neg(r->value.mpz, n1->value.mpz);
      break;
    case V_MPQ:
      mpq_init(r->value.mpq);
      mpq_neg(r->value.mpq, n1->value.mpq);
      break;
#endif
    case V_FLOAT:
      r->value.f = -n1->value.f;
      r->type = V_FLOAT;
      break;
  }

  succeed;
}


static int
ar_u_plus(Number n1, Number r)
{ cpNumber(r, n1);

  succeed;
}


static int
ar_eval(Number n1, Number r)
{ cpNumber(r, n1);

  succeed;
}


static int
ar_abs(Number n1, Number r)
{ switch(n1->type)
  { case V_INTEGER:
      if ( n1->value.i == PLMININT )
      {
#ifdef O_GMP
	promoteToMPZNumber(n1);
	r->type = V_MPZ;
#else
	if ( !promoteToFloatNumber(n1) )
	  return FALSE;
	r->type = V_FLOAT;
#endif
	/*FALLTHROUGH*/
      } else
      { r->value.i = abs(n1->value.i);
	r->type = V_INTEGER;
	break;
      }
#ifdef O_GMP
    case V_MPZ:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_abs(r->value.mpz, n1->value.mpz);
      break;
    case V_MPQ:
      r->type = V_MPQ;
      mpq_init(r->value.mpq);
      mpq_abs(r->value.mpq, n1->value.mpq);
      break;
#endif
    case V_FLOAT:
    { if ( signbit(n1->value.f) )
	r->value.f = -n1->value.f;
      else
	r->value.f = n1->value.f;
      r->type = V_FLOAT;
      break;
    }
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate argument to rounded integer.  If   the  double  is outside the
PLMININT/PLMAXINT range it is integer  anyway,  so   we  do  not have to
consider rounding for conversion to MPZ.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
ar_integer(Number n1, Number r)
{ switch(n1->type)
  { case V_INTEGER:
#ifdef O_GMP
    case V_MPZ:
#endif
      cpNumber(r, n1);
      succeed;
#ifdef O_GMP
    case V_MPQ:
    { mpq_t q;
      mpq_t half;

      mpq_init(q);
      mpq_init(half);
      mpq_set_ui(half, 1, 2);		/* 1/2 */
      if ( mpq_sgn(n1->value.mpq) > 0 )
	mpq_add(q, n1->value.mpq, half);
      else
	mpq_sub(q, n1->value.mpq, half);

      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_set_q(r->value.mpz, q);
      mpq_clear(q);
      mpq_clear(half);
      succeed;
    }
#endif
    case V_FLOAT:
    { if ( n1->value.f <= PLMAXINT && n1->value.f >= PLMININT )
      { if ( n1->value.f > 0 )
	{ r->value.i = (int64_t)(n1->value.f + 0.5);
	  if ( r->value.i < 0 )		/* Why can this happen? */
	    r->value.i = PLMAXINT;
	} else
	{ r->value.i = (int64_t)(n1->value.f - 0.5);
	  if ( r->value.i > 0 )
	    r->value.i = PLMININT;
	}

	r->type = V_INTEGER;
	succeed;
      }
#ifdef O_GMP
      r->type = V_MPZ;
      mpz_init_set_d(r->value.mpz, n1->value.f);
      succeed;
#else
#ifdef HAVE_RINT
      r->value.f = rint(n1->value.f);
      r->type = V_FLOAT;
      succeed;
#else
      return PL_error("integer", 1, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
#endif
    }
  }

  assert(0);
  fail;
}


static int
ar_float(Number n1, Number r)
{ cpNumber(r, n1);

  return promoteToFloatNumber(r);
}


static int				/* ISO Prolog: R --> Z */
ar_floor(Number n1, Number r)
{ switch(n1->type)
  { case V_INTEGER:
      cpNumber(r, n1);
      succeed;
#ifdef O_GMP
    case V_MPZ:
      cpNumber(r, n1);
      succeed;
    case V_MPQ:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_set_q(r->value.mpz, n1->value.mpq);
      if ( mpq_sgn(n1->value.mpq) < 0 &&
	   mpz_cmp_si(mpq_denref(n1->value.mpq), 1L) != 0 )
	mpz_sub_ui(r->value.mpz, r->value.mpz, 1L);
      succeed;
#endif
    case V_FLOAT:
    {
#ifdef HAVE_FLOOR
      r->type = V_FLOAT;
      r->value.f = floor(n1->value.f);
      if ( !toIntegerNumber(r, TOINT_CONVERT_FLOAT|TOINT_TRUNCATE) )
      { return PL_error("floor", 1, NULL, ERR_EVALUATION, ATOM_int_overflow);
      }
#else /*HAVE_FLOOR*/
      if ( n1->value.f > (double)PLMININT && n1->value.f < (double)PLMAXINT )
      { r->value.i = (int64_t)n1->value.f;
	if ( n1->value.f < 0 && (double)r->value.i > n1->value.f )
	  r->value.i--;
	r->type = V_INTEGER;
      } else
      {
#ifdef O_GMP
	r->type = V_MPZ;
	mpz_init_set_d(r->value.mpz, n1->value.f);
	if ( n1->value.f < 0 &&
	     mpz_get_d(r->value.mpz) > n1->value.f )
	  mpz_sub_ui(r->value.mpz, r->value.mpz, 1L);
#else
	return PL_error("floor", 1, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
      }
#endif /*HAVE_FLOOR*/
    }
  }

  succeed;
}


static int				/* ISO Prolog: R --> Z */
ar_ceil(Number n1, Number r)
{ switch(n1->type)
  { case V_INTEGER:
      cpNumber(r, n1);
      succeed;
#ifdef O_GMP
    case V_MPZ:
      cpNumber(r, n1);
      succeed;
    case V_MPQ:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_set_q(r->value.mpz, n1->value.mpq);
      if ( mpq_sgn(n1->value.mpq) > 0 &&
	   mpz_cmp_si(mpq_denref(n1->value.mpq), 1L) != 0 )
	mpz_add_ui(r->value.mpz, r->value.mpz, 1L);
      succeed;
#endif
    case V_FLOAT:
    {
#ifdef HAVE_CEIL
       r->type = V_FLOAT;
       r->value.f = ceil(n1->value.f);
       if ( !toIntegerNumber(r, TOINT_CONVERT_FLOAT|TOINT_TRUNCATE) )
       { return PL_error("ceil", 1, NULL, ERR_EVALUATION, ATOM_int_overflow);
       }
#else /*HAVE_CEIL*/
       if ( n1->value.f > (double)PLMININT && n1->value.f < (double)PLMAXINT )
       { r->value.i = (int64_t)n1->value.f;
	 if ( (double)r->value.i < n1->value.f )
	   r->value.i++;
	 r->type = V_INTEGER;
       } else
       {
#ifdef O_GMP
         r->type = V_MPZ;
	 mpz_init_set_d(r->value.mpz, n1->value.f);
	 if ( mpz_get_d(r->value.mpz) < n1->value.f )
	   mpz_add_ui(r->value.mpz, r->value.mpz, 1L);
#else
         return PL_error("ceil", 1, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
       }
#endif /*HAVE_CEIL*/
    }
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
X is float_integer_part(X) + float_fractional_part(X)

If X < 0, both float_integer_part(X) and float_integer_part(X) are <= 0
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
ar_float_fractional_part(Number n1, Number r)
{ switch(n1->type)
  { case V_INTEGER:
#ifdef O_GMP
    case V_MPZ:
#endif
      r->value.i = 0;
      r->type = V_INTEGER;
      succeed;
#ifdef O_GMP
    case V_MPQ:
      r->type = V_MPQ;
      mpq_init(r->value.mpq);
      mpz_tdiv_q(mpq_numref(r->value.mpq),
		 mpq_numref(n1->value.mpq),
		 mpq_denref(n1->value.mpq));
      mpz_set_ui(mpq_denref(r->value.mpq), 1);
      mpq_sub(r->value.mpq, n1->value.mpq, r->value.mpq);
      succeed;
#endif
    case V_FLOAT:
    { double ip;

      r->value.f = modf(n1->value.f, &ip);
      r->type = V_FLOAT;
    }
  }

  succeed;
}


static int
ar_float_integer_part(Number n1, Number r)
{ switch(n1->type)
  { case V_INTEGER:
#ifdef O_GMP
    case V_MPZ:
#endif
      cpNumber(r, n1);
      succeed;
#ifdef O_GMP
    case V_MPQ:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_tdiv_q(r->value.mpz,
		 mpq_numref(n1->value.mpq),
		 mpq_denref(n1->value.mpq));
      succeed;
#endif
    case V_FLOAT:
    { double ip;

      (void)modf(n1->value.f, &ip);
      r->value.f = ip;
      r->type = V_FLOAT;
      succeed;
    }
  }

  assert(0);
  fail;
}


static int
ar_truncate(Number n1, Number r)
{ switch(n1->type)
  {
#ifdef O_GMP
    case V_MPQ:
      if ( mpq_sgn(n1->value.mpq) >= 0 )
	return ar_floor(n1, r);
      else
	return ar_ceil(n1, r);
#endif
    case V_FLOAT:
      if ( n1->value.f >= 0.0 )
	return ar_floor(n1, r);
      else
	return ar_ceil(n1, r);
    default:
      cpNumber(r, n1);
      succeed;
  }
}


#ifdef O_GMP
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include <fcntl.h>

#define RAND_SEED_LEN 128
#define MIN_RAND_SEED_LEN 16

static int
seed_from_dev(const char *dev ARG_LD)
{ int done = FALSE;
#if defined(S_ISCHR) && !defined(__WINDOWS__)
  int fd;

  if ( (fd=open(dev, O_RDONLY)) )
  { struct stat buf;

    if ( fstat(fd, &buf) == 0 && S_ISCHR(buf.st_mode) )
    { char seedarray[RAND_SEED_LEN];
      mpz_t seed;
      size_t rd = 0;
      ssize_t n;

      while ( rd < MIN_RAND_SEED_LEN )
      { if ( (n=read(fd, seedarray+rd, sizeof(seedarray)-rd)) > 0 )
	  rd += n;
	else
	  break;
      }

      if ( rd >= MIN_RAND_SEED_LEN )
      { DEBUG(1, Sdprintf("Seed random using %ld bytes from %s\n",
			  (long)rd, dev));

	LD->gmp.persistent++;
	mpz_init(seed);
	mpz_import(seed, rd, 1, sizeof(char), 0, 0, seedarray);
	gmp_randseed(LD->arith.random.state, seed);
	mpz_clear(seed);
	LD->gmp.persistent--;

	done = TRUE;
      }
    }

    close(fd);
  }
#endif /*S_ISCHR*/

  return done;
}



static int
seed_from_crypt_context(ARG1_LD)
{
#ifdef __WINDOWS__
  HCRYPTPROV hCryptProv;
  char *user_name = "seed_random";
  BYTE seedarray[RAND_SEED_LEN];
  mpz_t seed;


  if ( CryptAcquireContext(&hCryptProv, user_name, NULL, PROV_RSA_FULL, 0) )
  { CryptGenRandom(hCryptProv, sizeof(seedarray), seedarray);
  } else if ( (GetLastError() == NTE_BAD_KEYSET) &&
	      CryptAcquireContext(&hCryptProv, user_name, NULL,
				  PROV_RSA_FULL, CRYPT_NEWKEYSET) )
  { CryptGenRandom(hCryptProv, sizeof(seedarray), seedarray);
  } else
  { return FALSE;
  }

  LD->gmp.persistent++;
  mpz_init(seed);
  mpz_import(seed, RAND_SEED_LEN, 1, sizeof(BYTE), 0, 0, seedarray);
  gmp_randseed(LD->arith.random.state, seed);
  mpz_clear(seed);
  LD->gmp.persistent--;

  return TRUE;
#else
#ifdef O_PLMT
  (void)__PL_ld;
#endif
  return FALSE;
#endif
}


static void
seed_random(ARG1_LD)
{ if ( !seed_from_dev("/dev/urandom" PASS_LD) &&
       !seed_from_dev("/dev/random" PASS_LD) &&
       !seed_from_crypt_context(PASS_LD1) )
  { double t[1] = { WallTime() };
    unsigned long key = 0;
    unsigned long *p = (unsigned long*)t;
    unsigned long *e = (unsigned long*)&t[1];

    for(; p<e; p++)
      key ^= *p;

    LD->gmp.persistent++;
    gmp_randseed_ui(LD->arith.random.state, key);
    LD->gmp.persistent--;
  }
}

#else /* O_GMP */

static void
seed_random(ARG1_LD)
{ setRandom(NULL);
}

#endif /*O_GMP*/

static void
init_random(ARG1_LD)
{
#ifdef O_GMP
  if ( !LD->arith.random.initialised )
  { LD->gmp.persistent++;
#ifdef HAVE_GMP_RANDINIT_MT
#define O_RANDOM_STATE 1
    gmp_randinit_mt(LD->arith.random.state);
#else
    gmp_randinit_default(LD->arith.random.state);
#endif
    LD->arith.random.initialised = TRUE;
    seed_random(PASS_LD1);
    LD->gmp.persistent--;
  }
#endif
}


static
PRED_IMPL("set_random", 1, set_random, 0)
{ PRED_LD
  atom_t name;
  int arity;

  init_random(PASS_LD1);

  if ( PL_get_name_arity(A1, &name, &arity) && arity == 1 )
  { term_t arg = PL_new_term_ref();

    _PL_get_arg(1, A1, arg);
    if ( name == ATOM_seed )
    { atom_t a;

      if ( PL_get_atom(arg, &a) && a == ATOM_random )
      { seed_random(PASS_LD1);
	return TRUE;
      } else
      { number n;

	if ( !PL_get_number(arg, &n) )
	  return PL_error(NULL, 0, "integer or 'random'",
			  ERR_TYPE, ATOM_seed, arg);
	switch(n.type)
	{
#ifdef O_GMP
	  case V_INTEGER:
	    gmp_randseed_ui(LD->arith.random.state,
			    (unsigned long)n.value.i);
	    return TRUE;
	  case V_MPZ:
	    gmp_randseed(LD->arith.random.state, n.value.mpz);
	    return TRUE;
#else
	  case V_INTEGER:
          { unsigned int seed = (unsigned int)n.value.i;
	    setRandom(&seed);
	    return TRUE;
	  }
#endif
	  default:
	    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_seed, arg);
	}
      }
#ifdef O_RANDOM_STATE
    } else if ( name == ATOM_state )
    { number n;

      if ( !PL_get_number(arg, &n) ||
	   n.type != V_MPZ )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_state, arg);

      mpz_set(LD->arith.random.state[0]._mp_seed, n.value.mpz);
      clearNumber(&n);

      return TRUE;
#endif /*O_GMP*/
    } else
    { return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_random_option, A1);
    }
  } else
  { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_random_option, A1);
  }
}


#ifdef O_RANDOM_STATE
static
PRED_IMPL("random_property", 1, random_property, 0)
{ PRED_LD
  atom_t name;
  int arity;

  init_random(PASS_LD1);

  if ( PL_get_name_arity(A1, &name, &arity) && arity == 1 )
  { term_t arg = PL_new_term_ref();

    _PL_get_arg(1, A1, arg);
    if ( name == ATOM_state )
    { int rc;
      number seed;

      seed.type = V_MPZ;
      mpz_init(seed.value.mpz);
      LD->arith.random.state[0]._mp_seed[0]._mp_size =
      LD->arith.random.state[0]._mp_seed[0]._mp_alloc;					      mpz_set(seed.value.mpz, LD->arith.random.state[0]._mp_seed);
      rc = PL_unify_number(arg, &seed);
      clearNumber(&seed);

      return rc;
    }
  }

  return FALSE;
}
#endif


static int
ar_random(Number n1, Number r)
{ GET_LD

  if ( !toIntegerNumber(n1, TOINT_CONVERT_FLOAT) )
    return PL_error("random", 1, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( ar_sign_i(n1) <= 0 )
    return mustBePositive("random", 1, n1);

  init_random(PASS_LD1);

  switch(n1->type)
  {
#ifdef O_GMP
    case V_INTEGER:
      promoteToMPZNumber(n1);
      assert(n1->type == V_MPZ);
      /*FALLTHROUGH*/
    case V_MPZ:
    { r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_urandomm(r->value.mpz, LD->arith.random.state, n1->value.mpz);

      succeed;
    }
#else
    case V_INTEGER:
      if ( n1->value.i < 1 )
	return mustBePositive("random", 1, n1);
      r->value.i = _PL_Random() % (uint64_t)n1->value.i;
      r->type = V_INTEGER;

      succeed;
#endif
    default:
      assert(0);
      fail;
  }
}

#ifndef UINT64_MAX
#define UINT64_MAX (~(uint64_t)0)
#endif

static int
ar_random_float(Number r)
{ GET_LD

  init_random(PASS_LD1);

  do
  {
#ifdef O_GMP
    mpf_t rop;
    mpf_init2(rop, sizeof(double)*8);
    mpf_urandomb(rop, LD->arith.random.state, sizeof(double)*8);
    r->value.f = mpf_get_d(rop);
    mpf_clear(rop);
#else
  r->value.f = _PL_Random()/(float)UINT64_MAX;
#endif
  } while (r->value.f == 0.0);

  r->type = V_FLOAT;
  succeed;
}


static int
ar_pi(Number r)
{ r->value.f = M_PI;

  r->type = V_FLOAT;
  succeed;
}


static int
ar_e(Number r)
{ r->value.f = M_E;

  r->type = V_FLOAT;
  succeed;
}


static int
ar_epsilon(Number r)
{ r->value.f = DBL_EPSILON;

  r->type = V_FLOAT;
  succeed;
}


static int
ar_cputime(Number r)
{ r->value.f = CpuTime(CPU_USER);

  r->type = V_FLOAT;
  succeed;
}


		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

static
PRED_IMPL("is", 2, is, PL_FA_ISO)	/* -Value is +Expr */
{ PRED_LD
  AR_CTX
  number arg;
  int rc;

  AR_BEGIN();

  if ( (rc=valueExpression(A2, &arg PASS_LD)) )
  { rc = PL_unify_number(A1, &arg);
    clearNumber(&arg);
  }

  AR_END();

  return rc;
}


/** current_arithmetic_function(?Term) is nondet.

True if Term is evaluable.
*/

static
PRED_IMPL("current_arithmetic_function", 1, current_arithmetic_function,
	  PL_FA_NONDETERMINISTIC)
{ PRED_LD
  unsigned int i;
  term_t head = A1;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { functor_t fd;

      if ( PL_is_variable(head) )
      { i = 0;
        break;
      } else if ( PL_get_functor(head, &fd) )
      {	return isCurrentArithFunction(fd) ? TRUE : FALSE;
      } else
        return PL_error(NULL, 0, NULL,
			ERR_TYPE, ATOM_callable, head);
    }
    case FRG_REDO:
      i = (int)CTX_INT;
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(; i<GD->arith.functions_allocated; i++)
  { if ( GD->arith.functions[i] )
    { functor_t f = functorArithFunction(i);

      if ( PL_unify_functor(head, f) )
	ForeignRedoInt(i+1);
    }
  }

  fail;
}


typedef struct
{ functor_t	functor;
  ArithF	function;
} ar_funcdef;

#define F_ISO 0x1
#define ADD(functor, func, flags) { functor, func }

static const ar_funcdef ar_funcdefs[] = {
  ADD(FUNCTOR_plus2,		pl_ar_add, F_ISO),
  ADD(FUNCTOR_minus2,		ar_minus, F_ISO),
  ADD(FUNCTOR_star2,		ar_mul, F_ISO),
  ADD(FUNCTOR_divide2,		ar_divide, F_ISO),
#ifdef O_GMP
  ADD(FUNCTOR_rational1,	ar_rational, 0),
  ADD(FUNCTOR_rationalize1,	ar_rationalize, 0),
  ADD(FUNCTOR_rdiv2,		ar_rdiv, 0),
#endif
  ADD(FUNCTOR_minus1,		ar_u_minus, F_ISO),
  ADD(FUNCTOR_plus1,		ar_u_plus, F_ISO),
  ADD(FUNCTOR_abs1,		ar_abs, F_ISO),
  ADD(FUNCTOR_max2,		ar_max, F_ISO),
  ADD(FUNCTOR_min2,		ar_min, F_ISO),

  ADD(FUNCTOR_mod2,		ar_mod, F_ISO),
  ADD(FUNCTOR_rem2,		ar_rem, F_ISO),
  ADD(FUNCTOR_div2,		ar_div, F_ISO),
  ADD(FUNCTOR_gdiv2,		ar_tdiv, 0),
  ADD(FUNCTOR_gcd2,		ar_gcd, 0),
  ADD(FUNCTOR_sign1,		ar_sign, F_ISO),

  ADD(FUNCTOR_and2,		ar_conjunct, F_ISO),
  ADD(FUNCTOR_bitor2,		ar_disjunct, F_ISO),
  ADD(FUNCTOR_rshift2,		ar_shift_right, F_ISO),
  ADD(FUNCTOR_lshift2,		ar_shift_left, F_ISO),
  ADD(FUNCTOR_xor2,		ar_xor, F_ISO),
  ADD(FUNCTOR_backslash1,	ar_negation, F_ISO),

  ADD(FUNCTOR_random1,		ar_random, 0),
#ifdef O_GMP
  ADD(FUNCTOR_random_float0,	ar_random_float, 0),
#endif

  ADD(FUNCTOR_integer1,		ar_integer, F_ISO),
  ADD(FUNCTOR_round1,		ar_integer, F_ISO),
  ADD(FUNCTOR_truncate1,	ar_truncate, F_ISO),
  ADD(FUNCTOR_float1,		ar_float, F_ISO),
  ADD(FUNCTOR_floor1,		ar_floor, F_ISO),
  ADD(FUNCTOR_ceil1,		ar_ceil, F_ISO),
  ADD(FUNCTOR_ceiling1,		ar_ceil, F_ISO),
  ADD(FUNCTOR_float_fractional_part1, ar_float_fractional_part, F_ISO),
  ADD(FUNCTOR_float_integer_part1, ar_float_integer_part, F_ISO),
  ADD(FUNCTOR_copysign2,	ar_copysign, 0),

  ADD(FUNCTOR_sqrt1,		ar_sqrt, F_ISO),
  ADD(FUNCTOR_sin1,		ar_sin, F_ISO),
  ADD(FUNCTOR_cos1,		ar_cos, F_ISO),
  ADD(FUNCTOR_tan1,		ar_tan, F_ISO),
  ADD(FUNCTOR_asin1,		ar_asin, F_ISO),
  ADD(FUNCTOR_acos1,		ar_acos, F_ISO),
  ADD(FUNCTOR_atan1,		ar_atan, F_ISO),
  ADD(FUNCTOR_atan2,		ar_atan2, 0),
  ADD(FUNCTOR_atan22,		ar_atan2, F_ISO),
  ADD(FUNCTOR_log1,		ar_log, F_ISO),
  ADD(FUNCTOR_exp1,		ar_exp, F_ISO),
  ADD(FUNCTOR_log101,		ar_log10, 0),
  ADD(FUNCTOR_hat2,		ar_pow, F_ISO),
  ADD(FUNCTOR_doublestar2,	ar_pow, F_ISO),
  ADD(FUNCTOR_pi0,		ar_pi, F_ISO),
  ADD(FUNCTOR_e0,		ar_e, 0),
  ADD(FUNCTOR_epsilon0,		ar_epsilon, 0),

  ADD(FUNCTOR_cputime0,		ar_cputime, 0),
  ADD(FUNCTOR_msb1,		ar_msb, 0),
  ADD(FUNCTOR_lsb1,		ar_lsb, 0),
  ADD(FUNCTOR_popcount1,	ar_popcount, 0),
  ADD(FUNCTOR_powm3,		ar_powm, 0),

  ADD(FUNCTOR_eval1,		ar_eval, 0)
};

#undef ADD

static size_t
registerFunction(functor_t f, ArithF func)
{ size_t index = indexFunctor(f);

  DEBUG(1, Sdprintf("Register functor %ld\n", (long)index));

  while ( index >= GD->arith.functions_allocated )
  { if ( GD->arith.functions_allocated == 0 )
    { size_t size = 256;

      GD->arith.functions = allocHeapOrHalt(size*sizeof(ArithF));
      memset(GD->arith.functions, 0, size*sizeof(ArithF));
      GD->arith.functions_allocated = size;
    } else
    { size_t size = GD->arith.functions_allocated*2;
      ArithF *new = allocHeapOrHalt(size*sizeof(ArithF));
      size_t half = GD->arith.functions_allocated*sizeof(ArithF);
      ArithF *old = GD->arith.functions;

      DEBUG(0, Sdprintf("Re-sized function-table to %ld\n", (long)size));

      memcpy(new, old, half);
      memset(addPointer(new,half), 0, half);
      GD->arith.functions = new;
      GD->arith.functions_allocated = size;
      freeHeap(old, half);
    }
  }

  GD->arith.functions[index] = func;

  return index;
}


static void
registerBuiltinFunctions()
{ int n, size = sizeof(ar_funcdefs)/sizeof(ar_funcdef);
  const ar_funcdef *d;

  for(d = ar_funcdefs, n=0; n<size; n++, d++)
    registerFunction(d->functor, d->function);
}


void
initArith(void)
{ registerBuiltinFunctions();

#ifdef O_INHIBIT_FP_SIGNALS
  fpsetmask(fpgetmask() & ~(FP_X_DZ|FP_X_INV|FP_X_OFL));
#endif
}


void
cleanupArith(void)
{
#ifdef O_INHIBIT_FP_SIGNALS
  fpresetsticky(FP_X_DZ|FP_X_INV|FP_X_OFL);
  fpsetmask(FP_X_DZ|FP_X_INV|FP_X_OFL);
#endif

  if ( GD->arith.functions )
  { freeHeap(GD->arith.functions, GD->arith.functions_allocated*sizeof(ArithF));
    GD->arith.functions = 0;
    GD->arith.functions_allocated = 0;
  }
}


#if O_COMPILE_ARITH

		/********************************
		*    VIRTUAL MACHINE SUPPORT    *
		*********************************/

int
indexArithFunction(functor_t f)
{ size_t index = indexFunctor(f);

  if ( index < GD->arith.functions_allocated )
  { if ( GD->arith.functions[index] )
      return (int)index;
  }

  return -1;
}


functor_t
functorArithFunction(unsigned int i)
{ FunctorDef fd = fetchFunctorArray(i);

  return fd->functor;
}


static ArithF
FunctionFromIndex(int index)
{ return GD->arith.functions[index];
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ar_func_n(code,  argc)  is  executed  by  the  A_FUNC*  virtual  machine
instructions. It invalidates all numbers it   pops  from the stack using
clearNumber()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
ar_func_n(int findex, int argc ARG_LD)
{ number result;
  int rval;
  ArithF f = FunctionFromIndex(findex);
  Number argv = argvArithStack(argc PASS_LD);

  DEBUG(0, if ( !f )
	     fatalError("No function at index %d", findex));

  switch(argc)
  { case 0:
      rval = (*f)(&result);
      break;
    case 1:
      rval = (*f)(argv, &result);
      break;
    case 2:
      rval = (*f)(argv, argv+1, &result);
      break;
    case 3:
      rval = (*f)(argv, argv+1, argv+2, &result);
      break;
    default:
      rval = FALSE;
      sysError("Too many arguments to arithmetic function");
  }

  popArgvArithStack(argc PASS_LD);

  if ( rval )
    pushArithStack(&result PASS_LD);

  return rval;
}

#endif /* O_COMPILE_ARITH */


		 /*******************************
		 *	  MISC INTERFACE	*
		 *******************************/

/* Evaluate a term to a 64-bit integer.  Term is of type
*/

int
PL_eval_expression_to_int64_ex(term_t t, int64_t *val)
{ GET_LD
  number n;
  int rval;

  if ( valueExpression(t, &n PASS_LD) )
  { if ( toIntegerNumber(&n, 0) )
    { switch(n.type)
      { case V_INTEGER:
	  *val = n.value.i;
	  rval = TRUE;
	  break;
#ifdef O_GMP
	case V_MPZ:
	{ if ( !(rval=mpz_to_int64(n.value.mpz, val)) )
	    rval = PL_error(NULL, 0, NULL, ERR_EVALUATION, ATOM_int_overflow);
	  break;
	}
#endif
	default:
	  assert(0);
      }
    } else
    { rval = PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer_expression, t);
    }

    clearNumber(&n);
  } else
  { rval = FALSE;
  }

  return rval;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(arith)
  PRED_DEF("is",   2, is,  PL_FA_ISO)
  PRED_DEF("<",	   2, lt,  PL_FA_ISO)
  PRED_DEF(">",	   2, gt,  PL_FA_ISO)
  PRED_DEF("=<",   2, leq, PL_FA_ISO)
  PRED_DEF(">=",   2, geq, PL_FA_ISO)
  PRED_DEF("=\\=", 2, neq, PL_FA_ISO)
  PRED_DEF("=:=",  2, eq,  PL_FA_ISO)
  PRED_DEF("current_arithmetic_function", 1, current_arithmetic_function,
	   PL_FA_NONDETERMINISTIC)
  PRED_DEF("succ", 2, succ, 0)
  PRED_DEF("plus", 3, plus, 0)
  PRED_DEF("between", 3, between, PL_FA_NONDETERMINISTIC)
  PRED_DEF("set_random", 1, set_random, 0)
#ifdef O_RANDOM_STATE
  PRED_DEF("random_property", 1, random_property, 0)
#endif
EndPredDefs
