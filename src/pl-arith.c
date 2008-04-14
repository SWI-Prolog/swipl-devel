/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

The  current  version  of  this  module  also  supports  Prolog  defined
arithmetic  functions.   In  the  current  version these can only return
numbers.  This should be changed to return arbitrary Prolog  terms  some
day.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-incl.h"
#undef abs			/* avoid abs() problem with MSVC++ */
#include <math.h>
#undef abs
#define abs(a) ((a) < 0 ? -(a) : (a))
#include <limits.h>
#ifdef HAVE_FLOAT_H
#include <float.h>
#ifdef __WINDOWS__
#define isnan(x) _isnan(x)
#endif
#endif
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

#ifdef fpclassify
#define HAVE_FPCLASSIFY 1
#endif

#undef LD
#define LD LOCAL_LD

#ifndef M_PI
#define M_PI (3.14159265358979323846)
#endif
#ifndef M_E
#define M_E (2.7182818284590452354)
#endif

#ifdef __WINDOWS__
#define LL(x) x ## i64
#else
#define LL(x) x ## LL
#endif

#ifdef HAVE___TRY
#include <excpt.h>
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

typedef int (*ArithF)();

struct arithFunction
{ ArithFunction next;		/* Next of chain */
  functor_t	functor;	/* Functor defined */
  ArithF	function;	/* Implementing function */
  Module	module;		/* Module visibility module */
  int		level;		/* Level of the module */
#if O_PROLOG_FUNCTIONS
  Procedure	proc;		/* Prolog defined functions */
#endif
#if O_COMPILE_ARITH
  code		index;		/* Index of function */
#endif
};

#define arithFunctionTable	(GD->arith.table)
#define function_array		(&GD->arith.functions)
#define FunctionFromIndex(n)	fetchBuffer(function_array, n, ArithFunction)

static ArithFunction	isCurrentArithFunction(functor_t, Module);
static int		registerFunction(ArithFunction f, int index);
static int		getCharExpression(term_t t, Number r ARG_LD);
static int		ar_add(Number n1, Number n2, Number r);
static int		ar_add_ui(Number n, long val);
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


typedef struct
{ number low;
  number high;
  int hinf;
} between_state;


static
PRED_IMPL("between", 3, between, PL_FA_NONDETERMINISTIC)
{ GET_LD
  between_state *state;
  term_t low = A1;
  term_t high = A2;
  term_t n = A3;

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

	PL_unify(n, low);
	if ( hinf == FALSE && cmpNumbers(&l, &h) == 0 )
	{ clearInteger(&l);
	  clearInteger(&h);
	  succeed;
	}

	state = allocHeap(sizeof(*state));
	cpNumber(&state->low, &l);
	cpNumber(&state->high, &h);
	state->hinf = hinf;
	clearInteger(&l);
	clearInteger(&h);
	ForeignRedoPtr(state);
      }
    case FRG_REDO:
      { state = CTX_PTR;

	ar_add_ui(&state->low, 1L);
	PL_unify_number(n, &state->low);
	if ( !state->hinf &&
	     cmpNumbers(&state->low, &state->high) == 0 )
	  goto cleanup;
	ForeignRedoPtr(state);
      }
    case FRG_CUTTED:
      { state = CTX_PTR;
      cleanup:
	clearInteger(&state->low);
	clearInteger(&state->high);
	freeHeap(state, sizeof(*state));
      }
    default:;
      succeed;
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
    ar_add(&i1, &one, &i2);
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
      ar_add(&m, &n, &o);
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

int
ar_compare(Number n1, Number n2, int what)
{ same_type_numbers(n1, n2);

  switch(n1->type)
  { case V_INTEGER:
      switch(what)
      { case LT: return n1->value.i <  n2->value.i; break;
	case GT: return n1->value.i >  n2->value.i; break;
	case LE: return n1->value.i <= n2->value.i; break;
	case GE: return n1->value.i >= n2->value.i; break;
	case NE: return n1->value.i != n2->value.i; break;
	case EQ: return n1->value.i == n2->value.i; break;
      }
      break;
#ifdef O_GMP
    case V_MPZ:
    { int rc = mpz_cmp(n1->value.mpz, n2->value.mpz);

      switch(what)
      { case LT: return rc <  0; break;
	case GT: return rc >  0; break;
	case LE: return rc <= 0; break;
	case GE: return rc >= 0; break;
	case NE: return rc != 0; break;
	case EQ: return rc == 0; break;
      }
      break;
    }
    case V_MPQ:
    { int rc = mpq_cmp(n1->value.mpq, n2->value.mpq);

      switch(what)
      { case LT: return rc <  0; break;
	case GT: return rc >  0; break;
	case LE: return rc <= 0; break;
	case GE: return rc >= 0; break;
	case NE: return rc != 0; break;
	case EQ: return rc == 0; break;
      }
      break;
    }
#endif
    case V_REAL:
      switch(what)
      { case LT: return n1->value.f <  n2->value.f; break;
	case GT: return n1->value.f >  n2->value.f; break;
	case LE: return n1->value.f <= n2->value.f; break;
	case GE: return n1->value.f >= n2->value.f; break;
	case NE: return n1->value.f != n2->value.f; break;
	case EQ: return n1->value.f == n2->value.f; break;
      }
      break;
  }  

  assert(0);
  fail;
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
PRED_IMPL("<", 2, lt, PL_FA_TRANSPARENT)
{ PRED_LD
  return compareNumbers(A1, A2, LT PASS_LD);
}

static
PRED_IMPL(">", 2, gt, PL_FA_TRANSPARENT)
{ PRED_LD
  return compareNumbers(A1, A2, GT PASS_LD);
}

static
PRED_IMPL("=<", 2, leq, PL_FA_TRANSPARENT)
{ PRED_LD
  return compareNumbers(A1, A2, LE PASS_LD);
}

static
PRED_IMPL(">=", 2, geq, PL_FA_TRANSPARENT)
{ PRED_LD
  return compareNumbers(A1, A2, GE PASS_LD);
}

static
PRED_IMPL("=\\=", 2, neq, PL_FA_TRANSPARENT)
{ PRED_LD
  return compareNumbers(A1, A2, NE PASS_LD);
}

static
PRED_IMPL("=:=", 2, eq, PL_FA_TRANSPARENT)
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


		/********************************
		*           FUNCTIONS           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
isCurrentArithFunction(functor_t f, Module m)
    Find existing arithmetic function definition for f using m as
    context.  

    The one we are looking for is the function that is in the most
    local module.  As the entries are sorted such that more specific
    functions are before global functions, we can pick the first
    one that is in our module path.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static ArithFunction
isCurrentArithFunction(functor_t f, Module m)
{ ArithFunction a;

  for(a = arithFunctionTable[functorHashValue(f, ARITHHASHSIZE)];
      !isTableRef(a) && a; a = a->next)
  { if ( a->functor == f )
    { if ( isSuperModule(a->module, m) )
	return a;
    }
  }

  return NULL;
}

#if O_PROLOG_FUNCTIONS

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Activating a Prolog predicate as function below the arithmetic functions
is/0, >, etc. `f' is the arithmetic function   to  be called. `t' is the
base term-reference of an array holding  the proper number of arguments.
`r' is the result of the evaluation.

This calling convention is somewhat  unnatural,   but  fits  best in the
calling convention required by ar_func_n() below.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
prologFunction(ArithFunction f, term_t av, Number r ARG_LD)
{ Definition def = getProcDefinition(f->proc);
  int arity = def->functor->arity;
  fid_t fid = PL_open_foreign_frame();
  qid_t qid;
  int rval;

  qid = PL_open_query(NULL, PL_Q_PASS_EXCEPTION, f->proc, av);

  if ( PL_next_solution(qid) )
  { rval = valueExpression(av+arity-1, r PASS_LD);
    PL_close_query(qid);
    PL_discard_foreign_frame(fid);
  } else
  { term_t except;

    if ( (except = PL_exception(qid)) )
    { rval = FALSE;
      PL_close_query(qid);
    } else
    { PL_close_query(qid);

#ifdef O_LIMIT_DEPTH
      if ( depth_reached > depth_limit )
	rval = FALSE;
      else
#endif
      { term_t goal = PL_new_term_ref();
	PL_cons_functor_v(goal, def->functor->functor, av);
	
	rval = PL_error(NULL, 0,
			"Aritmetic function must succeed or throw exception",
			ERR_FAILED, goal);
      }
    }

    PL_close_foreign_frame(fid);
  }

  return rval;
}

#endif /* O_PROLOG_FUNCTIONS */

static int
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


int
eval_expression(term_t t, Number r, int recursion ARG_LD)
{ ArithFunction f;
  functor_t functor;
  Word p = valTermRef(t);
  word w;

  deRef(p);
  w = *p;

  switch(tag(w))
  { case TAG_INTEGER:
      get_integer(w, r);
      succeed;
    case TAG_FLOAT:
      r->value.f = valReal(w);
      r->type = V_REAL;
      succeed;
    case TAG_VAR:
      return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    case TAG_ATOM:
      functor = lookupFunctorDef(w, 0);
      break;
    case TAG_COMPOUND:
      functor = functorTerm(w);
      break;
    default:
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_number, t);
  }

  if ( !(f = isCurrentArithFunction(functor,
				    contextModule(environment_frame))))
  { if ( functor == FUNCTOR_dot2 )	/* handle "a" (make function) */
      return getCharExpression(t, r PASS_LD);
    else
      return PL_error(NULL, 0, NULL, ERR_NOT_EVALUABLE, functor);
  }

  if ( recursion > 100 && !PL_is_acyclic(t) )
    return PL_error(NULL, 0, "cyclic term", ERR_TYPE, ATOM_expression, t);

#if O_PROLOG_FUNCTIONS
  if ( f->proc )
  { int rval, n, arity = arityFunctor(functor);
    fid_t fid = PL_open_foreign_frame();
    term_t h0 = PL_new_term_refs(arity+1); /* one extra for the result */

    for(n=0; n<arity; n++)
    { number n1;

      _PL_get_arg(n+1, t, h0+n);
      if ( eval_expression(h0+n, &n1, recursion+1 PASS_LD) )
      { _PL_put_number(h0+n, &n1);
	clearNumber(&n1);
      } else
      { PL_close_foreign_frame(fid);
	fail;
      }
    }

    rval = prologFunction(f, h0, r PASS_LD);
    PL_close_foreign_frame(fid);

    return rval;
  }
#endif

  DEBUG(3, Sdprintf("Starting __try ...\n"));

  { int rval;

#ifdef HAVE___TRY
#ifndef EXCEPTION_EXECUTE_HANDLER	/* lcc */
#define EXCEPTION_EXECUTE_HANDLER 1
#endif
    __try
    {
#else
    LD->in_arithmetic++;
#endif
    switch(arityFunctor(functor))
    { case 0:
	rval = (*f->function)(r);
        break;
      case 1:	
      { term_t a = PL_new_term_ref();
	number n1;

	_PL_get_arg(1, t, a);
	if ( eval_expression(a, &n1, recursion+1 PASS_LD) )
	{ rval = (*f->function)(&n1, r);
	  clearNumber(&n1);
	} else
	  rval = FALSE;

	PL_reset_term_refs(a);
	break;
      }
      case 2:
      { term_t a = PL_new_term_ref();
	number n1, n2;

	_PL_get_arg(1, t, a);
	if ( eval_expression(a, &n1, recursion+1 PASS_LD) )
	{ _PL_get_arg(2, t, a);
	  if ( eval_expression(a, &n2, recursion+1 PASS_LD) )
	  { rval = (*f->function)(&n1, &n2, r);
	    clearNumber(&n2);
	  } else
	  { rval = FALSE;
	  }
	  clearNumber(&n1);
	} else
	  rval = FALSE;

	PL_reset_term_refs(a);
	break;
      }
      case 3:
      { term_t a = PL_new_term_ref();
	number n1, n2, n3;

	_PL_get_arg(1, t, a);
	if ( eval_expression(a, &n1, recursion+1 PASS_LD) )
	{ _PL_get_arg(2, t, a);
	  if ( eval_expression(a, &n2, recursion+1 PASS_LD) )
	  { _PL_get_arg(3, t, a);
	    if ( eval_expression(a, &n3, recursion+1 PASS_LD) )
	    { rval = (*f->function)(&n1, &n2, &n3, r);
	      clearNumber(&n3);
	    } else
	    { rval = FALSE;
	    }
	    clearNumber(&n2);
	  } else
	  { rval = FALSE;
	  }
	  clearNumber(&n1);
	} else
	  rval = FALSE;

	PL_reset_term_refs(a);
	break;
      }
      default:
	sysError("Illegal arity for arithmic function");
        rval = FALSE;
    }
#if defined(HAVE___TRY)
    } __except(EXCEPTION_EXECUTE_HANDLER)
    { warning("Floating point exception");
#ifndef O_RUNTIME
      Sfprintf(Serror, "[PROLOG STACK:\n");
      backTrace(NULL, 10);
      Sfprintf(Serror, "]\n");
#endif
      pl_abort(ABORT_NORMAL);
    }
#else /*HAVE___TRY*/
    LD->in_arithmetic--;
#endif /*HAVE___TRY*/

    if ( rval && r->type == V_REAL && !check_float(r->value.f) )
      rval = FALSE;

    return rval;
  }
}


int
valueExpression(term_t t, Number r ARG_LD)
{ return eval_expression(t, r, 0 PASS_LD);
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
	   ATOM_character, wordToTermRef(p));

  return EOF;
}


static int
getCharExpression(term_t t, Number r ARG_LD)
{ Word a, p = valTermRef(t);
  int chr;

  deRef(p);

  a = argTermP(*p, 0);
  if ( (chr = arithChar(a PASS_LD)) == EOF )
    fail;

  a = argTermP(*p, 1);
  if ( !isNil(*a) )
    return PL_error(".", 2, "\"x\" must hold one character", ERR_TYPE,
		    ATOM_nil, wordToTermRef(a));

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
    case V_REAL:
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

    if ( trueFeature(ISO_FEATURE) )
      return PL_error("+", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);

  promoteToRealNumber(n);
#endif

  succeed;
}



		/********************************
		*     ARITHMETIC FUNCTIONS      *
		*********************************/

static int
ar_add_ui(Number n, long add)
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
#endif
    default:
      ;
  }

  assert(0);
  fail;
}

static int
ar_add(Number n1, Number n2, Number r)
{ same_type_numbers(n1, n2);

  switch(n1->type)
  { case V_INTEGER:
    { r->value.i = n1->value.i + n2->value.i; 
    
      if ( (n1->value.i > 0 && n2->value.i > 0 && r->value.i <= 0) ||
	   (n1->value.i < 0 && n2->value.i < 0 && r->value.i >= 0) )
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
    case V_REAL:
    { r->value.f = n1->value.f + n2->value.f; 
      r->type = V_REAL;
      succeed;
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
    
      if ( (n1->value.i > 0 && n2->value.i < 0 && r->value.i <= 0) ||
	   (n1->value.i < 0 && n2->value.i > 0 && r->value.i >= 0) )
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
    case V_REAL:
    { r->value.f = n1->value.f - n2->value.f; 
      r->type = V_REAL;
      succeed;
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

      { int64_t mod = n1->value.i % n2->value.i;
	if ( mod != 0 && ( mod < 0 ) != ( n2->value.i < 0 ) )
	  mod += n2->value.i;
	r->value.i = mod;
      }
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

  switch(n2->type)
  { case V_INTEGER:
      if ( n2->value.i < LONG_MIN  ||
	   n2->value.i > LONG_MAX )
	return int_too_big();
      else
	shift = (long)n2->value.i;
      break;
#ifdef O_GMP
    case V_MPZ:
      if ( mpz_cmp_si(n2->value.mpz, LONG_MIN) < 0 ||
	   mpz_cmp_si(n2->value.mpz, LONG_MAX) > 0 )
	return int_too_big();
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
      if ( dir < 0 )
      {
#ifdef O_GMP				/* msb() is 0..63 */
	if ( msb64(n1->value.i) + shift >= (int)(sizeof(int64_t)*8-1) )
	{ promoteToMPZNumber(n1);
	  goto mpz;
	} else
#endif
	{ r->value.i = n1->value.i << shift; 
	}
      } else
      { r->value.i = n1->value.i >> shift; 
      }
      r->type = V_INTEGER; 
      succeed; 
#ifdef O_GMP
    case V_MPZ: 
    mpz:
      r->type = V_MPZ; 
      mpz_init(r->value.mpz); 
      if ( dir < 0 )
	mpz_mul_2exp(r->value.mpz, n1->value.mpz, shift);
      else
	mpz_fdiv_q_2exp(r->value.mpz, n1->value.mpz, shift); 
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


/* Unary functions requiring double argument */

#define UNAIRY_FLOAT_FUNCTION(name, op) \
  static int \
  name(Number n1, Number r) \
  { promoteToRealNumber(n1); \
    r->value.f = op(n1->value.f); \
    r->type    = V_REAL; \
    succeed; \
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
  { promoteToRealNumber(n1); \
    promoteToRealNumber(n2); \
    r->value.f = func(n1->value.f, n2->value.f); \
    r->type = V_REAL; \
    succeed; \
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
  promoteToRealNumber(n1);
  promoteToRealNumber(n2);
  r->value.f = pow(n1->value.f, n2->value.f);
  r->type = V_REAL;

  succeed;
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
		  ERR_NOT_IMPLEMENTED_FEATURE, "powm/3");
#endif
}


static int
ar_sqrt(Number n1, Number r)
{ promoteToRealNumber(n1);
  if ( n1->value.f < 0 )
    return PL_error("sqrt", 1, NULL, ERR_AR_UNDEF);
  r->value.f = sqrt(n1->value.f);
  r->type    = V_REAL;
  succeed;
}


static int
ar_asin(Number n1, Number r)
{ promoteToRealNumber(n1);
  if ( n1->value.f < -1.0 || n1->value.f > 1.0 )
    return PL_error("asin", 1, NULL, ERR_AR_UNDEF);
  r->value.f = asin(n1->value.f);
  r->type    = V_REAL;
  succeed;
}


static int
ar_acos(Number n1, Number r)
{ promoteToRealNumber(n1);
  if ( n1->value.f < -1.0 || n1->value.f > 1.0 )
    return PL_error("acos", 1, NULL, ERR_AR_UNDEF);
  r->value.f = acos(n1->value.f);
  r->type    = V_REAL;
  succeed;
}


static int
ar_log(Number n1, Number r)
{ promoteToRealNumber(n1);
  if ( n1->value.f <= 0.0 )
    return PL_error("log", 1, NULL, ERR_AR_UNDEF);
  r->value.f = log(n1->value.f);
  r->type    = V_REAL;
  succeed;
}


static int
ar_log10(Number n1, Number r)
{ promoteToRealNumber(n1);
  if ( n1->value.f <= 0.0 )
    return PL_error("log10", 1, NULL, ERR_AR_UNDEF);
  r->value.f = log10(n1->value.f);
  r->type    = V_REAL;
  succeed;
}


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
      return PL_error("//", 2, NULL, ERR_DIV_BY_ZERO);

    r->value.i = n1->value.i / n2->value.i;
    r->type = V_INTEGER;

    succeed;
  }

#ifdef O_GMP
  promoteToMPZNumber(n1);
  promoteToMPZNumber(n2);
  
  if ( mpz_sgn(n2->value.mpz) == 0 )
    return PL_error("//", 2, NULL, ERR_DIV_BY_ZERO);

  r->type = V_MPZ;
  mpz_init(r->value.mpz);
  mpz_tdiv_q(r->value.mpz, n1->value.mpz, n2->value.mpz);

  succeed;
#endif
}


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
    case V_REAL:
      return (n1->value.f < 0.0 ? -1 : n1->value.f > 0.0 ? 1 : 0);
    default:
      assert(0);
      fail;
  }
}

static int
ar_sign(Number n1, Number r)
{ r->value.i = ar_sign_i(n1);
  r->type = V_INTEGER;
  succeed;
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
    case V_REAL:
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

  if ( !trueFeature(ISO_FEATURE) )
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
      case V_REAL:
	break;
    }
  }

					/* TBD: How to handle Q? */
  promoteToRealNumber(n1);
  promoteToRealNumber(n2);
  if ( n2->value.f == 0.0 )
    return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);
  r->value.f = n1->value.f / n2->value.f;
  r->type = V_REAL;

  succeed;
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
    if ( prod/y == x )
    { *r = prod;
    
      return TRUE;
    }
    return FALSE;
  }
}


static int
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
    case V_REAL:
      r->value.f = n1->value.f * n2->value.f;
      r->type = V_REAL;
      succeed;
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
    case V_REAL:
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
domainErrorNumber(const char *f, int a, Number n, atom_t error)
{ GET_LD
  term_t t = PL_new_term_ref();
      
  PL_unify_number(t, n);
  return PL_error(f, a, NULL, ERR_DOMAIN, error, t);
}


static int
notLessThanZero(const char *f, int a, Number n)
{ return domainErrorNumber(f, a, n, ATOM_not_less_than_zero);
}


static int
mustBePositive(const char *f, int a, Number n)
{ return domainErrorNumber(f, a, n, ATOM_not_less_than_one);
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
popcount64(int64_t i)
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

      r->value.i = popcount64(n1->value.i);
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
  	promoteToRealNumber(n1);
	r->type = V_REAL;
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
    case V_REAL:
      r->value.f = -n1->value.f;
      r->type = V_REAL;
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
	promoteToRealNumber(n1);
	r->type = V_REAL;
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
    case V_REAL:
    { r->value.f = abs(n1->value.f);
      r->type = V_REAL;
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
    case V_REAL:
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
      r->type = V_REAL;
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
  promoteToRealNumber(r);

  succeed;
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
    case V_REAL:
    {
#ifdef HAVE_FLOOR
      r->type = V_REAL;
      r->value.f = floor(n1->value.f);
      if ( !toIntegerNumber(r, TOINT_CONVERT_FLOAT|TOINT_TRUNCATE) )
      { return PL_error("floor", 1, NULL, ERR_EVALUATION, ATOM_int_overflow);
      }
#else /*HAVE_FLOOR*/
      if ( n1->value.f > (double)PLMININT && n1->value.f < (double)PLMAXINT )
      { r->value.i = (int64_t)n1->value.f;
	if ( n1->value.f < 0 && (real)r->value.i > n1->value.f )
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
    case V_REAL:
    {
#ifdef HAVE_CEIL
       r->type = V_REAL;
       r->value.f = ceil(n1->value.f);
       if ( !toIntegerNumber(r, TOINT_CONVERT_FLOAT|TOINT_TRUNCATE) )
       { return PL_error("ceil", 1, NULL, ERR_EVALUATION, ATOM_int_overflow);
       }
#else /*HAVE_CEIL*/
       if ( n1->value.f > (double)PLMININT && n1->value.f < (double)PLMAXINT )
       { r->value.i = (int64_t)n1->value.f;
	 if ( (real)r->value.i < n1->value.f )
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
    case V_REAL:
    { double ip;

      r->value.f = modf(n1->value.f, &ip);
      r->type = V_REAL;
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
    case V_REAL:
    { double ip;

      (void)modf(n1->value.f, &ip);
      r->value.f = ip;
      r->type = V_REAL;
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
    case V_REAL:
      if ( n1->value.f >= 0.0 )
	return ar_floor(n1, r);
      else
	return ar_ceil(n1, r);
    default:
      cpNumber(r, n1);
      succeed;
  }
}


static int
ar_random(Number n1, Number r)
{ uint64_t bound;

  if ( !toIntegerNumber(n1, TOINT_CONVERT_FLOAT) )
    return PL_error("random", 1, NULL, ERR_AR_TYPE, ATOM_integer, n1);

  switch(n1->type)
  {
#ifdef O_GMP
    case V_MPZ:
    { int64_t i;

      if ( !mpz_to_int64(n1->value.mpz, &i) )
	return PL_error("random", 1, NULL, ERR_REPRESENTATION, ATOM_integer);
      if ( i < 1 )
	return mustBePositive("random", 1, n1);
      bound = (uint64_t)i;
      break;
    }
#endif
    case V_INTEGER:
      if ( n1->value.i < 1 )
	return mustBePositive("random", 1, n1);
      bound = (uint64_t)n1->value.i;
      break;
    default:
      assert(0);
      fail;
  }

  r->value.i = _PL_Random() % bound;
  r->type = V_INTEGER;

  succeed;
}


static int
ar_pi(Number r)
{ r->value.f = M_PI;

  r->type = V_REAL;
  succeed;
}


static int
ar_e(Number r)
{ r->value.f = M_E;

  r->type = V_REAL;
  succeed;
}


static int
ar_cputime(Number r)
{ r->value.f = CpuTime(CPU_USER);

  r->type = V_REAL;
  succeed;
}


		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

static
PRED_IMPL("is", 2, is, PL_FA_TRANSPARENT)	/* -Value is +Expr */
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


#if O_PROLOG_FUNCTIONS
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functions are module-sensitive. We has the arithmetic functions on their
functor and keep them sorted in   the chain, most-specific module first.
See also isCurrentArithFunction()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$arithmetic_function", 2, arithmetic_function, PL_FA_TRANSPARENT)
{ GET_LD
  Procedure proc;
  functor_t fd;
  FunctorDef fdef;
  ArithFunction f, a, *ap;
  Module m = NULL;
  term_t head = PL_new_term_ref();
  size_t v;
  int index, rc;

  PL_strip_module(A1, &m, head);
  if ( !PL_get_functor(head, &fd) )
    return PL_error(NULL, 0, NULL,
		    ERR_TYPE, ATOM_callable, head);
  fdef = valueFunctor(fd);
  if ( fdef->arity < 1 )
    return PL_error(NULL, 0, NULL,
		    ERR_DOMAIN, ATOM_not_less_than_one, head);
  if ( !PL_get_integer_ex(A2, &index) )
    fail;

  proc = lookupProcedure(fd, m);
  fd = lookupFunctorDef(fdef->name, fdef->arity - 1);
  if ( (f = isCurrentArithFunction(fd, m)) && f->module == m )
    succeed;				/* already registered */

  v = functorHashValue(fd, ARITHHASHSIZE);
  f = allocHeap(sizeof(struct arithFunction));
  f->functor  = fd;
  f->function = NULL;
  f->module   = m;
  f->level    = m->level;
  f->proc     = proc;

  startCritical;
  for(ap = &arithFunctionTable[v], a=*ap; ; ap = &a->next, a = *ap)
  { if ( !a || isTableRef(a) || f->level >= a->level )
    { f->next = a;
      *ap = f;
      break;
    }
  }
  rc = registerFunction(f, index);
  endCritical;

  return rc;
}

word
pl_current_arithmetic_function(term_t f, control_t h)
{ GET_LD
  ArithFunction a;
  Module m = NULL;
  term_t head = PL_new_term_ref();

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { functor_t fd;

      PL_strip_module(f, &m, head);

      if ( PL_is_variable(head) )
      { a = arithFunctionTable[0];
        break;
      } else if ( PL_get_functor(head, &fd) )
      {	return isCurrentArithFunction(fd, m) ? TRUE : FALSE;
      } else
        return PL_error(NULL, 0, NULL,
			ERR_TYPE, ATOM_callable, f);
    }
    case FRG_REDO:
      PL_strip_module(f, &m, head);

      a = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for( ; a; a = a->next )
  { Module m2;

    while( isTableRef(a) )
    { a = unTableRef(ArithFunction, a);
      if ( !a )
        fail;
    }

    for(m2 = m; m2; )
    { if ( m2 == a->module && a == isCurrentArithFunction(a->functor, m) )
      { if ( PL_unify_functor(f, a->functor) )
	  return_next_table(ArithFunction, a, ;);
      }
      if ( m2->supers )			/* TBD: multiple supers! */
      { m2 = m2->supers->value;
      } else
	fail;
    }
  }

  fail;
}


static
PRED_IMPL("$prolog_arithmetic_function", 2, prolog_arithmetic_function,
	  PL_FA_NONDETERMINISTIC|PL_FA_TRANSPARENT)
{ PRED_LD
  int i, mx;
  term_t tmp;
  fid_t fid;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
      i = 0;
      break;
    case FRG_REDO:
      i = (int)CTX_INT;
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  tmp = PL_new_term_ref();
  mx = (int)entriesBuffer(function_array, ArithFunction);

  fid = PL_open_foreign_frame();
  for( ; i<mx; i++ )
  { ArithFunction f = FunctionFromIndex(i);

    PL_put_functor(tmp, f->functor);
    if ( f->proc &&
	 PL_unify_term(A1,
		       PL_FUNCTOR, FUNCTOR_colon2,
		         PL_ATOM, f->module->name,
		         PL_TERM, tmp) &&
	 PL_unify_integer(A2, (intptr_t)f->index) )
    { if ( ++i == mx )
	succeed;
      ForeignRedoInt(i);
    }

    PL_rewind_foreign_frame(fid);
  }

  fail;
}

#endif /* O_PROLOG_FUNCTIONS */

typedef struct
{ functor_t	functor;
  ArithF	function;
} ar_funcdef;

#define ADD(functor, func) { functor, func }

static const ar_funcdef ar_funcdefs[] = {
  ADD(FUNCTOR_plus2,		ar_add),
  ADD(FUNCTOR_minus2,		ar_minus),
  ADD(FUNCTOR_star2,		ar_mul),
  ADD(FUNCTOR_divide2,		ar_divide),
#ifdef O_GMP
  ADD(FUNCTOR_rational1,	ar_rational),
  ADD(FUNCTOR_rationalize1,	ar_rationalize),
  ADD(FUNCTOR_rdiv2,		ar_rdiv),
#endif
  ADD(FUNCTOR_minus1,		ar_u_minus),
  ADD(FUNCTOR_plus1,		ar_u_plus),
  ADD(FUNCTOR_abs1,		ar_abs),
  ADD(FUNCTOR_max2,		ar_max),
  ADD(FUNCTOR_min2,		ar_min),

  ADD(FUNCTOR_mod2,		ar_mod),
  ADD(FUNCTOR_rem2,		ar_rem),
  ADD(FUNCTOR_div2,		ar_div),
  ADD(FUNCTOR_sign1,		ar_sign),

  ADD(FUNCTOR_and2,		ar_conjunct),
  ADD(FUNCTOR_or2,		ar_disjunct),
  ADD(FUNCTOR_rshift2,		ar_shift_right),
  ADD(FUNCTOR_lshift2,		ar_shift_left),
  ADD(FUNCTOR_xor2,		ar_xor),
  ADD(FUNCTOR_backslash1,	ar_negation),

  ADD(FUNCTOR_random1,		ar_random),

  ADD(FUNCTOR_integer1,		ar_integer),
  ADD(FUNCTOR_round1,		ar_integer),
  ADD(FUNCTOR_truncate1,	ar_truncate),
  ADD(FUNCTOR_float1,		ar_float),
  ADD(FUNCTOR_floor1,		ar_floor),
  ADD(FUNCTOR_ceil1,		ar_ceil),
  ADD(FUNCTOR_ceiling1,		ar_ceil),
  ADD(FUNCTOR_float_fractional_part1, ar_float_fractional_part),
  ADD(FUNCTOR_float_integer_part1, ar_float_integer_part),

  ADD(FUNCTOR_sqrt1,		ar_sqrt),
  ADD(FUNCTOR_sin1,		ar_sin),
  ADD(FUNCTOR_cos1,		ar_cos),
  ADD(FUNCTOR_tan1,		ar_tan),
  ADD(FUNCTOR_asin1,		ar_asin),
  ADD(FUNCTOR_acos1,		ar_acos),
  ADD(FUNCTOR_atan1,		ar_atan),
  ADD(FUNCTOR_atan2,		ar_atan2),
  ADD(FUNCTOR_log1,		ar_log),
  ADD(FUNCTOR_exp1,		ar_exp),
  ADD(FUNCTOR_log101,		ar_log10),
  ADD(FUNCTOR_hat2,		ar_pow),
  ADD(FUNCTOR_doublestar2,	ar_pow),
  ADD(FUNCTOR_pi0,		ar_pi),
  ADD(FUNCTOR_e0,		ar_e),

  ADD(FUNCTOR_cputime0,		ar_cputime),
  ADD(FUNCTOR_msb1,		ar_msb),
  ADD(FUNCTOR_lsb1,		ar_lsb),
  ADD(FUNCTOR_popcount1,	ar_popcount),
  ADD(FUNCTOR_powm3,		ar_powm),

  ADD(FUNCTOR_eval1,		ar_eval)
};

#undef ADD

static int
registerFunction(ArithFunction f, int index)
{ int i = (int)entriesBuffer(function_array, ArithFunction);

  if ( index )
  { if ( index != i )
    { fatalError("Mismatch in arithmetic function index (%d != %d)",
		 index, i);
      fail;				/* NOTREACHED */
    }
  }

  f->index = i;
  addBuffer(function_array, f, ArithFunction);

  succeed;
}


static void
registerBuiltinFunctions()
{ GET_LD
  int n, size = sizeof(ar_funcdefs)/sizeof(ar_funcdef);
  ArithFunction f = allocHeap(size * sizeof(struct arithFunction));
  const ar_funcdef *d;

					/* grow to desired size immediately */
  growBuffer(function_array, size * sizeof(ArithFunction));
  memset(f, 0, size * sizeof(struct arithFunction));

  for(d = ar_funcdefs, n=0; n<size; n++, f++, d++)
  { int v = (int)functorHashValue(d->functor, ARITHHASHSIZE);

    f->functor  = d->functor;
    f->function = d->function;
    f->module   = MODULE_system;
    f->level    = 0;			/* level of system module */
    f->next     = arithFunctionTable[v];
    arithFunctionTable[v] = f;
    registerFunction(f, 0);
    DEBUG(1, Sdprintf("Registered %s/%d at %d, index=%d\n",
		      stringAtom(nameFunctor(f->functor)),
		      arityFunctor(f->functor),
		      v,
		      f->index));
  }			       
}


void
initArith(void)
{ initBuffer(function_array);
					/* link the table to enumerate */
  { ArithFunction *f;
    int n;

    for(n=0, f = arithFunctionTable; n < (ARITHHASHSIZE-1); n++, f++)
      *f = makeTableRef(f+1);
  }

  registerBuiltinFunctions();

#ifdef O_INHIBIT_FP_SIGNALS
  fpsetmask(fpgetmask() & ~(FP_X_DZ|FP_X_INV|FP_X_OFL));
#endif
}


void
cleanupArith(void)
{ discardBuffer(function_array);

#ifdef O_INHIBIT_FP_SIGNALS
  fpresetsticky(FP_X_DZ|FP_X_INV|FP_X_OFL);
  fpsetmask(FP_X_DZ|FP_X_INV|FP_X_OFL);
#endif
}


#if O_COMPILE_ARITH

		/********************************
		*    VIRTUAL MACHINE SUPPORT    *
		*********************************/

int
indexArithFunction(functor_t fdef, Module m)
{ ArithFunction f;

  if ( !(f = isCurrentArithFunction(fdef, m)) )
    return -1;

  return (int)f->index;
}


functor_t
functorArithFunction(int n)
{ return FunctionFromIndex(n)->functor;
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
  ArithFunction f = FunctionFromIndex(findex);
  Number argv = argvArithStack(argc PASS_LD);

  if ( f->proc )			/* Prolog implementation */
  { intptr_t lsafe = (char*)lTop - (char*)lBase;

    lTop = (LocalFrame)argFrameP(lTop, 1); /* needed for is/2.  See A_IS */
    { fid_t fid = PL_open_foreign_frame();
      term_t h0 = PL_new_term_refs(argc+1);
      int n;
    
      for(n=0; n<argc; n++)
	_PL_put_number(h0+n, argv+n);

      rval = prologFunction(f, h0, &result PASS_LD);
      PL_close_foreign_frame(fid);
    }
    lTop = addPointer(lBase, lsafe);
  } else
  { switch(argc)
    { case 0:
	rval = (*f->function)(&result);
        break;
      case 1:
	rval = (*f->function)(argv, &result);
        break;
      case 2:
	rval = (*f->function)(argv, argv+1, &result);
	break;
      case 3:
	rval = (*f->function)(argv, argv+1, argv+2, &result);
        break;
      default:
	rval = FALSE;
        sysError("Too many arguments to arithmetic function");
    }
  }
    
  popArgvArithStack(argc PASS_LD);

  if ( rval )
  { if ( result.type == V_REAL && !check_float(result.value.f) )
      return FALSE;

    pushArithStack(&result PASS_LD);
  }

  return rval;
}

#endif /* O_COMPILE_ARITH */


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(arith)
  PRED_DEF("is",   2, is,  PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF("<",	   2, lt,  PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF(">",	   2, gt,  PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF("=<",   2, leq, PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF(">=",   2, geq, PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF("=\\=", 2, neq, PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF("=:=",  2, eq,  PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF("$prolog_arithmetic_function", 2, prolog_arithmetic_function,
	   PL_FA_NONDETERMINISTIC|PL_FA_TRANSPARENT)
  PRED_DEF("$arithmetic_function", 2, arithmetic_function, PL_FA_TRANSPARENT)
  PRED_DEF("succ", 2, succ, 0)
  PRED_DEF("plus", 3, plus, 0)
  PRED_DEF("between", 3, between, PL_FA_NONDETERMINISTIC)
EndPredDefs
