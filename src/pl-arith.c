/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
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

#ifdef __MINGW32__
#include <winsock2.h>
#include <windows.h>
#include <wincrypt.h>
#endif

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-arith.h"
#include "pl-fli.h"
#include "pl-funct.h"
#include "pl-prims.h"
#include "pl-gc.h"
#include "pl-read.h"
#include "os/pl-prologflag.h"
#include <math.h>
#include <limits.h>
#ifdef HAVE_FLOAT_H
#include <float.h>
#ifdef _MSC_VER
#ifndef isnan
#define isnan(x) _isnan(x)
#endif
#define copysign(x,y) _copysign(x,y)
#endif
#endif
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif
#include <fenv.h>

#ifndef DBL_MAX
#define DBL_MAX     1.7976931348623157e+308
#endif
#ifndef DBL_MIN
#define DBL_MIN     2.2250738585072014e-308
#endif
#ifndef DBL_EPSILON
#define DBL_EPSILON 0.00000000000000022204
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

static double const_nan;
static double const_inf;
static double const_neg_inf;

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

#if USE_LD_MACROS
#define	set_roundtoward(p, old)		LDFUNC(set_roundtoward, p, old)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static int		ar_minus(Number n1, Number n2, Number r);
static int		mul64(int64_t x, int64_t y, int64_t *r);
static int		notLessThanZero(const char *f, int a, Number n);
static int		mustBePositive(const char *f, int a, Number n);
static int		set_roundtoward(Word p, Number old);

#undef LDFUNC_DECLARATIONS


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

	if ( !ar_add_ui(&state->low, 1) ||
	     !PL_unify_number(n, &state->low) )
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
    rc = ( pl_ar_add(&i1, &one, &i2) &&
	   PL_unify_number(A2, &i2)
	 );
  } else if ( !canBind(*p1) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, A1);

  p2 = valTermRef(A2); deRef(p2);

  if ( isInteger(*p2) )
  { get_integer(*p2, &i2);
    switch( ar_sign_i(&i2) )
    { case 1:
	rc = ( ar_minus(&i2, &one, &i1) &&
	       PL_unify_number(A1, &i1)
	     );
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


#define var_or_integer(t, n, which, mask) LDFUNC(var_or_integer, t, n, which, mask)
static int
var_or_integer(DECL_LD term_t t, number *n, int which, int *mask)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isInteger(*p) )
  { get_integer(*p, n);
    *mask |= which;
    succeed;
  }
  if ( canBind(*p) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


static
PRED_IMPL("plus", 3, plus, 0)
{ GET_LD
  number m, n, o;
  int mask = 0;
  int rc;

  if ( !var_or_integer(A1, &m, 0x1, &mask) ||
       !var_or_integer(A2, &n, 0x2, &mask) ||
       !var_or_integer(A3, &o, 0x4, &mask) )
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
		*   LOGICAL NUMBER FUNCTION     *
		*********************************/

static
PRED_IMPL("bounded_number", 3, bounded_number, 0)
{ PRED_LD
  number n, lo, hi;
  int rc;

  if ( PL_get_number(A3, &n) )
  { switch(n.type)
    {
#ifdef O_GMP
      case V_MPZ:
#endif
      case V_INTEGER:
      { cpNumber(&lo, &n);
	cpNumber(&hi, &n);
	ar_add_ui(&lo, -1);
	ar_add_ui(&hi, 1);
	break;
      }
#if O_GMP
      case V_MPQ:
	promoteToFloatNumber(&n);
      /*FALLTHROUGH*/
#endif
      case V_FLOAT:
      { if ( isfinite(n.value.f) )
	{ lo.type = V_FLOAT;
	  lo.value.f = nexttoward(n.value.f,-INFINITY);
	  hi.type = V_FLOAT;
	  hi.value.f = nexttoward(n.value.f, INFINITY);
	} else
	{ clearNumber(&n);
	  return FALSE;
	}
	break;
      }
    }

    rc = ( ((PL_get_number(A1, &lo)) ? (cmpNumbers(&lo, &n) == -1)
	                             : PL_unify_number(A1, &lo)) &&
	   ((PL_get_number(A2, &hi)) ? (cmpNumbers(&n, &hi) == -1)
				     : PL_unify_number(A2, &hi))
	 );

  } else
  { rc = PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_number, A1);
  }
  clearNumber(&n);
  clearNumber(&lo);
  clearNumber(&hi);

  return rc;
}

		 /*******************************
		 *	 BIGNUM FUNCTIONS	*
		 *******************************/

#ifdef O_GMP

#define get_mpz(t, n) LDFUNC(get_mpz, t, n)
static int
get_mpz(DECL_LD term_t t, Number n)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isInteger(*p) )
  { get_integer(*p, n);
    promoteToMPZNumber(n);

    return TRUE;
  }

  return PL_type_error("integer", t);
}


/**
 * divmod(+Dividend, +Divisor, -Quotient, -Remainder)
 *
 * Defined as
 *
 *   - Quotient  is div(Dividend, Divisor)
 *   - Remainder is mod(Dividend, Divisor)
 */

static
PRED_IMPL("divmod", 4, divmod, 0)
{ PRED_LD
  number N = {V_INTEGER}, D = {V_INTEGER};
  int rc = FALSE;

  if ( get_mpz(A1, &N) &&
       get_mpz(A2, &D) )
  { if ( mpz_sgn(D.value.mpz) != 0 )
    { number Q = {V_MPZ}, R = {V_MPZ};

      mpz_init(R.value.mpz);
      mpz_init(Q.value.mpz);
      mpz_fdiv_qr(Q.value.mpz, R.value.mpz, N.value.mpz, D.value.mpz);
      rc = ( PL_unify_number(A3, &Q) &&
	     PL_unify_number(A4, &R)
	   );
      clearNumber(&R);
      clearNumber(&Q);
    } else
    { rc = PL_error("divmod", 2, NULL, ERR_DIV_BY_ZERO);
    }
  }

  clearNumber(&N);
  clearNumber(&D);

  return rc;
}

/**
 * nth_integer_root_and_remainder(+N, +I, -Root, -Remainder)
 */

static
PRED_IMPL("nth_integer_root_and_remainder", 4,
	  nth_integer_root_and_remainder, 0)
{ PRED_LD
  number N = {V_INTEGER};
  long I;
  int rc = FALSE;

  if ( PL_get_long_ex(A1, &I) &&
       get_mpz(A2, &N) )
  { if ( I >= 1 )
    { number root = {V_MPZ};
      number rem = {V_MPZ};

      if ( mpz_sgn(N.value.mpz) < 0 &&
	   I % 2 == 0 )
      { rc = PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
	goto out;
      }

      mpz_init(root.value.mpz);
      mpz_init(rem.value.mpz);
      mpz_rootrem(root.value.mpz, rem.value.mpz,
		  N.value.mpz, (unsigned long)I);
      rc = ( PL_unify_number(A3, &root) &&
	     PL_unify_number(A4, &rem)
	   );
      clearNumber(&root);
      clearNumber(&rem);
    } else
    { rc = PL_domain_error("not_less_than_one", A1);
    }
  }

out:
  clearNumber(&N);

  return rc;
}


static
PRED_IMPL("rational", 3, rational, 0)
{ PRED_LD
  Word p = valTermRef(A1);

  deRef(p);
  if ( isRational(*p) )
  { if ( isMPQNum(*p) )
    { number n, num, den;
      int rc;

      get_rational(*p, &n);
      assert(n.type == V_MPQ);

      num.type = V_MPZ;
      den.type = V_MPZ;
      mpz_init(num.value.mpz);
      mpz_init(den.value.mpz);
      mpz_set(num.value.mpz, mpq_numref(n.value.mpq));
      mpz_set(den.value.mpz, mpq_denref(n.value.mpq));

      rc = ( PL_unify_number(A2, &num) &&
	     PL_unify_number(A3, &den) );

      clearNumber(&num);
      clearNumber(&den);

      return rc;
    } else
    { return ( PL_unify(A1, A2) &&
	       PL_unify_integer(A3, 1) );
    }
  }

  return FALSE;
}


#endif /*O_GMP*/

static
PRED_IMPL("float_parts", 4, float_parts, 0)
{ PRED_LD
  double d;

  if ( PL_get_float_ex(A1, &d) )
  { double m;
    int e;

    m = frexp(d, &e);
    return ( PL_unify_float(A2, m) &&
	     PL_unify_integer(A3, 2) &&
	     PL_unify_integer(A4, e) );
  }

  return FALSE;
}


		/********************************
		*           COMPARISON          *
		*********************************/

/* implements <, =<, >, >=, =:= and =\=
 */

int
ar_compare(Number n1, Number n2, int what)
{ int diff = cmpNumbers(n1, n2);		/* nan compares CMP_NOTEQ */

  switch(what)
  { case LT: return diff == CMP_LESS;
    case GT: return diff == CMP_GREATER;
    case LE: return (diff == CMP_LESS) || (diff == CMP_EQUAL);
    case GE: return (diff == CMP_GREATER) || (diff == CMP_EQUAL);
    case NE: return diff != CMP_EQUAL;
    case EQ: return diff == CMP_EQUAL;
    default:
      assert(0);
      return FALSE;
  }
}


#define compareNumbers(n1, n2, what) LDFUNC(compareNumbers, n1, n2, what)
static word
compareNumbers(DECL_LD term_t n1, term_t n2, int what)
{ AR_CTX
  number left, right;
  int rc;

  AR_BEGIN();

  if ( valueExpression(n1, &left) &&
       valueExpression(n2, &right) )
  { rc = ar_compare(&left, &right, what);

    clearNumber(&left);
    clearNumber(&right);
    AR_END();
  } else
  { AR_CLEANUP();
    rc = FALSE;
  }

  return rc;
}

static
PRED_IMPL("<", 2, lt, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, LT);
}

static
PRED_IMPL(">", 2, gt, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, GT);
}

static
PRED_IMPL("=<", 2, leq, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, LE);
}

static
PRED_IMPL(">=", 2, geq, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, GE);
}

static
PRED_IMPL("=\\=", 2, neq, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, NE);
}

static
PRED_IMPL("=:=", 2, eq, PL_FA_ISO)
{ PRED_LD
  return compareNumbers(A1, A2, EQ);
}


		 /*******************************
		 *	 ARITHMETIC STACK	*
		 *******************************/

Number
growArithStack(DECL_LD)
{ Number n;

  if ( LD->arith.stack.top == LD->arith.stack.max )
  { size_t size;

    if ( LD->arith.stack.base )
    { size = (size_t)(LD->arith.stack.max - LD->arith.stack.base);
      LD->arith.stack.base = PL_realloc(LD->arith.stack.base,
					size*sizeof(number)*2);
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
check_float(Number n)
{ PL_error_code code = ERR_NO_ERROR;
#ifdef HAVE_FPCLASSIFY
  switch(fpclassify(n->value.f))
  { case FP_NAN:
      code = ERR_AR_UNDEF;
      break;
    case FP_SUBNORMAL:
      code = ERR_AR_UNDERFLOW;
      break;
    case FP_INFINITE:
      code = ERR_AR_OVERFLOW;
      break;
  }
#else
#ifdef HAVE_FPCLASS
  switch(fpclass(n->value.f))
  { case FP_SNAN:
    case FP_QNAN:
      code = ERR_AR_UNDEF;
      break;
    case FP_NINF:
    case FP_PINF:
      code = ERR_AR_OVERFLOW;
      break;
    case FP_NDENORM:			/* pos/neg denormalized non-zero */
    case FP_PDENORM:
      code = ERR_AR_UNDERFLOW;
      break;
    case FP_NNORM:			/* pos/neg normalized non-zero */
    case FP_PNORM:
    case FP_NZERO:			/* pos/neg zero */
    case FP_PZERO:
      break;
  }
#else
#ifdef HAVE__FPCLASS
  switch(_fpclass(n->value.f))
  { case _FPCLASS_SNAN:
    case _FPCLASS_QNAN:
      code = ERR_AR_UNDEF;
      break;
    case _FPCLASS_NINF:
    case _FPCLASS_PINF:
      code = ERR_AR_OVERFLOW;
      break;
  }
#else
#ifdef HAVE_ISNAN
  if ( isnan(n->value.f) )
    code = ERR_AR_UNDEF;
#endif
#ifdef HAVE_ISINF
  if ( isinf(n->value.f) )
    code = ERR_AR_OVERFLOW;
#endif
#endif /*HAVE__FPCLASS*/
#endif /*HAVE_FPCLASS*/
#endif /*HAVE_FPCLASSIFY*/

  if ( code != ERR_NO_ERROR )
  { GET_LD

    switch(code)
    { case ERR_AR_OVERFLOW:
	if ( LD->arith.f.flags & FLT_OVERFLOW )
	  return TRUE;
        break;
      case ERR_AR_UNDERFLOW:
	if ( LD->arith.f.flags & FLT_UNDERFLOW )
	  return TRUE;
        break;
      case ERR_AR_UNDEF:
	n->value.f = const_nan;
	if ( LD->arith.f.flags & FLT_UNDEFINED )
	  return TRUE;
        break;
      default:
	assert(0);
    }

    return PL_error(NULL, 0, NULL, code);
  }

  return TRUE;
}

static int
check_zero_div(int sign_n, Number r, char *func, int arity)
{ GET_LD

  if ( LD->arith.f.flags & FLT_ZERO_DIV )
  { r->type = V_FLOAT;
    r->value.f = copysign(const_inf,sign_n);
    return TRUE;
  } else
  { return PL_error(func, arity, NULL, ERR_DIV_BY_ZERO);
  }
}


#ifdef O_GMP
static int
check_mpq(Number r)
{ GET_LD
  size_t sz;

  if ( (sz=LD->arith.rat.max_rational_size) != (size_t)-1 )
  { int szn = mpq_numref(r->value.mpq)->_mp_size;
    int szd = mpq_denref(r->value.mpq)->_mp_size;

    if ( szn < 0 ) szn = -szn;
    if ( szd < 0 ) szd = -szd;

    if ( ( szn + szd ) * sizeof(mp_limb_t) > sz )
    { atom_t action = LD->arith.rat.max_rational_size_action;

      if ( action == ATOM_float )
	promoteToFloatNumber(r);
      else if ( action == ATOM_error )
	return PL_error(NULL, 0, "requires more than max_rational_size bytes",
			ERR_AR_TRIPWIRE, ATOM_max_rational_size, r);
      else
	assert(0);
    }
  }

  return TRUE;
}
#endif

		 /*******************************
		 *	     EVALULATE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
valueExpression() evaluates an `evaluable term'.

This new implementation avoids using the C-stack   to be able to process
more deeply nested terms and to be able  to recover in the unlikely case
that terms are still too deeply nested.

If it finds a term, it starts processing at the last argument, working back
to the start. If it finds  the   functor  itself, it evaluates the pushed
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
{ word w = 0;

  popSegStack(stack, &w, word);
  *wr = w & (word)0x1;
  *pp = (Word)(w & ~(word)0x1);
}


int
valueExpression(DECL_LD term_t expr, number *result)
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
  functor_t functor;
  int old_round_mode = fegetround();

  deRef(p);
  start = p;
  LD->in_arithmetic++;

  for(;;)
  { switch(tag(*p))
    { case TAG_INTEGER:
	get_rational(*p, n);
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
      { ArithF f;

	functor = lookupFunctorDef(*p, 0);
      arity0:
	if ( (f = isCurrentArithFunction(functor)) )
	{ if ( (*f)(n) != TRUE )
	    goto error;
	} else
	{ if ( isTextAtom(*p) )
	  { PL_error(NULL, 0, NULL, ERR_NOT_EVALUABLE, functor);
	  } else
	  { PL_error(NULL, 0, NULL, ERR_TYPE,
		     ATOM_evaluable, pushWordAsTermRef(p));
	    popTermRef();
	  }
	  goto error;
	}
	break;
      }
      case TAG_STRING:
	if ( getCharExpression(p, n) != TRUE )
	  goto error;
        break;
      case TAG_COMPOUND:
      { Functor term = valueTerm(*p);
	size_t arity = arityFunctor(term->definition);

	if ( term->definition == FUNCTOR_dot2 )
	{ if ( getCharExpression(p, n) != TRUE )
	    goto error;
	  break;
	}

	if ( arity == 0 )
	{ functor = term->definition;
	  goto arity0;
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

	  if ( (rc=is_acyclic(start)) == TRUE )
	  { known_acyclic = TRUE;
	  } else
	  { if ( rc == MEMORY_OVERFLOW )
	      PL_error(NULL, 0, NULL, ERR_NOMEM);
	    else
	      PL_error(NULL, 0, "cyclic term", ERR_TYPE, ATOM_expression, expr);
	    goto error;
	  }
	}
	if ( term->definition == FUNCTOR_roundtoward2 )
	{ number crnd;

	  if ( !set_roundtoward(&term->arguments[1], &crnd) )
	    goto error;
	  if ( !pushSegStack(&arg_stack, crnd, number) )
	  { PL_no_memory();
	    goto error;
	  }
	  p = &term->arguments[0];
	} else
	{ p = &term->arguments[arity-1];
	}
	walk_ref = FALSE;
	n = &n_tmp;
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
      { size_t arity = arityFunctor(functor);

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
    fesetround(old_round_mode);
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
arithChar(DECL_LD Word p)
{ deRef(p);

  if ( isInteger(*p) )
  { intptr_t chr = valInt(*p);

    if ( chr >= 0 && chr <= PLMAXWCHAR )
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


int
getCharExpression(DECL_LD Word p, Number r)
{ word w = *p;

  switch(tag(w))
  { case TAG_STRING:
    { size_t len;

      if ( isBString(w) )
      { char *s = getCharsString(w, &len);

	if ( len == 1 )
	{ r->value.i = s[0]&0xff;
	  r->type = V_INTEGER;
	  return TRUE;
	}
      } else
      { pl_wchar_t *ws = getCharsWString(w, &len);

	if ( len == 1 )
	{ r->value.i = ws[0];
	  r->type = V_INTEGER;
	  return TRUE;
	}
      }

    len_not_one:
      PL_error(NULL, 0, "\"x\" must hold one character", ERR_TYPE,
		 ATOM_nil, pushWordAsTermRef(p));
      popTermRef();
      return FALSE;
    }
    case TAG_COMPOUND:
    { Word a = argTermP(w, 0);
      int chr;

      if ( (chr = arithChar(a)) == EOF )
	fail;

      a = argTermP(w, 1);
      if ( !isNil(*a) )
	goto len_not_one;

      r->value.i = chr;
      r->type = V_INTEGER;

      return TRUE;
    }
    default:
      assert(0);
      return FALSE;
  }
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
      if ( !check_float(n) )
	return FALSE;
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
      return PL_error(NULL, 0, NULL, ERR_EVALUATION, ATOM_int_overflow);

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
    /*FALLTHROUGH*/
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

      return check_mpq(n);
    }
#endif
    case V_FLOAT:
    { n->value.f += (double)add;

      return check_float(n);
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
{ if ( !same_type_numbers(n1, n2) )
    return FALSE;

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
      return check_mpq(r);
    }
#endif
    case V_FLOAT:
    { r->value.f = n1->value.f + n2->value.f;
      r->type = V_FLOAT;

      return check_float(r);
    }
  }

  assert(0);
  fail;
}


static int
ar_minus(Number n1, Number n2, Number r)
{ if ( !same_type_numbers(n1, n2) )
    return FALSE;

  switch(n1->type)
  { case V_INTEGER:
    { r->value.i = (uint64_t)n1->value.i - (uint64_t)n2->value.i;

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
      return check_mpq(r);
      succeed;
    }
#endif
    case V_FLOAT:
    { r->value.f = n1->value.f - n2->value.f;
      r->type = V_FLOAT;

      return check_float(r);
    }
  }

  assert(0);
  fail;
}


static int
ar_even(Number i)
{ switch(i->type)
  { case V_INTEGER:
      return i->value.i % 2 == 0;
#ifdef O_GMP
    case V_MPZ:
      return mpz_fdiv_ui(i->value.mpz, 2) == 0;
#endif
    default:
      assert(0);
      fail;
  }
}


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

  if ( !same_type_numbers(n1, n2) )
    return FALSE;

  switch(n1->type)
  { case V_INTEGER:
      if ( n2->value.i == 0 )
	return PL_error("mod", 2, NULL, ERR_DIV_BY_ZERO);

      if ( n2->value.i != -1 || n1->value.i != INT64_MIN )
	r->value.i = mod(n1->value.i, n2->value.i);
      else
	r->value.i = 0;
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
int_too_big(void)
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
	  bits += MSB64(n1->value.i);
	else if ( n1->value.i == PLMININT )
	  bits += sizeof(int64_t)*8;
	else
	  bits += MSB64(-n1->value.i);

	if ( bits >= (int)(sizeof(int64_t)*8-1) )
	{ promoteToMPZNumber(n1);
	  goto mpz;
	} else
#endif
	{ r->value.i = n1->value.i << shift;
	}
      } else
      { if ( shift >= (long)sizeof(int64_t)*8 )
	  r->value.i = (n1->value.i >= 0 ? 0 : -1);
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

	if ( (msb/sizeof(char)) > (uint64_t)globalStackLimit() )
	{ mpz_clear(r->value.mpz);
	  return int_too_big();
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
same_positive_ints(const char *fname, Number n1, Number n2)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error(fname, 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error(fname, 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

  if ( !same_type_numbers(n1, n2) )
    return FALSE;

  switch(n1->type)
  { case V_INTEGER:
    { int64_t a = n1->value.i;
      int64_t b = n2->value.i;

      if ( a < 0 )
      { a = -(uint64_t)a;
	if ( a < 0 )
	{ promote:
#ifdef O_GMP
	  promoteToMPZNumber(n1);
	  promoteToMPZNumber(n2);
	  goto case_gmp;
#else
	  return PL_error(fname, 2, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
	}
      }
      if ( b < 0 )
      { b = -(uint64_t)b;
	if ( b < 0 )
	  goto promote;
      }

      n1->value.i = a;
      n2->value.i = b;
      break;
    }
#ifdef O_GMP
    case V_MPZ:
    case_gmp:
      /* we don't really need to make absolute here as the GMP functions
       * ignore the sign anyway
       */
      break;
#endif
    default:
      assert(0);
  }

  return TRUE;
}


static int64_t
i64_gcd(int64_t a, int64_t b)
{ int64_t t;

  if ( a == 0 )
    return b;
  if ( b == 0 )
    return a;

  while(b != 0)
  { t = b;
    b = a % b;
    a = t;
  }

  return a;
}


static int
ar_gcd(Number n1, Number n2, Number r)
{ if ( !same_positive_ints("gcd", n1, n2) )
    return FALSE;

  switch(n1->type)
  { case V_INTEGER:
    { r->type = V_INTEGER;
      r->value.i = i64_gcd(n1->value.i, n2->value.i);
      break;
    }
#ifdef O_GMP
    case V_MPZ:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_gcd(r->value.mpz, n1->value.mpz, n2->value.mpz);
      break;
#endif
    default:
      assert(0);
  }

  return TRUE;
}

static int
ar_lcm(Number n1, Number n2, Number r)
{ if ( !same_positive_ints("lcm", n1, n2) )
    return FALSE;

  switch(n1->type)
  { case V_INTEGER:
    { int64_t prod;

      if ( mul64(n1->value.i, n2->value.i, &prod) )
      { r->type = V_INTEGER;
	if ( prod != 0 )
	  r->value.i = prod/i64_gcd(n1->value.i, n2->value.i);
	else
	  r->value.i = 0;
	return TRUE;
      }
    }
#ifndef O_GMP
      return PL_error("lcm", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);
#else
      promoteToMPZNumber(n1);
      promoteToMPZNumber(n2);
    case V_MPZ:
      r->type = V_MPZ;
      mpz_init(r->value.mpz);
      mpz_lcm(r->value.mpz, n1->value.mpz, n2->value.mpz);
      break;
#endif
    default:
      assert(0);
  }

  return TRUE;
}


/* Unary functions requiring double argument */

#define UNAIRY_FLOAT_FUNCTION(name, op) \
  static int \
  name(Number n1, Number r) \
  { if ( !promoteToFloatNumber(n1) ) return FALSE; \
    r->value.f = op(n1->value.f); \
    r->type    = V_FLOAT; \
    return check_float(r); \
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
    if ( !same_type_numbers(n1, n2) ) \
      return FALSE; \
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
    if ( !same_type_numbers(n1, n2) ) \
      return FALSE; \
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
    return check_float(r); \
  }

UNAIRY_FLOAT_FUNCTION(ar_sin, sin)
UNAIRY_FLOAT_FUNCTION(ar_cos, cos)
UNAIRY_FLOAT_FUNCTION(ar_tan, tan)
UNAIRY_FLOAT_FUNCTION(ar_sinh, sinh)
UNAIRY_FLOAT_FUNCTION(ar_cosh, cosh)
UNAIRY_FLOAT_FUNCTION(ar_tanh, tanh)
UNAIRY_FLOAT_FUNCTION(ar_asinh, asinh)
UNAIRY_FLOAT_FUNCTION(ar_acosh, acosh)
UNAIRY_FLOAT_FUNCTION(ar_atanh, atanh)
UNAIRY_FLOAT_FUNCTION(ar_atan, atan)
UNAIRY_FLOAT_FUNCTION(ar_exp, exp)
UNAIRY_FLOAT_FUNCTION(ar_erf, erf)
UNAIRY_FLOAT_FUNCTION(ar_erfc, erfc)

BINAIRY_FLOAT_FUNCTION(ar_atan2, atan2)

BINAIRY_INT_FUNCTION(ar_disjunct,    "\\/", |, mpz_ior)
BINAIRY_INT_FUNCTION(ar_conjunct,    "/\\", &, mpz_and)
BINAIRY_INT_FUNCTION(ar_xor,         "xor", ^, mpz_xor)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ar_pow() is exponentiation. We do this in integers if possible. However,
GMP crashes the entire process by calling   abort() if it discovers that
the resulting value will not fit  in   the  address  space. Therefore we
estimage the size and verify that it will in on the global stack limit.

I doubt that the computation is accurate,   but it is highly unlikely we
won't run out of memory if we create an integer that requires almost the
complete stack size. It is also not a  problem if we underestimate a bit
as long as the result fits  in  the   address  space.  In that case, the
normal overflow handling will nicely generate a resource error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_GMP
static void
mpz_set_num(mpz_t mpz, Number n)
{ switch ( n->type )
  { case V_MPZ:
      mpz_set(mpz, n->value.mpz);
      break;
    case V_INTEGER:
      mpz_init_set_si64(mpz, n->value.i);
      break;
    default:
      assert(0);
  }
}

static int
get_int_exponent(Number n, unsigned long *expp)
{ long exp;
  int64_t i;

  switch(n->type)
  { case V_INTEGER:
      i = n->value.i;
      break;
    case V_MPZ:
      if ( !mpz_to_int64(n->value.mpz, &i) )
	return int_too_big();
      break;
    default:
      assert(0);
      return FALSE;
  }

  exp = (long)i;
#if SIZEOF_LONG < 8
  if ( (int64_t)exp != i )
    return int_too_big();
#endif

  if ( exp >= 0 )
    *expp = (unsigned long)exp;
  else if ( -exp != exp )
    *expp = (unsigned long)-exp;
  else
   return int_too_big();

  return TRUE;
}

/* cond_minus_pow() handles rounding mode issues calculating pow with
   negative base float have to reverse to_positive and to_negative.
*/

static double
cond_minus_pow(double base, double exp)
{ double res;

  if ( base < 0 )
  { switch( fegetround() )
    { case FE_UPWARD:
	fesetround(FE_DOWNWARD);
        res = -pow(-base,exp);
	fesetround(FE_UPWARD);
	break;
      case FE_DOWNWARD:
	fesetround(FE_UPWARD);
        res = -pow(-base,exp);
	fesetround(FE_DOWNWARD);
	break;
      default:
	res = -pow(-base,exp);
    }
  } else
  { res = pow(base,exp);
  }

  return res;
}


static int
ar_smallint(Number n, int *i)
{ switch(n->type)
  { case V_INTEGER:
      if ( n->value.i >= -1 && n->value.i <= 1 )
      { *i = n->value.i;
	return TRUE;
      }
      return FALSE;
    case V_MPZ:
      if ( mpz_cmp_si(n->value.mpz, -1L) >= 0 &&
	   mpz_cmp_si(n->value.mpz,  1L) <= 0 )
      { *i = mpz_get_si(n->value.mpz);
	return TRUE;
      }
      return FALSE;
    default:
      assert(0);
      return FALSE;
  }
}

#endif /*O_GMP*/

static inline int
sign_f(double f)
{ return
    f < 0 ? -1 :
    f > 0 ?  1 :
	     0 ;  /* sign_f(NaN) = 0 */
}

static int
ar_pow(Number n1, Number n2, Number r)
{ int zero_div_sign;
  int exp_sign;
#ifdef O_GMP
  unsigned long exp;
  int exp_nan;
  int n1_val;

  if ( n2->type == V_FLOAT )
  { exp_nan  = isnan(n2->value.f);
    exp_sign = sign_f(n2->value.f);
  } else
  { exp_nan = FALSE;
    exp_sign = ar_sign_i(n2);
  }
  r->type = V_INTEGER;				/* for all special cases */

  if ( exp_sign == 0 && !exp_nan )		/* test for X**0 */
  { r->value.i = 1;
    return TRUE;
  }

  if ( intNumber(n1) && ar_smallint(n1, &n1_val) )
  { if ( n1_val == 1 )				/* 1**X => 1 */
    { r->value.i = 1;
      return TRUE;
    }
    if ( n1_val == 0 && !exp_nan )		/* n1==0, non-zero(n2) */
    { if ( exp_sign > 0)
      { r->value.i = 0;				/* positive exp => 0 */
        return TRUE;
      } else					/* negative exp => zero_div */
      { return check_zero_div(ar_sign_i(n1), r, "**", 2);
      }
    }
    if ( n1_val == -1 && intNumber(n2) )	/* check n1 == -1 */
    { r->value.i = ar_even(n2) ? 1 : -1;
      return TRUE;
    }
  }

  if ( intNumber(n1) && intNumber(n2) )
  { if ( !get_int_exponent(n2, &exp) )
      return FALSE;

    if ( exp_sign < 0 )
    { GET_LD

      if ( truePrologFlag(PLFLAG_RATIONAL) )
      { promoteToMPQNumber(n1);
        goto int_pow_neg_int;
      }
      goto doreal;
    }

  { GET_LD				/* estimate the size, see above */
    size_t  op1_bits;
    int64_t r_bits;

    switch(n1->type)
    { case V_INTEGER:
	op1_bits = MSB64(n1->value.i);
        break;
      case V_MPZ:
	op1_bits = mpz_sizeinbase(n1->value.mpz, 2);
        break;
      default:
	assert(0);
        fail;
    }

    if ( !( mul64(op1_bits, exp, &r_bits) &&
	    r_bits/8 < (int64_t)globalStackLimit()
	  ) )
      return int_too_big();
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
  } /* end if ( intNumber(n1) && intNumber(n2) ) */

  if ( n1->type == V_MPQ && intNumber(n2) )
  { number nr, nd, nrp, ndp, nexp;

    if ( !get_int_exponent(n2, &exp) )
      return FALSE;

    if ( exp_sign == 0 )
    { r->type = V_INTEGER;
      r->value.i = 1;
      return TRUE;
    }

  int_pow_neg_int:
    nexp.type = V_INTEGER;
    nexp.value.i = exp;

    nr.type = V_MPZ;
    nr.value.mpz[0] = mpq_numref(n1->value.mpq)[0];
    nr.value.mpz->_mp_alloc = 0;	/* read-only */
    nd.type = V_MPZ;
    nd.value.mpz[0] = mpq_denref(n1->value.mpq)[0];
    nd.value.mpz->_mp_alloc = 0;

    if ( ar_pow(&nr, &nexp, &nrp) &&
         ar_pow(&nd, &nexp, &ndp) )
    { r->type = V_MPQ;
      mpq_init(r->value.mpq);
      if ( exp_sign > 0 )
      { mpz_set_num(mpq_numref(r->value.mpq), &nrp);
        mpz_set_num(mpq_denref(r->value.mpq), &ndp);
      } else
      { mpz_set_num(mpq_numref(r->value.mpq), &ndp);
        mpz_set_num(mpq_denref(r->value.mpq), &nrp);
      }
      mpq_canonicalize(r->value.mpq);

      clearNumber(&nrp);
      clearNumber(&ndp);

      return check_mpq(r);
    }

    clearNumber(&nrp);
    clearNumber(&ndp);
  } /* end MPQ^int */

  if ( n2->type == V_MPQ )			/* X ^ rat */
  { long r_den;

    if ( exp_sign == -1 )
      mpz_neg(mpq_numref(n2->value.mpq), mpq_numref(n2->value.mpq));

    r_den = mpz_get_ui(mpq_denref(n2->value.mpq));

    switch (n1->type)
    { case V_INTEGER:
      { mpz_init_set_si(r->value.mpz,n1->value.i);
        goto int_to_rat;
      }
      case V_MPZ:
      { mpz_init_set(r->value.mpz,n1->value.mpz);

      int_to_rat:
	r->type = V_MPZ;			/* int ^ rat */
						/* neg ^ int/even is undefined */
        if ( mpz_sgn(r->value.mpz) == -1 && !(r_den & 1))
	{ mpz_clear(r->value.mpz);
	  r->type = V_FLOAT;
	  r->value.f = const_nan;
	  return check_float(r);
	}

        if ( mpz_root(r->value.mpz,r->value.mpz,r_den))
        { unsigned long r_num = mpz_get_ui(mpq_numref(n2->value.mpq));

          if ( r_num > LONG_MAX )	/* numerator exceeds mpz_pow_ui range */
          { mpz_clear(r->value.mpz);
            if ( promoteToFloatNumber(n1) )
              goto doreal_mpq;
            else return FALSE;
          } else
          { mpz_pow_ui(r->value.mpz,r->value.mpz,r_num);

            if (exp_sign == -1)		/* create mpq=1/r->value */
            { mpz_t tempz;

              mpz_init_set(tempz,r->value.mpz);
              mpz_clear(r->value.mpz);
              r->type = V_MPQ;
              mpq_init(r->value.mpq);
              mpq_set_z(r->value.mpq,tempz);
              mpq_inv(r->value.mpq,r->value.mpq);
              mpz_clear(tempz);
              return check_mpq(r);
            } else
            { return TRUE;
            }
          }
        } else				/* root inexact */
        { mpz_clear(r->value.mpz);
          if ( promoteToFloatNumber(n1) )
            goto doreal_mpq;
          else return FALSE;
        }
        break;
      }
      case V_MPQ:
      { int rat_result;
        unsigned long r_num;

        r->type = V_MPQ;
        mpq_init(r->value.mpq);
        mpq_set(r->value.mpq, n1->value.mpq);

					/* neg ^ int / even */
        if ( (mpq_sgn(r->value.mpq) == -1 ) && !(r_den & 1))
        { mpq_clear(r->value.mpq);
          r->type = V_FLOAT;
          r->value.f = const_nan;
          return check_float(r);
        }

        rat_result = ( mpz_root(mpq_numref(r->value.mpq),
				mpq_numref(r->value.mpq),r_den) &&
		       mpz_root(mpq_denref(r->value.mpq),
				mpq_denref(r->value.mpq),r_den)
		     );

        r_num = mpz_get_ui(mpq_numref(n2->value.mpq));
        if ( rat_result && (r_num < LONG_MAX) )	/* base = base^P */
        { mpz_pow_ui(mpq_numref(r->value.mpq),mpq_numref(r->value.mpq),r_num);
          mpz_pow_ui(mpq_denref(r->value.mpq),mpq_denref(r->value.mpq),r_num);

          if ( exp_sign == -1 )
            mpq_inv(r->value.mpq,r->value.mpq);

          return check_mpq(r);
        } else				/* exponent out of range for mpz_pow_ui */
        { mpq_clear(r->value.mpq);

          if ( promoteToFloatNumber(n1) )
	    goto doreal_mpq;
          else
	    return FALSE;
        }
        assert(0);
      }
      case V_FLOAT:
      { if ( n1->value.f == 0.0 ) goto doreal;  /* general case of 0.0**X */
        if ( n1->value.f < 0  && !( r_den & 1 ))
	{ r->value.f = const_nan;	/* negative base, even denominator */
	} else
        {
	doreal_mpq:
	  mpq_init(r->value.mpq);
	  mpq_set_ui(r->value.mpq,1,mpz_get_ui(mpq_denref(n2->value.mpq)));
	  double dexp = mpq_get_d(r->value.mpq);  /* float(1/n2.den) */
	  mpq_clear(r->value.mpq);
	  r->value.f = pow(cond_minus_pow(n1->value.f, dexp),
			   mpz_get_ui(mpq_numref(n2->value.mpq)));
	  if ( exp_sign == -1 )
	    r->value.f = 1.0/r->value.f;
        }

        r->type = V_FLOAT;
        return check_float(r);
      }
    } /* end switch (n1->type) */
    assert(0);
  } /* end if ( n2->type == V_MPQ ) */

doreal:
#endif /*O_GMP*/
  zero_div_sign = ( (n2->type == V_INTEGER) && (!ar_even(n2)) &&
		    signbit(n1->value.f) ) ? -1 : 1;

  if ( !promoteToFloatNumber(n1) ||
       !promoteToFloatNumber(n2) )
    return FALSE;

#ifndef O_GMP
  exp_sign = sign_f(n2->value.f);
#endif

  if ( n1->value.f == 0.0 && exp_sign == -1 )
    return check_zero_div(zero_div_sign, r, "**", 2);
  if ( n1->value.f == 1.0 )
    r->value.f = 1.0;			/* broken on e.g., mipsel */
  else
    r->value.f = pow(n1->value.f, n2->value.f);
  r->type = V_FLOAT;

  return check_float(r);
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

  if ( ar_sign_i(base) < 0 ) return notLessThanZero("powm", 3, base);
  if ( ar_sign_i(exp)  < 0 ) return notLessThanZero("powm", 3, exp);
  if ( ar_sign_i(mod) <= 0 ) return  mustBePositive("powm", 3, mod);

  r->type = V_MPZ;
  mpz_init(r->value.mpz);
  mpz_powm(r->value.mpz, base->value.mpz, exp->value.mpz, mod->value.mpz);

  succeed;
#else
  return PL_error("powm", 3, "requires unbounded arithmetic (GMP) support",
		  ERR_NOT_IMPLEMENTED, "powm/3");
#endif
}

#if 0
/* These tests originate from the days that float errors used
 * to be signalling on many systems.  Nowadays this is no longer
 * the case.  We leave the code in for just-in-case.
 */
#define AR_UNDEFINED_IF(func, arity, test, r)			\
	if ( test )						\
	{ GET_LD						\
	  if ( LD->arith.f.flags & FLT_UNDEFINED )		\
	  { r->type = V_FLOAT;					\
	    r->value.f = const_nan;				\
	    return TRUE;					\
	  } else						\
	  { return PL_error(func, arity, NULL, ERR_AR_UNDEF);	\
	  }							\
	}
#define AR_DIV_ZERO_IF(func, arity, n, d, r)			\
	if ( d == 0.0 )						\
	{ GET_LD						\
	  if ( LD->arith.f.flags & FLT_ZERO_DIV )		\
	  { r->type = V_FLOAT;					\
	    r->value.f = signbit(n) == signbit(d)		\
			? const_inf				\
			: const_neg_inf;			\
	    return TRUE;					\
	  } else						\
	  { return PL_error(func, arity, NULL, ERR_DIV_BY_ZERO);\
	  }							\
	}
#else
#define AR_UNDEFINED_IF(func, arity, test, r) (void)0
#define AR_DIV_ZERO_IF(func, arity, n, d, r)  (void)0
#endif

static int
ar_sqrt(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  AR_UNDEFINED_IF("sqrt", 1,  n1->value.f < 0, r);
  r->value.f = sqrt(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r);
}


static int
ar_asin(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  AR_UNDEFINED_IF("asin", 1, n1->value.f < -1.0 || n1->value.f > 1.0, r);
  r->value.f = asin(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r);
}


static int
ar_acos(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  AR_UNDEFINED_IF("ascos", 1, n1->value.f < -1.0 || n1->value.f > 1.0, r);
  r->value.f = acos(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r);
}


static int
ar_log(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  AR_UNDEFINED_IF("log", 1, n1->value.f <= 0.0 , r);
  r->value.f = log(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r);
}


static int
ar_log10(Number n1, Number r)
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  AR_UNDEFINED_IF("log10", 1, n1->value.f <= 0.0, r);
  r->value.f = log10(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r);
}


static int
ar_lgamma(Number n1, Number r)  // custom lgamma() to ensure positive inf
{ if ( !promoteToFloatNumber(n1) )
    return FALSE;
  r->value.f = (n1->value.f <= 0.0) ? INFINITY : lgamma(n1->value.f);
  r->type    = V_FLOAT;

  return check_float(r);
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
    return PL_error("div", 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error("div", 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

#ifdef O_GMP
  if ( n1->type == V_INTEGER && n2->type == V_INTEGER )
#endif
  { if ( n2->value.i == 0 )
      return PL_error("div", 2, NULL, ERR_DIV_BY_ZERO);

    if ( !(n2->value.i == -1 && n1->value.i == PLMININT) )
    { r->value.i = n1->value.i / n2->value.i;
      if ((n1->value.i > 0) != (n2->value.i > 0) &&
          n1->value.i % n2->value.i != 0)
        --r->value.i;
      r->type = V_INTEGER;

      succeed;
    }
  }

#ifdef O_GMP
  promoteToMPZNumber(n1);
  promoteToMPZNumber(n2);

  if ( mpz_sgn(n2->value.mpz) == 0 )
    return PL_error("div", 2, NULL, ERR_DIV_BY_ZERO);

  r->type = V_MPZ;
  mpz_init(r->value.mpz);
  mpz_fdiv_q(r->value.mpz, n1->value.mpz, n2->value.mpz);

  succeed;
#else
  return PL_error("div", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
}

/* Broken, at least on SunOS 5.11, gcc 4.8.  No clue under what conditions.
   The results of configure and final linking differ.  Anyway, just doing
   our own is most likely the safe solution.
 */
#ifdef __sun
#undef HAVE_SIGNBIT
#endif

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
  { r->value.f = isnan(n1->value.f) ? const_nan :
       (n1->value.f < 0 ? -1.0 : n1->value.f > 0.0 ? 1.0 : 0.0);
    r->type = V_FLOAT;
  } else
  { r->value.i = ar_sign_i(n1);
    r->type = V_INTEGER;
  }

  succeed;
}


int
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
  { if ( isnan(n1->value.f) )
      r->value.f = const_nan;
    else
      r->value.f = copysign(n1->value.f, n2->value.f);
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
ar_nexttoward(Number n1, Number n2, Number r)
{ if ( promoteToFloatNumber(n1) &&
       promoteToFloatNumber(n2) )
  { if ( n1->type == V_FLOAT && n2->type == V_FLOAT )
    { r->value.f = nexttoward(n1->value.f, n2->value.f);
      r->type = V_FLOAT;

      return check_float(r);
    }
  }

  return FALSE;
}

static int
set_roundtoward(DECL_LD Word p, Number old)
{ deRef(p);

  old->type = V_INTEGER;
  old->value.i = fegetround();

  if ( *p == ATOM_to_nearest )
    fesetround(FE_TONEAREST);
  else if ( *p == ATOM_to_positive )
    fesetround(FE_UPWARD);
  else if ( *p == ATOM_to_negative )
    fesetround(FE_DOWNWARD);
  else if ( *p == ATOM_to_zero )
    fesetround(FE_TOWARDZERO);
  else if ( isAtom(*p) )
    return PL_error(NULL, 0, NULL, ERR_PTR_DOMAIN, ATOM_round, p);
  else
    return PL_error(NULL, 0, NULL, ERR_PTR_TYPE, ATOM_atom, p);

  return TRUE;
}

static int
ar_roundtoward(Number n1, Number n2, Number r)
{ cpNumber(r, n1);

  assert(n2->type == V_INTEGER);
  fesetround(n2->value.i);

  return TRUE;
}


static int
ar_rem(Number n1, Number n2, Number r)
{ if ( !toIntegerNumber(n1, 0) )
    return PL_error("rem", 2, NULL, ERR_AR_TYPE, ATOM_integer, n1);
  if ( !toIntegerNumber(n2, 0) )
    return PL_error("rem", 2, NULL, ERR_AR_TYPE, ATOM_integer, n2);

  if ( !same_type_numbers(n1, n2) )
    return FALSE;
  switch(n1->type)
  { case V_INTEGER:
      if ( n2->value.i == 0 )
	return PL_error("rem", 2, NULL, ERR_DIV_BY_ZERO);

      if ( n2->value.i != -1 || n1->value.i != INT64_MIN )
	r->value.i = n1->value.i % n2->value.i;
      else
	r->value.i = 0;
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
  return promoteToMPQNumber(r);
}

static int
ar_numerator(Number n1, Number r)
{ if ( intNumber(n1) )
  { cpNumber(r, n1);
    return TRUE;
  }
  if ( n1->type == V_MPQ )
  { r->type = V_MPZ;
    mpz_init(r->value.mpz);
    mpz_set(r->value.mpz, mpq_numref(n1->value.mpq));
    return TRUE;
  }

  return PL_error("numerator", 1, NULL, ERR_AR_TYPE, ATOM_rational, n1);
}


static int
ar_denominator(Number n1, Number r)
{ if ( intNumber(n1) )
  { r->type = V_INTEGER;
    r->value.i = 1;
    return TRUE;
  }
  if ( n1->type == V_MPQ )
  { r->type = V_MPZ;
    mpz_init(r->value.mpz);
    mpz_set(r->value.mpz, mpq_denref(n1->value.mpq));
    return TRUE;
  }

  return PL_error("denominator", 1, NULL, ERR_AR_TYPE, ATOM_rational, n1);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A is rationalize(Float)

Introduced on the suggestion of Richard   O'Keefe  after the Common Lisp
standard. The algorithm is taken from figure  3 in ``A Rational Rotation
Method for Robust Geometric Algorithms'' by John Canny, Bruce Donald and
Eugene K. Ressler.  Found at

http://www.cs.dartmouth.edu/~brd/papers/rotations-scg92.pdf

(*) Comment by Keri Harris:

The result of p1/q1 is retained  in  a   FP  stack  register at a higher
precision (80 bits); it  is  not  stored   in  a  variable.  This  extra
precision skews the results when  preforming   the  subtraction,  as one
operand contains extra precision:

        (extended double precision)     (double precision)
    d =           p1/q1              -     n1->value.f;

Forcing the result of p1/q1 to be stored in a variable produces expected
results with rationalize/1:

    volatile double p1_q1 = p1/q1;
    d = p1_q1 - n1->value.f;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
ar_rationalize(Number n1, Number r)
{ switch(n1->type)
  { case V_INTEGER:
    case V_MPZ:
    case V_MPQ:
      cpNumber(r, n1);
      return TRUE;
    case V_FLOAT:
      switch(fpclassify(n1->value.f))
      { case FP_NAN:
	  return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
        case FP_INFINITE:
	  return PL_error(NULL, 0, NULL, ERR_AR_RAT_OVERFLOW);
      }

      mpq_init(r->value.mpq);
      mpq_set_double(r->value.mpq, n1->value.f);
      r->type = V_MPQ;
      return check_mpq(r);
  }

  assert(0);
  fail;
}


int
ar_rdiv_mpz(Number n1, Number n2, Number r)
{ if ( mpz_divisible_p(n1->value.mpz, n2->value.mpz) )
  { mpz_init(r->value.mpz);
    r->type = V_MPZ;
    mpz_divexact(r->value.mpz, n1->value.mpz, n2->value.mpz);
  } else
  { r->type = V_MPQ;
    mpq_init(r->value.mpq);
    mpz_set(mpq_numref(r->value.mpq), n1->value.mpz);
    mpz_set(mpq_denref(r->value.mpq), n2->value.mpz);
    mpq_canonicalize(r->value.mpq);
    return check_mpq(r);
  }

  return TRUE;
}


static int
ar_rdiv(Number n1, Number n2, Number r)
{ if ( toIntegerNumber(n1, 0) &&
       toIntegerNumber(n2, 0) )
  { promoteToMPZNumber(n1);
    promoteToMPZNumber(n2);

    if ( mpz_sgn(n2->value.mpz) == 0 )
      return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);

    return ar_rdiv_mpz(n1, n2, r);
  } else if ( ratNumber(n1) && ratNumber(n2) )
  { promoteToMPQNumber(n1);
    promoteToMPQNumber(n2);

    if ( mpz_sgn(mpq_numref(n2->value.mpq)) == 0 )
      return PL_error("/", 2, NULL, ERR_DIV_BY_ZERO);

    r->type = V_MPQ;
    mpq_init(r->value.mpq);
    mpq_div(r->value.mpq, n1->value.mpq, n2->value.mpq);
    return check_mpq(r);
  } else if ( !ratNumber(n1) )
  { return PL_error("rdiv", 2, NULL, ERR_AR_TYPE, ATOM_rational, n1);
  } else
  { return PL_error("rdiv", 2, NULL, ERR_AR_TYPE, ATOM_rational, n2);
  }

  return TRUE;
}
#endif /*O_GMP*/


static int
ar_divide(Number n1, Number n2, Number r)
{ GET_LD

  if ( (n2->type == V_FLOAT) && isinf(n2->value.f) )  // X/inf
  { if (n1->type == V_FLOAT)    // float --> signed 0.0 or NaN
    { r->type = V_FLOAT;
      r->value.f = isfinite(n1->value.f) ? 0.0*sign_f(n1->value.f)*sign_f(n2->value.f) : const_nan;
      return check_float(r);
    } else                      // non-float --> 0
    { r->type = V_INTEGER;
      r->value.i = 0;
      succeed;
    }
  }

  if ( !truePrologFlag(PLFLAG_ISO) )
  { if ( !same_type_numbers(n1, n2) )
      return FALSE;

    switch(n1->type)
    { case V_INTEGER:
	if ( n2->value.i == LL(0) )
	  return check_zero_div(ar_sign_i(n1), r, "/", 2);
        if ( n1->value.i % n2->value.i == 0 )
	{ r->value.i = n1->value.i / n2->value.i;
	  r->type = V_INTEGER;
	  succeed;
	}
#ifdef O_GMP
	if ( truePrologFlag(PLFLAG_RATIONAL) )
	{ promoteToMPZNumber(n1);
	  promoteToMPZNumber(n2);
	  return ar_rdiv_mpz(n1, n2, r);
	}
#endif
	break;
#ifdef O_GMP
      case V_MPZ:
	if ( mpz_sgn(n2->value.mpz) == 0 )
	  return check_zero_div(ar_sign_i(n1), r, "/", 2);
	if ( mpz_divisible_p(n1->value.mpz, n2->value.mpz) )
	{ mpz_init(r->value.mpz);
	  r->type = V_MPZ;
	  mpz_divexact(r->value.mpz, n1->value.mpz, n2->value.mpz);
	  succeed;
	}
	if ( truePrologFlag(PLFLAG_RATIONAL) )
	  return ar_rdiv_mpz(n1, n2, r);
        break;
      case V_MPQ:
	if ( mpq_sgn(n2->value.mpq) == 0 )
	  return check_zero_div(ar_sign_i(n1), r, "/", 2);
        mpq_init(r->value.mpq);
	r->type = V_MPQ;
	mpq_div(r->value.mpq, n1->value.mpq, n2->value.mpq);
	return check_mpq(r);
#endif
      case V_FLOAT:
	break;
    }
  } // ! PLAG_ISO

					/* TBD: How to handle Q? */
  if ( !promoteToFloatNumber(n1) ||
       !promoteToFloatNumber(n2) )
    return FALSE;

  /* separate zero-div case from general overflow, Note: sign_f(nan)=0 */
  if ( (n2->value.f == 0.0) && (sign_f(n1->value.f) != 0) )
    return check_zero_div((signbit(n1->value.f)==signbit(n2->value.f)) ? 1 : -1,
			  r, "/", 2);

  r->value.f = n1->value.f / n2->value.f;
  r->type = V_FLOAT;

  return check_float(r);
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
      { ay = -(uint64_t)y;
	sign = -1;
      }
    } else
    { ax = -(uint64_t)x;
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
      prod = -(uint64_t)prod;
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
{ if ( !same_type_numbers(n1, n2) )
    return FALSE;

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
      return check_mpq(r);
#else
      return PL_error("*", 2, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
    case V_FLOAT:
      r->value.f = n1->value.f * n2->value.f;
      r->type = V_FLOAT;

      return check_float(r);
  }

  assert(0);
  fail;
}


/* min/2 and max/2 have two special cases.  If one of the arguments is
 * NaN we must select the other and for these functions -0.0 < 0.0,
 * while they compare == for normal float comparison.
 */

static int
is_min_zero(const Number n)
{ return n->type == V_FLOAT && n->value.f == 0.0 && signbit(n->value.f);
}

static int
ar_max(Number n1, Number n2, Number r)
{ int diff = cmpNumbers(n1, n2);

  if ( diff == CMP_NOTEQ )			/* one or both nan */
  { if ( n1->type == V_FLOAT && isnan(n1->value.f) )
      cpNumber(r, n2);
    else
      cpNumber(r, n1);
  } else if ( diff == CMP_EQUAL )
  { if ( is_min_zero(n1) )
    { cpNumber(r, n2);
    } else if ( is_min_zero(n2) )
    { cpNumber(r, n1);
    } else
    { if ( !make_same_type_numbers(n1, n2) )
	return FALSE;
      cpNumber(r, n1);
    }
  } else if ( diff > 0 )
  { cpNumber(r, n1);
  } else
  { cpNumber(r, n2);
  }

  return TRUE;
}


static int
ar_min(Number n1, Number n2, Number r)
{ int diff = cmpNumbers(n1, n2);

  if ( diff == CMP_NOTEQ )			/* if one or both nan's */
  { if (n1->type == V_FLOAT && isnan(n1->value.f))
      cpNumber(r, n2);
    else
      cpNumber(r, n1);
  } else if ( diff == CMP_EQUAL )
  { if ( is_min_zero(n1) )
    { cpNumber(r, n1);
    } else if ( is_min_zero(n2) )
    { cpNumber(r, n2);
    } else
    { if ( !make_same_type_numbers(n1, n2) )
	return FALSE;
      cpNumber(r, n1);
    }
  } else if ( diff < 0 )
  { cpNumber(r, n1);
  } else
  { cpNumber(r, n2);
  }

  return TRUE;
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

      r->value.i = MSB64(n1->value.i);
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
{
#ifdef HAVE__BUILTIN_POPCOUNT
  return __builtin_popcountll(i);
#else
  int c;
  size_t j;
  int64_t m = LL(1);

  for(j=0,c=0; j<sizeof(i)*8; j++, m<<=1)
  { if ( i&m )
      c++;
  }

  return c;
#endif
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

/* bit(I,K) is the K-th bit of I
*/

#ifndef HAVE_MP_BITCNT_T
typedef unsigned long mp_bitcnt_t;
#endif

#define MP_BITCNT_T_MIN 0
#define MP_BITCNT_T_MAX (~(mp_bitcnt_t)0)

static int
ar_getbit(Number I, Number K, Number r)
{ mp_bitcnt_t bit;

  if ( !toIntegerNumber(I, 0) )
    return PL_error("bit", 2, NULL, ERR_AR_TYPE, ATOM_integer, I);
  if ( !toIntegerNumber(K, 0) )
    return PL_error("bit", 2, NULL, ERR_AR_TYPE, ATOM_integer, K);

  switch(K->type)
  { case V_INTEGER:
      if ( K->value.i < 0 )
	return notLessThanZero("bit", 2, K);
      if ( sizeof(mp_bitcnt_t) < 8 &&
	   K->value.i > MP_BITCNT_T_MAX )
      { too_large:
	r->value.i = 0;
	r->type    = V_INTEGER;
	return TRUE;
      }
      bit = K->value.i;
      break;
#ifdef O_GMP
    case V_MPZ:
      if ( mpz_sgn(K->value.mpz) < 0 )
	return notLessThanZero("bit", 2, K);
      if ( mpz_cmp_ui(K->value.mpz, MP_BITCNT_T_MAX) > 0 )
	goto too_large;
      bit = mpz_get_ui(K->value.mpz);
      break;
#endif
    default:
      bit = 0;
      assert(0);
  }

  switch(I->type)
  { case V_INTEGER:
      if (  I->value.i < 0 )
	return notLessThanZero("bit", 2, I);

      if ( bit >= 8*sizeof(I->value.i) )
	goto too_large;
      r->value.i = (I->value.i & ((int64_t)1<<bit)) != 0;
      r->type    = V_INTEGER;
      return TRUE;
#ifdef O_GMP
    case V_MPZ:
      if ( mpz_sgn(I->value.mpz) < 0 )
	return notLessThanZero("bit", 2, I);

      r->value.i = mpz_tstbit(I->value.mpz, bit);
      r->type = V_INTEGER;
      return TRUE;
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
      return check_mpq(r);
#endif
    case V_FLOAT:
      r->value.f = isnan(n1->value.f) ? n1->value.f : -n1->value.f;
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
      { r->value.i = llabs(n1->value.i);
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
      return TRUE;
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
      return TRUE;
    }
#endif
    case V_FLOAT:
    { if ( isnan(n1->value.f) || isinf(n1->value.f) )
      { cpNumber(r, n1);
	return TRUE;
      }

      if ( n1->value.f <= (float)PLMAXINT && n1->value.f >= (float)PLMININT )
      {
#if SIZEOF_LONG == 8
	r->value.i = lround(n1->value.f);
#else
#if LLROUND_OK				/* Broken on MinGW (11.2) */
        r->value.i = llround(n1->value.f) ;
#else
        /* The fix is to add not +/- 0.5, but +/- nextafter(0.5,-1.)
         * See https://gcc.gnu.org/ml/gcc-patches/2006-10/msg00917.html and follow-ups
         */
        r->value.i = n1->value.f + copysign(nexttoward(0.5, -1.0), n1->value.f) ;
#endif
#endif
	if ( n1->value.f > 0 && r->value.i < 0 )
	  r->value.i = PLMAXINT;
	else if ( n1->value.f < 0 && r->value.i > 0 )
	  r->value.i = PLMININT;

	r->type = V_INTEGER;
	return TRUE;
      }
#ifdef O_GMP
      r->type = V_MPZ;
      mpz_init_set_d(r->value.mpz, n1->value.f);
      return TRUE;
#else
#ifdef HAVE_RINT
      r->value.f = rint(n1->value.f);
      r->type = V_FLOAT;
      return TRUE;
#else
      return PL_error("integer", 1, NULL, ERR_EVALUATION, ATOM_int_overflow);
#endif
#endif
    }
  }

  assert(0);
  return FALSE;
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
    { if ( isnan(n1->value.f) || isinf(n1->value.f) )
      { cpNumber(r, n1);
	return TRUE;
      }
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
	     mpX_round(mpz_get_d(r->value.mpz)) > n1->value.f )
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
    { if ( isnan(n1->value.f) || isinf(n1->value.f) )
      { cpNumber(r, n1);
	return TRUE;
      }
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
	if ( mpX_round(mpz_get_d(r->value.mpz)) < n1->value.f )
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

#ifdef MODF_OK
      r->value.f = modf(n1->value.f, &ip);
#else
      /* return -0.0 for fractional part of -0.0 (IEEE754) */
      r->value.f = copysign(modf(n1->value.f, &ip), n1->value.f);
#endif
      r->type = V_FLOAT;
      return check_float(r);
    }
    default:
      assert(0);
      return FALSE;
  }
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
      return check_float(r);
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
      if ( isnan(n1->value.f) || isinf(n1->value.f) )
      { cpNumber(r, n1);
	return TRUE;
      }
      if ( n1->value.f >= 0.0 )
	return ar_floor(n1, r);
      else
	return ar_ceil(n1, r);
    default:
      cpNumber(r, n1);
      return TRUE;
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

#define seed_from_dev(dev) LDFUNC(seed_from_dev, dev)
static int
seed_from_dev(DECL_LD const char *dev)
{ int done = FALSE;
#if defined(S_ISCHR) && !defined(__WINDOWS__)
  int fd;

  if ( (fd=open(dev, O_RDONLY)) != -1 )
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



#define seed_from_crypt_context(_) LDFUNC(seed_from_crypt_context, _)
static int
seed_from_crypt_context(DECL_LD)
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
  return FALSE;
#endif
}


#define seed_random(_) LDFUNC(seed_random, _)
static void
seed_random(DECL_LD)
{ if ( !seed_from_dev("/dev/urandom") &&
       !seed_from_dev("/dev/random") &&
       !seed_from_crypt_context() )
  { union
    { double t;
      unsigned long l[sizeof(double)/sizeof(long)];
    } u;
    unsigned long key = 0;
    int i;

    u.t = WallTime();

    for(i=0; i<sizeof(double)/sizeof(long); i++)
      key ^= u.l[i];

    LD->gmp.persistent++;
    gmp_randseed_ui(LD->arith.random.state, key);
    LD->gmp.persistent--;
  }
}

#else /* O_GMP */

#define seed_random(_) LDFUNC(seed_random, _)
static void
seed_random(DECL_LD)
{ setRandom(NULL);
}

#endif /*O_GMP*/

#define init_random(_) LDFUNC(init_random, _)
static void
init_random(DECL_LD)
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
    seed_random();
    LD->gmp.persistent--;
  }
#endif
}


static
PRED_IMPL("set_random", 1, set_random, 0)
{ PRED_LD
  atom_t name;
  size_t arity;

  init_random();

  if ( PL_get_name_arity(A1, &name, &arity) && arity == 1 )
  { term_t arg = PL_new_term_ref();

    _PL_get_arg(1, A1, arg);
    if ( name == ATOM_seed )
    { atom_t a;

      if ( PL_get_atom(arg, &a) && a == ATOM_random )
      { seed_random();
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
  size_t arity;

  init_random();

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

  init_random();

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

  init_random();

  do
  {
#ifdef O_GMP
    mpf_t rop;
    mpf_init2(rop, sizeof(double)*8);
    mpf_urandomb(rop, LD->arith.random.state, sizeof(double)*8);
    r->value.f = mpX_round(mpf_get_d(rop));
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
{ r->value.f = (fegetround() == FE_UPWARD) ? nexttoward(M_PI,INFINITY) : M_PI;

  r->type = V_FLOAT;
  succeed;
}


static int
ar_e(Number r)
{ r->value.f = (fegetround() == FE_UPWARD) ? nexttoward(M_E,INFINITY) : M_E;

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
ar_inf(Number r)
{ static number n = {0};

  if ( n.type != V_FLOAT )
  { n.value.f = const_inf;
    n.type = V_FLOAT;
  }

  *r = n;

  succeed;
}


static int
ar_nan(Number r)
{ static number n = {0};

  if ( n.type != V_FLOAT )
  { n.value.f = const_nan;
    n.type = V_FLOAT;
  }

  *r = n;

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) valueExpression() cannot use GC, but can return a number whose value
is a GPM number pointing at  the  global   stack.  If  this is the case,
PL_unify_number() may not invoke GC, so we  must check that we have room
for the required attribute wakeup and trailing before we start.

is/2 is the  only  victim  of  this   issue,  as  the  other  arithmetic
predicates (>/2, etc.) only use their arguments as inputs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("is", 2, is, PL_FA_ISO)	/* -Value is +Expr */
{ PRED_LD
  AR_CTX
  number arg;
  int rc;

  if ( !hasGlobalSpace(0) )		/* see (*) */
  { if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  AR_BEGIN();
  if ( (rc=valueExpression(A2, &arg)) )
  { rc = PL_unify_number(A1, &arg);
    clearNumber(&arg);
    AR_END();
  } else
  { AR_CLEANUP();
  }

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
  ADD(FUNCTOR_numerator1,	ar_numerator, 0),
  ADD(FUNCTOR_denominator1,	ar_denominator, 0),
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
  ADD(FUNCTOR_div2,		ar_div, F_ISO),		/* div/2 */
  ADD(FUNCTOR_gdiv2,		ar_tdiv, 0),		/* (//)/2 */
  ADD(FUNCTOR_gcd2,		ar_gcd, 0),
  ADD(FUNCTOR_lcm2,		ar_lcm, 0),
  ADD(FUNCTOR_sign1,		ar_sign, F_ISO),

  ADD(FUNCTOR_and2,		ar_conjunct, F_ISO),
  ADD(FUNCTOR_bitor2,		ar_disjunct, F_ISO),
  ADD(FUNCTOR_rshift2,		ar_shift_right, F_ISO),
  ADD(FUNCTOR_lshift2,		ar_shift_left, F_ISO),
  ADD(FUNCTOR_xor2,		ar_xor, F_ISO),
  ADD(FUNCTOR_backslash1,	ar_negation, F_ISO),

  ADD(FUNCTOR_random1,		ar_random, 0),
  ADD(FUNCTOR_random_float0,	ar_random_float, 0),

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
  ADD(FUNCTOR_nexttoward2,	ar_nexttoward, 0),
  ADD(FUNCTOR_roundtoward2,	ar_roundtoward, 0),

  ADD(FUNCTOR_sqrt1,		ar_sqrt, F_ISO),
  ADD(FUNCTOR_sin1,		ar_sin, F_ISO),
  ADD(FUNCTOR_cos1,		ar_cos, F_ISO),
  ADD(FUNCTOR_tan1,		ar_tan, F_ISO),
  ADD(FUNCTOR_asin1,		ar_asin, F_ISO),
  ADD(FUNCTOR_acos1,		ar_acos, F_ISO),
  ADD(FUNCTOR_atan1,		ar_atan, F_ISO),
  ADD(FUNCTOR_atan2,		ar_atan2, 0),
  ADD(FUNCTOR_atan22,		ar_atan2, F_ISO),
  ADD(FUNCTOR_sinh1,		ar_sinh, 0),
  ADD(FUNCTOR_cosh1,		ar_cosh, 0),
  ADD(FUNCTOR_tanh1,		ar_tanh, 0),
  ADD(FUNCTOR_asinh1,		ar_asinh, 0),
  ADD(FUNCTOR_acosh1,		ar_acosh, 0),
  ADD(FUNCTOR_atanh1,		ar_atanh, 0),
  ADD(FUNCTOR_lgamma1,		ar_lgamma, 0),
  ADD(FUNCTOR_log1,		ar_log, F_ISO),
  ADD(FUNCTOR_erf1,		ar_erf, 0),
  ADD(FUNCTOR_erfc1,		ar_erfc, 0),
  ADD(FUNCTOR_exp1,		ar_exp, F_ISO),
  ADD(FUNCTOR_log101,		ar_log10, 0),
  ADD(FUNCTOR_hat2,		ar_pow, F_ISO),
  ADD(FUNCTOR_doublestar2,	ar_pow, F_ISO),
  ADD(FUNCTOR_pi0,		ar_pi, F_ISO),
  ADD(FUNCTOR_e0,		ar_e, 0),
  ADD(FUNCTOR_epsilon0,		ar_epsilon, 0),
  ADD(FUNCTOR_inf0,		ar_inf, 0),
  ADD(FUNCTOR_nan0,		ar_nan, 0),

  ADD(FUNCTOR_cputime0,		ar_cputime, 0),
  ADD(FUNCTOR_msb1,		ar_msb, 0),
  ADD(FUNCTOR_lsb1,		ar_lsb, 0),
  ADD(FUNCTOR_popcount1,	ar_popcount, 0),
  ADD(FUNCTOR_getbit2,		ar_getbit, 0),
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
    { size_t size = 512;

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


static atom_t float_rounding_names[5] = {0};

static double
nan15(void)
{ number n;
  unsigned char *end;

  if ( str_number((const unsigned char *)"1.5NaN", &end, &n, 0) == NUM_OK )
  { assert(isnan(n.value.f));
    return n.value.f;
  } else
  { assert(0);
    return NAN;
  }
}


double
PL_nan(void)
{ return const_nan;
}


void
initArith(void)
{ GET_LD
  registerBuiltinFunctions();

#ifdef O_INHIBIT_FP_SIGNALS
  fpsetmask(fpgetmask() & ~(FP_X_DZ|FP_X_INV|FP_X_OFL));
#endif

  const_nan     = nan15();
  const_inf     = HUGE_VAL;
  const_neg_inf = -HUGE_VAL;

#ifdef O_GMP
  LD->arith.rat.max_rational_size = (size_t)-1;
  LD->arith.rat.max_rational_size_action = ATOM_error;

  setPrologFlag("max_rational_size",	    FT_INTEGER, -1);
  setPrologFlag("max_rational_size_action", FT_ATOM,    "error");
#endif

  LD->arith.f.flags = FLT_ROUND_NEAREST|FLT_UNDERFLOW;
  setPrologFlag("float_overflow",  FT_ATOM, "error");
  setPrologFlag("float_zero_div",  FT_ATOM, "error");
  setPrologFlag("float_undefined", FT_ATOM, "error");
  setPrologFlag("float_underflow", FT_ATOM, "ignore");
  setPrologFlag("float_rounding",  FT_ATOM, "to_nearest");
  float_rounding_names[FLT_ROUND_NEAREST] = ATOM_to_nearest;
  float_rounding_names[FLT_ROUND_TO_POS]  = ATOM_to_positive;
  float_rounding_names[FLT_ROUND_TO_NEG]  = ATOM_to_negative;
  float_rounding_names[FLT_ROUND_TO_ZERO] = ATOM_to_zero;
  setPrologFlag("float_min",  FT_FLOAT|FF_READONLY, DBL_MIN);
  setPrologFlag("float_max",  FT_FLOAT|FF_READONLY, DBL_MAX);
  setPrologFlag("float_max_integer",  FT_FLOAT|FF_READONLY, 9007199254740992.0);
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

int
is_arith_flag(atom_t k)
{ return (
#ifdef O_GMP
           k == ATOM_max_rational_size ||
	   k == ATOM_max_rational_size_action ||
#endif
	   k == ATOM_float_overflow ||
	   k == ATOM_float_zero_div ||
	   k == ATOM_float_undefined ||
	   k == ATOM_float_underflow ||
	   k == ATOM_float_rounding );
}

int
get_arith_flag(DECL_LD term_t val, atom_t k)
{ atom_t a;
#ifdef O_GMP
  size_t sz;

  if ( k == ATOM_max_rational_size &&
       (sz=LD->arith.rat.max_rational_size) != (size_t)-1 )
    return PL_unify_uint64(val, sz);
  if ( k == ATOM_max_rational_size_action )
    return PL_unify_atom(val, LD->arith.rat.max_rational_size_action);
#endif
  if ( k == ATOM_float_overflow )
    a = LD->arith.f.flags & FLT_OVERFLOW ? ATOM_infinity : ATOM_error;
  else if ( k == ATOM_float_zero_div )
    a = LD->arith.f.flags & FLT_ZERO_DIV ? ATOM_infinity : ATOM_error;
  else if ( k == ATOM_float_undefined )
    a = LD->arith.f.flags & FLT_UNDEFINED ? ATOM_nan : ATOM_error;
  else if ( k == ATOM_float_underflow )
    a = LD->arith.f.flags & FLT_UNDERFLOW ? ATOM_ignore : ATOM_error;
  else if ( k == ATOM_float_rounding )
    a = float_rounding_names[LD->arith.f.flags&FLT_ROUND_MASK];
  else
    return FALSE;

  return PL_unify_atom(val, a);
}

#ifdef O_GMP
static int
set_restraint(term_t t, size_t *valp)
{ GET_LD
  atom_t inf;

  if ( PL_get_atom(t, &inf) && inf == ATOM_infinite )
  { *valp = (size_t)-1;
    return TRUE;
  }
  return PL_get_size_ex(t, valp);
}

#define set_restraint_action(t, key, valp) LDFUNC(set_restraint_action, t, key, valp)
static int
set_restraint_action(DECL_LD term_t t, atom_t key, atom_t *valp)
{ atom_t act;

  if ( PL_get_atom_ex(t, &act) )
  { if ( act == ATOM_error )
    { ok:
      *valp = act;
      return TRUE;
    }

    if ( key == ATOM_max_rational_size_action &&
	 ( act == ATOM_float ) )
      goto ok;

    return PL_domain_error("max_rational_size_action", t);
  }

  return FALSE;
}
#endif

static int rounding_mode[5] =
  {0, FE_TONEAREST, FE_UPWARD, FE_DOWNWARD, FE_TOWARDZERO};

int
atom_to_rounding(atom_t a, int *m)
{ int i;

  for(i=1; i<=FLT_ROUND_TO_ZERO; i++)
  { if ( float_rounding_names[i] == a )
    { *m = i;
      return TRUE;
    }
  }

  return FALSE;
}


atom_t
float_rounding_name(int i)
{ return float_rounding_names[i];
}


void
set_rounding(int mode)
{ fesetround(rounding_mode[mode]);
}


int
set_arith_flag(DECL_LD term_t val, atom_t key)
{ atom_t a;

#ifdef O_GMP
  if ( key == ATOM_max_rational_size )
    return set_restraint(val, &LD->arith.rat.max_rational_size);
  if ( key == ATOM_max_rational_size_action )
    return set_restraint_action(
	       val, key,
	       &LD->arith.rat.max_rational_size_action);
#endif

  if ( PL_get_atom_ex(val, &a) )
  { if ( key == ATOM_float_overflow )
    { if (      a == ATOM_error    ) clear(&(LD->arith.f), FLT_OVERFLOW);
      else if ( a == ATOM_infinity )   set(&(LD->arith.f), FLT_OVERFLOW);
      else goto dom;
    } else if ( key == ATOM_float_zero_div )
    { if (      a == ATOM_error    ) clear(&(LD->arith.f), FLT_ZERO_DIV);
      else if ( a == ATOM_infinity )   set(&(LD->arith.f), FLT_ZERO_DIV);
      else goto dom;
    } else if ( key == ATOM_float_undefined )
    { if (      a == ATOM_error    ) clear(&(LD->arith.f), FLT_UNDEFINED);
      else if ( a == ATOM_nan )        set(&(LD->arith.f), FLT_UNDEFINED);
      else goto dom;
    } else if ( key == ATOM_float_underflow )
    { if (      a == ATOM_error    ) clear(&(LD->arith.f), FLT_UNDERFLOW);
      else if ( a == ATOM_ignore )     set(&(LD->arith.f), FLT_UNDERFLOW);
      else goto dom;
    } else if ( key == ATOM_float_rounding )
    { int i;

      if ( atom_to_rounding(a, &i) )
      { clear(&(LD->arith.f), FLT_ROUND_MASK);
	LD->arith.f.flags |= i;
	set_rounding(i);
	return TRUE;
      }
      goto dom;
    } else
      return FALSE;

    return TRUE;

  dom:
    return PL_domain_error("flag_value", val);
  }

  return FALSE;
}


static
PRED_IMPL("float_class", 2, float_class, 0)
{ PRED_LD
  Word p = valTermRef(A1);

  deRef(p);
  if ( isFloat(*p) )
  { double f = valFloat(*p);
    atom_t a;

    switch(fpclassify(f))
    { case FP_NAN:		a = ATOM_nan;	    break;
      case FP_INFINITE:		a = ATOM_infinite;  break;
      case FP_ZERO:		a = ATOM_zero;      break;
      case FP_SUBNORMAL:	a = ATOM_subnormal; break;
      case FP_NORMAL:		a = ATOM_normal;    break;
      default:			assert(0); a = ATOM_false;
    }

    return PL_unify_atom(A2, a);
  }

  return PL_type_error("float", A1);
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
ar_func_n(DECL_LD int findex, int argc)
{ number result;
  int rval;
  ArithF f = FunctionFromIndex(findex);
  Number argv = argvArithStack(argc);

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
      rval = (*f)(argv+1, argv, &result);
      break;
    case 3:
      rval = (*f)(argv+2, argv+1, argv, &result);
      break;
    default:
      rval = FALSE;
      sysError("Too many arguments to arithmetic function");
  }

  popArgvArithStack(argc);

  if ( rval )
    pushArithStack(&result);

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

  if ( valueExpression(t, &n) )
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
          return FALSE;
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
  PRED_DEF("is",	  2, is,	  PL_FA_ISO)
  PRED_DEF("<",		  2, lt,	  PL_FA_ISO)
  PRED_DEF(">",		  2, gt,	  PL_FA_ISO)
  PRED_DEF("=<",	  2, leq,	  PL_FA_ISO)
  PRED_DEF(">=",	  2, geq,	  PL_FA_ISO)
  PRED_DEF("=\\=",	  2, neq,	  PL_FA_ISO)
  PRED_DEF("=:=",	  2, eq,	  PL_FA_ISO)
  PRED_DEF("succ",	  2, succ,	  0)
  PRED_DEF("plus",	  3, plus,	  0)
  PRED_DEF("between",	  3, between,	  PL_FA_NONDETERMINISTIC)
  PRED_DEF("bounded_number",	  3, bounded_number,	  0)
  PRED_DEF("float_class", 2, float_class, 0)
  PRED_DEF("float_parts", 4, float_parts, 0)

  PRED_DEF("current_arithmetic_function", 1, current_arithmetic_function,
	   PL_FA_NONDETERMINISTIC)

#ifdef O_GMP
  PRED_DEF("divmod", 4, divmod, 0)
  PRED_DEF("nth_integer_root_and_remainder", 4,
	   nth_integer_root_and_remainder, 0)
  PRED_DEF("rational", 3, rational, 0)
#endif
  PRED_DEF("set_random", 1, set_random, 0)
#ifdef O_RANDOM_STATE
  PRED_DEF("random_property", 1, random_property, 0)
#endif
EndPredDefs
