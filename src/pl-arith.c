/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: arithmetic built in functions
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

#include <math.h>			/* avoid abs() problem with MSVC++ */
#include "pl-incl.h"
#ifndef M_PI
#define M_PI (3.14159265358979323846)
#endif
#ifndef M_E
#define M_E (2.7182818284590452354)
#endif

#ifdef WIN32
#include <excpt.h>
#endif

#define MAXARITHFUNCTIONS (100)

typedef struct arithFunction * 	ArithFunction;
typedef int (*ArithF)();

struct arithFunction
{ ArithFunction next;		/* Next of chain */
  FunctorDef	functor;	/* Functor defined */
  ArithF	function;	/* Implementing function */
  Module	module;		/* Module visibility module */
#if O_PROLOG_FUNCTIONS
  Procedure	proc;		/* Prolog defined functions */
#endif
#if O_COMPILE_ARITH
  code		index;		/* Index of function */
#endif
};

forwards ArithFunction	isCurrentArithFunction(FunctorDef, Module);
static void		promoteToRealNumber(Number n);

static ArithFunction arithFunctionTable[ARITHHASHSIZE];
static code next_index;
static ArithFunction functions;


		/********************************
		*   LOGICAL INTEGER FUNCTIONS   *
		*********************************/

word
pl_between(term_t low, term_t high, term_t n, word b)
{ switch( ForeignControl(b) )
  { case FRG_FIRST_CALL:
      { long l, h, i;

	if ( !PL_get_long(low, &l) ||
	     !PL_get_long(high, &h) )
	  return warning("between/3: instantiation fault");

	if ( PL_get_long(n, &i) )
	{ if ( i >= l && i <= h )
	    succeed;
	  fail;
	}
	if ( !PL_is_variable(n) )
	  return warning("between/3: instantiation fault");
	if ( h < l )
	  fail;

	PL_unify_integer(n, l);
	if ( l == h )
	  succeed;
	ForeignRedoInt(l);
      }
    case FRG_REDO:
      { long next = ForeignContextInt(b) + 1;
	long h;

	PL_unify_integer(n, next);
	PL_get_long(high, &h);
	if ( next == h )
	  succeed;
	ForeignRedoInt(next);
      }
    default:;
      succeed;
  }
}

word
pl_succ(term_t n1, term_t n2)
{ long i;

  if ( PL_get_long(n1, &i) )
    return PL_unify_integer(n2, i+1);
  if ( PL_get_long(n2, &i) )
    return PL_unify_integer(n1, i-1);

  return warning("succ/2: instantiation fault");
}

word
pl_plus(term_t a, term_t b, term_t c)
{ long m, n, o;

  if ( PL_get_long(a, &m) )
  { if ( PL_get_long(b, &n) )
      return PL_unify_integer(c, m+n);
    if ( PL_get_long(c, &o) )
      return PL_unify_integer(b, o-m);
  } else
  { if ( PL_get_long(b, &n) && PL_get_long(c, &o) )
      return PL_unify_integer(a, o-n);
  }

  return warning("plus/3: instantiation fault");
}


		/********************************
		*           COMPARISON          *
		*********************************/

int
ar_compare(Number n1, Number n2, int what)
{ int result;

  if ( intNumber(n1) && intNumber(n2) )
  { switch(what)
    { case LT:	result = n1->value.i <  n2->value.i; break;
      case GT:  result = n1->value.i >  n2->value.i; break;
      case LE:	result = n1->value.i <= n2->value.i; break;
      case GE:	result = n1->value.i >= n2->value.i; break;
      case NE:	result = n1->value.i != n2->value.i; break;
      case EQ:	result = n1->value.i == n2->value.i; break;
      default:	fail;
    }
    if ( result )
      succeed;
  } else
  { promoteToRealNumber(n1);
    promoteToRealNumber(n2);

    switch(what)
    { case LT:	result = n1->value.f <  n2->value.f; break;
      case GT:  result = n1->value.f >  n2->value.f; break;
      case LE:	result = n1->value.f <= n2->value.f; break;
      case GE:	result = n1->value.f >= n2->value.f; break;
      case NE:	result = n1->value.f != n2->value.f; break;
      case EQ:	result = n1->value.f == n2->value.f; break;
      default:	fail;
    }
    if ( result )
      succeed;
  }  

  fail;
}


word
compareNumbers(term_t n1, term_t n2, int what)
{ number left, right;

  TRY(valueExpression(n1, &left) &&
      valueExpression(n2, &right));

  return ar_compare(&left, &right, what);
}


word
pl_lessNumbers(term_t n1, term_t n2)			/* </2 */
{ return compareNumbers(n1, n2, LT);
}

word
pl_greaterNumbers(term_t n1, term_t n2)			/* >/2 */
{ return compareNumbers(n1, n2, GT);
}

word
pl_lessEqualNumbers(term_t n1, term_t n2)		/* =</2 */
{ return compareNumbers(n1, n2, LE);
}

word
pl_greaterEqualNumbers(term_t n1, term_t n2)		/* >=/2 */
{ return compareNumbers(n1, n2, GE);
}

word
pl_nonEqualNumbers(term_t n1, term_t n2)		/* =\=/2 */
{ return compareNumbers(n1, n2, NE);
}

word
pl_equalNumbers(term_t n1, term_t n2)			/* =:=/2 */
{ return compareNumbers(n1, n2, EQ);
}

		/********************************
		*           FUNCTIONS           *
		*********************************/

static ArithFunction
isCurrentArithFunction(register FunctorDef f, register Module m)
{ register ArithFunction a;
  ArithFunction r = NULL;
  int level = 30000;

  for(a = arithFunctionTable[pointerHashValue(f, ARITHHASHSIZE)];
      a && !isTableRef(a); a = a->next)
  { if ( a->functor == f )
    { register Module m2;
      register int l;

      for( m2 = m, l = 0; m2; m2 = m2->super, l++ )
      { if ( m2 == a->module && l < level )
	{ r = a;
	  level = l;
	}
      }
    }
  }

  return r;
}

#if HAVE_SIGNAL
typedef void (*OsSigHandler)(int);

static void
realExceptionHandler(int sig, int type, SignalContext scp, char *addr)
{
#ifndef BSD_SIGNALS
  signal(sig, (OsSigHandler)realExceptionHandler);
#endif
  if ( status.arithmetic > 0 )
  { warning("Floating point exception");
#ifndef O_RUNTIME
    Sfprintf(Serror, "[PROLOG STACK:\n");
    backTrace(NULL, 10);
    Sfprintf(Serror, "]\n");
#endif
    pl_abort();
  } else
  { deliverSignal(sig, type, scp, addr);
  }
}
#endif

#if __TURBOC__
static int
realExceptionHandler(e)
struct exception *e;
{ warning("Floating point exception");

  pl_abort();
  /*NOTREACHED*/
  fail;				/* make tc happy */
}
#endif


#if O_PROLOG_FUNCTIONS

static int prologFunction(ArithFunction, term_t, Number);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Activating a Prolog predicate as function below the arithmetic functions
is/0, >, etc. `f' is the arithmetic function   to  be called. `t' is the
base term-reference of an array holding  the proper number of arguments.
`r' is the result of the evaluation.

This calling convention is somewhat  unnatural,   but  fits  best in the
calling convention required by ar_func_n() below.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
prologFunction(ArithFunction f, term_t av, Number r)
{ int arity = f->proc->definition->functor->arity;
  fid_t cid = PL_open_foreign_frame();
  qid_t qid;
  int rval;

  qid = PL_open_query(NULL, TRUE, f->proc, av);

  if ( PL_next_solution(qid) )
    rval = valueExpression(av+arity-1, r);
  else
  { warning("Arithmetic function %s failed", procedureName(f->proc));
    rval = FALSE;
  }

  PL_close_query(qid);
  PL_discard_foreign_frame(cid);

  return rval;
}

#endif /* O_PROLOG_FUNCTIONS */

int
valueExpression(term_t t, Number r)
{ ArithFunction f;
  FunctorDef fDef;
  Word p = valTermRef(t);
  word w;

  deRef(p);
  w = *p;

  switch(tag(w))
  { case TAG_INTEGER:
      r->value.i = valInteger(w);
      r->type = V_INTEGER;
      succeed;
    case TAG_FLOAT:
      r->value.f = valReal(w);
      r->type = V_REAL;
      succeed;
    case TAG_VAR:
      return warning("Unbound variable in arithmetic expression");
    case TAG_ATOM:
      fDef = lookupFunctorDef(w, 0);
      break;
    case TAG_COMPOUND:
      fDef = functorTerm(w);
      break;
    default:
      return warning("Illegal data type in arithmetic expression");
  }

  if ( !(f = isCurrentArithFunction(fDef, contextModule(environment_frame))))
  { if ( fDef == FUNCTOR_dot2 )		/* handle "a" (make function) */
    { Word a, p = valTermRef(t);

      deRef(p);
      a = argTermP(*p, 0);
      deRef(a);
      if ( isTaggedInt(*a) )
      { a = argTermP(*p, 1);
	deRef(a);
	if ( *a == ATOM_nil )
	{ r->value.i = valInt(*a);
	  r->type = V_INTEGER;
	  succeed;
	}
      }
      return warning("./2: Bad character code");
    } else
    { return warning("Unknown arithmetic operator: %s/%d",
		     stringAtom(fDef->name), fDef->arity);
    }
  }

#if O_PROLOG_FUNCTIONS
  if ( f->proc )
  { int rval, n, arity = fDef->arity;
    term_t h0 = PL_new_term_refs(arity+1); /* one extra for the result */

    for(n=0; n<arity; n++)
    { number n1;

      PL_get_arg(n+1, t, h0+n);
      if ( valueExpression(h0+n, &n1) )
      { _PL_put_number(h0+n, &n1);
      } else
	fail;
    }

    rval = prologFunction(f, h0, r);
    PL_reset_term_refs(h0);
    return rval;
  }
#endif

  { int rval;

#ifdef WIN32
    __try
    {
#else
    status.arithmetic++;
#endif
    switch(fDef->arity)
    { case 0:
	rval = (*f->function)(r);
        break;
      case 1:	
      { term_t a = PL_new_term_ref();
	number n1;

	PL_get_arg(1, t, a);
	if ( valueExpression(a, &n1) )
	  rval = (*f->function)(&n1, r);
	else
	  rval = FALSE;

	PL_reset_term_refs(a);
	break;
      }
      case 2:
      { term_t a = PL_new_term_ref();
	number n1, n2;

	PL_get_arg(1, t, a);
	if ( valueExpression(a, &n1) )
	{ PL_get_arg(2, t, a);
	  if ( valueExpression(a, &n2) )
	    rval = (*f->function)(&n1, &n2, r);
	  else
	    rval = FALSE;
	} else
	  rval = FALSE;

	PL_reset_term_refs(a);
	break;
      }
      default:
	sysError("Illegal arity for arithmic function");
        rval = FALSE;
    }
#ifdef WIN32
    } __except(EXCEPTION_EXECUTE_HANDLER)
    { warning("Floating point exception");
      Sfprintf(Serror, "[PROLOG STACK:\n");
      backTrace(NULL, 10);
      Sfprintf(Serror, "]\n");
      pl_abort();
    }
#else
    status.arithmetic--;
#endif

    return rval;
  }
}

		 /*******************************
		 *	     CONVERSION		*
		 *******************************/

static void
promoteToRealNumber(Number n)
{ if ( intNumber(n) )
  { n->value.f = (real)n->value.i;
    n->type = V_REAL;
  }
}


int
toIntegerNumber(Number n)
{ if ( floatNumber(n) )
  { long l;

#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( !((n->value.f >= PLMININT) && (n->value.f <= PLMAXINT)) )
      fail;
#endif

    l = (long)n->value.f;
    if ( n->value.f == (real) l )
    { n->value.i = l;
      n->type = V_INTEGER;
      succeed;
    }

    fail;
  }

  succeed;
} 


void
canoniseNumber(Number n)
{ if ( n->type == V_REAL )		/* only if not explicit! */
  { long l;

#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( !((n->value.f >= PLMININT) && (n->value.f <= PLMAXINT)) )
      return;
#endif

    l = (long)n->value.f;
    if ( n->value.f == (real) l )
    { n->value.i = l;
      n->type = V_INTEGER;
    }
  }
}


		/********************************
		*     ARITHMETIC FUNCTIONS      *
		*********************************/

static int
ar_add(Number n1, Number n2, Number r)
{ if ( intNumber(n1) && intNumber(n2) ) 
  { r->value.i = n1->value.i + n2->value.i; 
    
    if ( n1->value.i > 0 && n2->value.i > 0 && r->value.i <= 0 )
      goto overflow;
    if ( n1->value.i < 0 && n2->value.i < 0 && r->value.i >= 0 )
      goto overflow;

    r->type = V_INTEGER;
    succeed;
  } 

overflow:
  promoteToRealNumber(n1);
  promoteToRealNumber(n2);
  r->value.f = n1->value.f + n2->value.f; 
  r->type = V_REAL;

  succeed;
}


static int
ar_minus(Number n1, Number n2, Number r)
{ if ( intNumber(n1) && intNumber(n2) ) 
  { r->value.i = n1->value.i - n2->value.i; 
    
    if ( n1->value.i > 0 && n2->value.i < 0 && r->value.i <= 0 )
      goto overflow;
    if ( n1->value.i < 0 && n2->value.i > 0 && r->value.i >= 0 )
      goto overflow;

    r->type = V_INTEGER;
    succeed;
  } 

overflow:
  promoteToRealNumber(n1);
  promoteToRealNumber(n2);
  r->value.f = n1->value.f - n2->value.f; 
  r->type = V_REAL;

  succeed;
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

#define BINAIRY_INT_FUNCTION(name, op) \
  static int \
  name(Number n1, Number n2, Number r) \
  { TRY(toIntegerNumber(n1) && toIntegerNumber(n2)); \
    r->value.i = n1->value.i op n2->value.i; \
    r->type = V_INTEGER; \
    succeed; \
  }

#define BINAIRY_FLOAT_FUNCTION(name, func) \
  static int \
  name(Number n1, Number n2, Number r) \
  { promoteToRealNumber(n1); \
    promoteToRealNumber(n2); \
    r->value.f = func(n1->value.f, n2->value.f); \
    r->type = V_REAL; \
    succeed; \
  }

UNAIRY_FLOAT_FUNCTION(ar_sqrt, sqrt)
UNAIRY_FLOAT_FUNCTION(ar_sin, sin)
UNAIRY_FLOAT_FUNCTION(ar_cos, cos)
UNAIRY_FLOAT_FUNCTION(ar_tan, tan)
UNAIRY_FLOAT_FUNCTION(ar_asin, asin)
UNAIRY_FLOAT_FUNCTION(ar_acos, acos)
UNAIRY_FLOAT_FUNCTION(ar_atan, atan)
UNAIRY_FLOAT_FUNCTION(ar_log, log)
UNAIRY_FLOAT_FUNCTION(ar_exp, exp)
UNAIRY_FLOAT_FUNCTION(ar_log10, log10)

BINAIRY_FLOAT_FUNCTION(ar_atan2, atan2)
BINAIRY_FLOAT_FUNCTION(ar_pow, pow);

BINAIRY_INT_FUNCTION(ar_mod, %)
BINAIRY_INT_FUNCTION(ar_div, /)
BINAIRY_INT_FUNCTION(ar_disjunct, |)
BINAIRY_INT_FUNCTION(ar_conjunct, &)
BINAIRY_INT_FUNCTION(ar_shift_right, >>)
BINAIRY_INT_FUNCTION(ar_shift_left, <<)
BINAIRY_INT_FUNCTION(ar_xor, ^)

static int
ar_sign(Number n1, Number r)
{ if ( intNumber(n1) )
    r->value.i = (n1->value.i < 0 ? -1 : n1->value.i > 0 ? 1 : 0);
  else
    r->value.f = (n1->value.f < 0 ? -1 : n1->value.f > 0 ? 1 : 0);

  r->type = V_INTEGER;
  succeed;
}


static int
ar_rem(Number n1, Number n2, Number r)
{ real f;

  if ( !toIntegerNumber(n1) ||
       !toIntegerNumber(n2) )
    return warning("rem/2: arguments must be integers");

  f = (real)n1->value.i / (real)n2->value.i;
  r->value.f = f - (real)((long) f);
  r->type = V_REAL;
  succeed;
}


static int
ar_divide(Number n1, Number n2, Number r)
{ if ( intNumber(n1) && intNumber(n2) )
  { if ( n1->value.i % n2->value.i == 0)
    { r->value.i = n1->value.i / n2->value.i;
      r->type = V_INTEGER;
      succeed;
    }
  }

  promoteToRealNumber(n1);
  promoteToRealNumber(n2);

  r->value.f = n1->value.f / n2->value.f;
  r->type = V_REAL;
  succeed;
}


static int
ar_times(Number n1, Number n2, Number r)
{ if ( intNumber(n1) && intNumber(n2) )
  { if ( abs(n1->value.i) >= (1 << 15) || abs(n2->value.i) >= (1 << 15) )
    { r->value.f = (real)n1->value.i * (real)n2->value.i;
      r->type = V_REAL;
      succeed;
    }
    r->value.i = n1->value.i * n2->value.i;
    r->type = V_INTEGER;
    succeed;
  }
  
  promoteToRealNumber(n1);
  promoteToRealNumber(n2);

  r->value.f = n1->value.f * n2->value.f;
  r->type = V_REAL;
  succeed;
}


static int
ar_max(Number n1, Number n2, Number r)
{ if ( intNumber(n1) && intNumber(n2) )
  { r->value.i = (n1->value.i > n2->value.i ? n1->value.i : n2->value.i);
    r->type = V_INTEGER;
    succeed;
  }

  promoteToRealNumber(n1);
  promoteToRealNumber(n2);

  r->value.f = (n1->value.f > n2->value.f ? n1->value.f : n2->value.f);
  r->type = V_REAL;
  succeed;
}


static int
ar_min(Number n1, Number n2, Number r)
{ if ( intNumber(n1) && intNumber(n2) )
  { r->value.i = (n1->value.i < n2->value.i ? n1->value.i : n2->value.i);
    r->type = V_INTEGER;
    succeed;
  }

  promoteToRealNumber(n1);
  promoteToRealNumber(n2);

  r->value.f = (n1->value.f < n2->value.f ? n1->value.f : n2->value.f);
  r->type = V_REAL;
  succeed;
}


static int
ar_negation(Number n1, Number r)
{ if ( !toIntegerNumber(n1) )
    return warning("is/2: argument to \\/1 should be an integer");

  r->value.i = ~n1->value.i;
  r->type = V_INTEGER;
  succeed;
}


static int
ar_u_minus(Number n1, Number r)
{ if ( intNumber(n1) )
  { r->value.i = -n1->value.i;
    r->type = V_INTEGER;
  } else
  { r->value.f = -n1->value.f;
    r->type = V_REAL;
  }

  succeed;
}


#undef abs
#define abs(a) ((a) < 0 ? -(a) : (a))

static int
ar_abs(Number n1, Number r)
{ if ( intNumber(n1) )
  { r->value.i = abs(n1->value.i);
    r->type = V_INTEGER;
  } else
  { r->value.f = abs(n1->value.f);
    r->type = V_REAL;
  }

  succeed;
}


static int
ar_integer(Number n1, Number r)
{ if ( intNumber(n1) )
  { *r = *n1;
    succeed;
  } else
  { if ( n1->value.f < PLMAXINT && n1->value.f > PLMININT )
    { r->value.i = (n1->value.f > 0 ? (long)(n1->value.f + 0.5)
			            : (long)(n1->value.f - 0.5));
      r->type = V_INTEGER;
      succeed;
    }
#ifdef HAVE_RINT
    r->value.f = rint(n1->value.f);
    r->type = V_REAL;
    succeed;
#else
    return warning("integer/1: argument too large");
#endif
  }
}


static int
ar_float(Number n1, Number r)
{ *r = *n1;
  promoteToRealNumber(r);
  r->type = V_EXPLICIT_REAL;		/* avoid canoniseNumber() */

  succeed;
}


static int
ar_floor(Number n1, Number r)
{ if ( intNumber(n1) )
    *r = *n1;
  else
  {
#ifdef HAVE_FLOOR
    r->value.f = floor(n1->value.f);
    r->type = V_REAL;
#else
    r->value.i = (long)n1->value.f;
    if ( n1->value.f < 0 && (real)r->value.i != n1->value.f )
      r->value.i--;
    r->type = V_INTEGER;
#endif
  }
  succeed;
}


static int
ar_ceil(Number n1, Number r)
{ if ( intNumber(n1) )
    *r = *n1;
  else
  {
#ifdef HAVE_CEIL
    r->value.f = ceil(n1->value.f);
    r->type = V_REAL;
#else
    r->value.i = (long)n1->value.f;
    if ( (real)r->value.i < n1->value.f )
       r->value.i)++;
    r->type = V_INTEGER;
#endif
  }

  succeed;
}


static int
ar_float_fractional_part(Number n1, Number r)
{ if ( intNumber(n1) )
  { r->value.i = 0;
    r->type = V_INTEGER;
  } else
  { if ( n1->value.f > 0 )
    { r->value.f = n1->value.f - floor(n1->value.f);
    } else
    { TRY(ar_ceil(n1, r));
      r->value.f = n1->value.f - ceil(n1->value.f);
    }
    r->type = V_REAL;
  }

  succeed;
}


static int
ar_float_integer_part(Number n1, Number r)
{ if ( intNumber(n1) )
    *r = *n1;
  else
  { if ( n1->value.f > 0 )
      return ar_floor(n1, r);
    else
      return ar_ceil(n1, r);
  }

  succeed;
}


static int
ar_truncate(Number n1, Number r)
{ return ar_float_integer_part(n1, r);
}


static int
ar_random(Number n1, Number r)
{ if ( !toIntegerNumber(n1) )
    return warning("random/1: n1->valueument should be an integer");

  r->value.i = Random() % n1->value.i;
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
{ r->value.f = CpuTime();

  r->type = V_REAL;
  succeed;
}


		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

word
pl_is(term_t v, term_t e)
{ number arg;

  if ( valueExpression(e, &arg) )
  { canoniseNumber(&arg);
    return _PL_unify_number(v, &arg);
  }

  fail;
}


#if O_PROLOG_FUNCTIONS
word
pl_arithmetic_function(term_t descr)
{ Procedure proc;
  Definition def;
  FunctorDef fd;
  register ArithFunction f;
  Module m = NULL;
  term_t head = PL_new_term_ref();
  int v;

  PL_strip_module(descr, &m, head);
  if ( !PL_get_functor(head, &fd) )
    return warning("arithmetic_function/1: Illegal head");
  if ( fd->arity < 1 )
    return warning("arithmetic_function/1: Illegal arity");

  proc = lookupProcedure(fd, m);
  def = proc->definition;
  fd = lookupFunctorDef(fd->name, fd->arity - 1);
  if ( (f = isCurrentArithFunction(fd, m)) && f->module == m )
    succeed;				/* already registered */

  if ( next_index >= MAXARITHFUNCTIONS )
    return warning("Cannot handle more than %d arithmetic functions",
		   MAXARITHFUNCTIONS);

  v = pointerHashValue(fd, ARITHHASHSIZE);
  f = &functions[next_index];
  f->functor  = fd;
  f->function = NULL;
  f->module   = m;
  f->proc     = proc;
  f->index    = next_index++;
  f->next     = arithFunctionTable[v];
  arithFunctionTable[v] = f;  

  succeed;
}

word
pl_current_arithmetic_function(term_t f, word h)
{ ArithFunction a;
  Module m = NULL;
  term_t head = PL_new_term_ref();

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { FunctorDef fd;

      PL_strip_module(f, &m, head);

      if ( PL_is_variable(head) )
      { a = arithFunctionTable[0];
        break;
      } else if ( PL_get_functor(head, &fd) )
      {	return isCurrentArithFunction(fd, m) ? TRUE : FALSE;
      } else
        return warning("current_arithmetic_function/2: instantiation fault");
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

    for(m2 = m; m2; m2 = m2->super)
    { if ( m2 == a->module && a == isCurrentArithFunction(a->functor, m) )
      { if ( PL_unify_functor(f, a->functor) )
	  return_next_table(ArithFunction, a, ;);
      }
    }
  }

  fail;
}

#endif /* O_PROLOG_FUNCTIONS */

#define ADD(functor, func) { (ArithFunction)NULL, functor, func }

static struct arithFunction ar_functions[MAXARITHFUNCTIONS] = {
  ADD(FUNCTOR_plus2,		ar_add),
  ADD(FUNCTOR_minus2,		ar_minus),
  ADD(FUNCTOR_star2,		ar_times),
  ADD(FUNCTOR_divide2,		ar_divide),
  ADD(FUNCTOR_minus1,		ar_u_minus),
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

  ADD((FunctorDef)NULL,		(ArithF)NULL)
};

#undef ADD


void
initArith(void)
{
#ifdef SIGFPE
  pl_signal(SIGFPE, (handler_t) realExceptionHandler);
#endif
#if __TURBOC__
  setmatherr(realExceptionHandler);
#endif

					/* link the table to enumerate */
  { register ArithFunction *f;
    register int n;

    for(n=0, f = arithFunctionTable; n < (ARITHHASHSIZE-1); n++, f++)
      *f = makeTableRef(f+1);
  }

					/* initialise it */
  { ArithFunction f;
    int v;

    functions = ar_functions;

    for( f = functions, next_index = 0; f->functor; f++, next_index++ )
    { v = pointerHashValue(f->functor, ARITHHASHSIZE);
      f->module = MODULE_system;
#if O_COMPILE_ARITH
      f->index = next_index;
#endif
      f->next = arithFunctionTable[v];
      arithFunctionTable[v] = f;
    }
  }
}

#if O_COMPILE_ARITH

		/********************************
		*    VIRTUAL MACHINE SUPPORT    *
		*********************************/

int
indexArithFunction(register FunctorDef fdef, register Module m)
{ register ArithFunction f;

  if ( (f = isCurrentArithFunction(fdef, m)) == (ArithFunction) NULL )
    return -1;

  return (int)f->index;
}


FunctorDef
functorArithFunction(int n)
{ return functions[n].functor;
}


bool
ar_func_n(code n, int argc, Number *stack)
{ number result;
  int rval;
  ArithFunction f = &functions[(int)n];
  Number sp = *stack;

  sp -= argc;
  if ( f->proc )
  { LocalFrame lSave = lTop;		/* TBD (check with stack!) */
    term_t h0;
    int n;

    lTop = (LocalFrame) (*stack);
    h0   = PL_new_term_refs(argc+1);
    
    for(n=0; n<argc; n++)
      _PL_put_number(h0+n, &sp[n]);

    rval = prologFunction(f, h0, &result);
    lTop = lSave;
  } else
  { switch(argc)
    { case 0:
	rval = (*f->function)(&result);
        break;
      case 1:
	rval = (*f->function)(sp, &result);
        break;
      case 2:
	rval = (*f->function)(sp, &sp[1], &result);
        break;
      default:
	rval = FALSE;
        sysError("Too many arguments to arithmetic function");
    }
  }

  if ( rval )
  { *sp++ = result;
    *stack = sp;
  }

  return rval;
}

#endif /* O_COMPILE_ARITH */
