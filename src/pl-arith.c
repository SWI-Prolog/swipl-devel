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
#include "pl-itf.h"
#ifndef M_PI
#define M_PI (3.141593)
#endif
#ifndef M_E
#define M_E (2.718282)
#endif

#define MAXARITHFUNCTIONS (100)

#define V_ERROR		FALSE		/* so we can use `fail' */
#define V_REAL		1
#define V_INTEGER	2

typedef struct arithFunction * 	ArithFunction;

typedef union
{ real		f;		/* value as real */
  long		i;		/* value as integer */
} number;

typedef number * Number;	/* pointer to a number */

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

forwards int		valueExpression(Word t, Number r);
forwards ArithFunction	isCurrentArithFunction(FunctorDef, Module);

static ArithFunction arithFunctionTable[ARITHHASHSIZE];
static code next_index;
static ArithFunction functions;

		 /*******************************
		 *	       TAGGING		*
		 *******************************/

#ifdef AVOID_0X80000000_BIT

/*#define DONOT_USE_BIT_32_FOR_INT 1*/

#if DONOT_USE_BIT_32_FOR_INT

word
fconsNum(long i)
{ return unMask(i<<LMASK_BITS) | INT_MASK;
}

long
fvalNum(word w)
{ return ((long)((w)<<4)>>(4+LMASK_BITS));
}

#else

word
fconsNum(long i)
{ i = (i<<LMASK_BITS) & 0x1fffffffL;
  i |= (i << 3) & 0x80000000L;
  i &= 0x8fffffffL;
  
  return (word) (i|INT_MASK);
}

long
fvalNum(word w)
{ long i = w;

  i &= 0xefffffffL;
  i |= (i>>3) & 0x10000000;
  i = (i << 3) >> (3+LMASK_BITS);

  return (long) i;
}

#endif

#endif /*AVOID_0X80000000_BIT*/


		/********************************
		*   LOGICAL INTEGER FUNCTIONS   *
		*********************************/

word
pl_between(register Word l, register Word h, register Word n, word b)
{ switch( ForeignControl(b) )
  { case FRG_FIRST_CALL:
      { if (!isInteger(*l) || !isInteger(*h))
	  return warning("between/3: instantiation fault");

	if (isInteger(*n))
	{ if (valNum(*l) > valNum(*n))
	    fail;
	  if (valNum(*h) < valNum(*n))
	    fail;
	  succeed;
	}
	if (!isVar(*n))
	  return warning("between/3: instantiation fault");
	if ( valNum(*h) < valNum(*l) )
	  fail;
	unifyAtomic(n, *l);

	ForeignRedo(valNum(*l));
      }
    case FRG_REDO:
      { long next = ForeignContext(b) + 1;
	word nextword;

	if (next > valNum(*h) )
	  fail;

	nextword = consNum(next);
	unifyAtomic(n, nextword);

	ForeignRedo(next);
      }
    default:;
      succeed;
  }
}

word
pl_succ(register Word n1, register Word n2)
{ if (isVar(*n1))
  { if (isInteger(*n2))
      return unifyAtomic(n1, consNum(valNum(*n2)-1));

    return warning("succ/2: instantiation fault");
  }

  if (isVar(*n2))
  { if (isInteger(*n1))
      return unifyAtomic(n2, consNum(valNum(*n1)+1));

    return warning("succ/2: instantiation fault");
  }

  if (isInteger(*n1) && isInteger(*n2) )
  { if (valNum(*n1) + 1 == valNum(*n2) )
      succeed;
    else
      fail;
  }

  return warning("succ/2: instantiation fault");
}

word
pl_plus(register Word a, register Word b, register Word c)
{ if (isVar(*a) && isInteger(*b) && isInteger(*c) )
    return unifyAtomic(a, consNum(valNum(*c) - valNum(*b)) );
  if (isInteger(*a) && isVar(*b) && isInteger(*c) )
    return unifyAtomic(b, consNum(valNum(*c) - valNum(*a)) );
  if (isInteger(*a) && isInteger(*b) && isVar(*c) )
    return unifyAtomic(c, consNum(valNum(*a) + valNum(*b)) );
  if (isInteger(*a) && isInteger(*b) && isInteger(*c) )
    if (valNum(*a) + valNum(*b) == valNum(*c) )
      succeed;

  fail;
}


		/********************************
		*           COMPARISON          *
		*********************************/

word
compareNumbers(Word n1, Word n2, int what)
{ int result;
  number left, right;
  int tl, tr;

  TRY( tl = valueExpression(n1, &left) );
  TRY( tr = valueExpression(n2, &right) );

  if (tl == V_INTEGER && tr == V_INTEGER)
  { switch(what)
    { case LT:	result = left.i <  right.i; break;
      case GT:  result = left.i >  right.i; break;
      case LE:	result = left.i <= right.i; break;
      case GE:	result = left.i >= right.i; break;
      case NE:	result = left.i != right.i; break;
      case EQ:	result = left.i == right.i; break;
      default:	fail;
    }
    if (result)
      succeed;
  } else
  { real F1, F2;

    F1 = (tl == V_INTEGER ? (real)left.i  : left.f);
    F2 = (tr == V_INTEGER ? (real)right.i : right.f);
    switch(what)
    { case LT:	result = F1 <  F2; break;
      case GT:  result = F1 >  F2; break;
      case LE:	result = F1 <= F2; break;
      case GE:	result = F1 >= F2; break;
      case NE:	result = F1 != F2; break;
      case EQ:	result = F1 == F2; break;
      default:	fail;
    }
    if (result)
      succeed;
  }  
  fail;
}

word
pl_lessNumbers(Word n1, Word n2)			/* </2 */
            
{ return compareNumbers(n1, n2, LT);
}

word
pl_greaterNumbers(Word n1, Word n2)		/* >/2 */
            
{ return compareNumbers(n1, n2, GT);
}

word
pl_lessEqualNumbers(Word n1, Word n2)		/* =</2 */
            
{ return compareNumbers(n1, n2, LE);
}

word
pl_greaterEqualNumbers(Word n1, Word n2)		/* >=/2 */
            
{ return compareNumbers(n1, n2, GE);
}

word
pl_nonEqualNumbers(Word n1, Word n2)		/* =\=/2 */
            
{ return compareNumbers(n1, n2, NE);
}

word
pl_equalNumbers(Word n1, Word n2)			/* =:=/2 */
            
{ return compareNumbers(n1, n2, EQ);
}


		/********************************
		*           FUNCTIONS           *
		*********************************/

/* not used any longer, but might be usefull to export via the interface
static
ArithFunction
newArithFunction(f, func)
FunctorDef f;
ArithF func;
{ int v = pointerHashValue(f, ARITHHASHSIZE);
  register ArithFunction a;

  for(a=arithFunctionTable[v]; a && !isRef((word)a); a=a->next)
  { if (a->functor == f)
      return a;
  }
  a = (ArithFunction) allocHeap(sizeof(struct arithFunction));
  a->next = arithFunctionTable[v];
  arithFunctionTable[v] = a;
  a->functor = f;
  a->function = func;

  return a;
}
*/

static
ArithFunction
isCurrentArithFunction(register FunctorDef f, register Module m)
{ register ArithFunction a;
  ArithFunction r = NULL;
  int level = 30000;

  for(a = arithFunctionTable[pointerHashValue(f, ARITHHASHSIZE)];
      a && !isRef((word)a); a = a->next)
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

static int prologFunction(ArithFunction, Word, Number);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Activating a Prolog predicate as function below the arithmetic functions
is/0, >, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
prologFunction(ArithFunction f, Word av, Number r)
                
        				/* pointer to term arguments */
         
{ word goal;
  int arity = f->proc->functor->arity;
  int n;
  Word ap;
  mark m;
  Word result;
  bool rval;
  LocalFrame fr = lTop;

  Mark(m);
  goal = globalFunctor(f->proc->functor);
  ap = argTermP(goal, 0);
  for(n=0; n < arity-1; n++)
  { number num;

    switch( valueExpression(av++, &num) )
    { case V_INTEGER:	*ap++ = consNum(num.i);
      			break;
      case V_REAL:	*ap++ = globalReal(num.f);
      			break;
      case V_ERROR:
      default:		unlockMark(&m);
      			return V_ERROR;
    }
  }

  if ( (LocalFrame) &av[1] > lBase )
  { lTop = (LocalFrame) &av[1];
    DEBUG(8, Sdprintf("Setting lTop to 0x%x\n", lTop));
  }
  DEBUG(2, Putf("calling "); pl_write(&goal); pl_nl());
  rval = PL_call(&goal, f->proc->definition->module);
  DEBUG(2, Putf("rval = %d; return ", rval); pl_write(&goal); pl_nl());
  lTop = fr;
  if ( rval == FALSE )
  { warning("Arithmetic function %s failed", procedureName(f->proc));
    unlockMark(&m);
    return V_ERROR;
  }

  result = argTermP(goal, arity-1);
  deRef(result);
  
  if ( isInteger(*result) )
  { r->i = valNum(*result);
    Undo(m);
    return V_INTEGER;
  } else if ( isReal(*result) )
  { r->f = valReal(*result);
    Undo(m);
    return V_REAL;
  } else
  { unlockMark(&m);
    warning("Arithmetic function %s did not bind return value to a number",
	    					procedureName(f->proc));
    fail;
  }
}

#endif /* O_PROLOG_FUNCTIONS */

static int
valueExpression(register Word t, Number r)
{ volatile ArithFunction f;
  volatile Word args;
  volatile FunctorDef fDef;

  deRef(t);

  if (isInteger(*t) )
  { r->i = valNum(*t);
    return V_INTEGER;
  }
  if (isReal(*t) )
  { r->f = valReal(*t);
    if ( r->f >= PLMININT && r->f <= PLMAXINT )
    { long i = (long) r->f;

      if ( r->f == (real)i )
      { r->i = i;
	return V_INTEGER;
      }
    }
    return V_REAL;
  }

  if ( isTerm(*t) )
  { fDef = functorTerm(*t);
    args = argTermP(*t, 0);
  } else if ( isAtom(*t) )
  { fDef = lookupFunctorDef((Atom)*t, 0);
    args = NULL;
  } else if ( isVar(*t) )
    return warning("Unbound variable in arithmetic expression");
  else
    return warning("Illegal data type in arithmetic expression");

  if ((f = isCurrentArithFunction(fDef,
				  contextModule(environment_frame))) == NULL)
    return warning("Unknown arithmetic operator: %s", stringAtom(fDef->name));

#if O_PROLOG_FUNCTIONS
  if ( f->proc != (Procedure) NULL )
    return prologFunction(f, args, r);
#endif

  { int type;
    Word a0, a1;

    status.arithmetic++;
    switch(fDef->arity)
    { case 0:	type = (*f->function)(r); break;
      case 1:	deRef2(args, a0);
		type = (*f->function)(a0, r);
		break;
      case 2:	deRef2(args, a0);
		deRef2((args+1), a1);
		type = (*f->function)(a0, a1, r);
		break;
      default:	sysError("Illegal arity for arithmic function");
		type = V_ERROR;
    }
    status.arithmetic--;

    if ( type == V_REAL )
    { if ( r->f >= PLMININT && r->f <= PLMAXINT )
      { long i = (long) r->f;

	if ( r->f == (real)i )
	{ r->i = i;
	  return V_INTEGER;
	}
      }
    }

    return type;
  }
}

		/********************************
		*     ARITHMETIC FUNCTIONS      *
		*********************************/

/* C-primitive binairy operators */

#define BINAIRYFUNCTION(name, op) \
  static int \
  name(n1, n2, r) \
  Word n1, n2; \
  Number r; \
  { number left, right; \
    int tl, tr; \
    TRY(tl = valueExpression(n1, &left) ); \
    TRY(tr = valueExpression(n2, &right) ); \
    if (tl == V_INTEGER && tr == V_INTEGER) \
    { r->i = left.i op right.i; \
      if ( r->i < PLMININT || r->i > PLMAXINT ) \
      { r->f = (real) r->i; \
        return V_REAL; \
      } \
      return V_INTEGER; \
    } \
    if (tl == V_REAL && tr == V_INTEGER) \
    { r->f = left.f op (real)right.i; \
      return V_REAL; \
    } \
    if (tl == V_INTEGER && tr == V_REAL) \
    { r->f = (real)left.i op right.f; \
      return V_REAL; \
    } \
    if (tl == V_REAL && tr == V_REAL) \
    { r->f = left.f op right.f; \
      return V_REAL; \
    } \
    return sysError("Arithmetic internal error"); \
  }

/* Real unairy functions. */

#define UNAIRYFUNCTION(name, op) \
  static int \
  name(n1, r) \
  Word n1; \
  Number r; \
  { number arg; \
    switch( valueExpression(n1, &arg) ) \
    { case V_INTEGER:	r->f = op((real)arg.i); \
			return V_REAL; \
      case V_REAL:	r->f = op(arg.f); \
			return V_REAL; \
      default:		fail; \
    } \
  }

#define BINAIRY_INT_FUNCTION(name, op) \
  static int \
  name(n1, n2, r) \
  Word n1, n2; \
  Number r; \
  { number left, right; \
    int tl, tr; \
    TRY(tl = valueExpression(n1, &left) ); \
    TRY(tr = valueExpression(n2, &right) ); \
    if (tl == V_INTEGER && tr == V_INTEGER) \
    { r->i = left.i op right.i; \
      return V_INTEGER; \
    } \
    return warning("is/2: arguments are not integers"); \
  }

#define BINAIRY_FLOAT_FUNCTION(name, func) \
  static int \
  name(n1, n2, r) \
  Word n1, n2; \
  Number r; \
  { number left, right; \
    real f1, f2; \
    int tl, tr; \
    TRY(tl = valueExpression(n1, &left) ); \
    TRY(tr = valueExpression(n2, &right) ); \
    f1 = (tl == V_INTEGER ? (real) left.i  : left.f); \
    f2 = (tr == V_INTEGER ? (real) right.i : right.f); \
    r->f = func(f1, f2); \
    return V_REAL; \
  }

BINAIRYFUNCTION(ar_add, +)
BINAIRYFUNCTION(ar_minus, -)

UNAIRYFUNCTION(ar_sqrt, sqrt)
UNAIRYFUNCTION(ar_sin, sin)
UNAIRYFUNCTION(ar_cos, cos)
UNAIRYFUNCTION(ar_tan, tan)
UNAIRYFUNCTION(ar_asin, asin)
UNAIRYFUNCTION(ar_acos, acos)
UNAIRYFUNCTION(ar_atan, atan)
UNAIRYFUNCTION(ar_log, log)
UNAIRYFUNCTION(ar_exp, exp)
UNAIRYFUNCTION(ar_log10, log10)

BINAIRY_FLOAT_FUNCTION(ar_atan2, atan2)

BINAIRY_INT_FUNCTION(ar_mod, %)
BINAIRY_INT_FUNCTION(ar_div, /)
BINAIRY_INT_FUNCTION(ar_disjunct, |)
BINAIRY_INT_FUNCTION(ar_conjunct, &)
BINAIRY_INT_FUNCTION(ar_shift_right, >>)
BINAIRY_INT_FUNCTION(ar_shift_left, <<)
BINAIRY_INT_FUNCTION(ar_xor, ^)

static int
ar_divide(Word n1, Word n2, Number r)
{ number left, right;
  int tl, tr;

  TRY( tl = valueExpression(n1, &left) );
  TRY( tr = valueExpression(n2, &right) );

  if (tl == V_INTEGER && tr == V_INTEGER)
  { if (left.i % right.i == 0)
    { r->i = left.i / right.i;
      return V_INTEGER;
    }
    r->f = (real)left.i / (real)right.i;

    return V_REAL;
  }
  if (tl == V_REAL && tr == V_INTEGER)
  { r->f = left.f / (real)right.i;
    return V_REAL;
  }
  if (tl == V_INTEGER && tr == V_REAL)
  { r->f = (real)left.i / right.f;
    return V_REAL;
  }
  if (tl == V_REAL && tr == V_REAL)
  { r->f = left.f / right.f;
    return V_REAL;
  }

  return sysError("Arithmetic internal error");
}

static int
ar_times(Word n1, Word n2, Number r)
{ number left, right;
  int tl, tr;

  TRY( tl = valueExpression(n1, &left) );
  TRY( tr = valueExpression(n2, &right) );

  if ( tl == V_INTEGER && tr == V_INTEGER )
  { if ( abs(left.i) >= (1 << 13) || abs(right.i) >= (1 << 13) )
    { r->f = (real)left.i * (real)right.i;
      return V_REAL;
    }
    r->i = left.i * right.i;
    return V_INTEGER;
  }
  if (tl == V_REAL && tr == V_INTEGER)
  { r->f = left.f * (real)right.i;
    return V_REAL;
  }
  if (tl == V_INTEGER && tr == V_REAL)
  { r->f = (real)left.i * right.f;
    return V_REAL;
  }
  if (tl == V_REAL && tr == V_REAL)
  { r->f = left.f * right.f;
    return V_REAL;
  }

  return sysError("Arithmetic internal error");
}

static
int
ar_pow(Word n1, Word n2, Number result)
{ number left, right;
  int tl, tr;
  real l, r;

  TRY( tl = valueExpression(n1, &left) );
  TRY( tr = valueExpression(n2, &right) );

  l = (tl == V_INTEGER ? (real)left.i  : left.f);
  r = (tr == V_INTEGER ? (real)right.i : right.f);

  result->f = pow(l, r);

  return V_REAL;
}

static
int
ar_max(Word n1, Word n2, Number result)
{ number left, right;
  int tl, tr;

  TRY( tl = valueExpression(n1, &left) );
  TRY( tr = valueExpression(n2, &right) );

  if ( tl == V_INTEGER && tr == V_INTEGER )
  { result->i = (left.i > right.i ? left.i : right.i);
    return V_INTEGER;
  } else
  { real l = (tl == V_INTEGER ? (real)left.i  : left.f);
    real r = (tr == V_INTEGER ? (real)right.i : right.f);

    result->f = (l > r ? l : r);
  }

  return V_REAL;
}

static
int
ar_min(Word n1, Word n2, Number result)
{ number left, right;
  int tl, tr;

  TRY( tl = valueExpression(n1, &left) );
  TRY( tr = valueExpression(n2, &right) );

  if ( tl == V_INTEGER && tr == V_INTEGER )
  { result->i = (left.i < right.i ? left.i : right.i);
    return V_INTEGER;
  } else
  { real l = (tl == V_INTEGER ? (real)left.i  : left.f);
    real r = (tr == V_INTEGER ? (real)right.i : right.f);

    result->f = (l < r ? l : r);
  }

  return V_REAL;
}

static
int
ar_dot(Word c, Word nil, Number r)
{ long chr;

  if ( isInteger(*c) && isNil(*nil) )
  { if ((chr = valNum(*c)) >= 0 && chr <= 255)
    { r->i = chr;
      return V_INTEGER;
    }
  }
  return warning("is/2: illegal character specification");
}    

static
int
ar_negation(Word n1, Number r)
{ number arg;

  switch( valueExpression(n1, &arg) )
  { case V_INTEGER:
	r->i = ~arg.i;
	return V_INTEGER;
    case V_REAL:
	return warning("is/2: argument to \\/1 should be an integer");
    default:
	fail;
  }
}

static
int
ar_u_minus(Word n1, Number r)
{ number arg;

  switch( valueExpression(n1, &arg) )
  { case V_INTEGER:	r->i = -arg.i;
			return V_INTEGER;
    case V_REAL:	r->f = -arg.f;
			return V_REAL;
    default:		fail;
  }
}

static
int
ar_abs(Word n1, Number r)
{ number arg;

  switch( valueExpression(n1, &arg) )
  { case V_INTEGER:	r->i = (arg.i < 0 ? -arg.i : arg.i);
			return V_INTEGER;
    case V_REAL:	r->f = (arg.f < 0 ? -arg.f : arg.f);
			return V_REAL;
    default:		fail;
  }
}

static
int
ar_integer(Word n1, Number r)
{ number arg;

  switch( valueExpression(n1, &arg) )
  { case V_INTEGER:	r->i = arg.i;
			return V_INTEGER;
    case V_REAL:	r->i = (arg.f > 0 ? (long)(arg.f + 0.5)
					  : (long)(arg.f - 0.5));
			return V_INTEGER;
    default:		fail;
  }
}

static
int
ar_floor(Word n1, Number r)
{ number arg;

  switch( valueExpression(n1, &arg) )
  { case V_INTEGER:	r->i = arg.i;
			return V_INTEGER;
    case V_REAL:	r->i = (long)arg.f;
			return V_INTEGER;
    default:		fail;
  }
}

static
int
ar_ceil(Word n1, Number r)
{ number arg;

  switch( valueExpression(n1, &arg) )
  { case V_INTEGER:	r->i = arg.i;
			return V_INTEGER;
    case V_REAL:	r->i = (long)arg.f;
			if ( (real)r->i < arg.f )
			  (r->i)++;
			return V_INTEGER;
    default:		fail;
  }
}

static
int
ar_random(Word n1, Number r)
{ number arg;

  switch( valueExpression(n1, &arg) )
  { case V_INTEGER:	r->i = Random() % arg.i;
			return V_INTEGER;
    case V_REAL:	return warning("is/2: argument to random/1 should be a positive integer");
    default:		fail;
  }
}

static
int
ar_pi(Number r)
{ r->f = M_PI;

  return V_REAL;
}

static
int
ar_e(Number r)
{ r->f = M_E;

  return V_REAL;
}

static
int
ar_cputime(Number r)
{ r->f = CpuTime();

  return V_REAL;
}


		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

word
pl_is(Word v, Word e)
{ number arg;

  switch( valueExpression(e, &arg) )
  { case V_INTEGER:
	return unifyAtomic(v, consNum(arg.i));
    case V_REAL:
	return unifyAtomic(v, globalReal(arg.f));
    default:
	fail;
  }
}

#if O_PROLOG_FUNCTIONS
word
pl_arithmetic_function(Word descr)
{ Procedure proc;
  FunctorDef fd;
  register ArithFunction f;
  Module m = NULL;
  int v;

  if ( stripModule(descr, &m) == NULL )
    fail;

  if ( (proc = findCreateProcedure(descr)) == (Procedure)NULL )
    fail;
  if ( proc->functor->arity < 1 )
    return warning("arithmetic_function/1: Illegal arity");
  fd = lookupFunctorDef(proc->functor->name, proc->functor->arity - 1);

  if ( (f = isCurrentArithFunction(fd, m)) != NULL && f->module == m )
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
pl_current_arithmetic_function(Word f, word h)
{ ArithFunction a;
  Module m = NULL;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( (f = stripModule(f, &m)) == NULL )
	fail;

      if ( isVar(*f) )
      { a = arithFunctionTable[0];
        break;
      } else if ( isTerm(*f) )
      {	if ( isCurrentArithFunction(functorTerm(*f), m) != NULL )
	  succeed;
	fail;
      } else
        return warning("current_arithmetic_function/2: instantiation fault");
    case FRG_REDO:
      if ( (f = stripModule(f, &m)) == NULL )
	fail;

      a = (ArithFunction) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for( ; a; a = a->next )
  { Module m2;

    while( isRef((word)a) )
    { a = *((ArithFunction *)unRef(a));
      if ( a == (ArithFunction) NULL )
        fail;
    }

    for(m2 = m; m2; m2 = m2->super)
    { if ( m2 == a->module && a == isCurrentArithFunction(a->functor, m) )
      { if ( unifyFunctor(f, a->functor) == TRUE )
	{ return_next_table(ArithFunction, a);
	}
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
  ADD(FUNCTOR_div2,		ar_div),

  ADD(FUNCTOR_and2,		ar_conjunct),
  ADD(FUNCTOR_or2,		ar_disjunct),
  ADD(FUNCTOR_rshift2,		ar_shift_right),
  ADD(FUNCTOR_lshift2,		ar_shift_left),
  ADD(FUNCTOR_xor2,		ar_xor),
  ADD(FUNCTOR_backslash1,	ar_negation),

  ADD(FUNCTOR_dot2,		ar_dot),
  ADD(FUNCTOR_random1,		ar_random),

  ADD(FUNCTOR_integer1,		ar_integer),
  ADD(FUNCTOR_floor1,		ar_floor),
  ADD(FUNCTOR_ceil1,		ar_ceil),

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
      *f = (ArithFunction) makeRef(f+1);
  }

					/* initialise it */
  { register ArithFunction f;
    register int v;

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
{ return functions[(int)n].functor;
}


bool
ar_func_n(register code n, int argc, register Word *stack)
{ number result;
  int type;
  ArithFunction f = &functions[(int)n];

  (*stack) -= argc;
  if ( f->proc != (Procedure) NULL )
    type = prologFunction(f, *stack, &result);
  else
  { 
#define F    type = (*f->function)
#define A(n) ((*stack) + (n))
#define R    &result
    switch(argc)
    { case 0:	F(R); break;
      case 1:	F(A(0), R); break;
      case 2:	F(A(0), A(1), R); break;
      case 3:	F(A(0), A(1), A(2), R); break;
      case 4:	F(A(0), A(1), A(2), A(3), R); break;
      case 5:	F(A(0), A(1), A(2), A(3), A(4), R); break;
      default:  type = V_ERROR;
      		sysError("Too many arguments to arithmetic function");
    }
#undef R
#undef A
#undef F
  }

  switch( type )
  { case V_INTEGER:	*(*stack)++ = consNum(result.i);
			succeed;
    case V_REAL:	*(*stack)++ = globalReal(result.f);
			succeed;
    default:		fail;
  }
}

#endif /* O_COMPILE_ARITH */

word
evaluate(Word p)
{ number result;

  switch( valueExpression(p, &result) )  
  { case V_INTEGER:	return consNum(result.i);
    case V_REAL:	return globalReal(result.f);
    case V_ERROR:
    default:		fail;
  }
}
