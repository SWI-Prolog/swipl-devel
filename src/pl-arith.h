/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
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

#ifndef PL_ARITH_H
#define PL_ARITH_H 1

/* LD->arith.float_flags values */
#define FLT_ROUND_NEAREST	0x0001
#define FLT_ROUND_TO_POS	0x0002
#define FLT_ROUND_TO_NEG	0x0003
#define FLT_ROUND_TO_ZERO	0x0004
#define FLT_ROUND_MASK		0x000f
#define FLT_OVERFLOW		0x0010
#define FLT_ZERO_DIV		0x0020
#define FLT_UNDEFINED		0x0040
#define FLT_UNDERFLOW		0x0080

#if USE_LD_MACROS
#define	ar_func_n(findex, argc)		LDFUNC(ar_func_n, findex, argc)
#define	valueExpression(p, n)		LDFUNC(valueExpression, p, n)
#define	arithChar(p)			LDFUNC(arithChar, p)
#define	getCharExpression(p, r)		LDFUNC(getCharExpression, p, r)
#define	growArithStack(_)		LDFUNC(growArithStack, _)
#define	get_arith_flag(val, k)		LDFUNC(get_arith_flag, val, k)
#define	set_arith_flag(val, k)		LDFUNC(set_arith_flag, val, k)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int		ar_compare(Number n1, Number n2, int what);
int		ar_compare_eq(Number n1, Number n2);
int		pl_ar_add(Number n1, Number n2, Number r);
int		ar_mul(Number n1, Number n2, Number r);
word		pl_current_arithmetic_function(term_t f, control_t h);
void		initArith(void);
void		cleanupArith(void);
int		indexArithFunction(functor_t fdef);
functor_t	functorArithFunction(unsigned int n);
bool		ar_func_n(int findex, int argc);
int		ar_add_ui(Number n, intptr_t add);
int		valueExpression(term_t p, Number n);
int		toIntegerNumber(Number n, int flags);
int		arithChar(Word p);
int		getCharExpression(Word p, Number r);
Number		growArithStack(void);
void		freeArithLocalData(PL_local_data_t *ld);
int		ar_sign_i(Number n1);
int		ar_signbit(Number n1);
int		check_float(Number n);
int		ar_rdiv_mpz(Number n1, Number n2, Number r);
int		PL_eval_expression_to_int64_ex(term_t t, int64_t *val);
int		is_arith_flag(atom_t k);
int		get_arith_flag(term_t val, atom_t k);
int		set_arith_flag(term_t val, atom_t k);
void		set_rounding(int mode);
int		atom_to_rounding(atom_t a, int *m);
atom_t		float_rounding_name(int m);
double		PL_nan(void);

#undef LDFUNC_DECLARATIONS


		 /*******************************
		 *	 INLINE FUNCTIONS	*
		 *******************************/

#define allocArithStack(_) LDFUNC(allocArithStack, _)
static inline Number
allocArithStack(DECL_LD)
{ if ( unlikely(LD->arith.stack.top == LD->arith.stack.max) )
    return growArithStack();

  return LD->arith.stack.top++;
}

#define pushArithStack(n) LDFUNC(pushArithStack, n)
static inline void
pushArithStack(DECL_LD Number n)
{ Number np = allocArithStack();

  *np = *n;				/* structure copy */
}

#define resetArithStack(_) LDFUNC(resetArithStack, _)
static inline void
resetArithStack(DECL_LD)
{ LD->arith.stack.top = LD->arith.stack.base;
}

#define argvArithStack(n) LDFUNC(argvArithStack, n)
static inline Number
argvArithStack(DECL_LD int n)
{ DEBUG(0, assert(LD->arith.stack.top - n >= LD->arith.stack.base));

  return LD->arith.stack.top - n;
}

#define popArgvArithStack(n) LDFUNC(popArgvArithStack, n)
static inline void
popArgvArithStack(DECL_LD int n)
{ DEBUG(0, assert(LD->arith.stack.top - n >= LD->arith.stack.base));

  for(; n>0; n--)
  { LD->arith.stack.top--;
    clearNumber(LD->arith.stack.top);
  }
}

		 /*******************************
		 *	      MPZ/MPQ		*
		 *******************************/

#define isMPQNum(w) LDFUNC(isMPQNum, w)
static inline int
isMPQNum(DECL_LD word w)
{ if ( tagex(w) == (TAG_INTEGER|STG_GLOBAL) )
  { Word p = addressIndirect(w);
    size_t wsize = wsizeofInd(*p);

    if ( wsize == WORDS_PER_INT64 )
      return FALSE;

    return p[1]&MP_RAT_MASK;
  }

  return FALSE;
}

#define isMPZNum(w) LDFUNC(isMPZNum, w)
static inline int
isMPZNum(DECL_LD word w)
{ if ( tagex(w) == (TAG_INTEGER|STG_GLOBAL) )
  { Word p = addressIndirect(w);
    size_t wsize = wsizeofInd(*p);

    if ( wsize == WORDS_PER_INT64 )
      return FALSE;

    return !(p[1]&MP_RAT_MASK);
  }

  return FALSE;
}

#endif /*PL_ARITH_H*/
