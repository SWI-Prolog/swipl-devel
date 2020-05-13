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

COMMON(int)		ar_compare(Number n1, Number n2, int what);
COMMON(int)		ar_compare_eq(Number n1, Number n2);
COMMON(int)		pl_ar_add(Number n1, Number n2, Number r);
COMMON(int)		ar_mul(Number n1, Number n2, Number r);
COMMON(word)		pl_current_arithmetic_function(term_t f, control_t h);
COMMON(void)		initArith(void);
COMMON(void)		cleanupArith(void);
COMMON(int)		indexArithFunction(functor_t fdef);
COMMON(functor_t)	functorArithFunction(unsigned int n);
COMMON(bool)		ar_func_n(int findex, int argc ARG_LD);
COMMON(int)		ar_add_ui(Number n, intptr_t add);
COMMON(int)		valueExpression(term_t p, Number n ARG_LD);
COMMON(int)		toIntegerNumber(Number n, int flags);
COMMON(int)		arithChar(Word p ARG_LD);
COMMON(int)		getCharExpression(Word p, Number r ARG_LD);
COMMON(Number)		growArithStack(ARG1_LD);
COMMON(void)		freeArithLocalData(PL_local_data_t *ld);
COMMON(int)		ar_sign_i(Number n1);
COMMON(int)		ar_signbit(Number n1);
COMMON(int)		check_float(Number n);
COMMON(int)		ar_rdiv_mpz(Number n1, Number n2, Number r);
COMMON(int)		PL_eval_expression_to_int64_ex(term_t t, int64_t *val);
COMMON(int)		is_arith_flag(atom_t k);
COMMON(int)		get_arith_flag(term_t val, atom_t k ARG_LD);
COMMON(int)		set_arith_flag(term_t val, atom_t k ARG_LD);
COMMON(void)		set_rounding(int mode);
COMMON(int)		atom_to_rounding(atom_t a, int *m);
COMMON(atom_t)		float_rounding_name(int m);


		 /*******************************
		 *	 INLINE FUNCTIONS	*
		 *******************************/

static inline Number
allocArithStack(ARG1_LD)
{ if ( unlikely(LD->arith.stack.top == LD->arith.stack.max) )
    return growArithStack(PASS_LD1);

  return LD->arith.stack.top++;
}

static inline void
pushArithStack(Number n ARG_LD)
{ Number np = allocArithStack(PASS_LD1);

  *np = *n;				/* structure copy */
}

static inline void
resetArithStack(ARG1_LD)
{ LD->arith.stack.top = LD->arith.stack.base;
}

static inline Number
argvArithStack(int n ARG_LD)
{ DEBUG(0, assert(LD->arith.stack.top - n >= LD->arith.stack.base));

  return LD->arith.stack.top - n;
}

static inline void
popArgvArithStack(int n ARG_LD)
{ DEBUG(0, assert(LD->arith.stack.top - n >= LD->arith.stack.base));

  for(; n>0; n--)
  { LD->arith.stack.top--;
    clearNumber(LD->arith.stack.top);
  }
}

		 /*******************************
		 *	      MPZ/MPQ		*
		 *******************************/

static inline int
isMPQNum__LD(word w ARG_LD)
{ if ( tagex(w) == (TAG_INTEGER|STG_GLOBAL) )
  { Word p = addressIndirect(w);
    size_t wsize = wsizeofInd(*p);

    if ( wsize == WORDS_PER_INT64 )
      return FALSE;

    return p[1]&MP_RAT_MASK;
  }

  return FALSE;
}

static inline int
isMPZNum__LD(word w ARG_LD)
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
