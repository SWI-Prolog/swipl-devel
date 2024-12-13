/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2024, University of Amsterdam
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

#include "pl-incl.h"

#ifndef O_PLGMP_INCLUDED
#define O_PLGMP_INCLUDED

#if USE_LD_MACROS
#define	PL_unify_number(t, n)		LDFUNC(PL_unify_number, t, n)
#define	PL_put_number(t, n)		LDFUNC(PL_put_number, t, n)
#define	get_number(w, n)		LDFUNC(get_number, w, n)
#define	PL_get_number(t, n)		LDFUNC(PL_get_number, t, n)
#define	put_int64(p, i, flags)		LDFUNC(put_int64, p, i, flags)
#define	put_uint64(at, l, flags)	LDFUNC(put_uint64, at, l, flags)
#define	put_number(at, n, flags)	LDFUNC(put_number, at, n, flags)
#define get_int64(w, ip)		LDFUNC(get_int64, w, ip)
#ifdef O_BIGNUM
#define	get_rational_no_int(w, n)	LDFUNC(get_rational_no_int, w, n)
#endif
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int	PL_unify_number(term_t t, Number n);
int	PL_put_number(term_t t, Number n);
void	get_number(word w, Number n);
int	PL_get_number(term_t t, Number n);
int	PL_get_number(term_t t, Number n);
int	put_uint64(Word at, uint64_t l, int flags);
int	put_int64(Word p, int64_t i, int flags);
int	put_number(Word at, Number n, int flags);
bool	get_int64(word w, int64_t *ip);
bool	promoteToFloatNumber(Number n);
bool	make_same_type_numbers(Number n1, Number n2) WUNUSED;
bool    promoteNumber(Number n1, numtype type) WUNUSED;
int	cmpNumbers(Number n1, Number n2);
int	cmpReals(Number n1, Number n2);
void	cpNumber(Number to, Number from);
#ifdef O_BIGNUM
void	get_rational_no_int(word w, number *n);
#endif

#undef LDFUNC_DECLARATIONS

#if O_BIGNUM
#if O_GMP
#include <gmp.h>

#define MPZ_ON_STACK(n)		(!((n)->_mp_alloc))
#define MPZ_SET_READONLY(n)	((n)->_mp_alloc = 0)
#define MPZ_LIMB_SIZE(n)	((n)->_mp_size)
#define MPZ_LIMBS(n)		((n)->_mp_d)
#define MPZ_STACK_EXTRA		(1)
#elif O_BF
#include "libbf/bf_gmp.h"
#include "pl-bf.h"

#define MPZ_ON_STACK(n)		(!(n->ctx))
#define MPZ_SET_READONLY(n)	((n)->ctx = NULL)
#define MPZ_LIMB_SIZE(n)	((n)->sign ? -(n)->len : (n)->len)
#define MPZ_LIMBS(n)		((n)->tab)
#define MPZ_STACK_EXTRA		(2)
#endif

#define O_MY_GMP_ALLOC 1
#define O_GMP_PRECHECK_ALLOCATIONS 1	/* GMP 4.2.3 uses abort() sometimes */

void	initGMP(void);
void	cleanupGMP(void);
void	get_bigint(word w, number *n);
Code	get_mpz_from_code(Code pc, mpz_t mpz);
Code	get_mpq_from_code(Code pc, mpq_t mpq);
int	promoteToMPZNumber(number *n);
int	promoteToMPQNumber(number *n);
void	ensureWritableNumber(Number n);
void	clearGMPNumber(Number n);
void	addMPZToBuffer(Buffer b, mpz_t mpz);
void	addMPQToBuffer(Buffer b, mpq_t mpq);
char *	loadMPZFromCharp(const char *data, Word r, Word *store);
char *	loadMPQFromCharp(const char *data, Word r, Word *store);
char *	skipMPZOnCharp(const char *data);
char *	skipMPQOnCharp(const char *data);
int	mpz_to_int64(mpz_t mpz, int64_t *i);
int	mpz_to_uint64(mpz_t mpz, uint64_t *i);
void	mpz_init_set_si64(mpz_t mpz, int64_t i);
double	mpz_to_double(mpz_t n);
double	mpq_to_double(mpq_t q);
void	mpq_set_double(mpq_t q, double f);
word	bignum_index(const word *p);

static inline void
clearNumber(Number n)
{ switch(n->type)
  { case V_INTEGER:
    case V_FLOAT:
      return;
    default:
      clearGMPNumber(n);
  }
}

static inline word
mpz_size_stack(int sz)
{ return ((word)sz<<1) & ~(word)MP_RAT_MASK;
}

static inline word
mpq_size_stack(int sz)
{ return ((word)sz<<1) | MP_RAT_MASK;
}

static inline int
mpz_stack_size(word w)
{ return (int)w>>1;
}

static inline int
mpq_stack_size(word w)
{ return (int)w>>1;
}

#ifdef O_GMP
static inline void
mpz_add_si(mpz_t r, const mpz_t n1, long add)
{ if ( add > 0 )
    mpz_add_ui(r, n1, add);
  else
    mpz_sub_ui(r, n1, -add);
}
#endif

static inline void
get_integer(word w, Number n)
{ if ( storage(w) == STG_INLINE )
  { n->type = V_INTEGER,
    n->value.i = valInt(w);
  } else
  { get_bigint(w, n);
  }
}


#define get_rational(w, n) LDFUNC(get_rational, w, n)
static inline void
get_rational(DECL_LD word w, Number n)
{ if ( storage(w) == STG_INLINE )
  { n->value.i = valInt(w);
    n->type = V_INTEGER;
  } else
  { get_rational_no_int(w, n);
  }
}

#else /*O_BIGNUM*/

#define get_integer(w, n) \
	do \
	{ (n)->type = V_INTEGER; \
	  (n)->value.i = valInt(w); \
	} while(0)
#define get_rational(w, n) \
	get_integer(w, n)

#define clearGMPNumber(n)	(void)0
#define clearNumber(n)		(void)0
#define ensureWritableNumber(n) (void)0
#define initGMP()		(void)0

#endif /*O_BIGNUM*/


		 /*******************************
		 *	  GMP ALLOCATION	*
		 *******************************/

#define FE_NOTSET (-1)
#define GMP_STACK_ALLOC 1024	/* in size_t units */

#if O_MY_GMP_ALLOC
typedef struct mp_mem_header
{ struct mp_mem_header *prev;
  struct mp_mem_header *next;
} mp_mem_header;

typedef struct ar_context
{ mp_mem_header	    *head;
  mp_mem_header	    *tail;
  int		     femode;
  size_t	     allocated;
  size_t	    *alloc_buf;
} ar_context;

#define AR_CTX \
	size_t     __PL_ar_buf[GMP_STACK_ALLOC]; \
	ar_context __PL_ar_ctx = {.alloc_buf = __PL_ar_buf};

#define AR_BEGIN() \
	do \
	{ assert(LD->gmp.context == NULL); \
	  __PL_ar_ctx.femode    = FE_NOTSET; \
	  LD->gmp.context	= &__PL_ar_ctx; \
	} while(0)
#define AR_END() \
	do \
	{ LD->gmp.context = NULL; \
	} while(0)
#define AR_CLEANUP() \
	do \
	{ LD->gmp.context = NULL; \
          if ( __PL_ar_ctx.femode != FE_NOTSET )	    \
	    fesetround(__PL_ar_ctx.femode); \
	  mp_cleanup(&__PL_ar_ctx); \
	} while(0)

#define AR_PERSISTENT(g)				\
  do							\
  { ar_context *__PL_ar_ctx_saved = LD->gmp.context;	\
  LD->gmp.context = NULL;				\
  g;							\
  LD->gmp.context = __PL_ar_ctx_saved;			\
  } while(0)


void	mp_cleanup(ar_context *ctx);

#else /*O_MY_GMP_ALLOC*/

typedef struct ar_context
{ int		     femode;
} ar_context;


#define AR_CTX		ar_context __PL_ar_ctx = {0};
#define AR_BEGIN() \
	do { __PL_ar_ctx.femode    = FE_NOTSET; \
	   } while(0)
#define AR_END()	(void)0
#define AR_CLEANUP() \
	do \
	{ if ( __PL_ar_ctx.femode != FE_NOTSET ) \
	    fesetround(__PL_ar_ctx.femode); \
	} while(0)

#endif /*O_MY_GMP_ALLOC*/

#endif /*O_PLGMP_INCLUDED*/
