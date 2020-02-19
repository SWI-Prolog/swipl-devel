/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2020, University of Amsterdam
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

#ifndef O_PLGMP_INCLUDED
#define O_PLGMP_INCLUDED

#define COMMON(type) SO_LOCAL type

#ifdef O_GMP
#include <gmp.h>

#define O_MY_GMP_ALLOC 1
#define O_GMP_PRECHECK_ALLOCATIONS 1	/* GMP 4.2.3 uses abort() sometimes */

COMMON(void)	initGMP(void);
COMMON(void)	cleanupGMP(void);
COMMON(void)	get_integer(word w, number *n);
COMMON(void)	get_rational(word w, number *n);
COMMON(Code)	get_mpz_from_code(Code pc, mpz_t mpz);
COMMON(Code)	get_mpq_from_code(Code pc, mpq_t mpq);
COMMON(int)	promoteToMPZNumber(number *n);
COMMON(int)	promoteToMPQNumber(number *n);
COMMON(void)	ensureWritableNumber(Number n);
COMMON(void)	clearGMPNumber(Number n);
COMMON(void)	addMPZToBuffer(Buffer b, mpz_t mpz);
COMMON(void)	addMPQToBuffer(Buffer b, mpq_t mpq);
COMMON(char *)	loadMPZFromCharp(const char *data, Word r, Word *store);
COMMON(char *)	loadMPQFromCharp(const char *data, Word r, Word *store);
COMMON(char *)	skipMPZOnCharp(const char *data);
COMMON(char *)	skipMPQOnCharp(const char *data);
COMMON(int)	mpz_to_int64(mpz_t mpz, int64_t *i);
COMMON(int)	mpz_to_uint64(mpz_t mpz, uint64_t *i);
COMMON(void)	mpz_init_set_si64(mpz_t mpz, int64_t i);
COMMON(double)	mpX_round(double f);

#define clearNumber(n) \
	do { if ( (n)->type != V_INTEGER ) clearGMPNumber(n); } while(0)

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
{ return (int)(w>>1);
}

static inline int
mpq_stack_size(word w)
{ return (int)(w>>1);
}

#else /*O_GMP*/

#define get_integer(w, n) \
	do \
	{ (n)->type = V_INTEGER; \
	  (n)->value.i = valInteger(w); \
	} while(0)
#define get_rational(w, n) \
	get_integer(w, n)

#define clearGMPNumber(n)	(void)0
#define clearNumber(n)		(void)0
#define ensureWritableNumber(n) (void)0
#define initGMP()		(void)0

#endif /*O_GMP*/


		 /*******************************
		 *	  GMP ALLOCATION	*
		 *******************************/

#define FE_NOTSET (-1)

#if O_MY_GMP_ALLOC
typedef struct mp_mem_header
{ struct mp_mem_header *prev;
  struct mp_mem_header *next;
  struct ar_context *context;
} mp_mem_header;

typedef struct ar_context
{ struct ar_context *parent;
  size_t	     allocated;
  int		     femode;
} ar_context;

#define O_GMP_LEAK_CHECK 0
#if O_GMP_LEAK_CHECK
#define GMP_LEAK_CHECK(g) g
#else
#define GMP_LEAK_CHECK(g)
#endif

#define AR_CTX	ar_context __PL_ar_ctx = {0};
#define AR_BEGIN() \
	do \
	{ __PL_ar_ctx.parent    = LD->gmp.context; \
	  __PL_ar_ctx.femode    = FE_NOTSET; \
	  LD->gmp.context	= &__PL_ar_ctx; \
	  GMP_LEAK_CHECK(__PL_ar_ctx.allocated = LD->gmp.allocated); \
	} while(0)
#define AR_END() \
	do \
	{ LD->gmp.context = __PL_ar_ctx.parent; \
	  GMP_LEAK_CHECK(if ( __PL_ar_ctx.allocated != LD->gmp.allocated ) \
			 { Sdprintf("GMP: lost %ld bytes\n", \
				    LD->gmp.allocated-__PL_ar_ctx.allocated); \
			 }) \
	} while(0)
#define AR_CLEANUP() \
	do \
	{ if ( __PL_ar_ctx.femode != FE_NOTSET ) \
	    fesetround(__PL_ar_ctx.femode); \
	  mp_cleanup(&__PL_ar_ctx); \
	} while(0)

COMMON(void)	mp_cleanup(ar_context *ctx);

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
