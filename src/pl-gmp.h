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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef O_PLGMP_INCLUDED
#define O_PLGMP_INCLUDED

#define COMMON(type) SO_LOCAL type

#ifdef O_GMP
#include <gmp.h>

#define O_MY_GMP_ALLOC 1
#define O_GMP_PRECHECK_ALLOCATIONS 1	/* GMP 4.2.3 uses abort() sometimes */

COMMON(void) 	initGMP(void);
COMMON(void) 	cleanupGMP(void);
COMMON(void)	get_integer(word w, number *n);
COMMON(void)	promoteToMPZNumber(number *n);
COMMON(void)	promoteToMPQNumber(number *n);
COMMON(void)	ensureWritableNumber(Number n);
COMMON(void)	clearGMPNumber(Number n);
COMMON(void)	addMPZToBuffer(Buffer b, mpz_t mpz);
COMMON(char *)	loadMPZFromCharp(const char *data, Word r, Word *store);
COMMON(char *)	skipMPZOnCharp(const char *data);
COMMON(int)	mpz_to_int64(mpz_t mpz, int64_t *i);

#define clearNumber(n) \
	do { if ( (n)->type != V_INTEGER ) clearGMPNumber(n); } while(0)
#else /*O_GMP*/

#define get_integer(w, n) \
	do \
	{ (n)->type = V_INTEGER; \
	  (n)->value.i = valInteger(w); \
	} while(0)

#define clearGMPNumber(n)	(void)0
#define clearNumber(n)		(void)0
#define ensureWritableNumber(n) (void)0
#define initGMP()		(void)0

#endif /*O_GMP*/


		 /*******************************
		 *	  GMP ALLOCATION	*
		 *******************************/

#if O_MY_GMP_ALLOC
typedef struct mp_mem_header
{ struct mp_mem_header *prev;
  struct mp_mem_header *next;
  struct ar_context *context;
} mp_mem_header;

typedef struct ar_context
{ struct ar_context *parent;
  size_t	     allocated;
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
	mp_cleanup(&__PL_ar_ctx)

COMMON(void)	mp_cleanup(ar_context *ctx);

#else /*O_MY_GMP_ALLOC*/

#define AR_CTX
#define AR_BEGIN()	(void)0
#define AR_END()	(void)0
#define AR_CLEANUP()	(void)0

#endif /*O_MY_GMP_ALLOC*/

#endif /*O_PLGMP_INCLUDED*/
