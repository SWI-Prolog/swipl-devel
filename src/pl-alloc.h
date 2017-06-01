/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2017, VU University Amsterdam
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

#ifndef PL_ALLOC_H_INCLUDED
#define PL_ALLOC_H_INCLUDED

#ifdef HAVE_BOEHM_GC

#ifdef O_PLMT
#define GC_THREADS 1
#endif
#define DYNAMIC_MARKS
#include <gc/gc.h>

#define allocForeignState(size)			GC_MALLOC_UNCOLLECTABLE(size)
#define freeForeignState(ptr, size)		GC_FREE(ptr)

#ifdef GC_DEBUG
COMMON(void) GC_linger(void *ptr);
#define GC_LINGER(p)				GC_linger(p)
#else
#define GC_LINGER(p)				((void)p)
#endif

#else /*HAVE_BOEHM_GC*/

#define GC_MALLOC(n)				malloc(n)
#define GC_MALLOC_ATOMIC(n)			malloc(n)
#define GC_MALLOC_IGNORE_OFF_PAGE(n)		malloc(n)
#define GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(n)	malloc(n)
#define GC_MALLOC_UNCOLLECTABLE(n)		malloc(n)
#define GC_MALLOC_ATOMIC_UNCOLLECTABLE(n)	malloc(n)
#define GC_REALLOC(p,s)				realloc(p,s)
#define GC_FREE(p)				free(p)
#define GC_LINGER(p)				((void)p)

#define allocForeignState(size)			allocHeapOrHalt(size)
#define freeForeignState(ptr, size)		freeHeap(ptr, size)

#endif /*HAVE_BOEHM_GC*/

		 /*******************************
		 *	      LINGER		*
		 *******************************/

typedef struct linger_list
{ struct linger_list *next;		/* Next lingering object */
  void		*object;		/* The lingering data */
  void	       (*unalloc)(void* obj);   /* actually free the object */
} linger_list;

COMMON(void)	linger(linger_list** list, void (*unalloc)(void *), void *object);
COMMON(void)	free_lingering(linger_list **list);


		 /*******************************
		 *	     PROTOTYPES		*
		 *******************************/

COMMON(void)		initAlloc(void);
#ifndef DMALLOC
COMMON(void *)		allocHeap(size_t n);
COMMON(void *)		allocHeapOrHalt(size_t n);
COMMON(void)		freeHeap(void *mem, size_t n);
#endif /*DMALLOC*/
COMMON(int)		enableSpareStack(Stack s);
COMMON(int)		outOfStack(void *stack, stack_overflow_action how);
COMMON(int)		raiseStackOverflow(int which);
COMMON(void)		outOfCore(void) NORETURN;
COMMON(Word)		allocGlobal__LD(size_t words ARG_LD);
COMMON(Word)		allocGlobalNoShift__LD(size_t words ARG_LD);
COMMON(void)		pushArgumentStack__LD(Word p ARG_LD);
COMMON(void)		initMemAlloc(void);
COMMON(Word)		allocString(size_t len ARG_LD);
COMMON(word)		globalString(size_t len, const char *s);
COMMON(word)		globalWString(size_t len, const pl_wchar_t *s);
COMMON(char *)		getCharsString__LD(word w, size_t *len ARG_LD);
COMMON(pl_wchar_t *)	getCharsWString__LD(word w, size_t *len ARG_LD);
COMMON(Word)		newTerm(void);
COMMON(int)		put_double(Word p, double f, int flags ARG_LD);
COMMON(int)		put_int64(Word p, int64_t i, int flags ARG_LD);
#if ALIGNOF_INT64_T != ALIGNOF_VOIDP
COMMON(int64_t)		valBignum__LD(word w ARG_LD);
#endif
COMMON(int)		equalIndirect(word r1, word r2);
COMMON(size_t)		gsizeIndirectFromCode(Code PC);
COMMON(word)		globalIndirectFromCode(Code *PC);
#ifndef xmalloc
COMMON(void *)		xmalloc(size_t size);
COMMON(void *)		xrealloc(void *mem, size_t size);
#endif

#endif /*PL_ALLOC_H_INCLUDED*/
