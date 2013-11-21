/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012 VU University Amsterdam

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
		 *	     PROTOTYPES		*
		 *******************************/

COMMON(void)		initAlloc(void);
COMMON(void)		freeHeap(void *mem, size_t n);
COMMON(int)		enableSpareStack(Stack s);
COMMON(int)		outOfStack(void *stack, stack_overflow_action how);
COMMON(int)		raiseStackOverflow(int which);
COMMON(void)		outOfCore(void) NORETURN;
COMMON(Word)		allocGlobal__LD(size_t words ARG_LD);
COMMON(Word)		allocGlobalNoShift__LD(size_t words ARG_LD);
COMMON(void *)		allocHeap(size_t n);
COMMON(void *)		allocHeapOrHalt(size_t n);
COMMON(void)		pushArgumentStack__LD(Word p ARG_LD);
COMMON(void)		initMemAlloc(void);
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
