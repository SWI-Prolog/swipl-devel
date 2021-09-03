/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2021, VU University Amsterdam
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
void GC_linger(void *ptr);
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
  gen_t		generation;		/* Linger generation */
  void		*object;		/* The lingering data */
  void	       (*unalloc)(void* obj);   /* actually free the object */
} linger_list;

void	linger(linger_list** list, void (*unalloc)(void *), void *object);
void	free_lingering(linger_list **list, gen_t generation);


		 /*******************************
		 *	     PROTOTYPES		*
		 *******************************/

#if USE_LD_MACROS
#define	allocGlobal(words)			LDFUNC(allocGlobal, words)
#define	allocGlobalNoShift(words)		LDFUNC(allocGlobalNoShift, words)
#define	f_pushArgumentStack(p)			LDFUNC(f_pushArgumentStack, p)
#define	allocString(len)			LDFUNC(allocString, len)
#define	getCharsString(w, len)			LDFUNC(getCharsString, w, len)
#define	getCharsWString(w, len)			LDFUNC(getCharsWString, w, len)
#define	put_double(p, f, flags)			LDFUNC(put_double, p, f, flags)
#define	put_int64(p, i, flags)			LDFUNC(put_int64, p, i, flags)
#define	VM_globalIndirectFromCode(pc)		LDFUNC(VM_globalIndirectFromCode, pc)
#define	VM_equalIndirectFromCode(a, pc)		LDFUNC(VM_equalIndirectFromCode, a, pc)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		initAlloc(void);
int		initMalloc(void);
size_t		heapUsed(void);
#ifndef DMALLOC
void *		allocHeap(size_t n);
void *		allocHeapOrHalt(size_t n);
void		freeHeap(void *mem, size_t n);
#endif /*DMALLOC*/
int		enableSpareStack(Stack s, int always);
void		enableSpareStacks(void);
int		outOfStack(void *stack, stack_overflow_action how);
int		raiseStackOverflow(int which);
void		outOfCore(void) NORETURN;
Word		allocGlobal(size_t words);
Word		allocGlobalNoShift(size_t words);
void		f_pushArgumentStack(Word p);
void		initMemAlloc(void);
Word		allocString(size_t len);
word		globalString(size_t len, const char *s);
word		globalWString(size_t len, const pl_wchar_t *s);
char *		getCharsString(word w, size_t *len);
pl_wchar_t *	getCharsWString(word w, size_t *len);
Word		newTerm(void);
int		put_double(Word p, double f, int flags);
int		put_int64(Word p, int64_t i, int flags);
/* valBignum(word w) moved to pl-inline.h */
int		equalIndirect(word r1, word r2);
ALLOC_INLINE
size_t		gsizeIndirectFromCode(Code PC);
word		globalIndirectFromCode(Code *PC);
void *		tmp_malloc(size_t req);
void *		tmp_realloc(void *mem, size_t req);
void		tmp_free(void *mem);
size_t		tmp_nalloc(size_t req);
size_t		tmp_nrealloc(void *mem, size_t req);
void *		stack_malloc(size_t req);
void *		stack_realloc(void *mem, size_t req);
void		stack_free(void *mem);
size_t		stack_nalloc(size_t req);
size_t		stack_nrealloc(void *mem, size_t req);
#ifndef xmalloc
void *		xmalloc(size_t size);
void *		xrealloc(void *mem, size_t size);
#endif
#if USE_ALLOC_INLINES || EMIT_ALLOC_INLINES
struct word_and_Code {
	word word;
	Code code;
};
#define WORD_AND_CODE(w,c) ((struct word_and_Code){(w),(c)})

ALLOC_INLINE struct word_and_Code
			VM_globalIndirectFromCode(Code pc);
ALLOC_INLINE struct word_and_Code
			VM_equalIndirectFromCode(word a, Code pc);
#endif

#undef LDFUNC_DECLARATIONS

#endif /*PL_ALLOC_H_INCLUDED*/
