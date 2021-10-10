/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2021, University of Amsterdam
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

#ifndef _PL_GC_H
#define _PL_GC_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	f_ensureStackSpace(gcells, tcells, flags)	LDFUNC(f_ensureStackSpace, gcells, tcells, flags)
#define	growLocalSpace(bytes, flags)			LDFUNC(growLocalSpace, bytes, flags)
#define	unmark_stacks(fr, ch, mask)			LDFUNC(unmark_stacks, fr, ch, mask)
#define	blockGC(flags)					LDFUNC(blockGC, flags)
#define	unblockGC(flags)				LDFUNC(unblockGC, flags)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int		considerGarbageCollect(Stack s);
void		call_tune_gc_hook(void);
int		garbageCollect(gc_reason_t reason);
word		pl_garbage_collect(term_t d);
gc_stat *	last_gc_stats(gc_stats *stats);
Word		findGRef(int n);
size_t		nextStackSizeAbove(size_t n);
int		shiftTightStacks(void);
int		growStacks(size_t l, size_t g, size_t t);
size_t		nextStackSize(Stack s, size_t minfree);
int		makeMoreStackSpace(int overflow, int flags);
int		f_ensureStackSpace(size_t gcells, size_t tcells,
				   int flags);
int		growLocalSpace(size_t bytes, int flags);
void		clearUninitialisedVarsFrame(LocalFrame, Code);
void		clearLocalVariablesFrame(LocalFrame fr);
void		setLTopInBody(void);
word		check_foreign(void);	/* DEBUG(CHK_SECURE...) stuff */
void		markAtomsOnStacks(PL_local_data_t *ld, void *ctx);
void		markPredicatesInEnvironments(PL_local_data_t *ld,
					     void *ctx);
QueryFrame	queryOfFrame(LocalFrame fr);
void		mark_active_environment(struct bit_vector *active,
					LocalFrame fr, Code PC);
void		unmark_stacks(LocalFrame fr, Choice ch, uintptr_t mask);
void		blockGC(int flags);	/* disallow garbage collect */
void		unblockGC(int flags);	/* re-allow garbage collect */

#undef LDFUNC_DECLARATIONS

/* Convenience macros */

#define ensureLocalSpace(n)	likely(ensureLocalSpace_ex(n))
#define ensureGlobalSpace(n,f)  likely(ensureStackSpace_ex(n,0,f))
#define ensureTrailSpace(n)     likely(ensureStackSpace_ex(0,n,ALLOW_GC))
#define ensureStackSpace(g,t)   likely(ensureStackSpace_ex(g,t,ALLOW_GC))

		 /*******************************
		 *	INLINE DEFINITIONS	*
		 *******************************/

#define ensureLocalSpace_ex(bytes) LDFUNC(ensureLocalSpace_ex, bytes)
static inline int
ensureLocalSpace_ex(DECL_LD size_t bytes)
{ int rc;

  if ( likely(addPointer(lTop, bytes) <= (void*)lMax) )
    return TRUE;

  if ( (rc=growLocalSpace(bytes, ALLOW_SHIFT)) == TRUE )
    return TRUE;

  return raiseStackOverflow(rc);
}

#define ensureStackSpace_ex(gcells, tcells, flags) LDFUNC(ensureStackSpace_ex, gcells, tcells, flags)
static inline int
ensureStackSpace_ex(DECL_LD size_t gcells, size_t tcells, int flags)
{ gcells += BIND_GLOBAL_SPACE;
  tcells += BIND_TRAIL_SPACE;

  if ( likely(gTop+gcells <= gMax) && likely(tTop+tcells <= tMax) )
    return TRUE;

  return f_ensureStackSpace(gcells, tcells, flags);
}

#endif /*_PL_GC_H*/
