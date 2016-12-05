/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2016, University of Amsterdam
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

#ifndef PL_PRIVITF_H_INCLUDED
#define PL_PRIVITF_H_INCLUDED

COMMON(int)	PL_get_char(term_t c, int *p, int eof);
COMMON(int)	PL_unify_char(term_t chr, int c, int mode);
COMMON(int)	PL_unify_predicate(term_t head, predicate_t pred, int how);



		 /*******************************
		 *	    LIST BUILDING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Quickly create a list on the stack. This   is for creating lists were we
can give an upperbound to the length  in advance. By allocation upfront,
we know there are no garbage  collections   or  stack-shifts  and we can
avoid using term-references to address the list.

    * allocList(size_t maxcells, list_ctx *ctx)
    Allocate enough space on the stack for a list of maxcells elements.
    The final list may be shorter!

    * addSmallIntList(list_ctx *ctx, int value)
    Add a small integer to the list

    * unifyList(term_t term, list_ctx *ctx);
    Unify term with the created list.  This closes the list and adjusts
    the top of the stack.

    * unifyDiffList(term_t head, term_t tail, list_ctx *ctx);
    Represent the list as Head\Tail.  This adjusts the top of the stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct list_ctx
{ Word lp;
  Word gstore;
} list_ctx;

static inline void
addAtomicList__LD(list_ctx *ctx, word value ARG_LD)
{ ctx->gstore[0] = consPtr(&ctx->gstore[1], TAG_COMPOUND|STG_GLOBAL);
  ctx->gstore[1] = FUNCTOR_dot2;
  ctx->gstore[2] = value;
  ctx->gstore += 3;
}

#define addSmallIntList(ctx, i) addAtomicList__LD(ctx, consInt(i)    PASS_LD)
#define addCharList(ctx, c)     addAtomicList__LD(ctx, codeToAtom(c) PASS_LD)

COMMON(int)	allocList(size_t maxcells, list_ctx *ctx);
COMMON(int)	unifyList(term_t term, list_ctx *ctx);
COMMON(int)	unifyDiffList(term_t head, term_t tail, list_ctx *ctx);

#endif /*PL_PRIVITF_H_INCLUDED*/
