/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

#ifndef PL_PRIVITF_H_INCLUDED
#define PL_PRIVITF_H_INCLUDED

COMMON(int) 	PL_get_char(term_t c, int *p, int eof);
COMMON(int) 	PL_unify_char(term_t chr, int c, int mode);
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
addSmallIntList__LD(list_ctx *ctx, int value ARG_LD)
{ ctx->gstore[0] = consPtr(&ctx->gstore[1], TAG_COMPOUND|STG_GLOBAL);
  ctx->gstore[1] = FUNCTOR_dot2;
  ctx->gstore[2] = consInt(value);
  ctx->gstore += 3;
}

#define addSmallIntList(ctx, i) addSmallIntList__LD(ctx, (i) PASS_LD)

COMMON(int)	allocList(size_t maxcells, list_ctx *ctx);
COMMON(int)	unifyList(term_t term, list_ctx *ctx);
COMMON(int)	unifyDiffList(term_t head, term_t tail, list_ctx *ctx);

#endif /*PL_PRIVITF_H_INCLUDED*/
