/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
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

#include "pl-incl.h"

#ifndef _PL_PRIMS_H
#define _PL_PRIMS_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	unify_ptrs(t1, t2, flags)	LDFUNC(unify_ptrs, t1, t2, flags)
#define	unify_vp(vp, val)		LDFUNC(unify_vp, vp, val)
#define	compareStandard(t1, t2, eq)	LDFUNC(compareStandard, t1, t2, eq)
#define	skip_list(l, tailp)		LDFUNC(skip_list, l, tailp)
#define	is_acyclic(p)			LDFUNC(is_acyclic, p)
#define	numberVars(t, opts, n)		LDFUNC(numberVars, t, opts, n)
#define	duplicate_term(in, copy)	LDFUNC(duplicate_term, in, copy)
#define	pl_statistics_ld(k, value, ld)	LDFUNC(pl_statistics_ld, k, value, ld)
#define	ground(p)			LDFUNC(ground, p)
#define	PL_same_term(t1, t2)		LDFUNC(PL_same_term, t1, t2)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int		unify_ptrs(Word t1, Word t2, int flags);
void		unify_vp(Word vp, Word val);
bool		can_unify(Word t1, Word t2, term_t ex);
int		compareStandard(Word t1, Word t2, int eq);
int		compareAtoms(atom_t a1, atom_t a2);
intptr_t	skip_list(Word l, Word *tailp);
intptr_t	lengthList(term_t list, int errors);
int		is_acyclic(Word p);
intptr_t	numberVars(term_t t, nv_options *opts, intptr_t n);
int		duplicate_term(term_t in, term_t copy);
word		stringToList(char *s);
foreign_t	pl_sub_atom(term_t atom,
			    term_t before, term_t len, term_t after,
			    term_t sub, control_t h);
word		pl_repeat(control_t h);
word		pl_fail(void);
word		pl_true(void);
word		pl_halt(term_t code);
int		pl_statistics_ld(term_t k, term_t value,
				 PL_local_data_t *ld);
int		set_pl_option(const char *name, const char *value);
word		pl_novice(term_t old, term_t new);
Word		ground(Word p);
int		PL_factorize_term(term_t term,
				  term_t template, term_t factors);
int		PL_var_occurs_in(term_t var, term_t value);
void		raiseInferenceLimitException(void);
int		PL_same_term(term_t t1, term_t t2);

#undef LDFUNC_DECLARATIONS


#endif /*_PL_PRIMS_H*/
