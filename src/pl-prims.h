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

int		unify_ptrs(Word t1, Word t2, int flags ARG_LD);
void		unify_vp(Word vp, Word val ARG_LD);
bool		can_unify(Word t1, Word t2, term_t ex);
int		compareStandard(Word t1, Word t2, int eq ARG_LD);
int		compareAtoms(atom_t a1, atom_t a2);
intptr_t	skip_list(Word l, Word *tailp ARG_LD);
intptr_t	lengthList(term_t list, int errors);
int		is_acyclic(Word p ARG_LD);
intptr_t	numberVars(term_t t, nv_options *opts, intptr_t n ARG_LD);
int		duplicate_term(term_t in, term_t copy ARG_LD);
word		stringToList(char *s);
foreign_t	pl_sub_atom(term_t atom,
			    term_t before, term_t len, term_t after,
			    term_t sub, control_t h);
word		pl_sub_string(term_t str,
			      term_t offset, term_t length, term_t after,
			      term_t sub, control_t h);
word		pl_repeat(control_t h);
word		pl_fail(void);
word		pl_true(void);
word		pl_halt(term_t code);
int		pl_statistics_ld(term_t k, term_t value,
				 PL_local_data_t *ld ARG_LD);
int		set_pl_option(const char *name, const char *value);
word		pl_novice(term_t old, term_t new);
Word		ground__LD(Word p ARG_LD);
int		PL_factorize_term(term_t term,
				  term_t template, term_t factors);
int		PL_var_occurs_in(term_t var, term_t value);
void		raiseInferenceLimitException(void);


#endif /*_PL_PRIMS_H*/