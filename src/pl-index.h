/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
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

#ifndef _PL_INDEX_H
#define _PL_INDEX_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	firstClause(av, fr, def, next)	LDFUNC(firstClause, av, fr, def, next)
#define	nextClause(chp, argv, fr, def)	LDFUNC(nextClause, chp, argv, fr, def)
#define getIndexOfTerm(t)		LDFUNC(getIndexOfTerm, t)
#define ci_set_flag(value, key)		LDFUNC(ci_set_flag, value, key)
#define ci_get_flag(term, key)		LDFUNC(ci_get_flag, term, key)
#define update_primary_index(def)	LDFUNC(update_primary_index, def)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

word		getIndexOfTerm(term_t t);
ClauseRef	firstClause(Word argv, LocalFrame fr, Definition def,
			    ClauseChoice next);
ClauseRef	nextClause(const ClauseChoice chp, const Word argv,
			   const LocalFrame fr, const Definition def);
int		addClauseToIndexes(Definition def, Clause cl,
				   ClauseRef where);
void		delClauseFromIndex(Definition def, Clause cl);
void		cleanClauseIndexes(Definition def, ClauseList cl,
				   DirtyDefInfo ddi,
				   gen_t start, Buffer tr_starts);
void		clearTriedIndexes(Definition def);
void		unallocClauseIndexTable(ClauseIndex ci);
void		deleteActiveClauseFromIndexes(Definition def, Clause cl);
bool		unify_index_pattern(Procedure proc, term_t value);
void		deleteIndexes(Definition def, ClauseList cl, bool isnew);
void		deleteIndexesDefinition(Definition def);
int		checkClauseIndexSizes(Definition def, int nindexable);
void		checkClauseIndexes(Definition def);
void		listIndexGenerations(Definition def, gen_t gen);
size_t		sizeofClauseIndexes(Definition def);
void		initClauseIndexing(void);
bool		ci_is_flag(atom_t key);
bool		ci_set_flag(term_t value, atom_t key);
bool		ci_get_flag(term_t t, atom_t key);
void		update_primary_index(Definition def);
word		index_of_word(word w);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_INDEX_H*/
