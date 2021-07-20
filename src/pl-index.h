/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
                              VU University Amsterdam
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
#define	firstClause(argv, fr, def, next)	LDFUNC(firstClause, argv, fr, def, next)
#define	nextClause(chp, argv, fr, def)		LDFUNC(nextClause, chp, argv, fr, def)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

word		getIndexOfTerm(term_t t);
ClauseRef	firstClause(Word argv, LocalFrame fr, Definition def,
			    ClauseChoice next);
ClauseRef	nextClause(ClauseChoice chp, Word argv, LocalFrame fr,
			   Definition def);
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
void		deleteIndexes(ClauseList cl, int isnew);
void		deleteIndexesDefinition(Definition def);
int		checkClauseIndexSizes(Definition def, int nindexable);
void		checkClauseIndexes(Definition def);
void		listIndexGenerations(Definition def, gen_t gen);
size_t		sizeofClauseIndexes(Definition def);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_INDEX_H*/