/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2015, VU University Amsterdam
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

#ifndef PL_DICT_H_INCLUDED
#define PL_DICT_H_INCLUDED

#define DICT_SORTED	0x1		/* Sort dict entries */

COMMON(int)	PL_is_dict(term_t t);
COMMON(int)	PL_for_dict(term_t dict,
			   int (*func)(term_t key,
				       term_t value,
				       int last,
				       void *closure),
			   void *closure,
			   int flags);

COMMON(functor_t) dict_functor(int pairs);
COMMON(int)	  dict_order_term_refs(term_t *av, int *indexes, int cnt ARG_LD);
COMMON(Word)	  dict_lookup_ptr(word dict, word name ARG_LD);
COMMON(int)	  resortDictsInClause(Clause clause);
COMMON(void)	  resortDictsInTerm(term_t t);

#define termIsDict(w) termIsDict__LD(w PASS_LD)
static inline int
termIsDict__LD(word w ARG_LD)
{ Functor f = valueTerm(w);
  FunctorDef fd = valueFunctor(f->definition);

  return ( fd->name == ATOM_dict && fd->arity%2 == 1 );
}

#endif /*PL_DICT_H_INCLUDED*/
