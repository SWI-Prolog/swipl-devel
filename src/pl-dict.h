/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2021, VU University Amsterdam
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

#ifndef PL_DICT_H_INCLUDED
#define PL_DICT_H_INCLUDED

#define DICT_SORTED	0x1		/* Sort dict entries */

#if USE_LD_MACROS
#define	pl_for_dict(dict, func, closure, flags)	LDFUNC(pl_for_dict, dict, func, closure, flags)
#define	dict_order(dict, dupl)			LDFUNC(dict_order, dict, dupl)
#define	dict_order_term_refs(av, indexes, cnt)	LDFUNC(dict_order_term_refs, av, indexes, cnt)
#define	dict_lookup_ptr(dict, name)		LDFUNC(dict_lookup_ptr, dict, name)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int	PL_is_dict(term_t t);
int	pl_for_dict(term_t dict,
		   int LDFUNCP (*func)(term_t key,
			       term_t value,
			       int last,
			       void *closure),
		   void *closure,
		   int flags);

functor_t dict_functor(int pairs);
int	  dict_order(Word dict, Word dupl);
int	  dict_order_term_refs(term_t *av, int *indexes, int cnt);
Word	  dict_lookup_ptr(word dict, word name);
int	  resortDictsInClause(Clause clause);
void	  resortDictsInTerm(term_t t);

#undef LDFUNC_DECLARATIONS

#define PL_for_dict(dict, funcname, closure, flags) pl_for_dict(dict, LDFUNC_REF(funcname), closure, flags)

#define termIsDict(w) LDFUNC(termIsDict, w)
static inline int
termIsDict(DECL_LD word w)
{ Functor f = valueTerm(w);
  FunctorDef fd = valueFunctor(f->definition);

  return ( fd->name == ATOM_dict && fd->arity%2 == 1 );
}

static inline int
is_dict_key(word w)
{ return isAtom(w) || isTaggedInt(w);
}

#endif /*PL_DICT_H_INCLUDED*/
