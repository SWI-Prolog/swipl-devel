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
#define	first_clause_search(av, fr, def, next) \
	LDFUNC(first_clause_search, av, fr, def, next)
#define	next_clause_search(chp, argv, fr, def) \
	LDFUNC(next_clause_search, chp, argv, fr, def)
#define getIndexOfTerm(t)		LDFUNC(getIndexOfTerm, t)
#define ci_set_flag(value, key)		LDFUNC(ci_set_flag, value, key)
#define ci_get_flag(term, key)		LDFUNC(ci_get_flag, term, key)
#define update_primary_index(def)	LDFUNC(update_primary_index, def)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

word		getIndexOfTerm(term_t t);
HOT ClauseRef	first_clause_search(Word argv, LocalFrame fr, Definition def,
				    ClauseChoice next);
HOT ClauseRef	next_clause_search(const ClauseChoice chp, const Word argv,
				   const LocalFrame fr, const Definition def);
int		addClauseToIndexes(Definition def, Clause cl,
				   ClauseRef where);
void		delClauseFromIndex(Definition def, Clause cl);
void		cleanClauseIndexes(Definition def, ClauseList cl,
				   DirtyDefInfo ddi,
				   gen_t start, Buffer tr_starts);
void		clearTriedIndexes(Definition def);
void		reconsiderIndexes(Definition def);
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

		 /*******************************
		 *	 CLAUSE SELECTION	*
		 *******************************/

#define O_INDEX_STATIC	   1

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  - MAX_LOOKAHEAD
    Maximum number of clauses we look ahead on indexed clauses for an
    alternative clause. If the choice is committed this is lost effort,
    if it reaches the end of the clause list without finding one the
    call is deterministic.
  - MIN_CLAUSES_FOR_INDEX
    Create an index if there are more than this number of clauses
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_LOOKAHEAD         (GD->clause_index.max_lookahead)
#define MIN_CLAUSES_FOR_INDEX (GD->clause_index.min_clauses)

/* Same as !cref->d.key || cref->d.key == k */
#define cref_matches(cref, k) (((cref)->d.key == 0) | ((cref)->d.key == (k)))

/* True if we do not need to check the generation */
#define is_clean_definition(def) \
	isoff(def, P_DYNAMIC|P_DIRTYREG|P_RELOADING)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
matching_clause() returns the first clause at or after `start` whose
key matches `key`, and sets `nextp` to a later alternative within
MAX_LOOKAHEAD (or NULL).  Returns NULL if there is no match.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline ClauseRef
matching_clause(ClauseRef start, word key, ClauseRef *nextp)
{ ClauseRef cref;

  for(cref = start; cref; cref = cref->next)
  { if ( cref_matches(cref, key) )
    { ClauseRef next;
      int maxsearch = MAX_LOOKAHEAD;

      for(next = cref->next; next; next = next->next)
      { if ( cref_matches(next, key) || --maxsearch == 0 )
	  break;
      }
      *nextp = next;
      return cref;
    }
  }

  *nextp = NULL;
  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
static_clause_key() computes the primary-index key for the common cases
of a clean static predicate.  Fails when first_clause_search() must be
used.  This covers the same cases as first_clause_guarded() for:

  - A predicate with few clauses that is called with an atom, small
    integer or compound as primary index argument.
  - A predicate that cannot be indexed.
  - A call with unbound primary index argument if the candidate
    indexes are fixed and there is no hash index.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline bool
static_clause_key(ClauseList clist, size_t arity, Word argv, word *pkey)
{ if ( clist->unindexed || arity == 0 )
  { *pkey = 0;
    return true;
  } else
  { word w = argv[clist->primary_index];

    while ( isRef(w) )
      w = *unRef(w);

    if ( tag(w) == TAG_COMPOUND )
      *pkey = *valPtr(w);		/* functor_t */
    else if ( tag(w) == TAG_ATOM || isTaggedInt(w) )
      *pkey = w;
    else if ( canBind(w) &&
	      clist->fixed_indexes && !clist->clause_indexes )
    { *pkey = 0;
      return true;
    } else
      return false;

    return clist->number_of_clauses <= (unsigned int)MIN_CLAUSES_FOR_INDEX;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
first_clause_static() / next_clause_static() handle the common cases for
a clean static predicate without building an index context.  On success
they store the result in `crefp` (which may be NULL) and return true.
They return false when first_clause_search() / next_clause_search() must
be used.

(*) The wrappers pass a local clause_choice to first_clause_search() /
next_clause_search().  That way the address of the caller's choice is
not passed to a function and the compiler can keep a local choice in
registers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define	first_clause_static(av, def, chp, crefp) \
	LDFUNC(first_clause_static, av, def, chp, crefp)
static inline bool
first_clause_static(DECL_LD Word argv, Definition def,
		    ClauseChoice chp, ClauseRef *crefp)
{
#if O_INDEX_STATIC
  if ( is_clean_definition(def) )
  { ClauseList clist = &def->impl.clauses;
    ClauseRef cref;
    ClauseRef next = NULL;
    word pkey;

    if ( !static_clause_key(clist, def->functor->arity, argv, &pkey) )
      return false;

    acquire_def(def);
    if ( pkey )
      cref = matching_clause(clist->first_clause, pkey, &next);
    else if ( (cref = clist->first_clause) )
      next = cref->next;
    release_def(def);

    /* Duplicate primary key: use the general algorithm. */
    if ( cref && pkey && next &&
	 next->d.key == pkey && cref->d.key == pkey )
      return false;

    if ( cref )
    { chp->key  = pkey;
      chp->cref = next;
    }
    *crefp = cref;
    return true;
  }
#endif
  return false;
}


#define	next_clause_static(chp, def, crefp) \
	LDFUNC(next_clause_static, chp, def, crefp)
static inline bool
next_clause_static(DECL_LD const ClauseChoice chp, Definition def,
		   ClauseRef *crefp)
{
#if O_INDEX_STATIC
  if ( is_clean_definition(def) )
  { MEMORY_ACQUIRE();
    acquire_def(def);
    if ( !chp->key )			/* not indexed */
    { if ( (*crefp = chp->cref) )
	chp->cref = (*crefp)->next;
    } else
    { ClauseRef next;

      *crefp = matching_clause(chp->cref, chp->key, &next);
      chp->cref = next;
    }
    release_def(def);
    return true;
  }
#endif
  return false;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
firstClause() and nextClause() try the static fast path and fall back
to first_clause_search() / next_clause_search() in pl-index.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if USE_LD_MACROS
#define	firstClause(av, fr, def, next) \
	LDFUNC(firstClause, av, fr, def, next)
#define	nextClause(chp, argv, fr, def) \
	LDFUNC(nextClause, chp, argv, fr, def)
#endif /*USE_LD_MACROS*/

static inline ClauseRef
firstClause(DECL_LD Word argv, LocalFrame fr, Definition def,
	    ClauseChoice chp)
{ ClauseRef cref;

  if ( first_clause_static(argv, def, chp, &cref) )
    return cref;

  { struct clause_choice gchp = {0};	/* (*) */
    cref = LDFUNC_NAME(first_clause_search)(LD, argv, fr, def, &gchp);
    *chp = gchp;
    return cref;
  }
}

static inline ClauseRef
nextClause(DECL_LD const ClauseChoice chp, const Word argv,
	   const LocalFrame fr, const Definition def)
{ ClauseRef cref;

  (void)argv;				/* we want to use these later */

  if ( next_clause_static(chp, def, &cref) )
    return cref;

  { struct clause_choice gchp = *chp;	/* (*) */
    cref = LDFUNC_NAME(next_clause_search)(LD, &gchp, argv, fr, def);
    *chp = gchp;
    return cref;
  }
}

#endif /*_PL_INDEX_H*/
