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

#include "pl-index.h"
#include "pl-comp.h"
#include "pl-rsort.h"
#include "pl-util.h"
#include "pl-proc.h"
#include "os/pl-prologflag.h"
#include "pl-fli.h"
#include "pl-wam.h"
#include <math.h>

#undef LD
#define LD LOCAL_LD

		 /*******************************
		 *           FEATURES           *
		 *******************************/

#define O_INDEX_QUICK_TEST 1
#define O_INDEX_STATIC	   1

		 /*******************************
		 *	     PARAMETERS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  - MAX_LOOKAHEAD
    Maximum number of clauses we look ahead on indexed clauses for an
    alternative clause. If the choice is committed this is lost effort,
    if it reaches the end of the clause list without finding one the
    call is deterministic.
  - MIN_SPEEDUP
    Do not create an index if the speedup is less
  - MAX_VAR_FRAC
    Do not create an index if the fraction of clauses with a variable
    in the target position exceeds this threshold.
  - MIN_CLAUSES_FOR_INDEX
    Create an index if there are more than this number of clauses
  - MIN_SPEEDUP_RATIO
    Need at least this ratio of #clauses/speedup for creating an index
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MIN_SPEEDUP           (GD->clause_index.min_speedup)
#define MAX_VAR_FRAC          (GD->clause_index.max_var_fraction)
#define MIN_SPEEDUP_RATIO     (GD->clause_index.min_speedup_ratio)
#define MAX_LOOKAHEAD         (GD->clause_index.max_lookahead)
#define MIN_CLAUSES_FOR_INDEX (GD->clause_index.min_clauses)


		 /*******************************
		 *	       TYPES		*
		 *******************************/

static const struct clause_index dead_index = {};
#define DEAD_INDEX   ((ClauseIndex)(&dead_index))
#define ISDEADCI(ci) ((ci) == DEAD_INDEX)

typedef struct hash_hints
{ float		speedup;		/* Expected speedup */
  unsigned	list : 1;		/* Use a list per key */
  unsigned	ln_buckets : 5;		/* Lg2 of #buckets to use */
  iarg_t	args[MAX_MULTI_INDEX];	/* Hash these arguments */
} hash_hints;

typedef struct index_context
{ gen_t		generation;		/* Current generation */
  Definition	predicate;		/* Current predicate */
  ClauseChoice	chp;			/* Clause choice point */
  int		depth;			/* current depth (0..) */
  iarg_t	position[MAXINDEXDEPTH+1]; /* Keep track of argument position */
} index_context, *IndexContext;

#if USE_LD_MACROS
#define	bestHash(av, ac, clist, better_than, hints, ctx) \
	LDFUNC(bestHash, av, ac, clist, better_than, hints, ctx)
#define	setClauseChoice(cref, ctx) \
	LDFUNC(setClauseChoice, cref, ctx)
#define	first_clause_guarded(argv, argc, clist, ctx) \
	LDFUNC(first_clause_guarded, argv, argc, clist, ctx)
#define find_multi_argument_hash(ac, clist, inst, ninst, bthan, hints, ctx) \
	LDFUNC(find_multi_argument_hash, ac, clist, inst, ninst, \
	       bthan, hints, ctx)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static bool	bestHash(Word av, size_t ac, ClauseList clist,
			 ClauseIndex better_than, hash_hints *hints,
			 IndexContext ctx);
static ClauseIndex hashDefinition(ClauseList clist, hash_hints *h,
				  IndexContext ctx);
static void	replaceIndex(Definition def, ClauseList cl,
			     ClauseIndex *cip, ClauseIndex ci);
static void	deleteIndexP(Definition def, ClauseList cl, ClauseIndex *cip);
static void	deleteIndex(Definition def, ClauseList cl, ClauseIndex ci);
static void	insertIndex(Definition def, ClauseList clist, ClauseIndex ci);
static void	setClauseChoice(ClauseRef cref, const IndexContext ctx);
static bool	addClauseToIndex(ClauseIndex ci, Clause cl, ClauseRef where);
static void	addClauseToListIndexes(Definition def, ClauseList cl,
				       Clause clause, ClauseRef where);
static void	insertIntoSparseList(ClauseRef cref,
				     ClauseRef *headp, ClauseRef *tailp,
				     ClauseRef where);
static ClauseRef first_clause_guarded(const Word argv, size_t argc,
				      ClauseList clist,
				      const IndexContext ctx);
static Code	skipToTerm(Clause clause, const iarg_t *position,
			   int *in_hvoid);
static void	unalloc_index_array(void *p);
static void	wait_for_index(ClauseIndex ci, ClauseList clist,
			       IndexContext ctx);
static void	completed_index(ClauseIndex ci);
static bool	realize_clause_index(ClauseIndex ci);
static ClauseIndex fill_clause_index(ClauseIndex ci, ClauseList clist,
				     IndexContext ctx);
static bool	find_multi_argument_hash(iarg_t ac, ClauseList clist,
					 iarg_t *inst, int ninst,
					 ClauseIndex better_than,
					 hash_hints *hints, IndexContext ctx);
static bool	set_candidate_indexes(Definition def, ClauseList clist,
				      int max, bool lock);
#undef LDFUNC_DECLARATIONS

/* We are reloading static code */
#define STATIC_RELOADING(pred) (LD->reload.generation && \
				isoff(pred, P_DYNAMIC))

/* Same as !cref->d.key || cref->d.key == k */
#define cref_matches(cref, k) (((cref)->d.key == 0) | ((cref)->d.key == (k)))

/* True if we do not need to check the generation */
#define is_clean_predicate(def) \
	isoff(ctx->predicate, P_DYNAMIC|P_DIRTYREG|P_RELOADING)

/* Map  a word  to  a hash  bucket offset.   This  now uses  Fibonacci
   hashing.  See https://en.wikipedia.org/wiki/Hash_function

   Possibly we should store the shift with the ClauseIndex?
 */

static inline unsigned int
hashIndex(word key, unsigned int buckets)
{ const int  key_bits = sizeof(key)*8;
  const word fib64 = 0x9E3779B97F4A7C15LL;
  const int  shift = key_bits-MSB(buckets);

  key ^= key >> shift;
  return (fib64*key) >> shift;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compute the index in the hash-array from   a machine word and the number
of buckets. This used to be simple, but now that our tag bits are on the
left side, simply masking will put most things on the same hash-entry as
it is very common for all clauses of   a predicate to have the same type
of object. Hence, we now use exclusive or of the real value part and the
tag-bits.

NOTE: this function must be kept  consistent with argKey() in pl-comp.c!
NOTE: This function returns 0 on non-indexable   fields, which is why we
guarantee that the value is non-0 for indexable values.
NOTE: Indirects should not collide  with   functor_t  to  allow for deep
indexing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline int
canIndex(word w)
{ for(;;)
  { switch(tag(w))
    { case TAG_VAR:
      case TAG_ATTVAR:
	return false;
      case TAG_REFERENCE:
	w = *unRef(w);
	continue;
      default:
	return true;
    }
  }
}

static inline word
indexOfWord(word w)
{ for(;;)
  { switch(tag(w))
    { case TAG_VAR:
      case TAG_ATTVAR:
	return 0;
      case TAG_ATOM:
	break;				/* atom_t */
      case TAG_INTEGER:
	if ( likely(storage(w) == STG_INLINE) )
	  break;
#if O_BIGNUM
	return bignum_index(addressIndirect(w));
#else
	assert(0);
#endif
      case TAG_STRING:		/* TODO: Only consider part of a long string */
      case TAG_FLOAT:
      { Word p = addressIndirect(w);
	size_t n = wsizeofInd(*p);

	return murmur_key(p+1, n*sizeof(*p));
      }
      case TAG_COMPOUND:
	w = *valPtr(w);			/* functor_t */
	break;
      case TAG_REFERENCE:
	w = *unRef(w);
	continue;
    }

    return w;
  }
}

word				/* non-inlined version used in tabling */
index_of_word(word w)
{ return indexOfWord(w);
}

word
getIndexOfTerm(DECL_LD term_t t)
{ word w = *valTermRef(t);

  return indexOfWord(w);
}


#define	next_clause_unindexed(ctx) \
	LDFUNC(next_clause_unindexed, ctx)

static ClauseRef
next_clause_unindexed(DECL_LD const IndexContext ctx)
{
#if O_INDEX_STATIC
  if ( is_clean_predicate(ctx->predicate) )
  { ClauseRef cref = ctx->chp->cref;
    if ( cref )
    { ctx->chp->key = 0;
      ctx->chp->cref = cref->next;
    }
    return cref;
  } else
#endif
  { for(ClauseRef cref=ctx->chp->cref; cref; cref = cref->next)
    { if ( visibleClauseCNT(cref->value.clause, ctx->generation) )
      { ctx->chp->key = 0;
	setClauseChoice(cref->next, ctx);
	return cref;
      }
    }
    return NULL;
  }
}

#define next_clause_primary_index(ctx) \
	LDFUNC(next_clause_primary_index, ctx)

static inline ClauseRef
next_clause_primary_index(DECL_LD const IndexContext ctx)
{ word key = ctx->chp->key;

#if O_INDEX_STATIC
  if ( is_clean_predicate(ctx->predicate) )
  { for(ClauseRef cref = ctx->chp->cref; cref; cref = cref->next)
    { if ( cref_matches(cref, key) )
      { ClauseRef result = cref;
	int maxsearch = MAX_LOOKAHEAD;

	for( cref = cref->next; cref; cref = cref->next )
	{ if ( cref_matches(cref, key) || --maxsearch == 0 )
	  { ctx->chp->cref = cref;
	    return result;
	  }
	}
	ctx->chp->cref = NULL;

	return result;
      }
    }
  } else
#endif /*O_INDEX_STATIC*/
  { for(ClauseRef cref = ctx->chp->cref; cref; cref = cref->next)
    { if ( cref_matches(cref, key) &&
	   visibleClauseCNT(cref->value.clause, ctx->generation))
      { ClauseRef result = cref;
	int maxsearch = MAX_LOOKAHEAD;

	for( cref = cref->next; cref; cref = cref->next )
	{ if ( cref_matches(cref, key) )
	  { if ( visibleClauseCNT(cref->value.clause, ctx->generation) )
	    { ctx->chp->cref = cref;
	      return result;
	    }
	  }
	  if ( --maxsearch == 0 )
	  { setClauseChoice(cref, ctx);
	    return result;
	  }
	}
	ctx->chp->cref = NULL;

	return result;
      }
    }
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
nextClauseFromBucket()

If we search for a functor there  are   two  options: we have a list for
this functor, in which case we can use   this or we don't. In the latter
case we must still perform the traditional   search as clauses without a
key may match.

(*) We gave up indexed search due to  exceeding depth. We should set key
to  0  to  avoid  using  the    first   argument  key  in  nextClause().
Alternatively we'd have to  reset  the   argv  to  the  appropriate deep
argument inside nextClause, but  unfortunately  we   do  not  know which
argument we are processing.

TBD: Keep a flag telling whether there are non-indexable clauses.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define nextClauseFromList(ci, argv, ctx) \
	LDFUNC(nextClauseFromList, ci, argv, ctx)

static ClauseRef
nextClauseFromList(DECL_LD ClauseIndex ci, Word argv, IndexContext ctx)
{ word key = ctx->chp->key;

  DEBUG(MSG_INDEX_FIND, Sdprintf("Searching for %s\n", keyName(key)));
  DEBUG(0, assert(ci->is_list));
  DEBUG(0, assert(ci->args[1] == 0));

  for(ClauseRef cref = ctx->chp->cref; cref; cref = cref->next)
  { if ( cref->d.key == key )
    { ClauseList cl = &cref->value.clauses;

      DEBUG(MSG_INDEX_DEEP, Sdprintf("Deep index for %s\n", keyName(key)));

      if ( isFunctor(cref->d.key) && ctx->depth < MAXINDEXDEPTH )
      { iarg_t an = ci->args[0]-1;
	Word a = argv+an;
	Functor at;
	size_t argc;

	deRef(a);
	DEBUG(0, assert(isTerm(*a)));
	at = valueTerm(*a);
	argv = at->arguments;
	argc = arityFunctor(at->definition);

	ctx->position[ctx->depth++] = an;
	ctx->position[ctx->depth]   = END_INDEX_POS;

	DEBUG(MSG_INDEX_DEEP,
	      Sdprintf("Recursive index for %s at level %d\n",
		       keyName(cref->d.key), ctx->depth));
	return first_clause_guarded(argv, argc, cl, ctx);
      }

      ctx->chp->key = 0;		/* See (*) */
      ctx->chp->cref = cl->first_clause;
      return next_clause_unindexed(ctx);
    }
  }

  if ( key )
  { ctx->chp->key = 0;
    DEBUG(MSG_INDEX_FIND, Sdprintf("Not found; trying variables\n"));
    for(ClauseRef cref = ctx->chp->cref; cref; cref = cref->next)
    { if ( !cref->d.key )
      { ClauseList cl = &cref->value.clauses;
	ctx->chp->cref = cl->first_clause;
	return next_clause_unindexed(ctx);
      }
    }
  } else
  { DEBUG(MSG_INDEX_FIND, Sdprintf("Not found\n"));
  }

  return NULL;
}

#define nextClauseFromBucket(ci, argv, ctx) \
	LDFUNC(nextClauseFromBucket, ci, argv, ctx)

static ClauseRef
nextClauseFromBucket(DECL_LD ClauseIndex ci, Word argv, IndexContext ctx)
{ if ( unlikely(ci->is_list) )
    return nextClauseFromList(ci, argv, ctx);

  return next_clause_primary_index(ctx);
}

/* Make sure the ClauseChoice contains a pointer to a clause that
   is still visible in generation.  This garantees that the clause will
   not be destroyed. Note that we do not have to perform the full
   visibility test, just avoid we end up at a clause reference that
   is free for CGC.
*/

static inline void
setClauseChoice(DECL_LD ClauseRef cref, const IndexContext ctx)
{
#if O_INDEX_STATIC
  if ( !is_clean_predicate(ctx->predicate) )
#endif
  { while ( cref &&
	    !visibleClauseCNT(cref->value.clause, ctx->generation) )
      cref = cref->next;
  }

  ctx->chp->cref = cref;
}


static inline word
join_multi_arg_keys(const word *key, unsigned int len)
{ word k = MurmurHashAligned2(key, sizeof(word)*len, MURMUR_SEED);
  return clean_index_key(k);
}

static inline word
indexKeyFromArgv(ClauseIndex ci, Word argv)
{ if ( likely(ci->args[1] == 0) )
  { return indexOfWord(argv[ci->args[0]-1]);
  } else
  { word key[MAX_MULTI_INDEX];
    unsigned int harg;

    for(harg=0; ci->args[harg]; harg++)
    { if ( !(key[harg] = indexOfWord(argv[ci->args[harg]-1])) )
	return 0;
    }

    return join_multi_arg_keys(key, harg);
  }
}

#ifdef O_INDEX_QUICK_TEST
/* Perform  a  quick  test  whether  a virtual  clause  index  can  be
 * activated.
 *
 * is_var() is optimized on the fact  that in most cases a variable is
 * a  one-step  reference  and  this  function  is  mostly  called  on
 * variables.
 */

static inline bool
is_var(Word p)
{ if ( likely(isRef(*p)) )
    p = unRef(*p);

  for(;;)
  { if ( canBind(*p) )
      return true;
    if ( unlikely(isRef(*p)) )
      p = unRef(*p);
    else
      return false;
  }
}

static inline bool
is_satifies_index(const ClauseIndex ci, const Word argv)
{ iarg_t a0 = ci->args[0];

  if ( a0 == 0 || is_var(&argv[a0-1]) )
    return false;		/* DEAD_INDEX and var first arg */

  for(int i=1; i<MAX_MULTI_INDEX && (a0=ci->args[i]); i++)
  { if ( is_var(&argv[a0-1]) )
      return false;
  }
  return true;
}
#else
#define is_satifies_index(ci,argv) true
#endif


#if defined(O_DEBUG) || defined(O_MAINTENANCE)
static char *
iargsName(const iarg_t args[MAX_MULTI_INDEX], char *buf)
{ static char sbuf[64];
  char *s;
  int i;

  if ( !buf )
    buf = sbuf;

  s = buf;
  *s++ = '[';
  for(i=0; args[i]; i++)
  { if ( i > 0 )
      *s++ = ',';
    Ssprintf(s, "%d", args[i]);
    s += strlen(s);
  }
  *s++ = ']';
  *s = EOS;

  return buf;
}
#endif

/* Given an index  with speedup for nclauses, should  we always accept
 * this or should we see whether there might be a better one?
 *
 * Reliance on  `MIN_CLAUSES_FOR_INDEX` seems  dubious as this  is not
 * about using the simple primary index or not.  It also seems dubious
 * to use `MIN_SPEEDUP_RATIO` rather than simply `MIN_SPEEDUP`.
 *
 * In  most cases  it seems  we should  explore all  options and  then
 * decide.
 */

static inline bool
consider_better_index(float speedup, unsigned int nclauses)
{ return ( nclauses > MIN_CLAUSES_FOR_INDEX &&
	   (float)nclauses/speedup > MIN_SPEEDUP_RATIO );
}

static ClauseIndex
existing_hash(ClauseIndex *cip, const Word argv, Word keyp)
{ for(; *cip; cip++)
  { ClauseIndex ci = *cip;
    word k;

    if ( ci->entries || is_satifies_index(ci, argv) )
    { if ( (k=indexKeyFromArgv(ci, argv)) )
      { *keyp = k;
	return ci;
      }
    }
  }

  return NULL;
}

/* Add a new index  to the clause list if it provides  a speedup of at
 * least `min_speedup`.  This is called if the best index is "poor" or
 * linear primary  clause index scanning  does not work  because there
 * are too many clauses or the argument is not instantiated.
 *
 * We  call  bestHash()  to  find  the  best  possible  index.   Next,
 * hashDefinition() first checks whether this index already exists and
 * creates it otherwise.
 *
 * @param better_than is an optional clause index.  When present
 * we are trying to improve on that index.
 * @return:
 *   - CI_RETRY
 *     Someone invalidated the index while we were building it or
 *     waiting for a thread to complete it.
 *   - NULL
 *     There is no better index possible
 *   - A ClauseIndex
 *     All went fine
 */

#define CI_RETRY ((ClauseIndex)1)

#define	createIndex(av, ac, clist, better_than, ctx) \
	LDFUNC(createIndex, av, ac, clist, better_than, ctx)

static ClauseIndex
createIndex(DECL_LD Word argv, size_t argc, const ClauseList clist,
	    ClauseIndex better_than, IndexContext ctx)
{ hash_hints hints;

  if ( bestHash(argv, argc, clist, better_than, &hints, ctx) )
  { ClauseIndex ci;

    if ( (ci=hashDefinition(clist, &hints, ctx)) )
    { while ( ci->incomplete )
	wait_for_index(ci, clist, ctx);
      if ( ci->invalid )
	return CI_RETRY;

      return ci;
    }

    return CI_RETRY;
  }

  return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
firstClause() finds the first applicable   clause  and leave information
for finding the next clause in chp.

TBD:
  - non-indexable predicates must use a different supervisor
  - Predicates needing reindexing should use a different supervisor
  - When to select best table?
  - When to ignore the best and try again?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ClauseRef
first_clause_guarded(DECL_LD const Word argv, size_t argc, ClauseList clist,
		     const IndexContext ctx)
{ ClauseRef cref;
  ClauseIndex *cip;
  ClauseChoice chp = ctx->chp;

  /* If `clist->unindexed`, no primary index is possible. */

  if ( clist->unindexed || argc == 0 )
  { chp->cref = clist->first_clause;
    return next_clause_unindexed(ctx);
  }

  /* Deal with possible hashes */
retry:
  if ( (cip=clist->clause_indexes) )
  { ClauseIndex best_index;

    best_index = existing_hash(cip, argv, &chp->key);

    if ( best_index )
    { if ( unlikely(!best_index->good) )
      { if ( !clist->fixed_indexes &&
	     !STATIC_RELOADING(ctx->predicate) &&
	     consider_better_index(best_index->speedup,
				   clist->number_of_clauses) )
	{ ClauseIndex ci;

	  DEBUG(MSG_JIT_POOR,
		Sdprintf("Poor index %s of %s (trying to find better)\n",
			 iargsName(best_index->args, NULL),
			 predicateName(ctx->predicate)));

	  if ( (ci=createIndex(argv, argc, clist, best_index, ctx)) )
	  { if ( unlikely(ci == CI_RETRY) )
	      goto retry;

	    chp->key = indexKeyFromArgv(ci, argv);
	    assert(chp->key);
	    best_index = ci;
	  }
	}

	if ( best_index->incomplete )
	{ wait_for_index(best_index, clist, ctx);
	  goto retry;
	}
      }

      unsigned int hi = hashIndex(chp->key, best_index->buckets);
      const ClauseBucket bkt = &best_index->entries[hi];
      if ( bkt->key && chp->key != bkt->key )
	return NULL;
      chp->cref = bkt->head;
      return nextClauseFromBucket(best_index, argv, ctx);
    }
  }

  iarg_t pindex = clist->primary_index;
  chp->key = indexOfWord(argv[pindex]);

  if ( clist->fixed_indexes )	/* set_candidate_indexes() has been run */
  { chp->cref = clist->first_clause;
    if ( chp->key )
      return next_clause_primary_index(ctx);
    else
      return next_clause_unindexed(ctx);
  }

  if ( unlikely(clist->number_of_clauses == 0) )
    return NULL;

  if ( isoff(ctx->predicate,
	     P_DYNAMIC|P_MULTIFILE|P_THREAD_LOCAL|P_FOREIGN) &&
       ctx->depth == 0 )
  { set_candidate_indexes(ctx->predicate, clist, 10, true);
    goto retry;
  }

  /* Try the primary index if  the corresponding argument is bound and
   * we have  less than  MIN_CLAUSES_FOR_INDEX clauses.  Accept  if we
   * have no clause or the next candidate has a different key.  If the
   * next candidate has the same key, deep indexing may help us, so we
   * will search for other indexes.
   */

  if ( chp->key &&
       ( clist->number_of_clauses <= MIN_CLAUSES_FOR_INDEX ||
	 STATIC_RELOADING(ctx->predicate)) )
  { chp->cref = clist->first_clause;
    cref = next_clause_primary_index(ctx);
    if ( !cref ||
	 !(chp->cref && chp->cref->d.key == chp->key &&
	   cref->d.key == chp->key) )
      return cref;
    /* else duplicate; see whether we can create a deep index */
    /* TBD: Avoid trying this every goal */
  } else
    cref = NULL;

  if ( !STATIC_RELOADING(ctx->predicate) )
  { ClauseIndex ci;

    if ( (ci=createIndex(argv, argc, clist, NULL, ctx)) )
    { if ( unlikely(ci == CI_RETRY) )
	goto retry;

      chp->key = indexKeyFromArgv(ci, argv);
      assert(chp->key);
      unsigned int hi = hashIndex(chp->key, ci->buckets);
      chp->cref = ci->entries[hi].head;
      return nextClauseFromBucket(ci, argv, ctx);
    }
  }

  if ( cref )			/* from next_clause_primary_index() call */
    return cref;

  chp->cref = clist->first_clause;
  if ( chp->key )
    return next_clause_primary_index(ctx);
  else
    return next_clause_unindexed(ctx);
}


ClauseRef
firstClause(DECL_LD Word argv, LocalFrame fr, Definition def,
	    ClauseChoice chp)
{ ClauseRef cref;
  index_context ctx =
    { .generation  = generationFrame(fr),
      .predicate   = def,
      .chp         = chp,
      .depth       = 0,
      .position[0] = END_INDEX_POS
    };

  if ( ison(def, P_DYNAMIC) )
    MEMORY_ACQUIRE();			/* sync with retract_clause() */
  acquire_def(def);
  cref = first_clause_guarded(argv,
			      def->functor->arity,
			      &def->impl.clauses,
			      &ctx);
#define CHK_STATIC_RELOADING() (LD->reload.generation && isoff(def, P_DYNAMIC))
  DEBUG(CHK_SECURE, assert(!cref || !chp->cref ||
			   visibleClause(chp->cref->value.clause,
					 generationFrame(fr)) ||
			   CHK_STATIC_RELOADING()));
  release_def(def);

  return cref;
}


ClauseRef
nextClause(DECL_LD const ClauseChoice chp, const Word argv,
	   const LocalFrame fr, const Definition def)
{ ClauseRef cref;

  (void)argv;				/* we want to use these later */

  MEMORY_ACQUIRE();
  acquire_def(def);
  index_context ctx;
  ctx.chp = chp;
  ctx.predicate = def;
  ctx.generation = generationFrame(fr);

  if ( !chp->key )			/* not indexed */
  { cref = next_clause_unindexed(&ctx);
  } else
  { cref = next_clause_primary_index(&ctx);
  }
  release_def(def);

  DEBUG(CHK_SECURE,
	assert(!cref || !chp->cref ||
	       visibleClause(chp->cref->value.clause, ctx.generation)));

  return cref;
}


		 /*******************************
		 *	   HASH SUPPORT		*
		 *******************************/

static int
cmp_iarg(const void *p1, const void *p2)
{ const iarg_t *u1 = p1;
  const iarg_t *u2 = p2;

  return *u1 < *u2 ? -1 : *u1 > *u2 ? 1 : 0;
}

static void
canonicalHap(iarg_t *hap)
{ int i;

  for(i=0; i<MAX_MULTI_INDEX; i++)
  { if ( !hap[i] )
    { int j;

      for(j=i; j<MAX_MULTI_INDEX; j++)
	hap[j] = 0;
      break;
    }
  }

  qsort(hap, i, sizeof(*hap), cmp_iarg);
}


static void
copytpos(iarg_t *to, const iarg_t *from)
{ iarg_t p;

  do
  { *to++ = p = *from++;
  } while ( p != END_INDEX_POS );
}


static bool
realize_clause_index(ClauseIndex ci)
{ size_t bytes = sizeof(struct clause_bucket) * ci->buckets;
  ClauseBucket buckets = allocHeapOrHalt(bytes);
  memset(buckets, 0, bytes);
  bool rc = COMPARE_AND_SWAP_PTR(&ci->entries, NULL, buckets);
  if ( rc )
    ATOMIC_INC(&GD->statistics.indexes.created);
  return rc;
}

static ClauseIndex
newClauseIndexTable(hash_hints *hints, bool realised, IndexContext ctx)
{ ClauseIndex ci = allocHeapOrHalt(sizeof(struct clause_index));

  memset(ci, 0, sizeof(*ci));
  static_assertion(sizeof(ci->args) == sizeof(hints->args));
  memcpy(ci->args, hints->args, sizeof(ci->args));
  ci->buckets	 = 2<<hints->ln_buckets;
  ci->is_list	 = hints->list;
  ci->incomplete = true;
  ci->speedup	 = hints->speedup;
  copytpos(ci->position, ctx->position);
  if ( realised )
    realize_clause_index(ci);

  return ci;
}


static void
freeClauseListRef(ClauseRef cref)
{ ClauseList cl = &cref->value.clauses;
  ClauseRef cr, next;

  deleteIndexes(NULL, cl, true);

  for(cr=cl->first_clause; cr; cr=next)
  { next = cr->next;
    lingerClauseRef(cr);
  }

  if ( cl->args )
  { assert(isFunctor(cref->d.key));

    freeHeap(cl->args, arityFunctor(cref->d.key)*sizeof(*cl->args));
  }
  freeHeap(cref, SIZEOF_CREF_LIST);
}


static void
vfree_clause_list_ref(void *cref)
{ freeClauseListRef(cref);
}


static void
lingerClauseListRef(Definition def, ClauseRef cref)
{ linger_always(&def->lingering, vfree_clause_list_ref, cref);
}


static void
unallocClauseIndexTableEntries(ClauseIndex ci)
{ ClauseBucket cb;
  int i;

  for(cb=ci->entries,i=ci->buckets; --i>=0; cb++)
  { ClauseRef cr, next;

    for(cr = cb->head; cr; cr = next)
    { next = cr->next;
      if ( ci->is_list )
	freeClauseListRef(cr);
      else
	lingerClauseRef(cr);
    }
  }

  freeHeap(ci->entries, ci->buckets * sizeof(struct clause_bucket));
}


void
unallocClauseIndexTable(ClauseIndex ci)
{ if ( ci->entries )
  { unallocClauseIndexTableEntries(ci);
    ATOMIC_INC(&GD->statistics.indexes.destroyed);
  }
  freeHeap(ci, sizeof(struct clause_index));
}


static ClauseRef
newClauseListRef(word key)
{ ClauseRef cref = allocHeapOrHalt(SIZEOF_CREF_LIST);

  memset(cref, 0, SIZEOF_CREF_LIST);
  cref->d.key = key;

  return cref;
}


static void
addToClauseList(ClauseRef cref, Clause clause, word key, ClauseRef where)
{ Definition def = clause->predicate;
  ClauseList cl = &cref->value.clauses;
  ClauseRef cr = newClauseRef(clause, key);

  if ( cl->first_clause )
  { if ( where == CL_END )
    { cl->last_clause->next = cr;
      cl->last_clause = cr;
    } else if ( where == CL_START )
    { cr->next = cl->first_clause;
      cl->first_clause = cr;
    } else
    { insertIntoSparseList(cr, &cl->first_clause, &cl->last_clause, where);
    }
    cl->number_of_clauses++;
  } else
  { cl->first_clause = cl->last_clause = cr;
    cl->number_of_clauses = 1;
  }

  addClauseToListIndexes(def, cl, clause, where);
}


static void
addClauseToListIndexes(Definition def, ClauseList cl, Clause clause,
		       ClauseRef where)
{ ClauseIndex *cip;

  if ( (cip=cl->clause_indexes) )
  { for(; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) || !ci->entries )
	continue;

      while ( ci->incomplete )
	wait_for_index(ci, cl, NULL);
      if ( ci->invalid )
	continue;

      if ( ci->size >= ci->resize_above ||
	   !addClauseToIndex(ci, clause, where) )
	deleteIndexP(def, cl, cip);
    }
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This linked list *headp *tailp is a sparse (filtered) list of the
clauses.  Insert cref into this list.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
insertIntoSparseList(ClauseRef cref,
		     ClauseRef *headp, ClauseRef *tailp,
		     ClauseRef where)
{ if ( !(*headp) )
  { *headp = *tailp = cref;
  } else if ( where == CL_END )
  { (*tailp)->next = cref;
    *tailp = cref;
  } else if ( where == CL_START )
  { cref->next = *headp;
    *headp = cref;
  } else
  { Clause clause = cref->value.clause;
    ClauseRef pred_cref = clause->predicate->impl.clauses.first_clause;
    ClauseRef ci_cref = *headp;
    ClauseRef ci_prev = NULL;

    for(; pred_cref; pred_cref=pred_cref->next)
    { if ( pred_cref == where || ci_prev == *tailp )
      { if ( ci_cref == *headp )
	{ cref->next = *headp;
	  *headp = cref;
	} else if ( ci_prev == *tailp )
	{ (*tailp)->next = cref;
	  *tailp = cref;
	} else
	{ cref->next = ci_cref;
	  ci_prev->next = cref;
	}

	return;
      }

      if ( pred_cref->value.clause == ci_cref->value.clause )
      { ci_prev = ci_cref;
	ci_cref = ci_cref->next;
      }
    }
  }
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add a clause to a bucket.  There are some special cases:

  - If the key denotes a functor, create or extend a clause-list for
    this functor.
  - If the key is non-indexable, add the clause both to the bucket
    chain and to all functor clause-lists (*). The latter also implies
    that if we create a functor clause-list we must add all
    non-indexable clauses to it (**).

Return how many indexable entries have been added to the bucket.

The non-indexable clauses are added to an   entry with key=0. This entry
must be used if none of the indexes matches.

Where is one of
  - CL_START (asserta)
  - CL_END   (assertz)
  - The clause reference before which the clause must be inserted.
    This is used by reconsult.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
addClauseBucket(ClauseBucket ch, Clause cl,
		word key, word arg1key, ClauseRef where, int is_list)
{ ClauseRef cr;

  if ( is_list )
  { ClauseRef cref;
    ClauseList vars = NULL;

    if ( key )
    { for(cref=ch->head; cref; cref=cref->next)
      { if ( cref->d.key == key )
	{ addToClauseList(cref, cl, arg1key, where);
	  DEBUG(MSG_INDEX_UPDATE,
		Sdprintf("Adding to existing %s\n", keyName(key)));
	  return 0;
	} else if ( !cref->d.key )
	{ vars = &cref->value.clauses;
	}
      }
    } else
    { for(cref=ch->head; cref; cref=cref->next)
      { if ( !cref->d.key )
	  vars = &cref->value.clauses;
	addToClauseList(cref, cl, arg1key, where);
      }
      if ( vars )
	return 0;
    }

    DEBUG(MSG_INDEX_UPDATE, Sdprintf("Adding new %s\n", keyName(key)));
    cr = newClauseListRef(key);
    if ( vars )				/* (**) */
    { for(cref=vars->first_clause; cref; cref=cref->next)
      { addToClauseList(cr, cref->value.clause, arg1key, CL_END);
	if ( ison(cref->value.clause, CL_ERASED) )	/* or do not add? */
	{ cr->value.clauses.number_of_clauses--;
	  cr->value.clauses.erased_clauses++;
	}
	DEBUG(MSG_INDEX_UPDATE, Sdprintf("Preparing var to clause-list for %s\n",
					 keyName(key)));
      }
      if ( cr->value.clauses.erased_clauses )
	ch->dirty++;
    }
    addToClauseList(cr, cl, arg1key, where);
  } else
  { cr = newClauseRef(cl, key);
    if ( !ch->head )
      ch->key = key;
    else if ( ch->key != key )
      ch->key = 0;		/* collision */
  }

  if ( is_list )
    where = CL_START;		/* doesn't matter where we insert */
  insertIntoSparseList(cr, &ch->head, &ch->tail, where);

  return key ? 1 : 0;
}


static void
deleteClauseList(ClauseList cl, Clause clause)
{ ClauseRef cr, prev=NULL;

  for(cr=cl->first_clause; cr; prev=cr, cr=cr->next)
  { if ( cr->value.clause == clause )
    { if ( !prev )
      { cl->first_clause = cr->next;
	if ( !cr->next )
	  cl->last_clause = NULL;
      } else
      { prev->next = cr->next;
	if ( !cr->next )
	  cl->last_clause = prev;
      }

      cl->number_of_clauses--;
      return;
    }
  }

  assert(0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
deleteClauseBucket() returns number of deleted   indexable  values. Note
that deleting with key=0  can  still   delete  indexable  values  if the
var-key is the only clause of an indexed clause-list.

TBD: We can delete clause-lists  that   are  indexable  but only contain
non-indexed clauses. This probably requires  us   to  keep  track of the
number of such clauses.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
deleteClauseBucket(ClauseBucket ch, Clause clause, word key, int is_list)
{ ClauseRef prev = NULL;
  ClauseRef c;

  if ( is_list )
  { if ( key )
    { for(c = ch->head; c; prev = c, c = c->next)
      { if ( c->d.key == key )
	{ ClauseList cl = &c->value.clauses;

	  deleteClauseList(cl, clause);
	  if ( !cl->first_clause )
	    goto delete;		/* will return 1 */
	}
      }

      return 0;
    } else
    { int deleted = 0;

      for(c = ch->head; c;)
      { ClauseList cl = &c->value.clauses;

	deleteClauseList(cl, clause);
	if ( !cl->first_clause )
	{ ClauseRef d;

	  d = c;
	  c = c->next;
	  if ( !prev )
	  { ch->head = d->next;
	    if ( !d->next )
	      ch->tail = NULL;
	  } else
	  { prev->next = d->next;
	    if ( !d->next)
	      ch->tail = prev;
	  }
	  if ( d->d.key )
	    deleted++;
	  freeClauseListRef(d);
	  continue;
	}
	prev = c;
	c = c->next;
      }

      return deleted;
    }
  } else
  { for(c = ch->head; c; prev = c, c = c->next)
    { if ( c->value.clause == clause )
      {
      delete:
	if ( !prev )
	{ ch->head = c->next;
	  if ( !c->next )
	    ch->tail = NULL;
	} else
	{ prev->next = c->next;
	  if ( !c->next)
	    ch->tail = prev;
	}
	if ( is_list )
	  freeClauseListRef(c);
	else
	  lingerClauseRef(c);
	return 1;
      }
    }

    assert(0);
    return 0;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) The  clause  list   may   have   been   marked    as   "dirty"   by
deleteActiveClauseFromBucket()  even though  it  does  not  contain the
clause being deleted. We reset the count when we have finished scanning
the list to the number of erased clauses remaining in the list.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
gcClauseList(ClauseList clist, DirtyDefInfo ddi, gen_t start, Buffer tr_starts)
{ ClauseRef cref=clist->first_clause, prev = NULL;
  size_t left = 0;

  while(cref && clist->erased_clauses)
  { Clause cl = cref->value.clause;

    if ( ison(cl, CL_ERASED) )
    { if ( ddi_is_garbage(ddi, start, tr_starts, cl) )
      { ClauseRef c = cref;

	clist->erased_clauses--;

	cref = cref->next;
	if ( !prev )
	{ clist->first_clause = c->next;
	  if ( !c->next )
	    clist->last_clause = NULL;
	} else
	{ prev->next = c->next;
	  if ( c->next == NULL)
	    clist->last_clause = prev;
	}

	lingerClauseRef(c);
	continue;
      } else
      { left++;
      }
    }

    prev = cref;
    cref = cref->next;
  }

  clist->erased_clauses = left; /* see (*) */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gcClauseBucket() removes all erased clauses from  the bucket and returns
the number of indexable entries that have been removed from the bucket.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
gcClauseBucket(Definition def, ClauseBucket ch,
	       unsigned int dirty, int is_list, DirtyDefInfo ddi,
	       gen_t start, Buffer tr_starts)
{ ClauseRef cref = ch->head, prev = NULL;
  int deleted = 0;

  while( cref && dirty )
  { if ( is_list )
    { ClauseList cl = &cref->value.clauses;

      if ( cl->erased_clauses )
      { gcClauseList(cl, ddi, start, tr_starts);
	if ( !cl->erased_clauses )
	  dirty--;

	if ( cl->first_clause == NULL )
	  goto delete;
      }
    } else
    { Clause cl = cref->value.clause;

      if ( ison(cl, CL_ERASED) && ddi_is_garbage(ddi, start, tr_starts, cl) )
      { ClauseRef c;

	dirty--;

      delete:
	c = cref;
	if ( cref->d.key )
	  deleted++;			/* only reduce size by indexed */

	cref = cref->next;
	if ( !prev )
	{ ch->head = c->next;
	  if ( !c->next )
	    ch->tail = NULL;
	} else
	{ prev->next = c->next;
	  if ( c->next == NULL)
	    ch->tail = prev;
	}

	if ( is_list )
	  lingerClauseListRef(def, c);
	else
	  lingerClauseRef(c);

	continue;
      }
    }

    prev = cref;
    cref = cref->next;
  }

  DEBUG(CHK_SECURE,
	{ if ( !is_list )
	  { for(cref=ch->head; cref; cref=cref->next)
	    { Clause cl = cref->value.clause;
	      assert( isoff(cl, CL_ERASED) ||
		      !ddi_is_garbage(ddi, start, tr_starts, cl)
		    );
	    }
	  }
	});

  ch->dirty = dirty;

  return deleted;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See also deleteActiveClauseFromIndexes() comment
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
cleanClauseIndex(Definition def, ClauseList cl, ClauseIndex ci,
		 DirtyDefInfo ddi, gen_t start, Buffer tr_starts)
{ if ( cl->number_of_clauses < ci->resize_below )
  { deleteIndex(def, cl, ci);
  } else
  { if ( ci->dirty )
    { ClauseBucket ch = ci->entries;
      int n = ci->buckets;

      for(; n; n--, ch++)
      { if ( ch->dirty )
	{ ci->size -= gcClauseBucket(def, ch, ch->dirty, ci->is_list,
				     ddi, start, tr_starts);
	  if ( !ch->dirty && --ci->dirty == 0 )
	    break;
	}
      }
    }

    assert((int)ci->size >= 0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cleanClauseIndexes() is called from cleanDefinition()   to remove clause
references erased before generation `active` from the indexes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
cleanClauseIndexes(Definition def, ClauseList cl, DirtyDefInfo ddi,
		   gen_t start, Buffer tr_starts)
{ ClauseIndex *cip;

  if ( (cip=cl->clause_indexes) )
  { for(; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;
      cleanClauseIndex(def, cl, ci, ddi, start, tr_starts);
    }
  }
}


void
clearTriedIndexes(Definition def)
{ int arity = def->functor->arity;
  int i;

  for(i=0; i<arity; i++)
  { arg_info *ainfo = &def->impl.clauses.args[i];

    ainfo->assessed = false;
  }

  def->impl.clauses.jiti_tried = 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clear the ->tried_index  vector  occasionally   for  dynamic  predicates
because evaluation may change. We  do  this   if  the  number of clauses
reaches the next  power  of  two.   We  use  P_SHRUNKPOW2  to inttroduce
histerases into the system, such that asserting/retracting around a pow2
boundery  does  not  lead  to  repeated    re-evaluation  of  the  index
capabilities.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
has_pow2_clauses(Definition def)
{ unsigned int nc = def->impl.clauses.number_of_clauses;

  return nc > 0 && 1<<MSB(nc) == nc;
}


static void
reconsider_index(Definition def)
{ if ( ison(def, P_DYNAMIC) )
  { if ( has_pow2_clauses(def) )
    { if ( ison(def, P_SHRUNKPOW2) )
      { clear(def, P_SHRUNKPOW2);
      } else
      { clearTriedIndexes(def);
      }
    }
  }
}


static void
shrunkpow2(Definition def)
{ if ( ison(def, P_DYNAMIC) )
  { if ( isoff(def, P_SHRUNKPOW2) && has_pow2_clauses(def) )
      set(def, P_SHRUNKPOW2);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
deleteActiveClauseFromBucket() maintains dirty  count   on  the  bucket,
which expresses the number of clause-references  in the chain that needs
updating. All clause-references are clause-lists   and thus we increment
the erased_clauses count thereof.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
deleteActiveClauseFromBucket(ClauseBucket cb, word key)
{ if ( !key )
  { ClauseRef cref;

    for(cref=cb->head; cref; cref=cref->next)
    { ClauseList cl = &cref->value.clauses;

      if ( cl->erased_clauses++ == 0 )
	cb->dirty++;
      cl->number_of_clauses--;
    }
  } else
  { ClauseRef cref;

    for(cref=cb->head; cref; cref=cref->next)
    { if ( cref->d.key == key )
      { ClauseList cl = &cref->value.clauses;

	if ( cl->erased_clauses++ == 0 )
	  cb->dirty++;
	cl->number_of_clauses--;

#ifdef O_DEBUG
	if ( DEBUGGING(CHK_SECURE) )
	{ ClauseRef cr;
	  unsigned int erased = 0;
	  unsigned int count = 0;

	  for(cr=cl->first_clause; cr; cr=cr->next)
	  { if ( ison(cr->value.clause, CL_ERASED) )
	      erased++;
	    else
	      count++;
	  }

	  assert(erased == cl->erased_clauses);
	  assert(count  == cl->number_of_clauses);
	}
#endif
	return;
      }
    }
    assert(0);
  }
}


static inline word
indexKeyFromClause(ClauseIndex ci, Clause cl, Code *end)
{ int h_void = 0;
  Code PC = skipToTerm(cl, ci->position, &h_void);

  if ( likely(ci->args[1] == 0) )
  { int arg = ci->args[0] - 1;
    word key;

    if ( arg > 0 )
      PC = skipArgs(PC, arg, &h_void);
    if ( end )
      *end = PC;
    if ( argKey(PC, 0, &key) )
      return key;
    return 0;
  } else
  { word key[MAX_MULTI_INDEX];
    int  pcarg = 1;
    int  harg;

    DEBUG(CHK_SECURE, if ( end ) *end = NULL);

    for(harg=0; ci->args[harg]; harg++)
    { if ( ci->args[harg] > pcarg )
	PC = skipArgs(PC, ci->args[harg]-pcarg, &h_void);
      pcarg = ci->args[harg];
      if ( !argKey(PC, 0, &key[harg]) )
	return 0;
    }

    return join_multi_arg_keys(key, harg);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deal with deletion of an active  clause   from  the indexes. This clause
cannot really be deleted as it might  still   be  alive  for goals of an
older generation. The task of   this deleteActiveClauseFromIndex() is to
maintain administration that makes it  easy   to  actually  clean up the
index if this is need. The actual cleanup is done by cleanClauseIndex().

On the clause index, it maintains a   `dirty' that indicates how many of
the buckets contain erased clauses. Each  bucket maintains a dirty count
that indicates the number  of  references   to  erased  clauses  in that
bucket.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
deleteActiveClauseFromIndex(ClauseIndex ci, Clause cl)
{ word key = indexKeyFromClause(ci, cl, NULL);

  if ( key == 0 )			/* not indexed */
  { int i;
    ClauseBucket cb;

    for(i=ci->buckets, cb = ci->entries; --i>=0; cb++)
    { if ( cb->dirty == 0 )
	ci->dirty++;
      if ( ci->is_list )
	deleteActiveClauseFromBucket(cb, key);
      else
	cb->dirty++;
    }
    assert(ci->dirty == ci->buckets);
  } else
  { int hi = hashIndex(key, ci->buckets);
    ClauseBucket cb = &ci->entries[hi];

    if ( cb->dirty == 0 )
      ci->dirty++;
    if ( ci->is_list )
      deleteActiveClauseFromBucket(cb, key);
    else
      cb->dirty++;
    assert(cb->dirty>0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
deleteActiveClauseFromIndexes() is called on a   retract  from a dynamic
predicate that is referenced or has too many clauses to justify a costly
update   of   its   clause   lists.    It     is    also   called   from
removeClausesPredicate(), which is called when reloading a source file.

For dynamic predicates, the predicate is  locked. L_PREDICATE is held if
def is static.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
deleteActiveClauseFromIndexes(Definition def, Clause cl)
{ ClauseIndex *cip;

  shrunkpow2(def);

  if ( (cip=def->impl.clauses.clause_indexes) )
  { for(; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) || !ci->entries )
	continue;

      while( ci->incomplete )
	wait_for_index(ci, NULL, NULL);
      if ( ci->invalid )
	continue;

      if ( ison(def, P_DYNAMIC) )
      { if ( def->impl.clauses.number_of_clauses < ci->resize_below )
	{ DEBUG(MSG_JIT_DELINDEX,
		Sdprintf("Deleted index %d from %s (shrunk too much)\n",
			 (int)ci->args[0], predicateName(def)));
	  deleteIndexP(def, &def->impl.clauses, cip);
	} else
	{ deleteActiveClauseFromIndex(ci, cl);
	}
      } else
      { deleteIndexP(def, &def->impl.clauses, cip);
      }
    }
  }
}



void
deleteIndexes(Definition def, ClauseList clist, bool isnew)
{ ClauseIndex *cip0;

  if ( isnew )
  { if ( (cip0=clist->clause_indexes) )
    { ClauseIndex *cip;

      for(cip = cip0; *cip; cip++)
      { ClauseIndex ci = *cip;

	if ( ISDEADCI(ci) )
	  continue;

	unallocClauseIndexTable(ci);
      }

      unalloc_index_array(cip0);
      clist->clause_indexes = NULL;
    }
  } else
  { if ( (cip0=clist->clause_indexes) )
    { ClauseIndex *cip;

      for(cip = cip0; *cip; cip++)
      { ClauseIndex ci = *cip;

	if ( ISDEADCI(ci) )
	  continue;
	deleteIndexP(def, clist, cip);
      }
    }
  }
}


/* Called when it is safe to reclaim all indexes.  I.e.,
   - When terminating and Prolog is stopped
   - If it is a thread-local definition and we cleanup the thread
   - If it is a temporary module that we are cleaning
*/

void
deleteIndexesDefinition(Definition def)
{ ClauseList clist = &def->impl.clauses;
  ClauseIndex *cip0;

  assert(GD->halt.cleaning != CLN_NORMAL ||
	 ison(def, P_LOCALISED) ||
	 !def->module ||
	 def->module->class == ATOM_temporary);

  if ( (cip0=clist->clause_indexes) )
  { clist->clause_indexes = NULL;
    for(ClauseIndex *cip = cip0; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;

      unallocClauseIndexTable(ci);
    }

    freeHeap(cip0, 0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add a clause to an index.  If   the  clause cannot be indexed (typically
because it has a variable at the  argument location), the clause must be
added to all indexes.

ClauseIndex->size maintains the number of elements  in the list that are
indexed. This is needed for resizing the index.

TBD: Merge compound detection with skipToTerm()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
addClauseToIndex(ClauseIndex ci, Clause cl, ClauseRef where)
{ Code pc = NULL;
  ClauseBucket ch = ci->entries;
  word arg1key = 0;

  if ( !ch )
    return true;

  word key = indexKeyFromClause(ci, cl, &pc);
  if ( ci->is_list )			/* find first argument key for term */
  { if ( key == 0 )
      return false;
    switch(decode(*pc))
    { case H_FUNCTOR:
      case H_LIST:
      case H_RFUNCTOR:
      case H_RLIST:
	pc = stepPC(pc);
	argKey(pc, 0, &arg1key);
    }
  }

  if ( key == 0 )			/* a non-indexable field */
  { int n = ci->buckets;

    for(; n; n--, ch++)
      addClauseBucket(ch, cl, key, arg1key, where, ci->is_list);
  } else
  { unsigned int hi = hashIndex(key, ci->buckets);

    DEBUG(MSG_INDEX_UPDATE, Sdprintf("Storing in bucket %d\n", hi));
    ci->size += addClauseBucket(&ch[hi], cl, key, arg1key, where, ci->is_list);
  }

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addClauseToIndexes() is called (only) by   assertProcedure(),  which has
the definition locked.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
addClauseToIndexes(Definition def, Clause clause, ClauseRef where)
{ addClauseToListIndexes(def, &def->impl.clauses, clause, where);
  reconsider_index(def);

  DEBUG(CHK_SECURE, checkDefinition(def));
  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called from unlinkClause(), which is called for retracting a clause from
a dynamic predicate which is not  referenced   and  has  few clauses. In
other cases, deleteActiveClauseFromIndex() is called.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
delClauseFromIndex(Definition def, Clause cl)
{ ClauseIndex *cip;

  shrunkpow2(def);

  for(cip=def->impl.clauses.clause_indexes; *cip; cip++)
  { ClauseIndex ci = *cip;
    ClauseBucket ch;
    word key;

    if ( ISDEADCI(ci) )
      continue;

    ch  = ci->entries;
    key = indexKeyFromClause(ci, cl, NULL);

    if ( key == 0 )			/* a non-indexable field */
    { int n = ci->buckets;

      for(; n; n--, ch++)
	deleteClauseBucket(ch, cl, key, ci->is_list);
    } else
    { int hi = hashIndex(key, ci->buckets);

      ci->size -= deleteClauseBucket(&ch[hi], cl, key, ci->is_list);
    }
  }
}


static void
wait_for_index(const ClauseIndex ci, ClauseList clist, IndexContext ctx)
{ if ( !ci->entries && ctx && realize_clause_index(ci) )
  { DEBUG(MSG_JIT,
	  { char an[64];
	    Sdprintf("[%d] realising index %s for %s\n",
		     PL_thread_self(), iargsName(ci->args, an),
		     predicateName(ctx->predicate)); });

    if ( !fill_clause_index(ci, clist, ctx) )
    { ci->invalid = true;
      return;
    }
  }

#ifdef O_PLMT
  DEBUG(MSG_JIT, Sdprintf("[%d] waiting for index %p ...\n",
			  PL_thread_self(), ci));
  pthread_mutex_lock(&GD->thread.index.mutex);
  if ( ci->incomplete )
    pthread_cond_wait(&GD->thread.index.cond, &GD->thread.index.mutex);
  pthread_mutex_unlock(&GD->thread.index.mutex);
  DEBUG(MSG_JIT, Sdprintf("[%d] index %p %sready\n",
			  PL_thread_self(), ci, ci->incomplete ? "not " : ""));
#endif
}


static void
completed_index(ClauseIndex ci)
{
#ifdef O_PLMT
  pthread_mutex_lock(&GD->thread.index.mutex);
  ci->incomplete = false;
  pthread_cond_broadcast(&GD->thread.index.cond);
  pthread_mutex_unlock(&GD->thread.index.mutex);
  DEBUG(MSG_JIT, Sdprintf("[%d] index %p completed\n",
			  PL_thread_self(), ci));
#else
  ci->incomplete = false;
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a hash-index on def  for  arg.   We  compute  the  hash unlocked,
checking at the end that nobody  messed   with  the clause list. If that
happened anyway, we retry. At the end,   we  lock the definition and add
the new index to the indexes of the predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ClauseIndex
fill_clause_index(ClauseIndex ci, ClauseList clist, IndexContext ctx)
{ for(ClauseRef cref = clist->first_clause; cref; cref = cref->next)
  { if ( isoff(cref->value.clause, CL_ERASED) )
    { if ( !addClauseToIndex(ci, cref->value.clause, CL_END) )
      { ci->invalid = true;
	completed_index(ci);
	deleteIndex(ctx->predicate, clist, ci);
	return NULL;
      }
    }
  }

  ci->resize_above = ci->size*2;
  ci->resize_below = ci->size/4;

  completed_index(ci);
  if ( !consider_better_index(ci->speedup, clist->number_of_clauses) )
    ci->good = true;		/* `good` means complete and sufficient */

  return ci;
}


static ClauseIndex
hashDefinition(ClauseList clist, hash_hints *hints, IndexContext ctx)
{ ClauseIndex ci;
  ClauseIndex *cip;

  DEBUG(MSG_JIT, Sdprintf("[%d] hashDefinition(%s, %s, %d) (%s)\n",
			  PL_thread_self(),
			  predicateName(ctx->predicate),
			  iargsName(hints->args, NULL), 2<<hints->ln_buckets,
			  hints->list ? "lists" : "clauses"));

#if defined(O_PLMT) && defined(O_DEBUG)
{ GET_LD
  assert(LD->thread.info->access.predicate == ctx->predicate);
}
#endif

  canonicalHap(hints->args);
  LOCKDEF(ctx->predicate);
  if ( (cip=clist->clause_indexes) )
  { for(; *cip; cip++)
    { ClauseIndex cio = *cip;

      if ( ISDEADCI(cio) )
	continue;

      if ( memcmp(cio->args, hints->args, sizeof(cio->args)) == 0 )
      { UNLOCKDEF(ctx->predicate);
	DEBUG(MSG_JIT, Sdprintf("[%d] already created\n", PL_thread_self()));
	return cio;
      }
    }
  }
  ci = newClauseIndexTable(hints, true, ctx);
  insertIndex(ctx->predicate, clist, ci);
  UNLOCKDEF(ctx->predicate);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Adding a pause here creates  a window where other threads may mark clauses
as erased (CL_ERASED).  This prevents them being added to the just-created
index.

See the test_cgc_1 test case in tests/GC/test_cgc_1.pl

  usleep(1000);
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  return fill_clause_index(ci, clist, ctx);
}


/* create a copy of an array of clause indexes, optionally inserting
 * `add` ordered by `speedup`.
 */

static ClauseIndex *
copyIndex(ClauseIndex *org, ClauseIndex add)
{ ClauseIndex *ncip;
  int size = add ? 1 : 0;

  if ( org )
  { ClauseIndex *cip;

    for(cip=org; *cip; cip++)
    { ClauseIndex ci = *cip;
      if ( !ISDEADCI(ci) )
	size++;
    }
  }

  if ( size )
  { ClauseIndex *ncipo;

    ncipo = ncip = allocHeapOrHalt((size+1)*sizeof(*ncip));
    if ( org )
    { ClauseIndex *cip;

      for(cip=org; *cip; cip++)
      { ClauseIndex ci = *cip;
	if ( !ISDEADCI(ci) )
	{ if ( add && add->speedup >= ci->speedup )
	  { *ncipo++ = add;
	    add = NULL;
	  }
	  *ncipo++ = ci;
	}
      }
    }
    if ( add )
      *ncipo++ = add;
    assert(ncipo-ncip == size);
    *ncipo = NULL;
  } else
    ncip = NULL;

  return ncip;
}


static int
cmp_indexes(const void *p1, const void *p2)
{ const ClauseIndex* const cip1 = p1;
  const ClauseIndex* const cip2 = p2;
  const ClauseIndex ci1 = *cip1;
  const ClauseIndex ci2 = *cip2;

  if ( ISDEADCI(ci1) )
  { if ( ISDEADCI(ci2) )
      return 0;
    return 1;
  } else if ( ISDEADCI(ci2) )
    return -1;

  return ci1->speedup < ci2->speedup ?  1 :
	 ci1->speedup > ci2->speedup ? -1 : 0;
}


static void
sortIndexes(ClauseIndex *cip)
{ if ( cip )
  { int i;

    for(i=0; cip[i]; i++)
      ;

    qsort(cip, i, sizeof(*cip), cmp_indexes);
  }
}


static int
isSortedIndexes(ClauseIndex *cip)
{ if ( cip )
  { float speedup = (float)PLMAXINT;

    for( ; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;
      if ( speedup < ci->speedup )
	return false;
      speedup = ci->speedup;
    }
  }

  return true;
}


static void
unalloc_index_array(void *p)
{ freeHeap(p, 0);
}

static void
setIndexes(Definition def, ClauseList cl, ClauseIndex *cip)
{ ClauseIndex *cipo = cl->clause_indexes;

  MEMORY_BARRIER();
  cl->clause_indexes = cip;
  if ( cipo )
    linger_always(&def->lingering, unalloc_index_array, cipo);
}


/* Caller must have the predicate locked */

static void
unalloc_ci(void *p)
{ DEBUG(MSG_JIT, Sdprintf("unallocClauseIndexTable(%p)\n", p));
  unallocClauseIndexTable(p);
}

static void				/* definition must be locked */
replaceIndex(Definition def, ClauseList cl, ClauseIndex *cip, ClauseIndex ci)
{ ClauseIndex old = *cip;

  *cip = ci;
  DEBUG(MSG_JIT, Sdprintf("[%d] replaceIndex(%s) %p-->%p (gen=%lld)\n",
			  PL_thread_self(),
			  predicateName(def),
			  old, ci, global_generation()));

  if ( !ISDEADCI(old) )
  { int i;
					/* delete corresponding assessments */
    for(i=0; i<MAX_MULTI_INDEX; i++)
    { unsigned int a;

      if ( (a=old->args[i]) )
      { arg_info *ai = &cl->args[a-1];

	ai->assessed = false;
      }
    }

    linger_always(&def->lingering, unalloc_ci, old);
  }

  if ( !isSortedIndexes(cl->clause_indexes) )
  { cip = copyIndex(cl->clause_indexes, NULL);
    sortIndexes(cip);
    setIndexes(def, cl, cip);
  }
}


static void
deleteIndexP(Definition def, ClauseList cl, ClauseIndex *cip)
{ replaceIndex(def, cl, cip, DEAD_INDEX);
}


static void
deleteIndex(Definition def, ClauseList clist, ClauseIndex ci)
{ ClauseIndex *cip;

  if ( (cip=clist->clause_indexes) )
  { for(; *cip; cip++)
    { if ( *cip == ci )
      { deleteIndexP(def, clist, cip);
	return;
      }
    }
  }

  assert(0);
}

/* Insert ci into clist->clause_indexes.  This is called while holding
 * LOCKDEF(def).
 */

static ClauseIndex
next_clause_index(ClauseIndex *cip)
{ for(; *cip; cip++)
  { ClauseIndex ci = *cip;
    if ( !ISDEADCI(ci) )
      return ci;
  }

  return NULL;
}

static void
insertIndex(Definition def, ClauseList clist, ClauseIndex add)
{ ClauseIndex *ocip;

  if ( (ocip=clist->clause_indexes) )
  { ClauseIndex prev = NULL;
    for(ClauseIndex *cip = ocip; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
      { ClauseIndex next = next_clause_index(cip+1);
	if ( (!prev || prev->speedup >= add->speedup) &&
	     (!next || next->speedup <= add->speedup) )
	{ *cip = add;
	  return;
	}
      } else
      { prev = ci;
      }
    }

    DEBUG(0, assert(isSortedIndexes(ocip)));
    ClauseIndex *ncip = copyIndex(ocip, add);
    setIndexes(def, clist, ncip);
  } else
  { ClauseIndex *cip = allocHeapOrHalt(2*sizeof(*cip));

    cip[0] = add;
    cip[1] = NULL;
    clist->clause_indexes = cip;
  }
}


#if defined(O_MAINTENANCE) || defined(O_DEBUG)

		 /*******************************
		 *	     CHECKING		*
		 *******************************/

int
checkClauseIndexSizes(Definition def, int nindexable)
{ ClauseIndex *cip;
  int rc = true;

  if ( (cip=def->impl.clauses.clause_indexes) )
  { for( ; *cip; cip++ )
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;

      if ( ci->size != nindexable )
      { Sdprintf("%s has inconsistent clause index->size",
		 predicateName(def));
	rc = false;
      }
    }
  }

  return rc;
}


void
checkClauseIndexes(Definition def)
{ ClauseIndex *cip;

  if ( (cip=def->impl.clauses.clause_indexes)  )
  {
#if O_PLMT
    GET_LD
#endif

    for(; *cip; cip++ )
    { ClauseIndex ci = *cip;
      unsigned int i;
      ClauseBucket cb;
      unsigned int ci_dirty = 0;	/* # dirty buckets */
      unsigned int ci_size = 0;		/* # indexable values in table */

      if ( ISDEADCI(ci) )
	continue;
      if ( ci->incomplete )		/* building async; stats may be wrong */
	continue;

      for(i=0,cb=ci->entries; i<ci->buckets; i++,cb++)
      { ClauseRef cref;
	unsigned int dirty = 0;
	Definition old;

	acquire_def2(def, old);
	for(cref=cb->head; cref; cref=cref->next)
	{ if ( cref->d.key )
	    ci_size++;

	  if ( ci->is_list )
	  { ClauseList cl = &cref->value.clauses;
	    ClauseRef cr;
	    unsigned int erased = 0;
	    unsigned int count = 0;

	    for(cr=cl->first_clause; cr; cr=cr->next)
	    { if ( ison(cr->value.clause, CL_ERASED) )
		erased++;
	      else
		count++;
	    }
	    assert(erased == cl->erased_clauses);
	    assert(count  == cl->number_of_clauses);
	    if ( erased )
	      dirty++;
	  } else
	  { Clause clause = cref->value.clause;

	    if ( ison(clause, CL_ERASED) )
	      dirty++;
	  }
	}
	release_def2(def, old);

	assert(cb->dirty == dirty);
	if ( cb->dirty )
	  ci_dirty++;
      }

      assert(ci->dirty == ci_dirty);
      assert(ci->size  == ci_size);
    }
  }
}



void
listIndexGenerations(Definition def, gen_t gen)
{ ClauseIndex *cip;

  if ( (cip=def->impl.clauses.clause_indexes) )
  { GET_LD

    for(; *cip; cip++)
    { ClauseIndex ci = *cip;
      unsigned int i;

      if ( ISDEADCI(ci) )
	continue;

      Sdprintf("\nHash %sindex for %s (%d dirty)\n",
	       ci->is_list ? "list-" : "", iargsName(ci->args, NULL), ci->dirty);

      for(i=0; i<ci->buckets; i++)
      { ClauseRef cref;

	if ( !ci->entries[i].head &&
	     !ci->entries[i].dirty )
	  continue;

	Sdprintf("\nEntries at i = %d, dirty = %d:\n",
		 i, ci->entries[i].dirty);

	acquire_def(def);
	for(cref=ci->entries[i].head; cref; cref=cref->next)
	{ if ( ci->is_list )
	  { ClauseList cl = &cref->value.clauses;
	    ClauseRef cr;

	    Sdprintf("List count=%d, erased=%d (%s)\n",
		     cl->number_of_clauses, cl->erased_clauses,
		     keyName(cref->d.key));

	    for(cr=cl->first_clause; cr; cr=cr->next)
	    { Clause clause = cr->value.clause;

	      Sdprintf("  %p: [%2d] %8s-%10s%s%s\n",
		       clause,
		       clauseNo(clause, 0),
		       generationName(clause->generation.created),
		       generationName(clause->generation.erased),
		       ison(clause, CL_ERASED) ? " erased" : "",
		       visibleClause(clause, gen) ? " v" : " X");
	    }
	  } else
	  { Clause clause = cref->value.clause;

	    Sdprintf("%p: [%2d] %8s-%10s%s%s%s\n",
		     clause,
		     clauseNo(clause, 0),
		     generationName(clause->generation.created),
		     generationName(clause->generation.erased),
		     ison(clause, CL_ERASED) ? " erased" : "",
		     visibleClause(clause, gen) ? " v " : " X ",
		     keyName(cref->d.key));
	  }
	}
	release_def(def);
      }
    }
  }
}


#endif /*O_MAINTENANCE||O_DEBUG*/


		 /*******************************
		 *	     ASSESSMENT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions access the index capabilities   for  a given key-set. It
establishes whether a hash makes  sense,   how  big  (#buckets) the hash
should be and whether to use a list of clauses or a list of clause-lists
as units in the hash. Lists of clauses are more compact and suitable for
arguments that have a good distribution (i.e.  few clauses match a key).
Lists of clause-lists are suitable  if   individual  keys  refer to many
clauses. In such cases we can create   secondary index to improve. There
are two cases for this:

  1. The keys are functors.  In that case we might be able to hash on
  one or more of the compound arguments.
  2. It can be profitable to create a secondary hash on another argument.
  Note that this can always be used, whether the key is a functor or
  not.

Even without secondary indices, lists can be   profitable  if a rare key
and a popular key collide on the same hash-bucket.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ASSESS_BUFSIZE 10

typedef struct key_asm
{ word		key;
  unsigned int	count;
  unsigned int	nvcomp;		/* compound with arguments */
} key_asm;

typedef struct hash_assessment
{ iarg_t	args[MAX_MULTI_INDEX]; /* arg for which to assess */
  size_t	allocated;		/* allocated size of array */
  size_t	size;			/* keys in array */
  size_t	var_count;		/* # non-indexable cases */
  size_t	funct_count;		/* # functor cases */
  float		stdev;			/* Standard deviation */
  float		speedup;		/* Expected speedup */
  unsigned	list : 1;		/* Put lists in the buckets */
  size_t	space;			/* Space indication */
  key_asm      *keys;			/* tmp key-set */
} hash_assessment;


typedef struct assessment_set
{ hash_assessment *assessments;
  int		  count;
  int		  allocated;
  hash_assessment buf[ASSESS_BUFSIZE];
} assessment_set;


static void
init_assessment_set(assessment_set *as)
{ as->assessments = as->buf;
  as->count	  = 0;
  as->allocated   = ASSESS_BUFSIZE;
}

static void
free_keys_in_assessment_set(assessment_set *as)
{ int i;
  hash_assessment *a;
  for (i=0, a=as->assessments; i<as->count; i++, a++)
    if ( a->keys )
      free(a->keys);
}

static void
free_assessment_set(assessment_set *as)
{ if ( as->assessments != as->buf )
    free(as->assessments);
}

static hash_assessment *
alloc_assessment(assessment_set *as, iarg_t *ia)
{ hash_assessment *a;

  if ( as->count >= as->allocated )
  { size_t newbytes = sizeof(*as->assessments)*2*as->allocated;

    if ( as->assessments == as->buf )
    { void *mem = malloc(newbytes);
      if ( mem )
      { as->assessments = mem;
	memcpy(as->assessments, as->buf, sizeof(as->buf));
      } else
	return NULL;
    } else
    { void *mem = realloc(as->assessments, newbytes);
      if ( mem )
	as->assessments = mem;
      else
	return NULL;
    }
    as->allocated *= 2;
  }

  a = &as->assessments[as->count++];
  memset(a, 0, sizeof(*a));
  memcpy(a->args, ia, sizeof(a->args));
  canonicalHap(a->args);

  return a;
}


static int
best_hash_assessment(const void *p1, const void *p2, void *ctx)
{ ClauseList clist = ctx;
  const iarg_t *a1 = p1;
  const iarg_t *a2 = p2;
  const arg_info *i1 = &clist->args[*a1];
  const arg_info *i2 = &clist->args[*a2];

  return i1->speedup - i2->speedup > 0.0 ? -1 :
	 i1->speedup - i2->speedup < 0.0 ?  1 : 0;
}


/* Sort assessments, represented by their argument position on
 * `clist` by decreasing speedup, i.e., best first.
 */

static void
sort_assessments(ClauseList clist,
		 iarg_t *instantiated, int ninstantiated)
{ sort_r(instantiated, ninstantiated, sizeof(*instantiated),
	 best_hash_assessment, clist);
}


static int
compar_keys(const void *p1, const void *p2)
{ const key_asm *k1 = p1;
  const key_asm *k2 = p2;

  return SCALAR_TO_CMP(k1->key, k2->key);
}


/* Figure out the minimum hash size to arrive at a perfect hash,
 * i.e., a hash without duplicates.  The hash assessment has an
 * array of a->size key_asm structs.
 */

static unsigned int
perfect_size(hash_assessment *a)
{ unsigned int ln_buckets = MSB(a->size)&0x1f;
  unsigned int buckets    = 2<<ln_buckets;

  for ( ; buckets <= 32; buckets *= 2, ln_buckets++ )
  { local_bitvector(filled, buckets);
    key_asm *kp = a->keys;
    key_asm *ke = &kp[a->size];

    for( ; kp < ke; kp++)
    { unsigned int i = hashIndex(kp->key, buckets);
      if ( !set_bit(filled, i) )
	break;
    }
    if ( kp == ke )
      return ln_buckets;
  }

  return ln_buckets;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Only the final call gets a clause_count > 0. Here we do the remainder of
the assessment. We could consider  for   a  separate  function to merely
reduce the set.

(*) Currently we cannot  combine  variables   with  functor  indexes  as
firstClause() deterministically goes into the matching functor and after
going into the recursive indexes  we  loose   the  context  to  find the
unbound clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
assess_remove_duplicates(hash_assessment *a, size_t clause_count)
{ a->speedup = 0.0;
  a->list    = false;

  if ( !a->keys )
    return false;

  key_asm *s = a->keys;
  key_asm *o = a->keys-1;
  key_asm *e = &s[a->size];
  word c = 0;				/* invalid key */
  size_t fc = 0;			/* #indexable compounds */
  size_t i  = 0;			/* #unique keys */
  float A=0.0, Q=0.0;

  qsort(a->keys, a->size, sizeof(key_asm), compar_keys);
  for( ; s<e; s++)
  { if ( s->key != c )
    { if ( i++ > 0 && clause_count )
      { float A0 = A;
	A = A+((float)o->count-A)/(float)(i-1);
	Q = Q+((float)o->count-A0)*((float)o->count-A);
	if ( o->nvcomp )
	  fc += o->nvcomp - 1;		/* no point if there is just one */
      }
      c = s->key;
      *++o = *s;
    } else
    { o->count  += s->count;
      s->nvcomp += s->nvcomp;
    }
  }
  if ( i > 0 && clause_count )
  { float A0 = A;
    A = A+((float)o->count-A)/(float)i;
    Q = Q+((float)o->count-A0)*((float)o->count-A);
    if ( o->nvcomp )
      fc += o->nvcomp - 1;
    a->funct_count = fc;
  }

  a->size        = i;
					/* assess quality */
  if ( clause_count )
  { a->stdev   = (float)sqrt(Q/(float)i);
    a->list    = false;

    if ( a->size == 1 )			/* Single value that is not compound */
    { if ( !isFunctor(a->keys[0].key) )
	return false;
    }

    a->speedup =            (float)(clause_count*a->size) /
	      (float)(clause_count - a->var_count + a->var_count*a->size);
					/* punish bad distributions */
    a->speedup /= (float)1.0 + a->stdev*(float)a->size/(float)clause_count;

    a->space = ( a->size * sizeof(struct clause_bucket) +
		 clause_count * SIZEOF_CREF_CLAUSE +
		 a->size * a->var_count * SIZEOF_CREF_CLAUSE );

    if ( a->speedup < (float)clause_count/MIN_SPEEDUP &&
	 a->funct_count > 0 &&
	 a->var_count == 0 )		/* See (*) */
    { a->list = true;
      a->space += a->size * SIZEOF_CREF_LIST;
    }

    if ( (float)a->var_count/(float)a->size > MAX_VAR_FRAC )
    { a->speedup = 0.0;
      return false;			/* not indexable */
    }
  }

  return true;
}


static int
assessAddKey(hash_assessment *a, word key, int nvcomp)
{ if ( a->size > 0 && a->keys[a->size-1].key == key )
  { a->keys[a->size-1].count++;		/* TBD: Keep last-key? */
    if ( nvcomp )
      a->keys[a->size-1].nvcomp++;
  } else if ( a->size < a->allocated )
  {
  put_key:
    a->keys[a->size].key    = key;
    a->keys[a->size].count  = 1;
    a->keys[a->size].nvcomp = nvcomp;
    a->size++;
  } else
  { if ( a->allocated == 0 )
    { a->allocated = 512;
      if ( !(a->keys = malloc(a->allocated*sizeof(*a->keys))) )
	return false;
    } else
    { assess_remove_duplicates(a, 0);
      if ( a->size*2 > a->allocated )
      { key_asm *new = realloc(a->keys, a->allocated*2*sizeof(*a->keys));
	if ( !new )
	  return false;
	a->keys = new;
	a->allocated *= 2;
      }
    }
    goto put_key;
  }

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Given an array of assessments, scan through   all  (life) clauses of the
predicate and add their key to the assessment.  To do this:

  - We first find all arguments involved in some index
  - Then, for each clause
    - We decompile adding placing the key of each argument involved in
      some index in the array `keys`.
    - Walk through the assessments and update them.

TBD: if some argument has too many   non-indexable  values we could stop
trying.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Code
skipToTerm(Clause clause, const iarg_t *position, int *in_hvoid)
{ int an;
  Code pc = clause->codes;

  for(; (an = *position) != END_INDEX_POS; position++)
  { code c;

    DEBUG(MSG_INDEX_DEEP,
	  Sdprintf("Skipping to arg %d for clause %d of %s\n",
		   an, clauseNo(clause, 0), predicateName(clause->predicate)));

    if ( an > 0 )
      pc = skipArgs(pc, an, in_hvoid);
  again:
    c = decode(*pc);
    switch(c)
    { case H_FUNCTOR:
      case H_LIST:
      case H_RFUNCTOR:
      case H_RLIST:
	break;
      case H_LIST_FF:			/* FF1, FF2 */
      { static code var[2];		/* Create dummy code */
	var[0] = encode(H_VOID);
	var[1] = encode(H_VOID);
	return var;
      }
      case I_CHP:
	pc = stepPC(pc);
	goto again;
#if defined(O_DEBUG) || defined(O_MAINTENANCE)
      case H_FIRSTVAR:
      case H_VAR:
      case H_VOID:
      case H_VOID_N:
      case H_POP:
      case I_EXITCATCH:
      case I_EXITRESET:
      case I_EXITFACT:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
	return pc;
      default:
        Sdprintf("Unexpected VM code %" PRIuPTR " at %p\n", c, pc);
	Sdprintf("\topcode=%s\n", codeTable[c].name);
	assert(0);
#else
      default:
	return pc;
#endif
    }
    pc = stepPC(pc);
  }

  return pc;
}


/* True if  the compound at `pc`  can be indexed.  This  is similar to
 * arg1Key(), but we keep it more  simple.
 *
 * As  we consider  a  compound  indexable, we  never  go into  nested
 * compounds.
 *
 * @param pc points at the term.
 */

static bool
indexableCompound(Code pc)
{ for(;; pc = stepPC(pc))
  { switch(decode(*pc))
    { case H_LIST_FF:
	return false;
      case I_CHP:
	continue;
      case H_FUNCTOR:
      case H_RFUNCTOR:
      case H_LIST:
      case H_RLIST:
	pc = stepPC(pc);				/* skip functor */
	for(;; pc = stepPC(pc))
	{ switch(decode(*pc))
	  { case H_FIRSTVAR:
	    case H_VAR:
	    case H_VOID:
	    case H_VOID_N:
	      continue;		/* Try next argument */
	    case H_POP:
	      return false;		/* End of argument list */
	    default:
	      return true;		/* Indexable */
	  }
	}
      default:
	assert(0);
	return false;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Access a number of arguments on their suitability for a set of clauses.

@param ac is the highest argument considered.  This is max(arity,MAXINDEXARG)
@param hash_assessment holds the assessments we want to establish.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
assess_scan_clauses(ClauseList clist, iarg_t ac,
		    hash_assessment *assessments, int assess_count,
		    IndexContext ctx)
{ hash_assessment *a;
  ClauseRef cref;
  int i;
  bit_vector *ai = alloca(sizeof_bitvector(ac));
  int kp[MAXINDEXARG+1];			/* key-arg positions */
  int nk = 0;					/* number of key args */
  int *kpp;
  word keys[MAXINDEXARG];
  bool nvcomp[MAXINDEXARG];

  /* Find the arguments we must check.  Assessments may be for
     multiple arguments.
   */
  init_bitvector(ai, ac);
  for(i=0, a=assessments; i<assess_count; i++, a++)
  { int j;

    for(j=0; a->args[j]; j++)
      set_bit(ai, a->args[j]-1);
  }

  /* kp[] is an array of arguments we must check, ending in -1
   */
  for(i=0; i<ac; i++)
  { if ( true_bit(ai, i) )
      kp[nk++] = i;
  }
  kp[nk] = -1;

  /* Step through the clause list */
  for(cref=clist->first_clause; cref; cref=cref->next)
  { Clause cl = cref->value.clause;
    Code pc;
    int carg = 0;
    int h_void = 0;

    if ( ison(cl, CL_ERASED) )
      continue;

    pc = skipToTerm(cl, ctx->position, &h_void);

    /* fill keys[i] with the value of arg kp[i] */
    for(kpp=kp; kpp[0] >= 0; kpp++)
    { if ( kpp[0] > carg )
	pc = skipArgs(pc, kpp[0]-carg, &h_void);
      carg = kpp[0];
      argKey(pc, 0, &keys[kpp[0]]);
      nvcomp[kpp[0]] = false;
						/* see whether this a compound */
      if ( isFunctor(keys[kpp[0]]) )		/* with nonvar args */
      { if ( indexableCompound(pc) )
	  nvcomp[kpp[0]] = true;
      }
    }

    for(i=0, a=assessments; i<assess_count; i++, a++)
    { if ( !a->args[1] )			/* single argument index */
      { word key;
	int an = a->args[0]-1;

	if ( (key=keys[an]) )
	  assessAddKey(a, key, nvcomp[an]);
	else
	  a->var_count++;
      } else					/* multi-argument index */
      { word key[MAX_MULTI_INDEX];
	int  harg;
	bool isvar = false;

	for(harg=0; a->args[harg]; harg++)
	{ if ( !(key[harg] = keys[a->args[harg]-1]) )
	  { isvar = true;
	    break;
	  }
	}
	if ( isvar )
	  a->var_count++;
	else
	  assessAddKey(a, join_multi_arg_keys(key, harg), false);
      }
    }
  }
}


static hash_assessment *
best_assessment(hash_assessment *assessments, int count, size_t clause_count)
{ int i;
  hash_assessment *a, *best = NULL;
  float minbest = MIN_SPEEDUP;

  for(i=0, a=assessments; i<count; i++, a++)
  { assess_remove_duplicates(a, clause_count);
    if ( a->speedup > minbest )
    { best = a;
      minbest = a->speedup;
    }
  }

  return best;
}

/* Update clist->args[i] for each candidate index in `aset`.  This fills
 * the argument's `ainfo` struct with the speedup, size and whether or not
 * a "list" index should be created"
 */

static void
assess_candidate_indexes(iarg_t ac, ClauseList clist, assessment_set *aset,
			 IndexContext ctx)
{ hash_assessment *a;
  int i;

  assess_scan_clauses(clist, ac, aset->assessments, aset->count, ctx);

  for(i=0, a=aset->assessments; i<aset->count; i++, a++)
  { arg_info *ainfo = &clist->args[a->args[0]-1];

    if ( assess_remove_duplicates(a, clist->number_of_clauses) )
    { DEBUG(MSG_JIT,
	    Sdprintf("Assess index %s of %s: speedup %f, stdev=%f\n",
		     iargsName(a->args, NULL),
		     predicateName(ctx->predicate),
		     a->speedup, a->stdev));

      ainfo->speedup    = a->speedup;
      ainfo->list       = a->list;
      ainfo->ln_buckets = perfect_size(a)&0x1f;
    } else
    { ainfo->speedup    = 0.0;
      ainfo->list       = false;
      ainfo->ln_buckets = 0;

      DEBUG(MSG_JIT, Sdprintf("Assess index %s of %s: not indexable\n",
			      iargsName(a->args, NULL),
			      predicateName(ctx->predicate)));
    }

    ainfo->assessed = true;

    if ( a->keys )
      free(a->keys);
  }
}

static arg_info *
ensure_arg_info(ClauseList clist, iarg_t ac)
{ if ( !clist->args )
  { arg_info *ai = allocHeapOrHalt(ac*sizeof(*ai));
    memset(ai, 0, ac*sizeof(*ai));
    if ( !COMPARE_AND_SWAP_PTR(&clist->args, NULL, ai) )
      freeHeap(ai, ac*sizeof(*ai));
  }

  return clist->args;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bestHash() finds the best argument for creating a hash, given a concrete
argument vector and a list of  clauses.   To  do  so, it establishes the
following figures:

  - Total number of non-erased clauses in cref
  - For each indexable argument
    - The count of distinct values
    - The count of non-indexable clauses (i.e. clauses with a var
      at that argument)

Now, the hash-table has a space   that  is #clauses*(nvars+1), while the
expected speedup is

	       #clauses * #distinct
	----------------------------------
	#clauses - #var + #var * #distinct

@return `true` if a best hash was found.  Details on the best hash are in
*hints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline bool
better_index(ClauseIndex better_than, float speedup, float min_speedup)
{ if ( better_than )
    return speedup > better_than->speedup*min_speedup;

  return true;
}

static void
cp_hints_from_arg_info(hash_hints *hints, iarg_t arg0, const arg_info *ai)
{ memset(hints, 0, sizeof(*hints));
  hints->args[0]    = (iarg_t)(arg0+1);
  hints->ln_buckets = ai->ln_buckets;
  hints->speedup    = ai->speedup;
  hints->list       = ai->list;
}

static void
cp_hints_from_assessment(hash_hints *hints, const hash_assessment *a)
{ memset(hints, 0, sizeof(*hints));
  memcpy(hints->args, a->args, sizeof(a->args));
  hints->ln_buckets = MSB(a->size)&0x1f;
  hints->speedup    = a->speedup;
  hints->list       = a->list;
}

static bool
bestHash(DECL_LD Word av, size_t argc,
	 ClauseList clist, ClauseIndex better_than,
	 hash_hints *hints, IndexContext ctx)
{ assessment_set aset;
  int best = -1;
  float best_speedup = 0.0;
  iarg_t ia[MAX_MULTI_INDEX] = {0};
  iarg_t *instantiated;
  int ninstantiated = 0;
  iarg_t ac = argc > MAXINDEXARG ? MAXINDEXARG : (iarg_t)argc;

					/* Step 1: find instantiated args */
  instantiated = alloca(ac*sizeof(*instantiated));
  for(iarg_t i=0; i<ac; i++)
  { if ( canIndex(av[i]) )
      instantiated[ninstantiated++] = i;
  }

  if ( ninstantiated == 0 )
    return false;
  init_assessment_set(&aset);		/* Prepare for assessment */
  ensure_arg_info(clist, ac);

					/* Step 2: find new unassessed args*/
  for(int i=0; i<ninstantiated; i++)
  { iarg_t arg = instantiated[i];

    if ( !clist->args[arg].assessed )
    { ia[0] = arg+1;
      alloc_assessment(&aset, ia);
    }
  }

  if ( aset.count )			/* Step 3: assess them */
  { assess_candidate_indexes(ac, clist, &aset, ctx);
    free_assessment_set(&aset);
  }

				/* Step 4: find the best (single) arg */
  for(int i=0; i<ninstantiated; i++)
  { iarg_t arg = instantiated[i];
    arg_info *ainfo = &clist->args[arg];

    if ( ainfo->speedup > best_speedup )
    { best = arg;
      best_speedup = ainfo->speedup;
    }
  }

  if ( best >= 0 )		/* Found at least one index */
  { if ( consider_better_index(best_speedup, clist->number_of_clauses) &&
	 ninstantiated > 1 )	/* ... but not a real good one ... */
    { DEBUG(MSG_JIT, Sdprintf("%s: %zd clauses, index [%d]: speedup = %f"
			      "; trying multi-argument index\n",
			      predicateName(ctx->predicate),
			      clist->number_of_clauses,
			      best+1, best_speedup));

      if ( find_multi_argument_hash(ac, clist, instantiated, ninstantiated,
				    better_than, hints, ctx) )
	return true;
    }

    if ( better_index(better_than, best_speedup, MIN_SPEEDUP) )
    { arg_info *ainfo = &clist->args[best];

      cp_hints_from_arg_info(hints, best, ainfo);

      return true;
    }
  }

  return false;
}

/* Try to find an index over multiple arguments if all single argument
 * indexes  provide poor  selectivity.   As is,  we  only combine  two
 * indexes,  although the  code  is  prepared to  combine  up to  with
 * `MAX_MULTI_INDEX` arguments in  a single index.
 *
 * @param instantiated contains the argument that are instantiated and
 * (thus) candidates for the multi-argument  index.  Note that none of
 * the indexes is  really good as in that case  we would have selected
 * this good one.
 */

static bool
find_multi_argument_hash(DECL_LD iarg_t ac, ClauseList clist,
			 iarg_t *instantiated, int ninstantiated,
			 ClauseIndex better_than,
			 hash_hints *hints, IndexContext ctx)
{ int ok, m, n;

  sort_assessments(clist, instantiated, ninstantiated);
  for( ok=0;
       ok<ninstantiated &&
	 clist->args[instantiated[ok]].speedup > MIN_SPEEDUP;
       ok++ )
    ;

  DEBUG(MSG_JIT, Sdprintf("  found %d candidate arguments\n", ok));

  if ( ok >= 2 && clist->jiti_tried <= ac )
  { assessment_set aset;
    iarg_t ia[MAX_MULTI_INDEX] = {0};
    hash_assessment *nbest;

    clist->jiti_tried++;
    init_assessment_set(&aset);
    for(m=1; m<ok; m++)
    { ia[1] = instantiated[m]+1;
      for(n=0; n<m; n++)
      { ia[0] = instantiated[n]+1;
	alloc_assessment(&aset, ia);
      }
    }

    assess_scan_clauses(clist, ac, aset.assessments, aset.count, ctx);
    nbest = best_assessment(aset.assessments, aset.count,
			    clist->number_of_clauses);
    if ( nbest && better_index(better_than, nbest->speedup, MIN_SPEEDUP) )
    { DEBUG(MSG_JIT, Sdprintf("%s: using index %s, speedup = %f\n",
			      predicateName(ctx->predicate),
			      iargsName(nbest->args, NULL),
			      nbest->speedup));
      memset(hints, 0, sizeof(*hints));
      memcpy(hints->args, nbest->args, sizeof(nbest->args));
      hints->ln_buckets = MSB(nbest->size);
      hints->speedup    = nbest->speedup;

      free_keys_in_assessment_set(&aset);
      free_assessment_set(&aset);
      return true;
    }
    free_keys_in_assessment_set(&aset);
    free_assessment_set(&aset);
  }

  return false;
}

		 /*******************************
		 *    FIND CANDIDATE INDEXES    *
		 *******************************/

/* Find all candidate indexes.  A candidate index is one of
 *
 *   - A single argument index better than MIN_SPEEDUP
 *   - A multi argument index better than MIN_SPEEDUP times the best
 *     single argument index it is built from.  This implies that
 *     candidates are constructed from indexes of at least MIN_SPEEDUP
 *     and less than number_of_clauses/MIN_SPEEDUP.
 *
 * Arguments  with mode  `-` are  excluded from  the evaluation.   The
 * discovered indexes are sorted by quality.
 *
 * @param nphints is the size of  the hints structure on entry.  It is
 * updated to the number of candidates.  As we limit to 2 arguments at
 * the  moment,  given  N  candidate arguments,  there  are  maximally
 * N+N*(N-1) or simply N^2 possible indexes.  If the maximum is lower,
 * we only  compute the top.   If the max is  much lower, we  only use
 * heuristics to limit the number  of two-argument indexes and thus we
 * may not end up with the ideal.
 */

static int
cmp_assessment(const void *p1, const void *p2)
{ hash_assessment * const *a1 = p1;
  hash_assessment * const *a2 = p2;

  return SCALAR_TO_CMP((*a1)->speedup, (*a2)->speedup);
}

/* ensure clist->args[a] is filled for all arguments (<ac)
 */

static void
assess_all_arguments(iarg_t ac, ClauseList clist, const IndexContext ctx)
{ assessment_set aset;

  init_assessment_set(&aset);
  for(iarg_t i=0; i<ac; i++)
  { arg_info *ai;

    if ( ctx->depth == 0 && mode_arg_is_unbound(ctx->predicate, i) )
      continue;

    if ( !((ai = &clist->args[i]) && ai->assessed) )
    { iarg_t ia[MAX_MULTI_INDEX] = {i+1, 0};
      alloc_assessment(&aset, ia);
    }
  }
  assess_candidate_indexes(ac, clist, &aset, ctx);
  free_assessment_set(&aset);
}

/* Create the "good" indexes.
 *
 * @param assessments is an array of 0-based arguments for clist.
 */

static int
create_good_indexes(const ClauseList clist,
		    const iarg_t *assessments, int nassessments,
		    hash_hints *hints, int *nphints, int max_hints)
{ int nhints = *nphints;
  int i;

  for(i=0; i<nassessments && nhints < max_hints ; i++)
  { int arg0 = assessments[i];
    const arg_info *ai = &clist->args[arg0];

    if ( (float)clist->number_of_clauses/ai->speedup < MIN_SPEEDUP )
    { if ( clist->number_of_clauses > MIN_CLAUSES_FOR_INDEX ||
	   clist->primary_index != arg0 )
	cp_hints_from_arg_info(&hints[nhints++], arg0, ai);
    } else
      break;
  }

  *nphints = nhints;
  return i;
}

/* Create the deep indexes and remove  bad indexes.  Note that a "bad"
 * list index may in be good after we realise the secondary indexes.
 */
static void
create_deep_indexes(const ClauseList clist,
		    iarg_t *assessments, int *npassessments,
		    hash_hints *hints, int *nphints, int max_hints)
{ int nhints = *nphints;
  int nassessments = *npassessments;
  iarg_t *keep = assessments;

  for(int i=0; i<nassessments; i++)
  { int arg0 = assessments[i];
    const arg_info *ai = &clist->args[arg0];

    if ( ai->list && nhints < max_hints )
      cp_hints_from_arg_info(&hints[nhints++], arg0, ai);
    else if ( ai->speedup > MIN_SPEEDUP )
      *keep++ = arg0;
  }

  *npassessments = keep-assessments;
  *nphints = nhints;
}



static bool
candidate_indexes(iarg_t ac, ClauseList clist, hash_hints *hints, int *nphints,
		  IndexContext ctx)
{ int max_hints = *nphints;
  int nhints = 0;
				/* Assess all non-yet-assessed arguments */
  assess_all_arguments(ac, clist, ctx);

				/* Sort by quality */
  iarg_t *assessments = alloca(sizeof(*assessments)*ac);
  int nassessments = 0;
  for(iarg_t i=0; i<ac; i++)
  { const arg_info *ai = &clist->args[i];
    if ( ai && ai->assessed &&
	 (ai->speedup > MIN_SPEEDUP || ai->list) )
      assessments[nassessments++] = i;
  }
  if ( nassessments == 0 )
  { *nphints = 0;
    return true;
  }
  sort_assessments(clist, assessments, nassessments);

  /* Create the good ones.  They are best and need not be combined */
  int good = create_good_indexes(clist,
				 assessments, nassessments,
				 hints, &nhints, max_hints);
  if ( nhints == max_hints )
  { *nphints = nhints;
    return true;
  }
  assessments += good;
  nassessments -= good;

  /* Create the poor deep indexes and remove the other poor ones */
  create_deep_indexes(clist, assessments, &nassessments,
		      hints, &nhints, max_hints);
  if ( nhints == max_hints )
  { *nphints = nhints;
    return true;
  }

  if ( nassessments >= 2 )	/* Add least two candidates */
  { int f=0, t=nassessments;
    iarg_t ia[MAX_MULTI_INDEX] = {0};

    for(;;)			/* If far too many, take the middle ones */
    { int l1 = t-f;		/* as these are the most promising */

      if ( l1 < 2 )
      { *nphints = nhints;
	return true;
      }
      l1--;
      if ( l1*l1 > max_hints-nhints )
      { if ( f%2 == 0 )
	  f++;
	else
	  t--;
      } else
	break;
    }

    assessment_set aset;
    init_assessment_set(&aset);
    for(int m=f+1; m<t; m++)
    { ia[1] = assessments[m]+1;
      for(int n=f; n<m; n++)
      { ia[0] = assessments[n]+1;
	alloc_assessment(&aset, ia);
      }
    }
    assess_scan_clauses(clist, ac, aset.assessments, aset.count, ctx);
    hash_assessment **alist = alloca(sizeof(*alist)*aset.count);
    hash_assessment **ap = alist;
    for(int i=0; i<aset.count; i++)
    { bool poor = false;
      hash_assessment *a = aset.assessments+i;

      assess_remove_duplicates(a, clist->number_of_clauses);
      for(int j=0; j<2; j++)
      { const arg_info *ai = &clist->args[a->args[j]-1];
	if ( a->speedup < ai->speedup*MIN_SPEEDUP )
	{ poor = true;
	  break;
	}
      }
      if ( !poor )
	*ap++ = a;
    }
    qsort(alist, ap-alist, sizeof(*alist), cmp_assessment);

    /* merge single and multiple argument indexes by speedup */
    hash_assessment **a = alist;
    int single = 0;
    while( nhints < max_hints )
    { if ( single < nassessments )
      { iarg_t si = assessments[single];
				/* skip the primary index */
	if ( clist->number_of_clauses <= MIN_CLAUSES_FOR_INDEX &&
	     clist->primary_index == si )
	{ single++;
	  continue;
	}
	const arg_info *ai = &clist->args[si];
	if ( a < ap )
	{ if ( (*a)->speedup > ai->speedup )
	  { cp_hints_from_assessment(&hints[nhints++], *a++);
	  } else
	  { cp_hints_from_arg_info(&hints[nhints++], si, ai);
	    single++;
	  }
	} else
	{ cp_hints_from_arg_info(&hints[nhints++], si, ai);
	  single++;
	}
      } else if ( a < ap )
      { cp_hints_from_assessment(&hints[nhints++], *a++);
      } else
	break;
    }
  }

  *nphints = nhints;
  return true;
}

/* Set the  candidate indexes  for a  predicate.  This  associates all
 * meaningful indexes with a predicate without realizing them.
 */

static ClauseIndex
get_existing_index(ClauseIndex **from, const hash_hints *hints)
{ ClauseIndex *cip = *from;

  if ( cip )
  { for(; *cip; cip++)
    { ClauseIndex ci = *cip;
      if ( !ISDEADCI(ci) )
      { if ( memcmp(ci->args, hints->args, sizeof(ci->args)) == 0 )
	{ *from = cip+1;
	  return ci;
	}
      }
    }
  }

  return NULL;
}

static bool
set_candidate_indexes(Definition def, ClauseList clist, int max, bool lock)
{ hash_hints *hints = alloca(sizeof(*hints)*max);
  index_context ctx = { .predicate = def, .position[0] = END_INDEX_POS };
  iarg_t ac = def->functor->arity > MAXINDEXARG ? MAXINDEXARG
						: (iarg_t)def->functor->arity;

  candidate_indexes(ac, clist, hints, &max, &ctx);
  if ( max > 0 )
  { ClauseIndex *cip = allocHeapOrHalt((max+1)*sizeof(*cip));

    if ( lock )
      LOCKDEF(def);
    ClauseIndex *org = clist->clause_indexes;
    for(int i=0; i<max; i++)
    { ClauseIndex ei = get_existing_index(&org, &hints[i]);
      if ( ei )
	cip[i] = ei;
      else
	cip[i] = newClauseIndexTable(&hints[i], false, &ctx);
    }
    cip[max] = NULL;
    setIndexes(def, clist, cip);
    clist->fixed_indexes = true;
    if ( lock )
      UNLOCKDEF(def);
  } else
  { clist->fixed_indexes = true;
  }

  return true;
}


		 /*******************************
		 *      PROLOG CONNECTION       *
		 *******************************/

static
PRED_IMPL("$set_candidate_indexes", 2, set_candidate_indexes,
	  PL_FA_TRANSPARENT)
{ Procedure proc;
  Definition def;
  int max;

  if ( !get_procedure(A1, &proc, 0, GP_RESOLVE|GP_NAMEARITY) )
    return false;
  def = proc->definition;
  if ( !PL_get_integer_ex(A2, &max) )
    return false;
  if ( max < 0 )
    return PL_domain_error("not_less_than_zero", A3);

  return set_candidate_indexes(def, &def->impl.clauses, max, true);
}


static atom_t i_tag_arg_info;
static atom_t i_tag_hash_info;
static atom_t i_keys[4];

static void
init_dict_keys(void)
{ if ( !i_tag_arg_info )
  { i_keys[0] = PL_new_atom("speedup");
    i_keys[1] = PL_new_atom("ln_buckets");
    i_keys[2] = PL_new_atom("list");
    i_keys[3] = PL_new_atom("arguments");
    i_tag_hash_info = PL_new_atom("hash_info");
    i_tag_arg_info  = PL_new_atom("arg_info");
  }
}

#define put_args(t, args) LDFUNC(put_args, t, args)

static bool
put_args(DECL_LD term_t t, const iarg_t args[MAX_MULTI_INDEX])
{ const iarg_t *ep;

  for(ep=args; ep-args < MAX_MULTI_INDEX && *ep; ep++)
    ;
  PL_put_nil(t);
  term_t el = PL_new_term_ref();
  if ( !el )
    return false;
  while( ep > args )
  { ep--;
    if ( !PL_put_integer(el, *ep) ||
	 !PL_cons_list(t, el, t) )
      return false;
  }
  PL_reset_term_refs(el);

  return true;
}

#define put_hints(t, hints) LDFUNC(put_hints, t, hints)

static bool
put_hints(DECL_LD term_t t, const hash_hints *hints)
{ term_t values;

  init_dict_keys();
  bool rc = ( (values = PL_new_term_refs(4)) &&
	      PL_put_float(values+0, hints->speedup) &&
	      PL_put_integer(values+1, hints->ln_buckets) &&
	      PL_put_bool(values+2, hints->list) &&
	      put_args(values+3, hints->args) &&
	      PL_put_dict(t, i_tag_hash_info, 4, i_keys, values) );
  if ( rc )
    PL_reset_term_refs(values);

  return rc;
}


/** '$candidate_indexes'(+PI, -Indexes, +Max)
 */

static
PRED_IMPL("$candidate_indexes", 3, candidate_indexes, PL_FA_TRANSPARENT)
{ Procedure proc;
  int max;

  if ( !get_procedure(A1, &proc, 0, GP_RESOLVE|GP_NAMEARITY) )
    return false;
  if ( !PL_get_integer_ex(A3, &max) )
    return false;
  if ( max < 0 )
    return PL_domain_error("not_less_than_zero", A3);

  hash_hints *hints = malloc(sizeof(*hints)*max);
  if ( !hints )
    return PL_no_memory();

  Definition def = proc->definition;
  size_t arity = def->functor->arity;
  iarg_t ac = arity < MAXINDEXARG ? (iarg_t)arity : MAXINDEXARG;
  index_context ctx = { .predicate = def, .position[0] = END_INDEX_POS };
  candidate_indexes(ac, &def->impl.clauses, hints, &max, &ctx);

  term_t tail = PL_copy_term_ref(A2);
  term_t head = PL_new_term_ref();
  term_t tmp  = PL_new_term_ref();

  bool rc = true;
  for(int i=0; i<max; i++)
  { if ( !put_hints(tmp, hints+i) ||
	 !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, tmp) )
    { rc = false;
      break;
    }
  }
  rc = rc && PL_unify_nil(tail);
  free(hints);

  return rc;
}

		 /*******************************
		 *      PRIMARY INDEX ARG       *
		 *******************************/

/* Find the 0-based argument on which to place the primary index.
 *
 * This  must be  an  argument for  which  argKey() returns  different
 * values.  Note  that non-var  is not  good as  there is  also little
 * point testing the same value over and over again.
 *
 * @return -1 if there is no meaningful primary index.
 */

#define PINDEX_POSSIBLE		(0)
#define PINDEX_IMPOSSIBLE	(-1)
#define PINDEX_MAYBEDEEP	(-2)

static int
can_be_primary_index(ClauseList clist, int arg0)
{ bool first = true;
  word key;

  for(ClauseRef cref = clist->first_clause; cref; cref = cref->next)
  { word clkey;

    if ( ison(cref->value.clause, CL_ERASED) )
      continue;

    argKey(cref->value.clause->codes, arg0, &clkey);
    if ( first )
    { first = false;
      key = clkey;
    } else if ( key != clkey )
      return PINDEX_POSSIBLE;
  }

  if ( !first && isFunctor(key) )
    return PINDEX_MAYBEDEEP;

  return PINDEX_IMPOSSIBLE;
}

/* Find the argument to use as primary index.  Returns one of
 *
 *   - An index (>=0)
 *   - PINDEX_IMPOSSIBLE: no meaningful indexing possible
 *   - PINDEX_MAYBEDEEP: some arg has all the same functor;
 *     maybe we can make a deep index.
 */

static int
preferred_primary_index(Definition def)
{ ClauseList clist = &def->impl.clauses;
  int rc = PINDEX_IMPOSSIBLE;

  /* is primary index ok? */
  if ( !mode_arg_is_unbound(def, clist->primary_index) )
  { bool first = true;
    word key = 0;		/* Keep compiler happy */
    for(ClauseRef cref = clist->first_clause; cref; cref = cref->next)
    { if ( ison(cref->value.clause, CL_ERASED) )
	continue;
      if ( first )
      { first = false;
	key = cref->d.key;
      } else if ( key != cref->d.key )
	return clist->primary_index;
    }
    if ( !first && isFunctor(key) )
      rc = PINDEX_MAYBEDEEP;
  }

  for(size_t a=0; a<def->functor->arity; a++)
  { if ( a != clist->primary_index &&
	 !mode_arg_is_unbound(def, a) )
    { int c = can_be_primary_index(clist, a);
      if ( c == PINDEX_POSSIBLE )
	return (int)a;
      if ( c == PINDEX_MAYBEDEEP )
	rc = PINDEX_MAYBEDEEP;
    }
  }

  return rc;
}

/* The primary index  argument is the clause argument  used to compute
 * the  `cref->d.key`  that is  used  for  the  fast linear  scan  for
 * clauses.  This  mechanism is used  instead of  a hash table  if the
 * primary  argument is  instantiated and  the predicate  has at  most
 * `MIN_CLAUSES_FOR_INDEX`.
 *
 * Note that  changing this is  not thread-safe.   One way to  make it
 * thread safe is to  set all keys to zero before,  so all clauses are
 * considered candidates.   Then we change  `clist->primary_index` and
 * finally we set all keys.   Alternatively, we can change this inside
 * setDefaultSupervisor(),   which  is   executed  before   the  first
 * execution of the predicate.
 */

static void
modify_primary_index_arg(Definition def, iarg_t an)
{ ClauseList clist = &def->impl.clauses;

  if ( clist->primary_index != an )
  { for(ClauseRef cref = clist->first_clause;
	cref;
	cref=cref->next)
    { Clause cl = cref->value.clause;

      argKey(cl->codes, an, &cref->d.key);
    }

    clist->primary_index = an;
    clist->unindexed = false;
  }
}

void
update_primary_index(DECL_LD Definition def)
{ ClauseList clist = &def->impl.clauses;

  if ( clist->pindex_verified )
    return;
  clist->pindex_verified = true;

  if ( def->functor->arity > 0 )
  { unsigned int noc = clist->number_of_clauses;

    if ( noc < MIN_CLAUSES_FOR_INDEX &&
	 isoff(def, P_DYNAMIC|P_MULTIFILE|P_THREAD_LOCAL|P_FOREIGN) &&
	 !STATIC_RELOADING(def) )
    { int arg0 = clist->primary_index;
      int argn = preferred_primary_index(def);
      if ( argn >= 0 )
      { if ( argn != arg0 )
	{ DEBUG(MSG_JIT_PRIMARY,
		Sdprintf("Set primary index for %s (%d clauses) to %d\n",
			 predicateName(def), noc, argn+1));
	  modify_primary_index_arg(def, argn);
	}
      } else
      { if ( argn == PINDEX_MAYBEDEEP )
	  set_candidate_indexes(def, clist, 10, false);
	clist->unindexed = clist->clause_indexes == NULL;
	DEBUG(MSG_JIT_PRIMARY,
	      if ( clist->unindexed )
		Sdprintf("No index for %s (%d clauses)\n",
		       predicateName(def), noc));
      }
    }
  } else
  { clist->unindexed = true;
  }
}

		 /*******************************
		 *  PREDICATE PROPERTY SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Index info is of the form

    Where - hash(Buckets, Speedup, SizeInBytes, IsList)

Where is one of

  - Simple index		single(ArgN)
  - Multi-argument index	multi([Arg1,Arg2,...])
  - Deep index			deep([Arg1,Arg2,...])

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
sizeofClauseIndex(ClauseIndex ci)
{ size_t size = sizeof(*ci);

  if ( ci->entries )
  { size_t vars = 0;
    ClauseRef cref;
    size_t usize = ci->is_list ? SIZEOF_CREF_LIST : SIZEOF_CREF_CLAUSE;

    size += ci->buckets * sizeof(*ci->entries);
    size += ci->size * usize;

    for(cref=ci->entries[0].head; cref; cref=cref->next)
    { if ( cref->d.key == 0 )
	vars++;
    }
    size += vars * ci->buckets * usize;
  }

  return size;
}


size_t
sizeofClauseIndexes(Definition def)
{
#ifdef O_PLMT
  GET_LD
#endif
  ClauseIndex *cip;
  size_t size = 0;

  if ( (cip=def->impl.clauses.clause_indexes) )
  { size += sizeof(*cip);	/* terminating NULL */
    acquire_def(def);
    for(; *cip; cip++)
    { ClauseIndex ci = *cip;

      size += sizeof(ci);
      if ( ISDEADCI(ci) )
	continue;
      size += sizeofClauseIndex(ci);
    }
    release_def(def);
  }

  return size;
}

static size_t
collisionCount(const ClauseIndex ci)
{ size_t cc = 0;

  if ( ci->entries )
  { for(size_t i=0; i<ci->buckets; i++)
    { const ClauseBucket bkt = &ci->entries[i];
      if ( bkt->head && !bkt->key )
	cc++;
    }
  }

  return cc;
}

/* Unify `t` with a dict representing a (hash) index.  Keys are:
 *
 *   - arguments: list of arguments hashed.
 *   - position:  Argument nesting.  [] is a toplevel index.
 *     [1,2] is an index in the 2nd argument of the 1st argument
 *     of the predicate.
 *   - buckets: # buckets of the hash
 *   - speedup: Expected speedup (a float)
 *   - list: Has sub indexes
 *   - size: Bytes used for the index
 *   - realised: bool indicating whether the index is realised.
 *   - collisions: # buckets that represent multiple keys
 */

#define NUM_INDEX_KEYS 8
static atom_t i_tag_hash = 0;
static atom_t i_index_keys[NUM_INDEX_KEYS];

static void
init_index_keys(void)
{ if ( !i_tag_hash )
  { i_index_keys[0] = PL_new_atom("arguments");
    i_index_keys[1] = PL_new_atom("position");
    i_index_keys[2] = PL_new_atom("buckets");
    i_index_keys[3] = PL_new_atom("speedup");
    i_index_keys[4] = PL_new_atom("list");
    i_index_keys[5] = PL_new_atom("size");
    i_index_keys[6] = PL_new_atom("realised");
    i_index_keys[7] = PL_new_atom("collisions");
    // NUM_INDEX_KEYS = 8
    i_tag_hash = PL_new_atom("hash");
  }
}

#define unify_clause_index(t, ci) LDFUNC(unify_clause_index, t, ci)

static bool
unify_clause_index(DECL_LD term_t t, ClauseIndex ci)
{ term_t values, pos, tmp;

  if ( !(values=PL_new_term_refs(NUM_INDEX_KEYS)) ||
       !(tmp=PL_new_term_ref()) )
    return false;

  if ( !put_args(values+0, ci->args) )
    return false;

  pos = values+1;
  if ( !PL_put_nil(pos) )
    return false;

  iarg_t *ap = ci->position;
  while(*ap != END_INDEX_POS)
    ap++;
  for(--ap; ap >= ci->position; ap--)
  { if ( !PL_put_integer(tmp, (*ap)+1) ||
	 !PL_cons_list(pos, tmp, pos) )
      return false;
  }

  if ( !PL_unify_int64(values+2, ci->buckets) ||
       !PL_unify_float(values+3, ci->speedup) ||
       !PL_unify_bool(values+4,  ci->is_list) ||
       !PL_unify_int64(values+5, sizeofClauseIndex(ci)) ||
       !PL_unify_bool(values+6,  ci->entries != NULL) ||
       !PL_unify_int64(values+7, collisionCount(ci)) )
    return false;

  init_index_keys();

  bool rc = ( PL_put_dict(tmp, i_tag_hash, NUM_INDEX_KEYS, i_index_keys,
			  values) &&
	      PL_unify(t, tmp) );

  PL_reset_term_refs(values);

  return rc;
}


#define add_deep_indexes(ci, head, tail) \
	LDFUNC(add_deep_indexes, ci, head, tail)

static bool
add_deep_indexes(DECL_LD ClauseIndex ci, term_t head, term_t tail)
{ size_t i;

  if ( !ci->entries )
    return true;

  for(i=0; i<ci->buckets; i++)
  { ClauseRef cref = ci->entries[i].head;

    for(; cref; cref = cref->next)
    { if ( isFunctor(cref->d.key) )
      { ClauseList cl = &cref->value.clauses;
	ClauseIndex *cip;

	if ( (cip = cl->clause_indexes) )
	{ for(; *cip; cip++)
	  { ClauseIndex ci = *cip;

	    if ( ISDEADCI(ci) )
	      continue;

	    if ( !PL_unify_list(tail, head, tail) ||
		 !unify_clause_index(head, ci) )
	      return false;
	    if ( ci->is_list &&
		 !add_deep_indexes(ci, head, tail) )
	      return false;
	  }
	}
      }
    }
  }

  return true;
}


bool
unify_index_pattern(Procedure proc, term_t value)
{ GET_LD
  Definition def = getProcDefinition(proc);
  ClauseIndex *cip;
  int rc = false;
  int found = 0;

  acquire_def(def);
  if ( (cip=def->impl.clauses.clause_indexes) )
  { term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    for(; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;

      found++;
      if ( !PL_unify_list(tail, head, tail) ||
	   !unify_clause_index(head, ci) )
	goto out;
      if ( ci->is_list )
      { if ( !add_deep_indexes(ci, head, tail) )
	  goto out;
      }
    }

    rc = found && PL_unify_nil(tail);
  }
out:
  release_def(def);

  return rc;
}

		 /*******************************
		 *         PROLOG FLAGS         *
		 *******************************/

typedef struct ci_flag
{ const char  *name;
  atom_t       symbol;
  unsigned int type;
  union {
    float *f;
    int   *i;
  } ptr;
} ci_flag;

#define CI_FFLAG(conf) \
  { .name = "ci_" #conf, .type = FT_FLOAT, .ptr.f = &GD->clause_index.conf }
#define CI_IFLAG(conf) \
  { .name = "ci_" #conf, .type = FT_INTEGER, .ptr.i = &GD->clause_index.conf }

static ci_flag ciflags[] =
{ CI_FFLAG(min_speedup),
  CI_FFLAG(max_var_fraction),
  CI_FFLAG(min_speedup_ratio),
  CI_IFLAG(max_lookahead),
  CI_IFLAG(min_clauses),
  { .name = 0 }
};

bool
ci_is_flag(atom_t key)
{ for(ci_flag *f = ciflags; f->name; f++)
  { if ( !f->symbol )
      f->symbol = PL_new_atom(f->name);
    if ( f->symbol == key )
      return true;
  }

  return false;
}

bool
ci_set_flag(DECL_LD term_t t, atom_t key)
{ for(ci_flag *f = ciflags; f->name; f++)
  { if ( !f->symbol )
      f->symbol = PL_new_atom(f->name);
    if ( f->symbol == key )
    { if ( f->type == FT_FLOAT )
      { double d;
	if ( PL_get_float_ex(t, &d) )
	{ *f->ptr.f = d;
	  return true;
	}
	return false;
      } else
      { int i;
	if ( PL_get_integer_ex(t, &i) )
	{ *f->ptr.i = i;
	  return true;
	}
	return false;
      }
    }
  }

  assert(0);
  return false;
}

bool
ci_get_flag(DECL_LD term_t t, atom_t key)
{ for(ci_flag *f = ciflags; f->name; f++)
  { if ( !f->symbol )
      f->symbol = PL_new_atom(f->name);
    if ( f->symbol == key )
    { if ( f->type == FT_FLOAT )
	return PL_unify_float(t, *f->ptr.f);
      else
	return PL_unify_integer(t, *f->ptr.i);
    }
  }
  assert(0);
  return false;
}

		 /*******************************
		 *             INIT             *
		 *******************************/

#define CI_CONF(name) GD->clause_index.name

void
initClauseIndexing(void)
{ i_tag_arg_info = 0;
  i_tag_hash = 0;

  CI_CONF(min_speedup)       = 1.5f;
  CI_CONF(max_var_fraction)  = 0.1f;
  CI_CONF(min_speedup_ratio) = 3.0f;
  CI_CONF(max_lookahead)     = 100;
  CI_CONF(min_clauses)       = 10;

  for(ci_flag *f = ciflags; f->name; f++)
  { f->symbol = 0;		/* allow restarting */
    if ( f->type == FT_FLOAT )
      setPrologFlag(f->name, f->type, *f->ptr.f);
    else
      setPrologFlag(f->name, f->type, *f->ptr.i);
  }
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define META PL_FA_TRANSPARENT

BeginPredDefs(index)
  PRED_DEF("$candidate_indexes",     3, candidate_indexes,     META)
  PRED_DEF("$set_candidate_indexes", 2, set_candidate_indexes, META)
EndPredDefs
