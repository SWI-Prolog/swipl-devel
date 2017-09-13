/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
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
#include "pl-rsort.h"
#include <math.h>

		 /*******************************
		 *	     PARAMETERS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  - MAXSEARCH
  Maximum number of clauses we look ahead on indexed clauses for an
  alternative clause. If the choice is committed this is lost effort, if
  it reaches the end of the clause list without finding one the call is
  deterministic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAXSEARCH      100
#define MIN_SPEEDUP    1.5


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define DEAD_INDEX   ((ClauseIndex)1)
#define ISDEADCI(ci) ((ci) == DEAD_INDEX)

typedef struct hash_hints
{ unsigned short args[MAX_MULTI_INDEX];	/* Hash these arguments */
  float		speedup;		/* Expected speedup */
  unsigned int	ln_buckets;		/* Lg2 of #buckets to use */
  unsigned	list : 1;		/* Use a list per key */
} hash_hints;

static int		bestHash(Word av, Definition def, ClauseIndex ci,
				 hash_hints *hints ARG_LD);
static ClauseIndex	hashDefinition(Definition def, hash_hints *h);
static void		replaceIndex(Definition def,
				     ClauseIndex *cip, ClauseIndex ci);
static void		deleteIndexP(Definition def, ClauseIndex *cip);
static void		deleteIndex(Definition def, ClauseIndex ci);
static void		insertIndex(Definition def, ClauseIndex ci);
static void		setClauseChoice(ClauseChoice chp, ClauseRef cref,
					gen_t generation ARG_LD);


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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline int
hashIndex(word key, int buckets)
{ word k = key >> LMASK_BITS;

  return (int)((key^k) & (buckets-1));
}


static inline word
indexOfWord(word w ARG_LD)
{ for(;;)
  { switch(tag(w))
    { case TAG_VAR:
      case TAG_ATTVAR:
	return 0;
      case TAG_ATOM:
	break;				/* atom_t */
      case TAG_INTEGER:
	if ( storage(w) == STG_INLINE )
	  break;
      /*FALLTHROUGH*/
      case TAG_STRING:
      case TAG_FLOAT:
      { Word p = addressIndirect(w);
	size_t n = wsizeofInd(*p);
	word k;

	k = MurmurHashAligned2(p+1, n*sizeof(*p), MURMUR_SEED);
	if ( !k ) k = 1;
	return k;
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


word
getIndexOfTerm(term_t t)
{ GET_LD
  word w = *valTermRef(t);

  return indexOfWord(w PASS_LD);
}


static inline ClauseRef
nextClauseArg1(ClauseChoice chp, gen_t generation ARG_LD)
{ ClauseRef cref = chp->cref;
  word key = chp->key;

  for( ; cref; cref = cref->next)
  { if ( (!cref->d.key || key == cref->d.key) &&
	 visibleClauseCNT(cref->value.clause, generation))
    { ClauseRef result = cref;
      int maxsearch = MAXSEARCH;

      for( cref = cref->next; cref; cref = cref->next )
      { if ( ((!cref->d.key || key == cref->d.key) &&
	      visibleClauseCNT(cref->value.clause, generation)) ||
	     --maxsearch == 0 )
	{ setClauseChoice(chp, cref, generation PASS_LD);

	  return result;
	}
      }
      chp->cref = NULL;

      return result;
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

TBD: Keep a flag telling whether there are non-indexable clauses.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ClauseRef
nextClauseFromBucket(ClauseChoice chp, gen_t generation, int is_list ARG_LD)
{ ClauseRef cref;
  word key = chp->key;

  if ( is_list )
  { DEBUG(MSG_INDEX_FIND, Sdprintf("Searching for %s\n", keyName(key)));

  non_indexed:
    for(cref = chp->cref; cref; cref = cref->next)
    { if ( cref->d.key == key )
      { ClauseList cl = &cref->value.clauses;
	ClauseRef cr;

	for(cr=cl->first_clause; cr; cr=cr->next)
	{ if ( visibleClauseCNT(cr->value.clause, generation) )
	  { setClauseChoice(chp, cr->next, generation PASS_LD);
	    return cr;
	  }
	}

	return NULL;
      }
    }

    if ( key )
    { key = 0;
      DEBUG(MSG_INDEX_FIND, Sdprintf("Not found; trying non-indexed\n"));
      goto non_indexed;
    } else
    { DEBUG(MSG_INDEX_FIND, Sdprintf("Not found\n"));
    }

    return NULL;
  }

  for(cref = chp->cref; cref; cref = cref->next)
  { if ( (!cref->d.key || key == cref->d.key) &&
	 visibleClauseCNT(cref->value.clause, generation))
    { ClauseRef result = cref;
      int maxsearch = MAXSEARCH;

      for( cref = cref->next; cref; cref = cref->next )
      { if ( ((!cref->d.key || key == cref->d.key) &&
	      visibleClauseCNT(cref->value.clause, generation)) ||
	     --maxsearch == 0 )
	{ setClauseChoice(chp, cref, generation PASS_LD);

	  return result;
	}
      }
      chp->cref = NULL;

      return result;
    }
  }

  return NULL;
}

/* Make sure the ClauseChoice contains a pointer to a clause that
   is visible in generation.  This garantees that the clause will
   not be destroyed.
*/

static void
setClauseChoice(ClauseChoice chp, ClauseRef cref, gen_t generation ARG_LD)
{ while ( cref && !visibleClauseCNT(cref->value.clause, generation) )
    cref = cref->next;
  chp->cref = cref;
}


static inline word
murmur_key(void *ptr, size_t n)
{ word k = MurmurHashAligned2(ptr, n, MURMUR_SEED);
  if ( !k ) k = 1;
  return k;
}


static inline word
indexKeyFromArgv(ClauseIndex ci, Word argv ARG_LD)
{ if ( likely(ci->args[1] == 0) )
  { return indexOfWord(argv[ci->args[0]-1] PASS_LD);
  } else
  { word key[MAX_MULTI_INDEX];
    int  harg;

    for(harg=0; ci->args[harg]; harg++)
    { if ( !(key[harg] = indexOfWord(argv[ci->args[harg]-1] PASS_LD)) )
	return 0;
    }

    return murmur_key(key, sizeof(word)*harg);
  }
}


#if defined(O_DEBUG) || defined(O_MAINTENANCE)
static char *
iargsName(const unsigned short args[MAX_MULTI_INDEX], char *buf)
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
firstClause() finds the first applicable   clause  and leave information
for finding the next clause in chp.

TBD:
  - non-indexable predicates must use a different supervisor
  - Predicates needing reindexing should use a different supervisor
  - When to select best table?
  - When to ignore the best and try again?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ClauseRef
first_clause_guarded(Word argv, LocalFrame fr,
		     Definition def, ClauseChoice chp ARG_LD)
{ ClauseRef cref;
  ClauseIndex *cip;
  hash_hints hints;
  gen_t generation = generationFrame(fr);

  if ( def->functor->arity == 0 )
    goto simple;			/* TBD: alt supervisor */

  if ( (cip=def->impl.clauses.clause_indexes) )
  { ClauseIndex best_index = NULL;

    for(; *cip; cip++)
    { ClauseIndex ci = *cip;
      word k;

      if ( ISDEADCI(ci) )
	continue;

      if ( (k=indexKeyFromArgv(ci, argv PASS_LD)) )
      { best_index = ci;
	chp->key = k;
	break;
      }
    }

    if ( best_index )
    { int hi;

      if ( def->impl.clauses.number_of_clauses > 10 &&
	   (float)def->impl.clauses.number_of_clauses/best_index->speedup > 10 &&
	   !LD->gen_reload )
      { DEBUG(MSG_JIT,
	      Sdprintf("Poor index %s of %s (trying to find better)\n",
		       iargsName(best_index->args, NULL), predicateName(def)));

	if ( bestHash(argv, def, best_index, &hints PASS_LD) )
	{ ClauseIndex ci;

	  DEBUG(MSG_JIT, Sdprintf("Found better at args %s\n",
				  iargsName(hints.args, NULL)));

	  if ( (ci=hashDefinition(def, &hints)) )
	  { chp->key = indexKeyFromArgv(ci, argv PASS_LD);
	    assert(chp->key);
	    best_index = ci;
	  }
	}
      }

      hi = hashIndex(chp->key, best_index->buckets);
      chp->cref = best_index->entries[hi].head;
      return nextClauseFromBucket(chp, generation,
				  best_index->is_list PASS_LD);
    }
  }

  if ( def->impl.clauses.number_of_clauses == 0 )
    return NULL;

  if ( (chp->key = indexOfWord(argv[0] PASS_LD)) &&
       (def->impl.clauses.number_of_clauses <= 10 || LD->gen_reload) )
  { chp->cref = def->impl.clauses.first_clause;
    return nextClauseArg1(chp, generation PASS_LD);
  }

  if ( !LD->gen_reload && bestHash(argv, def, NULL, &hints PASS_LD) )
  { ClauseIndex ci;

    if ( (ci=hashDefinition(def, &hints)) )
    { int hi;

      chp->key = indexKeyFromArgv(ci, argv PASS_LD);
      assert(chp->key);
      hi = hashIndex(chp->key, ci->buckets);
      chp->cref = ci->entries[hi].head;
      return nextClauseFromBucket(chp, generation, ci->is_list PASS_LD);
    }
  }

  if ( chp->key )
  { chp->cref = def->impl.clauses.first_clause;
    return nextClauseArg1(chp, generation PASS_LD);
  }

simple:
  for(cref = def->impl.clauses.first_clause; cref; cref = cref->next)
  { if ( visibleClauseCNT(cref->value.clause, generation) )
    { chp->key = 0;
      setClauseChoice(chp, cref->next, generation PASS_LD);
      break;
    }
  }

  return cref;
}


ClauseRef
firstClause(Word argv, LocalFrame fr, Definition def, ClauseChoice chp ARG_LD)
{ ClauseRef cref;

  acquire_def(def);
  cref = first_clause_guarded(argv, fr, def, chp PASS_LD);
  DEBUG(CHK_SECURE, assert(!cref || !chp->cref ||
			   visibleClause(chp->cref->value.clause,
					 generationFrame(fr))));
  release_def();

  return cref;
}


ClauseRef
nextClause__LD(ClauseChoice chp, Word argv, LocalFrame fr, Definition def ARG_LD)
{ gen_t generation = generationFrame(fr);
  ClauseRef cref;

  (void)argv;				/* we want to use these later */
  (void)def;				/* to create secondary indexes */

  acquire_def(def);
  if ( !chp->key )			/* not indexed */
  { for(cref=chp->cref; cref; cref = cref->next)
    { if ( visibleClauseCNT(cref->value.clause, generation) )
      { setClauseChoice(chp, cref->next, generation PASS_LD);
	break;
      }
    }
  } else
  { cref = nextClauseArg1(chp, generation PASS_LD);
  }
  DEBUG(CHK_SECURE, assert(!cref || !chp->cref ||
			   visibleClause(chp->cref->value.clause,
					 generationFrame(fr))));
  release_def();

  return cref;
}


		 /*******************************
		 *	   HASH SUPPORT		*
		 *******************************/

static int
cmp_ushort(const void *p1, const void *p2)
{ const unsigned short *u1 = p1;
  const unsigned short *u2 = p2;

  return *u1 < *u2 ? -1 : *u1 > *u2 ? 1 : 0;
}

static void
canonicalHap(unsigned short *hap)
{ int i;

  for(i=0; i<MAX_MULTI_INDEX; i++)
  { if ( !hap[i] )
    { int j;

      for(j=i; j<MAX_MULTI_INDEX; j++)
	hap[j] = 0;
      break;
    }
  }

  qsort(hap, i, sizeof(*hap), cmp_ushort);
}


static ClauseIndex
newClauseIndexTable(unsigned short *hap, hash_hints *hints)
{ ClauseIndex ci = allocHeapOrHalt(sizeof(struct clause_index));
  unsigned int buckets;
  size_t bytes;

  buckets = 2<<hints->ln_buckets;
  bytes = sizeof(struct clause_bucket) * buckets;

  canonicalHap(hap);

  memset(ci, 0, sizeof(*ci));
  memcpy(ci->args, hap, sizeof(ci->args));
  ci->buckets = buckets;
  ci->is_list = hints->list;
  ci->speedup = hints->speedup;
  ci->entries = allocHeapOrHalt(bytes);

  memset(ci->entries, 0, bytes);

  return ci;
}


static void
freeClauseListRef(ClauseRef cref)
{ ClauseList cl = &cref->value.clauses;
  ClauseRef cr, next;

  for(cr=cl->first_clause; cr; cr=next)
  { next = cr->next;
    lingerClauseRef(cr);
  }

  freeHeap(cref, SIZEOF_CREF_LIST);
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
{ unallocClauseIndexTableEntries(ci);
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
addClauseList(ClauseRef cref, Clause clause, ClauseRef where)
{ ClauseList cl = &cref->value.clauses;
  ClauseRef cr = newClauseRef(clause, 0); /* TBD: key? */

  if ( cl->first_clause )
  { if ( where != CL_START )
    { cl->last_clause->next = cr;
      cl->last_clause = cr;
    } else
    { cr->next = cl->first_clause;
      cl->first_clause = cr;
    }
    cl->number_of_clauses++;
  } else
  { cl->first_clause = cl->last_clause = cr;
    cl->number_of_clauses = 1;
  }

  /* TBD: Add to sub-indexes */
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
addClauseBucket(ClauseBucket ch, Clause cl,
		word key, ClauseRef where, int is_list)
{ ClauseRef cr;

  if ( is_list )
  { ClauseRef cref;
    ClauseList vars = NULL;

    if ( key )
    { for(cref=ch->head; cref; cref=cref->next)
      { if ( cref->d.key == key )
	{ addClauseList(cref, cl, where);
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
	addClauseList(cref, cl, where);
      }
      if ( vars )
	return 0;
    }

    DEBUG(MSG_INDEX_UPDATE, Sdprintf("Adding new %s\n", keyName(key)));
    cr = newClauseListRef(key);
    if ( vars )				/* (**) */
    { for(cref=vars->first_clause; cref; cref=cref->next)
      { addClauseList(cr, cref->value.clause, CL_END);
	if ( true(cref->value.clause, CL_ERASED) )	/* or do not add? */
	{ cr->value.clauses.number_of_clauses--;
	  cr->value.clauses.erased_clauses++;
	}
	DEBUG(MSG_INDEX_UPDATE, Sdprintf("Preparing var to clause-list for %s\n",
					 keyName(key)));
      }
      if ( cr->value.clauses.erased_clauses )
	ch->dirty++;
    }
    addClauseList(cr, cl, where);
  } else
  { cr = newClauseRef(cl, key);
  }

  if ( !ch->tail )
  { ch->head = ch->tail = cr;
  } else
  { if ( where == CL_END )
    { ch->tail->next = cr;
      ch->tail = cr;
    } else if ( where == CL_START )
    { cr->next = ch->head;
      ch->head = cr;
    } else
    { ClauseRef pred_cref = cl->predicate->impl.clauses.first_clause;
      ClauseRef ci_cref = ch->head;
      ClauseRef ci_prev = NULL;
      for(; pred_cref; pred_cref=pred_cref->next)
      { if ( pred_cref == where || ci_prev == ch->tail )
        { if ( ci_cref == ch->head )
          { cr->next = ch->head;
            ch->head = cr;
          } else if ( ci_prev == ch->tail )
          { ch->tail->next = cr;
            ch->tail = cr;
          } else
          { cr->next = ci_cref;
            ci_prev->next = cr;
          }
          break;
        }
        if ( is_list )
	  assert(0); /* TODO: get working with deep-indexing */
        if ( pred_cref->value.clause == ci_cref->value.clause )
        { ci_prev = ci_cref;
          ci_cref = ci_cref->next;
        }
      }
    }
  }

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


static void
gcClauseList(ClauseList cl)
{ ClauseRef cref=cl->first_clause, prev = NULL;

  while(cref && cl->erased_clauses)
  { if ( true(cref->value.clause, CL_ERASED) )
    { ClauseRef c = cref;

      cl->erased_clauses--;

      cref = cref->next;
      if ( !prev )
      { cl->first_clause = c->next;
	if ( !c->next )
	  cl->last_clause = NULL;
      } else
      { prev->next = c->next;
	if ( c->next == NULL)
	  cl->last_clause = prev;
      }

      lingerClauseRef(c);
    } else
    { prev = cref;
      cref = cref->next;
    }
  }

  DEBUG(CHK_SECURE,
	{ for(cref=cl->first_clause; cref; cref=cref->next)
	  { assert(false(cref->value.clause, CL_ERASED));
	  }
	});

  assert(cl->erased_clauses==0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gcClauseBucket() removes all erased clauses from  the bucket and returns
the number of indexable entries that have been removed from the bucket.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
gcClauseBucket(Definition def, ClauseBucket ch,
	       unsigned int dirty, int is_list, gen_t active)
{ ClauseRef cref = ch->head, prev = NULL;
  int deleted = 0;

  while( cref && dirty )
  { if ( is_list )
    { ClauseList cl = &cref->value.clauses;

      if ( cl->erased_clauses )
      { gcClauseList(cl);
	dirty--;

	if ( cl->first_clause == NULL )
	  goto delete;
      }
    } else
    { Clause cl = cref->value.clause;

      if ( true(cl, CL_ERASED) && cl->generation.erased < active )
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
	  freeClauseListRef(c);
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
	      assert( false(cl, CL_ERASED) ||
		      cl->generation.erased >= active
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
cleanClauseIndex(Definition def, ClauseIndex ci, gen_t active)
{ if ( def->impl.clauses.number_of_clauses < ci->resize_below )
  { deleteIndex(def, ci);
  } else
  { if ( ci->dirty )
    { ClauseBucket ch = ci->entries;
      int n = ci->buckets;

      for(; n; n--, ch++)
      { if ( ch->dirty )
	{ ci->size -= gcClauseBucket(def, ch, ch->dirty, ci->is_list, active);
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
cleanClauseIndexes(Definition def, gen_t active)
{ ClauseIndex *cip;

  if ( (cip=def->impl.clauses.clause_indexes) )
  { for(; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;
      cleanClauseIndex(def, ci, active);
    }
  }
}


void
clearTriedIndexes(Definition def)
{ int arity = def->functor->arity;
  int i;

  for(i=0; i<arity; i++)
  { arg_info *ainfo = &def->args[i];

    ainfo->assessed = FALSE;
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

  return 1<<MSB(nc) == nc;
}


static void
reconsider_index(Definition def)
{ if ( true(def, P_DYNAMIC) )
  { if ( has_pow2_clauses(def) )
    { if ( true(def, P_SHRUNKPOW2) )
      { clear(def, P_SHRUNKPOW2);
      } else
      { clearTriedIndexes(def);
      }
    }
  }
}


static void
shrunkpow2(Definition def)
{ if ( true(def, P_DYNAMIC) )
  { if ( false(def, P_SHRUNKPOW2) && has_pow2_clauses(def) )
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
	  { if ( true(cr->value.clause, CL_ERASED) )
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
indexKeyFromClause(ClauseIndex ci, Clause cl)
{ Code PC = cl->codes;

  if ( likely(ci->args[1] == 0) )
  { int arg = ci->args[0] - 1;
    word key;

    if ( arg > 0 )
      PC = skipArgs(PC, arg);
    if ( argKey(PC, 0, &key) )
      return key;
    return 0;
  } else
  { word key[MAX_MULTI_INDEX];			/* TBD: special case for 1 arg */
    int  pcarg = 1;
    int  harg;

    for(harg=0; ci->args[harg]; harg++)
    { if ( ci->args[harg] > pcarg )
	PC = skipArgs(PC, ci->args[harg]-pcarg);
      pcarg = ci->args[harg];
      if ( !argKey(PC, 0, &key[harg]) )
	return 0;
    }

    return murmur_key(key, sizeof(word)*harg);
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
{ word key = indexKeyFromClause(ci, cl);

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

      if ( ISDEADCI(ci) )
	continue;

      if ( true(def, P_DYNAMIC) )
      { if ( def->impl.clauses.number_of_clauses < ci->resize_below )
	{ DEBUG(MSG_JIT_DELINDEX,
		Sdprintf("Deleted index %d from %s (shrunk too much)\n",
			 (int)ci->args[0], predicateName(def)));
	  deleteIndexP(def, cip);
	} else
	{ deleteActiveClauseFromIndex(ci, cl);
	}
      } else
      { deleteIndexP(def, cip);
      }
    }
  }
}



void
deleteIndexes(Definition def, int isnew)
{ ClauseIndex *cip;

  assert(isnew);			/* TBD for non-new */

  if ( (cip=def->impl.clauses.clause_indexes) )
  { for(; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;

      unallocClauseIndexTable(ci);
    }

    def->impl.clauses.clause_indexes = NULL;
  }
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add a clause to an index.  If   the  clause cannot be indexed (typically
because it has a variable at the  argument location), the clause must be
added to all indexes.

ClauseIndex->size maintains the number of elements  in the list that are
indexed. This is needed for resizing the index.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
addClauseToIndex(ClauseIndex ci, Clause cl, ClauseRef where)
{ ClauseBucket ch = ci->entries;
  word key = indexKeyFromClause(ci, cl);

  if ( key == 0 )			/* a non-indexable field */
  { int n = ci->buckets;

    for(; n; n--, ch++)
      addClauseBucket(ch, cl, key, where, ci->is_list);
  } else
  { int hi = hashIndex(key, ci->buckets);

    DEBUG(MSG_INDEX_UPDATE, Sdprintf("Storing in bucket %d\n", hi));
    ci->size += addClauseBucket(&ch[hi], cl, key, where, ci->is_list);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addClauseToIndexes() is called (only) by   assertProcedure(),  which has
the definition locked.

We currently fail if the clause was not   added at the end or the start.
This may be the case while  reconsulting.   In  this  case we delete the
index after completion of the reconsult.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
addClauseToIndexes(Definition def, Clause cl, ClauseRef where)
{ ClauseIndex *cip;
  int rc = TRUE;

  if ( (cip=def->impl.clauses.clause_indexes) )
  { for(; *cip; cip++)
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;

      if ( ci->size >= ci->resize_above )
	deleteIndexP(def, cip);
      else
	addClauseToIndex(ci, cl, where);
    }
  }

  reconsider_index(def);

  DEBUG(CHK_SECURE, checkDefinition(def));
  return rc;
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
    key = indexKeyFromClause(ci, cl);

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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a hash-index on def  for  arg.   We  compute  the  hash unlocked,
checking at the end that nobody  messed   with  the clause list. If that
happened anyway, we retry. At the end,   we  lock the definition and add
the new index to the indexes of the predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ClauseIndex
hashDefinition(Definition def, hash_hints *hints)
{ GET_LD
  ClauseRef cref;
  ClauseIndex ci;
  ClauseIndex *cip;

  DEBUG(MSG_JIT, Sdprintf("hashDefinition(%s, %s, %d) (%s)\n",
			  predicateName(def),
			  iargsName(hints->args, NULL), 2<<hints->ln_buckets,
			  hints->list ? "lists" : "clauses"));

  for(;;)
  { ClauseRef first, last;

    ci = newClauseIndexTable(hints->args, hints);

    acquire_def(def);
    first = def->impl.clauses.first_clause;
    last  = def->impl.clauses.last_clause;

    for(cref = first; cref; cref = cref->next)
    { if ( false(cref->value.clause, CL_ERASED) )
	addClauseToIndex(ci, cref->value.clause, CL_END);
    }
    release_def(def);

    LOCKDEF(def);
    if ( first == def->impl.clauses.first_clause &&
	 last  == def->impl.clauses.last_clause )
      break;
    UNLOCKDEF(def);
    unallocClauseIndexTable(ci);
  }

  ci->resize_above = ci->size*2;
  ci->resize_below = ci->size/4;

  if ( (cip=def->impl.clauses.clause_indexes) )
  { for(; *cip; cip++)
    { ClauseIndex cio = *cip;

      if ( ISDEADCI(cio) )
	continue;

      if ( memcmp(cio->args, hints->args, sizeof(ci->args)) == 0 )
      { replaceIndex(def, cip, ci);
	goto out;
      }
    }
  }

  insertIndex(def, ci);

out:
  UNLOCKDEF(def);

  return ci;
}


static ClauseIndex *
copyIndex(ClauseIndex *org, int extra)
{ ClauseIndex *ncip;
  int size = extra;

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
    int i;

    ncipo = ncip = allocHeapOrHalt((size+1)*sizeof(*ncip));
    if ( org )
    { ClauseIndex *cip;

      for(cip=org; *cip; cip++)
      { ClauseIndex ci = *cip;
	if ( !ISDEADCI(ci) )
	  *ncipo++ = ci;
      }
    }
    for(i=0; i<extra; i++)
      *ncipo++ = DEAD_INDEX;
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
	return FALSE;
      speedup = ci->speedup;
    }
  }

  return TRUE;
}


static void
unalloc_index_array(void *p)
{ freeHeap(p, 0);
}

static void
setIndexes(Definition def, ClauseIndex *cip)
{ ClauseIndex *cipo = def->impl.clauses.clause_indexes;

  def->impl.clauses.clause_indexes = cip;
  if ( cipo )
    linger(&def->lingering, unalloc_index_array, cipo);
}


/* Caller must have the predicate locked */

static void
unalloc_ci(void *p)
{ unallocClauseIndexTable(p);
}

static void				/* definition must be locked */
replaceIndex(Definition def, ClauseIndex *cip, ClauseIndex ci)
{ ClauseIndex old = *cip;

  *cip = ci;
  DEBUG(MSG_JIT, Sdprintf("%d: replaceIndex(%s) %p-->%p\n",
			  PL_thread_self(),
			  predicateName(def),
			  old, ci));

  if ( !ISDEADCI(old) )
    linger(&def->lingering, unalloc_ci, old);

  if ( !isSortedIndexes(def->impl.clauses.clause_indexes) )
  { cip = copyIndex(def->impl.clauses.clause_indexes, 0);
    sortIndexes(cip);
    setIndexes(def, cip);
  }
}


static void
deleteIndexP(Definition def, ClauseIndex *cip)
{ replaceIndex(def, cip, DEAD_INDEX);
}


static void
deleteIndex(Definition def, ClauseIndex ci)
{ ClauseIndex *cip;

  if ( (cip=def->impl.clauses.clause_indexes) )
  { for(; *cip; cip++)
    { if ( *cip == ci )
      { deleteIndexP(def, cip);
	return;
      }
    }
  }

  assert(0);
}


static void
insertIndex(Definition def, ClauseIndex ci)
{ ClauseIndex *ocip;

  if ( (ocip=def->impl.clauses.clause_indexes) )
  { ClauseIndex *cip = ocip;
    ClauseIndex *ncip;
    int dead = 0;

    for(; *cip; cip++)
    { if ( ISDEADCI(*cip) )
      { *cip = ci;
	if ( isSortedIndexes(ocip) )
	  return;
	*cip = DEAD_INDEX;
	dead++;
      }
    }

    ncip = copyIndex(ocip, 1);
    for(cip=ncip; *cip; cip++)
    { if ( ISDEADCI(*cip) )
	*cip = ci;
    }
    sortIndexes(ncip);
    setIndexes(def, ncip);
  } else
  { ClauseIndex *cip = allocHeapOrHalt(2*sizeof(*cip));

    cip[0] = ci;
    cip[1] = NULL;
    def->impl.clauses.clause_indexes = cip;
  }
}


#if defined(O_MAINTENANCE) || defined(O_DEBUG)

		 /*******************************
		 *	     CHECKING		*
		 *******************************/

int
checkClauseIndexSizes(Definition def, int nindexable)
{ ClauseIndex *cip;
  int rc = TRUE;

  if ( (cip=def->impl.clauses.clause_indexes) )
  { for( ; *cip; cip++ )
    { ClauseIndex ci = *cip;

      if ( ISDEADCI(ci) )
	continue;

      if ( ci->size != nindexable )
      { Sdprintf("%s has inconsistent clause index->size",
		 predicateName(def));
	rc = FALSE;
      }
    }
  }

  return rc;
}


void
checkClauseIndexes(Definition def)
{ ClauseIndex *cip;

  if ( (cip=def->impl.clauses.clause_indexes)  )
  { GET_LD

    for(; *cip; cip++ )
    { ClauseIndex ci = *cip;
      unsigned int i;
      ClauseBucket cb;
      unsigned int ci_dirty = 0;	/* # dirty buckets */
      unsigned int ci_size = 0;		/* # indexable values in table */

      if ( ISDEADCI(ci) )
	continue;

      for(i=0,cb=ci->entries; i<ci->buckets; i++,cb++)
      { ClauseRef cref;
	unsigned int dirty = 0;

	acquire_def(def);
	for(cref=cb->head; cref; cref=cref->next)
	{ if ( cref->d.key )
	    ci_size++;

	  if ( ci->is_list )
	  { ClauseList cl = &cref->value.clauses;
	    ClauseRef cr;
	    unsigned int erased = 0;
	    unsigned int count = 0;

	    for(cr=cl->first_clause; cr; cr=cr->next)
	    { if ( true(cr->value.clause, CL_ERASED) )
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

	    if ( true(clause, CL_ERASED) )
	      dirty++;
	  }
	}
	release_def(def);

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

	      Sdprintf("  %p: [%2d] %8u-%10u%s%s\n",
		       clause,
		       clauseNo(def, clause, 0),
		       clause->generation.created,
		       clause->generation.erased,
		       true(clause, CL_ERASED) ? " erased" : "",
		       visibleClause(clause, gen) ? " v" : " X");
	    }
	  } else
	  { Clause clause = cref->value.clause;

	    Sdprintf("%p: [%2d] %8u-%10u%s%s%s\n",
		     clause,
		     clauseNo(def, clause, 0),
		     clause->generation.created,
		     clause->generation.erased,
		     true(clause, CL_ERASED) ? " erased" : "",
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
  size_t	count;
} key_asm;

typedef struct hash_assessment
{ unsigned short args[MAX_MULTI_INDEX]; /* arg for which to assess */
  size_t	allocated;		/* allocated size of array */
  size_t	size;			/* keys in array */
  size_t	var_count;		/* # non-indexable cases */
  size_t	funct_count;		/* # functor cases */
  float		stdev;			/* Standard deviation */
  float		speedup;		/* Expected speedup */
  unsigned	list;			/* Put lists in the buckets */
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
free_assessment_set(assessment_set *as)
{ if ( as->assessments != as->buf )
    free(as->assessments);
}

static hash_assessment *			/* TBD: resource error */
alloc_assessment(assessment_set *as, unsigned short *ia)
{ hash_assessment *a;

  if ( as->count >= as->allocated )
  { size_t newbytes = sizeof(*as->assessments)*2*as->allocated;

    if ( as->assessments == as->buf )
    { as->assessments = malloc(newbytes);
      memcpy(as->assessments, as->buf, sizeof(as->buf));
    } else
    { as->assessments = realloc(as->assessments, newbytes);
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
{ Definition def = ctx;
  const unsigned short *a1 = p1;
  const unsigned short *a2 = p2;
  const arg_info *i1 = &def->args[*a1];
  const arg_info *i2 = &def->args[*a2];


  return i1->speedup - i2->speedup > 0 ? -1 :
	 i1->speedup - i2->speedup < 0 ?  1 : 0;
}


static void
sort_assessments(Definition def,
		 unsigned short *instantiated, int ninstantiated)
{ sort_r(instantiated, ninstantiated, sizeof(*instantiated),
	 best_hash_assessment, def);
}


static int
compar_keys(const void *p1, const void *p2)
{ const key_asm *k1 = p1;
  const key_asm *k2 = p2;
  intptr_t d = (k1->key - k2->key);

  return d < 0 ? -1 : d > 0 ? 1 : 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Only the final call gets a clause_count > 0. Here we do the remainder of
the assessment. We could consider  for   a  seperate  function to merely
reduce the set.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
assess_remove_duplicates(hash_assessment *a, size_t clause_count)
{ key_asm *s = a->keys;
  key_asm *o = a->keys-1;
  key_asm *e = &s[a->size];
  word c = 0;				/* invalid key */
  size_t fc = 0;
  size_t i  = 0;
  float A=0.0, Q=0.0;

  a->speedup = 0.0;
  if ( !a->keys )
    return FALSE;

  qsort(a->keys, a->size, sizeof(key_asm), compar_keys);
  for( ; s<e; s++)
  { if ( s->key != c )
    { if ( i++ > 0 && clause_count )
      { float A0 = A;
	A = A+((float)o->count-A)/(float)(i-1);
	Q = Q+((float)o->count-A0)*((float)o->count-A);
      }
      c = s->key;
      if ( tagex(s->key) == (TAG_ATOM|STG_GLOBAL) )
	fc++;
      *++o = *s;
    } else
    { o->count += s->count;
    }
  }
  if ( i > 0 && clause_count )
  { float A0 = A;
    A = A+((float)o->count-A)/(float)i;
    Q = Q+((float)o->count-A0)*((float)o->count-A);
  }

  a->size        = i;
  a->funct_count = fc;

					/* assess quality */
  if ( clause_count )
  { a->stdev   = (float)sqrt(Q/(float)i);
    a->list    = FALSE;

    if ( a->size == 1 )			/* Single value that is not compound */
    {
#ifdef O_DEEP_INDEX
      if ( tagex(a->keys[0].key) != (TAG_ATOM|STG_GLOBAL) )
#endif
	return FALSE;
    }

    a->speedup =            (float)(clause_count*a->size) /
	      (float)(clause_count - a->var_count + a->var_count*a->size);
					/* punish bad distributions */
    a->speedup /= (float)1.0 + a->stdev*(float)a->size/(float)clause_count;

    a->space = ( a->size * sizeof(struct clause_bucket) +
		 clause_count * SIZEOF_CREF_CLAUSE +
		 a->size * a->var_count * SIZEOF_CREF_CLAUSE );

#ifdef O_DEEP_INDEX
    if ( clause_count/a->size > 10 ||
	 a->stdev > 3 )
    { a->list = TRUE;
      a->space += a->size * SIZEOF_CREF_LIST;
    }
#endif

    if ( (float)a->var_count/(float)a->size > 0.1 )
    { a->speedup = 0.0;
      return FALSE;			/* not indexable */
    }
  }

  return TRUE;
}


static int
assessAddKey(hash_assessment *a, word key)
{ if ( a->size < a->allocated )
  {
  put_key:
    a->keys[a->size].key   = key;
    a->keys[a->size].count = 1;
    a->size++;
  } else
  { if ( a->allocated == 0 )
    { a->allocated = 512;
      if ( !(a->keys = malloc(a->allocated*sizeof(*a->keys))) )
	return FALSE;
    } else
    { assess_remove_duplicates(a, 0);
      if ( a->size*2 > a->allocated )
      { key_asm *new = realloc(a->keys, a->allocated*2*sizeof(*a->keys));
	if ( !new )
	  return FALSE;
	a->keys = new;
	a->allocated *= 2;
      }
    }
    goto put_key;
  }

  return TRUE;
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

static void
assess_scan_clauses(Definition def,
		    hash_assessment *assessments, int assess_count)
{ hash_assessment *a;
  ClauseRef cref;
  int i;
  bit_vector *ai = alloca(sizeof_bitvector(def->functor->arity));
  int ac = 0;
  int kp[MAXARITY+1];				/* key-arg positions */
  int nk = 0;					/* number of key args */
  int *kpp;
  word keys[MAXARITY];

  init_bitvector(ai, def->functor->arity);
  for(i=0, a=assessments; i<assess_count; i++, a++)
  { int j;

    for(j=0; a->args[j]; j++)
    { if ( !true_bit(ai, a->args[j]-1) )
      { set_bit(ai, a->args[j]-1);
	ac++;
      }
    }
  }

  for(i=0; i<def->functor->arity; i++)
  { if ( true_bit(ai, i) )
      kp[nk++] = i;
  }
  kp[nk] = -1;

  for(cref=def->impl.clauses.first_clause; cref; cref=cref->next)
  { Clause cl = cref->value.clause;
    Code pc = cref->value.clause->codes;
    int carg = 0;

    if ( true(cl, CL_ERASED) )
      continue;

    for(kpp=kp; kpp[0] >= 0; kpp++)
    { if ( kpp[0] > carg )
	pc = skipArgs(pc, kpp[0]-carg);
      carg = kpp[0];
      argKey(pc, 0, &keys[kpp[0]]);
    }

    for(i=0, a=assessments; i<assess_count; i++, a++)
    { word key[MAX_MULTI_INDEX];
      int  harg;

      for(harg=0; a->args[harg]; harg++)
      { if ( !(key[harg] = keys[a->args[harg]-1]) )
	{ a->var_count++;
	  goto next_assessment;
	}
      }

      assessAddKey(a, murmur_key(key, sizeof(word)*harg));
    next_assessment:
      ;
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

@returns TRUE if a best hash was found.  Details on the best hash are in
*hints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
bestHash(Word av, Definition def, ClauseIndex ci, hash_hints *hints ARG_LD)
{ int i;
  int arity = (int)def->functor->arity;
  assessment_set aset;
  hash_assessment *a;
  int best = -1;
  float best_speedup = 0.0;
  unsigned short ia[MAX_MULTI_INDEX] = {0};
  unsigned short instantiated[arity];	/* GCC dynamic array */
  int ninstantiated = 0;

  init_assessment_set(&aset);

					/* Step 1: find instantiated args */
  for(i=0; i<arity; i++)
  { if ( indexOfWord(av[i] PASS_LD) )
      instantiated[ninstantiated++] = i;
  }
					/* Step 2: find non-yet assessed args */
  for(i=0; i<ninstantiated; i++)
  { int arg = instantiated[i];

    if ( !def->args[arg].assessed )
    { ia[0] = arg+1;
      a = alloc_assessment(&aset, ia);
    }
  }

  if ( aset.count )			/* Step 3: assess them */
  { assess_scan_clauses(def, aset.assessments, aset.count);

    for(i=0, a=aset.assessments; i<aset.count; i++, a++)
    { arg_info *ainfo = &def->args[a->args[0]-1];

      if ( assess_remove_duplicates(a, def->impl.clauses.number_of_clauses) )
      { DEBUG(MSG_JIT,
	      Sdprintf("Assess index %s of %s: speedup %f, stdev=%f\n",
		       iargsName(a->args, NULL), predicateName(def),
		       a->speedup, a->stdev));

	ainfo->speedup    = a->speedup;
	ainfo->ln_buckets = MSB(a->size);
      } else
      { ainfo->speedup    = 0.0;
	ainfo->ln_buckets = 0;

	DEBUG(MSG_JIT, Sdprintf("Assess index %s of %s: not indexable\n",
				iargsName(a->args, NULL), predicateName(def)));
      }

      ainfo->assessed = TRUE;

      if ( a->keys )
	free(a->keys);
    }

    free_assessment_set(&aset);
  }

					/* Step 4: find the best (single) arg */
  for(i=0; i<ninstantiated; i++)
  { int arg = instantiated[i];
    arg_info *ainfo = &def->args[arg];

    if ( ainfo->speedup > best_speedup )
    { best = arg;
      best_speedup = ainfo->speedup;
    }
  }

  if ( best >= 0 &&
       (float)def->impl.clauses.number_of_clauses/best_speedup > 3 )
  { int ok, m, n;

    sort_assessments(def, instantiated, ninstantiated);
    for( ok=0;
	 ok<ninstantiated && def->args[instantiated[ok]].speedup > MIN_SPEEDUP;
	 ok++ )
      ;

    if ( ok >= 2 && ++def->impl.clauses.jiti_tried <= arity )
    { hash_assessment *nbest;

      DEBUG(MSG_JIT, Sdprintf("%s: %zd clauses, index [%d]: speedup = %f"
			      "; %d promising arguments\n",
			      predicateName(def),
			      def->impl.clauses.number_of_clauses,
			      best+1, best_speedup, ok));

      init_assessment_set(&aset);
      for(m=1; m<ok; m++)
      { ia[1] = instantiated[m]+1;
	for(n=0; n<m; n++)
	{ ia[0] = instantiated[n]+1;
	  alloc_assessment(&aset, ia);
	}
      }

      assess_scan_clauses(def, aset.assessments, aset.count);
      nbest = best_assessment(aset.assessments, aset.count,
			      def->impl.clauses.number_of_clauses);
      if ( nbest && nbest->speedup > best_speedup*MIN_SPEEDUP )
      { DEBUG(MSG_JIT, Sdprintf("%s: using index %s, speedup = %f\n",
				predicateName(def), iargsName(nbest->args, NULL),
				nbest->speedup));
	memset(hints, 0, sizeof(*hints));
	memcpy(hints->args, nbest->args, sizeof(nbest->args));
	hints->ln_buckets = MSB(nbest->size);
	hints->speedup    = nbest->speedup;

	free_assessment_set(&aset);
	return TRUE;
      }
      free_assessment_set(&aset);
    }
  }

  if ( best >= 0 && (!ci || best_speedup > ci->speedup) )
  { arg_info *ainfo = &def->args[best];

    memset(hints, 0, sizeof(*hints));
    hints->args[0]    = best+1;
    hints->ln_buckets = ainfo->ln_buckets;
    hints->speedup    = ainfo->speedup;

    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *  PREDICATE PROPERTY SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Index info is

	[Arg1, Arg2, ...] - hash(Buckets, Speedup, IsList)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unify_clause_index(term_t t, ClauseIndex ci)
{ GET_LD
  term_t args = PL_new_term_ref();
  term_t tmp  = PL_new_term_ref();

  if ( (args=PL_new_term_ref()) &&
       (tmp =PL_new_term_ref()) )
  { int i;

    PL_put_nil(args);
    for(i=MAX_MULTI_INDEX-1; i>= 0; i--)
    { if ( ci->args[i] )
      { if ( !PL_put_integer(tmp, ci->args[i]) ||
	     !PL_cons_list(args, tmp, args) )
	  return FALSE;
      }
    }

    return PL_unify_term(t,
			 PL_FUNCTOR, FUNCTOR_minus2,
			   PL_TERM, args,
			   PL_FUNCTOR_CHARS, "hash", 3,
			     PL_INT, (int)ci->buckets,
			     PL_DOUBLE, (double)ci->speedup,
			     PL_BOOL, ci->is_list);
  }

  return FALSE;
}

bool
unify_index_pattern(Procedure proc, term_t value)
{ GET_LD
  Definition def = getProcDefinition__LD(proc->definition PASS_LD);
  ClauseIndex *cip;
  int rc = FALSE;
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
    }

    rc = found && PL_unify_nil(tail);
  }
out:
  release_def(def);

  return rc;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(index)
EndPredDefs
