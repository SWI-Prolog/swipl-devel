/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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

#include "pl-incl.h"
#include <math.h>

typedef struct hash_hints
{ unsigned int	buckets;		/* # buckets to use */
  float		speedup;		/* Expected speedup */
  unsigned	list : 1;		/* Use a list per key */
} hash_hints;

static int		bestHash(Word av, Definition def,
				 float minbest, struct bit_vector *tried,
				 hash_hints *hints);
static ClauseIndex	hashDefinition(Definition def, int arg, hash_hints *h);
static void		replaceIndex(Definition def,
				     ClauseIndex old, ClauseIndex ci);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Maximum number of clauses we  look  ahead   on  indexed  clauses  for an
alternative clause. If the choice is committed   this is lost effort, if
it reaches the end of the clause list   without  finding one the call is
deterministic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAXSEARCH 100

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
      case TAG_STRING:
	return 0L;
      case TAG_INTEGER:
	if ( storage(w) != STG_INLINE )
	{ Word p = valIndirectP(w);
	  word key;

#if SIZEOF_VOIDP == 4
          DEBUG(9, Sdprintf("Index for " INT64_FORMAT " = 0x%x\n",
			    valBignum(w), p[0]^p[1]));
	  key = p[0]^p[1];
#else
	  key = p[0];
#endif
	  if ( !key )
	    key++;
          return key;
	}
        /*FALLTHROUGH*/
      case TAG_ATOM:
	break;				/* atom_t */
      case TAG_FLOAT:
      { Word p = valIndirectP(w);
	word key;

	switch(WORDS_PER_DOUBLE)
	{ case 2:
	    key = p[0]^p[1];
	    break;
	  case 1:
	    key = p[0];
	    break;
	  default:
	    assert(0);
	    return 0L;
	}

	if ( !key )
	  key++;
	return key;
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
nextClauseArg1(ClauseChoice chp, gen_t generation)
{ ClauseRef cref = chp->cref;
  word key = chp->key;

  for( ; cref; cref = cref->next)
  { if ( (!cref->key || key == cref->key) &&
	 visibleClause(cref->value.clause, generation))
    { ClauseRef result = cref;
      int maxsearch = MAXSEARCH;

      for( cref = cref->next; cref; cref = cref->next )
      { if ( ((!cref->key || key == cref->key) &&
	      visibleClause(cref->value.clause, generation)) ||
	     --maxsearch == 0 )
	{ chp->cref = cref;

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

TBD: Keep a flag telling is whether there are non-indexable clauses.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ClauseRef
nextClauseFromBucket(ClauseChoice chp, gen_t generation, int is_list)
{ ClauseRef cref;
  word key = chp->key;

  if ( is_list )
  { DEBUG(MSG_INDEX_FIND, Sdprintf("Searching for %s\n", keyName(key)));

  non_indexed:
    for(cref = chp->cref; cref; cref = cref->next)
    { if ( cref->key == key )
      { ClauseList cl = &cref->value.clauses;
	ClauseRef cr;

	for(cr=cl->first_clause; cr; cr=cr->next)
	{ if ( visibleClause(cr->value.clause, generation) )
	  { chp->cref = cr->next;
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
  { if ( (!cref->key || key == cref->key) &&
	 visibleClause(cref->value.clause, generation))
    { ClauseRef result = cref;
      int maxsearch = MAXSEARCH;

      for( cref = cref->next; cref; cref = cref->next )
      { if ( ((!cref->key || key == cref->key) &&
	      visibleClause(cref->value.clause, generation)) ||
	     --maxsearch == 0 )
	{ chp->cref = cref;

	  return result;
	}
      }
      chp->cref = NULL;

      return result;
    }
  }

  return NULL;
}


static inline word
indexKeyFromArgv(ClauseIndex ci, Word argv ARG_LD)
{ return indexOfWord(argv[ci->args[0]-1] PASS_LD);
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

ClauseRef
firstClause(Word argv, LocalFrame fr, Definition def, ClauseChoice chp ARG_LD)
{ ClauseRef cref;
  ClauseIndex ci;
  hash_hints hints;
  int best;

  if ( def->functor->arity == 0 )
    goto simple;				/* TBD: alt supervisor */

  if ( def->impl.clauses.clause_indexes )
  { float speedup = 0.0;
    ClauseIndex best_index = NULL;

    for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
    { if ( ci->speedup > speedup )
      { word k;

	if ( (k=indexKeyFromArgv(ci, argv PASS_LD)) )
	{ chp->key = k;
	  speedup = ci->speedup;
	  best_index = ci;
	}
      }
    }

    if ( best_index )
    { int hi;

      if ( def->impl.clauses.number_of_clauses > 10 &&
	   def->impl.clauses.number_of_clauses/best_index->speedup > 10 )
      { DEBUG(MSG_JIT,
	      Sdprintf("Poor index in arg %d of %s (try to find better)\n",
		       best_index->args[0], predicateName(def)));

	if ( !best_index->tried_better )
	{ best_index->tried_better = new_bitvector(def->functor->arity);

	  for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
	    set_bit(best_index->tried_better, ci->args[0]-1);
	}

	if ( (best=bestHash(argv, def,
			    best_index->speedup, best_index->tried_better,
			    &hints)) >= 0 )
	{ DEBUG(MSG_JIT, Sdprintf("Found better at arg %d\n", best+1));

	  if ( (ci=hashDefinition(def, best+1, &hints)) )
	  { chp->key = indexKeyFromArgv(ci, argv PASS_LD);
	    assert(chp->key);
	    best_index = ci;
	  }
	}
      }

      hi = hashIndex(chp->key, best_index->buckets);
      chp->cref = best_index->entries[hi].head;
      return nextClauseFromBucket(chp, generationFrame(fr), best_index->is_list);
    }
  }

  if ( def->impl.clauses.number_of_clauses == 0 )
    return NULL;

  if ( (chp->key = indexOfWord(argv[0] PASS_LD)) &&
       def->impl.clauses.number_of_clauses <= 10 )
  { chp->cref = def->impl.clauses.first_clause;
    return nextClauseArg1(chp, generationFrame(fr));
  }


  if ( (best=bestHash(argv, def, 0.0, NULL, &hints)) >= 0 )
  { if ( (ci=hashDefinition(def, best+1, &hints)) )
    { int hi;

      chp->key = indexKeyFromArgv(ci, argv PASS_LD);
      assert(chp->key);
      hi = hashIndex(chp->key, ci->buckets);
      chp->cref = ci->entries[hi].head;
      return nextClauseFromBucket(chp, generationFrame(fr), ci->is_list);
    }
  }

  if ( chp->key )
  { chp->cref = def->impl.clauses.first_clause;
    return nextClauseArg1(chp, generationFrame(fr));
  }

simple:
  for(cref = def->impl.clauses.first_clause; cref; cref = cref->next)
  { if ( visibleClause(cref->value.clause, generationFrame(fr)) )
    { chp->cref = cref->next;
      chp->key = 0;
      return cref;
    }
  }

  return NULL;
}


ClauseRef
nextClause(ClauseChoice chp, Word argv, LocalFrame fr, Definition def)
{ (void)argv;				/* we want to use these later */
  (void)def;				/* to create secondary indexes */

  if ( !chp->key )			/* not indexed */
  { ClauseRef cref;

    for(cref=chp->cref; cref; cref = cref->next)
    { if ( visibleClause(cref->value.clause, generationFrame(fr)) )
      { chp->cref = cref->next;
	return cref;
      }
    }
    return NULL;
  } else
  { return nextClauseArg1(chp, generationFrame(fr));
  }
}


		 /*******************************
		 *	   HASH SUPPORT		*
		 *******************************/

static ClauseIndex
newClauseIndexTable(int arg, hash_hints *hints)
{ ClauseIndex ci = allocHeapOrHalt(sizeof(struct clause_index));
  unsigned int m = 4;
  size_t bytes;

  while(m<hints->buckets)
    m *= 2;
  hints->buckets = m;
  bytes = sizeof(struct clause_bucket) * hints->buckets;

  memset(ci, 0, sizeof(*ci));
  ci->args[0] = arg;
  ci->buckets = hints->buckets;
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
    freeClauseRef(cr);
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
	freeClauseRef(cr);
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
  cref->key = key;

  return cref;
}


static void
addClauseList(ClauseRef cref, Clause clause, int where)
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
addClauseBucket(ClauseBucket ch, Clause cl, word key, int where, int is_list)
{ ClauseRef cr;

  if ( is_list )
  { ClauseRef cref;
    ClauseList vars = NULL;

    if ( key )
    { for(cref=ch->head; cref; cref=cref->next)
      { if ( cref->key == key )
	{ addClauseList(cref, cl, where);
	  DEBUG(MSG_INDEX_UPDATE,
		Sdprintf("Adding to existing %s\n", keyName(key)));
	  return 0;
	} else if ( !cref->key )
	{ vars = &cref->value.clauses;
	}
      }
    } else
    { for(cref=ch->head; cref; cref=cref->next)
      { if ( !cref->key )
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
  { if ( where != CL_START )
    { ch->tail->next = cr;
      ch->tail = cr;
    } else
    { cr->next = ch->head;
      ch->head = cr;
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
      { if ( c->key == key )
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
	  if ( d->key )
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
	  freeClauseRef(c);
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

      freeClauseRef(c);
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
gcClauseBucket(ClauseBucket ch, unsigned int dirty, int is_list)
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
    { if ( true(cref->value.clause, CL_ERASED) )
      { ClauseRef c;

	dirty--;

      delete:
	c = cref;
	if ( cref->key )
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
	  freeClauseRef(c);

	continue;
      }
    }

    prev = cref;
    cref = cref->next;
  }

  DEBUG(CHK_SECURE,
	{ if ( !is_list )
	  { for(cref=ch->head; cref; cref=cref->next)
	    { assert(false(cref->value.clause, CL_ERASED));
	    }
	  }
	});

  ch->dirty = 0;

  return deleted;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See also deleteActiveClauseFromIndexes() comment
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
cleanClauseIndex(Definition def, ClauseIndex ci)
{ if ( ci->size - def->impl.clauses.erased_clauses < ci->resize_below )
  { replaceIndex(def, ci, NULL);
  } else
  { if ( ci->dirty )
    { ClauseBucket ch = ci->entries;
      int n = ci->buckets;

      for(; n; n--, ch++)
      { if ( ch->dirty )
	{ ci->size -= gcClauseBucket(ch, ch->dirty, ci->is_list);
	  if ( --ci->dirty == 0 )
	    break;
	}
      }
    }

    assert((int)ci->size >= 0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cleanClauseIndexes() is called from cleanDefinition(),   which is called
either locked or otherwise safe for  concurrency while the definition is
not referenced. This is the time that we can remove cells from the index
chains and can reclaim old indexes.

If we reclaim old  indexes,  there   apparently  have  been  significant
changes to the predicate  and  therefore   we  also  delete  the `tried'
bitvector to force reevaluation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
unallocOldClauseIndexes(Definition def)
{ if ( def->old_clause_indexes )
  { ClauseIndexList li = def->old_clause_indexes;
    ClauseIndexList next;

    def->old_clause_indexes = NULL;

    for(; li; li=next)
    { next = li->next;

      unallocClauseIndexTable(li->index);
      freeHeap(li, sizeof(*li));
    }

    if ( def->tried_index )
      clear_bitvector(def->tried_index);
  }
}


void
cleanClauseIndexes(Definition def)
{ ClauseIndex ci;

  for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
    cleanClauseIndex(def, ci);

  unallocOldClauseIndexes(def);
}


void
unallocClauseIndexes(Definition def)
{ ClauseIndex ci, next;

  for(ci=def->impl.clauses.clause_indexes; ci; ci=next)
  { next = ci->next;
    unallocClauseIndexTable(ci);
  }

  unallocOldClauseIndexes(def);
  if ( def->tried_index )
    free_bitvector(def->tried_index);
}


void
clearTriedIndexes(Definition def)
{ struct bit_vector *v;

  if ( (v=def->tried_index) )
    clear_bitvector(v);
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
  { struct bit_vector *tried;

    if ( (tried=def->tried_index) && has_pow2_clauses(def) )
    { if ( true(def, P_SHRUNKPOW2) )
      { clear(def, P_SHRUNKPOW2);
      } else
      { clear_bitvector(tried);
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
    { if ( cref->key == key )
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
{ word key;

  argKey(cl->codes, ci->args[0]-1, &key);
  return key;
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
removeClausesProcedure(), which is called when reloading a source file.

For dynamic predicates, the predicate is  locked. L_PREDICATE is held if
def is static.

Note that ci->size includes erased clauses. Maybe we should change that.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
deleteActiveClauseFromIndexes(Definition def, Clause cl)
{ ClauseIndex ci, next;

  shrunkpow2(def);

  for(ci=def->impl.clauses.clause_indexes; ci; ci=next)
  { next = ci->next;

    if ( true(def, P_DYNAMIC) )
    { if ( ci->size - def->impl.clauses.erased_clauses < ci->resize_below )
	replaceIndex(def, ci, NULL);
      else
	deleteActiveClauseFromIndex(ci, cl);
    } else
    { replaceIndex(def, ci, NULL);
    }
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
addClauseToIndex(ClauseIndex ci, Clause cl, int where)
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
addClauseToIndexes(Definition def, Clause cl, int where)
{ ClauseIndex ci, next;

  for(ci=def->impl.clauses.clause_indexes; ci; ci=next)
  { next = ci->next;

    if ( ci->size >= ci->resize_above )
      replaceIndex(def, ci, NULL);
    else
      addClauseToIndex(ci, cl, where);
  }

  reconsider_index(def);

  DEBUG(CHK_SECURE, checkDefinition(def));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called from unlinkClause(), which is called for retracting a clause from
a dynamic predicate which is not  referenced   and  has  few clauses. In
other cases, deleteActiveClauseFromIndex() is called.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
delClauseFromIndex(Definition def, Clause cl)
{ ClauseIndex ci;

  shrunkpow2(def);

  for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
  { ClauseBucket ch = ci->entries;
    word key = indexKeyFromClause(ci, cl);

    if ( key == 0 )			/* a non-indexable field */
    { int n = ci->buckets;

      for(; n; n--, ch++)
	ci->size -= deleteClauseBucket(ch, cl, key, ci->is_list);
    } else
    { int hi = hashIndex(key, ci->buckets);

      ci->size -= deleteClauseBucket(&ch[hi], cl, key, ci->is_list);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a hash-index on def for  arg.  It   is  ok  to do so unlocked for
static predicates, but if another thread  did   the  job, we discard our
result. For dynamic  or  multifile  predicates,   we  need  to  keep the
predicate locked while building the  hash-table   because  we  will miss
clauses that are added while building.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ClauseIndex
hashDefinition(Definition def, int arg, hash_hints *hints)
{ ClauseRef cref;
  ClauseIndex ci, old;
  ClauseIndex *cip;
  int dyn_or_multi;

  DEBUG(MSG_JIT, Sdprintf("hashDefinition(%s, %d, %d) (%s)\n",
			  predicateName(def), arg, hints->buckets,
			  hints->list ? "lists" : "clauses"));

  ci = newClauseIndexTable(arg, hints);

  if ( (dyn_or_multi=true(def, P_DYNAMIC|P_MULTIFILE)) )
    LOCKDEF(def);
  for(cref = def->impl.clauses.first_clause; cref; cref = cref->next)
  { if ( false(cref->value.clause, CL_ERASED) )
      addClauseToIndex(ci, cref->value.clause, CL_END);
  }
  ci->resize_above = ci->size*2;
  ci->resize_below = ci->size/4;

  if ( !dyn_or_multi )
    LOCKDEF(def);
  for(old=def->impl.clauses.clause_indexes; old; old=old->next)
  { if ( old->args[0] == arg )
      break;
  }

  if ( !old )				/* this is a new table */
  { ClauseIndex conc;

    for(conc=def->impl.clauses.clause_indexes; conc; conc=conc->next)
    { if ( conc->args[0] == arg )
      { UNLOCKDEF(def);
	unallocClauseIndexTable(ci);
	return conc;
      }
    }
					/* insert at the end */
    for(cip=&def->impl.clauses.clause_indexes; *cip; cip = &(*cip)->next)
      ;
    *cip = ci;
  } else				/* replace (resize) old */
  { replaceIndex(def, old, ci);
  }
  UNLOCKDEF(def);

  return ci;
}


/* Caller must have the predicate locked */

static void				/* definition must be locked */
replaceIndex(Definition def, ClauseIndex old, ClauseIndex ci)
{ ClauseIndex *cip;
  ClauseIndexList c = allocHeapOrHalt(sizeof(*c));

  for(cip=&def->impl.clauses.clause_indexes;
      *cip && *cip != old;
      cip = &(*cip)->next)
    ;

  DEBUG(MSG_JIT, Sdprintf("%d: replaceIndex(%s) %p-->%p\n",
			  PL_thread_self(),
			  predicateName(def),
			  old, ci));

  if ( ci )
  { ci->next = old->next;
    MemoryBarrier();			/* lock only synchronizes updates */
    *cip = ci;
  } else				/* this is a delete */
  { *cip = old->next;
  }

  c->index = old;
  c->next = def->old_clause_indexes;
  def->old_clause_indexes = c;
}


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

typedef struct key_asm
{ word		key;
  uintptr_t	count;
} key_asm;

typedef struct hash_assessment
{ int		arg;			/* arg for which to assess */
  size_t	allocated;		/* allocated size of array */
  size_t	size;			/* keys in array */
  size_t	var_count;		/* # non-indexable cases */
  size_t	funct_count;		/* # functor cases */
  float		stdev;			/* Standard deviation */
  float		speedup;		/* Expected speedup */
  int		list;			/* Put lists in the buckets */
  size_t	space;			/* Space indication */
  key_asm      *keys;			/* tmp key-set */
} hash_assessment;


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

  if ( !a->keys )
    return FALSE;

  qsort(a->keys, a->size, sizeof(key_asm), compar_keys);
  for( ; s<e; s++)
  { if ( s->key != c )
    { if ( i++ > 0 && clause_count )
      { float A0 = A;
	A = A+((float)o->count-A)/(float)i;
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
    a->speedup /= 1.0+a->stdev;			/* punish bad distributions */

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
      return FALSE;			/* not indexable */
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ASSESS_BUFSIZE 10
#define MIN_SPEEDUP 1.5

static int
bestHash(Word av, Definition def,
	 float minbest, struct bit_vector *tried,
	 hash_hints *hints)
{ GET_LD
  int i;
  ClauseRef cref;
  hash_assessment assess_buf[ASSESS_BUFSIZE];
  hash_assessment *assessments = assess_buf;
  int assess_allocated = ASSESS_BUFSIZE;
  int assess_count = 0;
  int clause_count = 0;
  hash_assessment *a;
  hash_assessment *best = NULL;		/* argument */
  int best_arg = -1;

  if ( !def->tried_index )
    def->tried_index = new_bitvector(def->functor->arity);

					/* Step 1: allocate assessments */
  for(i=0; i<(int)def->functor->arity; i++)
  { word k;

    if ( !true_bit(def->tried_index, i) &&	/* non-indexable */
	 !(tried && true_bit(tried, i)) &&	/* already tried, not better */
	 (k=indexOfWord(av[i] PASS_LD)) )
    { if ( assess_count	>= assess_allocated )
      { size_t newbytes = sizeof(*assessments)*2*assess_allocated;

	if ( assessments == assess_buf )
	{ assessments = malloc(newbytes);
	  memcpy(assessments, assess_buf, sizeof(assess_buf));
	} else
	{ assessments = realloc(assessments, newbytes);
	}
	assess_allocated *= 2;
      }
      a = &assessments[assess_count++];
      memset(a, 0, sizeof(*a));
      a->arg = i;
    }
  }

  if ( assess_count == 0 )
    return -1;				/* no luck */

					/* Step 2: assess */
  for(cref=def->impl.clauses.first_clause; cref; cref=cref->next)
  { Clause cl = cref->value.clause;
    Code pc = cref->value.clause->codes;
    int carg = 0;

    if ( true(cl, CL_ERASED) )
      continue;

    for(i=0, a=assessments; i<assess_count; i++, a++)
    { word k;

      if ( carg < a->arg )
      { pc = skipArgs(pc, a->arg-carg);
	carg = a->arg;
      }
      if ( argKey(pc, 0, &k) )
      { assessAddKey(a, k);
      } else
      { a->var_count++;
      }
    }

    clause_count++;
  }

  for(i=0, a=assessments; i<assess_count; i++, a++)
  {
    if ( assess_remove_duplicates(a, clause_count) )
    { DEBUG(MSG_JIT,
	    Sdprintf("Assess arg %d of %s: speedup %f, stdev=%f\n",
		     a->arg+1, predicateName(def), a->speedup, a->stdev));

      if ( a->speedup > minbest )
      { best = a;
	minbest = a->speedup;
      } else if ( tried )
      { set_bit(tried, a->arg);
      }
    } else
    { set_bit(def->tried_index, a->arg);
      DEBUG(MSG_JIT, Sdprintf("Assess arg %d of %s: not indexable\n",
			      a->arg+1, predicateName(def)));
    }

    if ( a->keys )
      free(a->keys);
  }

  if ( best )
  { best_arg       = best->arg;
    hints->buckets = (unsigned int)best->size;
    hints->speedup = best->speedup;
    hints->list    = best->list;
  }

  if ( assessments != assess_buf )
    free(assessments);

  return best_arg;
}


		 /*******************************
		 *  PREDICATE PROPERTY SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Index info is

	Arg - hash(Buckets, Speedup, IsList)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unify_clause_index(term_t t, ClauseIndex ci)
{ return PL_unify_term(t,
		       PL_FUNCTOR, FUNCTOR_minus2,
			 PL_INT, (int)ci->args[0],
			 PL_FUNCTOR_CHARS, "hash", 3,
			   PL_INT, (int)ci->buckets,
			   PL_DOUBLE, (double)ci->speedup,
		           PL_BOOL, ci->is_list);
}

bool
unify_index_pattern(Procedure proc, term_t value)
{ GET_LD
  Definition def = getProcDefinition__LD(proc->definition PASS_LD);
  ClauseIndex ci;

  if ( (ci=def->impl.clauses.clause_indexes) )
  { term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    for(; ci; ci=ci->next)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !unify_clause_index(head, ci) )
	return FALSE;
    }

    return PL_unify_nil(tail);
  }

  return FALSE;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(index)
EndPredDefs
