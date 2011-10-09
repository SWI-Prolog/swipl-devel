/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
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

static int		bestHash(Word av, Definition def,
				 int *buckets, float *best_speedup);
static ClauseIndex	hashDefinition(Definition def, int arg, int buckets);
static ClauseIndex	resizeHashDefinition(Definition def, ClauseIndex old);
static int		reassessHash(Definition def,
				     ClauseIndex ci, float *speedup);
static void		replaceIndex(Definition def,
				     ClauseIndex old, ClauseIndex ci);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Maximum number of clauses we  look  ahead   on  indexed  clauses  for an
alternative clause. If the choice is committed   this is lost effort, it
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
nextClauseArg1(ClauseChoice chp, uintptr_t generation)
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
nextClauseFromBucket(ClauseChoice chp, uintptr_t generation)
{ ClauseRef cref;
  word key = chp->key;

  if ( tagex(key) == (TAG_ATOM|STG_GLOBAL) )
  { DEBUG(1, Sdprintf("Searching for %s\n", functorName(key)));

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

    DEBUG(1, Sdprintf("%s not found; trying non-indexed\n", functorName(key)));
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
  int best, buckets;
  float speedup;

  if ( def->functor->arity == 0 )
    goto simple;

retry:
  if ( def->impl.clauses.clause_indexes )
  { float speedup = 0.0;
    ClauseIndex best_index = NULL;

    for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
    { if ( ci->speedup > speedup )
      { word k;

	if ( (k=indexOfWord(argv[ci->arg-1] PASS_LD)) )
	{ chp->key = k;
	  speedup = ci->speedup;
	  best_index = ci;
	}
      }
    }

    if ( best_index )
    { int hi;

      if ( best_index->size > best_index->resize_above )
      { if ( !(best_index = resizeHashDefinition(def, best_index)) )
	  goto retry;
      }

      hi = hashIndex(chp->key, best_index->buckets);
      chp->cref = best_index->entries[hi].head;
      return nextClauseFromBucket(chp, generationFrame(fr));
    }
  }

  if ( def->impl.clauses.number_of_clauses == 0 )
    return NULL;

  if ( (chp->key = indexOfWord(argv[0] PASS_LD)) &&
       def->impl.clauses.number_of_clauses <= 10 )
  { chp->cref = def->impl.clauses.first_clause;
    return nextClauseArg1(chp, generationFrame(fr));
  }

  if ( (best=bestHash(argv, def, &buckets, &speedup)) >= 0 )
  { if ( (ci=hashDefinition(def, best+1, buckets)) )
    { int hi;

      ci->speedup = speedup;
      chp->key = indexOfWord(argv[ci->arg-1] PASS_LD);
      assert(chp->key);
      hi = hashIndex(chp->key, ci->buckets);
      chp->cref = ci->entries[hi].head;
      return nextClauseFromBucket(chp, generationFrame(fr));
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
nextClause(ClauseChoice chp, Word argv,
	   LocalFrame fr, Definition def ARG_LD)
{ if ( !chp->key )			/* not indexed */
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
newClauseIndexTable(int arg, int buckets)
{ GET_LD
  ClauseIndex ci = allocHeapOrHalt(sizeof(struct clause_index));
  ClauseBucket ch;
  int m = 4;

  while(m<buckets)
    m *= 2;
  buckets = m;

  memset(ci, 0, sizeof(*ci));
  ci->buckets  = buckets;
  ci->arg      = arg;
  ci->entries  = allocHeapOrHalt(sizeof(struct clause_bucket) * buckets);

  for(ch = ci->entries; buckets; buckets--, ch++)
  { ch->head = ch->tail = NULL;
    ch->dirty = 0;
  }

  return ci;
}


static void
freeClauseRefOrList(ClauseRef cref ARG_LD)
{ if ( tagex(cref->key) == (TAG_ATOM|STG_GLOBAL) )
  { ClauseList cl = &cref->value.clauses;
    ClauseRef cr, next;

    for(cr=cl->first_clause; cr; cr=next)
    { next = cr->next;
      freeClauseRef(cr PASS_LD);
    }

    freeHeap(cref, SIZEOF_CREF_LIST);
  } else
  { freeClauseRef(cref PASS_LD);
  }
}


static void
unallocClauseIndexTableEntries(ClauseBucket entries, int buckets ARG_LD)
{ ClauseBucket ch;
  int i;

  for(ch=entries,i=buckets; --i>=0; ch++)
  { ClauseRef cr, next;

    for(cr = ch->head; cr; cr = next)
    { next = cr->next;
      freeClauseRefOrList(cr PASS_LD);
    }
  }

  freeHeap(entries, buckets * sizeof(struct clause_bucket));
}


void
unallocClauseIndexTable(ClauseIndex ci)
{ GET_LD

  unallocClauseIndexTableEntries(ci->entries, ci->buckets PASS_LD);
  freeHeap(ci, sizeof(struct clause_index));
}


static ClauseRef
newClauseListRef(word key ARG_LD)
{ ClauseRef cref = allocHeapOrHalt(SIZEOF_CREF_LIST);

  memset(cref, 0, sizeof(*cref));
  cref->key = key;

  return cref;
}


static void
addClauseList(ClauseRef cref, Clause clause, int where ARG_LD)
{ ClauseList cl = &cref->value.clauses;
  ClauseRef cr = newClauseRef(clause, 0 PASS_LD);	/* TBD: key? */

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

  /* TBD: Add to indexes */
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
addClauseBucket(ClauseBucket ch, Clause cl, word key, int where ARG_LD)
{ ClauseRef cr;

  if ( tagex(key) == (TAG_ATOM|STG_GLOBAL) )	/* functor */
  { ClauseRef cref;

    for(cref=ch->head; cref; cref=cref->next)
    { if ( cref->key == key )
      { addClauseList(cref, cl, where PASS_LD);
	DEBUG(1, Sdprintf("Adding to existing %s\n", functorName(key)));
	return 0;
      }
    }

    DEBUG(1, Sdprintf("Adding new %s\n", functorName(key)));
    cr = newClauseListRef(key PASS_LD);
    for(cref=ch->head; cref; cref=cref->next)	/* (**) */
    { if ( !cref->key )
      { addClauseList(cr, cref->value.clause, CL_END PASS_LD);
	DEBUG(1, Sdprintf("Preparing var to clause-list for %s\n",
			  functorName(key)));
      }
    }
    addClauseList(cr, cl, where PASS_LD);
  } else
  { if ( !key )
    { ClauseRef cref;				/* (*) */

      for(cref=ch->head; cref; cref=cref->next)
      { if ( tagex(cref->key) == (TAG_ATOM|STG_GLOBAL) )
	{ addClauseList(cref, cl, where PASS_LD);
	  DEBUG(1, Sdprintf("Adding var to clause-list for %s\n",
			    functorName(cref->key)));
	}
      }
    }

    cr = newClauseRef(cl, key PASS_LD);
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
deleteClauseList(ClauseRef cref, Clause clause)
{ ClauseList cl = &cref->value.clauses;
  ClauseRef cr, prev=NULL;

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


static void
deleteClauseBucket(ClauseBucket ch, Clause clause, word key)
{ ClauseRef prev = NULL;
  ClauseRef c;

  if ( tagex(key) == (TAG_ATOM|STG_GLOBAL) )
  { for(c = ch->head; c; prev = c, c = c->next)
    { if ( c->key == key )
      { deleteClauseList(c, clause);
	break;
      }
    }
  } else
  { for(c = ch->head; c; prev = c, c = c->next)
    { if ( tagex(c->key) == (TAG_ATOM|STG_GLOBAL) )
      { if ( !key )
	  deleteClauseList(c, clause);
      } else
      { if ( c->value.clause == clause )
	{ if ( !prev )
	  { ch->head = c->next;
	    if ( !c->next )
	      ch->tail = NULL;
	  } else
	  { prev->next = c->next;
	    if ( !c->next)
	      ch->tail = prev;
	  }
	  break;
	}
      }
    }
  }
}


static void
gcClauseList(ClauseList cl ARG_LD)
{ ClauseRef cref=cl->first_clause, prev = NULL;

  while(cref && cl->erased_clauses)
  { if ( true(cref->value.clause, ERASED) )
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

      freeClauseRef(c PASS_LD);
    } else
    { prev = cref;
      cref = cref->next;
    }
  }

#if O_SECURE
  for(cref=cl->first_clause; cref; cref=cref->next)
  { assert(false(cref->value.clause, ERASED));
  }
#endif

  assert(cl->erased_clauses==0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gcClauseBucket() removes all erased clauses from  the bucket and returns
the number of indexable entries that have been removed from the bucket.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
gcClauseBucket(ClauseBucket ch, unsigned int dirty ARG_LD)
{ ClauseRef cref = ch->head, prev = NULL;
  int deleted = 0;

  while( cref && dirty )
  { if ( tagex(cref->key) == (TAG_ATOM|STG_GLOBAL) )
    { ClauseList cl = &cref->value.clauses;

      if ( cl->erased_clauses )
      { gcClauseList(cl PASS_LD);
	dirty--;

	if ( cl->first_clause == NULL )
	  goto delete;
      }
    } else
    { if ( true(cref->value.clause, ERASED) )
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

	freeClauseRefOrList(c PASS_LD);
	continue;
      }
    }

    prev = cref;
    cref = cref->next;
  }

#if O_SECURE
  for(cref=ch->head; cref; cref=cref->next)
  { if ( tagex(cref->key) == (TAG_ATOM|STG_GLOBAL) )
    { ClauseList cl = &cref->value.clauses;
      ClauseRef cr;

      assert(cl->first_clause);
      assert(cl->erased_clauses==0);
      for(cr=cl->first_clause; cr; cr=cr->next)
      { assert(false(cr->value.clause, ERASED));
      }
    } else
    { assert(false(cref->value.clause, ERASED));
    }
  }
#endif

  ch->dirty = 0;

  return deleted;
}


static void
cleanClauseIndex(Definition def, ClauseIndex ci ARG_LD)
{ if ( ci->size < ci->resize_below )
  { replaceIndex(def, ci, NULL);
  } else
  { if ( ci->dirty )
    { ClauseBucket ch = ci->entries;
      int n = ci->buckets;

      for(; n; n--, ch++)
      { if ( ch->dirty )
	{ ci->size -= gcClauseBucket(ch, ch->dirty PASS_LD);
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

If the index is too large, it is  simply discarded. It will be recreated
when (and if) it is needed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
cleanClauseIndexes(Definition def ARG_LD)
{ ClauseIndex ci;

  for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
    cleanClauseIndex(def, ci PASS_LD);

  if ( def->old_clause_indexes )
  { ClauseIndexList li = def->old_clause_indexes;
    ClauseIndexList next;

    def->old_clause_indexes = NULL;

    for(; li; li=next)
    { next = li->next;

      unallocClauseIndexTable(li->index);
      freeHeap(li, sizeof(*li));
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
deleteActiveClauseFromBucket() maintains dirty  count   on  the  bucket,
which expresses the number of clause-references  in the chain that needs
updating. If a clause-reference  is  a   clause-list,  it  increases the
erased_clauses count thereof.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
deleteActiveClauseFromBucket(ClauseBucket cb, word key)
{ if ( !key )
  { ClauseRef cref;

    for(cref=cb->head; cref; cref=cref->next)
    { if ( tagex(cref->key) == (TAG_ATOM|STG_GLOBAL) )
      { ClauseList cl = &cref->value.clauses;

	if ( cl->erased_clauses++ == 0 )
	  cb->dirty++;
	cl->number_of_clauses--;
      }
    }

    cb->dirty++;				/* for the one directly added */
  } else if ( tagex(key) == (TAG_ATOM|STG_GLOBAL) )
  { ClauseRef cref;

    for(cref=cb->head; cref; cref=cref->next)
    { if ( cref->key == key )
      { ClauseList cl = &cref->value.clauses;

	if ( cl->erased_clauses++ == 0 )
	  cb->dirty++;
	cl->number_of_clauses--;

#ifdef O_SECURE
	{ ClauseRef cr;
	  unsigned int erased = 0;
	  unsigned int count = 0;

	  for(cr=cl->first_clause; cr; cr=cr->next)
	  { if ( true(cr->value.clause, ERASED) )
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
  } else
  { cb->dirty++;
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
{ word key;

  argKey(cl->codes, ci->arg-1, FALSE, &key);

  if ( key == 0 )			/* not indexed */
  { int i;
    ClauseBucket cb;

    for(i=ci->buckets, cb = ci->entries; --i>=0; cb++)
    { if ( cb->dirty == 0 )
	ci->dirty++;
      deleteActiveClauseFromBucket(cb, key);
    }
    assert(ci->dirty == ci->buckets);
  } else
  { int hi = hashIndex(key, ci->buckets);
    ClauseBucket cb = &ci->entries[hi];

    if ( cb->dirty == 0 )
      ci->dirty++;
    deleteActiveClauseFromBucket(cb, key);
    assert(cb->dirty>0);
  }
}


void
deleteActiveClauseFromIndexes(Definition def, Clause cl)
{ ClauseIndex ci;

  for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
    deleteActiveClauseFromIndex(ci, cl);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add a clause to an index.  If   the  clause cannot be indexed (typically
because it has a variable at the  argument location), the clause must be
added to all indexes.

ClauseIndex->size maintains the number of elements  in the list that are
indexed. This is needed for resizing the index.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
addClauseToIndex(ClauseIndex ci, Clause cl, int where ARG_LD)
{ ClauseBucket ch = ci->entries;
  word key;

  argKey(cl->codes, ci->arg-1, FALSE, &key);

  if ( key == 0 )			/* a non-indexable field */
  { int n = ci->buckets;

    for(; n; n--, ch++)
      addClauseBucket(ch, cl, key, where PASS_LD);
  } else
  { int hi = hashIndex(key, ci->buckets);

    DEBUG(4, Sdprintf("Storing in bucket %d\n", hi));
    ci->size += addClauseBucket(&ch[hi], cl, key, where PASS_LD);
  }
}


void
addClauseToIndexes(Definition def, Clause cl, int where ARG_LD)
{ ClauseIndex ci;

  for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
    addClauseToIndex(ci, cl, where PASS_LD);

  SECURE(checkDefinition(def));
}


void
delClauseFromIndex(Definition def, Clause cl)
{ ClauseIndex ci;

  for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
  { ClauseBucket ch = ci->entries;
    word key;

    argKey(cl->codes, ci->arg-1, FALSE, &key);

    if ( key == 0 )			/* a non-indexable field */
    { int n = ci->buckets;

      for(; n; n--, ch++)
	deleteClauseBucket(ch, cl, key);
    } else
    { int hi = hashIndex(key, ci->buckets);

      deleteClauseBucket(&ch[hi], cl, key);
      ci->size--;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a hash-index on def for arg. It is   ok to do so unlocked, but if
another thread did  the  job,  we   discard  our  result.  Another issue
concerns resizing. If our new table is  a resized version of an existing
table, we can only discard the existing table if it is not in use. There
are two conditions:

  1. def is dynamic and def->references == 1
  2. def is static.  In this case we must leave it to clause-GC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ClauseIndex
hashDefinition(Definition def, int arg, int buckets)
{ GET_LD
  ClauseRef cref;
  ClauseIndex ci, old;
  ClauseIndex *cip;

  for(old=def->impl.clauses.clause_indexes; old; old=old->next)
  { if ( old->arg == arg )
      break;
  }

  DEBUG(2, Sdprintf("hashDefinition(%s, %d, %d)\n",
		    predicateName(def), arg, buckets));

  ci = newClauseIndexTable(arg, buckets);

  for(cref = def->impl.clauses.first_clause; cref; cref = cref->next)
  { if ( false(cref->value.clause, ERASED) )
      addClauseToIndex(ci, cref->value.clause, CL_END PASS_LD);
  }
  ci->resize_above = ci->size*2;
  ci->resize_below = ci->size/4;

  LOCKDEF(def);
  if ( !old )				/* this is a new table */
  { ClauseIndex conc;

    for(conc=def->impl.clauses.clause_indexes; conc; conc=conc->next)
    { if ( conc->arg == arg )
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


static ClauseIndex
resizeHashDefinition(Definition def, ClauseIndex old)
{ int newbuckets;
  float speedup;
  ClauseIndex ci;

  if ( (newbuckets=reassessHash(def, old, &speedup)) < 0 )
  { LOCKDEF(def);
    replaceIndex(def, old, NULL);
    UNLOCKDEF(def);

    return NULL;
  }

  if ( (ci=hashDefinition(def, old->arg, newbuckets)) )
    ci->speedup = speedup;

  return ci;
}


static void				/* definition must be locked */
replaceIndex(Definition def, ClauseIndex old, ClauseIndex ci)
{ GET_LD
  ClauseIndex *cip;

  for(cip=&def->impl.clauses.clause_indexes; *cip && *cip != old; cip = &(*cip)->next)
    ;

  if ( true(def, DYNAMIC) && def->references == 1 )
  { if ( ci )
    { ci->next = old->next;		/* replace */
      *cip = ci;
    } else
    { *cip = old->next;
    }
    unallocClauseIndexTable(old);
  } else				/* insert before old */
  { ClauseIndexList c = allocHeapOrHalt(sizeof(*c));

    if ( ci )
    { ci->next = old->next;
      MemoryBarrier();			/* lock only synchronizes updates */
      *cip = ci;
    } else				/* this is a delete */
    { *cip = old->next;
    }

    old->erased = TRUE;
    c->index = old;
    c->next = def->old_clause_indexes;
    def->old_clause_indexes = c;
  }
}


		 /*******************************
		 *	     ASSESSMENT		*
		 *******************************/

typedef struct hash_assessment
{ int		arg;			/* arg for which to assess */
  size_t	allocated;		/* allocated size of array */
  size_t	size;			/* keys in array */
  size_t	var_count;		/* # non-indexable cases */
  word	       *keys;			/* tmp key-set */
} hash_assessment;

static int
compar_keys(const void *p1, const void *p2)
{ const word *k1 = p1;
  const word *k2 = p2;
  intptr_t d = (*k1-*k2);

  return d < 0 ? -1 : d > 0 ? 1 : 0;
}

static void
assess_remove_duplicates(hash_assessment *a)
{ Word s = a->keys;
  Word o = a->keys;
  Word e = &s[a->size];
  word c = 0;				/* invalid key */

  qsort(a->keys, a->size, sizeof(word), compar_keys);
  for( ; s<e; s++)
  { if ( *s != c )
      c = *o++ = *s;
  }
  a->size = o - a->keys;
}

static int
assessAddKey(hash_assessment *a, word key)
{ if ( a->size < a->allocated )
  { a->keys[a->size++] = key;
  } else
  { if ( a->allocated == 0 )
    { a->allocated = 512;
      if ( !(a->keys = malloc(a->allocated*sizeof(*a->keys))) )
	return FALSE;
      a->keys[a->size++] = key;
    } else
    { assess_remove_duplicates(a);
      if ( a->size*2 > a->allocated )
      { word *new = realloc(a->keys, a->allocated*2*sizeof(*a->keys));
	if ( !new )
	  return FALSE;
	a->keys = new;
	a->allocated *= 2;
      }
      a->keys[a->size++] = key;
    }
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
bestHash(Word av, Definition def, int *buckets, float *speedup)
{ GET_LD
  int i;
  ClauseRef cref;
  hash_assessment assess_buf[ASSESS_BUFSIZE];
  hash_assessment *assessments = assess_buf;
  int assess_allocated = ASSESS_BUFSIZE;
  int assess_count = 0;
  int clause_count = 0;
  hash_assessment *a;
  int best = -1;
  float  best_speedup = MIN_SPEEDUP;
  size_t best_size = 0;

  if ( !def->tried_index )
    def->tried_index = new_bitvector(def->functor->arity);

					/* Step 1: allocate assessments */
  for(i=0; i<def->functor->arity; i++)
  { word k;

    if ( !true_bit(def->tried_index, i) && (k=indexOfWord(av[i] PASS_LD)) )
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

    if ( true(cl, ERASED) )
      continue;

    for(i=0, a=assessments; i<assess_count; i++, a++)
    { word k;

      if ( carg < a->arg )
      { pc = skipArgs(pc, a->arg-carg);
	carg = a->arg;
      }
      if ( argKey(pc, 0, FALSE, &k) )
      { assessAddKey(a, k);
      } else
      { a->var_count++;
      }
    }

    clause_count++;
  }

  for(i=0, a=assessments; i<assess_count; i++, a++)
  { size_t space;
    float speedup;

    assess_remove_duplicates(a);
    if ( a->keys )
    { free(a->keys);			/* also implies a-size>0 */

      space   = clause_count*a->var_count;
      speedup =            (float)(clause_count*a->size) /
	        (float)(clause_count - a->var_count + a->var_count*a->size);

      DEBUG(2, Sdprintf("Assess arg %d of %s: speedup %f, space = %ld\n",
			a->arg+1, predicateName(def), speedup, (long)space));

      if ( speedup > best_speedup )
      { best         = a->arg;
	best_speedup = speedup;
	best_size    = a->size;
      } else if ( speedup <= MIN_SPEEDUP )
      { set_bit(def->tried_index, a->arg);
      }
    } else
    { set_bit(def->tried_index, a->arg);
      DEBUG(2, Sdprintf("Assess arg %d of %s: not indexable\n",
			a->arg+1, predicateName(def)));
    }
  }

  if ( assessments != assess_buf )
    free(assessments);

  if ( best >= 0 )
  { *buckets = best_size;
    *speedup = best_speedup;
  }

  return best;
}


static int
reassessHash(Definition def, ClauseIndex ci, float *speedup)
{ hash_assessment a_store;
  hash_assessment *a = &a_store;
  ClauseRef cref;
  int clause_count = def->impl.clauses.number_of_clauses;

  memset(a, 0, sizeof(*a));

  for(cref=def->impl.clauses.first_clause; cref; cref=cref->next)
  { Clause cl = cref->value.clause;
    word k;

    if ( true(cl, ERASED) )
      continue;

    if ( argKey(cref->value.clause->codes, ci->arg-1, FALSE, &k) )
    { assessAddKey(a, k);
    } else
    { a->var_count++;
    }
  }

  if ( a->keys )
  { size_t size;
    float sp;
    free(a->keys);

    size = a->size;
    sp   =            (float)(clause_count*a->size) /
	        (float)(clause_count - a->var_count + a->var_count*a->size);

    if ( sp < 2.0 )
      return -1;

    *speedup = sp;
    return size;
  }

  return -1;				/* not hashable */
}

		 /*******************************
		 *  PREDICATE PROPERTY SUPPORT	*
		 *******************************/

static int
unify_clause_index(term_t t, ClauseIndex ci)
{ return PL_unify_term(t,
		       PL_FUNCTOR, FUNCTOR_minus2,
			 PL_INT, (int)ci->arg,
			 PL_FUNCTOR_CHARS, "hash", 2,
			   PL_INT, (int)ci->buckets,
			   PL_DOUBLE, (double)ci->speedup);
}

bool
unify_index_pattern(Procedure proc, term_t value)
{ GET_LD
  Definition def = proc->definition;
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
		 *	     PREDICATES		*
		 *******************************/

/** hash(:PredInd, +ArgSpec) is det.

*/

static
PRED_IMPL("hash", 2, hash, PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure proc;
  int argn;

  term_t pred = A1;

  if ( !PL_get_integer_ex(A2, &argn) )
    return FALSE;
  if ( argn < 1 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_not_less_than_one, A2);

  if ( get_procedure(pred, &proc, 0, GP_NAMEARITY|GP_CREATE) )
  { Definition def = getProcDefinition(proc);
    int size, minsize;
    ClauseIndex ci;

    if ( def->impl.clauses.clause_indexes )		/* already hashed; won't change */
      succeed;

    if ( true(def, FOREIGN) )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		      ATOM_hash, ATOM_foreign, proc);
    if ( argn > def->functor->arity )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_argument, A2);

    for(ci=def->impl.clauses.clause_indexes; ci; ci=ci->next)
    { if ( ci->arg == argn )
	succeed;			/* Hashed index already provided */
    }

    minsize = def->impl.clauses.number_of_clauses / 2,
    size = 4;
    while (size < minsize)
      size *= 2;

    hashDefinition(def, argn, size);

    succeed;
  }

  fail;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(index)
  PRED_DEF("hash", 2, hash, PL_FA_TRANSPARENT)
EndPredDefs
