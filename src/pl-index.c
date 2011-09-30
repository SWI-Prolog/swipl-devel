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

static int		bestHash(Word av, ClauseRef cref, int *buckets);
static ClauseIndex	hashDefinition(Definition def, int arg, int buckets);

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
	 visibleClause(cref->clause, generation))
    { ClauseRef result = cref;
      int maxsearch = MAXSEARCH;

      for( cref = cref->next; cref; cref = cref->next )
      { if ( ((!cref->key || key == cref->key) &&
	      visibleClause(cref->clause, generation)) ||
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ClauseRef
firstClause(Word argv, LocalFrame fr, Definition def, ClauseChoice chp ARG_LD)
{ ClauseRef cref;
  ClauseIndex ci;
  int best, buckets;

  if ( def->functor->arity == 0 )
    goto simple;

  for(ci=def->hash_info; ci; ci=ci->next)
  { if ( (chp->key=indexOfWord(argv[ci->arg-1] PASS_LD)) )
    { int hi = hashIndex(chp->key, ci->buckets);

      chp->cref = ci->entries[hi].head;
      return nextClauseArg1(chp, generationFrame(fr));
    }
  }

  if ( def->number_of_clauses == 0 )
    return NULL;

  if ( (chp->key = indexOfWord(argv[0] PASS_LD)) &&
       def->number_of_clauses <= 10 )
  { chp->cref = def->definition.clauses;
    return nextClauseArg1(chp, generationFrame(fr));
  }

  if ( (best=bestHash(argv, def->definition.clauses, &buckets)) >= 0 )
  { if ( (ci=hashDefinition(def, best+1, buckets)) )
    { int hi;

      chp->key = indexOfWord(argv[ci->arg-1] PASS_LD);
      assert(chp->key);
      hi = hashIndex(chp->key, ci->buckets);
      chp->cref = ci->entries[hi].head;
      return nextClauseArg1(chp, generationFrame(fr));
    }
  }

  if ( chp->key )
  { chp->cref = def->definition.clauses;
    return nextClauseArg1(chp, generationFrame(fr));
  }

simple:
  for(cref = def->definition.clauses; cref; cref = cref->next)
  { if ( visibleClause(cref->clause, generationFrame(fr)) )
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
    { if ( visibleClause(cref->clause, generationFrame(fr)) )
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
  ClauseChain ch;
  int m = 4;

  while(m<buckets)
    m *= 2;
  buckets = m;

  memset(ci, 0, sizeof(*ci));
  ci->buckets  = buckets;
  ci->arg      = arg;
  ci->entries  = allocHeapOrHalt(sizeof(struct clause_chain) * buckets);

  for(ch = ci->entries; buckets; buckets--, ch++)
  { ch->head = ch->tail = NULL;
    ch->dirty = 0;
  }

  return ci;
}


static void
unallocClauseIndexTableEntries(ClauseChain entries, int buckets ARG_LD)
{ ClauseChain ch;
  int i;

  for(ch=entries,i=buckets; --i>=0; ch++)
  { ClauseRef cr, next;

    for(cr = ch->head; cr; cr = next)
    { next = cr->next;
      freeHeap(cr, sizeof(*cr));
    }
  }

  freeHeap(entries, buckets * sizeof(struct clause_chain));
}

void
unallocClauseIndexTable(ClauseIndex ci)
{ GET_LD

  unallocClauseIndexTableEntries(ci->entries, ci->buckets PASS_LD);
  freeHeap(ci, sizeof(struct clause_index));
}


static void
appendClauseChain(ClauseChain ch, Clause cl, word key, int where ARG_LD)
{ ClauseRef cr = newClauseRef(cl, key PASS_LD);

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
}


static void
deleteClauseChain(ClauseChain ch, Clause clause)
{ ClauseRef prev = NULL;
  ClauseRef c;

  for(c = ch->head; c; prev = c, c = c->next)
  { if ( c->clause == clause )
    { if ( !prev )
      { ch->head = c->next;
	if ( !c->next )
	  ch->tail = NULL;
      } else
      { prev->next = c->next;
	if ( !c->next)
	  ch->tail = prev;
      }
    }
  }
}


static int
resizeClauseIndex(ClauseIndex ci, ClauseRef cref, int newbuckets ARG_LD)
{ size_t bytes = sizeof(struct clause_chain) * newbuckets;
  ClauseChain chains = allocHeapOrHalt(bytes);
  ClauseChain oldentries = ci->entries;
  int oldbuckets = ci->buckets;

  memset(chains, 0, bytes);
  for(; cref; cref=cref->next)
  { Clause clause = cref->clause;
    word key;

    if ( argKey(clause->codes, ci->arg-1, FALSE, &key) )
    { int hi = hashIndex(key, newbuckets);
      appendClauseChain(&chains[hi], clause, key, CL_END PASS_LD);
    } else
    { int hi;

      for(hi=0; hi<newbuckets; hi++)
	appendClauseChain(&chains[hi], clause, 0, CL_END PASS_LD);
    }
  }
  ci->entries = chains;
  ci->buckets = newbuckets;
  unallocClauseIndexTableEntries(oldentries, oldbuckets PASS_LD);

  return TRUE;
}


static int
gcClauseChain(ClauseChain ch, unsigned int dirty ARG_LD)
{ ClauseRef cref = ch->head, prev = NULL;
  int deleted = 0;

  while( cref && dirty )
  { if ( true(cref->clause, ERASED) )
    { ClauseRef c = cref;

      if ( cref->key )
	deleted++;			/* only reduce size by indexed */
      dirty--;

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

      freeClauseRef(c PASS_LD);
    } else
    { prev = cref;
      cref = cref->next;
    }
  }

  ch->dirty = 0;

  return deleted;
}


static void
gcClauseIndex(ClauseIndex ci ARG_LD)
{ if ( ci->dirty )
  { ClauseChain ch = ci->entries;
    int n = ci->buckets;

    for(; n; n--, ch++)
    { if ( ch->dirty )
	ci->size -= gcClauseChain(ch, ch->dirty PASS_LD);
      if ( --ci->dirty == 0 )
	break;
    }
  }

  assert((int)ci->size >= 0);
}


void
cleanClauseIndex(ClauseIndex ci, ClauseRef clauses ARG_LD)
{ gcClauseIndex(ci PASS_LD);

  if ( ci->size > ci->dim_ok_size*2 )
  { int newbuckets = ci->buckets;

    while(ci->size > newbuckets)
      newbuckets *= 2;

    resizeClauseIndex(ci, clauses, newbuckets PASS_LD);
  }
}


void
deleteActiveClauseFromIndex(ClauseIndex ci, Clause cl)
{ word key;

  argKey(cl->codes, ci->arg-1, FALSE, &key);

  if ( key == 0 )			/* not indexed */
  { if ( ci->dirty != ci->buckets )
    { int i;
      ClauseChain ch;

      ci->dirty = ci->buckets;
      for(i=ci->buckets, ch = ci->entries; --i>=0; ch++)
	ch->dirty++;
    }
  } else
  { int hi = hashIndex(key, ci->buckets);

    if ( ci->entries[hi].dirty == 0 )
      ci->dirty++;
    ci->entries[hi].dirty++;
  }
}


void
deleteActiveClauseFromIndexes(Definition def, Clause cl)
{ ClauseIndex ci;

  for(ci=def->hash_info; ci; ci=ci->next)
    deleteActiveClauseFromIndex(ci, cl);
}


/* MT: caller must have predicate locked */

void
addClauseToIndex(ClauseIndex ci, Clause cl, int where ARG_LD)
{ ClauseChain ch = ci->entries;
  word key;

  argKey(cl->codes, ci->arg-1, FALSE, &key);

  if ( key == 0 )			/* a non-indexable field */
  { int n = ci->buckets;

    for(; n; n--, ch++)
      appendClauseChain(ch, cl, key, where PASS_LD);
  } else
  { int hi = hashIndex(key, ci->buckets);

    DEBUG(4, Sdprintf("Storing in bucket %d\n", hi));
    appendClauseChain(&ch[hi], cl, key, where PASS_LD);
    ci->size++;
  }
}


void
delClauseFromIndex(Definition def, Clause cl)
{ ClauseIndex ci;

  for(ci=def->hash_info; ci; ci=ci->next)
  { ClauseChain ch = ci->entries;
    word key;

    argKey(cl->codes, ci->arg-1, FALSE, &key);

    if ( key == 0 )			/* a non-indexable field */
    { int n = ci->buckets;

      for(; n; n--, ch++)
	deleteClauseChain(ch, cl);
    } else
    { int hi = hashIndex(key, ci->buckets);

      deleteClauseChain(&ch[hi], cl);
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

  for(old=def->hash_info; old; old=old->next)
  { if ( old->arg == arg )
      break;
  }

  DEBUG(2, Sdprintf("hashDefinition(%s, %d, %d)\n",
		    predicateName(def), arg, buckets));

  ci = newClauseIndexTable(arg, buckets);

  for(cref = def->definition.clauses; cref; cref = cref->next)
  { if ( false(cref->clause, ERASED) )
      addClauseToIndex(ci, cref->clause, CL_END PASS_LD);
  }
  ci->dim_ok_size = ci->size;

  LOCKDEF(def);
  if ( !old )				/* this is a new table */
  { ClauseIndex conc;

    for(conc=def->hash_info; conc; conc=conc->next)
    { if ( conc->arg == arg )
      { UNLOCKDEF(def);
	unallocClauseIndexTable(ci);
	return conc;
      }
    }
  } else				/* replace (resize) old */
  { if ( true(def, DYNAMIC) )
    { assert(0);			/* TBD */
    } else
    { assert(0);
    }
  }
  for(cip=&def->hash_info; *cip; cip = &(*cip)->next)
    ;
  *cip = ci;
  UNLOCKDEF(def);

  return ci;
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

static int
bestHash(Word av, ClauseRef cref, int *buckets)
{ GET_LD
  int i, arity = cref->clause->procedure->definition->functor->arity;
  hash_assessment assess_buf[ASSESS_BUFSIZE];
  hash_assessment *assessments = assess_buf;
  int assess_allocated = ASSESS_BUFSIZE;
  int assess_count = 0;
  int clause_count = 0;
  hash_assessment *a;
  int best = -1;
  float  best_speedup = 1.5;
  size_t best_size = 0;

					/* Step 1: allocate assessments */
  for(i=0; i<arity; i++)
  { word k;

    if ( (k=indexOfWord(av[i] PASS_LD)) )
    { if ( assess_count	>= assess_allocated )
      { size_t newbytes = sizeof(*assessments)*2*assess_allocated;

	if ( assessments == assess_buf )
	{ assessments = malloc(newbytes);
	  memcpy(assessments, assess_buf, sizeof(assess_buf));
	} else
	  assessments = realloc(assessments, newbytes);
      }
      a = &assessments[assess_count++];
      memset(a, 0, sizeof(*a));
      a->arg = i;
    }
  }

  if ( assess_count == 0 )
    return -1;				/* no luck */

					/* Step 2: assess */
  for(; cref; cref=cref->next)
  { Clause cl = cref->clause;
    Code pc = cref->clause->codes;
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

      if ( speedup > best_speedup )
      { best         = a->arg;
	best_speedup = speedup;
	best_size    = a->size;
      }
    }
  }

  if ( assessments != assess_buf )
    free(assessments);

  if ( best >= 0 )
    *buckets = best_size;

  return best;
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

    if ( def->hash_info )		/* already hashed; won't change */
      succeed;

    if ( true(def, FOREIGN) )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		      ATOM_hash, ATOM_foreign, proc);
    if ( argn > def->functor->arity )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_argument, A2);

    for(ci=def->hash_info; ci; ci=ci->next)
    { if ( ci->arg == argn )
	succeed;			/* Hashed index already provided */
    }

    minsize = def->number_of_clauses / 2,
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
