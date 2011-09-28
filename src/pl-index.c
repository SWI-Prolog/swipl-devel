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
nextClauseArg1(uintptr_t generation, ClauseChoice chp)
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

#ifdef O_LOGICAL_UPDATE
# define gen (fr->generation)
#else
# define gen 0L
#endif

  if ( def->indexPattern & NEED_REINDEX )
    reindexDefinition(def);

  for(ci=def->hash_info; ci; ci=ci->next)
  { if ( (chp->key=indexOfWord(argv[ci->arg-1] PASS_LD)) )
    { int hi = hashIndex(chp->key, def->hash_info->buckets);

      chp->cref = def->hash_info->entries[hi].head;
      return nextClauseArg1(gen, chp);
    }
  }

  if ( def->indexPattern == 0x0L )
  {
  noindex:
    for(cref = def->definition.clauses; cref; cref = cref->next)
    { if ( visibleClause(cref->clause, gen) )
      { chp->cref = cref->next;
	chp->key = 0;
        return cref;
      }
    }
    return NULL;
  } else if ( def->indexPattern == 0x1L )
  { if ( !(chp->key = indexOfWord(argv[0] PASS_LD)) )
      goto noindex;

    chp->cref = def->definition.clauses;
    return nextClauseArg1(gen, chp);
  } else
  { assert(0);
  }

#undef gen
}


ClauseRef
nextClause(ClauseChoice chp, Word argv,
	   LocalFrame fr, Definition def ARG_LD)
{
#ifdef O_LOGICAL_UPDATE
  #define gen (fr->generation)
#else
  #define gen 0L
#endif

  if ( !chp->key )			/* not indexed */
  { ClauseRef cref;

    for(cref=chp->cref; cref; cref = cref->next)
    { if ( visibleClause(cref->clause, gen) )
      { chp->cref = cref->next;
	return cref;
      }
    }
    return NULL;
  } else
  { return nextClauseArg1(gen, chp);
  }

#undef gen
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

  ci->buckets  = buckets;
  ci->size     = 0;
  ci->alldirty = FALSE;
  ci->arg      = arg;
  ci->next     = NULL;
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
gcClauseChain(ClauseChain ch, int dirty ARG_LD)
{ ClauseRef cref = ch->head, prev = NULL;
  int deleted = 0;

  while( cref && dirty != 0 )
  { if ( true(cref->clause, ERASED) )
    { ClauseRef c = cref;

      if ( dirty > 0 )
      { deleted++;
	dirty--;
      }

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
{ ClauseChain ch = ci->entries;
  int n = ci->buckets;

  if ( ci->alldirty )
  { for(; n; n--, ch++)
      ci->size -= gcClauseChain(ch, -1 PASS_LD); /* do them all */
  } else
  { for(; n; n--, ch++)
    { if ( ch->dirty )
	ci->size -= gcClauseChain(ch, (int)ch->dirty PASS_LD);
    }
  }
}


void
cleanClauseIndex(ClauseIndex ci, ClauseRef clauses ARG_LD)
{ gcClauseIndex(ci PASS_LD);
  if ( ci->size > ci->buckets * 2 )
  { int newbuckets = ci->buckets;

    while(ci->size > newbuckets)
      newbuckets *= 2;

    resizeClauseIndex(ci, clauses, newbuckets PASS_LD);
  }
}


void
markDirtyClauseIndex(ClauseIndex ci, Clause cl)
{ word key;

  argKey(cl->codes, ci->arg-1, FALSE, &key);

  if ( key == 0 )			/* not indexed */
  { ci->alldirty = TRUE;
  } else
  { int hi = hashIndex(key, ci->buckets);
    ci->entries[hi].dirty++;
  }
}


/* MT: caller must have predicate locked */

void
addClauseToIndex(ClauseIndex ci, Clause cl, int where ARG_LD)
{ ClauseChain ch = ci->entries;
  word key;

  assert(ci->arg == 1);

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
      if ( false(def, NEEDSREHASH) && ci->size*4 < ci->buckets )
      { set(def, NEEDSREHASH);
	if ( true(def, DYNAMIC) && def->references == 0 )
	{ DEBUG(0, Sdprintf("Should clean %s\n", predicateName(def)));
	  /* TBD: need to clear right away if dynamic and not referenced */
	  /* see assertProcedure() for similar case.  To do that locking */
	  /* needs to be sorted out */
	}
      }
    }
  }
}


/* MT: Caller must have predicate locked
*/

bool
hashDefinition(Definition def, int arg, int buckets)
{ GET_LD
  ClauseRef cref;
  ClauseIndex ci = newClauseIndexTable(arg, buckets);
  ClauseIndex *cip;

  DEBUG(2, Sdprintf("hashDefinition(%s, %d, %d)\n",
		    predicateName(def), arg, buckets));

  ci = newClauseIndexTable(arg, buckets);

  for(cref = def->definition.clauses; cref; cref = cref->next)
  { if ( false(cref->clause, ERASED) )
      addClauseToIndex(ci, cref->clause, CL_END PASS_LD);
  }

  for(cip=&def->hash_info; *cip; cip = &(*cip)->next)
    ;
  *cip = ci;

  return TRUE;
}


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

    LOCKDEF(def);
    minsize = def->number_of_clauses / 2,
    size = 4;
    while (size < minsize)
      size *= 2;

    hashDefinition(def, argn, size);
    UNLOCKDEF(def);

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
