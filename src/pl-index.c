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


void
getIndex(Word argv, unsigned long pattern, word *index
	 ARG_LD)
{ if ( pattern == 0x1L )
  { *index = indexOfWord(*argv PASS_LD);

    return;
  } else
  { assert(0);
  }

  return;
}


word
getIndexOfTerm(term_t t)
{ GET_LD
  word w = *valTermRef(t);

  return indexOfWord(w PASS_LD);
}


static inline ClauseRef
nextClauseArg1(ClauseRef cref, uintptr_t generation,
	       ClauseRef *next, word key)
{ for(;cref ; cref = cref->next)
  { if ( (!cref->key || key == cref->key) &&
	 visibleClause(cref->clause, generation))
    { ClauseRef result = cref;
      int maxsearch = MAXSEARCH;

      for( cref = cref->next; cref; cref = cref->next )
      { if ( ((!cref->key || key == cref->key) &&
	      visibleClause(cref->clause, generation)) ||
	     --maxsearch == 0 )
	{ *next = cref;

	  return result;
	}
      }
      *next = NULL;

      return result;
    }
  }

  return NULL;
}


ClauseRef
firstClause(Word argv, LocalFrame fr, Definition def, ClauseRef *next ARG_LD)
{ ClauseRef cref;

#ifdef O_LOGICAL_UPDATE
# define gen (fr->generation)
#else
# define gen 0L
#endif

again:
  if ( def->indexPattern == 0x0L )
  {
  noindex:
    for(cref = def->definition.clauses; cref; cref = cref->next)
    { if ( visibleClause(cref->clause, gen) )
      { *next = cref->next;
        return cref;
      }
    }
    return NULL;
  } else if ( def->indexPattern == 0x1L )
  { word key = indexOfWord(*argv PASS_LD);

    if ( key == 0L )
      goto noindex;

    if ( def->hash_info )
    { int hi = hashIndex(key, def->hash_info->buckets);

      cref = def->hash_info->entries[hi].head;
    } else
      cref = def->definition.clauses;

    return nextClauseArg1(cref, gen, next, key);
  } else if ( def->indexPattern & NEED_REINDEX )
  { reindexDefinition(def);
    goto again;
  } else
  { assert(0);
  }

#undef gen
}


ClauseRef
findClause(ClauseRef cref, Word argv,
	   LocalFrame fr, Definition def, ClauseRef *next ARG_LD)
{
#ifdef O_LOGICAL_UPDATE
  #define gen (fr->generation)
#else
  #define gen 0L
#endif

  if ( def->indexPattern == 0x0L )	/* not indexed */
  { noindex:
    for(;;cref = cref->next)
    { if ( cref )
      { if ( visibleClause(cref->clause, gen) )
	{ *next = cref->next;
	  return cref;
	}
      } else
	return NULL;
    }
  } else if ( def->indexPattern == 0x1L ) /* first-argument indexing */
  { word key = indexOfWord(*argv PASS_LD);

    if ( !key )
      goto noindex;

    return nextClauseArg1(cref, gen, next, key);
  } else if ( def->indexPattern & NEED_REINDEX )
  { reindexDefinition(def);
    return findClause(cref, argv, fr, def, next PASS_LD);
  } else
  { assert(0);
  }

#undef gen
}


		 /*******************************
		 *	   HASH SUPPORT		*
		 *******************************/

static ClauseIndex
newClauseIndexTable(int buckets)
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
  ci->entries  = allocHeapOrHalt(sizeof(struct clause_chain) * buckets);

  for(ch = ci->entries; buckets; buckets--, ch++)
  { ch->head = ch->tail = NULL;
    ch->dirty = 0;
  }

  return ci;
}


void
unallocClauseIndexTable(ClauseIndex ci)
{ GET_LD
  ClauseChain ch;
  int buckets = ci->buckets;

  for(ch = ci->entries; buckets; buckets--, ch++)
  { ClauseRef cr, next;

    for(cr = ch->head; cr; cr = next)
    { next = cr->next;
      freeHeap(cr, sizeof(*cr));
    }
  }

  freeHeap(ci->entries, ci->buckets * sizeof(struct clause_chain));
  freeHeap(ci, sizeof(struct clause_index));
}


static void
appendClauseChain(ClauseChain ch, Clause cl, int where ARG_LD)
{ ClauseRef cr = newClauseRef(cl PASS_LD);

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


#define INFINT (~(1<<(INTBITSIZE-1)))

void
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
markDirtyClauseIndex(ClauseIndex ci, Clause cl)
{ word key;

  argKey(cl->codes, FALSE, &key);	/* TBD: this is just arg1 */

  if ( key == 0 )			/* not indexed */
  { ci->alldirty = TRUE;
  } else
  { int hi = hashIndex(key, ci->buckets);
    ci->entries[hi].dirty++;
  }
}


/* MT: caller must have predicate locked */

void
addClauseToIndex(Definition def, Clause cl, int where ARG_LD)
{ ClauseIndex ci = def->hash_info;
  ClauseChain ch = ci->entries;
  word key;

  argKey(cl->codes, FALSE, &key);	/* TBD: this is only arg1 */

  if ( key == 0 )			/* a non-indexable field */
  { int n = ci->buckets;

    SECURE({ word k;
	     assert(!argKey(cl->codes, FALSE, &k));
	   });

    DEBUG(1,
	  if ( def->indexPattern == 0x1 )
	    Sdprintf("*** Adding unindexed clause to index of %s\n",
		     predicateName(def)));

    for(; n; n--, ch++)
      appendClauseChain(ch, cl, where PASS_LD);
  } else
  { int hi = hashIndex(key, ci->buckets);

    DEBUG(4, Sdprintf("Storing in bucket %d\n", hi));
    appendClauseChain(&ch[hi], cl, where PASS_LD);
    ci->size++;
  }
}


void
delClauseFromIndex(Definition def, Clause cl)
{ ClauseIndex ci = def->hash_info;
  ClauseChain ch = ci->entries;
  word key;

  argKey(cl->codes, FALSE, &key);	/* TBD: this is only arg1 */

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


/* MT: Caller must have predicate locked
*/

bool
hashDefinition(Definition def, int buckets)
{ GET_LD
  ClauseRef cref;

  DEBUG(2, Sdprintf("hashDefinition(%s, %d)\n", predicateName(def), buckets));

  def->hash_info = newClauseIndexTable(buckets);

  for(cref = def->definition.clauses; cref; cref = cref->next)
  { if ( false(cref->clause, ERASED) )
      addClauseToIndex(def, cref->clause, CL_END PASS_LD);
  }

  succeed;
}

word
pl_hash(term_t pred)
{ Procedure proc;

  if ( get_procedure(pred, &proc, 0, GP_CREATE) )
  { GET_LD
    Definition def = getProcDefinition(proc);
    int size, minsize;

    if ( def->hash_info )		/* already hashed; won't change */
      succeed;

    if ( true(def, FOREIGN) )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		      ATOM_hash, ATOM_foreign, proc);
    if ( def->functor->arity == 0 )
      return PL_error(NULL, 0, "hash needs arguments", ERR_REPRESENTATION,
		      ATOM_arity);

    LOCKDEF(def);
    indexDefinition(def, 0x1L);		/* index in 1st argument */

    minsize = def->number_of_clauses / 4,
    size = 64;
    while (size < minsize)
      size *= 2;

					/* == reindexDefinition(), but */
					/* we cannot call this as it would */
					/* deadlock */
    if ( def->indexPattern & NEED_REINDEX )
      def->indexPattern = 0x1L;

    hashDefinition(def, size);
    UNLOCKDEF(def);

    succeed;
  }

  fail;
}
