/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "pl-incl.h"

#define LOCK()   PL_LOCK(L_PREDICATE)
#define UNLOCK() PL_UNLOCK(L_PREDICATE)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clause indexing.  Clauses store an  `index  structure',  which  provides
summary information on the unification behaviour of the clause (e.i. its
head  arguments.   This  structure  consists  of  two words: a key and a
varmask.  Indexing can be done with upto 4 arguments.   Both  words  are
divided  into  the  same  number  of  bit  groups  as  there are indexed
arguments.  If an argument  is  indexable  (atom,  integer  or  compound
term),  the  corresponding  bit group is filled with bits taken from the
atom  pointer,  integer  or  functor  pointer.    In   this   case   all
corresponding  bits  in  the varmask field are 1.  Otherwise the bits in
both the varmask and the key are all 0.

To find a clause using indexing, we calculate an  index  structure  from
the  calling arguments to the goal using the same rules.  Now, we can do
a mutual `and' using the varmasks on the keys and  compare  the  result.
If  equal  a  good  chance  for a possible unification exists, otherwise
unification will definitely fail.  See matchIndex() and findClause().

Care has been taken to get this code as fast as  possible,  notably  for
indexing only on the first argument as this is default.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* 1 <= c <= 4 */

#define SHIFT(c, a)	((LONGBITSIZE/(c)) * a)
#define MASK(c)		(c == 1 ? ~0L : ((1L << (LONGBITSIZE/(c))) - 1))
#define VM(c, a)	((unsigned long)(~(MASK(c) << SHIFT(c, a))))

#define Shift(c, a)	(mask_shift[c][a])
#define Mask(c)		(mask_mask[c])
#define varMask(c, a)	(variable_mask[c][a])

#define matchIndex(i1, i2)	(((i1).key & (i2).varmask) ==\
				  ((i2).key & (i1).varmask))

static unsigned long variable_mask[][4] =
  { { 0,        0,        0,        0 }, 
#ifdef DONOT_AVOID_SHIFT_WARNING
    { VM(1, 0), 0,        0,        0 },
#else
    { (unsigned long)~0L,      0,        0,        0 },
#endif
    { VM(2, 0), VM(2, 1), 0,        0 }, 
    { VM(3, 0), VM(3, 1), VM(3, 2), 0 }, 
    { VM(4, 0), VM(4, 1), VM(4, 2), VM(4, 3) }
  };

static int mask_shift[][4] =
  { { 0,           0,           0,           0 }, 
    { SHIFT(1, 0), 0,           0,           0 }, 
    { SHIFT(2, 0), SHIFT(2, 1), 0,           0 }, 
    { SHIFT(3, 0), SHIFT(3, 1), SHIFT(3, 2), 0 }, 
    { SHIFT(4, 0), SHIFT(4, 1), SHIFT(4, 2), SHIFT(4, 3) }
  };

static unsigned long mask_mask[] =
  { 0,
#ifdef DONOT_AVOID_SHIFT_WARNING
    MASK(1),
#else
    0L,
#endif
    MASK(2), MASK(3), MASK(4)
  };


int
cardinalityPattern(register unsigned long pattern)
{ register int result = 0;

  for(; pattern; pattern >>= 1)
    if (pattern & 0x1)
      result++;

  return result;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compute the index in the hash-array from   a machine word and the number
of buckets. This used to be simple, but now that our tag bits are on the
left side, simply masking will put most things on the same hash-entry as
it is very common for all clauses of   a predicate to have the same type
of object. Hence, we now use exclusive or of the real value part and the
tag-bits.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline int
hashIndex(word key, int buckets)
{ unsigned long k = key >> LMASK_BITS;

  return (key^k) & (buckets-1);
}


static word
indexOfWord(word w ARG_LD)
{ for(;;)
  { switch(tag(w))
    { case TAG_VAR:
      case TAG_STRING:
      case TAG_FLOAT:
	return 0L;
      case TAG_INTEGER:
	if ( storage(w) != STG_INLINE )
	  return valBignum(w);
      case TAG_ATOM:
	break;				/* atom_t */
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
getIndex(Word argv, unsigned long pattern, int card, struct index *index
	 ARG_LD)
{ if ( pattern == 0x1L )
  { index->key     = indexOfWord(*argv PASS_LD);
    index->varmask = (index->key ? (unsigned long) ~0L : 0L);

    return;
  } else
  { word key;
    int a;

    index->key = 0;
    index->varmask = (unsigned long) ~0L;			/* all 1s */

    for(a = 0; a < card; a++, pattern >>= 1, argv++)
    { for(;(pattern & 0x1) == 0; pattern >>= 1)
	argv++;

      key = indexOfWord(*argv PASS_LD);
      if ( !key )
      { index->varmask &= varMask(card, a);
      }
      key = key ^ (key >> LMASK_BITS);	/* see hashIndex() */
      index->key |= ((key & Mask(card)) << Shift(card, a) );
    }
  }

  return;
}


#if 0
#undef visibleClause
static int
visibleClause(Clause cl, unsigned long gen)
{ int rval = (cl->generation.created <= gen &&
	      cl->generation.erased   > gen);

  if ( !rval )
  { if ( false(cl, ERASED) )
    { DEBUG(2, Sdprintf("Ignored clause %p from %s "
			"(created %lu, erased %lu, gen %lu)\n",
			cl, procedureName(cl->procedure),
			cl->generation.created,
			cl->generation.erased,
			gen));
    }
    trap_gdb();
  }

  return rval;
}
#endif


ClauseRef
findClause(ClauseRef cref, Word argv,
	   LocalFrame fr, Definition def, ClauseRef *next)
{
#ifdef O_LOGICAL_UPDATE
  unsigned long gen = fr->generation;
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
  { GET_LD
    word key = indexOfWord(*argv PASS_LD);

    if ( !key )
      goto noindex;

    for(;cref ; cref = cref->next)
    { Clause clause = cref->clause;

      if ( (key & clause->index.varmask) == clause->index.key &&
	   visibleClause(clause, gen))
      { ClauseRef result = cref;
      
	for( cref = cref->next; cref; cref = cref->next )
	{ clause = cref->clause;
	  if ( (key&clause->index.varmask) == clause->index.key &&
	       visibleClause(clause, gen))
	  { *next = cref;

	    return result;
	  }
	}
	*next = NULL;

	return result;
      }
    }
    return NULL;
  } else if ( def->indexPattern & NEED_REINDEX )
  { assert(def->hash_info == NULL);
    reindexDefinition(def);
    return findClause(cref, argv, fr, def, next);
  } else
  { struct index argIndex;
    GET_LD

    getIndex(argv, def->indexPattern, def->indexCardinality, &argIndex
	     PASS_LD);
    for(; cref; cref = cref->next)
    { if ( matchIndex(argIndex, cref->clause->index) &&
	   visibleClause(cref->clause, gen))
      { ClauseRef result = cref;
      
	for( cref = cref->next; cref; cref = cref->next )
	{ if ( matchIndex(argIndex, cref->clause->index) &&
	       visibleClause(cref->clause, gen))
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
}


static ClauseRef
nextClause(ClauseRef cref, unsigned long generation,
	   ClauseRef *next, Index ctx ARG_LD)
{ if ( ctx->varmask == ~0x0L )		/* first argument only */
  { word key = ctx->key;

    for(;cref ; cref = cref->next)
    { Clause clause = cref->clause;

      if ( (key & clause->index.varmask) == clause->index.key &&
	   visibleClause(clause, generation))
      { ClauseRef result = cref;
      
	for( cref = cref->next; cref; cref = cref->next )
	{ clause = cref->clause;
	  if ( (key&clause->index.varmask) == clause->index.key &&
	       visibleClause(clause, generation))
	  { *next = cref;

	    return result;
	  }
	}
	*next = NULL;

	return result;
      }
    }
  } else if ( ctx->varmask == 0x0L )	/* no indexing */
  { for(; cref; cref = cref->next)
    { if ( visibleClause(cref->clause, generation) )
      { *next = cref->next;
        return cref;
      }
    }
  } else				/* general (multi-arg) indexing */
  { DEBUG(2, Sdprintf("Multi-argument indexing on %s ...",
		      cref ? procedureName(cref->clause->procedure) : "?"));
    for(; cref; cref = cref->next)
    { if ( matchIndex(*ctx, cref->clause->index) &&
	   visibleClause(cref->clause, generation))
      { ClauseRef result = cref;
      
	for( cref = cref->next; cref; cref = cref->next )
	{ if ( matchIndex(*ctx, cref->clause->index) &&
	       visibleClause(cref->clause, generation))
	  { *next = cref;

	    DEBUG(2, Sdprintf("ndet\n"));
	    return result;
	  }
	}
        DEBUG(2, Sdprintf("det\n"));
	*next = NULL;

	return result;
      }
    }
    DEBUG(2, Sdprintf("NULL\n"));
  }

  return NULL;
}


ClauseRef
firstClause(Word argv, LocalFrame fr, Definition def, ClauseRef *next ARG_LD)
{ ClauseRef cref;
  struct index buf;
  Index ctx = &buf;

#ifdef O_LOGICAL_UPDATE
  unsigned long gen = fr->generation;
#else
  #define gen 0L
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

    ctx->key     = key;
    ctx->varmask = (unsigned long) ~0x0L;
    if ( def->hash_info )
    { int hi = hashIndex(key, def->hash_info->buckets);

      cref = def->hash_info->entries[hi].head;
    } else
      cref = def->definition.clauses;
  } else if ( def->indexPattern & NEED_REINDEX )
  { reindexDefinition(def);
    goto again;
  } else
  { getIndex(argv, def->indexPattern, def->indexCardinality, ctx PASS_LD);
    cref = def->definition.clauses;
  }

  return nextClause(cref, gen, next, ctx PASS_LD);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Recalculate the index of  a  clause  after  the  index  pattern  on  the
predicate  has been changed.  The head of the clause is decompiled.  The
resulting term is simply discarded as it cannot have links to any  other
part of the stacks (e.g. backtrailing is not needed).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
reindexClause(Clause clause)
{ Procedure proc = clause->procedure;
  unsigned long pattern = proc->definition->indexPattern & ~NEED_REINDEX;

  if ( pattern == 0x0 )
    succeed;
  if ( false(clause, ERASED) )
  { if ( pattern == 0x1 )		/* the 99.9% case.  Speedup a little */
    { word key;

      if ( arg1Key(clause, &key) )
      { clause->index.key     = key;
	clause->index.varmask = (unsigned long)~0L;
      } else
      { clause->index.key     = 0L;
	clause->index.varmask = 0L;
      }
    } else
    { GET_LD
  
      fid_t fid = PL_open_foreign_frame();
      term_t head = PL_new_term_ref();

      decompileHead(clause, head);
      getIndex(argTermP(*valTermRef(head), 0),
	       pattern,
	       proc->definition->indexCardinality,
	       &clause->index
	       PASS_LD);
      PL_discard_foreign_frame(fid);
    }
  }

  succeed;
}


bool
unify_index_pattern(Procedure proc, term_t value)
{ Definition def = proc->definition;
  unsigned long pattern = (def->indexPattern & ~NEED_REINDEX);
  int n, arity = def->functor->arity;

  if ( pattern == 0 )
    fail;

  if ( PL_unify_functor(value, def->functor->functor) )
  { term_t a = PL_new_term_ref();

    for(n=0; n<arity; n++, pattern >>= 1)
    { if ( !PL_get_arg(n+1, value, a) ||
	   !PL_unify_integer(a, (pattern & 0x1) ? 1 : 0) )
	fail;
    }

    succeed;
  }

  fail;
}

		 /*******************************
		 *	   HASH SUPPORT		*
		 *******************************/

static ClauseIndex
newClauseIndexTable(int buckets)
{ ClauseIndex ci = allocHeap(sizeof(struct clause_index));
  ClauseChain ch;
  int m = 4;

  while(m<buckets)
    m *= 2;
  buckets = m;

  ci->buckets  = buckets;
  ci->size     = 0;
  ci->alldirty = FALSE;
  ci->entries  = allocHeap(sizeof(struct clause_chain) * buckets);
  
  for(ch = ci->entries; buckets; buckets--, ch++)
  { ch->head = ch->tail = NULL;
    ch->dirty = 0;
  }

  return ci;
}


void
unallocClauseIndexTable(ClauseIndex ci)
{ ClauseChain ch;
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
appendClauseChain(ClauseChain ch, Clause cl, int where)
{ ClauseRef cr = newClauseRef(cl);

  if ( !ch->tail )
    ch->head = ch->tail = cr;
  else
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
gcClauseChain(ClauseChain ch, int dirty)
{ ClauseRef cref = ch->head, prev = NULL;
  int deleted = 0;

  while( cref && dirty != 0 )
  { if ( true(cref->clause, ERASED) )
    { ClauseRef c = cref;
      
      if ( dirty > 0 )
      { assert(c->clause->index.varmask != 0); /* must be indexed */
	deleted++;
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

      freeClauseRef(c);
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
gcClauseIndex(ClauseIndex ci)
{ ClauseChain ch = ci->entries;
  int n = ci->buckets;
    
  if ( ci->alldirty )
  { for(; n; n--, ch++)
      ci->size -= gcClauseChain(ch, -1); /* do them all */
  } else
  { for(; n; n--, ch++)
    { if ( ch->dirty )
	ci->size -= gcClauseChain(ch, ch->dirty);
    }
  }
}


void
markDirtyClauseIndex(ClauseIndex ci, Clause cl)
{ if ( cl->index.varmask == 0 )
    ci->alldirty = TRUE;
  else
  { int hi = hashIndex(cl->index.key, ci->buckets);
    ci->entries[hi].dirty++;
  }
}


void
addClauseToIndex(Definition def, Clause cl, int where)
{ ClauseIndex ci = def->hash_info;
  ClauseChain ch = ci->entries;

  if ( cl->index.varmask == 0 )		/* a non-indexable field */
  { int n = ci->buckets;

    SECURE({ word k;
	     assert(!arg1Key(cl, &k));
	   });
    
    DEBUG(1,
	  if ( def->indexPattern == 0x1 )
	    Sdprintf("*** Adding unindexed clause to index of %s\n",
		     predicateName(def)));

    for(; n; n--, ch++)
      appendClauseChain(ch, cl, where);
  } else
  { int hi = hashIndex(cl->index.key, ci->buckets);
    
    DEBUG(4, Sdprintf("Storing in bucket %d\n", hi));
    appendClauseChain(&ch[hi], cl, where);

    if ( ++ci->size / 2 > ci->buckets )
    { enterDefinition(def);
      set(def, NEEDSREHASH);
      leaveDefinition(def);
    }
  }
}


void
delClauseFromIndex(ClauseIndex ci, Clause cl)
{ ClauseChain ch = ci->entries;

  if ( cl->index.varmask == 0 )		/* a non-indexable field */
  { int n = ci->buckets;
    
    for(; n; n--, ch++)
      deleteClauseChain(ch, cl);
  } else
  { int hi = hashIndex(cl->index.key, ci->buckets);
    
    deleteClauseChain(&ch[hi], cl);
    ci->size--;
  }
}


bool
hashDefinition(Definition def, int buckets)
{ ClauseRef cref;

  if ( true(def, FOREIGN) )
    fail;
  if ( def->indexPattern != 0x1 )
    fail;

  DEBUG(2, Sdprintf("hashDefinition(%s, %d)\n", predicateName(def), buckets));

  LOCK();
  def->hash_info = newClauseIndexTable(buckets);

  for(cref = def->definition.clauses; cref; cref = cref->next)
  { if ( false(cref->clause, ERASED) )
      addClauseToIndex(def, cref->clause, CL_END);
  }
  UNLOCK();

  succeed;

}

word
pl_hash(term_t pred)
{ Procedure proc;

  if ( get_procedure(pred, &proc, 0, GP_CREATE) )
  { Definition def = proc->definition;

    if ( false(def, FOREIGN) && def->indexPattern & NEED_REINDEX )
      reindexDefinition(def);

    return hashDefinition(def, 256);
  }

  fail;
}
