/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Keri Harris
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-funct.h"
#include "pl-fli.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functor (name/arity) handling.  A functor is a unique object (like atoms).
See pl-atom.c for many useful comments on the representation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef LD
#define LD LOCAL_LD

#define functorDefTable (GD->functors.table)

#ifdef O_PLMT

#define acquire_functor_table(t, b) \
  { FunctorTable _ft; \
    do \
    { _ft = LOAD_SEQ_CST(functorDefTable); \
      PUBLISH_HAZARD(LD->thread.info->access.functor_table, _ft); \
    } while ( _ft != LOAD_SEQ_CST(functorDefTable) ); \
    t = _ft->table; \
    b = _ft->buckets; \
  }

#define release_functor_table() \
  { LD->thread.info->access.functor_table = NULL; \
  }

#else

#define acquire_functor_table(t, b) \
  { t = functorDefTable->table; \
    b = functorDefTable->buckets; \
  }

#define release_functor_table() (void)0

#endif

static void	  allocFunctorTable(void);
static void	  rehashFunctors(void);

static void
allocateFunctorBlock(int idx)
{ PL_LOCK(L_MISC);

  if ( !GD->functors.array.blocks[idx] )
  { size_t bs = (size_t)1<<idx;
    FunctorDef *newblock;

    if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(FunctorDef))) )
      outOfCore();

    memset(newblock, 0, bs*sizeof(FunctorDef));
    GD->functors.array.blocks[idx] = newblock-bs;
  }

  PL_UNLOCK(L_MISC);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functor garbage collection

Functors are collected by atom garbage collection (AGC).  A functor is
alive if it is

  - permanent (FUNCTOR_PERMANENT_REF).  This is the default for the
    kernel: lookupFunctorDef() returns permanent functors, so kernel
    code may keep them, as it always could.  Procedures, record keys,
    trie keys and flag/3 keys make their functor permanent.  A count
    that reaches FUNCTOR_REF_SATURATED also makes it permanent.
  - referenced (FUNCTOR_REF_COUNT()) by clauses, records and foreign
    code.  See acquireFunctor() and releaseFunctor().  The public
    PL_new_functor() adds a reference (lookupFunctorRef()), as
    PL_new_atom() does.  PL_register_functor() and
    PL_unregister_functor() add and remove references.
  - marked by AGC (FUNCTOR_MARKED_REF) from the stacks, findall bags and
    message queues, or from a thread's `volatile_functor`.

Only lookupVolatileFunctor() creates functors that are not permanent.
It is used by predicates that build terms from data, such as =../2,
functor/3 and read/1.  The caller must store the functor in a term on
the global stack before it calls lookupVolatileFunctor() again.  Until
then, LD->atoms.volatile_functor protects it.  If AGC is active, the
functor is also marked, as the stacks may already have been scanned.

collectFunctors() runs under L_FUNCTOR.  It sets FUNCTOR_DESTROY_REF on
all candidates, rechecks the volatile functor of all threads and frees
the remaining candidates.  A lock-free lookup that finds a functor
publishes it as volatile functor and then checks it is not destroyed.
So, either the collector sees the volatile functor, or the lookup sees
FUNCTOR_DESTROY_REF and retries under L_FUNCTOR.  This is the hazard
pointer protocol.

The struct functorDef of a collected functor is not freed, but kept for
reuse by a new functor with the same index, so a lock-free lookup never
accesses freed memory.  A lookup that walks a chain through a reused
functor may miss the functor it looks for and retries under L_FUNCTOR.
As it compares the name and arity, it cannot find a wrong functor.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) The first two may  not  be   reordered  because  lookup  will return
fd->functor if it finds a valid functor. The second barrier ensures only
valid functors appear in the array.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
addFunctorToArray(FunctorDef fd)
{ size_t index;
  int idx;
  size_t amask;

  index = ATOMIC_INC(&GD->functors.highest) - 1;
  idx = MSB(index);

  if ( !GD->functors.array.blocks[idx] )
  { allocateFunctorBlock(idx);
  }

  amask = (fd->arity < F_ARITY_MASK ? fd->arity : F_ARITY_MASK);
  fd->functor = MK_FUNCTOR(index, amask);
  GD->functors.array.blocks[idx][index] = fd;
  MEMORY_RELEASE();			/* See (*) */
  fd->flags |= VALID_F;

  DEBUG(CHK_SECURE, assert(fd->arity == arityFunctor(fd->functor)));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lookup is lock-free.  Creating a functor  and rehashing the table happen
under L_FUNCTOR.  A new functor is  added   to  the  array (which is what
rehashFunctors() uses) before it is published   in  the table.  A lookup
may miss an existing functor while  rehashFunctors()  relinks the chains.
Therefore we search again under L_FUNCTOR before   we  conclude that the
functor does not exist.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define find_functor(atom, arity) LDFUNC(find_functor, atom, arity)

static FunctorDef
find_functor(DECL_LD atom_t atom, size_t arity)
{ FunctorDef *table;
  int buckets;
  FunctorDef rc = NULL;

  acquire_functor_table(table, buckets);
  int v = (int)pointerHashValue(atom, buckets);
  for(FunctorDef f = table[v]; f; f = f->next)
  { if ( atom == f->name && f->arity == arity &&
	 FUNCTOR_IS_VALID(f->flags) )
    { rc = f;
      break;
    }
  }
  release_functor_table();

  return rc;
}

/* Find a functor in the current table.  Caller holds L_FUNCTOR.  The
   table holds no destroyed functors as collectFunctors() removes them
   while holding L_FUNCTOR.
*/

static FunctorDef
find_functor_locked(atom_t atom, size_t arity)
{ FunctorTable t = functorDefTable;
  int v = (int)pointerHashValue(atom, t->buckets);

  for(FunctorDef f = t->table[v]; f; f = f->next)
  { if ( atom == f->name && f->arity == arity )
      return f;
  }

  return NULL;
}

/* True if `f`, found by a lock-free lookup, is the live functor
   name/arity.  Called after publishing our interest in `f`.
*/

static inline bool
is_live_functor(FunctorDef f, atom_t atom, size_t arity)
{ return ( !(LOAD_SEQ_CST(f->references) & FUNCTOR_DESTROY_REF) &&
	   FUNCTOR_IS_VALID(LOAD_SEQ_CST(f->flags)) &&
	   f->name == atom && f->arity == arity );
}

/* Create a new functor.  Caller holds L_FUNCTOR and checked it does not
   exist.
*/

static FunctorDef
create_functor(atom_t atom, size_t arity, unsigned int refs)
{ if ( functorDefTable->buckets * 2 < GD->statistics.functors )
    rehashFunctors();

  FunctorTable t = functorDefTable;
  int v = (int)pointerHashValue(atom, t->buckets);
  FunctorDef f;

  if ( (f=GD->functors.free) )		/* reuse a collected functor */
  { size_t amask = (arity < F_ARITY_MASK ? arity : F_ARITY_MASK);

    GD->functors.free = f->next_free;
    f->next_free  = NULL;
    f->name       = atom;
    f->arity      = arity;
    f->references = refs;
    f->functor    = MK_FUNCTOR(indexFunctor(f->functor), amask);
    f->next       = t->table[v];
    MEMORY_RELEASE();
    f->flags      = VALID_F;
  } else
  { f = (FunctorDef) allocHeapOrHalt(sizeof(struct functorDef));
    f->functor    = 0L;
    f->name       = atom;
    f->arity      = arity;
    f->flags      = 0;
    f->references = refs;
    f->next_free  = NULL;
    f->next       = t->table[v];
    addFunctorToArray(f);
  }
  MEMORY_RELEASE();			/* publish complete functor */
  t->table[v] = f;

  ATOMIC_INC(&GD->statistics.functors);
  PL_register_atom(atom);
  DEBUG(9, Sdprintf("%p (new)\n", f));

  return f;
}


/* Lookup or create the functor name/arity.  The functor is permanent */

functor_t
lookupFunctorDef(DECL_LD atom_t atom, size_t arity)
{ FunctorDef f;

  if ( (f=find_functor(atom, arity)) )
  { unsigned int ref = f->references;

    if ( (ref&FUNCTOR_PERMANENT_REF) )	/* permanent functors are never */
      return f->functor;		/* collected nor reused */
    ATOMIC_OR(&f->references, FUNCTOR_PERMANENT_REF);
    if ( is_live_functor(f, atom, arity) )
      return f->functor;
  }

  PL_LOCK(L_FUNCTOR);
  if ( (f=find_functor_locked(atom, arity)) )
    ATOMIC_OR(&f->references, FUNCTOR_PERMANENT_REF);
  else
    f = create_functor(atom, arity, FUNCTOR_PERMANENT_REF);
  PL_UNLOCK(L_FUNCTOR);

  return f->functor;
}


/* Lookup or create the functor name/arity and add a counted reference.
   This is PL_new_functor() for foreign code, which must call
   PL_unregister_functor() if it no longer needs the functor.
*/

functor_t
lookupFunctorRef(DECL_LD atom_t atom, size_t arity)
{ FunctorDef f;

  if ( (f=find_functor(atom, arity)) )
  { for(;;)
    { unsigned int ref = f->references;

      if ( (ref&FUNCTOR_PERMANENT_REF) )
      { if ( f->name == atom && f->arity == arity &&
	     FUNCTOR_IS_VALID(f->flags) )
	  return f->functor;		/* permanent: never reused */
	break;
      }
      if ( (ref&FUNCTOR_DESTROY_REF) )
	break;
      unsigned int nref = ref+1;
      if ( FUNCTOR_REF_COUNT(nref) >= FUNCTOR_REF_SATURATED )
	nref |= FUNCTOR_PERMANENT_REF;
      if ( COMPARE_AND_SWAP_UINT(&f->references, ref, nref) )
      { if ( f->name == atom && f->arity == arity &&
	     FUNCTOR_IS_VALID(LOAD_SEQ_CST(f->flags)) )
	  return f->functor;
	releaseFunctor(f->functor);	/* collected and reused */
	break;
      }
    }
  }

  PL_LOCK(L_FUNCTOR);			/* collectFunctors() holds this */
  if ( (f=find_functor_locked(atom, arity)) )
    acquireFunctor(f->functor);
  else
    f = create_functor(atom, arity, 1);
  PL_UNLOCK(L_FUNCTOR);

  return f->functor;
}


/* Publish `f` as the volatile functor of this thread.  See the start of
   this section.
*/

#define publish_volatile_functor(f) LDFUNC(publish_volatile_functor, f)

static inline void
publish_volatile_functor(DECL_LD functor_t f)
{ PUBLISH_HAZARD(LD->atoms.volatile_functor, f);
  if ( LOAD_SEQ_CST(GD->atoms.gc_active) )
    markFunctor(f);
}

static void
considerFunctorGC(void)
{ if ( GD->atoms.margin != 0 &&
       ATOMIC_INC(&GD->functors.collectable) % GD->atoms.margin == 0 )
    signalGCThread(SIG_ATOM_GC);
}

/* Lookup or create the functor name/arity for building a term.  The
   functor may be collected if it is not referenced.  The caller must
   put it on the global stack before calling us again.
*/

functor_t
lookupVolatileFunctor(DECL_LD atom_t atom, size_t arity)
{ FunctorDef f;

  if ( (f=find_functor(atom, arity)) )
  { functor_t ft = f->functor;

    if ( (f->references&FUNCTOR_PERMANENT_REF) ||
	 LD->atoms.volatile_functor == ft ) /* already protected */
      return ft;
    publish_volatile_functor(ft);
    if ( is_live_functor(f, atom, arity) && f->functor == ft )
      return ft;
  }

  bool new = false;
  PL_LOCK(L_FUNCTOR);
  if ( !(f=find_functor_locked(atom, arity)) )
  { f = create_functor(atom, arity, 0);
    new = true;
  }
  publish_volatile_functor(f->functor);
  PL_UNLOCK(L_FUNCTOR);
  if ( new )
    considerFunctorGC();

  return f->functor;
}


/* Make `f` permanent */

void
permanentFunctor(functor_t f)
{ if ( unlikely(GD->halt.cleaning == CLN_DATA) )
    return;				/* see cleanupFunctors() */
  FunctorDef fd = valueFunctor(f);

  if ( !(fd->references&FUNCTOR_PERMANENT_REF) )
    ATOMIC_OR(&fd->references, FUNCTOR_PERMANENT_REF);
}


/* Add/remove a counted reference to `f`.  As the caller has a reference
   from a term or another counted reference, `f` cannot be destroyed.
*/

void
acquireFunctor(functor_t f)
{ if ( unlikely(GD->halt.cleaning == CLN_DATA) )
    return;				/* see cleanupFunctors() */
  FunctorDef fd = valueFunctor(f);

  if ( !(fd->references&FUNCTOR_PERMANENT_REF) &&
       FUNCTOR_REF_COUNT(ATOMIC_INC(&fd->references)) >= FUNCTOR_REF_SATURATED )
    ATOMIC_OR(&fd->references, FUNCTOR_PERMANENT_REF); /* avoid overflow */
}

void
releaseFunctor(functor_t f)
{ if ( unlikely(GD->halt.cleaning == CLN_DATA) )
    return;				/* see cleanupFunctors() */
  FunctorDef fd = valueFunctor(f);

  if ( !(fd->references&FUNCTOR_PERMANENT_REF) )
  { DEBUG(0, assert(FUNCTOR_REF_COUNT(fd->references) > 0));
    ATOMIC_DEC(&fd->references);
  }
}


/* Mark `w` if it is a functor.  Called by AGC for words that look like
   functors.  We must be prepared for garbage.
*/

void
markFunctor(functor_t w)
{ w &= ~(functor_t)(MARK_MASK|FIRST_MASK); /* visited by a term walker */
  size_t i = indexFunctor(w);

  if ( i >= GD->functors.builtin && i < GD->functors.highest )
  { FunctorDef *b = GD->functors.array.blocks[MSB(i)];
    FunctorDef f;

    if ( b && (f=b[i]) && f->functor == (functor_t)w )
    { unsigned int ref = f->references;

      if ( !(ref&(FUNCTOR_MARKED_REF|FUNCTOR_PERMANENT_REF)) )
	ATOMIC_OR(&f->references, FUNCTOR_MARKED_REF);
    }
  }
}


/* Remove `f` from the chain of the current table.  Lock-free lookups
   that are at `f` continue with the rest of the chain.
*/

static void
unlink_functor(FunctorDef f)
{ FunctorTable t = functorDefTable;
  FunctorDef *fp = &t->table[pointerHashValue(f->name, t->buckets)];

  for(; *fp; fp = &(*fp)->next)
  { if ( *fp == f )
    { *fp = f->next;
      return;
    }
  }
  assert(0);
}


static void
keep_volatile_functor(PL_local_data_t *ld, void *ctx)
{ functor_t w = LOAD_SEQ_CST(ld->atoms.volatile_functor);
  bool *hidden = ctx;

  if ( (LOAD_SEQ_CST(ld->atoms.functor_hide)&1) )
    *hidden = true;			/* see startHideFunctors() */

  if ( w )
  { FunctorDef f = valueFunctor(w);

    if ( f->functor == w && (f->references&FUNCTOR_DESTROY_REF) )
      ATOMIC_AND(&f->references, ~FUNCTOR_DESTROY_REF);
  }
}


/* Collect the functors that are not permanent, referenced or marked.
   Called by AGC after marking.  Returns the number of collected
   functors.  Their name atoms are unregistered, so AGC may collect
   them.
*/

size_t
collectFunctors(void)
{ GET_LD
  size_t collected = 0;
  tmp_buffer candidates;

  initBuffer(&candidates);
  PL_LOCK(L_FUNCTOR);
  size_t high = GD->functors.highest;
  for(size_t i=GD->functors.builtin; i<high; i++)
  { FunctorDef *b = GD->functors.array.blocks[MSB(i)];
    FunctorDef f;

    if ( !b || !(f=b[i]) || !FUNCTOR_IS_VALID(f->flags) )
      continue;
    for(;;)
    { unsigned int ref = f->references;

      if ( (ref&FUNCTOR_PERMANENT_REF) )
	break;
      if ( (ref&FUNCTOR_MARKED_REF) || FUNCTOR_REF_COUNT(ref) )
      { if ( (ref&FUNCTOR_MARKED_REF) )
	  ATOMIC_AND(&f->references, ~FUNCTOR_MARKED_REF);
	break;
      }
      if ( COMPARE_AND_SWAP_UINT(&f->references, ref,
				 ref|FUNCTOR_DESTROY_REF) )
      { addBuffer(&candidates, f, FunctorDef);
	break;
      }
    }
  }

  if ( !isEmptyBuffer(&candidates) )
  { bool hidden = false;

    MEMORY_BARRIER();
    keep_volatile_functor(LD, &hidden);
#ifdef O_PLMT
    forThreadLocalDataUnsuspended(keep_volatile_functor, &hidden);
#endif

    FunctorDef *fp = baseBuffer(&candidates, FunctorDef);
    FunctorDef *ep = topBuffer(&candidates, FunctorDef);
    for(; fp < ep; fp++)
    { FunctorDef f = *fp;

      unsigned int ref = LOAD_SEQ_CST(f->references);

      if ( hidden || (ref&FUNCTOR_MARKED_REF) )
      { ATOMIC_AND(&f->references,	/* hidden or restored by a */
		   ~(FUNCTOR_DESTROY_REF|FUNCTOR_MARKED_REF)); /* walker */
      } else if ( (ref&FUNCTOR_DESTROY_REF) )
      { unlink_functor(f);
	f->flags = 0;			/* clear VALID_F */
	PL_unregister_atom(f->name);
	f->name = ATOM_garbage_collected; /* make stale use visible */
	f->next_free = GD->functors.free;
	GD->functors.free = f;
	collected++;
      }
    }
  }
  GD->statistics.functors -= (int)collected;
  GD->functors.collected += collected;
  GD->functors.collectable = 0;
  PL_UNLOCK(L_FUNCTOR);
  discardBuffer(&candidates);

  return collected;
}


/* Number of functors that were collected */

int64_t
collectedFunctors(void)
{ return GD->functors.collected;
}


static void
maybe_free_functor_tables(void)
{ FunctorTable t = functorDefTable;

  while ( t )
  { FunctorTable t2 = t->prev;
    if ( t2 && !pl_functor_table_in_use(t2) )
    { t->prev = t2->prev;
      freeHeap(t2->table, t2->buckets * sizeof(FunctorDef));
      freeHeap(t2, sizeof(functor_table));
    }
    t = t->prev;
  }
}


static void
rehashFunctors(void)
{ FunctorTable newtab;
  size_t index;
  int i, last = false;

  if ( functorDefTable->buckets * 2 >= GD->statistics.functors )
    return;

  newtab = allocHeapOrHalt(sizeof(*newtab));
  newtab->buckets = functorDefTable->buckets * 2;
  newtab->table = allocHeapOrHalt(newtab->buckets * sizeof(FunctorDef));
  memset(newtab->table, 0, newtab->buckets * sizeof(FunctorDef));
  newtab->prev = functorDefTable;

  DEBUG(MSG_HASH_STAT,
	Sdprintf("Rehashing functor-table (%d --> %d)\n",
		 functorDefTable->buckets, newtab->buckets));

  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    FunctorDef *b;

    if ( (b=GD->functors.array.blocks[i]) )
    { size_t high = GD->functors.highest;

      if ( upto >= high )
      { upto = high;
	last = true;
      }

      for(; index<upto; index++)
      { FunctorDef f = b[index];

	if ( f && FUNCTOR_IS_VALID(f->flags) )
	{ size_t v = pointerHashValue(f->name, newtab->buckets);

	  f->next = newtab->table[v];
	  newtab->table[v] = f;
	}
      }
    }
  }

  MEMORY_RELEASE();			/* publish filled table */
  functorDefTable = newtab;
  maybe_free_functor_tables();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is lookupFunctorDef(), but failing (returns   0)  if the functor is
not known.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

functor_t
isCurrentFunctor(atom_t atom, size_t arity)
{ GET_LD
  functor_t rc;

  FunctorDef f;

  if ( (f=find_functor(atom, arity)) )
  { rc = f->functor;
  } else
  { PL_LOCK(L_FUNCTOR);			/* we may miss it during a rehash */
    if ( (f=find_functor_locked(atom, arity)) )
      rc = f->functor;
    else
      rc = 0;
    PL_UNLOCK(L_FUNCTOR);
  }

  return rc;
}

typedef struct
{ atom_t name;
  char   arity;
} builtin_functor;

#define FUNCTOR(n, a) { n, a }
static const builtin_functor functors[] = {
#include "pl-funct.ic"
FUNCTOR(NULL_ATOM, 0)
};
#undef FUNCTOR


static void
allocFunctorTable(void)
{ functorDefTable = allocHeapOrHalt(sizeof(*functorDefTable));
  functorDefTable->buckets = FUNCTORHASHSIZE;
  functorDefTable->table = allocHeapOrHalt(FUNCTORHASHSIZE * sizeof(FunctorDef));
  memset(functorDefTable->table, 0, FUNCTORHASHSIZE * sizeof(FunctorDef));
  functorDefTable->prev = NULL;
}


static void
registerBuiltinFunctors(void)
{ int size = sizeof(functors)/sizeof(builtin_functor) - 1;
  FunctorDef f = allocHeapOrHalt(size * sizeof(struct functorDef));
  const builtin_functor *d;

  GD->statistics.functors = size;

  for(d = functors; d->name; d++, f++)
  { size_t v = pointerHashValue(d->name, functorDefTable->buckets);

    f->name             = d->name;
    f->arity            = d->arity;
    f->flags		= 0;
    f->references	= FUNCTOR_PERMANENT_REF;
    f->next_free	= NULL;
    f->next             = functorDefTable->table[v];
    functorDefTable->table[v]  = f;
    addFunctorToArray(f);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functors are compiled with compileBody().   Make sure this is kept
consistent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerControlFunctors()
{ static functor_t control[] =
  { FUNCTOR_comma2,
    FUNCTOR_semicolon2,
    FUNCTOR_bar2,
    FUNCTOR_ifthen2,
    FUNCTOR_softcut2,
    FUNCTOR_not_provable1,
    FUNCTOR_colon2,			/* Module:Goal */
    FUNCTOR_dollar1,			/* $(Goal) */
#ifdef O_CALL_AT_MODULE
    FUNCTOR_at_sign2,			/* Goal@Module */
#endif
    (functor_t) 0
  };
  functor_t *f;

  for(f	= control; *f; f++)
  { valueFunctor(*f)->flags |= CONTROL_F;
  }
}


static void
registerArithFunctors()
{ static functor_t arith[] =
  { FUNCTOR_ar_equals2,
    FUNCTOR_ar_not_equal2,
    FUNCTOR_smaller2,
    FUNCTOR_larger2,
    FUNCTOR_smaller_equal2,
    FUNCTOR_larger_equal2,
    FUNCTOR_is2,
    (functor_t) 0
  };
  functor_t *f;

  for(f	= arith; *f; f++)
  { valueFunctor(*f)->flags |= ARITH_F;
  }
}


void
initFunctors(void)
{ PL_LOCK(L_FUNCTOR);
  if ( !functorDefTable )
  { initAtoms();
    allocFunctorTable();
    GD->functors.highest = 1;
    registerBuiltinFunctors();
    GD->functors.builtin = GD->functors.highest;
    registerControlFunctors();
    registerArithFunctors();
  }
  PL_UNLOCK(L_FUNCTOR);
}


void
cleanupFunctors(void)
{ FunctorTable table = functorDefTable;
				/* permanentFunctor(), acquireFunctor() and */
				/* releaseFunctor() are no-ops during CLN_DATA, */
				/* as clauses and records may be freed later */

  if ( table )
  { int i;
    int builtin_count      = sizeof(functors)/sizeof(builtin_functor) - 1;
    FunctorDef builtin     = GD->functors.array.blocks[0][1];
    FunctorDef builtin_end = builtin+builtin_count;
    FunctorDef *fp0;

    freeHeap(builtin, builtin_count * sizeof(struct functorDef));

    for(i=0; (fp0=GD->functors.array.blocks[i]); i++)
    { size_t bs = (size_t)1<<i;
      size_t upto = (size_t)2<<i;
      size_t high = GD->functors.highest;
      FunctorDef *fp, *ep;

      fp0 += bs;
      fp = fp0;
      ep=fp+bs;
      if ( upto > high )
	ep -= upto-high;

      for(; fp<ep; fp++)
      { FunctorDef f = *fp;

	if ( !(f>=builtin && f<=builtin_end) )
	  freeHeap(f, sizeof(*f));
      }

      GD->functors.array.blocks[i] = NULL;
      PL_free(fp0);
    }

    while ( table )
    { FunctorTable prev = table->prev;
      freeHeap(table->table, table->buckets * sizeof(FunctorDef));
      freeHeap(table, sizeof(functor_table));
      table = prev;
    }
    table = NULL;
  }
}


#if TEST
checkFunctors()
{ FunctorDef f;
  int n;

  for( n=0; n < functor_buckets; n++ )
  { f = functorDefTable[n];
    for( ;f ; f = f->next )
    { if ( f->arity < 0 || f->arity > 10 )	/* debugging only ! */
        Sdprintf("[ERROR: Functor %ld has dubious arity: %d]\n", f, f->arity);
      if ( !isArom(f->name) )
        Sdprintf("[ERROR: Functor %ld has illegal name: %ld]\n", f, f->name);
      if ( !( f->next == (FunctorDef) NULL ||
	      inCore(f->next)) )
	Sdprintf("[ERROR: Functor %ld has illegal next: %ld]\n", f, f->next);
    }
  }
}
#endif

foreign_t
pl_current_functor(term_t name, term_t arity, control_t h)
{ GET_LD
  atom_t nm = 0;
  size_t index;
  int i, last=false;
  int  ar;
  fid_t fid;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_get_atom(name, &nm) &&
	   PL_get_integer(arity, &ar) )
	return isCurrentFunctor(nm, ar) ? true : false;

      if ( !(PL_is_integer(arity) || PL_is_variable(arity)) )
	return PL_error("current_functor", 2, NULL, ERR_TYPE,
			ATOM_integer, arity);

      if ( !(PL_is_atom(name) || PL_is_variable(name)) )
	return PL_error("current_functor", 2, NULL, ERR_TYPE,
			ATOM_atom, name);
      index = 1;
      break;
    case FRG_REDO:
      PL_get_atom(name, &nm);
      index = ForeignContextInt(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  fid = PL_open_foreign_frame();
  PL_LOCK(L_FUNCTOR);
  for(i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->functors.highest;
    FunctorDef *b = GD->functors.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = true;
    }

    for(; index<upto; index++)
    { FunctorDef fd = b[index];

      if ( fd && FUNCTOR_IS_VALID(fd->flags) &&
           (!nm || nm == fd->name) )
      { if ( PL_unify_atom(name, fd->name) &&
	     PL_unify_integer(arity, fd->arity) )
	{ PL_UNLOCK(L_FUNCTOR);
	  ForeignRedoInt(index+1);
	} else
	{ PL_rewind_foreign_frame(fid);
	}
      }
    }
  }

  PL_UNLOCK(L_FUNCTOR);
  return false;
}


size_t
functor_space(void)
{ size_t size = ((size_t)2<<MSB(GD->functors.highest))*sizeof(FunctorDef);

  size += GD->functors.highest * sizeof(struct functorDef);

  return size;
}
