/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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
#include "pl-idgset.h"

/* See pl-idgset.h for the representation.  Positions are unsigned; a
   set holds at most 2^31 entries.
*/

#define is_single(s)	((s)&IDG_SET_SINGLE)
#define single_ptr(s)	((uintptr_t)(s)&~(uintptr_t)IDG_SET_SINGLE)
#define set_block(s)	((idg_set_block*)(s))
#define entry_ptr(e)	((e)&~(uintptr_t)IDG_SET_DEAD)
#define is_dead(e)	((e)&IDG_SET_DEAD)

static inline uint32_t *
block_index(const idg_set_block *b)
{ return (uint32_t*)&b->entries[b->capacity];
}

static inline size_t
sizeof_block(unsigned capacity, unsigned index_bits)
{ return ( sizeof(idg_set_block) +
	   capacity*sizeof(uintptr_t) +
	   (index_bits ? ((size_t)1<<index_bits)*sizeof(uint32_t) : 0) );
}

static inline unsigned
hash_slot(uintptr_t p, unsigned bits)
{ uint64_t h = (uint64_t)p * 0x9e3779b97f4a7c15ULL;

  return (unsigned)(h >> (64-bits));
}


		 /*******************************
		 *	    WRITE LOCKING	*
		 *******************************/

/* Writers of a set that may be accessed by other threads lock a stripe.
   We share the stripes with the concurrent tries.  A writer never holds
   two stripes.
*/

#ifdef O_PLMT
static inline simpleMutex *
lock_set(const idg_set *set, idg_set_block **lingering)
{ if ( lingering )
  { uint64_t h = (uint64_t)(uintptr_t)set * 0x9e3779b97f4a7c15ULL;
    simpleMutex *m = &GD->tries.stripes[h >> (64-8)];

    static_assertion(TRIE_STRIPES == 256);
    simpleMutexLock(m);
    return m;
  }

  return NULL;
}

static inline void
unlock_set(simpleMutex *m)
{ if ( m )
    simpleMutexUnlock(m);
}
#else
#define lock_set(set, lingering) NULL
#define unlock_set(m) (void)(m)
typedef void simpleMutex;
#endif


		 /*******************************
		 *	       BLOCKS		*
		 *******************************/

/* Blocks are allocated from the pool of the table of the node that owns
   the set, so they count as table space and are subject to its limit.
   NULL if the pool is exhausted, raising a resource error.
*/

static idg_set_block *
new_block(unsigned capacity, alloc_pool *pool)
{ unsigned bits = 0;
  idg_set_block *b;
  size_t bytes;

  if ( capacity > IDG_SET_LINEAR_MAX )
  { while( ((unsigned)1<<bits) < capacity*2 )	/* at least two slots */
      bits++;					/* per entry */
  }
  bytes = sizeof_block(capacity, bits);
  if ( !(b = alloc_from_pool(pool, bytes)) )
    return NULL;
  b->count      = 0;
  b->capacity   = capacity;
  b->dead       = 0;
  b->index_bits = bits;
  b->older      = NULL;
  if ( bits )
    memset(block_index(b), 0, ((size_t)1<<bits)*sizeof(uint32_t));

  return b;
}

static void
free_block_now(void *ptr, void *ctx)
{ idg_set_block *b = ptr;

  free_to_pool(ctx, b, sizeof_block(b->capacity, b->index_bits));
}

static void
free_block(idg_set_block *b, alloc_pool *pool)
{ idg_free_or_defer(free_block_now, b, pool);
}

/* A block was replaced or removed from its set.  If others may read
   it, it is kept until the node is destroyed.
*/

static void
retire_block(idg_set_block *b, idg_set_block **lingering, alloc_pool *pool)
{ if ( lingering )
  { idg_set_block *o;

    do
    { o = *lingering;
      b->older = o;
    } while( !COMPARE_AND_SWAP_PTR(lingering, o, b) );
  } else
  { free_block(b, pool);
  }
}

void
idg_set_free_lingering(idg_set_block **lingering, alloc_pool *pool)
{ idg_set_block *b = *lingering;

  *lingering = NULL;
  while(b)
  { idg_set_block *o = b->older;

    free_block(b, pool);
    b = o;
  }
}

static void
index_insert(idg_set_block *b, uintptr_t p, unsigned pos)
{ uint32_t *idx = block_index(b);
  unsigned mask = ((unsigned)1<<b->index_bits)-1;
  unsigned s = hash_slot(p, b->index_bits);

  while( idx[s] )
    s = (s+1)&mask;
  idx[s] = pos+1;
}

/* Find the position of p, also if it is dead.  -1 if p is not in b */

static int
find_pos(const idg_set_block *b, uintptr_t p)
{ unsigned count = b->count;

  MEMORY_ACQUIRE();
  if ( !b->index_bits )
  { for(unsigned i=0; i<count; i++)
    { if ( entry_ptr(b->entries[i]) == p )
	return (int)i;
    }
  } else
  { const uint32_t *idx = block_index(b);
    unsigned mask = ((unsigned)1<<b->index_bits)-1;
    unsigned s = hash_slot(p, b->index_bits);
    uint32_t v;

    while( (v=idx[s]) )
    { unsigned pos = v-1;

      if ( entry_ptr(b->entries[pos]) == p )
	return (int)pos;
      s = (s+1)&mask;
    }
  }

  return -1;
}

/* Append p to b, which has room.  Readers see the entry before it is
   found through the index or `count`.
*/

static void
append(idg_set_block *b, uintptr_t p)
{ unsigned pos = b->count;

  b->entries[pos] = p;
  if ( b->index_bits )
  { MEMORY_RELEASE();
    index_insert(b, p, pos);
  }
  MEMORY_RELEASE();
  b->count = pos+1;
}

/* A larger copy of b.  Entries keep their position.  NULL if the pool
   is exhausted.
*/

static idg_set_block *
grow_block(const idg_set_block *b, alloc_pool *pool)
{ idg_set_block *nb = new_block(b->capacity*2, pool);

  if ( !nb )
    return NULL;
  memcpy(nb->entries, b->entries, b->count*sizeof(uintptr_t));
  nb->dead = b->dead;
  if ( nb->index_bits )
  { for(unsigned i=0; i<b->count; i++)
      index_insert(nb, entry_ptr(b->entries[i]), i);
  }
  nb->count = b->count;

  return nb;
}


		 /*******************************
		 *		API		*
		 *******************************/

/* Add p.  True if p was added, false if it was already in the set and
   -1 if the pool is exhausted, raising a resource error.
*/

int
idg_set_add(idg_set *set, void *ptr, idg_set_block **lingering,
	    alloc_pool *pool)
{ uintptr_t p = (uintptr_t)ptr;
  simpleMutex *m = lock_set(set, lingering);
  idg_set s = *set;
  int added = true;

  assert(!(p&IDG_SET_SINGLE));
  if ( !s )
  { *set = p|IDG_SET_SINGLE;
  } else if ( is_single(s) )
  { if ( single_ptr(s) == p )
    { added = false;
    } else
    { idg_set_block *b = new_block(4, pool);

      if ( b )
      { append(b, single_ptr(s));
	append(b, p);
	MEMORY_RELEASE();
	*set = (idg_set)b;
      } else
      { added = -1;
      }
    }
  } else
  { idg_set_block *b = set_block(s);
    int pos = find_pos(b, p);

    if ( pos >= 0 )
    { if ( is_dead(b->entries[pos]) )
      { b->entries[pos] = p;
	b->dead--;
      } else
      { added = false;
      }
    } else
    { if ( b->count == b->capacity )
      { idg_set_block *nb = grow_block(b, pool);

	if ( nb )
	{ append(nb, p);
	  MEMORY_RELEASE();
	  *set = (idg_set)nb;
	  retire_block(b, lingering, pool);
	} else
	{ added = -1;
	}
      } else
      { append(b, p);
      }
    }
  }
  unlock_set(m);

  return added;
}

/* Delete p.  True if p was in the set */

bool
idg_set_delete(idg_set *set, const void *ptr, idg_set_block **lingering,
	       alloc_pool *pool)
{ uintptr_t p = (uintptr_t)ptr;
  simpleMutex *m = lock_set(set, lingering);
  idg_set s = *set;
  bool deleted = false;

  if ( is_single(s) )
  { if ( single_ptr(s) == p )
    { *set = 0;
      deleted = true;
    }
  } else if ( s )
  { idg_set_block *b = set_block(s);
    int pos = find_pos(b, p);

    if ( pos >= 0 && !is_dead(b->entries[pos]) )
    { b->entries[pos] = p|IDG_SET_DEAD;
      deleted = true;
      if ( ++b->dead == b->count )	/* nothing left */
      { *set = 0;
	retire_block(b, lingering, pool);
      }
    }
  }
  unlock_set(m);

  return deleted;
}

bool
idg_set_contains(const idg_set *set, const void *ptr)
{ uintptr_t p = (uintptr_t)ptr;
  idg_set s = *set;

  if ( is_single(s) )
  { return single_ptr(s) == p;
  } else if ( s )
  { const idg_set_block *b = set_block(s);
    int pos = find_pos(b, p);

    return pos >= 0 && !is_dead(b->entries[pos]);
  }

  return false;
}

size_t
idg_set_size(const idg_set *set)
{ idg_set s = *set;

  if ( is_single(s) )
    return 1;
  if ( s )
  { const idg_set_block *b = set_block(s);

    return b->count - b->dead;
  }

  return 0;
}

size_t
idg_set_sizeof(const idg_set *set)
{ idg_set s = *set;

  if ( s && !is_single(s) )
  { const idg_set_block *b = set_block(s);

    return sizeof_block(b->capacity, b->index_bits);
  }

  return 0;
}

/* Bytes used by the blocks on a lingering list */

size_t
idg_set_lingering_sizeof(const idg_set_block *b)
{ size_t size = 0;

  for(; b; b = b->older)
    size += sizeof_block(b->capacity, b->index_bits);

  return size;
}

/* Empty the set, calling func(p, ctx) for each pointer it held.  The set
   is detached first, so func runs without holding the stripe and may
   modify other sets.
*/

void
idg_set_clear(idg_set *set, void (*func)(void *p, void *ctx), void *ctx,
	      idg_set_block **lingering, alloc_pool *pool)
{ simpleMutex *m = lock_set(set, lingering);
  idg_set s = *set;

  *set = 0;
  unlock_set(m);

  if ( is_single(s) )
  { if ( func )
      (*func)((void*)single_ptr(s), ctx);
  } else if ( s )
  { idg_set_block *b = set_block(s);

    if ( func )
    { for(unsigned i=0; i<b->count; i++)
      { uintptr_t e = b->entries[i];

	if ( !is_dead(e) )
	  (*func)((void*)e, ctx);
      }
    }
    retire_block(b, lingering, pool);
  }
}

/* Next live pointer or NULL.  The set is read again for each step, so
   the enumeration survives the set growing or being cleared.
*/

void *
idg_set_next(idg_set_enum *e)
{ idg_set s = *e->set;

  if ( is_single(s) )
  { if ( e->pos == 0 )
    { e->pos = 1;
      return (void*)single_ptr(s);
    }
  } else if ( s )
  { const idg_set_block *b = set_block(s);
    unsigned count = b->count;

    MEMORY_ACQUIRE();
    while( e->pos < count )
    { uintptr_t v = b->entries[e->pos++];

      if ( !is_dead(v) )
	return (void*)v;
    }
  }

  return NULL;
}


		 /*******************************
		 *	 EXTERNAL READERS	*
		 *******************************/

/* See pl-idgset.h.  The writer unpublishes, issues a full barrier and
   reads `readers`; a reader increments `readers`, issues a full barrier
   and reads the pointers.  So, if a writer sees no readers, no reader
   can see what it frees.  If it sees readers, it decides under the
   mutex, as does the last reader that takes the deferred objects.
*/

typedef struct idg_deferred
{ struct idg_deferred *next;
  idg_free_func	free;
  void	       *ptr;
  void	       *ctx;
} idg_deferred;

#ifdef O_PLMT

static struct
{ int		readers;		/* active external readers */
  idg_deferred *deferred;		/* freed while there are readers */
  simpleMutex	mutex;
} idg_readers;

void
initIDGReaders(void)
{ simpleMutexInit(&idg_readers.mutex);
}

void
idg_free_or_defer(idg_free_func func, void *ptr, void *ctx)
{ MEMORY_BARRIER();
  if ( idg_readers.readers )
  { simpleMutexLock(&idg_readers.mutex);
    if ( idg_readers.readers )
    { idg_deferred *d = allocHeapOrHalt(sizeof(*d));

      d->free = func;
      d->ptr  = ptr;
      d->ctx  = ctx;
      d->next = idg_readers.deferred;
      idg_readers.deferred = d;
      simpleMutexUnlock(&idg_readers.mutex);
      return;
    }
    simpleMutexUnlock(&idg_readers.mutex);
  }

  (*func)(ptr, ctx);
}

void
idg_reader_enter(void)
{ simpleMutexLock(&idg_readers.mutex);
  idg_readers.readers++;
  simpleMutexUnlock(&idg_readers.mutex);
  MEMORY_BARRIER();
}

void
idg_reader_exit(void)
{ idg_deferred *d = NULL;

  simpleMutexLock(&idg_readers.mutex);
  if ( --idg_readers.readers == 0 )
  { d = idg_readers.deferred;
    idg_readers.deferred = NULL;
  }
  simpleMutexUnlock(&idg_readers.mutex);

  while(d)
  { idg_deferred *next = d->next;

    (*d->free)(d->ptr, d->ctx);
    freeHeap(d, sizeof(*d));
    d = next;
  }
}

#else /*O_PLMT*/

void
initIDGReaders(void)
{
}

void
idg_free_or_defer(idg_free_func func, void *ptr, void *ctx)
{ (*func)(ptr, ctx);
}

void
idg_reader_enter(void)
{
}

void
idg_reader_exit(void)
{
}

#endif /*O_PLMT*/


		 /*******************************
		 *	      REGISTRY		*
		 *******************************/

/* Slot `index` is in block k at offset v-(BASE<<k), where v=index+BASE
   and k is the most significant bit of v minus IDG_REG_BASE_BITS.
*/

#define IDG_REG_BASE ((size_t)1<<IDG_REG_BASE_BITS)
#define IS_FREE_SLOT(v) ((v)&0x1)

static inline unsigned
reg_block(size_t index, size_t *offset)
{ size_t v = index+IDG_REG_BASE;
  unsigned k = MSB64((int64_t)v) - IDG_REG_BASE_BITS;

  *offset = v - (IDG_REG_BASE<<k);
  return k;
}

static inline size_t
reg_block_size(unsigned k)
{ return IDG_REG_BASE<<k;
}

#ifdef O_PLMT
#define REG_LOCK(reg)	simpleMutexLock(&(reg)->mutex)
#define REG_UNLOCK(reg)	simpleMutexUnlock(&(reg)->mutex)
#else
#define REG_LOCK(reg)	(void)0
#define REG_UNLOCK(reg)	(void)0
#endif

idg_registry *
idg_registry_new(void)
{ idg_registry *reg = allocHeapOrHalt(sizeof(*reg));

  memset(reg, 0, sizeof(*reg));
  reg->references = 1;
#ifdef O_PLMT
  simpleMutexInit(&reg->mutex);
#endif

  return reg;
}

static void
free_registry_now(void *ptr, void *ctx)
{ idg_registry *reg = ptr;
  (void)ctx;

  for(unsigned k=0; k<IDG_REG_BLOCKS; k++)
  { if ( reg->blocks[k] )
      free_to_pool(reg->pool, reg->blocks[k],
		   reg_block_size(k)*sizeof(uintptr_t));
  }
#ifdef O_PLMT
  simpleMutexDelete(&reg->mutex);
#endif
  freeHeap(reg, sizeof(*reg));
}

/* Add node; returns its slot.  Adds a reference.  The slots are
   allocated from pool, the table pool of the nodes, so they count as
   table space.  All nodes of a registry come from the same pool.
   Returns (size_t)-1 if the pool is exhausted, raising a resource
   error.
*/

size_t
idg_registry_add(idg_registry *reg, void *node, alloc_pool *pool)
{ size_t index, offset;
  unsigned k;

  REG_LOCK(reg);
  if ( reg->free )
  { index = reg->free-1;
    k = reg_block(index, &offset);
    reg->free = (size_t)(reg->blocks[k][offset]>>1);
    reg->blocks[k][offset] = (uintptr_t)node;
  } else
  { index = reg->count;
    k = reg_block(index, &offset);
    if ( !reg->blocks[k] )
    { size_t bytes = reg_block_size(k)*sizeof(uintptr_t);
      uintptr_t *b;

      assert(!reg->pool || reg->pool == pool);
      if ( !(b = alloc_from_pool(pool, bytes)) )
      { REG_UNLOCK(reg);
	return (size_t)-1;
      }
      reg->pool = pool;
      memset(b, 0, bytes);
      MEMORY_RELEASE();
      reg->blocks[k] = b;
    }
    reg->blocks[k][offset] = (uintptr_t)node;
    MEMORY_RELEASE();
    reg->count = index+1;
  }
  reg->references++;
  REG_UNLOCK(reg);

  return index;
}

/* Delete the node in slot `index`.  Drops a reference */

void
idg_registry_delete(idg_registry *reg, size_t index)
{ size_t offset;
  unsigned k = reg_block(index, &offset);

  REG_LOCK(reg);
  reg->blocks[k][offset] = ((uintptr_t)reg->free<<1)|0x1;
  reg->free = index+1;
  REG_UNLOCK(reg);
  idg_registry_release(reg);
}

void
idg_registry_release(idg_registry *reg)
{ idg_registry_release_n(reg, 1);
}

/* Release n references at once, e.g., those of all nodes when they are
   destroyed together.
*/

void
idg_registry_release_n(idg_registry *reg, size_t n)
{ size_t left;

  REG_LOCK(reg);
  assert(reg->references >= n);
  left = reg->references -= n;
  REG_UNLOCK(reg);
  if ( left == 0 )
    idg_free_or_defer(free_registry_now, reg, NULL);
}

size_t
idg_registry_count(const idg_registry *reg)
{ size_t count = reg->count;

  MEMORY_ACQUIRE();
  return count;
}

/* Bytes used by the slots of the registry */

size_t
idg_registry_sizeof(const idg_registry *reg)
{ size_t size = 0;

  for(unsigned k=0; k<IDG_REG_BLOCKS; k++)
  { if ( reg->blocks[k] )
      size += reg_block_size(k)*sizeof(uintptr_t);
  }

  return size;
}

/* The node in slot `index` or NULL if the slot is free */

void *
idg_registry_get(const idg_registry *reg, size_t index)
{ size_t offset;
  unsigned k = reg_block(index, &offset);
  const uintptr_t *b = reg->blocks[k];
  uintptr_t v;

  if ( !b )
    return NULL;
  v = b[offset];
  if ( IS_FREE_SLOT(v) )
    return NULL;

  return (void*)v;
}
