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

static idg_set_block *
new_block(unsigned capacity)
{ unsigned bits = 0;
  idg_set_block *b;
  size_t bytes;

  if ( capacity > IDG_SET_LINEAR_MAX )
  { while( ((unsigned)1<<bits) < capacity*2 )	/* at least two slots */
      bits++;					/* per entry */
  }
  bytes = sizeof_block(capacity, bits);
  b = allocHeapOrHalt(bytes);
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
free_block(idg_set_block *b)
{ freeHeap(b, sizeof_block(b->capacity, b->index_bits));
}

/* A block was replaced or removed from its set.  If others may read
   it, it is kept until the node is destroyed.
*/

static void
retire_block(idg_set_block *b, idg_set_block **lingering)
{ if ( lingering )
  { idg_set_block *o;

    do
    { o = *lingering;
      b->older = o;
    } while( !COMPARE_AND_SWAP_PTR(lingering, o, b) );
  } else
  { free_block(b);
  }
}

void
idg_set_free_lingering(idg_set_block **lingering)
{ idg_set_block *b = *lingering;

  *lingering = NULL;
  while(b)
  { idg_set_block *o = b->older;

    free_block(b);
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

/* A larger copy of b.  Entries keep their position. */

static idg_set_block *
grow_block(const idg_set_block *b)
{ idg_set_block *nb = new_block(b->capacity*2);

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

/* Add p.  True if p was added, false if it was already in the set */

bool
idg_set_add(idg_set *set, void *ptr, idg_set_block **lingering)
{ uintptr_t p = (uintptr_t)ptr;
  simpleMutex *m = lock_set(set, lingering);
  idg_set s = *set;
  bool added = true;

  assert(!(p&IDG_SET_SINGLE));
  if ( !s )
  { *set = p|IDG_SET_SINGLE;
  } else if ( is_single(s) )
  { if ( single_ptr(s) == p )
    { added = false;
    } else
    { idg_set_block *b = new_block(4);

      append(b, single_ptr(s));
      append(b, p);
      MEMORY_RELEASE();
      *set = (idg_set)b;
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
      { idg_set_block *nb = grow_block(b);

	append(nb, p);
	MEMORY_RELEASE();
	*set = (idg_set)nb;
	retire_block(b, lingering);
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
idg_set_delete(idg_set *set, const void *ptr, idg_set_block **lingering)
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
	retire_block(b, lingering);
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

/* Empty the set, calling func(p, ctx) for each pointer it held.  The set
   is detached first, so func runs without holding the stripe and may
   modify other sets.
*/

void
idg_set_clear(idg_set *set, void (*func)(void *p, void *ctx), void *ctx,
	      idg_set_block **lingering)
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
    retire_block(b, lingering);
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
