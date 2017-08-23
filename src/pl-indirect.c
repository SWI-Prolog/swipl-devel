/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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
#include "pl-indirect.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Indirect datatypes are represented by  a   tagged  pointer to the global
stack. The global stack stores the data using  a guard cell on both ends
of the actual data. The guard cell indicates  the length of the blob and
is needed for the upward  and  downward   scans  needed  by  the garbage
collector. For example, a float is represented using:

   <ptr, tagged with TAG_FLOAT|STG_GLOBAL>
     |
     |-----> [guard size 1]  (* size 2 on 32-bit hardware *)
	     [IEEE double]
	     [guard size 1]

This does not play well with the tries  as defined in pl-trie.c where we
want to switch on a term represented as   a  single word. We fix this by
`interning' the indirects. The intern table  is comparable to the global
atom table. I consists of a  dynamic   array  of  interned indirects and
represents the indirect as a tagged index into this array.

We keep the design similar to the atom   table.  We have two options for
GC: use basically the same as atom-GC   or  always copy indirects to the
global stack. In the latter case  there   are  never references from the
volatile areas and thus we can use purely reference count based GC.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	 rehash_indirect_table(indirect_table *tab);
static int	 bump_ref(indirect *h, unsigned int refs);
static indirect *reserve_indirect(indirect_table *tab, word val ARG_LD);
static indirect *create_indirect(indirect *h, size_t index, word val ARG_LD);

/* TBD: register with LD structure */
#define acquire_itable_buckets(tab) (tab->table)
#define acquire_itable_bucket(b) (void)0
#define release_itable_buckets() (void)0

#define TIGHT(buckets, tab) ((buckets)->size < (tab)->count)

#define INDIRECT_STATE_MASK		((unsigned int)0x3 << (INTBITSIZE-2))
#define INDIRECT_RESERVED_REFERENCE	((unsigned int)0x1 << (INTBITSIZE-1))
#define INDIRECT_VALID_REFERENCE	((unsigned int)0x1 << (INTBITSIZE-2))

#define INDIRECT_IS_FREE(ref)		(((ref) & INDIRECT_STATE_MASK) == 0)
#define INDIRECT_IS_RESERVED(ref)	((ref) & INDIRECT_RESERVED_REFERENCE)
#define INDIRECT_IS_VALID(ref)		((ref) & INDIRECT_VALID_REFERENCE)

#define INDIRECT_REF_COUNT_MASK		(~INDIRECT_STATE_MASK)
#define INDIRECT_REF_COUNT(ref)		((ref) & INDIRECT_REF_COUNT_MASK)


indirect_table *
new_indirect_table(void)
{ indirect_table *tab = PL_malloc(sizeof(*tab));
  indirect_array *arr = &tab->array;
  indirect_buckets *newtab = PL_malloc(sizeof(*newtab));
  int i;

  memset(tab, 0, sizeof(*tab));
#ifdef O_PLMT
  simpleMutexInit(&tab->mutex);
#endif

  for(i=0; i<MSB(PREALLOCATED_INDIRECT_BLOCKS); i++)
  { arr->blocks[i] = arr->preallocated;
  }

  newtab->size = 8;
  newtab->buckets = PL_malloc(newtab->size*sizeof(*newtab->buckets));
  memset(newtab->buckets, 0, newtab->size*sizeof(*newtab->buckets));
  newtab->prev = NULL;
  tab->table = newtab;
  tab->no_hole_before = 1;
  tab->highest = 1;

  return tab;
}


static void
clean_block(indirect *block, size_t size)
{ indirect *end = block+size;
  indirect *b = block;

  for(; b < end; b++)
  { if ( b->data )
      PL_free(b->data);
  }
}

void
destroy_indirect_table(indirect_table *tab)
{ int i;
  indirect_buckets *buckets, *prev;
  indirect_array *arr = &tab->array;

#ifdef O_PLMT
  simpleMutexDelete(&tab->mutex);
#endif
  clean_block(arr->preallocated, PREALLOCATED_INDIRECT_BLOCKS);
  for(i=MSB(PREALLOCATED_INDIRECT_BLOCKS); i<MAX_INDIRECT_BLOCKS; i++)
  { if ( arr->blocks[i] )
    { size_t bs = (size_t)1<<i;
      indirect *block = arr->blocks[i]+bs;

      clean_block(block, bs);
      PL_free(block);
    }
  }

  for(buckets = tab->table; buckets; buckets = prev)
  { prev = buckets->prev;

    PL_free(buckets->buckets);
    PL_free(buckets);
  }

  PL_free(tab);
}


word
intern_indirect(indirect_table *tab, word val, int create ARG_LD)
{ Word	 idata     = addressIndirect(val);	/* points at header */
  size_t isize     = wsizeofInd(*idata);	/* include header */
  unsigned int key = MurmurHashAligned2(idata+1, isize*sizeof(word), MURMUR_SEED);
  indirect_buckets *buckets;

  for(;;)
  { buckets = acquire_itable_buckets(tab);
    unsigned int ki = key & (buckets->size-1);
    indirect *head = buckets->buckets[ki];
    indirect *h;

    acquire_itable_bucket(&buckets->buckets[ki]);
    for(h=buckets->buckets[ki]; h; h = h->next)
    { unsigned int ref = h->references;

      if ( INDIRECT_IS_VALID(ref) &&
	   idata[0] == h->header &&
	   memcmp(idata+1, h->data, isize*sizeof(word)) == 0 )
      { if ( bump_ref(h, ref) )
	{ release_itable_buckets();
	  return h->handle;
	}
      }
    }

    if ( TIGHT(buckets, tab) )
    { simpleMutexLock(&tab->mutex);
      rehash_indirect_table(tab);
      simpleMutexUnlock(&tab->mutex);
    }

    if ( buckets != tab->table || head != buckets->buckets[ki] )
      continue;				/* try again */

    if ( create )
    { indirect *h = reserve_indirect(tab, val PASS_LD);

      h->next = buckets->buckets[ki];
      if ( !COMPARE_AND_SWAP(&buckets->buckets[ki], head, h) ||
	   buckets != tab->table )
      { PL_free(h->data);
	h->references = 0;
	continue;			/* try again */
      }

      h->references = 1 | INDIRECT_VALID_REFERENCE | INDIRECT_RESERVED_REFERENCE;
      ATOMIC_INC(&tab->count);
      release_itable_buckets();

      return h->handle;
    } else
    { release_itable_buckets();
      return 0;
    }
  }
}


static int
bump_ref(indirect *h, unsigned int refs)
{ for(;;)
  { if ( COMPARE_AND_SWAP(&h->references, refs, refs+1) )
    { return TRUE;
    } else
    { refs = h->references;
      if ( !INDIRECT_IS_VALID(refs) )
	return FALSE;
    }
  }
}


static void
allocate_indirect_block(indirect_table *tab, int idx)
{ simpleMutexLock(&tab->mutex);
  if ( !tab->array.blocks[idx] )
  { size_t bs = (size_t)1<<idx;
    indirect *newblock;

    if ( !(newblock=PL_malloc(bs*sizeof(*newblock))) )
      outOfCore();

    memset(newblock, 0, bs*sizeof(*newblock));
    tab->array.blocks[idx] = newblock-bs;
  }
  simpleMutexUnlock(&tab->mutex);
}


static indirect *
reserve_indirect(indirect_table *tab, word val ARG_LD)
{ size_t index;
  int i;
  int last = FALSE;

  for(index=tab->no_hole_before, i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    indirect *b = tab->array.blocks[i];

    if ( upto >= tab->highest )
    { upto = tab->highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { indirect *a = b + index;
      unsigned int refs = a->references;

      if ( INDIRECT_IS_FREE(refs) &&
	   COMPARE_AND_SWAP(&a->references, refs, INDIRECT_RESERVED_REFERENCE) )
      { tab->no_hole_before = index+1;
	return create_indirect(a, index, val PASS_LD);
      }
    }
  }
  tab->no_hole_before = tab->highest;

  for(;;)
  { int idx;
    indirect *a;
    unsigned int refs;

    index = tab->highest;
    idx = MSB(index);

    if ( !tab->array.blocks[idx] )
      allocate_indirect_block(tab, idx);

    a = &tab->array.blocks[idx][index];
    refs = a->references;

    if ( INDIRECT_IS_FREE(refs) &&
	 COMPARE_AND_SWAP(&a->references, refs, INDIRECT_RESERVED_REFERENCE) )
    { ATOMIC_INC(&tab->highest);
      return create_indirect(a, index, val PASS_LD);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Ideally, we use a different storage mask, so we can distinguish interned
and normal indirects. STG_STATIC however is  an alias for STG_INLINE, so
we cannot distinguish inlined integers from bignums and MPZ integers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static indirect *
create_indirect(indirect *h, size_t index, word val ARG_LD)
{ Word	 idata = addressIndirect(val);	/* points at header */
  size_t isize = wsizeofInd(*idata);	/* include header */

  h->handle = (index<<LMASK_BITS)|tag(val)|STG_GLOBAL; /* (*) */
  h->header = idata[0];
  h->data   = PL_malloc(isize*sizeof(word));
  memcpy(h->data, &idata[1], isize*sizeof(word));

  return h;
}


static void
rehash_indirect_table(indirect_table *tab)
{ if ( TIGHT(tab->table, tab) )
  { indirect_buckets *oldtab = tab->table;
    indirect_buckets *newtab = PL_malloc(sizeof(*newtab));
    unsigned int mask;
    size_t index;
    int i, last=FALSE;

    newtab->size    = oldtab->size * 2;
    newtab->buckets = PL_malloc(newtab->size*sizeof(*newtab->buckets));
    memset(newtab->buckets, 0, newtab->size*sizeof(*newtab->buckets));
    newtab->prev    = oldtab;

    mask = newtab->size - 1;
    for(index=1, i=0; !last; i++)
    { size_t upto = (size_t)2<<i;
      indirect *b = tab->array.blocks[i];

      if ( upto >= tab->highest )
      { upto = tab->highest;
	last = TRUE;
      }

      for(; index<upto; index++)
      { indirect *a = b+index;

	if ( INDIRECT_IS_VALID(a->references) )
	{ size_t sz = wsizeofInd(a->header);
	  unsigned int v;

	  v = MurmurHashAligned2(a->data, sz*sizeof(word), MURMUR_SEED) & mask;
	  a->next = newtab->buckets[v];
	  newtab->buckets[v] = a;
	}
      }
    }

    tab->table = newtab;
  }
}


word
extern_indirect(indirect_table *tab, word val, Word *gp ARG_LD)
{ size_t index = val>>LMASK_BITS;
  int idx = MSB(index);
  indirect *h = &tab->array.blocks[idx][index];
  size_t wsize = wsizeofInd(h->header);
  Word p, r;

  if ( !hasGlobalSpace(wsize+2) )
  { int rc;

    if ( (rc=ensureGlobalSpace(wsize+2, ALLOW_GC)) != TRUE )
    { raiseStackOverflow(rc);
      return 0;
    }
  }

  if ( gp )
    r = p = *gp;
  else
    r = p = gTop;
  *p++ = h->header;
  memcpy(p, h->data, wsize*sizeof(word));
  p += wsize;
  *p++ = h->header;

  if ( gp )
    *gp = p;
  else
    gTop = p;

  return consPtr(r, tag(val)|STG_GLOBAL);
}


size_t
gsize_indirect(indirect_table *tab, word val)
{ size_t index = val>>LMASK_BITS;
  int idx = MSB(index);
  indirect *h = &tab->array.blocks[idx][index];
  size_t wsize = wsizeofInd(h->header);

  return wsize+2;
}
