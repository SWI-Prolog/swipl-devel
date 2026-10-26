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


#ifndef _PL_IDGSET_H
#define _PL_IDGSET_H

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An idg_set is the set of edges of an IDG node in one direction: the nodes
it affects or the nodes it depends on.  Most sets hold no or one edge, a
few hold thousands.  The set is a single word:

  - 0
    The set is empty.
  - pointer|IDG_SET_SINGLE
    The set holds one pointer.
  - idg_set_block*
    The set holds a block of pointers.  Entries are appended and never
    move, so enumerating by position is (weakly) consistent with adding
    and deleting.  A deleted entry keeps its pointer with IDG_SET_DEAD
    set; adding the same pointer again revives it.  Blocks with more
    than IDG_SET_LINEAR_MAX entries are followed by an open addressing
    index of 1<<index_bits uint32_t slots holding the position+1 of an
    entry (0: empty slot).

Blocks are allocated from the table pool (alloc_pool) of the node that
owns the set, so the IDG counts as table space.  Functions that modify
a set get this pool.

Readers do not lock.  Writers either own the set (the node is private and
all nodes it is connected to are private) or pass `lingering`.  In that
case writes lock a stripe that is selected on the set and replaced blocks
are kept on `*lingering` as another thread may still be reading them.
These are freed using idg_set_free_lingering() when the node is
destroyed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef uintptr_t idg_set;

#define IDG_SET_SINGLE	   0x1
#define IDG_SET_DEAD	   0x1
#define IDG_SET_LINEAR_MAX 8

typedef struct idg_set_block
{ unsigned	count;			/* entries in use (live and dead) */
  unsigned	capacity;		/* allocated entries */
  unsigned	dead;			/* entries with IDG_SET_DEAD */
  unsigned	index_bits;		/* 0: no index */
  struct idg_set_block *older;		/* next on the lingering list */
  uintptr_t	entries[];		/* followed by the index */
} idg_set_block;

typedef struct idg_set_enum
{ const idg_set *set;			/* the set we enumerate */
  unsigned	 pos;			/* next position */
} idg_set_enum;

int	idg_set_add(idg_set *set, void *p, idg_set_block **lingering,
		    struct alloc_pool *pool);
bool	idg_set_delete(idg_set *set, const void *p,
		       idg_set_block **lingering, struct alloc_pool *pool);
bool	idg_set_contains(const idg_set *set, const void *p);
size_t	idg_set_size(const idg_set *set);
size_t	idg_set_sizeof(const idg_set *set);
size_t	idg_set_lingering_sizeof(const idg_set_block *b);
void	idg_set_clear(idg_set *set,
		      void (*func)(void *p, void *ctx), void *ctx,
		      idg_set_block **lingering, struct alloc_pool *pool);
void	idg_set_free_lingering(idg_set_block **lingering,
			       struct alloc_pool *pool);
void   *idg_set_next(idg_set_enum *e);

static inline void
idg_set_enum_init(idg_set_enum *e, const idg_set *set)
{ e->set = set;
  e->pos = 0;
}

static inline bool
idg_set_is_empty(const idg_set *set)
{ return idg_set_size(set) == 0;
}

		 /*******************************
		 *	 EXTERNAL READERS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Another thread may inspect the IDG of a thread, e.g., to show it.  Such a
reader calls idg_reader_enter() before and idg_reader_exit() after
following any IDG pointer.  While there are readers, IDG nodes, the
blocks of their edge sets and answer tries are not freed but kept on a
list that the last reader frees.  Writers first unpublish what they free
and then call idg_free_or_defer(), which frees at once if there are no
readers.

The IDG nodes of a thread are in its idg_registry; those of shared tables
in a global one.  A reader finds the registry of another thread through
acquire_ldata() and enters the reader section before releasing the
thread's data.  A registry is kept alive by its thread and its nodes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define IDG_REG_BASE_BITS 6		/* first block holds 64 slots */
#define IDG_REG_BLOCKS	  32

typedef struct idg_registry
{ int		references;		/* the thread and the nodes */
  size_t	count;			/* slots in use (high water) */
  size_t	free;			/* first free slot+1 (0: none) */
  struct alloc_pool *pool;		/* table pool the slots come from */
#ifdef O_PLMT
  simpleMutex	mutex;			/* serializes writers */
#endif
  uintptr_t    *blocks[IDG_REG_BLOCKS]; /* node or (next free+1)<<1|1 */
} idg_registry;

typedef void (*idg_free_func)(void *ptr, void *ctx);

void	idg_free_or_defer(idg_free_func func, void *ptr, void *ctx);
void	idg_reader_enter(void);
void	idg_reader_exit(void);

idg_registry *idg_registry_new(void);
size_t	idg_registry_add(idg_registry *reg, void *node,
			 struct alloc_pool *pool);
void	idg_registry_delete(idg_registry *reg, size_t index);
void	idg_registry_release(idg_registry *reg);
void   *idg_registry_get(const idg_registry *reg, size_t index);
size_t	idg_registry_count(const idg_registry *reg);
size_t	idg_registry_sizeof(const idg_registry *reg);
void	initIDGReaders(void);

#endif /*_PL_IDGSET_H*/
