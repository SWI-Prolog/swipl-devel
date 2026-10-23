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

bool	idg_set_add(idg_set *set, void *p, idg_set_block **lingering);
bool	idg_set_delete(idg_set *set, const void *p,
		       idg_set_block **lingering);
bool	idg_set_contains(const idg_set *set, const void *p);
size_t	idg_set_size(const idg_set *set);
size_t	idg_set_sizeof(const idg_set *set);
void	idg_set_clear(idg_set *set,
		      void (*func)(void *p, void *ctx), void *ctx,
		      idg_set_block **lingering);
void	idg_set_free_lingering(idg_set_block **lingering);
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

#endif /*_PL_IDGSET_H*/
