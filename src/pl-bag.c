/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-incl.h"

#undef LD
#define LD LOCAL_LD

		 /*******************************
		 *	    TEMP MALLOC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate memory for  findall  bags  in   chunks  that  can  be discarded
together  and  preallocate  the  first    chunk.  This  approach  avoids
fragmentation and reduces the number of  allocation calls. The latter is
notably needed to reduce allocation contention   due to intensive use of
findall/3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FIRST_CHUNK_SIZE (64*sizeof(void*))

typedef struct mem_chunk
{ struct mem_chunk *prev;
  size_t	size;
  size_t	used;
} mem_chunk;

typedef struct mem_pool
{ mem_chunk    *chunks;
  mem_chunk	first;
  char		first_data[FIRST_CHUNK_SIZE];
} mem_pool;

static void
init_mem_pool(mem_pool *mp)
{ mp->chunks = &mp->first;
  mp->first.size = FIRST_CHUNK_SIZE;
  mp->first.used = 0;
}

static void *
alloc_mem_pool(mem_pool *mp, size_t bytes)
{ char *ptr;

  if ( mp->chunks->used + bytes <= mp->chunks->size )
  { ptr = &((char *)(mp->chunks+1))[mp->chunks->used];
    mp->chunks->used += bytes;
  } else
  { size_t chunksize = (bytes < 1000 ? 4000 : bytes);
    mem_chunk *c = PL_malloc_atomic_unmanaged(chunksize+sizeof(mem_chunk));

    if ( c )
    { c->size    = chunksize;
      c->used    = bytes;
      c->prev    = mp->chunks;
      mp->chunks = c;
      ptr        = (char *)(mp->chunks+1);
    } else
      return NULL;
  }

  return ptr;
}

static void
clear_mem_pool(mem_pool *mp)
{ mem_chunk *c, *p;

  for(c=mp->chunks; c != &mp->first; c=p)
  { p = c->prev;
    PL_free(c);
  }
}


		 /*******************************
		 *        FINDALL SUPPORT	*
		 *******************************/

#define FINDALL_MAGIC	0x37ac78fe

typedef struct findall_bag
{ struct findall_bag *parent;		/* parent bag */
  intptr_t	magic;			/* FINDALL_MAGIC */
  size_t	solutions;		/* count # solutions */
  size_t	gsize;			/* required size on stack */
  mem_pool	records;		/* stored records */
  segstack	answers;		/* list of answers */
  Record	answer_buf[64];		/* tmp space */
} findall_bag;


static
PRED_IMPL("$new_findall_bag", 0, new_findall_bag, 0)
{ PRED_LD
  findall_bag *bag;

  if ( !LD->bags.bags )			/* outer one */
  { if ( !LD->bags.default_bag )
      LD->bags.default_bag = PL_malloc(sizeof(*bag));
    bag = LD->bags.default_bag;
  } else
  { bag = PL_malloc(sizeof(*bag));
  }

  if ( !bag )
    return PL_no_memory();

  bag->magic     = FINDALL_MAGIC;
  bag->solutions = 0;
  bag->gsize     = 0;
  bag->parent    = LD->bags.bags;
  init_mem_pool(&bag->records);
  initSegStack(&bag->answers, sizeof(Record),
	       sizeof(bag->answer_buf), bag->answer_buf);
  MemoryBarrier();
  LD->bags.bags = bag;

  return TRUE;
}


static void *
alloc_record(void *ctx, size_t bytes)
{ findall_bag *bag = ctx;

  return alloc_mem_pool(&bag->records, bytes);
}


static
PRED_IMPL("$add_findall_bag", 1, add_findall_bag, 0)
{ PRED_LD
  findall_bag *bag = LD->bags.bags;
  Record r;

  if ( !(r = compileTermToHeap__LD(A1, alloc_record, bag, R_NOLOCK PASS_LD)) )
    return PL_no_memory();
  if ( !pushRecordSegStack(&bag->answers, r) )
    return PL_no_memory();
  bag->gsize += r->gsize;
  bag->solutions++;

  if ( bag->gsize + bag->solutions*3 > limitStack(global)/sizeof(word) )
    return outOfStack(&LD->stacks.global, STACK_OVERFLOW_RAISE);

  return FALSE;				/* force backtracking of generator */
}


static
PRED_IMPL("$collect_findall_bag", 2, collect_findall_bag, 0)
{ PRED_LD
  findall_bag *bag = LD->bags.bags;

  if ( bag->solutions )
  { size_t space = bag->gsize + bag->solutions*3;
    term_t list = PL_copy_term_ref(A2);
    term_t answer = PL_new_term_ref();
    Record *rp;
    int rc;

    if ( !hasGlobalSpace(space) )
    { if ( (rc=ensureGlobalSpace(space, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
    }

    while ( (rp=topOfSegStack(&bag->answers)) )
    { Record r = *rp;
      copyRecordToGlobal(answer, r, ALLOW_GC PASS_LD);
      PL_cons_list(list, answer, list);
      popTopOfSegStack(&bag->answers);
    }
    DEBUG(CHK_SECURE, assert(emptySegStack(&bag->answers)));

    return PL_unify(A1, list);
  } else
    return PL_unify(A1, A2);
}


static
PRED_IMPL("$destroy_findall_bag", 0, destroy_findall_bag, 0)
{ PRED_LD
  findall_bag *bag = LD->bags.bags;

  assert(bag);
  assert(bag->magic == FINDALL_MAGIC);
  bag->magic = 0;
  clearSegStack(&bag->answers);
  clear_mem_pool(&bag->records);
  LD->bags.bags = bag->parent;
  if ( bag != LD->bags.default_bag )
    PL_free(bag);

  return TRUE;
}


		 /*******************************
		 *	  ATOM-GC SUPPORT	*
		 *******************************/

static void
markAtomsAnswers(void *data)
{ Record r = *((Record*)data);

  markAtomsRecord(r);
}


void
markAtomsFindall(PL_local_data_t *ld)
{ findall_bag *bag = ld->bags.bags;

  for( ; bag; bag = bag->parent )
    scanSegStack(&bag->answers, markAtomsAnswers);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(bag)
  PRED_DEF("$new_findall_bag", 0, new_findall_bag, 0)
  PRED_DEF("$add_findall_bag", 1, add_findall_bag, 0)
  PRED_DEF("$collect_findall_bag", 2, collect_findall_bag, 0)
  PRED_DEF("$destroy_findall_bag", 0, destroy_findall_bag, 0)
EndPredDefs
