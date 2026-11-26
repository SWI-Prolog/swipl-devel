/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
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

#include "pl-incl.h"
#include "pl-allocpool.h"

alloc_pool *
new_alloc_pool(const char *name, size_t limit)
{ alloc_pool *p = malloc(sizeof(*p));

  if ( p )
  { memset(p, 0, sizeof(*p));
    p->limit = limit;
    p->name  = name;
  } else
  { PL_resource_error("memory");
  }

  return p;
}

void
free_alloc_pool(alloc_pool *pool)
{ pool->freed = true;
  if ( pool->size == 0 )
    free(pool);
}


void *
alloc_from_pool(alloc_pool *pool, size_t bytes)
{ void *mem;

  if ( pool )
  { if ( pool->size+bytes <= pool->limit )
    { ATOMIC_ADD(&pool->size, bytes);
    } else if ( pool->size+bytes <= pool->limit + pool->reclaimable )
    { ATOMIC_ADD(&pool->size, bytes);	/* see pool_add_reclaimable() */
      requestLocalSweep();
    } else
    { PL_resource_error(pool->name);
      return NULL;
    }
  }

  if ( (mem=malloc(bytes)) )
    return mem;

  PL_resource_error("memory");
  return NULL;
}

/* Memory of the pool that is garbage, but is only released by atom-GC.
 * This is the case for abolished private answer tries that are owned by
 * their symbol (see release_local_trie()).  Such memory does not count
 * for the limit.  If we exceed the limit using this memory, we ask for
 * a local sweep, which reclaims the tries whose symbols are no longer
 * referenced.
 */

void
pool_add_reclaimable(alloc_pool *pool, size_t bytes)
{ ATOMIC_ADD(&pool->reclaimable, bytes);
}

void
pool_sub_reclaimable(alloc_pool *pool, size_t bytes)
{ assert(bytes <= pool->reclaimable);
  ATOMIC_SUB(&pool->reclaimable, bytes);
}

void
free_to_pool(alloc_pool *pool, void *mem, size_t bytes)
{ free(mem);

  if ( pool )
  { assert(bytes <= pool->size);
    ATOMIC_SUB(&pool->size, bytes);
    if ( pool->freed && pool->size == 0 )
      free(pool);
  }
}
