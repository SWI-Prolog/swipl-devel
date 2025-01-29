/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2024, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
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
#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Segmented stack handling. A segmented stack is a stack that is allocated
in segments, This means we cannot   compare  addresses otherwise then by
identity.  We use a segmented stack for cycle detection.

Measurements with the chunksize on SuSE Linux  10.2 indicate there is no
measurable performance change above approximately  256 bytes. We'll keep
the figure on the safe  side  for   systems  with  less efficient malloc
implementations.

Note  that  atom-gc  requires   completely    asynchronous   calling  of
scanSegStack() and therefore pushSegStack()/popSegStack()  must push the
data before updating the pointers.

TBD: Avoid instruction/cache write reordering in push/pop.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
next_chunk_size(segstack *stack)
{ if ( stack->last && stack->last->allocated )
  { return stack->last->size*2;
  } else
  { size_t size = 1024;
    while(size < 4*stack->unit_size)
      size *= 2;
    return size;
  }
}

void *
pushSegStack_(segstack *stack, void *data)
{ if ( segStackHasSpace(stack, stack->unit_size) )
  { char *ptr = stack->top;

    if ( data )
      memcpy(ptr, data, stack->unit_size);
    stack->top = ptr+stack->unit_size;

    return ptr;
  } else
  { size_t chunksize = tmp_nalloc(next_chunk_size(stack));
    segchunk *chunk = tmp_malloc(chunksize);

    if ( !chunk )
      return NULL;			/* out of memory */

    chunk->allocated = true;
    chunk->size = chunksize;
    chunk->next = NULL;
    chunk->previous = stack->last;
    chunk->top = CHUNK_DATA(chunk);	/* async scanning */
    if ( stack->last )
    { stack->last->next = chunk;
      stack->last->top = stack->top;
      stack->top = chunk->top;		/* async scanning */
      stack->last = chunk;
    } else
    { stack->top = chunk->top;		/* async scanning */
      stack->last = stack->first = chunk;
    }

    stack->base = CHUNK_DATA(chunk);
    stack->max  = addPointer(chunk, chunk->size);
    if ( data )
      memcpy(CHUNK_DATA(chunk), data, stack->unit_size);
    stack->top  = CHUNK_DATA(chunk) + stack->unit_size;

    return CHUNK_DATA(chunk);
  }
}


int
pushRecordSegStack(segstack *stack, Record r)
{ if ( stack->top + sizeof(r) <= stack->max )
  { Record *rp = (Record*)stack->top;

    *rp++ = r;
    stack->top = (char*)rp;

    return true;
  } else
  { return !!pushSegStack_(stack, &r);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pop data. Note that we leave the first chunk associated with the stack
to speedup frequent small usage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
popSegStack_(segstack *stack, void *data)
{ again:

  if ( segStackHasData(stack) )
  { stack->top -= stack->unit_size;
    if ( data )
      memcpy(data, stack->top, stack->unit_size);

    return true;
  } else
  { segchunk *chunk = stack->last;

    if ( chunk )
    { if ( chunk->previous )
      { stack->last = chunk->previous;
	stack->last->next = NULL;
	if ( chunk->allocated )
	  tmp_free(chunk);

	chunk = stack->last;
	stack->base = CHUNK_DATA(chunk);
	stack->max  = addPointer(chunk, chunk->size);
	stack->top  = chunk->top;
	stack->chunk_count--;
	goto again;
      }
    }

    return false;
  }
}


void *
topOfSegStack(segstack *stack)
{ segchunk *chunk;

  if ( stack->top >= stack->base + stack->unit_size )
  { return stack->top - stack->unit_size;
  } else if ( stack->last && (chunk=stack->last->previous) )
  { assert(chunk->top - stack->unit_size >= CHUNK_DATA(chunk));
    return chunk->top - stack->unit_size;
  }

  return NULL;
}


void
popTopOfSegStack_(segstack *stack)
{ again:

  if ( stack->top >= stack->base + stack->unit_size )
  { stack->top -= stack->unit_size;
  } else
  { segchunk *chunk = stack->last;

    if ( chunk )
    { if ( chunk->previous )
      { segchunk *del = chunk;

	stack->chunk_count--;
	stack->last = chunk->previous;
	stack->last->next = NULL;
	chunk = stack->last;
	stack->top  = chunk->top;
	MEMORY_BARRIER();		/* Sync with scanSegStack() */
	stack->base = CHUNK_DATA(chunk);
	stack->max  = addPointer(chunk, chunk->size);

	if ( del->allocated )
	  tmp_free(del);

	goto again;
      }
    }

    assert(0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
scanSegStack(segstack *stack, void (*func)(void *cell))
Walk along all living cells on the stack and call func on them.  The stack
is traversed last-to-first.

This is used by markAtomsFindall(), which runs concurrent with findall/3
in the thread being scanned. New records pushed while AGC is in progress
are marked by findall/3 itself.  Findall/3   can  concurrently  pop this
stack. It may do this quicker than the   marking, but this is no problem
because the pointers remain valid as all   records  are reclaimed at the
end of findall/3 by calling clear_mem_pool(). It   is  only a problem if
popTopOfSegStack_() pops a chunk because  the   popped  chunk  is freed.
Therefore $collect_findall_bag/2 locks if it needs to call the expensive
chunk-popping popTopOfSegStack_().

With thanks to Eugeniy Meshcheryakov for   finding this and running many
tests. The issue  is   triggered  by tests/thread/thread_agc_findall.pl,
notably on slow single core hardware.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
scan_chunk(segstack *stack, char *top, char *base, void (*func)(void *cell))
{ while(top >= base+stack->unit_size)
  { top -= stack->unit_size;
    (*func)((void*)top);
  }
}


void
scanSegStack(segstack *stack, void (*func)(void *cell))
{ segchunk *chunk;

  if ( (chunk=stack->last) )		/* something there */
  { if ( stack->base == CHUNK_DATA(chunk) )
    { char *top = stack->top;

      if ( !(top >  (char*)chunk &&
	     top <= (char*)((intptr_t)chunk + chunk->size)) )
	top = chunk->top;              /* top has moved to new chunk */
      scan_chunk(stack, top, CHUNK_DATA(chunk), func);
      chunk = chunk->previous;
    }
    for(; chunk; chunk=chunk->previous)
      scan_chunk(stack, chunk->top, CHUNK_DATA(chunk), func);
  }
}

void
free_segstack_chunks(segchunk *c)
{ segchunk *n;

  for(; c; c = n)
  { n = c->next;
    tmp_free(c);
  }
}

void
clearSegStack_(segstack *s)
{ segchunk *c = s->first;
  segchunk *n;

  if ( !c->allocated )			/* statically allocated first chunk */
  { n = c->next;

    c->next = NULL;
    s->last = c;
    s->base = s->top = c->top;
    s->max  = addPointer(c, c->size);
    s->chunk_count = 1;

    free_segstack_chunks(n);
  } else				/* all dynamic chunks */
  { free_segstack_chunks(c);
    memset(s, 0, sizeof(*s));
  }
}
