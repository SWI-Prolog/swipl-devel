/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2013, University of Amsterdam
                              VU University Amsterdam
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

int
pushSegStack_(segstack *stack, void *data)
{ if ( stack->top + stack->unit_size <= stack->max )
  { memcpy(stack->top, data, stack->unit_size);
    stack->top += stack->unit_size;

    return TRUE;
  } else
  { segchunk *chunk = PL_malloc(SEGSTACK_CHUNKSIZE);

    if ( !chunk )
      return FALSE;			/* out of memory */

    chunk->allocated = TRUE;
    chunk->size = SEGSTACK_CHUNKSIZE;
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
    memcpy(CHUNK_DATA(chunk), data, stack->unit_size);
    stack->top  = CHUNK_DATA(chunk) + stack->unit_size;

    return TRUE;
  }
}


int
pushRecordSegStack(segstack *stack, Record r)
{ if ( stack->top + sizeof(r) <= stack->max )
  { Record *rp = (Record*)stack->top;

    *rp++ = r;
    stack->top = (char*)rp;

    return TRUE;
  } else
  { return pushSegStack_(stack, &r);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pop data. Note that we leave the first chunk associated with the stack
to speedup frequent small usage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
popSegStack_(segstack *stack, void *data)
{ again:

  if ( stack->top >= stack->base + stack->unit_size )
  { stack->top -= stack->unit_size;
    memcpy(data, stack->top, stack->unit_size);

    return TRUE;
  } else
  { segchunk *chunk = stack->last;

    if ( chunk )
    { if ( chunk->previous )
      { stack->last = chunk->previous;
	stack->last->next = NULL;
	if ( chunk->allocated )
	  PL_free(chunk);

	chunk = stack->last;
	stack->base = CHUNK_DATA(chunk);
	stack->max  = addPointer(chunk, chunk->size);
	stack->top  = chunk->top;
	goto again;
      }
    }

    return FALSE;
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

	stack->last = chunk->previous;
	stack->last->next = NULL;
	chunk = stack->last;
	stack->top  = chunk->top;
	MemoryBarrier();		/* Sync with scanSegStack() */
	stack->base = CHUNK_DATA(chunk);
	stack->max  = addPointer(chunk, chunk->size);

	if ( del->allocated )
	  PL_free(del);

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
tests. The issue is triggered by Tests/thread_agc_findall.pl, notably on
slow single core hardware.
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
clearSegStack_(segstack *s)
{ segchunk *c = s->first;
  segchunk *n;

  if ( !c->allocated )		/* statically allocated first chunk */
  { n = c->next;

    c->next = NULL;
    s->last = c;
    s->base = s->top = c->top;
    s->max  = addPointer(c, c->size);

    for(c=n; c; c = n)
    { n = c->next;
      PL_free(c);
    }
  } else				/* all dynamic chunks */
  { for(; c; c = n)
    { n = c->next;
      PL_free(c);
    }
    memset(s, 0, sizeof(*s));
  }
}
