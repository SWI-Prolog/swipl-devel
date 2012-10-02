/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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
popTopOfSegStack(segstack *stack)
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

This is used by markAtomsFindall(), which   is  called asynchronously by
AGC. Note that this is _not_ concurrent.  The thread is either signalled
(Unix) or stopped (Windows). We notably   need good synchronization with
popTopOfSegStack(). Notably, we must  ensure   that  stack->top  is only
valid if stack->base == chunk->data.
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
      chunk->top = stack->top;		/* close last chunk */
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
