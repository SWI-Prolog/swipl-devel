/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CHUNKSIZE (1*1024)

void *
allocSegStack(segstack *stack)
{ if ( stack->top + stack->unit_size <= stack->max )
  { void *r = stack->top;
    stack->top += stack->unit_size;
    stack->count++;

    return r;
  } else
  { segchunk *chunk = PL_malloc(CHUNKSIZE);

    if ( !chunk )
      return NULL;			/* out of memory */

    chunk->previous = stack->last;
    if ( stack->last )
    { stack->last->next = chunk;
      stack->last->top = stack->top;
      stack->last = chunk;
    } else
    { stack->last = stack->first = chunk;
    }

    stack->base = chunk->data;
    stack->max  = addPointer(chunk, CHUNKSIZE);
    stack->top  = chunk->data + stack->unit_size;
    stack->count++;

    return chunk->data;
  }
}


int
pushSegStack(segstack *stack, void* data)
{ void *e;

  if ( (e = allocSegStack(stack)) )
  { memcpy(e, data, stack->unit_size);

    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pop data. Note that we leave the first chunk associated with the stack
to speedup frequent small usage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
popSegStack(segstack *stack, void *data)
{ again:

  if ( stack->top >= stack->base + stack->unit_size )
  { stack->top -= stack->unit_size;
    memcpy(data, stack->top, stack->unit_size);
    stack->count--;

    return TRUE;
  } else
  { segchunk *chunk = stack->last;

    if ( chunk )
    { if ( chunk->previous )
      { stack->last = chunk->previous;
	stack->last->next = NULL;
	PL_free(chunk);
      
	chunk = stack->last;
	stack->base = chunk->data;
	stack->max  = addPointer(chunk, CHUNKSIZE);
	stack->top  = chunk->top;
	goto again;
      } 
#if 0
        else
      { PL_free(chunk);
	stack->first = stack->last = NULL;
	stack->base = stack->max = stack->top = NULL;
      }
#endif
    }

    return FALSE;
  }
}


void
clearSegStack(segstack *s)
{ segchunk *c, *n;

  c = s->first;
  memset(s, 0, sizeof(*s));

  for(; c; c = n)
  { n = c->next;
    PL_free(c);
  }
}
