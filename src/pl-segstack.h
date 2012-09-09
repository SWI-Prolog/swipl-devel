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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PL_SEGSTACK_H_INCLUDED
#define PL_SEGSTACK_H_INCLUDED

#define SEGSTACK_CHUNKSIZE (1*1024)

typedef struct segchunk
{ char  *top;				/* top when closed */
  size_t size;				/* size of the chunk */
					/* below clean using memset() */
  int	 allocated;			/* must call free on it */
  struct segchunk *next;		/* double linked list */
  struct segchunk *previous;
  char	 data[1];			/* data on my back */
} segchunk;

typedef struct
{ size_t   unit_size;
					/* below clean using memset() */
  segchunk *first;
  segchunk *last;
  char	   *base;
  char	   *top;
  char	   *max;
} segstack;


static inline int
emptySegStack(segstack *s)
{ return (s->top == s->base) &&
	 (s->last == NULL || s->last->previous == NULL);
}


#define popSegStack(stack, to, type) \
	( ((stack)->top >= (stack)->base + sizeof(type))	\
		? ( (stack)->top -= sizeof(type),		\
		    *to = *(type*)(stack)->top,			\
		    TRUE					\
		  )						\
		: !(stack)->last || !(stack)->last->previous ? FALSE \
		: popSegStack_((stack), to)			\
	)

#define pushSegStack(stack, data, type) \
	( ((stack)->top + sizeof(type) <= (stack)->max)	\
		? ( *(type*)(stack)->top = data,		\
		    (stack)->top += sizeof(type),		\
		    TRUE					\
		  )						\
		: pushSegStack_((stack), &data)			\
	)

COMMON(int)	pushSegStack_(segstack *stack, void* data) WUNUSED;
COMMON(int)	pushRecordSegStack(segstack *stack, Record r) WUNUSED;
COMMON(int)	popSegStack_(segstack *stack, void *data);
COMMON(void*)	topOfSegStack(segstack *stack);
COMMON(void)	popTopOfSegStack(segstack *stack);
COMMON(void)	scanSegStack(segstack *s, void (*func)(void *cell));
COMMON(void)	clearSegStack_(segstack *s);

		 /*******************************
		 *	       INLINE		*
		 *******************************/

static inline void
clearSegStack(segstack *s)
{ if ( s->first )
    clearSegStack_(s);
}


static inline void
topsOfSegStack(segstack *stack, int count, void **tops)
{ char *p = stack->top - stack->unit_size;
  char *base = stack->base;

  for(;;)
  { while(count > 0 && p >= base)
    { *tops++ = p;
      p -= stack->unit_size;
      count--;
    }

    if ( count > 0 )
    { segchunk *chunk = stack->last->previous;

      p = chunk->top - stack->unit_size;
      base = chunk->data;
    } else
      break;
  }
}


static inline void
initSegStack(segstack *stack, size_t unit_size, size_t len, void *data)
{ stack->unit_size = unit_size;

  if ( len )
  { segchunk *chunk = data;

#if O_DEBUG
    assert(len > sizeof(*chunk));
#endif
    chunk->size = len;
    stack->base = stack->top = chunk->top = chunk->data;
    stack->last = stack->first = chunk;
    stack->max  = addPointer(chunk, len);
    memset(&chunk->allocated, 0,
	   offsetof(segchunk,data)-offsetof(segchunk,allocated));
  } else
  { memset(&stack->first, 0, sizeof(*stack)-offsetof(segstack,first));
  }
}

#endif /*PL_SEGSTACK_H_INCLUDED*/
