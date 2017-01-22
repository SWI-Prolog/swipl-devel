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

#ifndef PL_SEGSTACK_H_INCLUDED
#define PL_SEGSTACK_H_INCLUDED

#define SEGSTACK_CHUNKSIZE (1*1024)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The segchunk struct has its data attached   to  its back. The data field
itself is typed double to ensure that   the  data is properly aligned to
allow access of doubles. This is needed to get properly aligned pointers
after topsOfSegStack() in evalExpression().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct segchunk
{ char  *top;				/* top when closed */
  size_t size;				/* size of the chunk */
					/* below clean using memset() */
  int	 allocated;			/* must call free on it */
  struct segchunk *next;		/* double linked list */
  struct segchunk *previous;
  double data[1];			/* data on my back */
} segchunk;

#define CHUNK_DATA(c)	((char *)(c)->data)

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
COMMON(void)	popTopOfSegStack_(segstack *stack);
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
      base = CHUNK_DATA(chunk);
    } else
      break;
  }
}


/* quickPopTopOfSegStack() only performs a pop if we do not
   need to discard a chunk.  $collect_findall_bag/2 needs
   addition synchronization in that case.
*/

static inline int
quickPopTopOfSegStack(segstack *stack)
{ if ( stack->top >= stack->base + stack->unit_size )
  { stack->top -= stack->unit_size;
    return TRUE;
  }

  return FALSE;
}


static inline void
popTopOfSegStack(segstack *stack)
{ if ( !quickPopTopOfSegStack(stack) )
    popTopOfSegStack_(stack);
}


static inline void
initSegStack(segstack *stack, size_t unit_size, size_t len, void *data)
{ stack->unit_size = unit_size;

  if ( len )
  { segchunk *chunk = data;

#if O_DEBUG
    assert(len > sizeof(*chunk));
    assert(unit_size%sizeof(void*) == 0);
#endif
    chunk->size = len;
    stack->base = stack->top = chunk->top = CHUNK_DATA(chunk);
    stack->last = stack->first = chunk;
    stack->max  = addPointer(chunk, len);
    memset(&chunk->allocated, 0,
	   offsetof(segchunk,data)-offsetof(segchunk,allocated));
  } else
  { memset(&stack->first, 0, sizeof(*stack)-offsetof(segstack,first));
  }
}

#endif /*PL_SEGSTACK_H_INCLUDED*/
