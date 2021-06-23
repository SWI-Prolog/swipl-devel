/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2020, University of Amsterdam
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
#include "os/pl-cstack.h"

int
growBuffer(Buffer b, size_t minfree)
{ size_t osz = b->max - b->base, sz = osz;
  size_t top = b->top - b->base;
  char *new;

  if ( b->top + minfree <= b->max )
    return TRUE;

  if ( sz < 512 )
    sz = 512;				/* minimum reasonable size */
  while( top + minfree > sz )
    sz *= 2;

  if ( b->base == b->static_buffer )
  { sz = tmp_nalloc(sz);
    if ( !(new = tmp_malloc(sz)) )
      return FALSE;

    memcpy(new, b->static_buffer, osz);
  } else
  { sz = tmp_nrealloc(b->base, sz);
    if ( !(new = tmp_realloc(b->base, sz)) )
      return FALSE;
  }

  b->base = new;
  b->top = b->base + top;
  b->max = b->base + sz;

  return TRUE;
}


		 /*******************************
		 *	      STACK		*
		 *******************************/

void
discardStringStack(string_stack *stack)
{ unsigned int i;

  for(i=stack->allocated; i>0;i--)
  { string_buffer *sb = &stack->buffers[MSB(i)][i];

    if ( sb )
      discardBuffer(&sb->buf);
    else
      break;
  }

  for(i=0; stack->buffers[i]; i++)
  { unsigned int nelem = 1<<i;
    string_buffer *ptr = stack->buffers[i]+nelem;

    free(ptr);
  }
}


static string_buffer *
allocNewStringBuffer(string_stack *stack)
{ int k = MSB(stack->allocated+1);

  if ( !stack->buffers[k] )
  { if ( k == MAX_LG_STACKED_STRINGS )
    { fatalError("Too many stacked strings");
      assert(0);
    } else
    { unsigned int nelem = 1<<k;
      string_buffer *buffers = malloc(nelem*sizeof(*buffers));
      stack->buffers[k] = buffers - nelem;
    }
  }

  stack->top = ++stack->allocated;

  return &stack->buffers[k][stack->allocated];
}


static string_buffer *
allocStringBuffer(string_stack *stack)
{ string_buffer *b;

  if ( stack->top < stack->allocated )
  { unsigned int i = ++stack->top;
    b = &stack->buffers[MSB(i)][i];
  } else
  { b = allocNewStringBuffer(stack);
    initBuffer(&b->buf);
  }

  if ( stack->top == stack->tripwire )
  { Sdprintf("String stack reached tripwire at %d.  C-Stack:\n",
	     stack->tripwire);
    print_c_backtrace("stacked strings");
  }

  return b;
}


static string_buffer *
currentBuffer(string_stack *stack)
{ if ( stack->top )
  { unsigned int i = stack->top;
    return &stack->buffers[MSB(i)][i];
  }

  return NULL;
}


static unsigned int
popStringBuffer(string_stack *stack)
{ assert(stack->top);

  if ( __builtin_popcount(stack->top) == 1 && stack->top > 4 )
  { unsigned int i;
    unsigned int k = MSB(stack->allocated);
    string_buffer *ptr = stack->buffers[k]+(1<<k);

    DEBUG(MSG_STRING_BUFFER,
	  Sdprintf("Discarding string buffers %d..%d\n",
		   stack->top, stack->allocated));

    assert(k == MSB(stack->top));

    for(i=stack->allocated; i>=stack->top; i--)
    { string_buffer *sb = &stack->buffers[k][i];

      if ( sb )
	discardBuffer(&sb->buf);
      else
	break;
    }

    free(ptr);
    stack->buffers[k] = NULL;
    stack->allocated = --stack->top;
  } else
  { unsigned int i = stack->top--;
    string_buffer *b = &stack->buffers[MSB(i)][i];
    emptyBuffer(&b->buf, BUFFER_DISCARD_ABOVE>>i);
  }

  return stack->top;
}





		 /*******************************
		 *	  STRING BUFFER		*
		 *******************************/

#define discardable_buffer	(LD->fli._discardable_buffer)
#define sTop			(LD->fli._string_buffer)


Buffer
findBuffer(int flags)
{ GET_LD
  Buffer b;

  if ( flags & BUF_STACK )
  { string_buffer *sb = allocStringBuffer(&LD->fli.string_buffers);

    sb->frame = environment_frame ? consTermRef(environment_frame) : 0x0;

    b = (Buffer)&sb->buf;
    DEBUG(MSG_STRING_BUFFER,
	  Sdprintf("Added string buffer entry %p with level %zd\n",
		   sb, (size_t)sb->frame));

    LD->alerted |= ALERT_BUFFER;
  } else
  { b = &discardable_buffer;

    if ( !b->base )
      initBuffer(b);
    else
      emptyBuffer(b, BUFFER_DISCARD_ABOVE);
  }

  return b;
}


char *
buffer_string(const char *s, int flags)
{ Buffer b = findBuffer(flags);
  size_t l = strlen(s) + 1;

  addMultipleBuffer(b, s, l, char);

  return baseBuffer(b, char);
}


int
unfindBuffer(Buffer b, int flags)
{ if ( flags & BUF_STACK )
  { GET_LD
    StringBuffer sb = currentBuffer(&LD->fli.string_buffers);

    DEBUG(MSG_STRING_BUFFER,
	  { StringBuffer sb = currentBuffer(&LD->fli.string_buffers);
	    Sdprintf("Deleting top string buffer %p\n", sb);
	  });

    if ( b == (Buffer)&sb->buf )
      popStringBuffer(&LD->fli.string_buffers);
    else
      Sdprintf("OOPS: unfindBuffer(): not top buffer\n");

    return TRUE;
  }

  return FALSE;
}


void
PL_mark_string_buffers(DECL_LD buf_mark_t *mark)
{ *mark = LD->fli.string_buffers.top;
}

void
PL_release_string_buffers_from_mark(DECL_LD buf_mark_t mark)
{ while(LD->fli.string_buffers.top > mark)
    popStringBuffer(&LD->fli.string_buffers);
}


API_STUB(void)
(PL_mark_string_buffers)(buf_mark_t *mark)
( PL_mark_string_buffers(mark); )

API_STUB(void)
(PL_release_string_buffers_from_mark)(buf_mark_t mark)
( PL_release_string_buffers_from_mark(mark); )


void
release_string_buffers_from_frame(DECL_LD LocalFrame fr)
{ word offset = consTermRef(fr);

  for(;;)
  { StringBuffer sb = currentBuffer(&LD->fli.string_buffers);

    if ( sb && sb->frame >= offset )
      popStringBuffer(&LD->fli.string_buffers);
    else
      break;
  }
}
