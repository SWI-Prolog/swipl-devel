/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2012, University of Amsterdam
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

int
growBuffer(Buffer b, size_t minfree)
{ size_t osz = b->max - b->base, sz = osz;
  size_t top = b->top - b->base;
  char *new;

  if ( b->max - b->top >= (int)minfree )
    return TRUE;

  if ( sz < 512 )
    sz = 512;				/* minimum reasonable size */
  while( top + minfree > sz )
    sz *= 2;

  if ( b->base == b->static_buffer )
  { if ( !(new = malloc(sz)) )
      return FALSE;

    memcpy(new, b->static_buffer, osz);
  } else
  { if ( !(new = realloc(b->base, sz)) )
      return FALSE;
  }

  b->base = new;
  b->top = b->base + top;
  b->max = b->base + sz;

  return TRUE;
}


		 /*******************************
		 *	    BUFFER RING		*
		 *******************************/

#define discardable_buffer	(LD->fli._discardable_buffer)
#define buffer_ring		(LD->fli._buffer_ring)
#define current_buffer_id	(LD->fli._current_buffer_id)

Buffer
findBuffer(int flags)
{ GET_LD
  Buffer b;

  if ( flags & BUF_RING )
  { if ( ++current_buffer_id == BUFFER_RING_SIZE )
      current_buffer_id = 0;
    b = &buffer_ring[current_buffer_id];
  } else
    b = &discardable_buffer;

  if ( !b->base )
    initBuffer(b);

  emptyBuffer(b);
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
unfindBuffer(int flags)
{ GET_LD
  if ( flags & BUF_RING )
  { if ( --current_buffer_id <= 0 )
      current_buffer_id = BUFFER_RING_SIZE-1;
  }

  fail;
}
