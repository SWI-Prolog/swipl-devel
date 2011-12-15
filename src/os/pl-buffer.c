/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

#include "pl-incl.h"

int
growBuffer(Buffer b, size_t minfree)
{ size_t osz = b->max - b->base, sz = osz;
  size_t top = b->top - b->base;

  if ( b->max - b->top >= (int)minfree )
    return TRUE;

  if ( sz < 512 )
    sz = 512;				/* minimum reasonable size */
  while( top + minfree > sz )
    sz *= 2;

  if ( b->base != b->static_buffer )
  { b->base = realloc(b->base, sz);
    if ( !b->base )
      return FALSE;
  } else			/* from static buffer */
  { char *new;

    if ( !(new = malloc(sz)) )
      return FALSE;

    memcpy(new, b->static_buffer, osz);
    b->base = new;
  }

  b->top = b->base + top;
  b->max = b->base + sz;

  return TRUE;
}


		 /*******************************
		 *	    BUFFER RING		*
		 *******************************/

#define discardable_buffer 	(LD->fli._discardable_buffer)
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
