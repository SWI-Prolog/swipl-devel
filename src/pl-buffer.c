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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "pl-incl.h"

void
growBuffer(Buffer b, long int minfree)
{ long osz = b->max - b->base, sz = osz;
  long top = b->top - b->base;

  if ( sz < 512 )
    sz = 512;				/* minimum reasonable size */
  while( top + minfree > sz )
    sz *= 2;

  if ( b->base != b->static_buffer )
  {
#ifdef BUFFER_USES_MALLOC
    b->base = realloc(b->base, sz);
    if ( !b->base )
      outOfCore();
#else
    char *old = b->base;
    b->base = allocHeap(sz);
    memcpy(b->base, old, osz);
#endif
  } else
  { char *old = b->base;
#ifdef BUFFER_USES_MALLOC
    b->base = malloc(sz);
    if ( !b->base )
      outOfCore();
#else
    b->base = allocHeap(sz);
#endif
    memcpy(b->base, old, osz);
  }

  b->top = b->base + top;
  b->max = b->base + sz;
}
