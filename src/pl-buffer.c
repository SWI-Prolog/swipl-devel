/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
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
