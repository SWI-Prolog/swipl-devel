/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include "pl-incl.h"

void
growBuffer(Buffer b, long int minfree)
{ long osz = b->max - b->base, sz = osz;
  long top = b->top - b->base;

  while( top + minfree > sz )
    sz *= 2;

  if ( b->base != b->static_buffer )
  { b->base = realloc(b->base, sz);
    if ( !b->base )
      outOfCore();
  } else
  { char *old = b->base;
    b->base = malloc(sz);
    if ( !b->base )
      outOfCore();
    memcpy(b->base, old, osz);
  }

  b->top = b->base + top;
  b->max = b->base + sz;
}
