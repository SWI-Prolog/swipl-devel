/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include "pl-incl.h"
#include "pl-buffer.h"

void
growBuffer(Buffer b, long int minfree)
{ long sz = b->max - b->base;
  long top = b->top - b->base;

  while( top + minfree > sz )
  { sz = sz ? 2*sz : 512;
  }

  b->base = (b->base ? realloc(b->base, sz) : malloc(sz));
  if ( !b->base )
    fatalError("Not enough memory");

  b->top = b->base + top;
  b->max = b->base + sz;
}
