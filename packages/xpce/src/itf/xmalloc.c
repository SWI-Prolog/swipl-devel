/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

		/********************************
		*        XMALLOC/XREALLOC	*
		********************************/

#ifdef HAVE_DMALLOC_H
#undef xmalloc
#undef xrealloc
#endif

void *
xmalloc(size_t nbytes)
{ void *rval = pceMalloc(nbytes);

  if ( !rval )
  { Cprintf("[PCE: Not enough memory]\n");
    exit(1);
  }

  return rval;
}


void *
xrealloc(void *ptr, size_t nbytes)
{ void *rval = pceRealloc(ptr, nbytes);

  if ( !rval )
  { Cprintf("[PCE: Not enough memory]\n");
    exit(1);
  }

  return rval;
}

