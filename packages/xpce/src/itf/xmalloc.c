/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include "md.h"

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <h/interface.h>

		/********************************
		*        XMALLOC/XREALLOC	*
		********************************/

void *
xmalloc(size_t nbytes)
{ void *rval = malloc(nbytes);

  if ( !rval )
  { fprintf(stderr, "[PCE: Not enough memory]\n");
    exit(1);
  }

  return rval;
}


void *
xrealloc(void *ptr, size_t nbytes)
{ void *rval = realloc(ptr, nbytes);

  if ( !rval )
  { fprintf(stderr, "[PCE: Not enough memory]\n");
    exit(1);
  }

  return rval;
}

