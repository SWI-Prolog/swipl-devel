/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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

