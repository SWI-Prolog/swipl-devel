/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

		 /*******************************
		 *	    ALLOCATION		*
		 *******************************/

void *
xmalloc(size_t bytes)
{ void *mem;

  if ( !(mem = malloc(bytes)) && bytes > 0 )
  { MessageBox(NULL, "Not enough memory", "Error", MB_OK);
    exit(1);
  }
  
  return mem;
}


void *
xrealloc(void *mem, size_t bytes)
{ void *newmem;

  if ( !(newmem = xrealloc(mem, bytes)) && bytes > 0 )
  { MessageBox(NULL, "Not enough memory", "Error", MB_OK);
    exit(1);
  }
  
  return newmem;
}
