/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <h/kernel.h>
#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif
#ifndef ALLOC_DEBUG
#ifndef O_RUNTIME
#if defined(_DEBUG) && defined(WIN32)
#define ALLOC_DEBUG 2
#else
#define ALLOC_DEBUG 0			/* 1 or 2 */
#endif
#else
#define ALLOC_DEBUG 0
#endif /*O_RUNTIME*/
#endif /*ALLOC_DEBUG*/
#include "alloc.h"

/* Use this to revert to plain malloc() and use external malloc
   debuggers */
/*#define ALLOCFAST 0*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Debugging note: This module can run at three debugging levels:

    ALLOC_DEBUG = 0
	Performs no runtime checks.

    ALLOC_DEBUG = 1
	Adds a word to each chunk that maintains the size.  Validates
	that unalloc() is called with the same size as alloc() and that
	unalloc() is not called twice on the same object.  Fills memory
	with ALLOC_MAGIC_BYTE that has been initially requested from the OS.
	This mode requires little runtime overhead.

    ALLOC_DEBUG = 2
	In this mode all memory that is considered uninitialised is filled
	with ALLOC_MAGIC_BYTE (0xcc).  unalloc() will fill the memory.
	alloc() will check that the memory is still all 0xcc, which traps
	occasions where unalloc'ed memory is changed afterwards.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ALLOC_MAGIC_FREE 0xcc
#define ALLOC_MAGIC_USED 0xbf
#define ALLOC_MAGIC_WORD 0xdf6556fd

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PCE allocates  memory for two purposes: for  object structures and for
alien data.  Most small chunks of memory that are allocated reoccur in
about  the same  relative numbers.    For  this reason  PCE addopts  a
perfect fit strategy for memory allocation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define offset(structure, field) ((int) &(((structure *)NULL)->field))

static inline Zone
allocate(int size)
{ unsigned char *p;
  long top, base;
  Zone z;
  unsigned int alloc_size = size + offset(struct zone, start);

  if ( alloc_size <= spacefree )
  { z = (Zone) spaceptr;
    spaceptr += alloc_size;
    spacefree -= alloc_size;

#if ALLOC_DEBUG
    z->size   = size;
    z->in_use = TRUE;
    z->magic  = ALLOC_MAGIC_WORD;
#endif
    return (Zone) &z->start;
  }

  if ( spacefree >= sizeof(struct zone) )
  {
    DEBUG(NAME_allocate, Cprintf("Unalloc remainder of %d bytes\n", spacefree));
#if ALLOC_DEBUG
    z = (Zone) spaceptr;
    z->size   = &spaceptr[spacefree] - (char *) &z->start;
    z->in_use = TRUE;
    z->magic  = ALLOC_MAGIC_WORD;
    unalloc(z->size, &z->start);
    assert((z->size % ROUNDALLOC) == 0);
    assert((z->size >= MINALLOC));
#else
    unalloc(spacefree, spaceptr);
    assert((spacefree % ROUNDALLOC) == 0);
    assert((spacefree >= MINALLOC));
#endif
  }

  p = pceMalloc(ALLOCSIZE);

#if ALLOC_DEBUG
  memset(p, ALLOC_MAGIC_FREE, ALLOCSIZE);
#endif

  top       = (long) p + ALLOCSIZE - 1;
  base      = (long) p;
  allocRange(p, ALLOCSIZE);

  spaceptr = p + alloc_size;
  spacefree = ALLOCSIZE - alloc_size;

#if ALLOC_DEBUG
  z = (Zone) p;
  z->size   = size;
  z->in_use = TRUE;
  z->magic  = ALLOC_MAGIC_WORD;
  memset(&z->start, ALLOC_MAGIC_USED, size);

  return (Zone) &z->start;
#else
  return (Zone) p;
#endif
}


#if ALLOC_DEBUG
static int
count_zone_chain(Zone z)
{ int n = 0;

  for( ; z; z = z->next )
    n++;

  return n;
}
#endif


Any
alloc(unsigned int n)
{ void *ptr;

  n = roundAlloc(n);
  allocbytes += n;

  if ( n <= ALLOCFAST )
  { Zone z;
    int m = n / sizeof(Zone);

    if ( (z = freeChains[m]) != NULL )	/* perfect fit */
    { 
#if ALLOC_DEBUG
      assert((long) z >= allocBase && (long) z <= allocTop);
      assert(z->in_use == FALSE);
      assert(z->magic  == ALLOC_MAGIC_WORD);
      assert((long)z->next % 4 == 0);

      z->in_use = TRUE;
#endif

      freeChains[m] = (Zone) z->next;
      wastedbytes -= n;

#if ALLOC_DEBUG > 1
      { unsigned char *p, *e;
	e = (unsigned char *)&z->next + sizeof(z->next);

	for(p = (unsigned char *)&z->start + n; --p >= e; )
	  assert(*p == ALLOC_MAGIC_FREE);
      }
#else
#if ALLOC_DEBUG
      setdata((Zone *)&z->start, 0, Zone, m);	/* should not be there */
#endif
#endif

#if ALLOC_DEBUG
      DEBUG(NAME_allocate,
	    Cprintf("alloc(%d): reuse, left %d\n",
		    n, count_zone_chain(freeChains[m])));
#endif

      ptr = &z->start;
      memset(ptr, ALLOC_MAGIC_USED, n);
      return ptr;
    }

#if ALLOC_DEBUG
  DEBUG(NAME_allocate, Cprintf("alloc(%d): new\n", n));
#endif

    return allocate(n);			/* new memory */
  }

  ptr = pceMalloc(n);
  allocRange(ptr, n);

#if ALLOC_DEBUG > 1
  memset(ptr, ALLOC_MAGIC_USED, n);
#endif

  return ptr;
}


void
unalloc(unsigned int n, Any p)
{ Zone z = p;
  n = roundAlloc(n);
  allocbytes -= n;
  
  if ( n <= ALLOCFAST )
  { int m = n / sizeof(Zone);
    assert((unsigned long)z >= allocBase && (unsigned long)z <= allocTop);

#if ALLOC_DEBUG
    assert((unsigned long)z % 4 == 0);
#if ALLOC_DEBUG > 1
    memset(p, ALLOC_MAGIC_FREE, n);
#endif
    z = (Zone) ((char *)z - offset(struct zone, start));
    assert(z->magic  == ALLOC_MAGIC_WORD);
    assert(z->in_use == TRUE);
    assert(z->size   == n);
    z->in_use = FALSE;
#endif

    wastedbytes += n;
    z->next = freeChains[m];
    freeChains[m] = z;

#if ALLOC_DEBUG
    DEBUG(NAME_allocate,
	  Cprintf("unalloc %d bytes for %s, m = %d, now %d\n",
		  n, pp(z), m, count_zone_chain(freeChains[m])));
#endif
    
    return;
  }

#if ALLOC_DEBUG > 1
  memset(p, ALLOC_MAGIC_FREE, n);
#endif

  pceFree(z);
}


void
initAlloc(void)
{ int t;

  spaceptr  = NULL;
  spacefree = 0;
  for (t=ALLOCFAST/sizeof(Zone); t>=0; t--)
    freeChains[t] = NULL;

  wastedbytes = allocbytes = 0;
  allocTop  = 0L;
  allocBase = 0xffffffff;
  alloc(sizeof(long));			/* initialise Top/Base */
#ifdef VARIABLE_POINTER_OFFSET
  pce_data_pointer_offset = allocBase & 0xf0000000L;
#endif
}



void
allocRange(void *low, int size)
{ unsigned long l = (unsigned long)low;

  if ( l < allocBase )
    allocBase = l;
  if ( l+size > allocTop )
    allocTop = l+size;
}


#if ALLOC_DEBUG
void
checkFreeChains()
{ int n;

  for(n=0; n<=ALLOCFAST/sizeof(Zone); n++)
  { Zone z = freeChains[n];

    for(; z != NULL; z = z->next)
    { assert((long)z >= allocBase && (long)z <= allocTop);
      assert(z->next == NULL ||
	     ((long)z->next >= allocBase && (long)z->next <= allocTop));
    }
  }
}
#endif 


status
listWastedCorePce(Pce pce, Bool ppcells)
{ unsigned int n;
  Zone z;
  int total = 0;

  Cprintf("Wasted core:\n");
  for(n=0; n <= ALLOCFAST/sizeof(Zone); n++)
  { if ( freeChains[n] != NULL )
    { unsigned long size = (unsigned long) n*sizeof(Zone);

      if ( ppcells == ON )
      { Cprintf("    Size = %ld:\n", size);
	for(z = freeChains[n]; z; z = z->next)
	{ Cprintf("\t%s\n", pp(z));
	  total += size;
	}
      } else
      { int m;

	for(z = freeChains[n], m = 0; z; z = z->next, m++)
	  ;
	Cprintf("\tSize = %3ld\t%4d cells:\n", size, m);
	total += size * m;
      }
    }
  }
  
  Cprintf("Total wasted: %ld bytes\n", total);

  succeed;
}


char *
save_string(const char *s)
{ char *t;

  t = alloc(strlen(s) + 1);
  strcpy(t, s);

  return t;
}


void
free_string(char *s)
{ unalloc(strlen(s)+1, s);
}
