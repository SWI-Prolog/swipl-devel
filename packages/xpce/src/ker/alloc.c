/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif
#ifndef ALLOC_DEBUG
#ifndef O_RUNTIME
#define ALLOC_DEBUG 0			/* 1 or 2 */
#else
#define ALLOC_DEBUG 0
#endif /*O_RUNTIME*/
#endif /*ALLOC_DEBUG*/
#include "alloc.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Debugging note: This module can run at three debugging levels:

    ALLOC_DEBUG = 0
	Performs no runtime checks.

    ALLOC_DEBUG = 1
	Adds a word to each chunk that maintains the size.  Validates
	that unalloc() is called with the same size as alloc() and that
	unalloc() is not called twice on the same object.  Clears memory
	to 0 that has been initially requested from the OS.  This mode
	requires little runtime overhead.

    ALLOC_DEBUG = 2
	In this mode all memory that is considered uninitialised is filled
	with ALLOC_MAGIC_BYTE (0xcc).  unalloc() will fill the memory.
	alloc() will check that the memory is still all 0xcc, which traps
	occasions where unalloc'ed memory is changed afterwards.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ALLOC_MAGIC_BYTE 0xcc
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
{ char *p;
  long top, base;
  Zone z;
  int alloc_size = size + offset(struct zone, start);

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

  if ( !(p = pceMalloc(ALLOCSIZE)) )
  { Cprintf("[PCE FATAL ERROR: malloc(%d) failed.  Swap space full?]\n",
	    ALLOCSIZE);
    exit(1);
  }
#if ALLOC_DEBUG > 1
  memset(p, ALLOC_MAGIC_BYTE, ALLOCSIZE);
#else
  memset(p, 0, ALLOCSIZE);
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

  return (Zone) &z->start;
#else
  return (Zone) p;
#endif
}


Any
alloc(register int n)
{ n = roundAlloc(n);
  allocbytes += n;

#if ALLOC_DEBUG
  DEBUG(NAME_allocate, Cprintf("alloc(%d)\n", n));
#endif

  if ( n <= ALLOCFAST )
  { register Zone z;
    register int m = n / sizeof(Zone);

    if ( (z = freeChains[m]) != NULL )	/* perfect fit */
    { 
#if ALLOC_DEBUG
      assert((long) z >= allocBase && (long) z <= allocTop);
      assert(z->in_use == FALSE);
      assert(z->magic  == ALLOC_MAGIC_WORD);

      z->in_use = TRUE;
#endif

#if ALLOC_DEBUG > 1
      { char *p;
	for(p = (char *)&z->start + n;
	    --p >= ((char *)&z->next + sizeof(z->next));
	    )
	  assert(*p == ALLOC_MAGIC_BYTE);
      }
#else
      memset(&z->start, 0, n);
#endif

      freeChains[m] = (Zone) z->next;
      wastedbytes -= n;

      return &z->start;
    }

    return allocate(n);			/* new memory */
  }

#if ALLOC_DEBUG > 1
{ Any p = pceMalloc(n);
  memset(p, ALLOC_MAGIC_BYTE, n);
  return p;
}
#else
  return pceMalloc(n);			/* malloc() it */
#endif
}


void
unalloc(register int n, Any p)
{ register Zone z = p;
  n = roundAlloc(n);
  allocbytes -= n;
  
  DEBUG(NAME_allocate, Cprintf("unalloc(%d)\n", n));

  if ( n <= ALLOCFAST )
  { assert((long)z >= allocBase && (long)z <= allocTop);

#if ALLOC_DEBUG
#if ALLOC_DEBUG > 1
    memset(p, ALLOC_MAGIC_BYTE, n);
#endif
    z = (Zone) ((char *)z - offset(struct zone, start));
    assert(z->magic  == ALLOC_MAGIC_WORD);
    assert(z->in_use == TRUE);
    assert(z->size   == n);
    z->in_use = FALSE;
#endif

    wastedbytes += n;
    n /= sizeof(Zone);
    z->next = freeChains[n];
    freeChains[n] = z;

    DEBUG(NAME_allocate,
	  Cprintf("unalloc for %s, m = %d\n", pp(z), n));
    
    return;
  }

#if ALLOC_DEBUG > 1
  memset(p, ALLOC_MAGIC_BYTE, n);
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
{ ulong l = (ulong)low;

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
{ int n;
  Zone z;

  Cprintf("Wasted core:\n");
  for(n=0; n <= ALLOCFAST/sizeof(Zone); n++)
  { if ( freeChains[n] != NULL )
    { if ( ppcells == ON )
      { Cprintf("    Size = %ld:\n", (ulong) n*sizeof(Zone));
	for(z = freeChains[n]; z; z = z->next)
	  Cprintf("\t%s\n", pp(z));
      } else
      { int m;

	for(z = freeChains[n], m = 0; z; z = z->next, m++)
	  ;
	Cprintf("\tSize = %3ld\t%4d cells:\n", (ulong) n*sizeof(Zone), m);
      }
    }
  }
  
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
