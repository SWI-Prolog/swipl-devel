/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <memory.h>
#ifndef O_RUNTIME
#define ALLOC_DEBUG 1
#else
#define ALLOC_DEBUG 0
#endif
#include "alloc.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PCE allocates  memory for two purposes: for  object structures and for
alien data.  Most small chunks of memory that are allocated reoccur in
about  the same  relative numbers.    For  this reason  PCE addopts  a
perfect fit strategy for memory allocation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Zone allocate(int);		/* forwards */

Any
alloc(register int n)
{ n = roundAlloc(n);
  allocbytes += n;

  DEBUG(NAME_allocate, printf("alloc(%d)\n", n));

  if ( n <= ALLOCFAST )
  { register Zone z;
    register int m = n / sizeof(Zone);

    if ( (z = freeChains[m]) != NULL )	/* perfect fit */
    { freeChains[m] = (Zone) z->next;
      if ( freeChains[m] == NULL )
	tailFreeChains[m] = NULL;

      wastedbytes -= n;

#if ALLOC_DEBUG
      assert((long) z >= allocBase && (long) z <= allocTop);
      z->in_use = TRUE;
#endif

      return &z->start;
    }

    return allocate(n);			/* new memory */
  }

  return malloc(n);			/* malloc() it */
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

#define offset(structure, field) ((int) &(((structure *)NULL)->field))

void
unalloc(register int n, Any p)
{ register Zone z = p;
  n = roundAlloc(n);
  allocbytes -= n;
  
  DEBUG(NAME_allocate, printf("unalloc(%d)\n", n));

  if ( n <= ALLOCFAST )
  { assert((long)z >= allocBase && (long)z <= allocTop);

#if ALLOC_DEBUG
    z = (Zone) ((char *)z - offset(struct zone, start));
    assert(z->in_use == TRUE);
    assert(z->size == n);
    z->in_use = FALSE;
#endif

    wastedbytes += n;
    n /= sizeof(Zone);
    z->next = freeChains[n];
    freeChains[n] = z;
    if ( !tailFreeChains[n] )
      tailFreeChains[n] = z;

    DEBUG(NAME_allocate,
	  printf("unalloc for %s, m = %d\n", pp(z), n));
    
    return;
  }

  free(z);
}


static Zone
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
    z->size = size;
    z->in_use = TRUE;
#endif
    return (Zone) &z->start;
  }

#if !ALLOC_DEBUG
  if ( spacefree >= sizeof(struct zone) )
    unalloc(spacefree, spaceptr);
#endif
  if ( !(p = malloc(ALLOCSIZE)) )
  { fprintf(stderr,
	    "[PCE FATAL ERROR: Malloc(%d) failed.  Swap space full?]\n",
	    ALLOCSIZE);
    exit(1);
  }
#if ALLOC_DEBUG
  memset(p, 0, ALLOCSIZE);
#endif

  top       = (long) p + ALLOCSIZE - 1;
  base      = (long) p;
  allocRange(p, ALLOCSIZE);

  spaceptr = p + alloc_size;
  spacefree = ALLOCSIZE - alloc_size;

#if ALLOC_DEBUG
  z = (Zone) p;
  z->size = size;
  z->in_use = TRUE;

  return (Zone) &z->start;
#else
  return (Zone) p;
#endif
}


void
initAlloc(void)
{ int t;

  spaceptr  = NULL;
  spacefree = 0;
  for (t=ALLOCFAST/sizeof(Zone); t>=0; t--)
    freeChains[t] = tailFreeChains[t] = NULL;

  wastedbytes = allocbytes = 0;
  allocTop  = 0L;
  allocBase = 0xffffffff;
  alloc(0);				/* initialise Top/Base */
}


void
allocRange(void *low, int size)
{ ulong l = (ulong)low;

  if ( l < allocBase )
    allocBase = l;
  if ( l+size > allocTop )
    allocTop = l+size;
}


status
listWastedCorePce(Pce pce, Bool ppcells)
{ int n;
  Zone z;

  printf("Wasted core:\n");
  for(n=0; n <= ALLOCFAST/sizeof(Zone); n++)
  { if ( freeChains[n] != NULL )
    { if ( ppcells == ON )
      { printf("    Size = %ld:\n", (ulong) n*sizeof(Zone));
	for(z = freeChains[n]; z; z = z->next)
	  printf("\t%s\n", pp(z));
      } else
      { int m;

	for(z = freeChains[n], m = 0; z; z = z->next, m++)
	  ;
	printf("\tSize = %3ld\t%4d cells:\n", (ulong) n*sizeof(Zone), m);
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
