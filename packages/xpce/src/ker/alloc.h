/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define ALLOCSIZE	10240		/* size of allocation space */
#define ALLOCFAST	1024

GLOBAL char    *spaceptr;	/* allocation space */
GLOBAL int	spacefree;	/* Free bytes in space */

GLOBAL long	allocbytes;	/* number of bytes allocated */
GLOBAL long	wastedbytes;	/* core in allocation chains */

typedef struct zone *Zone;	/* memory zone */

struct zone
{ 
#if ALLOC_DEBUG
  unsigned	in_use : 1;		/* Zone is in_use (1) or freed (0) */
  unsigned	size   : 31;		/* Size of the zone (bytes) */
  ulong		magic;			/* Magic word */
#endif
  ulong		start;			/* Reserved (start of zone) */
  Zone		next;			/* Next zone of this size */
};

GLOBAL Zone freeChains[ALLOCFAST/sizeof(Zone)+1];

#define struct_offset(structure, field) ((int) &(((structure *)NULL)->field))
#define MINALLOC    (sizeof(struct zone) - struct_offset(struct zone, start))
#define ROUNDALLOC  (sizeof(void *))

#define roundAlloc(n) ((n) < MINALLOC          ? MINALLOC : \
		       ((n) % ROUNDALLOC) == 0 ? (n) : \
					         ((n)|(ROUNDALLOC-1))+1)
