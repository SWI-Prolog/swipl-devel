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

#define ALLOCSIZE	65000		/* size of allocation space */
#define ALLOCFAST	1024

GLOBAL char    *spaceptr;	/* allocation space */
GLOBAL unsigned int	spacefree;	/* Free bytes in space */

GLOBAL long	allocbytes;	/* number of bytes allocated */
GLOBAL long	wastedbytes;	/* core in allocation chains */

typedef struct zone *Zone;	/* memory zone */

struct zone
{ 
#if ALLOC_DEBUG
  unsigned	in_use : 1;		/* Zone is in_use (1) or freed (0) */
  unsigned	size   : 31;		/* Size of the zone (bytes) */
  unsigned long magic;			/* Magic word */
#endif
  unsigned long start;			/* Reserved (start of zone) */
  Zone		next;			/* Next zone of this size */
};

GLOBAL Zone freeChains[ALLOCFAST/sizeof(Zone)+1];

#define struct_offset(structure, field) ((int) &(((structure *)NULL)->field))
#define MINALLOC    (sizeof(struct zone) - struct_offset(struct zone, start))
#define ROUNDALLOC  (sizeof(void *))

#define roundAlloc(n) ((n) <= MINALLOC ? MINALLOC : \
		       (((n) + (ROUNDALLOC - 1)) & ~(ROUNDALLOC-1)))
