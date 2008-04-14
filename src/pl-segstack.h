/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

#ifndef PL_SEGSTACK_H_INCLUDED
#define PL_SEGSTACK_H_INCLUDED

typedef struct segchunk
{ struct segchunk *next;		/* double linked list */
  struct segchunk *previous;
  char  *top;				/* top when closed */
  char	 data[1];			/* data on my back */
} segchunk;

typedef struct
{ segchunk *first;
  segchunk *last;
  size_t   unit_size;
  char	   *base;
  char	   *top;
  char	   *max;
  size_t    count;
} segstack;


COMMON(void *)	allocSegStack(segstack *stack);
COMMON(int)	pushSegStack(segstack *stack, void* data);
COMMON(int)	popSegStack(segstack *stack, void *data);
COMMON(void)	scanSegStack(segstack *s, void (*func)(void *cell));
COMMON(void)	clearSegStack(segstack *s);


#endif /*PL_SEGSTACK_H_INCLUDED*/
