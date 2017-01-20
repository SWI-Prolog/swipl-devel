/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef _PL_INDIRECT_H
#define _PL_INDIRECT_H

#define MAX_INDIRECT_BLOCKS		32
#define PREALLOCATED_INDIRECT_BLOCKS	32

		 /*******************************
		 *	      TYPES		*
		 *******************************/

typedef struct indirect
{ unsigned int		references;	/* reference count */
  word			handle;		/* public handle */
  word			header;		/* indirect header */
  word		       *data;		/* associated data */
  struct indirect      *next;		/* next in hash chain */
} indirect;

typedef struct indirect_array		/* dynamic array of indirects */
{ indirect *blocks[MAX_INDIRECT_BLOCKS];
  indirect  preallocated[PREALLOCATED_INDIRECT_BLOCKS];
} indirect_array;

typedef struct indirect_buckets		/* The actual buckets */
{ unsigned int	size;
  indirect    **buckets;
  struct indirect_buckets *prev;
} indirect_buckets;

typedef struct
{ indirect_array    array;		/* dynamic array of indirects */
  indirect_buckets *table;		/* the current table */
  size_t	    no_hole_before;	/* find free place */
  size_t	    highest;		/* highest returned indirect */
  size_t	    count;		/* number of indirects in table */
#ifdef O_PLMT
  simpleMutex	    mutex;		/* for resizing */
#endif
} indirect_table;


		 /*******************************
		 *	     FUNCTIONS		*
		 *******************************/

COMMON(indirect_table*)	new_indirect_table(void);
COMMON(void)		destroy_indirect_table(indirect_table *tab);
COMMON(word)		intern_indirect(indirect_table *tab, word val,
					int create ARG_LD);
COMMON(word)		extern_indirect(indirect_table *tab,
					word val, Word *gp ARG_LD);
COMMON(size_t)		gsize_indirect(indirect_table *tab, word val);

#endif /*_PL_INDIRECT_H*/
