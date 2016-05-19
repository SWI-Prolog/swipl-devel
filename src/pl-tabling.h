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

#ifndef _PL_TABLING_H
#define _PL_TABLING_H
#include "pl-trie.h"

typedef enum
{ CLUSTER_ANSWERS,
  CLUSTER_SUSPENSIONS
} cluster_type;

#define WORKLIST_MAGIC	0x67e9124e


		 /*******************************
		 *     GLOBAL ENTRY POINTS	*
		 *******************************/

typedef struct worklist_set
{ buffer members;
} worklist_set;


		 /*******************************
		 *	   TABLE WORKLIST	*
		 *******************************/

typedef struct cluster
{ cluster_type type;
  struct cluster *next;
  struct cluster *prev;
  buffer members;
} cluster;

typedef struct worklist
{ cluster      *head;			/* answer and dependency clusters */
  cluster      *tail;
  cluster      *riac;			/* rightmost inner answer cluster */
  int		magic;			/* WORKLIST_MAGIC */
  unsigned	executing : 1;		/* $tbl_wkl_work/3 in progress */
  unsigned	in_global_wl : 1;	/* already in global worklist */

  trie	       *table;			/* table I belong to */
} worklist;


COMMON(void) clearThreadTablingData(PL_local_data_t *ld);

#endif /*_PL_TABLING_H*/
