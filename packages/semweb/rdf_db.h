/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

#ifndef RDFDB_H_INCLUDED
#define RDFDB_H_INCLUDED
#ifdef WITH_MD5
#include "md5.h"
#endif

#define URL_subPropertyOf \
	"http://www.w3.org/2000/01/rdf-schema#subPropertyOf"

#define OBJ_UNTYPED	0x0		/* partial: don't know */
#define	OBJ_RESOURCE	0x1
#define OBJ_LITERAL	0x2

#define BY_NONE	0x00			/* 0 */
#define BY_S	0x01			/* 1 */
#define BY_P	0x02			/* 2 */
#define BY_O	0x04			/* 4 */
#define BY_SP	(BY_S|BY_P)		/* 3 */
#define BY_SO	(BY_S|BY_O)		/* 5 */
#define BY_OP	(BY_P|BY_O)		/* 6 */
#define BY_SPO	(BY_S|BY_P|BY_O)	/* 7 */

#define INDEX_TABLES 		        7
#define INITIAL_TABLE_SIZE   		1024*1024
#define INITIAL_PREDICATE_TABLE_SIZE	1024
#define INITIAL_SOURCE_TABLE_SIZE	64

#define NO_LINE	((unsigned long)-1L)

#define MATCH_CASE	0x0		/* Default: perfect match */
#define	MATCH_EXACT	0x1		/* case-insensitive */
#define	MATCH_SUBSTRING	0x2		/* substring */
#define	MATCH_WORD	0x3		/* whole word */
#define	MATCH_PREFIX	0x4		/* prefix */

typedef struct cell
{ void *	value;			/* represented resource */
  struct cell  *next;			/* next in chain */
} cell;


typedef struct list
{ cell *head;				/* first in list */
  cell *tail;				/* tail of list */
} list;


typedef struct predicate
{ atom_t	    name;		/* name of the predicate */
  list	            subPropertyOf;	/* the one I'm subPropertyOf */
  list		    siblings;		/* my subProperties */
  struct predicate *root;		/* Root of property tree */
  struct predicate *next;		/* next in hash-table */
  struct predicate *oldroot;		/* from previous run */
  int		    visited;		/* loop detection */
  struct predicate *inverse_of;		/* my inverse predicate */
  unsigned 	    transitive : 1;	/* P(a,b)&P(b,c) --> P(a,c) */
} predicate;


typedef struct source
{ struct source    *next;		/* next in table */
  atom_t	    name;		/* name of the source */
  int		    triple_count;	/* # triples associated to it */
#ifdef WITH_MD5
  md5_byte_t 	    digest[16];		/* MD5 digest */
  unsigned	    md5 : 1;		/* do/don't record MD5 */
#endif
} source;  


#define t_match next[0]

typedef struct triple
{ atom_t	subject;
  predicate*	predicate;
  atom_t	object;
  atom_t	source;			/* where it comes from */
  struct triple*next[INDEX_TABLES];	/* hash-table next links */
  unsigned	objtype : 2;
  unsigned	indexed : 3;		/* Partials: BY_* */
  unsigned	erased  : 1;		/* If TRUE, triple is erased */
  unsigned	first   : 1;		/* I'm the first on subject */
  unsigned	match   : 3;		/* How to match literals */
  unsigned	inversed : 1;		/* Partials: using inverse match */
  unsigned	is_duplicate : 1;	/* I'm a duplicate */
  unsigned	duplicates : 20;	/* Duplicate count */
  unsigned long line;			/* source-line number */
} triple;


#endif /*RDFDB_H_INCLUDED*/
