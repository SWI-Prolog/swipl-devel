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

#define RDF_VERSION 20100		/* 2.1.0 */

#define URL_subPropertyOf \
	"http://www.w3.org/2000/01/rdf-schema#subPropertyOf"


		 /*******************************
		 *               C		*
		 *******************************/

#define OBJ_UNTYPED	0x0		/* partial: don't know */
#define	OBJ_RESOURCE	0x1
#define OBJ_STRING	0x2
#define OBJ_INTEGER	0x3
#define OBJ_DOUBLE	0x4
#define OBJ_TERM	0x5

#define Q_NONE		0x0
#define Q_LANG		0x1
#define Q_TYPE		0x2

#define BY_NONE	0x00			/* 0 */
#define BY_S	0x01			/* 1 */
#define BY_P	0x02			/* 2 */
#define BY_O	0x04			/* 4 */
#define BY_SP	(BY_S|BY_P)		/* 3 */
#define BY_SO	(BY_S|BY_O)		/* 5 */
#define BY_OP	(BY_P|BY_O)		/* 6 */
#define BY_SPO	(BY_S|BY_P|BY_O)	/* 7 */

#define INDEX_TABLES 		        7
#define INITIAL_TABLE_SIZE   		8*1024
#define INITIAL_PREDICATE_TABLE_SIZE	1024
#define INITIAL_SOURCE_TABLE_SIZE	64

#define NO_LINE	((unsigned long)-1L)

#define MAX_LIKE_CHOICES	100	/* max *'s in like pattern */

#define STR_MATCH_CASE		0x0	/* Default: perfect match */
#define	STR_MATCH_EXACT		0x1	/* case-insensitive */
#define	STR_MATCH_SUBSTRING	0x2	/* substring */
#define	STR_MATCH_WORD		0x3	/* whole word */
#define	STR_MATCH_PREFIX	0x4	/* prefix */
#define STR_MATCH_LIKE		0x5	/* SeRQL *like* match */

typedef struct cell
{ void *	value;			/* represented resource */
  struct cell  *next;			/* next in chain */
} cell;


typedef struct list
{ cell *head;				/* first in list */
  cell *tail;				/* tail of list */
} list;


#define DISTINCT_DIRECT 0		/* for ->distinct_subjects, etc */
#define DISTINCT_SUB    1

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
  long		    triple_count;	/* # triples on this predicate */

  long		    distinct_updated[2];/* Is count still valid? */
  long		    distinct_count[2];  /* Triple count at last update */
  long		    distinct_subjects[2];/* # distinct subject values */
  long		    distinct_objects[2];/* # distinct object values */
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
  union
  { atom_t	resource;
    atom_t	string;
    long	integer;
    double	real;
    struct
    { record_t  record;
      int       len;
    } term;				/* external record */
  } object;
  atom_t	type_or_lang;		/* Type or language for literals */
  atom_t	source;			/* where it comes from */
  struct triple*next[INDEX_TABLES];	/* hash-table next links */
  unsigned	objtype : 3;
  unsigned	indexed : 3;		/* Partials: BY_* */
  unsigned	qualifier : 2;		/* Lang/Type qualifier */
  unsigned	erased  : 1;		/* If TRUE, triple is erased */
  unsigned	first   : 1;		/* I'm the first on subject */
  unsigned	match   : 3;		/* How to match literals */
  unsigned	inversed : 1;		/* Partials: using inverse match */
  unsigned	is_duplicate : 1;	/* I'm a duplicate */
  unsigned	duplicates : 18;	/* Duplicate count */
  unsigned long line;			/* source-line number */
} triple;


typedef enum
{ TR_MARK,				/* mark start for nesting */
  TR_ASSERT,				/* rdf_assert */
  TR_RETRACT,				/* rdf_retractall */
  TR_UPDATE,				/* rdf_update */
  TR_UPDATE_SRC,			/* rdf_update */
  TR_UPDATE_MD5,			/* update md5 src */
  TR_RESET				/* rdf_reset_db */
} tr_type;


typedef struct transaction_record
{ struct transaction_record    *previous;
  struct transaction_record    *next;
  tr_type			type;
  triple		       *triple;		/* new/deleted triple */
  union
  { triple		       *triple; 	/* used for update */
    struct
    { atom_t			atom;
      unsigned long		line;
    } src;
    struct
    { source		       *source;
      md5_byte_t	       *digest;
    } md5;
  } update;
} transaction_record;


#if defined(_REENTRANT) && defined(WIN32)
enum
{ SIGNAL     = 0,
  MAX_EVENTS = 1
} win32_event_t;

typedef struct
{ HANDLE events[MAX_EVENTS];		/* events to be signalled */
  int    waiters;			/* # waiters */
} win32_cond_t;
#endif

typedef struct rdf_db
{ triple       *by_none, *by_none_tail;
  triple      **table[INDEX_TABLES];
  triple      **tail[INDEX_TABLES];
  int	       *counts[INDEX_TABLES];
  int		table_size[INDEX_TABLES];
  long		created;		/* #triples created */
  long		erased;			/* #triples erased */
  long		freed;			/* #triples actually erased */
  long		subjects;		/* subjects (unique first) */
  long		indexed[8];		/* Count calls */
  predicate   **pred_table;		/* Hash-table of predicates */
  int		pred_table_size;	/* #entries in the table */
  int		pred_count;		/* #predicates */
  int		active_queries;		/* Calls with choicepoints */
  int		need_update;		/* We need to update */
  long		agenda_created;		/* #visited nodes in agenda */
  long		duplicates;		/* #duplicate triples */
  long		generation;		/* generation-id of the database */
  source      **source_table;		/* Hash table of sources */
  int      	source_table_size;	/* Entries in table */
  source	*last_source;		/* last accessed source */
  transaction_record *tr_first;		/* first transaction record */
  transaction_record *tr_last;		/* last transaction record */
  int		tr_nesting;		/* nesting depth of transactions */
#ifdef _REENTRANT
#ifdef WIN32
  CRITICAL_SECTION	mutex;
  CRITICAL_SECTION	hash_mutex;
  win32_cond_t		rdcondvar;
  win32_cond_t		wrcondvar;
  win32_cond_t		upcondvar;
#else
  pthread_mutex_t	mutex;
  pthread_mutex_t	hash_mutex;
  pthread_cond_t	rdcondvar;
  pthread_cond_t	wrcondvar;
  pthread_cond_t	upcondvar;
#endif
  int			waiting_readers;
  int			waiting_writers;
  int			waiting_upgrade;
  int		       *read_by_thread;
#endif
  int			writer;
  int			allow_readers;
  int			readers;
  int			lock_level;	/* recursive locks */
} rdf_db;

#endif /*RDFDB_H_INCLUDED*/
