/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#ifndef _TABLE_H_INCLUDED
#define _TABLE_H_INCLUDED

#define TABLE_VERSION "1.2.2"

#include <SWI-Prolog.h>
#include "order.h"
#ifdef WIN32
#include <windows.h>
#endif

#define TABLE_MAGIC	32635373	/* just a number */

#define MAXFIELDS	256		/* max fields in a file */

#define FIELD_ATOM	0		/* type of the field */
#define FIELD_STRING	1
#define FIELD_CODELIST	2		/* list of ASCII codes */
#define FIELD_INTEGER	3
#define FIELD_FLOAT	4

#define FIELD_SORTED	0x1		/* Flags: field is sorted */
#define FIELD_UNIQUE	0x2		/* There is only one entry */
#define FIELD_DOWNCASE	0x4		/* Downcase the input from the field */
#define FIELD_MAPSPACETOUNDERSCORE 0x8	/* ' ' --> '_' mapping */
#define FIELD_ALLOWBADNUM	0x10	/* ignore (fail) on bad numerical */

#define CMP_DICT	0x1		/* dictionary order */
#define CMP_IGNORECASE	0x2		/* ignore case */

typedef long	table_offset_t;			/* general offset */

typedef struct fieldtag
{ atom_t	name;			/* name of the field */
  int		index;			/* ordinal number */
  int		type;			/* type of the field */
  int		width;			/* > 0: fixed-width field */
  int		arg;			/* argument in term */
  OrdTable	ord;			/* ordering table (if sorted oddly) */
  int		flags;			/* general flags */
} field, *Field;


typedef struct tabletag
{ int		magic;			/* TABLE_MAGIC */
  atom_t	file;			/* name of the file */
  int 		nfields;		/* # fields in the table */
  Field		fields;			/* field description terms */
  int		keyfield;		/* 0-based index of key (or -1) */
  int		record_sep;		/* record separator */
  int		field_sep;		/* field separator */
  int		escape;			/* escape character (-1: none) */
  char	       *escape_table;		/* escape mapping table */
  functor_t	record_functor;		/* functor for record */
  char	       *window;			/* pointer to the current window */
  long	        window_size;		/* size of the current window */
  char	       *buffer;			/* buffer for the file */
  long		size;			/* size of the `window' */
#ifdef WIN32
  HANDLE	hfile;			/* handle to the file */
  HANDLE	hmap;			/* handle to the map */
#endif
#ifdef __unix__
  int		fd;			/* file descriptor */
#endif
} table, *Table;


#define QUERY_DONTCARE	   0x01		/* don't consider this field */
#define QUERY_EXACT	   0x02		/* Must match exactly */
#define QUERY_PREFIX	   0x04		/* Prefix match */
#define QUERY_SUBSTRING	   0x08		/* Substring match */
#define QUERY_MALLOCVAL    0x10		/* value is malloced ptr */
#define QUERY_READ	   0x20		/* read this field */

#define TECH_LINEAR        0x1		/* simple linear search */
#define TECH_BINARY	   0x2		/* use binary search */
#define TECH_UNIQUE	   0x4		/* key is unique */

typedef struct fieldquerytag
{ union
  { long	i;			/* value as integer */
    char *	s;			/* value as string */
    double	f;			/* value as float */
    void *	ptr;			/* anonymous pointer */
    term_t	term;			/* value as term (read) */
  } value;
  int		length;			/* length of string */
  OrdTable	ord;			/* ordering table */
  int 		flags;
} queryfield, *QueryField;

typedef struct querytag
{ Table		table;			/* table to work on */
  table_offset_t offset;		/* current working offset */
  int		nvars;			/* number of variable (read) fields */
  int		technique;		/* use binary search */
  queryfield	field[1];		/* the fields */
} query, *Query;

#endif /*_TABLE_H_INCLUDED*/
