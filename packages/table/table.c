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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define O_ORDER				/* include order.c package */

#include "table.h"
#include "error.h"
#include <stdlib.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <stdio.h>
#include <memory.h>
#include <string.h>
#include <assert.h>
#ifdef __unix__
#include <errno.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_FLOATINGPOINT_H
#include <floatingpoint.h>		/* strtod() prototype */
#endif

#include <SWI-Stream.h>

#ifdef O_DEBUG
#define DEBUG(g) g
#else
#define DEBUG(g)
#endif

#ifdef O_ORDER
#include "order.h"
#endif

#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif

#undef offsetof
#define offsetof(t, f) ((long)&(((t*)0)->f))

#define sizeofquery(t) offsetof(query, field[t->nfields])

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog management of external tables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
open_table(+File, +Fields, +Options, -Handle)
	Open a file as a table.  Fields is a list of field-types occuring
	in the file.  A field type is one of:

		atom		read field as atom
		string		read field as SWI-Prolog string
		code_list	read field as list of ASCII codes
		integer		read field as integer
		float		read field as floating point number

	Options is a list of options.  Each option is of the form
	<Name>(<Value>>).  Defined options are:

		record_separator(Code)
				Defines the separator between two
				records.  Default is 10 (newline)

		field_separator(Code)
				Defines the separator between two
				fields in a record.  Default is
				32 (space), implying all blank
				space.

 		sorted(Field)
				Defines the file is sorted in the
				specified field name.  In this
				case lookup will exploit binary
				search if possible.

		unique(Field)
				Defines the field to be unique.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	    PROTOTYPES		*
		 *******************************/

static foreign_t unify_field_info(term_t t, Field field);


		 /*******************************
		 *	      CONSTANTS		*
		 *******************************/

#ifndef EOS
#define EOS (0)
#endif

#define isBlank(c) ((c) == ' ' || (c) == '\t' || (c) == '\r' )

static atom_t ATOM_atom;
static atom_t ATOM_code_list;
static atom_t ATOM_downcase;
static atom_t ATOM_eq;
static atom_t ATOM_escape;
static atom_t ATOM_exact;
static atom_t ATOM_field;
static atom_t ATOM_field_count;
static atom_t ATOM_field_separator;
static atom_t ATOM_file;
static atom_t ATOM_float;
static atom_t ATOM_functor;
static atom_t ATOM_integer;
static atom_t ATOM_key_field;
static atom_t ATOM_map_space_to_underscore;
static atom_t ATOM_prefix;
static atom_t ATOM_record;
static atom_t ATOM_record_separator;
static atom_t ATOM_size;
static atom_t ATOM_skip;
static atom_t ATOM_sorted;
static atom_t ATOM_string;
static atom_t ATOM_substring;
static atom_t ATOM_syntax;
static atom_t ATOM_unique;
static atom_t ATOM_width;
static atom_t ATOM_window;
static atom_t ATOM_arg;

static functor_t FUNCTOR_minus2;

static void
init_constants()
{
  ATOM_arg			= PL_new_atom("arg");
  ATOM_atom			= PL_new_atom("atom");
  ATOM_code_list		= PL_new_atom("code_list");
  ATOM_downcase			= PL_new_atom("downcase");
  ATOM_eq			= PL_new_atom("=");
  ATOM_escape			= PL_new_atom("escape");
  ATOM_exact			= PL_new_atom("exact");
  ATOM_field			= PL_new_atom("field");
  ATOM_field_count		= PL_new_atom("field_count");
  ATOM_field_separator  	= PL_new_atom("field_separator");
  ATOM_file			= PL_new_atom("file");
  ATOM_float			= PL_new_atom("float");
  ATOM_functor			= PL_new_atom("functor");
  ATOM_integer			= PL_new_atom("integer");
  ATOM_key_field		= PL_new_atom("key_field");
  ATOM_map_space_to_underscore	= PL_new_atom("map_space_to_underscore");
  ATOM_prefix			= PL_new_atom("prefix");
  ATOM_record	        	= PL_new_atom("record");
  ATOM_record_separator 	= PL_new_atom("record_separator");
  ATOM_size			= PL_new_atom("size");
  ATOM_skip			= PL_new_atom("skip");
  ATOM_sorted			= PL_new_atom("sorted");
  ATOM_string			= PL_new_atom("string");
  ATOM_substring		= PL_new_atom("substring");
  ATOM_syntax			= PL_new_atom("syntax");
  ATOM_unique			= PL_new_atom("unique");
  ATOM_width			= PL_new_atom("width");
  ATOM_window			= PL_new_atom("window");

  FUNCTOR_minus2		= PL_new_functor(PL_new_atom("-"), 2);
}



		 /*******************************
		 *	 HIGH-LEVEL GET-*	*
		 *******************************/

static int
get_table(term_t handle, Table *table)
{ long l;

  if ( PL_get_long(handle, &l) )
  { Table t = (Table)l;

    if ( t->magic == TABLE_MAGIC )
    { *table = t;
      return TRUE;
    }
  }
    
  return FALSE;
}


static int
get_type(atom_t typename, int *type)
{      if ( typename == ATOM_atom )
    *type = FIELD_ATOM;
  else if ( typename == ATOM_string )
    *type = FIELD_STRING;
  else if ( typename == ATOM_code_list )
    *type = FIELD_CODELIST;
  else if ( typename == ATOM_integer )
    *type = FIELD_INTEGER;
  else if ( typename == ATOM_float )
    *type = FIELD_FLOAT;
  else
    return FALSE;

  return TRUE;
}


static int
get_field_flag(atom_t name, term_t arg, Field field)
{ if ( name == ATOM_sorted )
  { field->flags |= FIELD_SORTED;
    if ( arg )
    { atom_t tab;

      if ( !PL_get_atom(arg, &tab) ||
	   !(field->ord = findOrdTable(tab)) )
	return FALSE;
    }
  } else if ( name == ATOM_unique && arg == 0 )
    field->flags |= FIELD_UNIQUE;
  else if ( name == ATOM_downcase && arg == 0 )
    field->flags |= FIELD_DOWNCASE;
  else if ( name == ATOM_syntax && arg == 0 )
    field->flags |= FIELD_ALLOWBADNUM;
  else if ( name == ATOM_map_space_to_underscore && arg == 0 )
    field->flags |= FIELD_MAPSPACETOUNDERSCORE;
  else if ( name == ATOM_width && arg )
    return PL_get_integer(arg, &field->width);
  else if ( name == ATOM_arg && arg )
    return PL_get_integer(arg, &field->arg);
  else if ( name == ATOM_skip && !arg )
    field->arg = 0;
  else
    return FALSE;

  return TRUE;
}


static void
default_escape_table(Table t)
{ int i;

  if ( !(t->escape_table = malloc(256)) )
  { PL_warning("Not enough memory");
    return;
  }
  
  for(i=0; i<256; i++)
    t->escape_table[i] = i;

  if ( t->escape == '\\' )
  { t->escape_table['b'] = '\b';
    t->escape_table['e'] = 27;
    t->escape_table['n'] = '\n';
    t->escape_table['r'] = '\r';
    t->escape_table['t'] = '\t';
  }
}


static int
get_char(term_t t, int *chr)
{ int i;

  if ( !PL_get_integer(t, &i) || i < 0 || i > 255 )
    return FALSE;
  *chr = i;

  return TRUE;
}


static foreign_t
pl_new_table(term_t file, term_t columns, term_t options, term_t handle)
{ Table table = malloc(sizeof(struct tabletag));	/* table to create */
  field fields[MAXFIELDS];		/* scratch field structures */
  int   nfields=0;			/* # collected fields */
  int   defarg=1;			/* default argument */
  term_t tail = PL_copy_term_ref(columns); /* for enumerating lists */
  term_t head = PL_new_term_ref();	/* scratch list-head */
  term_t arg  = PL_new_term_ref();	/* scratch argument */

  table->record_functor = 0;		/* not filled */
  table->keyfield = -1;
  table->escape = -1;
  table->escape_table = NULL;

  if ( !PL_get_atom(file, &table->file) )
    return error(ERR_INSTANTIATION, "open_table/4", 1, file);

  while(PL_get_list(tail, head, tail))
  { int arity;
    atom_t typename;

    if ( !PL_get_name_arity(head, &fields[nfields].name, &arity) ||
	 arity < 1 || arity > 2 ||
	 !PL_get_arg(1, head, arg) ||
	 !PL_get_atom(arg, &typename) ||
	 !get_type(typename, &fields[nfields].type) )
      return error(ERR_INSTANTIATION, "new_table/4", 2, columns);

    fields[nfields].index = nfields;	/* index number (0..) */
    fields[nfields].width = 0;		/* variable-width field */
    fields[nfields].flags = 0;		/* default */
    fields[nfields].arg   = defarg;	/* default field identifier */
    fields[nfields].ord   = NULL;	/* ordering table */

    if ( arity == 2 )
    { fid_t fid = PL_open_foreign_frame();
      term_t tail2 = PL_new_term_ref();
      term_t head2 = PL_new_term_ref();
      term_t arg2  = PL_new_term_ref();

      PL_get_arg(2, head, tail2);
      while(PL_get_list(tail2, head2, tail2))
      { atom_t a;
	int optarity;

	if ( PL_get_name_arity(head2, &a, &optarity) )
	{ if ( optarity == 1 )
	  { PL_get_arg(1, head2, arg2);

	    if ( !get_field_flag(a, arg2, &fields[nfields]) )
	      goto colerr;
	  } else
	  { if ( !get_field_flag(a, 0, &fields[nfields]) )
	      goto colerr;
	  }
	} else
	{ colerr:
	  return error(ERR_INSTANTIATION, "new_table/4", 2, columns);
	}
      }
      if ( !PL_get_nil(tail2) )
	return error(ERR_INSTANTIATION, "new_table/4", 2, columns);
      
      if ( fields[nfields].flags & FIELD_SORTED )
	table->keyfield = nfields;

      PL_close_foreign_frame(fid);
    }

    if ( fields[nfields].arg > 0 )
      defarg = fields[nfields].arg + 1;

    nfields++;
  }
  if ( !PL_get_nil(tail) )
    return error(ERR_INSTANTIATION, "new_table/4", 2, columns);

  table->record_sep = '\n';
  table->field_sep  = ' ';

  PL_put_term(tail, options);
  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) )
      goto err3;
    if ( name == ATOM_escape && arity == 2 )
    { term_t head2 = PL_new_term_ref();
      term_t tail2 = PL_new_term_ref();

      PL_get_arg(1, head, arg);

      if ( !PL_get_integer(arg, &table->escape) )
	goto err3;

      default_escape_table(table);
      PL_get_arg(2, head, tail2);
      while(PL_get_list(tail2, head2, tail2))
      { atom_t name;
	int arity;
	int f, t;

	if ( !PL_get_name_arity(head2, &name, &arity) ||
	     name != ATOM_eq || arity != 2 )
	  goto err3;
	if ( !PL_get_arg(1, head2, arg) ||
	     !get_char(arg, &f) ||
	     !PL_get_arg(2, head2, arg) ||
	     !get_char(arg, &t) )
	  goto err3;

	table->escape_table[f] = t;
      }
      if ( PL_get_nil(tail2) )
	continue;
      goto err3;
    }

    if ( arity != 1 )
      goto err3;
    PL_get_arg(1, head, arg);
    if ( name == ATOM_record_separator )
    { if ( !PL_get_integer(arg, &table->record_sep) )
	goto err3;
    } else if ( name == ATOM_field_separator )
    { if ( !PL_get_integer(arg, &table->field_sep) )
	goto err3;
    } else if ( name == ATOM_functor )
    { if ( !PL_get_functor(arg, &table->record_functor) )
	goto err3;
    } else
      goto err3;
  }
  if ( !PL_get_nil(tail) )
  { err3:
    return error(ERR_INSTANTIATION, "new_table/4", 3, options);
  }

  table->nfields = nfields;
  table->fields = malloc(sizeof(struct fieldtag)*nfields);
  memcpy(&table->fields[0], fields, sizeof(struct fieldtag)*nfields);
  
  if ( !table->record_functor )
  { int	maxarg=0;
    int i;

    for(i=0; i<nfields; i++)
      maxarg = max(maxarg, fields[i].arg);

    table->record_functor = PL_new_functor(ATOM_record, maxarg);
  }

  table->magic  = TABLE_MAGIC;
  table->size   = -1;
  table->buffer = NULL;
  table->window = NULL;
#ifdef WIN32
  table->hfile  = NULL;
  table->hmap   = NULL;
#endif
#ifdef __unix__
  table->fd     = -1;
#endif

  return PL_unify_integer(handle, (long)table);
}


static int
open_table(Table table)
{ if ( !table->buffer )
  {
#ifdef WIN32
    BY_HANDLE_FILE_INFORMATION info;

    table->hfile = CreateFile(PL_atom_chars(table->file),
			      GENERIC_READ,
			      FILE_SHARE_READ,
			      NULL,
			      OPEN_EXISTING,
			      FILE_ATTRIBUTE_NORMAL,
			      NULL);
    if ( !table->hfile )
      goto errio;

    if ( !GetFileInformationByHandle(table->hfile, &info) )
      goto errio;
    
    table->size = info.nFileSizeLow;

    table->hmap = CreateFileMapping(table->hfile,
				    NULL,
				    PAGE_READONLY,
				    0L,
				    table->size,
				    NULL);
    if ( !table->hmap )
      goto errio;

    table->buffer = MapViewOfFile(table->hmap,
				  FILE_MAP_READ,
				  0L, 0L, /* offset */
				  0L);	/* size (0=all) */
				  
    if ( !table->buffer )
      goto errio;
    
    table->window      = table->buffer;
    table->window_size = table->size;

    return TRUE;

					/* error happened */
  errio:
  { int id = GetLastError();

    if ( table->hmap )
      CloseHandle(table->hmap);
    if ( table->hfile )
      CloseHandle(table->hfile);
    table->buffer = NULL;
    table->window = NULL;
    table->hfile  = NULL;
    table->hmap   = NULL;

    return error(ERR_IO, "open_table/1", id, NULL);
  }
#endif /*WIN32*/

#ifdef __unix__
    struct stat buf;

#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif

    if ( (table->fd = open(PL_atom_chars(table->file), O_RDONLY)) < 0 )
      goto errio;
    if ( fstat(table->fd, &buf) < 0 )
      goto errio;

    table->size = buf.st_size;
    
    if ( (table->buffer = mmap(NULL, table->size,
			       PROT_READ, MAP_SHARED|MAP_NORESERVE,
			       table->fd, 0)) == (char *) -1 )
      goto errio;
    close(table->fd);
    table->fd = -1;

    table->window      = table->buffer;
    table->window_size = table->size;

    return TRUE;

					/* error happened */
  errio:
    if ( table->buffer )
      munmap(table->buffer, table->size);
    if ( table->fd >= 0 )
      close(table->fd);
    table->buffer = NULL;
    table->window = NULL;
    table->fd     = -1;

    return error(ERR_IO, "open_table/1", errno, NULL);
#endif
  }

  return TRUE;
}


static foreign_t
pl_table_window(term_t handle, term_t start, term_t size)
{ Table table;
  long from;
  long wsize;

  if ( !get_table(handle, &table) )
    return error(ERR_INSTANTIATION, "table_window/3", 1, handle);
  if ( !PL_get_long(start, &from) || from < 0 )
    return error(ERR_INSTANTIATION, "table_window/3", 2, start);
  if ( !PL_get_long(size, &wsize) || wsize < 0 )
    return error(ERR_INSTANTIATION, "table_window/3", 3, size);

  if ( from > table->size )
    from = table->size;
  table->window = table->buffer + from;

  if ( table->window + wsize > table->buffer + table->size )
    wsize = (table->buffer + table->size) - table->window;
  table->window_size = wsize;

  PL_succeed;
}


static foreign_t
pl_get_table_attribute(term_t handle, term_t name, term_t value)
{ Table table;
  atom_t n;
  int arity;

  if ( !get_table(handle, &table) )
    return error(ERR_INSTANTIATION, "get_table_attribute/3", 1, handle);

  
  if ( PL_get_name_arity(name, &n, &arity) )
  { if ( n == ATOM_file && arity == 0 )
      return PL_unify_atom(value, table->file);

    if ( n == ATOM_field && arity == 1 )
    { term_t a = PL_new_term_ref();
      int i;
      
      PL_get_arg(1, name, a);
      if ( PL_get_integer(a, &i) )
      { if ( i >= 1 && i <= table->nfields )
	  return unify_field_info(value, &table->fields[i-1]);

	return FALSE;
      }

      goto ierr2;
    }

    if ( n == ATOM_field_separator && arity == 0 )
      return PL_unify_integer(value, table->field_sep);
    if ( n == ATOM_record_separator && arity == 0 )
      return PL_unify_integer(value, table->record_sep);
    if ( n == ATOM_field_count && arity == 0 )
      return PL_unify_integer(value, table->nfields);
    if ( n == ATOM_key_field && arity == 0 )
    { if ( table->keyfield >= 0 )
	return PL_unify_integer(value, table->keyfield+1);
      else
	return FALSE;
    }

    if ( !open_table(table) )
      return FALSE;

    if ( n == ATOM_size && arity == 0 )
      return PL_unify_integer(value, table->size);
    if ( n == ATOM_window && arity == 0 )
      return PL_unify_term(value,
			   PL_FUNCTOR, FUNCTOR_minus2,
			     PL_LONG, table->window - table->buffer,
			     PL_LONG, table->window_size);
  }

ierr2:
    return error(ERR_INSTANTIATION, "get_table_attribute/3", 2, name);
}


static foreign_t
unify_field_info(term_t t, Field field)	/* name(Type, Flags) */
{ term_t flags = PL_new_term_ref();
  term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(flags);
  int options = 0;
  atom_t type;

  switch(field->type)
  { case FIELD_ATOM:
      type = ATOM_atom;
      break;
    case FIELD_STRING:
      type = ATOM_string;
      break;
    case FIELD_CODELIST:
      type = ATOM_code_list;
      break;
    case FIELD_INTEGER:
      type = ATOM_integer;
      break;
    case FIELD_FLOAT:
      type = ATOM_float;
      break;
    default:
      assert(0);
  }

  if ( field->flags & FIELD_UNIQUE )
  { PL_unify_list(tail, head, tail);
    PL_unify_atom(head, ATOM_unique);
    options++;
  }
  if ( field->flags & FIELD_DOWNCASE )
  { PL_unify_list(tail, head, tail);
    PL_unify_atom(head, ATOM_downcase);
    options++;
  }
  if ( field->flags & FIELD_ALLOWBADNUM )
  { PL_unify_list(tail, head, tail);
    PL_unify_atom(head, ATOM_syntax);
    options++;
  }
  if ( field->flags & FIELD_MAPSPACETOUNDERSCORE )
  { PL_unify_list(tail, head, tail);
    PL_unify_atom(head, ATOM_map_space_to_underscore);
    options++;
  }
  if ( field->flags & FIELD_SORTED )
  { PL_unify_list(tail, head, tail);
    if ( field->ord )
      PL_unify_term(head, PL_FUNCTOR, PL_new_functor(ATOM_sorted, 1),
			    PL_ATOM, field->ord->name);
    else
      PL_unify_atom(head, ATOM_sorted);
    options++;
  }
  if ( field->width > 0 )
  { PL_unify_term(head, PL_FUNCTOR, PL_new_functor(ATOM_width, 1),
			    PL_INT, field->width);
    options++;
  }
  if ( field->arg > 0 )
  { PL_unify_term(head, PL_FUNCTOR, PL_new_functor(ATOM_arg, 1),
			    PL_INT, field->arg);
    options++;
  }

  if ( options )
  { PL_unify_nil(tail);
    return PL_unify_term(t, PL_FUNCTOR, PL_new_functor(field->name, 2),
			 	PL_ATOM, type,
			 	PL_TERM, flags);
  } else
  { return PL_unify_term(t, PL_FUNCTOR, PL_new_functor(field->name, 1),
			  	PL_ATOM, type);

  }
}


static foreign_t
pl_open_table(term_t handle)
{ Table table;

  if ( !get_table(handle, &table) )
    return error(ERR_INSTANTIATION, "open_table/1", 1, handle);

  return open_table(table);
}


static foreign_t
pl_close_table(term_t handle)
{ Table table;

  if ( !get_table(handle, &table) )
    return error(ERR_INSTANTIATION, "close_table/1", 1, handle);

  if ( table->buffer )
  {
#ifdef WIN32
    if ( table->buffer )
      UnmapViewOfFile(table->buffer);
    if ( table->hmap )
      CloseHandle(table->hmap);
    if ( table->hfile )
      CloseHandle(table->hfile);
    table->size   = -1;
    table->buffer = NULL;
    table->hfile  = NULL;
    table->hmap   = NULL;
#endif
#ifdef __unix__
    if ( table->buffer )
      munmap(table->buffer, table->size);
    if ( table->fd >= 0 )
      close(table->fd);
    table->size   = -1;
    table->buffer = NULL;
    table->fd     = -1;
#endif
    table->window = NULL;
  }

  PL_succeed;
}


static foreign_t
pl_free_table(term_t handle)
{ Table table;

  if ( !pl_close_table(handle) )
    PL_fail;

  get_table(handle, &table);
  
  table->magic = 0;			/* so it won't be recognised */
  if ( table->escape_table )
    free(table->escape_table);

  free(table);

  PL_succeed;
}


		 /*******************************
		 *         POSITIONING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns the start of the next non-empty  record. Returns the argument if
the pointer is at the start of a record.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static table_offset_t
find_next_record(Table t, table_offset_t start)
{ int er = t->record_sep;
  char *s = t->window + start;
  char *e = t->window + t->window_size;

  if ( start <= 0 )
  { s = t->window;
    goto out;
  }

  if ( s[-1] == er )
    goto out;

  for( ; *s != er && s < e; s++ )
    ;

out:
  while(*s == er && s < e)
    s++;

  return s-t->window;
}


static table_offset_t
find_start_of_record(Table t, table_offset_t start)
{ char *s;
  int er = t->record_sep;

  if ( start < 0 || start > t->window_size )
    return -1;

  if ( start == t->window_size && start > 0 )
    start--;

  s = t->window + start;

  if ( *s == er )
  { char *e = t->window + t->window_size;

    while(*s == er && s<e)
      s++;

    return s-t->window;
  } else
  { while(s>t->window && s[-1] != er)
      s--;

    return s-t->window;
  }    
}


static table_offset_t
previous_record(Table t, table_offset_t start)
{ char *s;
  int er = t->record_sep;
    
  if ( start < 0 || start > t->window_size )
    return -1;

  s = t->window + start - 1;

  while(s>=t->window && *s == er)
    s--;

  return find_start_of_record(t, s-t->window);
}


static foreign_t
pl_previous_record(term_t handle, term_t here, term_t prev)
{ Table t;
  table_offset_t start;

  if ( !get_table(handle, &t) )
    return error(ERR_INSTANTIATION, "previous_record/3", 1, handle);
  if ( !PL_get_long(here, &start) )
    return error(ERR_INSTANTIATION, "previous_record/3", 2, here);

  if ( !open_table(t) )
    PL_fail;

  if ( start <= 0 )
    PL_fail;

  if ( (start = previous_record(t, start)) < 0 )
    PL_fail;

  return PL_unify_integer(prev, start);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
start_of_record(+Table, +From, +To, -StartOfRecord)
	Enumerates all start-positions of records in the interval
	[from, to).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_start_of_record(term_t handle,		/* table */
		   term_t from, term_t to, 	/* range  */
		   term_t recstart,		/* return */
		   control_t control)		/* backtracking control */
{ Table table;
  table_offset_t n, f, t;
  char *end;				/* pointer to end of search */
  char *start;				/* start of search */
  int er;

  switch(PL_foreign_control(control))
  { case PL_FIRST_CALL:
      if ( !PL_get_long(from, &f) )
	return error(ERR_INSTANTIATION, "start_of_record/4", 2, from);
      break;
    case PL_REDO:
      f = PL_foreign_context(control);
      break;
    case PL_CUTTED:
    default:
      PL_succeed;
  }

  if ( !get_table(handle, &table) )
    return error(ERR_INSTANTIATION, "start_of_record/4", 1, handle);
  if ( !PL_get_long(to, &t) )
    return error(ERR_INSTANTIATION, "start_of_record/4", 3, t);

  if ( !open_table(table) )
    return FALSE;
					/* find end of search */
  if ( t < 0 || t > table->window_size )
    end = table->window + table->window_size;
  else
    end = table->window + t;

					/* find start of search */
  if ( f <= 0 )
    start = table->window;
  else 
    start = table->window + f;
    
  if ( start > end )
    PL_fail;
  
  er = table->record_sep;

  if ( start == table->window || start[-1] == er )
    goto found;
  while( *start != er && start < end )
    start++;

  if ( start >= end )
    PL_fail;

found:
  while(*start == er && start < end )
    start++;

  n = start-table->window;
  
  if ( PL_unify_integer(recstart, n) )
    PL_retry(n+1);
  
  PL_fail;
}


		 /*******************************
		 *       READING RECORDS	*
		 *******************************/

static int
field_boundaries(Table t, Field f, table_offset_t start,
		 char **sf, char **zf, table_offset_t *next)
{ char *s = t->window + start;
  char *e = t->window + t->window_size;
  char *z;
  int ef = t->field_sep;
  int er = t->record_sep;

  if ( f->width > 0 )			/* fixed-width field */
  { z = s + f->width;
    if ( z > e )
      return FALSE;
    if ( next )
      *next = z - t->window;
  } else
  { if ( ef == ' ' )
    { for( ; isBlank(*s); s++ )		/* skip leading blanks */
      { if ( s >= e )
	  return FALSE;
      }
					/* record to next blank */
      for(z=s+1; !isBlank(*z) && *z != er; z++)
      { if ( z >= e )
	  return FALSE;
      }
    } else
    { for(z=s; *z != ef && *z != er; z++)
      { if ( z >= e )
	  return FALSE;
      }
      if ( *z == er && er == '\n' && z[-1] == '\r' )
      { if ( next )
	  *next = z - t->window + 1;
	z--;
	goto out;
      }
    }

    if ( next )
      *next = z - t->window + 1;
  }

out:
  *sf = s;
  *zf = z;

  return TRUE;
}


int
digitval(int chr)
{ if ( chr >= '0' && chr <= '9' )
    return chr - '0';

  return -1;
}


static void
tab_memcpy(Table table, int flags, char *to, const char *from, int len)
{ int i = len;
  char *t = to;

  if ( flags & FIELD_DOWNCASE )
  { for( ; i-- > 0; t++, from++)
    { int c = *from & 0xff;

      if ( c == table->escape && i > 0 )
      { int c2 = *++from & 0xff;
	c = table->escape_table[c2];
	i--;
      }
      *t = (isupper(c) ? tolower(c) : c);
    }
    *t = EOS;
  } else if ( table->escape >= 0 )
  { for( ; i-- > 0; from++)
    { int c = *from & 0xff;

      if ( c == table->escape && i > 0 )
      { int c2 = *++from & 0xff;
	c = table->escape_table[c2];
	i--;
      }
      *t++ = c;
    }
    *t = EOS;
  } else
  { strncpy(to, from, len);
    to[len] = EOS;
  }
	
  if ( flags & FIELD_MAPSPACETOUNDERSCORE )
  { char *q;

    for(q=to; *q; q++)
    { if ( *q == ' ' )
	*q = '_';
    }
  }
}


static int
unify_field_text(Table t, int flags, int type,
		 term_t arg, const char *s, int len)
{ char *tmp;
  int rval = FALSE;
#ifndef HAVE_ALLOCA
  char buf[256];
#endif

  if ( (flags&(FIELD_DOWNCASE|FIELD_MAPSPACETOUNDERSCORE)) ||
       t->escape >= 0 )
  {
#ifdef HAVE_ALLOCA
    tmp = alloca(len+1);
#else
    if ( len < 256 )
      tmp = buf;
    else
      tmp = malloc(len+1);
#endif
    tab_memcpy(t, flags, tmp, s, len);
    len = strlen(tmp);
    s = tmp;
  }

  switch(type)
  { case FIELD_ATOM:
      rval = PL_unify_atom_nchars(arg, len, s);
      break;
    case FIELD_STRING:
      rval = PL_unify_string_nchars(arg, len, s);
      break;
    case FIELD_CODELIST:
      rval = PL_unify_list_nchars(arg, len, s);
      break;
  }

#ifndef HAVE_ALLOCA
  if ( buf != tmp )
    free(tmp);
#endif  
  return rval;
}



static int
read_field(Table t, Field f, table_offset_t start, table_offset_t *end, term_t arg)
{ char *s, *z;
  int type;

  if ( !field_boundaries(t, f, start, &s, &z, end) )
    return FALSE;

  if ( !arg )
    return TRUE;			/* just skipping */

  switch((type=f->type))
  { case FIELD_ATOM:
    case_atom:;
    case FIELD_STRING:
    case FIELD_CODELIST:
      return unify_field_text(t, f->flags, arg, type, s, z-s);
    case FIELD_INTEGER:
    { long l = 0;
      char *a = s;
      int digits=0;

      for(; a<z; a++)
      { int code;

	if ( (code = digitval(*a)) >= 0 )
	{ digits++;
	  l = l*10+code;
	}
	else if ( !isBlank(*a) )
	{ if ( f->flags & FIELD_ALLOWBADNUM )
	  { type = FIELD_ATOM;
	    goto case_atom;
	  }
	  return error(ERR_FORMAT, "read_record", s-t->window, f);
	}
      }

      if ( !digits )
      { if ( f->flags & FIELD_ALLOWBADNUM )
	{ type = FIELD_ATOM;
	  goto case_atom;
	}
	return error(ERR_FORMAT, "read_record", s-t->window, f);
      }

      return PL_unify_integer(arg, l);
    }
    case FIELD_FLOAT:
    { char *e;
      double g;

      g = strtod(s, &e);
      while( e < z && isBlank(*e) )
	e++;
      if ( z == e )
	return PL_unify_float(arg, g);
      else if ( f->flags & FIELD_ALLOWBADNUM )
      { type = FIELD_ATOM;
	goto case_atom;
      } else
	return error(ERR_FORMAT, "read_record", s-t->window, f);
    }
  }


  return TRUE;
}


static int
read_record(Table t, table_offset_t start, table_offset_t *end, term_t record)
{ int n;
  Field f;
  term_t arg = PL_new_term_ref();

  if ( !open_table(t) )			/* just make sure */
    return FALSE;

  if ( !PL_unify_functor(record, t->record_functor) )
    return FALSE;
  
  f = t->fields;
  for(n=1; n<=t->nfields; n++, f++)
  { if ( f->arg > 0 )
    { if ( !PL_get_arg(f->arg, record, arg) ||
	   !read_field(t, f, start, &start, arg) )
	return FALSE;
    } else
    { if ( !read_field(t, f, start, &start, 0) )
	return FALSE;
    }
  }

  if ( end )
    *end = find_next_record(t, start);

  return TRUE;
}


foreign_t
pl_read_record(term_t handle, term_t from, term_t to, term_t record)
{ Table table;
  table_offset_t start, end;

  if ( !get_table(handle, &table) )
    return error(ERR_INSTANTIATION, "read_record/4", 1, handle);
  if ( !PL_get_long(from, &start) )
    return error(ERR_INSTANTIATION, "read_record/4", 2, from);

  if ( !open_table(table) )
    return FALSE;

  if ( (start = find_start_of_record(table, start)) < 0 )
    PL_fail;

  if ( read_record(table, start, &end, record) )
    return PL_unify_integer(to, end);

  PL_fail;
}


foreign_t
pl_read_record_data(term_t handle, term_t from, term_t to, term_t record)
{ Table table;
  table_offset_t start, end;
  int len;

  if ( !get_table(handle, &table) )
    return error(ERR_INSTANTIATION, "read_record/4", 1, handle);
  if ( !PL_get_long(from, &start) )
    return error(ERR_INSTANTIATION, "read_record/4", 2, from);

  if ( !open_table(table) )
    return FALSE;

  if ( (start = find_start_of_record(table, start)) < 0 )
    PL_fail;
  end = find_next_record(table, start+1);

  if ( end <= start || !PL_unify_integer(to, end) )
    return FALSE;

  len = end-start-1;

  return PL_unify_string_nchars(record, len, start+table->window);
}


foreign_t
pl_read_fields(term_t handle, term_t from, term_t to, term_t fields)
{ Table table;
  Field f;
  table_offset_t start;
  term_t tail = PL_copy_term_ref(fields);
  term_t head = PL_new_term_ref();
  int i;
#ifdef HAVE_ALLOCA
  term_t *argv;
#else
  term_t argv[MAXFIELDS*sizeof(term_t)];
#endif

  if ( !get_table(handle, &table) )
    return error(ERR_INSTANTIATION, "read_fields/4", 1, handle);
  if ( !PL_get_long(from, &start) )
    return error(ERR_INSTANTIATION, "read_fields/4", 2, from);

  if ( !open_table(table) ||
       (start = find_start_of_record(table, start)) < 0 )
    return FALSE;

#ifdef HAVE_ALLOCA
  argv = alloca(sizeof(term_t) * table->nfields);
#endif
  for(i=0; i<table->nfields; i++)
    argv[i] = 0;

  while(PL_get_list(tail, head, tail))
  { atom_t a;
    int arity;

    if ( !PL_get_name_arity(head, &a, &arity) || arity != 1 )
      return error(ERR_INSTANTIATION, "read_fields/4", 4, fields);
    
    for(i=0; i<table->nfields; i++)
    { if ( table->fields[i].name == a )
      { argv[i] = PL_new_term_ref();
	PL_get_arg(1, head, argv[i]);
	goto cont;
      }
    }

					/* no such record */
    return error(ERR_INSTANTIATION, "read_fields/4", 4, fields);

  cont:
    ;
  }
  if ( !PL_get_nil(tail) )
    return error(ERR_INSTANTIATION, "read_fields/4", 4, fields);

  f = table->fields;
  for(i=0; i<table->nfields; i++, f++)
  { if ( argv[i] )
    { if ( !read_field(table, f, start, &start, argv[i]) )
	return FALSE;
    } else
    { if ( !read_field(table, f, start, &start, 0) )
	return FALSE;
    }
  }

  return PL_unify_integer(to, find_next_record(table, start));
}


		 /*******************************
		 *	      SEARCH		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	-1: query before field
	 0: equal
	 1: query after field
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MR_BIND		0x01		/* bind results */
#define MR_KEY_ONLY	0x02		/* only compare the key (=sorted) */
					/* field */

#define MATCH_NORECORD -2		/* bad record */
#define MATCH_LT       -1		/* < */
#define MATCH_EQ	0		/* == */
#define MATCH_GT	1		/* > */
#define MATCH_NE	2		/* != */

static int
match_field(Table t, Field f, QueryField q, table_offset_t start, table_offset_t *end, int bind)
{ char *a, *z;

  if ( !field_boundaries(t, f, start, &a, &z, end) )
    return MATCH_NORECORD;

  if ( ((q->flags & QUERY_DONTCARE) && !bind) ||
       (q->flags == QUERY_DONTCARE) )
    return MATCH_EQ;

  switch(f->type)
  { case FIELD_ATOM:
    case FIELD_STRING:
    case FIELD_CODELIST:
    { int len = z-a;
#ifdef HAVE_ALLOCA
      char *tmp = alloca(len+1);
#define DEALLOC()
#else
      char buf[1024];
      char *tmp;

      if ( len+1<1024 )
	tmp = buf;
      else
	tmp = malloc(len+1);
#define DEALLOC() do { if ( tmp != buf ) free(tmp); } while(0)
#endif

      tab_memcpy(t, f->flags, tmp, a, len);

      if ( q->flags & QUERY_READ )
      { switch(f->type)
	{ case FIELD_ATOM:
	    PL_unify_atom_chars(q->value.term, tmp);
	    break;
	  case FIELD_STRING:
	    PL_unify_string_chars(q->value.term, tmp);
	    break;
	  case FIELD_CODELIST:
	    PL_unify_list_chars(q->value.term, tmp);
	    break;
	}

	DEALLOC();
	return MATCH_EQ;
      }

      if ( q->flags & QUERY_EXACT )
      { if ( q->ord )
	{ int r = compare_strings(q->value.s, tmp, -1, q->ord);

	  DEBUG(Sdprintf("Using ord %s for '%s' <-> '%s'\n",
			 PL_atom_chars(q->ord->name),
			 q->value.s, tmp));
	  DEALLOC();
	  return r;
	} else
	{ int r = strcmp(q->value.s, tmp);
	  DEALLOC();
	  return r < 0 ? MATCH_LT : r > 0 ? MATCH_GT : MATCH_EQ;
	}
      } else if ( q->flags & QUERY_PREFIX )
      { if ( q->ord )
	{ int r = compare_strings(q->value.s, tmp, q->length, q->ord);
	  DEALLOC();
	  return r;
	} else
	{ int r = strncmp(q->value.s, tmp, q->length);
	  DEALLOC();
	  return r < 0 ? MATCH_LT : r > 0 ? MATCH_GT : MATCH_EQ;
	}
      } else if ( q->flags & QUERY_SUBSTRING ) /* Use Boyle Moore */
      { if ( q->ord )
	{ int ls = q->length;
	  int i;

	  for(i=0; i+ls<=len; i++)
	  { if ( compare_strings(q->value.s, &tmp[i], ls, q->ord) == 0 )
	    { DEALLOC();
	      return MATCH_EQ;
	    }
	  }
          DEALLOC();
	  return MATCH_NE;
	} else
	{ int ls = q->length;
	  int i;

	  for(i=0; i+ls<=len; i++)
	  { if ( strncmp(q->value.s, &tmp[i], ls) == 0 )
	    { DEALLOC();
	      return MATCH_EQ;
	    }
	  }
	  DEALLOC();
	  return MATCH_NE;
	}
      }
    }

    case FIELD_INTEGER:
    { long l = 0;
      int digits=0;

      for(; a<z; a++)
      { int code;

	if ( (code = digitval(*a)) >= 0 )
	{ digits++;
	  l = l*10+code;
	}
	else if ( !isBlank(*a) )
	{ if ( f->flags & FIELD_ALLOWBADNUM )
	    return MATCH_NE;
	  error(ERR_FORMAT, "match_record", a-t->window, f);
	  return MATCH_NORECORD;
	}
      }

      if ( !digits )
      { if ( f->flags & FIELD_ALLOWBADNUM )
	  return MATCH_NE;
	error(ERR_FORMAT, "match_record", a-t->window, f);
      }

      if ( q->flags & QUERY_READ )
      { PL_unify_integer(q->value.term, l);
	return MATCH_EQ;
      }

      return q->value.i == l ? MATCH_EQ : q->value.i < l ? MATCH_LT : MATCH_GT;
    }

    case FIELD_FLOAT:
    { char *e;
      double x;

      x = strtod(a, &e);
      while( e < z && isBlank(*e) )
	e++;
      if ( z != e )
      { if ( f->flags & FIELD_ALLOWBADNUM )
	  return MATCH_NE;
	error(ERR_FORMAT, "match_record", a-t->window, f);
	return MATCH_NORECORD;
      }

      if ( q->flags & QUERY_READ )
      { PL_unify_float(q->value.term, x);
	return MATCH_EQ;
      }

      return q->value.f == x ? MATCH_EQ : q->value.f < x ? MATCH_LT : MATCH_GT;
    }
    default:
      return MATCH_NORECORD;
  }
}


static int
match_record(Query q, table_offset_t start, table_offset_t *end, int flags)
{ int n;
  Field f;
  int rval = MATCH_EQ;
  Table t = q->table;
  QueryField qf = q->field;
  table_offset_t orgstart = start;

  f = t->fields;
  for(n=1; n<=t->nfields; n++, f++, qf++)
  { int match;

    if ( (flags & MR_KEY_ONLY) && !(f->flags & FIELD_SORTED) )
      continue;

    match = match_field(t, f, qf, start, &start, (flags&MR_BIND));
    
    switch(match)
    { case MATCH_NORECORD:
	rval = match;
        goto out;
      case MATCH_EQ:
	continue;
      case MATCH_LT:
      case MATCH_GT:
      case MATCH_NE:
      default:
	if ( rval == MATCH_EQ || (f->flags & FIELD_SORTED) )
	  rval = match;
    }
  }

  out:
  if ( end )
  { if ( start <= orgstart )
      start = orgstart+1;		/* avoid being trapped! */
    *end = find_next_record(t, start);
  }

  return rval;
}


static table_offset_t
execute_binary_search(Query q)
{ Table t       = q->table;
  table_offset_t low     = 0;
  table_offset_t high    = t->window_size;
  table_offset_t here    = find_start_of_record(t, (low+high)/2);

  for(;;)
  { table_offset_t next;

    switch( match_record(q, here, &next, MR_KEY_ONLY) )
    { case MATCH_NORECORD:
      { if ( here >= t->window_size )
	  return FALSE;
	here = next;
	continue;
      }
      case MATCH_LT:
      { high = here;
	here = find_start_of_record(t, (low+high)/2);
	DEBUG(Sdprintf("<, %d %d %d\n", low, here, high));
	goto next;
      }
      case MATCH_GT:
      { low = here;
	here = find_start_of_record(t, (low+high)/2);
	DEBUG(Sdprintf(">, %d %d %d\n", low, here, high));
	goto next;
      }
      case MATCH_EQ:
	DEBUG(Sdprintf("=, %d\n", here));
	if ( t->fields[t->keyfield].flags & FIELD_UNIQUE )
	{ q->technique |= TECH_UNIQUE;
	  return here;
	} else
	{ table_offset_t first = here, prev = here;
	
	  while(prev > 0)		/* find the first */
	  { prev = previous_record(t, prev);
	    if ( match_record(q, prev, &next, MR_KEY_ONLY) == MATCH_EQ )
	    { first = prev;
	    } else
	      break;
	  }
	  return first;
	}
      default:
      case MATCH_NE:
	return -1;
    }

  next:
    if ( low == here )
    { while(here <= high && here < t->window_size)
      { switch( match_record(q, here, &next, MR_KEY_ONLY) )
	{ case MATCH_EQ:
	    return here;
	}
	here = next;
      }
      return -1;
    }
  }
}


static void
free_query(Query q)
{ int t = q->table->nfields;
  int n;

  for(n=0; n<t; n++)
  { if ( q->field[n].flags & QUERY_MALLOCVAL )
      free(q->field[n].value.ptr);
  }

  free(q);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rebind_query_vars() refills the invalidated  value.term   slots  of  the
field structures for a redo in pl_in_table().   No  checking needs to be
done as the initial call has already done that.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
rebind_query_vars(Query q, term_t from)
{ if ( q->nvars > 0 )
  { term_t tail = PL_copy_term_ref(from);
    term_t head = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();
    int varsleft = q->nvars;

    while(PL_get_list(tail, head, tail))
    { PL_get_arg(1, head, arg);

      if ( PL_is_variable(arg) )
      { atom_t name;
	int arity;
	int i;
	
	PL_get_name_arity(head, &name, &arity);
	for(i=0; i<q->table->nfields; i++)
	{ if ( q->table->fields[i].name == name )
	  { QueryField qf = &q->field[i];
	    qf->value.term = PL_copy_term_ref(arg);
	    if ( --varsleft == 0 )
	      return;
	    break;
	  }
	}
      }
    }
  }
}


static Query
make_query(Table t, term_t from)
{ int sizeq = sizeofquery(t);
  Query q = malloc(sizeq);
  term_t tail = PL_copy_term_ref(from);
  term_t head = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();
  int i;

  q->table     = t;
  q->offset    = 0L;
  q->nvars     = 0;
  q->technique = 0;
  for(i=0; i<t->nfields; i++)
    q->field[i].flags = QUERY_DONTCARE;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) || arity < 1 || arity > 2 )
      goto err2;
    PL_get_arg(1, head, arg);

    for(i=0; i<t->nfields; i++)
    { if ( t->fields[i].name == name )
      { QueryField qf = &q->field[i];

	if ( PL_is_variable(arg) )
	{ qf->flags |= QUERY_READ;
	  qf->value.term = PL_copy_term_ref(arg);
	  q->nvars++;
	  goto cont;
	}

	qf->flags &= ~QUERY_DONTCARE;
	assert(qf->flags == 0);

	switch(t->fields[i].type)	/* get value to search for */
	{ case FIELD_ATOM:
	  case FIELD_STRING:
	  case FIELD_CODELIST:
	    if ( !PL_get_atom_chars(arg, &qf->value.s) )
	    { char *tmp;
	      if ( !PL_get_chars(arg, &tmp, CVT_ALL) )
		goto err2;
	      qf->value.s = malloc(strlen(tmp)+1);
	      strcpy(qf->value.s, tmp);
	      qf->flags |= QUERY_MALLOCVAL;
	    }
	    qf->length = strlen(qf->value.s);
	    break;
	  case FIELD_INTEGER:
	    if ( !PL_get_long(arg, &qf->value.i) )
	      goto err2;
	    break;
	  case FIELD_FLOAT:
	    if ( !PL_get_float(arg, &qf->value.f) )
	      goto err2;
	    break;
	}

	qf->ord = t->fields[i].ord;

	if ( arity == 1 )
	{ qf->flags |= QUERY_EXACT;	/* default; */
	} else
	{ atom_t opt;
	  atom_t tab;
	  int a;

	  PL_get_arg(2, head, arg);
	  if ( !PL_get_name_arity(arg, &opt, &a) || a > 1 )
	    goto err2;
	  if ( opt == ATOM_prefix )
	    qf->flags |= QUERY_PREFIX;
	  else if ( opt == ATOM_substring )
	    qf->flags |= QUERY_SUBSTRING;
	  else if ( opt == ATOM_eq )
	    qf->flags |= QUERY_EXACT;
	  else
	    goto err2;

	  if ( PL_get_arg(1, arg, arg) )
	  { if ( !PL_get_atom(arg, &tab) ||
		 !(tab == ATOM_exact || (qf->ord = findOrdTable(tab))) )
	      goto err2;
	  }
	}

	goto cont;
      }
    }
    goto err2;

    cont:;
  }

  if ( !PL_get_nil(tail) )
  { err2:
    free_query(q);
    return NULL;
  }

  if ( t->keyfield >= 0 &&
       (q->field[t->keyfield].flags == QUERY_EXACT ||
	q->field[t->keyfield].flags == QUERY_PREFIX) &&
       q->field[t->keyfield].ord   == t->fields[t->keyfield].ord )
    q->technique |= TECH_BINARY;
  else
    q->technique |= TECH_LINEAR;

  return q;
}


static int
unique_match(Query q)
{ int i;
  QueryField qf = q->field;
  Field f = q->table->fields;

  for(i=0; i<q->table->nfields; i++, qf++, f++)
  { if ( !(qf->flags & QUERY_DONTCARE) && f->flags & FIELD_UNIQUE )
      return TRUE;
  }
  
  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
in_table(+Handle, [ +Field(?Spec), ... ], -RecordId)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_in_table(term_t handle, term_t spec, term_t record, control_t control)
{ Query q;

  switch(PL_foreign_control(control))
  { case PL_FIRST_CALL:
    { Table t;
      
      if ( !get_table(handle, &t) )
	return error(ERR_INSTANTIATION, "in_table/3", 1, handle);
      if ( !open_table(t) )
	PL_fail;
      if ( !(q=make_query(t, spec)) )
	return error(ERR_INSTANTIATION, "in_table/3", 2, handle);
      if ( !PL_is_variable(record) )
	return error(ERR_INSTANTIATION, "in_table/3", 3, record);

      if ( q->technique & TECH_BINARY )
      { if ( (q->offset = execute_binary_search(q)) == -1 )
	{ free_query(q);
	  return FALSE;
	}
      }

      break;
    }
    case PL_REDO:
      q = PL_foreign_context_address(control);
      rebind_query_vars(q, spec);
      break;
    case PL_CUTTED:
    default:
      q = PL_foreign_context_address(control);
      if ( q )
	free_query(q);
      PL_succeed;
  }


  if ( q->technique & TECH_BINARY )
  { table_offset_t next;

    DEBUG(Sdprintf("Binary search, match at offset=%ld\n", q->offset));

    if ( q->technique & TECH_UNIQUE )
    { if ( match_record(q, q->offset, &next, MR_BIND) == MATCH_EQ )
      { PL_unify_integer(record, q->offset);
	if ( unique_match(q) )
	{ free_query(q);
	  return TRUE;
	}
	q->offset = next;

	PL_retry_address(q);
      }
    } else
    { DEBUG(Sdprintf("Non-unique match\n"));

      do
      { DEBUG(Sdprintf("Trying offset %ld\n", q->offset));

	if ( match_record(q, q->offset, &next, 0) == MATCH_EQ )
	{ match_record(q, q->offset, &next, MR_BIND);
	  PL_unify_integer(record, q->offset);
	  if ( unique_match(q) )
	  { free_query(q);
	    return TRUE;
	  }
	  q->offset = next;

	  PL_retry_address(q);
	}

	q->offset = next;
      } while( match_record(q, q->offset, &next, MR_KEY_ONLY) == MATCH_EQ );
    }

    free_query(q);
    return FALSE;
  } 


  while(q->offset < q->table->window_size)
  { table_offset_t next;

    if ( match_record(q, q->offset, &next, 0) == MATCH_EQ )
    { match_record(q, q->offset, &next, MR_BIND);
      PL_unify_integer(record, q->offset);
      if ( unique_match(q) )
      { free_query(q);
	return TRUE;
      }
      q->offset = next;

      PL_retry_address(q);
    }

    q->offset = next;
  }

  free_query(q);
  PL_fail;
}

		 /*******************************
		 *	       VERSION		*
		 *******************************/

static foreign_t
pl_table_version(term_t version, term_t date)
{ if ( PL_unify_atom_chars(version, TABLE_VERSION) &&
       PL_unify_atom_chars(date, __DATE__) )
    return TRUE;

  return FALSE;
}


		 /*******************************
		 *         INSTALLATION		*
		 *******************************/

install_t
install_table()
{ init_constants();

#ifdef O_ORDER
  install_order();
#endif

  PL_register_foreign("table_version",         2, pl_table_version,       0);
  PL_register_foreign("new_table",             4, pl_new_table,           0);
  PL_register_foreign("open_table",            1, pl_open_table,          0);
  PL_register_foreign("close_table",           1, pl_close_table,         0);
  PL_register_foreign("free_table",	       1, pl_free_table,	  0);
  PL_register_foreign("table_window",	       3, pl_table_window,	  0);
  PL_register_foreign("read_table_record",     4, pl_read_record,         0);
  PL_register_foreign("read_table_record_data",4, pl_read_record_data,    0);
  PL_register_foreign("read_table_fields",     4, pl_read_fields,         0);
  PL_register_foreign("get_table_attribute",   3, pl_get_table_attribute, 0);
  PL_register_foreign("table_previous_record", 3, pl_previous_record,	  0);
  PL_register_foreign("table_start_of_record", 4, pl_start_of_record,
		      PL_FA_NONDETERMINISTIC);
  PL_register_foreign("in_table",	       3, pl_in_table,
		      PL_FA_NONDETERMINISTIC);
}


install_t
install()
{ install_table();
}
