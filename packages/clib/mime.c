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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <rfc2045.h>
#include "error.h"
#include <malloc.h>
#include <errno.h>

#define max(x, y) ((x)>(y) ? (x) : (y))


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines an interface to   the rfc2045 (MIME) parsing library
by Double Precision, Inc, part of the maildrop system.

Parsing MIME messages is accomplished  using   a  single predicate. This
predicate parses the input  and  returns   a  complex  term  holding the
various MIME message  parts.  The  mime   message  is  encoded  into the
following structure: 

	mime(Attributes, Data, SubMimeList)

Where Data is the (decoded) field data   returned as an atom, Attributes
is a property-list and SubMimeList is a  list of mime/3 terms reflecting
the sub-parts. Attributes contains the following members:

	# id(Atom)
	# description(Atom)
	# language(Atom)
	# md5(Atom)
	# type(Atom)
	# character_set(Atom)
	# transfer_encoding(Atom)
	# disposition(Atom)
	# filename(Atom)
	# name(Atom)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static atom_t	 ATOM_;
static atom_t	 ATOM_stream;
static functor_t FUNCTOR_type1;
static functor_t FUNCTOR_transfer_encoding1;
static functor_t FUNCTOR_character_set1;
static functor_t FUNCTOR_mime3;
static functor_t FUNCTOR_id1;
static functor_t FUNCTOR_description1;
static functor_t FUNCTOR_language1;
static functor_t FUNCTOR_md51;
static functor_t FUNCTOR_disposition1;
static functor_t FUNCTOR_name1;
static functor_t FUNCTOR_filename1;

struct dbuf
{ char *buf;
  int size;
  int allocated;
};

static int
add_data(const char *ndata, size_t len, void *closure)
{ struct dbuf *dbuf = closure;

  if ( dbuf->size + (int)len > dbuf->allocated )
  { dbuf->allocated = max(dbuf->allocated, max(1024, dbuf->size + (int)len));
    if ( dbuf->buf )
      dbuf->buf = realloc(dbuf->buf, dbuf->allocated);
    else
      dbuf->buf = malloc(dbuf->allocated);

    if ( !dbuf->buf )
    { pl_error("mime_parse", 3, NULL, ERR_ERRNO, errno);
      return -1;
    }
  }

  memcpy(dbuf->buf+dbuf->size, ndata, len);
  dbuf->size += len;

  return 0;
}



static int
mime_unify_data(term_t data, struct rfc2045 *rfc, const char *buffer)
{ off_t start_pos, end_pos, start_body, nlines, nbodylines;
  struct dbuf dbuf;
  int rval;

  dbuf.buf       = NULL;
  dbuf.size      = 0;
  dbuf.allocated = 0;

  rfc2045_mimepos(rfc,
		  &start_pos, &end_pos, &start_body, &nlines, &nbodylines);
  rfc2045_cdecode_start(rfc, add_data, &dbuf);
  if ( rfc2045_cdecode(rfc, buffer+start_body, end_pos-start_body) == 0 &&
       rfc2045_cdecode_end(rfc) == 0 )
  { rval = PL_unify_atom_nchars(data, dbuf.size, dbuf.buf);
  } else
    rval = FALSE;

  if ( dbuf.buf )
    free(dbuf.buf);

  return rval;
}


/* add_attribute() adds a name(value) term to the list if value is provided
   (i.e. not NULL and non "")
*/

static int
add_attribute(term_t list, const char *value, functor_t functor)
{ if ( value && value[0] )
  { term_t h = PL_new_term_ref();
    int rval;

    rval = PL_unify_list(list, h, list) &&
	   PL_unify_term(h, PL_FUNCTOR, functor, PL_CHARS, value);

    PL_reset_term_refs(h);
    return rval;
  }

  return TRUE;
}


static int
mime_unify(term_t result, struct rfc2045 *rfc, const char *buffer)
{ term_t data = PL_new_term_ref();
  term_t subs = PL_new_term_ref();
  term_t atts = PL_new_term_ref();
  
  if ( !PL_unify_term(result,
		      PL_FUNCTOR, FUNCTOR_mime3,
		        PL_TERM, atts,
		        PL_TERM, data,
		        PL_TERM, subs) )
    return FALSE;

  if ( rfc->isdummy )
  { PL_unify_nil(data);			/* dubious */
    if ( !PL_unify_nil(atts) )
      return FALSE;
  } else
  { term_t at = PL_copy_term_ref(atts);
    const char *type, *enc, *cset;
    const char *disp, *name, *fnam;

    const char *id   = rfc2045_content_id(rfc);
    const char *desc = rfc2045_content_description(rfc);
    const char *lang = rfc2045_content_language(rfc);
    const char *md5  = rfc2045_content_md5(rfc);
    
    rfc2045_mimeinfo(rfc, &type, &enc, &cset);
    rfc2045_dispositioninfo(rfc, &disp, &name, &fnam);

    if ( !add_attribute(at, type, FUNCTOR_type1) )              return FALSE;
    if ( !add_attribute(at, enc,  FUNCTOR_transfer_encoding1) ) return FALSE;
    if ( !add_attribute(at, cset, FUNCTOR_character_set1) )     return FALSE;
    if ( !add_attribute(at, id,   FUNCTOR_id1) )                return FALSE;
    if ( !add_attribute(at, desc, FUNCTOR_description1) )       return FALSE;
    if ( !add_attribute(at, lang, FUNCTOR_language1) )          return FALSE;
    if ( !add_attribute(at, disp, FUNCTOR_disposition1) )       return FALSE;
    if ( !add_attribute(at, name, FUNCTOR_name1) )              return FALSE;
    if ( !add_attribute(at, fnam, FUNCTOR_filename1) )          return FALSE;
    if ( !add_attribute(at, md5,  FUNCTOR_md51) )               return FALSE;

    if ( !PL_unify_nil(at) )
      return FALSE;
  }

  if ( rfc->firstpart )
  { term_t st = PL_copy_term_ref(subs);
    term_t s  = PL_new_term_ref();
    struct rfc2045 *sub;

    if ( !PL_unify_atom(data, ATOM_) )
      return FALSE;

    for(sub=rfc->firstpart; sub; sub = sub->next)
    { if ( sub->isdummy )
	continue;

      if ( !PL_unify_list(st, s, st) ||
	   !mime_unify(s, sub, buffer) )
	return FALSE;
    }
    return PL_unify_nil(st);
  } else
  { if ( !PL_unify_nil(subs) ||
	 !mime_unify_data(data, rfc, buffer) )
      return FALSE;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_character_data()
    Get a buffer of data from a specification.  Currently the following
    specs are acceptable:

	stream(Stream)		All data from this stream
	stream(Stream, N)	At most N characters from stream
	Atom, String, CodeList	Data from native Prolog character data
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_character_data(term_t from, char **data, size_t *len, int *malloced)
{ atom_t name;
  int arity;
  char *buf;
  int size;

  if ( PL_get_name_arity(from, &name, &arity) && arity > 0 )
  { if ( name == ATOM_stream )
    { IOSTREAM *stream;
      term_t arg = PL_new_term_ref();

      PL_get_arg(1, from, arg);
      if ( !PL_get_stream_handle(arg, &stream) )
	return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, from, "stream");

      if ( arity == 1 )			/* stream(Stream) */
      { int c;
	size_t done, allocated = 1024;

	if ( !(buf = malloc(allocated)) )
	  return pl_error(NULL, 0, NULL, ERR_ERRNO, errno);
    
	for( done=0; (c=Sgetc(stream)) != EOF; )
	{ if ( done >= allocated )
	  { allocated *= 2;

	    if ( !(buf = realloc(buf, allocated)) )
	      return pl_error(NULL, 0, NULL, ERR_ERRNO, errno);
	  }

	  buf[done++] = c;
	}

	*len = done;
	*data = buf;
	*malloced = TRUE;

        return TRUE;
      }	else if ( arity == 2 )		/* stream(Stream, Length) */
      { long size;
	long done;
	int c;

	PL_get_arg(2, from, arg);
	if ( !PL_get_long(arg, &size) || size < 0 )
	  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, arg, "natural");
	
	if ( !(buf = malloc(size)) )
	  return pl_error(NULL, 0, NULL, ERR_ERRNO, errno);
    
	for( done=0; (c=Sgetc(stream)) != EOF && done < size; )
	  buf[done++] = c;

	*len = done;
	*data = buf;
	*malloced = TRUE;

        return TRUE;
      }
    }
  } else if ( PL_get_nchars(from, &size, data, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { *len = size;
    *malloced = FALSE;

    return TRUE;
  }

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, from, "data");
}



foreign_t
mime_parse(term_t handle, term_t result)
{ char *buf;
  size_t len;
  int malloced;
  struct rfc2045 *rfc;
  int rval;

  if ( !get_character_data(handle, &buf, &len, &malloced) )
    return FALSE;

  rfc = rfc2045_alloc();
  rfc2045_parse(rfc, buf, len);
  rval = mime_unify(result, rfc, buf);

  if ( malloced )
    free(buf);
  rfc2045_free(rfc);

  return rval;
}

		 /*******************************
		 *	       ERRORS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Not typically elegant, but the documentation  whishes us to call exit(),
which is even worse.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
rfc2045_error(const char *errmsg)
{ term_t e = PL_new_term_ref();

  PL_unify_term(e,
		PL_FUNCTOR_CHARS, "error", 2,
		  PL_FUNCTOR_CHARS, "mime", 1,
		    PL_CHARS, errmsg,
		  PL_VARIABLE);

  PL_throw(e);
}

		 /*******************************
		 *	      INSTALL		*
		 *******************************/

#define mkfunctor(n, a) PL_new_functor(PL_new_atom(n), a)


install_t
install_mime()
{ ATOM_			     = PL_new_atom("");
  ATOM_stream		     = PL_new_atom("stream");

  FUNCTOR_type1		     = mkfunctor("type", 1);
  FUNCTOR_transfer_encoding1 = mkfunctor("transfer_encoding", 1);
  FUNCTOR_character_set1     = mkfunctor("character_set", 1);
  FUNCTOR_mime3	             = mkfunctor("mime", 3);
  FUNCTOR_id1                = mkfunctor("id", 1);
  FUNCTOR_description1       = mkfunctor("description", 1);
  FUNCTOR_language1          = mkfunctor("language", 1);
  FUNCTOR_md51               = mkfunctor("md5", 1);
  FUNCTOR_disposition1       = mkfunctor("disposition", 1);
  FUNCTOR_name1		     = mkfunctor("name", 1);
  FUNCTOR_filename1	     = mkfunctor("filename", 1);

  PL_register_foreign("mime_parse", 2, mime_parse, 0);
}
