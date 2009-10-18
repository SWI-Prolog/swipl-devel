/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include "error.h"

#define streq(s,q) (strcmp((s), (q)) == 0)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Memory-files

make_memory_file(-Handle)
free_memory_file(+Handle)
open_memory_file(+Handle, +Mode, -Stream)
size_memory_file(+Handle, -Size)
memory_file_to_codes(+Handle, -Codes)
memory_file_to_atom(+Handle, -Atom)
atom_to_memory_file(+Atom, -Handle)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static functor_t FUNCTOR_memory_file1;
static atom_t ATOM_encoding;
static atom_t ATOM_unknown;
static atom_t ATOM_octet;
static atom_t ATOM_ascii;
static atom_t ATOM_iso_latin_1;
static atom_t ATOM_text;
static atom_t ATOM_utf8;
static atom_t ATOM_unicode_be;
static atom_t ATOM_unicode_le;
static atom_t ATOM_wchar_t;
static atom_t ATOM_read;
static atom_t ATOM_write;
static atom_t ATOM_free_on_close;

#define MEMFILE_MAGIC	0x5624a6b3L
#define NOSIZE ((size_t)-1)

typedef struct
{ long		magic;			/* MEMFILE_MAGIC */
  IOENC		encoding;		/* encoding of the data */
  int		free_on_close;		/* free if it is closed */
  char	       *data;			/* data of the file */
  size_t	data_size;		/* byte-size of data */
  size_t	size;			/* size in characters */
  IOSTREAM     *stream;			/* Stream hanging onto it */
  atom_t 	atom;			/* Created from atom */
} memfile;


static int
unify_memfile(term_t handle, memfile *f)
{ return PL_unify_term(handle,
		       PL_FUNCTOR, FUNCTOR_memory_file1,
		         PL_POINTER, f);
}


static int
get_memfile(term_t handle, memfile **f)
{ if ( PL_is_functor(handle, FUNCTOR_memory_file1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    PL_get_arg(1, handle, a);
    if ( PL_get_pointer(a, &ptr) )
    { memfile *m = ptr;

      if ( m->magic == MEMFILE_MAGIC )
      { *f = ptr;
        return TRUE;
      }
      return pl_error(NULL, 0, NULL, ERR_EXISTENCE,
		      "memory_file", handle);
    }
  }

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1,
		  handle, "memory_file");
}


static foreign_t
new_memory_file(term_t handle)
{ memfile *m = calloc(1, sizeof(*m));

  if ( !m )
    return pl_error(NULL, 0, NULL, ERR_ERRNO, errno,
		    "create", "memory_file", handle);

  m->magic = MEMFILE_MAGIC;
  m->encoding = ENC_UTF8;
  m->data = 0;
  m->size = 0;

  if ( unify_memfile(handle, m) )
    return TRUE;

  m->magic = 0;
  free(m);
  return FALSE;
}


static int
destroy_memory_file(memfile *m)
{ if ( m->stream )
    Sclose(m->stream);
  if ( m->atom )
    PL_unregister_atom(m->atom);
  else if ( m->data )
    Sfree(m->data);			/* MS-Windows: malloc by other DLL! */
  m->magic = 0;
  free(m);

  return TRUE;
}


static foreign_t
free_memory_file(term_t handle)
{ memfile *m;

  if ( get_memfile(handle, &m) )
    return destroy_memory_file(m);

  return FALSE;
}


static void
closehook(void *closure)
{ memfile *m = closure;

  m->stream = NULL;
  if ( m->free_on_close )
    destroy_memory_file(m);
}


static foreign_t
alreadyOpen(term_t handle, const char *op)
{ return pl_error(NULL, 0, "already open",
		  ERR_PERMISSION, handle, op, "memory_file");
}


static struct encname
{ IOENC  code;
  atom_t *name;
} encoding_names[] =
{ { ENC_UNKNOWN,     &ATOM_unknown },
  { ENC_OCTET,       &ATOM_octet },
  { ENC_ASCII,       &ATOM_ascii },
  { ENC_ISO_LATIN_1, &ATOM_iso_latin_1 },
  { ENC_ANSI,	     &ATOM_text },
  { ENC_UTF8,        &ATOM_utf8 },
  { ENC_UNICODE_BE,  &ATOM_unicode_be },
  { ENC_UNICODE_LE,  &ATOM_unicode_le },
  { ENC_WCHAR,	     &ATOM_wchar_t },
  { ENC_UNKNOWN,     NULL },
};


IOENC
atom_to_encoding(atom_t a)
{ struct encname *en;

  for(en=encoding_names; en->name; en++)
  { if ( *en->name == a )
      return en->code;
  }

  return ENC_UNKNOWN;
}


static int
get_encoding(term_t t, IOENC *enc)
{ atom_t en;

  if ( PL_get_atom(t, &en) )
  { IOENC encoding;

    if ( (encoding = atom_to_encoding(en)) == ENC_UNKNOWN )
      return pl_error(NULL, 0, NULL, ERR_DOMAIN, t, "encoding");

    *enc = encoding;
    return TRUE;
  }

  return pl_error(NULL, 0, NULL, ERR_TYPE, t, "encoding");
}


static foreign_t
open_memory_file4(term_t handle, term_t mode, term_t stream, term_t options)
{ memfile *m;
  char *x;
  atom_t iom;
  IOSTREAM *fd;
  IOENC encoding;
  int free_on_close = FALSE;

  if ( !get_memfile(handle, &m) )
    return FALSE;
  if ( m->stream )
    return alreadyOpen(handle, "open");
  if ( !PL_get_atom(mode, &iom) )
    return pl_error("open_memory_file", 3, NULL, ERR_ARGTYPE, 2,
		    mode, "io_mode");
  encoding = m->encoding;

  if ( options )
  { term_t tail = PL_copy_term_ref(options);
    term_t head = PL_new_term_ref();

    while(PL_get_list(tail, head, tail))
    { int arity;
      atom_t name;

      if ( PL_get_name_arity(head, &name, &arity) && arity == 1 )
      { term_t arg = PL_new_term_ref();

	PL_get_arg(1, head, arg);
	if ( name == ATOM_encoding )
	{ if ( !get_encoding(arg, &encoding) )
	    return FALSE;
	} else if ( name == ATOM_free_on_close )
	{ if ( !PL_get_bool(arg, &free_on_close) )
	    return pl_error("open_memory_file", 4, NULL, ERR_TYPE,
			    arg, "boolean");
	}
      } else
	return pl_error("open_memory_file", 4, NULL, ERR_TYPE, head, "option");
    }
    if ( !PL_get_nil(tail) )
      return pl_error("open_memory_file", 4, NULL, ERR_TYPE, tail, "list");
  }

  if ( iom == ATOM_write )
  { x = "w";
    if ( m->atom )
      return pl_error("open_memory_file", 3, NULL, ERR_PERMISSION,
		      handle, "write", "memory_file");
    if ( m->data )
    { Sfree(m->data);
      m->data = NULL;
    }
    m->data_size = 0;
    m->size = NOSIZE;			/* don't know */
    m->encoding = encoding;
  } else if ( iom == ATOM_read )
  { x = "r";
    m->free_on_close = free_on_close;
  } else
  { return pl_error("open_memory_file", 3, NULL, ERR_DOMAIN,
		    mode, "io_mode");
  }

  if ( !(fd = Sopenmem(&m->data, &m->data_size, x)) )
    return pl_error("open_memory_file", 3, NULL, ERR_ERRNO, errno,
		    "create", "memory_file", handle);

  fd->close_hook = closehook;
  fd->closure = m;
  fd->encoding = encoding;
  m->stream = fd;

  return PL_unify_stream(stream, fd);
}


static foreign_t
open_memory_file(term_t handle, term_t mode, term_t stream)
{ return open_memory_file4(handle, mode, stream, 0);
}



static foreign_t
size_memory_file(term_t handle, term_t size)
{ memfile *m;

  if ( get_memfile(handle, &m) )
  { if ( m->stream && !m->atom )
      return alreadyOpen(handle, "size");
    if ( m->data )
    { if ( m->size == NOSIZE )
      { switch( m->encoding )
	{ case ENC_ISO_LATIN_1:
	  case ENC_OCTET:
	    m->size = m->data_size;
	    break;
	  case ENC_WCHAR:
	    m->size = m->data_size / sizeof(wchar_t);
	    break;
	  case ENC_UTF8:
	    m->size = PL_utf8_strlen(m->data, m->data_size);
	    break;
	  default:
	    assert(0);
	    return FALSE;
	}
      }
      return PL_unify_integer(size, m->size);
    } else
      return PL_unify_integer(size, 0);
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
utf8_position_memory_file(+MF, -Here, -Size)

Given  MF  is  a  UTF-8  encoded  memory   file,  unify  here  with  the
byte-position of the read-pointer and Size with   the  total size of the
memory file in bytes. This is a bit hacky predicate, but the information
is easily available at low cost, while it is very valuable for producing
answers  in  content-length  computation  of    the   HTTP  server.  See
http_wrapper.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
utf8_position(term_t handle, term_t here, term_t size)
{ memfile *m;

  if ( !get_memfile(handle, &m) )
    return FALSE;
  if ( m->encoding != ENC_UTF8 )
    return pl_error(NULL, 0, "no UTF-8 encoding",
		    ERR_PERMISSION, handle, "utf8_position", "memory_file");
  if ( !PL_unify_integer(size, m->data_size) )
    return FALSE;
  if ( m->stream )
  { IOPOS *op = m->stream->position;
    long p;

    m->stream->position = NULL;
    p = Stell(m->stream);
    m->stream->position = op;

    return PL_unify_integer(here, p);
  } else
    return PL_unify_integer(here, 0);
}


static foreign_t
atom_to_memory_file(term_t atom, term_t handle)
{ atom_t a;

  if ( PL_get_atom(atom, &a) )
  { memfile *m = calloc(1, sizeof(*m));

    if ( !m )
      return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "create", "memory_file", handle);

    m->atom = a;
    PL_register_atom(m->atom);
    m->magic = MEMFILE_MAGIC;

    if ( (m->data = (char *)PL_atom_nchars(a, &m->size)) )
    { m->encoding = ENC_ISO_LATIN_1;
      m->data_size = m->size;
    } else if ( (m->data = (char *)PL_atom_wchars(a, &m->size)) )
    { m->encoding = ENC_WCHAR;
      m->data_size = m->size * sizeof(wchar_t);
    } else if ( PL_blob_data(a, &m->size, NULL) )
    { m->data = PL_blob_data(a, &m->data_size, NULL);
      m->encoding = ENC_OCTET;
      m->size = m->data_size;
    }

    if ( unify_memfile(handle, m) )
      return TRUE;
    else
    { PL_unregister_atom(m->atom);
      m->magic = 0;
      free(m);
      return FALSE;
    }
  } else
  { return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1,
		    atom, "atom");
  }
}


static foreign_t
memory_file_to_text(term_t handle, term_t atom, term_t encoding, int flags)
{ memfile *m;

  if ( get_memfile(handle, &m) )
  { IOENC enc;

    if ( encoding )
    { if ( !get_encoding(encoding, &enc) )
	return FALSE;
    } else
      enc = m->encoding;

    if ( m->stream )
      return alreadyOpen(handle, "to_atom");
    if ( m->data )
    { switch(enc)
      { case ENC_ISO_LATIN_1:
        case ENC_OCTET:
	  return PL_unify_chars(atom, flags, m->data_size, m->data);
	case ENC_WCHAR:
	  return PL_unify_wchars(atom, flags, m->data_size/sizeof(wchar_t), (pl_wchar_t*)m->data);
	case ENC_UTF8:
	  return PL_unify_chars(atom, flags|REP_UTF8, m->data_size, m->data);
	default:
	  assert(0);
      }
    } else
      return PL_unify_chars(atom, flags, 0, "");
  }

  return FALSE;
}


static foreign_t
memory_file_to_atom2(term_t handle, term_t atom)
{ return memory_file_to_text(handle, atom, 0, PL_ATOM);
}


static foreign_t
memory_file_to_atom3(term_t handle, term_t atom, term_t encoding)
{ return memory_file_to_text(handle, atom, encoding, PL_ATOM);
}


static foreign_t
memory_file_to_codes2(term_t handle, term_t atom)
{ return memory_file_to_text(handle, atom, 0, PL_CODE_LIST);
}


static foreign_t
memory_file_to_codes3(term_t handle, term_t atom, term_t encoding)
{ return memory_file_to_text(handle, atom, encoding, PL_CODE_LIST);
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install_memfile()
{ if ( PL_query(PL_QUERY_VERSION) <= 50505 )
  { PL_warning("Requires SWI-Prolog version 5.5.6 or later");
    return;
  }

  FUNCTOR_memory_file1 = PL_new_functor(PL_new_atom("$memory_file"), 1);
  MKATOM(encoding);
  MKATOM(unknown);
  MKATOM(octet);
  MKATOM(ascii);
  MKATOM(iso_latin_1);
  MKATOM(text);
  MKATOM(utf8);
  MKATOM(unicode_be);
  MKATOM(unicode_le);
  MKATOM(wchar_t);
  MKATOM(read);
  MKATOM(write);
  MKATOM(free_on_close);

  PL_register_foreign("new_memory_file",      1, new_memory_file,      0);
  PL_register_foreign("free_memory_file",     1, free_memory_file,     0);
  PL_register_foreign("size_memory_file",     2, size_memory_file,     0);
  PL_register_foreign("open_memory_file",     3, open_memory_file,     0);
  PL_register_foreign("open_memory_file",     4, open_memory_file4,    0);
  PL_register_foreign("atom_to_memory_file",  2, atom_to_memory_file,  0);
  PL_register_foreign("memory_file_to_atom",  2, memory_file_to_atom2, 0);
  PL_register_foreign("memory_file_to_codes", 2, memory_file_to_codes2,0);
  PL_register_foreign("memory_file_to_atom",  3, memory_file_to_atom3, 0);
  PL_register_foreign("memory_file_to_codes", 3, memory_file_to_codes3,0);
  PL_register_foreign("utf8_position_memory_file", 3, utf8_position,   0);
}
