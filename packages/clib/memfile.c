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
#include <string.h>
#include <stdlib.h>
#include <assert.h>
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
#define MEMFILE_MAGIC	0x5624a6b3L
#define NOSIZE ((unsigned int)-1)

typedef struct
{ long		magic;			/* MEMFILE_MAGIC */
  IOENC		encoding;		/* encoding of the data */
  char	       *data;			/* data of the file */
  unsigned int	data_size;		/* byte-size of data */
  unsigned int	size;			/* size in characters */
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
    return pl_error(NULL, 0, NULL, ERR_ERRNO);

  m->magic = MEMFILE_MAGIC;
  m->data = 0;
  m->size = 0;

  return unify_memfile(handle, m);
}


static foreign_t
free_memory_file(term_t handle)
{ memfile *m;

  if ( get_memfile(handle, &m) )
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

  return FALSE;
}


static void
closehook(void *closure)
{ memfile *m = closure;

  m->stream = NULL;
}


static foreign_t
alreadyOpen(term_t handle, const char *op)
{ return pl_error(NULL, 0, "already open",
		  ERR_PERMISSION, handle, op, "memory_file");
}


static foreign_t
open_memory_file(term_t handle, term_t mode, term_t stream)
{ memfile *m;
  char *x;
  char *s;
  IOSTREAM *fd;

  if ( !get_memfile(handle, &m) )
    return FALSE;
  if ( m->stream )
    return alreadyOpen(handle, "open");
  if ( !PL_get_atom_chars(mode, &s) )
    return pl_error("open_memory_file", 3, NULL, ERR_ARGTYPE, 2,
		    mode, "io_mode");

  if ( streq(s, "write") )
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
    m->encoding = ENC_UTF8;
  } else if ( streq(s, "read") )
    x = "r";
  else
    return pl_error("open_memory_file", 3, NULL, ERR_DOMAIN,
		    mode, "io_mode");

  if ( !(fd = Sopenmem(&m->data, (int *)&m->data_size, x)) )
    return pl_error("open_memory_file", 3, NULL, ERR_ERRNO,
		    "memory_file", "create");

  fd->close_hook = closehook;
  fd->closure = m;
  fd->encoding = m->encoding;
  m->stream = fd;

  return PL_unify_stream(stream, fd);
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


static foreign_t
atom_to_memory_file(term_t atom, term_t handle)
{ atom_t a;

  if ( PL_get_atom(atom, &a) )
  { memfile *m = calloc(1, sizeof(*m));

    if ( !m )
      return pl_error(NULL, 0, NULL, ERR_ERRNO);

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

    return unify_memfile(handle, m);
  } else
  { return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1,
		    atom, "atom");
  }
}


static foreign_t
memory_file_to_atom(term_t handle, term_t atom)
{ memfile *m;

  if ( get_memfile(handle, &m) )
  { if ( m->stream )
      return alreadyOpen(handle, "to_atom");
    if ( m->data )
    { switch(m->encoding)
      { case ENC_ISO_LATIN_1:
        case ENC_OCTET:
	  return PL_unify_atom_nchars(atom, m->data_size, m->data);
	case ENC_WCHAR:
	  return PL_unify_wchars(atom, PL_ATOM, m->data_size/sizeof(wchar_t), (pl_wchar_t*)m->data);
	case ENC_UTF8:
	  return PL_unify_term(atom, PL_NUTF8_CHARS, m->data_size, m->data);
	default:
	  assert(0);
      }
    } else
      return PL_unify_atom_nchars(atom, 0, "");
  }

  return FALSE;
}


static foreign_t
memory_file_to_codes(term_t handle, term_t codes)
{ memfile *m;

  if ( get_memfile(handle, &m) )
  { if ( m->stream )
      return alreadyOpen(handle, "to_codes");
    if ( m->data )
    { switch(m->encoding)
      { case ENC_ISO_LATIN_1:
	case ENC_OCTET:
	  return PL_unify_list_ncodes(codes, m->data_size, m->data);
	case ENC_WCHAR:
	  return PL_unify_wchars(codes, PL_CODE_LIST, m->data_size/sizeof(wchar_t), (pl_wchar_t*)m->data);
	case ENC_UTF8:
	  return PL_unify_term(codes, PL_NUTF8_CODES, m->data_size, m->data);
	default:
	  assert(0);
      }
    } else
      return PL_unify_list_ncodes(codes, 0, "");
  }

  return FALSE;
}


install_t
install_memfile()
{ if ( PL_query(PL_QUERY_VERSION) <= 50505 )
  { PL_warning("Requires SWI-Prolog version 5.5.6 or later");
    return;
  }

  FUNCTOR_memory_file1 = PL_new_functor(PL_new_atom("$memory_file"), 1);

  PL_register_foreign("new_memory_file",      1, new_memory_file,      0);
  PL_register_foreign("free_memory_file",     1, free_memory_file,     0);
  PL_register_foreign("size_memory_file",     2, size_memory_file,     0);
  PL_register_foreign("open_memory_file",     3, open_memory_file,     0);
  PL_register_foreign("atom_to_memory_file",  2, atom_to_memory_file,  0);
  PL_register_foreign("memory_file_to_atom",  2, memory_file_to_atom,  0);
  PL_register_foreign("memory_file_to_codes", 2, memory_file_to_codes, 0);
}
