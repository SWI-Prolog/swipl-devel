/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <string.h>
#include <stdlib.h>
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static functor_t FUNCTOR_memory_file1;
#define MEMFILE_MAGIC	0x5624a6b3L

typedef struct
{ long  magic;				/* MEMFILE_MAGIC */
  char *data;				/* data of the file */
  int   size;				/* length of the file */
  IOSTREAM *stream;			/* Stream hanging onto it */
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
    return FALSE;

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
    if ( m->data )
      free(m->data);
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
    x = "w";
  else if ( streq(s, "read") )
    x = "r";
  else
    return pl_error("open_memory_file", 3, NULL, ERR_DOMAIN,
		    mode, "io_mode");

  if ( !(fd = Sopenmem(&m->data, &m->size, x)) )
    return pl_error("open_memory_file", 3, NULL, ERR_ERRNO,
		    "memory_file", "create");

  fd->close_hook = closehook;
  fd->closure = m;
  m->stream = fd;

  return PL_unify_stream(stream, fd);
}


static foreign_t
size_memory_file(term_t handle, term_t size)
{ memfile *m;

  if ( get_memfile(handle, &m) )
  { if ( m->stream )
      return alreadyOpen(handle, "size");
    if ( m->data )
    { return PL_unify_integer(size, m->size);
    }
  }

  return FALSE;
}


static foreign_t
memory_file_to_atom(term_t handle, term_t atom)
{ memfile *m;

  if ( get_memfile(handle, &m) )
  { if ( m->stream )
      return alreadyOpen(handle, "to_atom");
    if ( m->data )
    { return PL_unify_atom_nchars(atom, m->size, m->data);
    }
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
    { return PL_unify_list_ncodes(codes, m->size, m->data);
    }
  }

  return FALSE;
}


install_t
install_memfile()
{ if ( PL_query(PL_QUERY_VERSION) <= 40006 )
  { PL_warning("Requires SWI-Prolog version 4.0.7 or later");
    return;
  }

  FUNCTOR_memory_file1 = PL_new_functor(PL_new_atom("$memory_file"), 1);

  PL_register_foreign("new_memory_file",      1, new_memory_file,      0);
  PL_register_foreign("free_memory_file",     1, free_memory_file,     0);
  PL_register_foreign("size_memory_file",     2, size_memory_file,     0);
  PL_register_foreign("open_memory_file",     3, open_memory_file,     0);
  PL_register_foreign("memory_file_to_atom",  2, memory_file_to_atom,  0);
  PL_register_foreign("memory_file_to_codes", 2, memory_file_to_codes, 0);
}
