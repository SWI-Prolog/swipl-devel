/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1998-2011, University of Amsterdam
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

#include "rc/rc.h"
#include "pl-incl.h"
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

static int
Scontrol_rc(void *handle, int action, void *arg)
{ RcMember m = ((RcObject)handle)->member;

  switch(action)
  { case SIO_GETSIZE:
    { int64_t *rval = arg;

      *rval = m->size;
      return 0;
    }
    case SIO_FLUSHOUTPUT:
    case SIO_SETENCODING:
      return 0;
    default:
      return -1;
  }
}


static IOFUNCTIONS rc_stream_functions =
{ (Sread_function)  rc_read,
  (Swrite_function) rc_write,
  (Sseek_function)  rc_seek,
  (Sclose_function) rc_close,
		    Scontrol_rc
};


		 /*******************************
		 *	STREAM CONNECTION	*
		 *******************************/

IOSTREAM *
SopenRC(void *rca, const char *name, const char *rcclass, int flags)
{ RcObject o = rc_open(rca, name, rcclass, flags);

  if ( o )
  { int sflags = ((flags & RC_WRONLY) ? SIO_OUTPUT : SIO_INPUT);

    return Snew(o, sflags, &rc_stream_functions);
  }

  return NULL;
}


		 /*******************************
		 *	 PROLOG PREDICATES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Connection between the resource library and SWI-Prolog, C-part.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_rc(term_t handle, RcArchive *a)
{ GET_LD
  void *p;

  if ( PL_get_pointer(handle, &p) )
  { *a = p;

    return TRUE;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE,
		  ATOM_resource_handle, handle);
}


foreign_t
pl_rc_handle(term_t h)
{ GET_LD

  if ( GD->resourceDB )
    return PL_unify_pointer(h, GD->resourceDB);

  return FALSE;
}


foreign_t
pl_rc_open(term_t rc_h,
	   term_t name, term_t class, term_t rw,
	   term_t handle)
{ GET_LD
  char *n, *c;
  RcArchive rc = NULL;
  atom_t how;
  int flags = 0, sflags = 0;		/* compiler isn't smart enough */

  if ( !get_rc(rc_h, &rc) )
    return FALSE;

  if ( PL_get_atom_ex(rw, &how) )
  { if ( how == ATOM_read )
    { flags = RC_RDONLY;
      sflags = SIO_INPUT;
    } else if ( how == ATOM_write )
    { flags = RC_WRONLY;
      sflags = SIO_OUTPUT;
    } else
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_io_mode, how);
  }

  if ( PL_get_chars(name, &n, CVT_ALL|CVT_EXCEPTION) )
  { RcObject o;

    if ( !PL_get_chars(class, &c, CVT_ALL) )
      c = NULL;

    if ( (o = rc_open(rc, n, c, flags)) )
    { IOSTREAM *stream;

      if ( !c )
      { rc_stat_buf stat;

	rc_stat(o, &stat);

	if ( !PL_unify_atom_chars(class, stat.rc_class) )
	{ rc_close(o);

	  return FALSE;
	}
      }

      if ( (stream = Snew(o, sflags, &rc_stream_functions)) )
      { if ( PL_unify_stream(handle, stream) )
	  return TRUE;

	Sclose(stream);
	return FALSE;
      }
    } else
      return FALSE;
  }

  fail;
}


foreign_t
pl_rc_open_archive(term_t file, term_t handle)
{ char *name;

  if ( PL_get_file_name(file, &name, 0) )
  { RcArchive a = rc_open_archive(name, RC_RDWR|RC_CREATE);

    if ( a )
    { GET_LD
      return PL_unify_pointer(handle, a);
    }
  }

  return FALSE;
}


foreign_t
pl_rc_close_archive(term_t rc_h)
{ RcArchive rc = NULL;

  if ( !get_rc(rc_h, &rc) )
    return FALSE;

  if ( rc->modified )
    rc_save_archive(rc, NULL);

  return rc_close_archive(rc);
}


foreign_t
pl_rc_save_archive(term_t rc_h, term_t to)
{ GET_LD
  RcArchive rc = NULL;
  char *file;

  if ( !get_rc(rc_h, &rc) )
    return FALSE;
  if ( !PL_get_file_name(to, &file, 0) &&
       !PL_is_variable(to) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_file_name, to);

  if ( rc_save_archive(rc, file) )
  { if ( PL_is_variable(to) &&
	 !PL_unify_atom_chars(to, rc->path) )
      return FALSE;			/* resource error */

    return TRUE;
  }

  return PL_error("rc_save_archive", 2,
		  rc_strerror(rc_errno), ERR_FILE_OPERATION,
		  ATOM_write, ATOM_file, to);
}


foreign_t
pl_rc_append_file(term_t rc_h,
		  term_t name, term_t class, term_t encoding,
		  term_t file)
{ RcArchive rc = NULL;
  char *n, *c = "data", *enc = "none", *f;

  if ( !get_rc(rc_h, &rc) )
    return FALSE;
  if ( !PL_get_chars(name, &n, CVT_ALL|CVT_EXCEPTION) ||
       !PL_get_chars(file, &f, CVT_ALL|CVT_EXCEPTION) )
    fail;

  if ( !PL_get_chars(class, &c, CVT_ALL|CVT_EXCEPTION) &&
       !PL_unify_atom_chars(class, c) )
    fail;
  if ( !PL_get_chars(encoding, &enc, CVT_ALL|CVT_EXCEPTION) &&
       !PL_unify_atom_chars(encoding, enc) )
    fail;

  if ( !rc_append_file(rc, n, c, enc, f) )
    return FALSE;

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$rc_members(+RCHandle, -ListOfMembers)
	Yields a list of rc(Name, Class), representing the members of the
	resource archive.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

foreign_t
pl_rc_members(term_t rc_h, term_t members)
{ GET_LD
  RcArchive rc = NULL;
  RcMember m;
  functor_t f;
  term_t tail = PL_copy_term_ref(members);
  term_t head = PL_new_term_ref();

  if ( !get_rc(rc_h, &rc) )
    return FALSE;

  f = PL_new_functor(PL_new_atom("rc"), 2);
  for(m = rc->members; m; m = m->next)
  { if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify_term(head,
			PL_FUNCTOR, f,
			  PL_CHARS, m->name,
			  PL_CHARS, m->rc_class) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}
