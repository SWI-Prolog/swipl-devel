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

#include <SWI-Prolog.h>
#include <errno.h>
#include <stdlib.h>
#include "error.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>

int
pl_error(plerrorid id, ...)
{ term_t except = PL_new_term_ref();
  term_t formal = PL_new_term_ref();
  term_t swi	= PL_new_term_ref();
  va_list args;
  char msgbuf[1024];
  char *msg = NULL;

  va_start(args, id);
  switch(id)
  { case ERR_ERRNO:
    { int err = va_arg(args, int);
      
      msg = strerror(err);

      switch(err)
      { case ENOMEM:
	  PL_unify_term(formal,
			PL_FUNCTOR_CHARS, "resource_error", 1,
			  PL_CHARS, "no_memory");
	  break;
	case EACCES:
	{ const char *file = va_arg(args,   const char *);
	  const char *action = va_arg(args, const char *);

	  PL_unify_term(formal,
			PL_FUNCTOR_CHARS, "permission_error", 3,
			  PL_CHARS, action,
			  PL_CHARS, "file",
			  PL_CHARS, file);
	  break;
	}
	case ENOENT:
	{ const char *file = va_arg(args, const char *);

	  PL_unify_term(formal,
			PL_FUNCTOR_CHARS, "existence_error", 2,
			  PL_CHARS, "file",
			  PL_CHARS, file);
	  break;
	}
	default:
	  PL_unify_atom_chars(formal, "system_error");
	  break;
      }
      break;
    }
    case ERR_TYPE:
    { const char *expected = va_arg(args, const char*);
      term_t actual        = va_arg(args, term_t);

      if ( PL_is_variable(actual) &&
	   strcmp(expected, "variable") != 0 )
	PL_unify_atom_chars(formal, "instantiation_error");
      else
	PL_unify_term(formal,
		      PL_FUNCTOR_CHARS, "type_error", 2,
		      PL_CHARS, expected,
		      PL_TERM, actual);
      break;
    }
    case ERR_DOMAIN:
    { const char *expected = va_arg(args, const char*);
      term_t actual        = va_arg(args, term_t);

      if ( PL_is_variable(actual) )
	PL_unify_atom_chars(formal, "instantiation_error");
      else
	PL_unify_term(formal,
		      PL_FUNCTOR_CHARS, "domain_error", 2,
		      PL_CHARS, expected,
		      PL_TERM, actual);
      break;
    }
    case ERR_EXISTENCE:
    { const char *type = va_arg(args, const char *);
      term_t obj  = va_arg(args, term_t);

      PL_unify_term(formal,
		    PL_FUNCTOR_CHARS, "existence_error", 2,
		    PL_CHARS, type,
		    PL_TERM, obj);

      break;
    }
    case ERR_FAIL:
    { term_t goal  = va_arg(args, term_t);

      PL_unify_term(formal,
		    PL_FUNCTOR_CHARS, "goal_failed", 1,
		    PL_TERM, goal);

      break;
    }
    case ERR_LIMIT:
    { const char *limit = va_arg(args, const char *);
      long maxval  = va_arg(args, long);

      PL_unify_term(formal,
		    PL_FUNCTOR_CHARS, "limit_exceeded", 2,
		    PL_CHARS, limit,
		    PL_LONG, maxval);

      break;
    }
    case ERR_PACKAGE_INT:
    { const char *pkg = va_arg(args, const char *);
      int          id = va_arg(args, int);
      const char *fmt = va_arg(args, const char *);

      vsprintf(msgbuf, fmt, args);
      msg = msgbuf;
      
      PL_unify_term(formal,
		    PL_FUNCTOR_CHARS, "package", 2,
		      PL_CHARS, pkg,
		      PL_INT, id);
      break;
    }
    default:
      assert(0);
  }
  va_end(args);

  if ( msg )
  { term_t predterm = PL_new_term_ref();
    term_t msgterm  = PL_new_term_ref();

    if ( msg )
    { PL_put_atom_chars(msgterm, msg);
    }

    PL_unify_term(swi,
		  PL_FUNCTOR_CHARS, "context", 2,
		    PL_TERM, predterm,
		    PL_TERM, msgterm);
  }

  PL_unify_term(except,
		PL_FUNCTOR_CHARS, "error", 2,
		  PL_TERM, formal,
		  PL_TERM, swi);


  return PL_raise_exception(except);
}
