/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
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
		    PL_INTEGER, maxval);

      break;
    }
    case ERR_MISC:
    { const char *id = va_arg(args, const char *);
      const char *fmt = va_arg(args, const char *);

      vsprintf(msgbuf, fmt, args);
      msg = msgbuf;
      
      PL_unify_term(formal,
		    PL_FUNCTOR_CHARS, "miscellaneous", 1,
		      PL_CHARS, id);
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

