/*  $Id$

    Part of BOOT
    Designed and implemented by Anja van der Hulst and Jan Wielemaker

    Copyright (C) 1999 TNO-FEL. All rights reserved.
*/

#include <errno.h>
#include <stdlib.h>
#include "clib.h"
#include <assert.h>
#include <string.h>

int
pl_error(const char *pred, int arity, const char *msg, int id, ...)
{ term_t except = PL_new_term_ref();
  term_t formal = PL_new_term_ref();
  term_t swi	= PL_new_term_ref();
  va_list args;

  va_start(args, id);
  switch(id)
  { case ERR_ERRNO:
    { int err = va_arg(args, int);
      
      msg = strerror(err);

      switch(err)
      { case ENOMEM:
	  PL_unify_term(formal,
			CompoundArg("resource_error", 1),
			  AtomArg("no_memory"));
	  break;
	case EACCES:
	{ const char *file = va_arg(args,   const char *);
	  const char *action = va_arg(args, const char *);

	  PL_unify_term(formal,
			CompoundArg("permission_error", 3),
			  AtomArg(action),
			  AtomArg("file"),
			  AtomArg(file));
	  break;
	}
	case ENOENT:
	{ const char *file = va_arg(args, const char *);

	  PL_unify_term(formal,
			CompoundArg("existence_error", 2),
			  AtomArg("file"),
			  AtomArg(file));
	  break;
	}
	default:
	  PL_unify_atom_chars(formal, "system_error");
	  break;
      }
      break;
    }
    case ERR_ARGTYPE:
    { int argn	      = va_arg(args, int);
      term_t actual   = va_arg(args, term_t);
      atom_t expected = PL_new_atom(va_arg(args, const char*));

      if ( PL_is_variable(actual) && expected != PL_new_atom("variable") )
	PL_unify_atom_chars(formal, "instantiation_error");
      else
	PL_unify_term(formal,
		      CompoundArg("type_error", 2),
		      PL_ATOM, expected,
		      PL_TERM, actual);
      break;
    }
    case ERR_EXISTENCE:
    { const char *type = va_arg(args, const char *);
      term_t obj  = va_arg(args, term_t);

      PL_unify_term(formal,
		    CompoundArg("existence_error", 2),
		    PL_CHARS, type,
		    PL_TERM, obj);

      break;
    }
    default:
      assert(0);
  }
  va_end(args);

  if ( pred || msg )
  { term_t predterm = PL_new_term_ref();
    term_t msgterm  = PL_new_term_ref();

    if ( pred )
    { PL_unify_term(predterm,
		    CompoundArg("divide", 2),
		      AtomArg(pred),
		      IntArg(arity));
    }
    if ( msg )
    { PL_put_atom_chars(msgterm, msg);
    }

    PL_unify_term(swi,
		  CompoundArg("context", 2),
		    PL_TERM, predterm,
		    PL_TERM, msgterm);
  }

  PL_unify_term(except,
		CompoundArg("error", 2),
		  PL_TERM, formal,
		  PL_TERM, swi);


  return PL_raise_exception(except);
}
