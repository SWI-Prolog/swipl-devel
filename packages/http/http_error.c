/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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

#define O_DEBUG 1

static functor_t FUNCTOR_error2;	/* error(Formal, Context) */
static functor_t FUNCTOR_type_error2;	/* type_error(Term, Expected) */
static functor_t FUNCTOR_domain_error2;	/* domain_error(Term, Expected) */
static functor_t FUNCTOR_permission_error3; /* permission_error(Op, Type, Term) */
static functor_t FUNCTOR_existence_error2; /* existence_error(Type, Term) */
static int debuglevel = 0;

#define MKFUNCTOR(name, arity) PL_new_functor(PL_new_atom(name), arity)

#ifdef O_DEBUG
#define DEBUG(n, g) if ( debuglevel >= n ) g
#else
#define DEBUG(n, g) (void)0
#endif


#ifdef O_DEBUG
static foreign_t
http_stream_debug(term_t level)
{ return PL_get_integer(level, &debuglevel);
}
#endif


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static int
type_error(term_t actual, const char *expected)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_type_error2,
		         PL_CHARS, expected,
		         PL_TERM, actual,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
domain_error(term_t actual, const char *domain)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_domain_error2,
		         PL_CHARS, domain,
		         PL_TERM, actual,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
existence_error(term_t actual, const char *type)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_existence_error2,
		         PL_CHARS, type,
		         PL_TERM, actual,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
permission_error(const char *op, const char *objtype, term_t obj)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_permission_error3,
		         PL_CHARS, op,
		         PL_CHARS, objtype,
		         PL_TERM, obj,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
instantiation_error()
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_CHARS, "instantiation_error",
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
get_int_ex(term_t t, int *i)
{ if ( PL_get_integer(t, i) )
    return TRUE;

  return type_error(t, "integer");
}


static int
get_bool_ex(term_t t, int *i)
{ if ( PL_get_bool(t, i) )
    return TRUE;

  return type_error(t, "boolean");
}


static void
init_errors()
{ FUNCTOR_error2            = MKFUNCTOR("error", 2);
  FUNCTOR_type_error2	    = MKFUNCTOR("type_error", 2);
  FUNCTOR_domain_error2     = MKFUNCTOR("domain_error", 2);
  FUNCTOR_existence_error2  = MKFUNCTOR("existence_error", 2);
  FUNCTOR_permission_error3 = MKFUNCTOR("permission_error", 3);

#ifdef O_DEBUG
  PL_register_foreign("http_stream_debug", 1, http_stream_debug, 0);
#endif
}
