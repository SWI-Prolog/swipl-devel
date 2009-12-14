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
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "clib.h"
#include "form.h"

static int
isinteger(const char *s, long *val, size_t len)
{ char *e;

  if ( len == (size_t)-1 )
    len = strlen(s);
  if ( len == 0 )
    return FALSE;

  *val = strtol(s, &e, 10);
  if ( e == s+len )
    return TRUE;

  return FALSE;
}


static int
isfloat(const char *s, double *val, size_t len)
{ char *e;

  if ( len == (size_t)-1 )
    len = strlen(s);
  if ( len == 0 )
    return FALSE;

  *val = strtod(s, &e);
  if ( e == s+len )
    return TRUE;

  return FALSE;
}


static int
add_to_form(const char *name, size_t nlen,
	    const char *value, size_t len,
	    void *closure)
{ term_t head = PL_new_term_ref();
  term_t tail = (term_t) closure;
  term_t val  = PL_new_term_ref();
  long vl;
  double vf;
  int rc;
  atom_t aname = 0;

  if ( isinteger(value, &vl, len) )
    rc = PL_put_integer(val, vl);
  else if ( isfloat(value, &vf, len) )
    rc = PL_put_float(val, vf);
  else
    rc = PL_unify_chars(val, PL_ATOM|REP_UTF8, len, value);

  rc = ( rc &&
	 PL_unify_list(tail, head, tail) &&
	 (aname = PL_new_atom_nchars(nlen, name)) &&
	 PL_unify_term(head,
		       PL_FUNCTOR, PL_new_functor(aname, 1),
		       PL_TERM, val) );

  if ( aname )
    PL_unregister_atom(aname);

  return rc;
}


static int
mp_add_to_form(const char *name, size_t nlen,
	       const char *value, size_t len,
	       const char *file, void *closure)
{ term_t head = PL_new_term_ref();
  term_t tail = (term_t) closure;
  term_t val  = PL_new_term_ref();
  long vl;
  double vf;
  int rc;
  atom_t aname = 0;

  if ( isinteger(value, &vl, len) )
    rc = PL_put_integer(val, vl);
  else if ( isfloat(value, &vf, len) )
    rc = PL_put_float(val, vf);
  else
    rc = PL_unify_chars(val, PL_ATOM|REP_UTF8, len, value);

  rc = ( rc &&
	 PL_unify_list(tail, head, tail) &&
	 (aname = PL_new_atom_nchars(nlen, name)) &&
	 PL_unify_term(head,
			PL_FUNCTOR, PL_new_functor(aname, 1),
			PL_TERM, val) );

  if ( aname )
    PL_unregister_atom(aname);

  return rc;
}


static foreign_t
pl_cgi_get_form(term_t form)
{ size_t len = 0;
  char *data;
  int must_free = FALSE;
  term_t list = PL_copy_term_ref(form);
  char *ct, *boundary;

  if ( !get_raw_form_data(&data, &len, &must_free) )
    return FALSE;

  if ( (ct = getenv("CONTENT_TYPE")) &&
       (boundary = strstr(ct, "boundary=")) )
  { boundary = strchr(boundary, '=')+1;

    switch( break_multipart(data, len, boundary,
			    mp_add_to_form, (void *)list) )
    { case FALSE:
	return FALSE;
      case TRUE:
	break;
      default:
	assert(0);
        return FALSE;
    }
  } else
  { switch( break_form_argument(data, add_to_form, (void *)list) )
    { case FALSE:
	return FALSE;
      case TRUE:
	break;
      case ERROR_NOMEM:
	return pl_error("cgi_get_form", 1, NULL,
			ERR_RESOURCE, "memory");
      case ERROR_SYNTAX_ERROR:
	return pl_error("cgi_get_form", 1, NULL,
			ERR_SYNTAX, "cgi_value");
      default:
	assert(0);
        return FALSE;
    }
  }

  if ( must_free )
    free(data);

  return PL_unify_nil(list);
}


install_t
install_cgi()
{ PL_register_foreign("cgi_get_form", 1, pl_cgi_get_form, 0);
}
