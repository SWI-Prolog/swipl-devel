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

#include "form.h"
#include <SWI-Prolog.h>
#include <string.h>
#include <stdlib.h> 
#include "clib.h"

static int
isinteger(const char *s, long *val, int len)
{ char *e;
  
  if ( len < 0 )
    len = strlen(s);
  if ( len == 0 )
    return FALSE;

  *val = strtol(s, &e, 10);
  if ( e == s+len )
    return TRUE;

  return FALSE;
}


static int
isfloat(const char *s, double *val, int len)
{ char *e;

  if ( len < 0 )
    len = strlen(s);
  if ( len == 0 )
    return FALSE;

  *val = strtod(s, &e);
  if ( e == s+len )
    return TRUE;

  return FALSE;
}


static int
add_to_form(const char *name, const char *value, void *closure)
{ term_t head = PL_new_term_ref();
  term_t tail = (term_t) closure;
  term_t val  = PL_new_term_ref();
  long vl;
  double vf;

  if ( isinteger(value, &vl, strlen(value)) )
    PL_put_integer(val, vl);
  else if ( isfloat(value, &vf, strlen(value)) )
    PL_put_float(val, vf);
  else
    PL_put_atom_chars(val, value);

  if ( !PL_unify_list(tail, head, tail) ||
       !PL_unify_term(head,
		      PL_FUNCTOR, PL_new_functor(PL_new_atom(name), 1),
		      PL_TERM, val) )
    return FALSE;

  return TRUE;
}


static int
mp_add_to_form(const char *name, const char *value, int len,
	       const char *file, void *closure)
{ term_t head = PL_new_term_ref();
  term_t tail = (term_t) closure;
  term_t val  = PL_new_term_ref();
  long vl;
  double vf;

  if ( isinteger(value, &vl, len) )
    PL_put_integer(val, vl);
  else if ( isfloat(value, &vf, len) )
    PL_put_float(val, vf);
  else
    PL_put_atom_nchars(val, len, value);

  if ( !PL_unify_list(tail, head, tail) ||
       !PL_unify_term(head,
		      PL_FUNCTOR, PL_new_functor(PL_new_atom(name), 1),
		      PL_TERM, val) )
    return FALSE;

  return TRUE;
}


static foreign_t
pl_cgi_get_form(term_t form)
{ int len = 0;
  char *data = get_raw_form_data(&len);
  term_t list = PL_copy_term_ref(form);
  char *ct, *boundary;

  if ( !data )
  { term_t ctx = PL_new_term_ref();

    PL_put_nil(ctx);
    return pl_error("cgi_get_form", 1, "no data?",
		    ERR_EXISTENCE, "cgi_form", ctx);
  }

  if ( (ct = getenv("CONTENT_TYPE")) &&
       (boundary = strstr(ct, "boundary=")) )
  { boundary = strchr(boundary, '=')+1;

    break_multipart(data, len, boundary, mp_add_to_form, (void *)list);
  } else
  { break_form_argument(data, add_to_form, (void *)list);
  }

  return PL_unify_nil(list);
}


install_t
install_cgi()
{ PL_register_foreign("cgi_get_form", 1, pl_cgi_get_form, 0);
}
