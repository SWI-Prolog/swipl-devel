/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amseterdam. All rights reserved.
*/

#include "form.h"
#include <SWI-Prolog.h>
#include <string.h>
#include <stdlib.h> 
#include "clib.h"

static int
isinteger(const char *s, long *val)
{ char *e;

  *val = strtol(s, &e, 10);
  if ( *s && !*e )
    return TRUE;

  return FALSE;
}


static int
isfloat(const char *s, double *val)
{ char *e;

  *val = strtod(s, &e);
  if ( *s && !*e )
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

  if ( isinteger(value, &vl) )
    PL_put_integer(val, vl);
  else if ( isfloat(value, &vf) )
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


static foreign_t
pl_cgi_get_form(term_t form)
{ char *data = get_raw_form_data(NULL);
  term_t list = PL_copy_term_ref(form);

  if ( !data )
  { term_t ctx = PL_new_term_ref();

    PL_put_nil(ctx);
    return pl_error("cgi_get_form", 1, "no data?",
		    ERR_EXISTENCE, "cgi_form", ctx);
  }

  break_form_argument(data, add_to_form, (void *)list);

  return TRUE;
}


install_t
install_cgi()
{ PL_register_foreign("cgi_get_form", 1, pl_cgi_get_form, 0);
}
