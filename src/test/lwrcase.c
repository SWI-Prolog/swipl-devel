/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
    Purpose: test load_foreign and friends
*/

#include <SWI-Prolog.h>
#include <ctype.h>

foreign_t
pl_lowercase(term u, term l)
{ char *copy;
  char *s, *q;
  atomic la;

  if ( !PL_is_atom(u) )
    return PL_warning("lowercase/2: instantiation fault");
  s = PL_atom_value(PL_atomic(u));
  copy = (char *) malloc(strlen(s)+1);

  for( q=copy; *s; q++, s++)
    *q = (isupper(*s) ? tolower(*s) : *s);
  *q = '\0';

  la = PL_new_atom(copy);
  free(copy);

  return PL_unify_atomic(l, la);
}

init_lowercase()
{ PL_register_foreign("lowercase", 2, pl_lowercase, 0);
}
