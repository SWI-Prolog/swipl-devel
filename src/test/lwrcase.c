/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
    Purpose: test load_foreign and friends
*/

#include <SWI-Prolog.h>
#include <ctype.h>

foreign_t
pl_lowercase(term_t u, term_t l)
{ char *copy;
  char *s, *q;
  atom_t la;

  if ( !PL_get_atom_chars(u, &s) )
    return PL_warning("lowercase/2: instantiation fault");
  copy = malloc(strlen(s)+1);

  for( q=copy; *s; q++, s++)
    *q = (isupper(*s) ? tolower(*s) : *s);
  *q = '\0';

  la = PL_new_atom(copy);
  free(copy);

  return PL_unify_atom(l, la);
}

install_t
install()
{ PL_register_foreign("lowercase", 2, pl_lowercase, 0);
}


install_t
uninstall()
{
}
