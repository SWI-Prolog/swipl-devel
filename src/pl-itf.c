/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: foreign language interface
*/

#include "pl-incl.h"

#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)

static inline word
valHandle(term_t r)
{ Word p = valTermRef(r);

  deRef(p);
  return *p;
}

		/********************************
		*           ANALYSIS            *
		*********************************/

atomic_t
_PL_atomic(term_t t)
{ return valHandle(t);
}


double
_PL_float_value(atomic_t a)
{ return valReal(a);
}


#if O_STRING
char *
_PL_string_value(atomic_t a)
{ return valString(a);
}
#endif /* O_STRING */


char *
_PL_list_string_value(term_t t)
{ char *s;

  if ( PL_get_list_chars(t, &s, 0) )
    return s;

  return NULL;
}


long
_PL_integer_value(atomic_t a)
{ return valInteger(a);
}


functor_t
_PL_functor(term_t t)
{ word w = valHandle(t);

  return isTerm(w) ? functorTerm(w) : (FunctorDef)NULL;
}


term_t
_PL_arg(term_t t, int n)
{ term_t a = PL_new_term_ref();
  word w = valHandle(t);
  Word p = argTermP(w, n-1);

  deRef(p);
  setHandle(a, isVar(*p) ? makeRef(p) : *p);

  return a;
}


term_t
_PL_strip_module(term_t t, Module *m)
{ term_t a = PL_new_term_ref();

  PL_strip_module(t, m, a);

  return a;
}

		/********************************
		*         CONSTRUCTION          *
		*********************************/

term_t
_PL_new_term()
{ term_t t = PL_new_term_ref();

  PL_put_variable(t);
  return t;
}


term_t
_PL_term(atomic_t a)
{ term_t t = PL_new_term_ref();

  setHandle(t, a);

  return t;
}


atomic_t
_PL_new_integer(long i)
{ return consInt(i);
}


atomic_t
_PL_new_float(double f)
{ return globalReal(f);
}


#if O_STRING
atomic_t
_PL_new_string(const char *s)
{ return globalString(s);
}
#endif /* O_STRING */


atomic_t
_PL_new_var()
{ return 0L;
}


