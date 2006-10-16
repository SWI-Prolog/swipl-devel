/*  $Id$

    Part of SWI-Prolog

    Author:        L.Damas, V.S.Costa, Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): Universidade do Porto, University of Amsterdam

    Original code create for YAP under Artistic license.  As this
    license is compatible (but less restrictive than) the SWI-Prolog
    license we keep the Artistic license.
*/


/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		random.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Random number generation                                 *
*									 *
*************************************************************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Ported to SWI-Prolog by Jan Wielemaker

To compile:

	plld -o random -shared -fpic random.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <SWI-Prolog.h>
#include <math.h>
#include <limits.h>

static functor_t FUNCTOR_rand3;

static short a1 = 27314, b1 = 9213, c1 = 17773;

static int
type_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "type_error", 2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
domain_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "domain_error", 2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
get_short_ex(term_t t, short *p)
{ long v;

  if ( !PL_get_long(t, &v) )
    return type_error(t, "integer");
  if ( v < SHRT_MIN || v > SHRT_MAX )
    return domain_error(t, "short integer");
  
  *p = (short)v;

  return TRUE;
}


static int
get_short_arg_ex(int a, term_t state, short *p)
{ term_t arg = PL_new_term_ref();

  _PL_get_arg(a, state, arg);
  return get_short_ex(arg, p);
}


static foreign_t
p_random(term_t rnd)
{
  double fli;
  long int t1, t2, t3;

  t1 = (a1 * 171) % 30269;
  t2 = (b1 * 172) % 30307;
  t3 = (c1 * 170) % 30323;
  fli = (t1/30269.0) + (t2/30307.0) + (t3/30323.0);
  a1 = t1;
  b1 = t2;
  c1 = t3;

  return PL_unify_float(rnd, fli-(int)(fli));
}

static foreign_t
p_setrand(term_t state)
{ if ( !PL_is_functor(state, FUNCTOR_rand3) )
    return type_error(state, "rand_state");

  if ( !get_short_arg_ex(1, state, &a1) ||
       !get_short_arg_ex(2, state, &b1) ||
       !get_short_arg_ex(3, state, &c1) )
    return FALSE;

  return TRUE;
}

static foreign_t
p_getrand(term_t state)
{ return PL_unify_term(state,
		       PL_FUNCTOR, FUNCTOR_rand3,
		         PL_INTEGER, a1,
		         PL_INTEGER, b1,
		         PL_INTEGER, c1);
}


install_t 
install_random()
{ FUNCTOR_rand3 = PL_new_functor(PL_new_atom("rand"), 3);

  PL_register_foreign("random",  1, p_random, 0);
  PL_register_foreign("setrand", 1, p_setrand, 0);
  PL_register_foreign("getrand", 1, p_getrand, 0);
}

