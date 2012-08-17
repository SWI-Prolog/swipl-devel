/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

#ifndef YAP_INTERFACE_H_INCLUDED
#define YAP_INTERFACE_H_INCLUDED

#include <SWI-Prolog.h>
#include <math.h>
#include <gmp.h>

#define YAP_UserCPredicate(name, func, arity) \
	PL_register_foreign(name, arity, func, PL_FA_VARARGS)

#define YAP_ARGS term_t YAP_ARG1, int _arity, control_t _ctx
#define YAP_PASS_ARGS YAP_ARG1, _arity, _ctx
#define YAP_ARG2 (YAP_ARG1+1)
#define YAP_ARG3 (YAP_ARG1+2)
#define YAP_ARG4 (YAP_ARG1+3)
#define YAP_ARG5 (YAP_ARG1+4)
#define YAP_ARG6 (YAP_ARG1+5)
#define YAP_ARG7 (YAP_ARG1+6)
#define YAP_ARG8 (YAP_ARG1+7)
#define YAP_ARG9 (YAP_ARG1+8)

typedef int64_t YAP_Int;
typedef atom_t YAP_Atom;
typedef functor_t YAP_Functor;
typedef term_t YAP_Term;
typedef foreign_t YAP_Rc;

#define YAP_IsAtomTerm(t) PL_is_atom(t)
#define YAP_IsIntTerm(t) PL_is_integer(t)
#define YAP_IsFloatTerm(t) PL_is_float(t)
#define YAP_IsPairTerm(t) (PL_is_list(t) && !PL_is_atom(t))
#define YAP_IsVarTerm(t) PL_is_variable(t)
#define YAP_IsNilTerm(t) PL_get_nil(t)
#define YAP_IsNonVarTerm(t) (!PL_is_variable(t))
#define YAP_IsApplTerm(t) PL_is_compound(t)
#define YAP_Deref(t) (t)

#define YAP_LookupAtom(s) PL_new_atom(s)
#define YAP_ArityOfFunctor(f) PL_functor_arity(f)
#define YAP_NameOfFunctor(f) PL_functor_name(f)
#define YAP_MkFunctor(name, arity) PL_new_functor(name, arity)
#define YAP_MkNilTerm(t) PL_put_nil(t)

#define YAP_Unify(t1, t2) PL_unify(t1, t2)

#define YAP_AllocSpaceFromYap(size) PL_malloc(size)
#define YAP_FreeSpaceFromYap(ptr) PL_free(ptr)

static inline atom_t
YAP_AtomOfTerm(term_t t)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
    return a;

  return (atom_t)0;
}

static inline const char *
YAP_AtomName(atom_t a)
{ const char *s;

  if ( a && (s=PL_atom_nchars(a, NULL)) )
    return s;

  return NULL;
}


static inline YAP_Int
YAP_IntOfTerm(term_t t)
{ int64_t i;

  if ( PL_get_int64(t, &i) )
    return i;

  return -1;
}


#ifndef NAN
#define NAN 0.0
#endif

static inline double
YAP_FloatOfTerm(term_t t)
{ double f;

  if ( PL_get_float(t, &f) )
    return f;

  return NAN;
}


static inline term_t
YAP_MkVarTerm(void)
{ return PL_new_term_ref();
}


static inline term_t
YAP_MkIntTerm(YAP_Int i)
{ term_t t = PL_new_term_ref();

  if ( PL_put_int64(t, i) )
    return t;

  return (term_t)0;			/* stack overflow */
}


static inline term_t
YAP_MkAtomTerm(atom_t a)
{ term_t t = PL_new_term_ref();

  PL_put_atom(t, a);

  return t;
}


static inline term_t
YAP_MkFloatTerm(double f)
{ term_t t = PL_new_term_ref();

  if ( PL_put_float(t, f) )
    return t;

  return (term_t)0;
}


static inline term_t
YAP_MkPairTerm(term_t head, term_t tail)
{ term_t t;

  if ( (t = PL_new_term_ref()) &&
       PL_cons_list(t, head, tail) )
    return t;

  return (term_t)0;
}


static inline term_t
YAP_MkApplTerm(functor_t f, int arity, term_t *tv)
{ term_t t = PL_new_term_ref();
  term_t t0;

  if ( arity == 1 )
  { t0 = *tv;
  } else
  { int i;

    t0 = PL_new_term_refs(arity);
    for(i=0; i<arity; i++)
      PL_put_term(t0+0, tv[i]);
  }

  if ( PL_cons_functor_v(t, f, t0) )
    return t;

  return (term_t)0;
}


static inline functor_t
YAP_FunctorOfTerm(term_t t)
{ functor_t f;

  if ( PL_get_functor(t, &f) )
    return f;

  return (functor_t)f;
}


static inline term_t
YAP_HeadOfTerm(term_t t)
{ term_t ht = PL_new_term_refs(2);

  if ( PL_get_list(t, ht, ht+1) )
    return ht;

  return 0;
}

static inline term_t
YAP_TailOfTerm(term_t t)
{ term_t ht = PL_new_term_refs(2);

  if ( PL_get_list(t, ht, ht+1) )
    return ht+1;

  return 0;
}

static inline term_t
YAP_ArgOfTerm(int arg, term_t t)
{ term_t a = PL_new_term_ref();

  _PL_get_arg(arg, t, a);

  return a;
}




#endif /*YAP_INTERFACE_H_INCLUDED*/
