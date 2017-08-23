/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2016, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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


/* NOTE: The arity is encoded in the functor.  We check consistency
*/

static inline term_t
YAP_MkNewApplTerm(functor_t f, int arity)
{ term_t t = PL_new_term_ref();

  assert(PL_functor_arity(f) == arity);
  if ( PL_put_functor(t, f) )
    return t;
  else
    return (term_t)0;
}


/* NOTE: This is expensive in SWI-Prolog.  YAP term-references are
   direct pointers and thus it merely returns a pointer to the
   array of arguments.  In SWI-Prolog, term references are indirect
   handles and thus we must allocate a handle for each argument.
*/

static inline term_t
YAP_ArgsOfTerm(term_t t)
{ atom_t name;
  int arity, i, res;
  term_t args;

  if ( !(res = PL_get_name_arity( t, &name, &arity)) ||
       !(args = PL_new_term_refs(arity)) ) /* Leaves an exception on failure */
    return (term_t)0;

  for (i=1; i<=arity; i++)
    _PL_get_arg(i, t, args+i-1);

  return args;
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
