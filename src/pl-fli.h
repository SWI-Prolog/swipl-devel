/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1996-2021, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include "pl-incl.h"

#ifndef _PL_FLI_H
#define _PL_FLI_H

#if USE_FLI_INLINES
#define FLI_INLINE static MAYBE_UNUSED
#else
#define FLI_INLINE
#endif

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

word		linkVal__LD(Word p ARG_LD);
word		linkValG__LD(Word p ARG_LD);
word		linkValNoG__LD(Word p ARG_LD);
void		bArgVar(Word ap, Word vp ARG_LD);
int		_PL_put_number__LD(term_t t, Number n ARG_LD);
FLI_INLINE
predicate_t	_PL_predicate(const char *name, int arity,
			      const char *module, predicate_t *bin);
void		initialiseForeign(int argc, char **argv);
void		cleanupInitialiseHooks(void);
atom_t		codeToAtom(int code);
int		PL_unify_term__LD(term_t t ARG_LD, ...);
int		PL_unify_termv(term_t t, va_list args);
int		PL_unify_termv__LD(term_t t ARG_LD, va_list args);
term_t		pushWordAsTermRef__LD(Word p ARG_LD);
void		popTermRef__LD(ARG1_LD);
int		_PL_get_arg__LD(size_t index, term_t t, term_t a ARG_LD);
term_t		PL_new_term_ref__LD(ARG1_LD);
term_t		PL_new_term_ref_noshift__LD(ARG1_LD);
term_t		PL_new_term_refs__LD(int n ARG_LD);
int		globalize_term_ref__LD(term_t t ARG_LD);
void		PL_reset_term_refs__LD(term_t r ARG_LD);
term_t		PL_copy_term_ref__LD(term_t from ARG_LD);
int		PL_unify__LD(term_t t1, term_t t2 ARG_LD);
int		PL_unify_output__LD(term_t t1, term_t t2 ARG_LD);
int		PL_unify_integer__LD(term_t t1, intptr_t i ARG_LD);
int		PL_unify_int64__LD(term_t t1, int64_t ARG_LD);
int		PL_unify_int64_ex__LD(term_t t1, int64_t ARG_LD);
int		PL_unify_functor__LD(term_t t, functor_t f ARG_LD);
FLI_INLINE int	PL_get_atom__LD(term_t t1, atom_t *a ARG_LD);
int		PL_get_text_as_atom(term_t t, atom_t *a, int flags);
FLI_INLINE int	PL_put_variable__LD(term_t t1 ARG_LD);
FLI_INLINE int	PL_put_atom__LD(term_t t1, atom_t a ARG_LD);
FLI_INLINE int	PL_put_integer__LD(term_t t1, long i ARG_LD);
FLI_INLINE int	PL_put_int64__LD(term_t t, int64_t i ARG_LD);
FLI_INLINE int	PL_put_intptr__LD(term_t t1, intptr_t i ARG_LD);
FLI_INLINE int	PL_is_atomic__LD(term_t t ARG_LD);
FLI_INLINE int	PL_is_functor__LD(term_t t, functor_t f ARG_LD);
FLI_INLINE int	PL_is_variable__LD(term_t t ARG_LD);
int		PL_strip_module__LD(term_t q, module_t *m,
				    term_t t, int flags ARG_LD) WUNUSED;
int		PL_strip_module_ex__LD(term_t raw, module_t *m,
				       term_t plain ARG_LD) WUNUSED;
int		PL_qualify(term_t raw, term_t qualified);
int		PL_get_integer__LD(term_t t, int *i ARG_LD);
int		PL_get_long__LD(term_t t, long *i ARG_LD);
int		PL_get_int64__LD(term_t t, int64_t *i ARG_LD);
int		PL_get_size_ex__LD(term_t t, size_t *i ARG_LD);
int		PL_get_pointer__LD(term_t t, void **ptr ARG_LD);
int		PL_put_term__LD(term_t t1, term_t t2 ARG_LD)/* WUNUSED*/;
int		PL_get_functor__LD(term_t t, functor_t *f ARG_LD);
int		PL_get_name_arity_sz__LD(term_t t, atom_t *name,
						 size_t *arity ARG_LD);
int		PL_get_uintptr(term_t t, size_t *i);
int		PL_unify_atom__LD(term_t t, atom_t a ARG_LD);
int		PL_unify_pointer__LD(term_t t, void *ptr ARG_LD);
int		PL_get_list__LD(term_t l, term_t h, term_t t ARG_LD);
FLI_INLINE int	PL_is_atom__LD(term_t t ARG_LD);
int		PL_unify_list__LD(term_t l, term_t h, term_t t ARG_LD);
int		PL_cons_list__LD(term_t l, term_t head, term_t tail
				 ARG_LD);
int		PL_cons_list_v(term_t list, size_t count, term_t elems);
int		PL_is_inf(term_t t);
int		PL_same_term__LD(term_t t1, term_t t2 ARG_LD);
int		isUCSAtom(Atom a);
atom_t		lookupUCSAtom(const pl_wchar_t *s, size_t len);
int		charCode(word w);
int		isCallable(word w ARG_LD);

void		registerForeignLicenses(void);
void            bindExtensions(const char *module,
			       const PL_extension *ext);
void		initForeign(void);
int		PL_rethrow(void);
FLI_INLINE int	PL_pending__LD(int sig ARG_LD);
int		PL_clearsig__LD(int sig ARG_LD);
void		cleanupCodeToAtom(void);
void		PL_clear_foreign_exception(LocalFrame fr);
int		has_emergency_space(void *sv, size_t needed);
FLI_INLINE
except_class	classify_exception__LD(term_t ex ARG_LD);
except_class    classify_exception_p__LD(Word p ARG_LD);
void		PL_abort_process(void) NORETURN;
void		resetForeign(void);

		 /*******************************
		 *	LD-USING FUNCTIONS	*
		 *******************************/

#define globalizeTermRef(t)	globalize_term_ref__LD(t PASS_LD)
#define linkVal(p)		linkVal__LD(p PASS_LD)
#define linkValG(p)		linkValG__LD(p PASS_LD)
#define linkValNoG(p)		linkValNoG__LD(p PASS_LD)
#define popTermRef()		popTermRef__LD(PASS_LD1)
#define pushWordAsTermRef(p)	pushWordAsTermRef__LD(p PASS_LD)
#define _PL_get_arg(n, t, a)	_PL_get_arg__LD(n, t, a PASS_LD)
#define _PL_put_number(t, n)	_PL_put_number__LD(t, n PASS_LD)
#define PL_new_term_ref()	PL_new_term_ref__LD(PASS_LD1)
#define PL_new_term_ref_noshift()	PL_new_term_ref_noshift__LD(PASS_LD1)
#define PL_new_term_refs(n)	PL_new_term_refs__LD(n PASS_LD)
#define PL_reset_term_refs(t)	PL_reset_term_refs__LD(t PASS_LD)
#define PL_copy_term_ref(t)	PL_copy_term_ref__LD(t PASS_LD)
#define PL_unify(t1, t2)	PL_unify__LD(t1, t2 PASS_LD)
#define PL_unify_integer(t, i)	PL_unify_integer__LD(t, i PASS_LD)
#define PL_unify_int64(t, i)	PL_unify_int64__LD(t, i PASS_LD)
#define PL_unify_functor(t, f)	PL_unify_functor__LD(t, f PASS_LD)
#define PL_unify_term(t, ...)	PL_unify_term__LD(t PASS_LD, __VA_ARGS__)
#define PL_unify_output(t1,t2)	PL_unify_output__LD(t1, t2 PASS_LD)
#define PL_get_atom(t, a)	PL_get_atom__LD(t, a PASS_LD)
#define PL_put_atom(t, a)	PL_put_atom__LD(t, a PASS_LD)
#define PL_put_variable(t)	PL_put_variable__LD(t PASS_LD)
#define PL_is_functor(t, f)	PL_is_functor__LD(t, f PASS_LD)
#define PL_put_integer(t, i)	PL_put_integer__LD(t, i PASS_LD)
#define PL_put_intptr(t, i)	PL_put_intptr__LD(t, i PASS_LD)
#define PL_strip_module(q, m, t) PL_strip_module__LD(q, m, t, 0 PASS_LD)
#define PL_get_integer(t, i)	PL_get_integer__LD(t, i PASS_LD)
#define PL_get_long(t, i)	PL_get_long__LD(t, i PASS_LD)
#define PL_get_int64(t, i)	PL_get_int64__LD(t, i PASS_LD)
#define PL_get_size_ex(t,i)	PL_get_size_ex__LD(t,i PASS_LD)
#define PL_get_pointer(t, ptr)	PL_get_pointer__LD(t, ptr PASS_LD)
#define PL_put_term(t1, t2)	PL_put_term__LD(t1, t2 PASS_LD)
#define PL_get_functor(t, f)	PL_get_functor__LD(t, f PASS_LD)
#define PL_unify_atom(t, a)	PL_unify_atom__LD(t, a PASS_LD)
#define _PL_unify_atomic(t, a)	PL_unify_atom__LD(t, a PASS_LD)
#define PL_unify_pointer(t, p)	PL_unify_pointer__LD(t, p PASS_LD)
#define PL_is_variable(t)	PL_is_variable__LD(t PASS_LD)
#define PL_is_atomic(t)		PL_is_atomic__LD(t PASS_LD)
#define PL_get_list(l, h, t)	PL_get_list__LD(l, h, t PASS_LD)
#define PL_is_atom(t)		PL_is_atom__LD(t PASS_LD)
#define PL_unify_list(l, h, t)	PL_unify_list__LD(l, h, t PASS_LD)
#define PL_cons_list(l, h, t)	PL_cons_list__LD(l, h, t PASS_LD)
#define PL_unify_int64_ex(t, i)	PL_unify_int64_ex__LD(t, i PASS_LD)
#define PL_pending(sig)	        PL_pending__LD(sig PASS_LD)
#define PL_clearsig(sig)        PL_clearsig__LD(sig PASS_LD)
#define PL_same_term(t1, t2)	PL_same_term__LD(t1, t2 PASS_LD)
#define PL_get_name_arity_sz(t,n,a) PL_get_name_arity_sz__LD(t,n,a PASS_LD)
#define PL_strip_module_ex(t,m,p) PL_strip_module_ex__LD(t,m,p PASS_LD)
#define classify_exception(ex)  classify_exception__LD(ex PASS_LD)
#define classify_exception_p(p) classify_exception_p__LD(p PASS_LD)

		 /*******************************
		 *	INLINE DEFINITIONS	*
		 *******************************/

#if USE_FLI_INLINES || EMIT_FLI_INLINES
#include "pl-wam.h"
	
#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)

    /* TODO: valHandle (which was in pl-codelisit.h) needs to be moved somewhere common! Namespacing it with fli_ for now to avoid conflicts. */
    static inline word
    fli_valHandle__LD(term_t r ARG_LD)
    { Word p = valTermRef(r);
    
      deRef(p);
      return *p;
    }
    #pragma push_macro("valHandle")
    #undef valHandle
    #define valHandle(r) fli_valHandle__LD(r PASS_LD)

FLI_INLINE int
PL_get_atom__LD(term_t t, atom_t *a ARG_LD)
{ word w = valHandle(t);

  if ( isAtom(w) )
  { *a = (atom_t) w;
    succeed;
  }
  fail;
}

FLI_INLINE int
PL_is_variable__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  return canBind(w) ? TRUE : FALSE;
}

FLI_INLINE int
PL_is_atom__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  if ( isTextAtom(w) )
    return TRUE;

  return FALSE;
}

FLI_INLINE int
PL_is_functor__LD(term_t t, functor_t f ARG_LD)
{ word w = valHandle(t);

  if ( hasFunctor(w, f) )
    succeed;

  fail;
}

FLI_INLINE int
PL_is_atomic__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  return !!isAtomic(w);
}

FLI_INLINE int
PL_put_variable__LD(term_t t ARG_LD)
{ Word p = valTermRef(t);

  setVar(*p);
  return TRUE;
}

FLI_INLINE int
PL_put_atom__LD(term_t t, atom_t a ARG_LD)
{ setHandle(t, a);
  return TRUE;
}

FLI_INLINE int
PL_put_int64__LD(term_t t, int64_t i ARG_LD)
{ word w = consInt(i);

  if ( valInt(w) != i &&
       put_int64(&w, i, ALLOW_GC PASS_LD) != TRUE )
    return FALSE;

  setHandle(t, w);
  return TRUE;
}

FLI_INLINE int
PL_put_integer__LD(term_t t, long i ARG_LD)
{ return PL_put_int64__LD(t, i PASS_LD);
}

FLI_INLINE int
PL_put_intptr__LD(term_t t, intptr_t i ARG_LD)
{ return PL_put_int64__LD(t, i PASS_LD);
}

FLI_INLINE predicate_t
_PL_predicate(const char *name, int arity, const char *module,
	      predicate_t *bin)
{ if ( !*bin )
    *bin = PL_predicate(name, arity, module);

  return *bin;
}

FLI_INLINE except_class
classify_exception__LD(term_t exception ARG_LD)
{ Word p;

  if ( !exception )
    return EXCEPT_NONE;

  p = valTermRef(exception);
  return classify_exception_p(p);
}

FLI_INLINE int
PL_pending__LD(int sig ARG_LD)
{ return pendingSignal(LD, sig);
}

    #pragma pop_macro("valHandle")

#endif /*USE_FLI_INLINES || EMIT_FLI_INLINESS*/

#endif /*_PL_FLI_H*/
