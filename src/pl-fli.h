/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1996-2024, University of Amsterdam
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

#if USE_LD_MACROS
#if O_VALIDATE_API
#define valid_term_t(t)				LDFUNC(valid_term_t, t)
#define valid_user_term_t(t)			LDFUNC(valid_user_term_t, t)
#endif
#define	linkVal(p)				LDFUNC(linkVal, p)
#define	linkValG(p)				LDFUNC(linkValG, p)
#define	linkValNoG(p)				LDFUNC(linkValNoG, p)
#define	bArgVar(ap, vp)				LDFUNC(bArgVar, ap, vp)
#define	_PL_put_number(t, n)			LDFUNC(_PL_put_number, t, n)
#define	PL_unify_term(t, ...)			LDFUNC(PL_unify_term, t, __VA_ARGS__)
#define PL_cons_functor(h, fd, ...)		LDFUNC(PL_cons_functor, h, fd, __VA_ARGS__)
#define	PL_unify_termv(t, args)			LDFUNC(PL_unify_termv, t, args)
#define	pushWordAsTermRef(p)			LDFUNC(pushWordAsTermRef, p)
#define	popTermRef(_)				LDFUNC(popTermRef, _)
#define	_PL_get_arg(index, t, a)		LDFUNC(_PL_get_arg, index, t, a)
#define PL_get_intptr(t, i)			LDFUNC(PL_get_intptr, t, i)
#define	PL_new_term_ref(_)			LDFUNC(PL_new_term_ref, _)
#define	PL_new_term_ref_noshift(_)		LDFUNC(PL_new_term_ref_noshift, _)
#define	PL_new_term_refs(n)			LDFUNC(PL_new_term_refs, n)
#define PL_free_term_ref(t)			LDFUNC(PL_free_term_ref, t)
#define PL_new_functor_sz(f, arity)		LDFUNC(PL_new_functor_sz, f, arity)
#define	globalizeTermRef(t)			LDFUNC(globalizeTermRef, t)
#define	PL_reset_term_refs(r)			LDFUNC(PL_reset_term_refs, r)
#define	PL_copy_term_ref(from)			LDFUNC(PL_copy_term_ref, from)
#define	PL_unify(t1, t2)			LDFUNC(PL_unify, t1, t2)
#define	PL_unify_output(t1, t2)			LDFUNC(PL_unify_output, t1, t2)
#define	PL_unify_integer(t1, i)			LDFUNC(PL_unify_integer, t1, i)
#define	PL_unify_int64(t1, int64_t)		LDFUNC(PL_unify_int64, t1, int64_t)
#define	PL_unify_int64_ex(t1, int64_t)		LDFUNC(PL_unify_int64_ex, t1, int64_t)
#define	PL_unify_functor(t, f)			LDFUNC(PL_unify_functor, t, f)
#define	PL_unify_nil(t)				LDFUNC(PL_unify_nil, t)
#define	PL_get_atom(t1, a)			LDFUNC(PL_get_atom, t1, a)
#define	PL_put_variable(t1)			LDFUNC(PL_put_variable, t1)
#define	PL_put_atom(t1, a)			LDFUNC(PL_put_atom, t1, a)
#define	PL_put_integer(t1, i)			LDFUNC(PL_put_integer, t1, i)
#define	PL_put_int64(t, i)			LDFUNC(PL_put_int64, t, i)
#define	PL_put_intptr(t1, i)			LDFUNC(PL_put_intptr, t1, i)
#define	PL_is_atomic(t)				LDFUNC(PL_is_atomic, t)
#define PL_is_number(t)				LDFUNC(PL_is_number, t)
#define	PL_is_functor(t, f)			LDFUNC(PL_is_functor, t, f)
#define	PL_is_variable(t)			LDFUNC(PL_is_variable, t)
#define	PL_is_attvar(t)				LDFUNC(PL_is_attvar, t)
#define	PL_strip_module_flags(q, m, t, flags)	LDFUNC(PL_strip_module_flags, q, m, t, flags)
#define	PL_strip_module_ex(raw, m, plain)	LDFUNC(PL_strip_module_ex, raw, m, plain)
#define	PL_get_integer(t, i)			LDFUNC(PL_get_integer, t, i)
#define	PL_get_long(t, i)			LDFUNC(PL_get_long, t, i)
#define	PL_get_int64(t, i)			LDFUNC(PL_get_int64, t, i)
#define	PL_get_pointer(t, ptr)			LDFUNC(PL_get_pointer, t, ptr)
#define	PL_put_term(t1, t2)			LDFUNC(PL_put_term, t1, t2)
#define	PL_get_functor(t, f)			LDFUNC(PL_get_functor, t, f)
#define	PL_get_name_arity_sz(t, name, arity)	LDFUNC(PL_get_name_arity_sz, t, name, arity)
#define	PL_unify_atom(t, a)			LDFUNC(PL_unify_atom, t, a)
#define	PL_unify_atomic(t, w)			LDFUNC(PL_unify_atomic, t, w)
#define PL_unify_bool(t, val)			LDFUNC(PL_unify_bool, t, val)
#define	PL_unify_pointer(t, ptr)		LDFUNC(PL_unify_pointer, t, ptr)
#define	PL_get_list(l, h, t)			LDFUNC(PL_get_list, l, h, t)
#define	PL_is_atom(t)				LDFUNC(PL_is_atom, t)
#define	PL_unify_list(l, h, t)			LDFUNC(PL_unify_list, l, h, t)
#define	PL_cons_list(l, head, tail)		LDFUNC(PL_cons_list, l, head, tail)
#define	isCallable(w)				LDFUNC(isCallable, w)
#define	PL_pending(sig)				LDFUNC(PL_pending, sig)
#define	PL_clearsig(sig)			LDFUNC(PL_clearsig, sig)
#define	classify_exception(ex)			LDFUNC(classify_exception, ex)
#define	classify_exception_p(p)			LDFUNC(classify_exception_p, p)
#define	get_string_text(w, text)		LDFUNC(get_string_text, w, text)
#define PL_unify_arg_sz(i, t, a)		LDFUNC(PL_unify_arg_sz, i, t, a)
#define PL_get_nil(l)				LDFUNC(PL_get_nil, l)
#define PL_is_list(l)				LDFUNC(PL_is_list, l)
#define PL_unify_float(t, f)			LDFUNC(PL_unify_float, t, f)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

#if O_VALIDATE_API
void		valid_term_t(term_t t);
void		valid_atom_t(atom_t t);
void		valid_functor_t(functor_t t);
void		valid_dict_key(atom_t a);
#else
#define valid_term_t(t) (void)0
#define valid_atom_t(t) (void)0
#define valid_functor_t(t) (void)0
#define valid_dict_key(t) (void)0
#endif

word		linkVal(Word p);
word		linkValG(Word p);
word		linkValNoG(Word p);
void		bArgVar(Word ap, Word vp);
bool		_PL_put_number(term_t t, Number n);
FLI_INLINE
predicate_t	_PL_predicate(const char *name, int arity,
			      const char *module, predicate_t *bin);
void		initialiseForeign(int argc, char **argv);
void		cleanupInitialiseHooks(void);
void		cleanAbortHooks(PL_local_data_t *ld);
atom_t		codeToAtom(int code);
int		atom_to_bool(atom_t a);
bool		PL_unify_term(term_t t, ...);
bool		PL_cons_functor(term_t h, functor_t fd, ...);
bool		PL_unify_termv(term_t t, va_list args);
term_t		pushWordAsTermRef(Word p);
void		popTermRef(void);
bool		_PL_get_arg(size_t index, term_t t, term_t a);
bool		PL_get_intptr(term_t t, intptr_t *i);
term_t		PL_new_term_ref(void);
term_t		PL_new_term_ref_noshift(void);
term_t		PL_new_term_refs(size_t n);
void		PL_free_term_ref(term_t t);
functor_t	PL_new_functor_sz(atom_t f, size_t arity);
int		globalizeTermRef(term_t t);
void		PL_reset_term_refs(term_t r);
term_t		PL_copy_term_ref(term_t from);
bool		PL_unify(term_t t1, term_t t2);
bool		PL_unify_output(term_t t1, term_t t2);
bool		PL_unify_integer(term_t t1, intptr_t i);
bool		PL_unify_int64(term_t t1, int64_t);
bool		PL_unify_int64_ex(term_t t1, int64_t);
bool		PL_unify_functor(term_t t, functor_t f);
bool		PL_unify_nil(term_t t);
FLI_INLINE bool	PL_get_atom(term_t t1, atom_t *a);
bool		PL_get_text_as_atom(term_t t, atom_t *a, int flags);
FLI_INLINE bool	PL_put_variable(term_t t1);
FLI_INLINE bool	PL_put_atom(term_t t1, atom_t a);
FLI_INLINE bool	PL_put_integer(term_t t1, long i);
FLI_INLINE bool	PL_put_int64(term_t t, int64_t i);
FLI_INLINE bool	PL_put_intptr(term_t t1, intptr_t i);
FLI_INLINE bool	PL_is_atomic(term_t t);
FLI_INLINE bool	PL_is_number(term_t t);
FLI_INLINE bool	PL_is_functor(term_t t, functor_t f);
FLI_INLINE bool	PL_is_variable(term_t t);
FLI_INLINE bool	PL_is_attvar(term_t t);
bool		PL_strip_module_flags(term_t q, module_t *m,
				      term_t t, int flags) WUNUSED;
bool		PL_strip_module_ex(term_t raw, module_t *m,
				   term_t plain) WUNUSED;
bool		PL_qualify(term_t raw, term_t qualified);
bool		PL_get_integer(term_t t, int *i);
bool		PL_get_long(term_t t, long *i);
bool		PL_get_int64(term_t t, int64_t *i);
/* PL_get_size_ex(term_t t, size_t *i) moved to pl-error.h */
bool		PL_get_pointer(term_t t, void **ptr);
bool		PL_put_term(term_t t1, term_t t2)/* WUNUSED*/;
bool		PL_get_functor(term_t t, functor_t *f);
bool		PL_get_name_arity_sz(term_t t, atom_t *name,
					     size_t *arity);
bool		PL_get_uintptr(term_t t, size_t *i);
bool		PL_unify_atom(term_t t, atom_t a);
bool		PL_unify_atomic(term_t t, word a);
bool		PL_unify_bool(term_t t, int val);
bool		PL_unify_pointer(term_t t, void *ptr);
bool		PL_unify_float(term_t t, double f);
bool		PL_get_list(term_t l, term_t h, term_t t);
FLI_INLINE bool	PL_is_atom(term_t t);
bool		PL_unify_list(term_t l, term_t h, term_t t);
bool		PL_cons_list(term_t l, term_t head, term_t tail);
bool		PL_cons_list_v(term_t list, size_t count, term_t elems);
bool		PL_is_inf(term_t t);
bool		PL_get_nil(term_t t);
bool		PL_is_list(term_t t);
/* PL_same_term(term_t t1, term_t t2) moved to pl-prims.h */
bool		isUCSAtom(Atom a);
atom_t		lookupUCSAtom(const pl_wchar_t *s, size_t len);
int		charCode(word w);
bool		isCallable(word w);
bool		PL_unify_arg_sz(size_t index, term_t t, term_t a);

void		registerForeignLicenses(void);
void            bindExtensions(const char *module,
			       const PL_extension *ext);
void		initForeign(void);
bool		PL_rethrow(void);
FLI_INLINE int	PL_pending(int sig);
bool		PL_clearsig(int sig);
void		cleanupCodeToAtom(void);
void		PL_clear_foreign_exception(LocalFrame fr);
bool		has_emergency_space(void *sv, size_t needed);
FLI_INLINE
except_class	classify_exception(term_t ex);
except_class    classify_exception_p(Word p);
void		PL_abort_process(void) NORETURN;
void		resetForeign(void);

bool		get_atom_ptr_text(Atom atom, PL_chars_t *text);
bool		get_atom_text(atom_t atom, PL_chars_t *text);
bool		get_string_text(word w, PL_chars_t *text);

#undef LDFUNC_DECLARATIONS

#define PL_strip_module(q, m, t) PL_strip_module_flags(q, m, t, 0)

		 /*******************************
		 *	INLINE DEFINITIONS	*
		 *******************************/

#if USE_FLI_INLINES || EMIT_FLI_INLINES
#include "pl-wam.h"

#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)


FLI_INLINE bool
PL_get_atom(DECL_LD term_t t, atom_t *a)
{ word w = valHandle(t);

  if ( isAtom(w) )
  { *a = (atom_t) w;
    return true;
  }
  return false;
}

FLI_INLINE bool
PL_is_variable(DECL_LD term_t t)
{ word w = valHandle(t);

  return canBind(w);
}

FLI_INLINE bool
PL_is_atom(DECL_LD term_t t)
{ word w = valHandle(t);

  return isTextAtom(w);
}

FLI_INLINE bool
PL_is_attvar(DECL_LD term_t t)
{ word w = valHandle(t);

  return isAttVar(w);
}

FLI_INLINE bool
PL_is_functor(DECL_LD term_t t, functor_t f)
{ word w = valHandle(t);

  return hasFunctor(w, f);
}

FLI_INLINE bool
PL_is_atomic(DECL_LD term_t t)
{ word w = valHandle(t);

  return !!isAtomic(w);
}

FLI_INLINE bool
PL_is_number(DECL_LD term_t t)
{ word w = valHandle(t);

  return isNumber(w);
}

FLI_INLINE bool
PL_put_variable(DECL_LD term_t t)
{ Word p = valTermRef(t);

  setVar(*p);
  return true;
}

FLI_INLINE bool
PL_put_atom(DECL_LD term_t t, atom_t a)
{ setHandle(t, a);
  return true;
}

FLI_INLINE bool
PL_put_int64(DECL_LD term_t t, int64_t i)
{ word w = consInt(i);

  if ( valInt(w) != i &&
       put_int64(&w, i, ALLOW_GC) != true )
    return false;

  setHandle(t, w);
  return true;
}

FLI_INLINE bool
PL_put_integer(DECL_LD term_t t, long i)
{ return PL_put_int64(t, i);
}

FLI_INLINE bool
PL_put_intptr(DECL_LD term_t t, intptr_t i)
{ return PL_put_int64(t, i);
}

FLI_INLINE predicate_t
_PL_predicate(const char *name, int arity, const char *module,
	      predicate_t *bin)
{ if ( !*bin )
    *bin = PL_predicate(name, arity, module);

  return *bin;
}

FLI_INLINE except_class
classify_exception(DECL_LD term_t exception)
{ Word p;

  if ( !exception )
    return EXCEPT_NONE;

  p = valTermRef(exception);
  return classify_exception_p(p);
}

FLI_INLINE int			/* -1, false, true */
PL_pending(DECL_LD int sig)
{ return pendingSignal(LD, sig);
}

#endif /*USE_FLI_INLINES || EMIT_FLI_INLINESS*/

#endif /*_PL_FLI_H*/
