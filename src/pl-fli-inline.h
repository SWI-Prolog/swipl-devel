#ifndef _PL_FLI_INLINE_H
#define _PL_FLI_INLINE_H

#include "pl-codelist.h"
#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)

FLI_INLINE(int)
PL_get_atom__LD(term_t t, atom_t *a ARG_LD)
{ word w = valHandle(t);

  if ( isAtom(w) )
  { *a = (atom_t) w;
    succeed;
  }
  fail;
}

FLI_INLINE(int)
PL_is_variable__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  return canBind(w) ? TRUE : FALSE;
}

FLI_INLINE(int)
PL_is_atom__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  if ( isTextAtom(w) )
    return TRUE;

  return FALSE;
}

FLI_INLINE(int)
PL_is_functor__LD(term_t t, functor_t f ARG_LD)
{ word w = valHandle(t);

  if ( hasFunctor(w, f) )
    succeed;

  fail;
}

FLI_INLINE(int)
PL_is_atomic__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  return !!isAtomic(w);
}

FLI_INLINE(int)
PL_put_variable__LD(term_t t ARG_LD)
{ Word p = valTermRef(t);

  setVar(*p);
  return TRUE;
}

FLI_INLINE(int)
PL_put_atom__LD(term_t t, atom_t a ARG_LD)
{ setHandle(t, a);
  return TRUE;
}

FLI_INLINE(int)
PL_put_int64__LD(term_t t, int64_t i ARG_LD)
{ word w = consInt(i);

  if ( valInt(w) != i &&
       put_int64(&w, i, ALLOW_GC PASS_LD) != TRUE )
    return FALSE;

  setHandle(t, w);
  return TRUE;
}

FLI_INLINE(int)
PL_put_integer__LD(term_t t, long i ARG_LD)
{ return PL_put_int64__LD(t, i PASS_LD);
}

FLI_INLINE(int)
PL_put_intptr__LD(term_t t, intptr_t i ARG_LD)
{ return PL_put_int64__LD(t, i PASS_LD);
}

FLI_INLINE(predicate_t)
_PL_predicate(const char *name, int arity, const char *module,
	      predicate_t *bin)
{ if ( !*bin )
    *bin = PL_predicate(name, arity, module);

  return *bin;
}

FLI_INLINE(except_class)
classify_exception__LD(term_t exception ARG_LD)
{ Word p;

  if ( !exception )
    return EXCEPT_NONE;

  p = valTermRef(exception);
  return classify_exception_p(p);
}

FLI_INLINE(int)
PL_pending__LD(int sig ARG_LD)
{ return pendingSignal(LD, sig);
}

#endif /* _PL_FLI_INLINE_H */
