/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Virtual machine instruction interpreter
*/

/*#define O_SECURE 1*/
/*#define O_DEBUG 1*/
#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog  new-style  foreign-language  interface.   This  new  foreign
interface is a mix of the old  interface using the ideas on term-handles
from  Quintus  Prolog.  Term-handles  are    integers  (unsigned  long),
describing the offset of the term-location relative   to the base of the
local stack.

If a C-function has to  store  intermediate   results,  it  can do so by
creating a new term-reference using   PL_new_term_ref().  This functions
allocates a cell on the local stack and returns the offset.

While a foreign function is on top of  the stack, the local stacks looks
like this:

						      | <-- lTop
	-----------------------------------------------
	| Allocated term-refs using PL_new_term_ref() |
	-----------------------------------------------
	| reserved for #term-refs (1)		      |
	-----------------------------------------------
	| foreign-function arguments (term-refs)      |
	-----------------------------------------------
	| Local frame of foreign function             |
	-----------------------------------------------

On a call-back to Prolog using  PL_call(),  etc., (1) is filled with the
number of term-refs allocated. This  information   (stored  as  a tagged
Prolog int) is used by the garbage collector to update the stack frames.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_SECURE
#define setHandle(h, w)		{ assert(*valTermRef(h) != QID_MAGIC); \
				  (*valTermRef(h) = (w)); \
				}
#else
#define setHandle(h, w)		(*valTermRef(h) = (w))
#endif
#define valHandleP(h)		valTermRef(h)

#undef ulong
#define ulong unsigned long

static inline word
valHandle(term_t r)
{ Word p = valTermRef(r);

  deRef(p);
  return *p;
}


		 /*******************************
		 *	   CREATE/RESET		*
		 *******************************/

#undef PL_new_term_refs
#undef PL_new_term_ref
#undef PL_reset_term_refs

term_t
PL_new_term_refs(int n)
{ Word t = (Word)lTop;
  term_t r = consTermRef(t);

  lTop = (LocalFrame)(t+n);
  verifyStack(local);

  while(n-- > 0)
  { SECURE(assert(*t != QID_MAGIC));
    setVar(*t++);
  }
  
  return r;
}


term_t
PL_new_term_ref()
{ Word t = (Word)lTop;
  term_t r = consTermRef(t);

  lTop = (LocalFrame)(t+1);
  verifyStack(local);
  SECURE(assert(*t != QID_MAGIC));
  setVar(*t);
  
  return r;
}


void
PL_reset_term_refs(term_t r)
{ lTop = (LocalFrame) valTermRef(r);
}


term_t
PL_copy_term_ref(term_t from)
{ Word t   = (Word)lTop;
  term_t r = consTermRef(t);
  Word p2  = valHandleP(from);

  lTop = (LocalFrame)(t+1);
  verifyStack(local);
  deRef(p2);
  *t = isVar(*p2) ? makeRef(p2) : *p2;
  
  return r;
}


		 /*******************************
		 *	       ATOMS		*
		 *******************************/

atom_t
PL_new_atom(const char *s)
{ return (atom_t) lookupAtom((char *)s); /* hack */
}


const char *
PL_atom_chars(atom_t a)
{ return (const char *) stringAtom(a);
}


functor_t
PL_new_functor(atom_t f,  int a)
{ return lookupFunctorDef(f, a);
}


atom_t
PL_functor_name(functor_t f)
{ return (atom_t) f->name;
}


int
PL_functor_arity(functor_t f)
{ return f->arity;
}


		 /*******************************
		 *    QUINTUS WRAPPER SUPPORT   *
		 *******************************/

bool
PL_cvt_i_integer(term_t p, long *c)
{ return PL_get_long(p, c);
}


bool
PL_cvt_i_float(term_t p, double *c)
{ return PL_get_float(p, c);
}


bool
PL_cvt_i_single(term_t p, float *c)
{ double f;

  if ( PL_get_float(p, &f) )
  { *c = (float)f;
    succeed;
  }

  fail;
}


bool
PL_cvt_i_string(term_t p, char **c)
{ return PL_get_chars(p, c, CVT_ATOM|CVT_STRING);
}


bool
PL_cvt_i_atom(term_t p, atom_t *c)
{ return PL_get_atom(p, c);
}


bool
PL_cvt_o_integer(long c, term_t p)
{ return PL_unify_integer(p, c);
}


bool
PL_cvt_o_float(double c, term_t p)
{ return PL_unify_float(p, c);
}


bool
PL_cvt_o_single(float c, term_t p)
{ return PL_unify_float(p, c);
}


bool
PL_cvt_o_string(const char *c, term_t p)
{ return PL_unify_atom_chars(p, c);
}


bool
PL_cvt_o_atom(atom_t c, term_t p)
{ return PL_unify_atom(p, c);
}


		 /*******************************
		 *	      COMPARE		*
		 *******************************/

int
PL_compare(term_t t1, term_t t2)
{ Word p1 = valHandleP(t1);
  Word p2 = valHandleP(t2);

  return compareStandard(p1, p2);	/* -1, 0, 1 */
}


		 /*******************************
		 *	      INTEGERS		*
		 *******************************/

word
makeNum(long i)
{ if ( inTaggedNumRange(i) )
    return consInt(i);

  return globalLong(i);
}


		 /*******************************
		 *	       CONS-*		*
		 *******************************/

void
PL_cons_functor(term_t h, functor_t fd, ...)
{ int arity = fd->arity;

  if ( arity == 0 )
  { setHandle(h, fd->name);
  } else
  { Word a = allocGlobal(1 + arity);
    va_list args;

    va_start(args, fd);
    setHandle(h, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
    *a++ = fd->functor;
    while(arity-- > 0)
    { term_t r = va_arg(args, term_t);
      Word p = valHandleP(r);

      deRef(p);
      *a++ = (isVar(*p) ? makeRef(p) : *p);
    }
    va_end(args);
  }
}


void
PL_cons_list(term_t l, term_t head, term_t tail)
{ Word a = allocGlobal(3);
  Word p;
  
  a[0] = FUNCTOR_dot2->functor;
  p = valHandleP(head);
  deRef(p);
  a[1] = (isVar(*p) ? makeRef(p) : *p);
  p = valHandleP(tail);
  deRef(p);
  a[2] = (isVar(*p) ? makeRef(p) : *p);

  setHandle(l, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
}


		 /*******************************
		 *	      GET-*		*
		 *******************************/

int
PL_get_atom(term_t t, atom_t *a)
{ word w = valHandle(t);

  if ( isAtom(w) )
  { *a = (atom_t) w;
    succeed;
  }
  fail;
}


int
PL_get_atom_chars(term_t t, char **s)
{ word w = valHandle(t);

  if ( isAtom(w) )
  { *s = stringAtom(w);
    succeed;
  }
  fail;
}

#ifdef O_STRING
int
PL_get_string(term_t t, char **s, int *len)
{ word w = valHandle(t);

  if ( isString(w) )
  { *s = valString(w);
    *len = sizeString(w);
    succeed;
  }
  fail;
}
#endif

#define BUFFER_RING_SIZE 4

static buffer	discardable_buffer;
static buffer	buffer_ring[BUFFER_RING_SIZE];
static int	current_buffer_id;

static Buffer
findBuffer(int flags)
{ Buffer b;

  if ( flags & BUF_RING )
  { if ( ++current_buffer_id == BUFFER_RING_SIZE )
      current_buffer_id = 0;
    b = &buffer_ring[current_buffer_id];
  } else
    b = &discardable_buffer;

  emptyBuffer(b);
  return b;
}


static int
unfindBuffer(int flags)
{ if ( flags & BUF_RING )
  { if ( --current_buffer_id <= 0 )
      current_buffer_id = BUFFER_RING_SIZE-1;
  }

  fail;
}


static char *
malloc_string(char *s)
{ char *c;
  int len = strlen(s)+1;

  if ( (c = malloc(len)) )
  { memcpy(c, s, len);
    return c;
  }

  sysError("Not enough memory");
  return NULL;
}


int
PL_get_list_chars(term_t l, char **s, unsigned flags)
{ Buffer b = findBuffer(flags);
  word list = valHandle(l);
  Word arg, tail;
  int c;
  char *r;

  while( isList(list) && !isNil(list) )
  { arg = argTermP(list, 0);
    deRef(arg);
    if ( isTaggedInt(*arg) && (c=(int)valInt(*arg)) > 0 && c < 256)
    { addBuffer(b, c, char);
      tail = argTermP(list, 1);
      deRef(tail);
      list = *tail;
      continue;
    }
    return unfindBuffer(flags);
  }
  if (!isNil(list))
    return unfindBuffer(flags);

  addBuffer(b, EOS, char);
  r = baseBuffer(b, char);

  if ( flags & BUF_MALLOC )
    *s = malloc_string(r);
  else
    *s = r;

  succeed;
}


int
PL_get_chars(term_t l, char **s, unsigned flags)
{ word w = valHandle(l);
  static char tmp[24];
  char *r;
  int type;

  if ( (flags & CVT_ATOM) && isAtom(w) )
  { type = PL_ATOM;
    r = stringAtom(w);
  } else if ( (flags & CVT_INTEGER) && isInteger(w) )
  { type = PL_INTEGER;
    Ssprintf(tmp, "%ld", valInteger(w) );
    r = tmp;
  } else if ( (flags & CVT_FLOAT) && isReal(w) )
  { type = PL_FLOAT;
    Ssprintf(tmp, "%f", valReal(w) );
    r = tmp;
#ifdef O_STRING
  } else if ( (flags & CVT_STRING) && isString(w) )
  { type = PL_STRING;
    r = valString(w);
#endif
  } else if ( (flags & CVT_LIST) )
  { return PL_get_list_chars(l, s, flags);
  } else if ( (flags & CVT_VARIABLE) )
  { type = PL_VARIABLE;
    r = varName(l);
  } else
    fail;
    
  if ( flags & BUF_MALLOC )
  { *s = malloc_string(r);
  } else if ( (flags & BUF_RING && type != PL_ATOM) ||
	      (type == PL_STRING) )
  { Buffer b = findBuffer(flags);
    int l = strlen(r) + 1;

    addMultipleBuffer(b, r, l, char);
    *s = baseBuffer(b, char);
  } else
    *s = r;

  succeed;
}


int
PL_get_integer(term_t t, int *i)
{ word w = valHandle(t);
  
  if ( isTaggedInt(w) )
  { *i = valInt(w);
    succeed;
  }
  if ( isBignum(w) )
  { *i = valBignum(w);
    succeed;
  }
  if ( isReal(w) )
  { real f = valReal(w);
    long l = (long) f;
    
    if ( (real)l == f )
    { *i = l;
      succeed;
    }
  }
  fail;
} 


int
PL_get_long(term_t t, long *i)
{ word w = valHandle(t);
  
  if ( isTaggedInt(w) )
  { *i = valInt(w);
    succeed;
  }
  if ( isBignum(w) )
  { *i = valBignum(w);
    succeed;
  }
  if ( isReal(w) )
  { real f = valReal(w);
    long l = (long) f;
    
    if ( (real)l == f )
    { *i = l;
      succeed;
    }
  }
  fail;
} 


int
PL_get_float(term_t t, double *f)
{ word w = valHandle(t);
  
  if ( isReal(w) )
  { *f = valReal(w);
    succeed;
  }
  if ( isTaggedInt(w) )
  { *f = (double) valInt(w);
    succeed;
  }
  if ( isBignum(w) )
  { *f = (double) valBignum(w);
    succeed;
  }
  fail;
}


int
PL_get_pointer(term_t t, void **ptr)
{ word w = valHandle(t);
  
  if ( isInteger(w) )
  { ulong p;

    if ( isTaggedInt(w) )
    { if ( w == consInt(0) )
      { p = 0L;
      } else 
      { p  = valInt(w) * sizeof(int) + base_addresses[STG_HEAP];
      }
    } else
    { p = valBignum(w);
    }

    *ptr = (void *)p;

    succeed;
  }

  fail;
} 



int
PL_get_name_arity(term_t t, atom_t *name, int *arity)
{ word w = valHandle(t);

  if ( isTerm(w) )
  { FunctorDef fd = functorTerm(w);

    *name =  fd->name;
    *arity = fd->arity;
    succeed;
  }
  if ( isAtom(w) )
  { *name = (atom_t)w;
    *arity = 0;
    succeed;
  }

  fail;
}


int
PL_get_functor(term_t t, functor_t *f)
{ word w = valHandle(t);

  if ( isTerm(w) )
  { *f = functorTerm(w);
    succeed;
  }
  if ( isAtom(w) )
  { *f = lookupFunctorDef(w, 0);
    succeed;
  }

  fail;
}


int
PL_get_module(term_t t, module_t *m)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  { *m = lookupModule(a);
    succeed;
  }

  fail;
}


int
PL_get_arg(int index, term_t t, term_t a)
{ word w = valHandle(t);

  if ( isTerm(w) && index > 0 )
  { Functor f = (Functor)valPtr(w);
    int arity = arityFunctor(f->definition);

    if ( --index < arity )
    { Word p = &f->arguments[index];

      deRef(p);

      if ( isVar(*p) )
	w = consPtr(p, TAG_REFERENCE|storage(w)); /* makeRef() */
      else
	w = *p;

      setHandle(a, w);
      succeed;
    }
  }

  fail;
}


int
PL_get_list(term_t l, term_t h, term_t t)
{ word w = valHandle(l);

  if ( isList(w) )
  { Word p1, p2;
    
    p1 = argTermP(w, 0);
    p2 = argTermP(w, 1);
    deRef(p1);
    deRef(p2);
    setHandle(h, isVar(*p1) ? makeRef(p1) : *p1);
    setHandle(t, isVar(*p2) ? makeRef(p2) : *p2);
    succeed;
  }
  fail;
}


int
PL_get_head(term_t l, term_t h)
{ word w = valHandle(l);

  if ( isList(w) )
  { Word p;
    
    p = argTermP(w, 0);
    deRef(p);
    setHandle(h, *p ? *p : makeRef(p));
    succeed;
  }
  fail;
}


int
PL_get_tail(term_t l, term_t t)
{ word w = valHandle(l);

  if ( isList(w) )
  { Word p;
    
    p = argTermP(w, 1);
    deRef(p);
    setHandle(t, *p ? *p : makeRef(p));
    succeed;
  }
  fail;
}


int
PL_get_nil(term_t l)
{ word w = valHandle(l);

  if ( isNil(w) )
    succeed;

  fail;
}


int
_PL_get_xpce_reference(term_t t, xpceref_t *ref)
{ word w = valHandle(t);

  if ( hasFunctor(w, FUNCTOR_xpceref1) )
  { Word p = argTermP(w, 0);

    do
    { if ( isTaggedInt(*p) )
      { ref->type    = PL_INTEGER;
	ref->value.i = valInt(*p);

	succeed;
      } 
      if ( isAtom(*p) )
      { ref->type    = PL_ATOM;
	ref->value.a = (atom_t) *p;

	succeed;
      }
      if ( isBignum(*p) )
      { ref->type    = PL_INTEGER;
	ref->value.i = valBignum(*p);

	succeed;
      }
    } while(isRef(*p) && (p = unRef(*p)));

    return -1;				/* error! */
  }

  fail;
}


		 /*******************************
		 *		IS-*		*
		 *******************************/

int
PL_is_variable(term_t t)
{ word w = valHandle(t);

  return isVar(w) ? TRUE : FALSE;
}


int
PL_is_atom(term_t t)
{ word w = valHandle(t);

  return isAtom(w) ? TRUE : FALSE;
}


int
PL_is_integer(term_t t)
{ word w = valHandle(t);

  return isInteger(w) ? TRUE : FALSE;
}


int
PL_is_float(term_t t)
{ word w = valHandle(t);

  return isReal(w) ? TRUE : FALSE;
}


int
PL_is_compound(term_t t)
{ word w = valHandle(t);

  return isTerm(w) ? TRUE : FALSE;
}


int
PL_is_functor(term_t t, functor_t f)
{ word w = valHandle(t);

  if ( hasFunctor(w, f) )
    succeed;

  fail;
}


int
PL_is_list(term_t t)
{ word w = valHandle(t);

  return (isList(w) || isNil(w)) ? TRUE : FALSE;
}


int
PL_is_atomic(term_t t)
{ word w = valHandle(t);

  return isAtomic(w) ? TRUE : FALSE;
}


int
PL_is_number(term_t t)
{ word w = valHandle(t);

  return isNumber(w) ? TRUE : FALSE;
}


#ifdef O_STRING
int
PL_is_string(term_t t)
{ word w = valHandle(t);

  return isString(w) ? TRUE : FALSE;
}

int
PL_unify_string_chars(term_t t, const char *s)
{ word str = globalString((char *)s);
  Word p = valHandleP(t);

  return unifyAtomic(p, str);
}

int
PL_unify_string_nchars(term_t t, int len, const char *s)
{ word str = globalNString(len, (char *)s);
  Word p = valHandleP(t);

  return unifyAtomic(p, str);
}

#endif

		 /*******************************
		 *             PUT-*  		*
		 *******************************/

void
PL_put_variable(term_t t)
{ Word p = allocGlobal(1);

  setVar(*p);
  setHandle(t, makeRef(p));
}


void
PL_put_atom(term_t t, atom_t a)
{ setHandle(t, a);
}


void
PL_put_atom_chars(term_t t, const char *s)
{ setHandle(t, lookupAtom(s));
}


void
PL_put_string_chars(term_t t, const char *s)
{ word w = globalString(s);

  setHandle(t, w);
}

void
PL_put_list_chars(term_t t, const char *chars)
{ int len = strlen(chars);
  
  if ( len == 0 )
  { setHandle(t, ATOM_nil);
  } else
  { Word p = allocGlobal(len*3);
    setHandle(t, consPtr(p, TAG_COMPOUND|STG_GLOBAL));

    for( ; *chars ; chars++)
    { *p++ = FUNCTOR_dot2->functor;
      *p++ = consInt((long)*chars & 0xff);
      *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);
      p++;
    }
    p[-1] = ATOM_nil;
  }
}

void
PL_put_integer(term_t t, long i)
{ setHandle(t, makeNum(i));
}


void
_PL_put_number(term_t t, Number n)
{ if ( intNumber(n) )
    PL_put_integer(t, n->value.i);
  else
    PL_put_float(t, n->value.f);
}


static ulong
pointerToLong(void *ptr)
{ ulong p = (ulong) ptr;

  if ( p > base_addresses[STG_HEAP] &&
       p % sizeof(int) == 0 )
  { p -= base_addresses[STG_HEAP];
    p /= sizeof(int);
    
    if ( p <= PLMAXTAGGEDINT )
      return p;
  }

  return (ulong)ptr;
}


void
PL_put_pointer(term_t t, void *ptr)
{ PL_put_integer(t, pointerToLong(ptr));
}


void
PL_put_float(term_t t, double f)
{ setHandle(t, globalReal(f));
}


void
PL_put_functor(term_t t, functor_t f)
{ int arity = f->arity;

  if ( arity == 0 )
  { setHandle(t, f->name);
  } else
  { Word a = allocGlobal(1 + arity);

    setHandle(t, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
    *a++ = f->functor;
    while(arity-- > 0)
      setVar(*a++);
  }
}


void
PL_put_list(term_t l)
{ Word a = allocGlobal(3);

  setHandle(l, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
  *a++ = FUNCTOR_dot2->functor;
  setVar(*a++);
  setVar(*a);
}


void
PL_put_nil(term_t l)
{ setHandle(l, ATOM_nil);
}


void
PL_put_term(term_t t1, term_t t2)
{ Word p2 = valHandleP(t2);

  deRef(p2);
  setHandle(t1, isVar(*p2) ? makeRef(p2) : *p2);
}


void
_PL_put_xpce_reference_i(term_t t, unsigned long r)
{ Word a = allocGlobal(2);

  setHandle(t, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
  *a++ = FUNCTOR_xpceref1->functor;
  *a++ = makeNum(r);
}


void
_PL_put_xpce_reference_a(term_t t, atom_t name)
{ Word a = allocGlobal(2);

  setHandle(t, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
  *a++ = FUNCTOR_xpceref1->functor;
  *a++ = name;
}


		 /*******************************
		 *	       UNIFY		*
		 *******************************/

int
PL_unify_atom(term_t t, atom_t a)
{ Word p = valHandleP(t);

  return unifyAtomic(p, a);
}


int
PL_unify_functor(term_t t, functor_t f)
{ Word p = valHandleP(t);

  return unifyFunctor(p, f);
}


int
PL_unify_atom_chars(term_t t, const char *chars)
{ Word p = valHandleP(t);

  return unifyAtomic(p, lookupAtom((char *)chars));
}


int
PL_unify_list_chars(term_t t, const char *chars)
{ term_t head = PL_new_term_ref();

  for( ; *chars; chars++ )
  { if ( !PL_unify_list(t, head, t) ||
	 !PL_unify_integer(head, (int)*chars & 0xff) )
      fail;
  }

  return PL_unify_nil(t);
}


int
PL_unify_integer(term_t t, long i)
{ Word p = valHandleP(t);

  return unifyAtomic(p, makeNum(i));
}


int
_PL_unify_number(term_t t, Number n)
{ if ( intNumber(n) )
    return PL_unify_integer(t, n->value.i);
  else
    return PL_unify_float(t, n->value.f);
}


int
PL_unify_pointer(term_t t, void *ptr)
{ return PL_unify_integer(t, pointerToLong(ptr));
}


int
PL_unify_float(term_t t, double f)
{ word w = globalReal(f);
  Word p = valHandleP(t);

  return unifyAtomic(p, w);
}


int
PL_unify_arg(int index, term_t t, term_t a)
{ word w = valHandle(t);

  if ( isTerm(w) && index > 0 && index <= functorTerm(w)->arity )
  { Word p = argTermP(w, index-1);
    Word p2 = valHandleP(a);

    return unify_ptrs(p, p2);
  }

  fail;
}


int					/* can be faster! */
PL_unify_list(term_t l, term_t h, term_t t)
{ if ( PL_unify_functor(l, FUNCTOR_dot2) )
  { PL_get_list(l, h, t);

    succeed;
  }

  fail;
}


int
PL_unify_nil(term_t l)
{ Word p = valHandleP(l);

  return unifyAtomic(p, ATOM_nil);
}


int
_PL_unify_xpce_reference(term_t t, xpceref_t *ref)
{ Word p = valHandleP(t);

  do
  { if ( isVar(*p) )
    { Word a = allocGlobal(2);
  
      *p = consPtr(a, TAG_COMPOUND|STG_GLOBAL);
      DoTrail(p);
      *a++ = FUNCTOR_xpceref1->functor;
      if ( ref->type == PL_INTEGER )
	*a++ = makeNum(ref->value.i);
      else
	*a++ = ref->value.a;
  
      succeed;
    } 
    if ( hasFunctor(*p, FUNCTOR_xpceref1) )
    { Word a = argTermP(*p, 0);
      word v = (ref->type == PL_INTEGER ? makeNum(ref->value.i)
					: ref->value.a);
  
      deRef(a);
      return unifyAtomic(a, v);
    }
  } while ( isRef(*p) && (p = unRef(*p)));

  fail;
}


		 /*******************************
		 *       ATOMIC (INTERNAL)	*
		 *******************************/

atomic_t
_PL_get_atomic(term_t t)
{ return valHandle(t);
}


int
_PL_unify_atomic(term_t t, atomic_t a)
{ Word p = valHandleP(t);

  return unifyAtomic(p, a);
}


void
_PL_put_atomic(term_t t, atomic_t a)
{ setHandle(t, a);
}


void
_PL_copy_atomic(term_t t, atomic_t arg) /* internal one */
{ word a;

  if ( isIndirect(arg) )
    a = globalIndirect(arg);
  else
    a = arg;
  
  setHandle(t, a);
}


		 /*******************************
		 *	       TYPE		*
		 *******************************/


int
PL_term_type(term_t t)
{ word w = valHandle(t);

  if ( isVar(w) )		return PL_VARIABLE;
  if ( isInteger(w) )		return PL_INTEGER;
  if ( isReal(w) )		return PL_FLOAT;
#if O_STRING
  if ( isString(w) )		return PL_STRING;
#endif /* O_STRING */
  if ( isAtom(w) )		return PL_ATOM;

  assert(isTerm(w));
  				return PL_TERM;
}

		 /*******************************
		 *	      UNIFY		*
		 *******************************/

int
PL_unify(term_t t1, term_t t2)
{ Word p1 = valHandleP(t1);
  Word p2 = valHandleP(t2);
  mark m;
  int rval;

  Mark(m);
  if ( !(rval = unify(p1, p2, environment_frame)) )
    Undo(m);

  return rval;  
}


		 /*******************************
		 *	       MODULES		*
		 *******************************/

int
PL_strip_module(term_t raw, module_t *m, term_t plain)
{ Word r = valHandleP(raw);
  Word p;

  if ( (p = stripModule(r, m)) )
  { setHandle(plain, isVar(*p) ? makeRef(p) : *p);
    succeed;
  }

  fail;
}

		/********************************
		*            MODULES            *
		*********************************/

module_t
PL_context()
{ return environment_frame ? contextModule(environment_frame)
			   : MODULE_user;
}

atom_t
PL_module_name(Module m)
{ return (atom_t) m->name;
}

module_t
PL_new_module(atom_t name)
{ return lookupModule(name);
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

predicate_t
PL_pred(functor_t functor, module_t module)
{ if ( module == NULL )
    module = PL_context();

  return lookupProcedure(functor, module);
}


predicate_t
PL_predicate(const char *name, int arity, const char *module)
{ Module m = module ? lookupModule(lookupAtom(module)) : PL_context();
  FunctorDef f = lookupFunctorDef(lookupAtom(name), arity);

  return PL_pred(f, m);
}


int
PL_predicate_info(predicate_t pred, atom_t *name, int *arity, module_t *m)
{ if ( pred->type == PROCEDURE_TYPE )
  { *name  = pred->definition->functor->name;
    *arity = pred->definition->functor->arity;
    *m     = pred->definition->module;

    succeed;
  }

  fail;
}

		 /*******************************
		 *	       CALLING		*
		 *******************************/

int
PL_call_predicate(Module ctx, int debug, predicate_t pred, term_t h0)
{ int rval;

  qid_t qid = PL_open_query(ctx, debug, pred, h0);
  rval = PL_next_solution(qid);
  PL_cut_query(qid);

  return rval;
}


bool
PL_call(term_t t, Module m)
{ return callProlog(m, t, TRUE);
}  


		/********************************
		*   UNDETERMINISTIC FOREIGNS    *
		********************************/

foreign_t
_PL_retry(long v)
{ ForeignRedoInt(v);
}


foreign_t
_PL_retry_address(void *v)
{ if ( (ulong)v & FRG_CONTROL_MASK )
    PL_fatal_error("PL_retry_address(0x%lx): bad alignment", (ulong)v);

  ForeignRedoPtr(v);
}


long
PL_foreign_context(control_t h)
{ return ForeignContextInt(h);
}

void *
PL_foreign_context_address(control_t h)
{ return ForeignContextPtr(h);
}


int
PL_foreign_control(control_t h)
{ return ForeignControl(h);
}


		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

static void
notify_registered_foreign(FunctorDef fd, Module m)
{ if ( status.initialised )
  { fid_t cid = PL_open_foreign_frame();
    term_t argv = PL_new_term_refs(2);
    static predicate_t pred;

    if ( !pred )
      pred = PL_predicate("$foreign_registered", 2, "system");

    PL_put_atom(argv+0, m->name);
    PL_put_functor(argv+1, fd);
    PL_call_predicate(MODULE_system, FALSE, pred, argv);
    PL_discard_foreign_frame(cid);
  }
}


bool
PL_register_foreign(const char *name, int arity, Func f, int flags)
{ Procedure proc;
  Definition def;
  Module m;
  FunctorDef fdef = lookupFunctorDef(lookupAtom(name), arity);

  m = (environment_frame ? contextModule(environment_frame)
			 : MODULE_system);

  proc = lookupProcedure(lookupFunctorDef(lookupAtom(name), arity), m);
  def = proc->definition;

  if ( true(def, LOCKED) )
  { warning("PL_register_foreign(): Attempt to redefine a system predicate: %s",
	    procedureName(proc));
    fail;
  }

  if ( def->definition.function )
    warning("PL_register_foreign(): redefined %s", procedureName(proc));
  if ( false(def, FOREIGN) && def->definition.clauses != NULL )
    abolishProcedure(proc, m);

  def->definition.function = f;
  def->indexPattern = 0;
  def->indexCardinality = 0;
  def->flags = 0;
  set(def, FOREIGN|TRACE_ME);
  clear(def, NONDETERMINISTIC);

  if ( (flags & PL_FA_NOTRACE) )	  clear(def, TRACE_ME);
  if ( (flags & PL_FA_TRANSPARENT) )	  set(def, TRANSPARENT);
  if ( (flags & PL_FA_NONDETERMINISTIC) ) set(def, NONDETERMINISTIC);

  notify_registered_foreign(fdef, m);

  succeed;
}  


bool
PL_load_extensions(PL_extension *ext)
{ PL_extension *e;
  Module m;

  m = (environment_frame ? contextModule(environment_frame)
			 : MODULE_system);

  for(e = ext; e->predicate_name; e++)
  { short flags = TRACE_ME;
    register Definition def;
    register Procedure proc;

    if ( e->flags & PL_FA_NOTRACE )	     flags &= ~TRACE_ME;
    if ( e->flags & PL_FA_TRANSPARENT )	     flags |= TRANSPARENT;
    if ( e->flags & PL_FA_NONDETERMINISTIC ) flags |= NONDETERMINISTIC;

    proc = lookupProcedure(lookupFunctorDef(lookupAtom(e->predicate_name),
					    e->arity), 
			   m);
    def = proc->definition;
    if ( true(def, LOCKED) )
    { warning("PL_load_extensions(): Attempt to redefine system predicate: %s",
	      procedureName(proc));
      continue;
    }
    if ( def->definition.function )
      warning("PL_load_extensions(): redefined %s", procedureName(proc));
    if ( false(def, FOREIGN) && def->definition.clauses != NULL )
      abolishProcedure(proc, m);
    set(def, FOREIGN);
    set(def, flags);
    def->definition.function = e->function;
    def->indexPattern = 0;
    def->indexCardinality = 0;

    notify_registered_foreign(def->functor, m);
  }    

  succeed;
}

		 /*******************************
		 *	 EMBEDDING PROLOG	*
		 *******************************/

int
PL_toplevel(void)
{ return prolog(lookupAtom("$toplevel"));
}


void
PL_halt(int status)
{ Halt(status);
}


		/********************************
		*            SIGNALS            *
		*********************************/

#if HAVE_SIGNAL
void
(*PL_signal(int sig, void (*func) (int)))(int)
{ void (*old)(int);

  if ( sig < 0 || sig >= MAXSIGNAL )
  { fatalError("PL_signal(): illegal signal number: %d", sig);
    return NULL;
  }

  if ( signalHandlers[sig].catched == FALSE )
  { old = signal(sig, func);
    signalHandlers[sig].os = func;
    
    return old;
  }

  old = signalHandlers[sig].user;
  signalHandlers[sig].user = func;

  return old;
}
#endif


		/********************************
		*         RESET (ABORTS)	*
		********************************/

typedef struct abort_handle * AbortHandle;

static struct abort_handle
{ AbortHandle	  next;			/* Next handle */
  PL_abort_hook_t function;		/* The handle itself */
} * abort_head = NULL,
  * abort_tail = NULL;


void
PL_abort_hook(PL_abort_hook_t func)
{ AbortHandle h = (AbortHandle) allocHeap(sizeof(struct abort_handle));
  h->next = NULL;
  h->function = func;

  if ( abort_head == NULL )
  { abort_head = abort_tail = h;
  } else
  { abort_tail->next = h;
    abort_tail = h;
  }
}


int
PL_abort_unhook(PL_abort_hook_t func)
{ AbortHandle h = abort_head;

  for(; h; h = h->next)
  { if ( h->function == func )
    { h->function = NULL;
      return TRUE;
    }
  }

  return FALSE;
}


void
resetForeign(void)
{ AbortHandle h = abort_head;

  for(; h; h = h->next)
    if ( h->function )
      (*h->function)();
}


		/********************************
		*      REINITIALISE (SAVE)	*
		********************************/

typedef struct reinit_handle * ReinitHandle;

static struct reinit_handle
{ ReinitHandle	  next;			/* Next handle */
  PL_reinit_hook_t function;		/* The handle itself */
} * reinit_head = NULL,
  * reinit_tail = NULL;


void
PL_reinit_hook(PL_reinit_hook_t func)
{ ReinitHandle h = reinit_head;

  for(; h; h = h->next)
  { if ( h->function == func )
      return;				/* already there */
  }

  h = (ReinitHandle) allocHeap(sizeof(struct reinit_handle));

  h->next = NULL;
  h->function = func;

  if ( reinit_head == NULL )
  { reinit_head = reinit_tail = h;
  } else
  { reinit_tail->next = h;
    reinit_tail = h;
  }
}


int
PL_reinit_unhook(PL_reinit_hook_t func)
{ ReinitHandle h = reinit_head;

  for(; h; h = h->next)
  { if ( h->function == func )
    { h->function = NULL;
      return TRUE;
    }
  }

  return FALSE;
}


void
reinitForeign(int argc, char **argv)
{ ReinitHandle h = reinit_head;

  for(; h; h = h->next)
    (*h->function)(argc, argv);
}


		 /*******************************
		 *	      PROMPT		*
		 *******************************/

extern int prompt_next;

void
PL_prompt1(const char *s)
{ prompt1((char *) s);
}


int
PL_ttymode(int fd)
{ if ( fd == 0 )
  { if ( status.notty )			/* -tty in effect */
      return PL_NOTTY;
    if ( ttymode == TTY_RAW )		/* get_single_char/1 and friends */
      return PL_RAWTTY;
    return PL_COOKEDTTY;		/* cooked (readline) input */
  } else
    return PL_NOTTY;
}


void
PL_write_prompt(int fd, int dowrite)
{ if ( fd == 0 )
  { if ( dowrite )
    { extern int Output;
      int old = Output;
      Output = 1;
      Putf("%s", PrologPrompt());
      pl_flush();
      Output = old;
    }

    pl_ttyflush();
    prompt_next = FALSE;
  }
}


void
PL_prompt_next(int fd)
{ if ( fd == 0 )
    prompt_next = TRUE;
}


char *
PL_prompt_string(int fd)
{ if ( fd == 0 )
    return PrologPrompt();

  return "";
}


void
PL_add_to_protocol(const char *buf, int n)
{ protocol((char *)buf, n);
}


		 /*******************************
		 *	   DISPATCHING		*
		 *******************************/

static PL_dispatch_hook_t dispatch_events = NULL;

PL_dispatch_hook_t
PL_dispatch_hook(PL_dispatch_hook_t hook)
{ PL_dispatch_hook_t old = dispatch_events;

  dispatch_events = hook;
  return old;
}

int
PL_dispatch(int fd, int wait)
{ int rval;

  if ( wait == PL_DISPATCH_INSTALLED )
    return dispatch_events ? TRUE : FALSE;

  if ( dispatch_events )
  { do
    { rval = (*dispatch_events)(fd);
    } while( wait == PL_DISPATCH_WAIT && rval == PL_DISPATCH_TIMEOUT );
  } else
    rval = PL_DISPATCH_INPUT;

  return rval;
}


		 /*******************************
		 *	    FEATURES		*
		 *******************************/

int
PL_set_feature(const char *name, int type, ...)
{ va_list args;
  int rval = TRUE;

  va_start(args, type);
  switch(type)
  { case PL_ATOM:
    { char *v = va_arg(args, char *);
      setFeature(lookupAtom(name), lookupAtom(v));
      break;
    }
    case PL_INTEGER:
    { int v = va_arg(args, int);
      setFeature(lookupAtom(name), makeNum(v));
      break;
    }
    default:
      rval = FALSE;
  }

  va_end(args);
  return rval;
}


		/********************************
		*           WARNINGS            *
		*********************************/

bool
PL_warning(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vwarning(fm, args);
  va_end(args);

  fail;
}

void
PL_fatal_error(const char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vfatalError(fm, args);
  va_end(args);
}


		/********************************
		*            ACTIONS            *
		*********************************/

bool
PL_action(int action, void *arg)
{ switch(action)
  { case PL_ACTION_TRACE:
      return (bool) pl_trace();
    case PL_ACTION_DEBUG:
      return (bool) pl_debug();
    case PL_ACTION_BACKTRACE:
#ifdef O_DEBUGGER
      backTrace(environment_frame, (int) arg);
      succeed;
#else
      warning("No Prolog backtrace in runtime version");
      fail;
#endif
    case PL_ACTION_BREAK:
      return (bool) pl_break();
    case PL_ACTION_HALT:
      Halt((int) arg);
      fail;				/* should not happen */
    case PL_ACTION_ABORT:
      return (bool) pl_abort();
    case PL_ACTION_SYMBOLFILE:
      loaderstatus.symbolfile = lookupAtom((char *) arg);
      succeed;
    case PL_ACTION_WRITE:
      Putf("%s", (char *)arg);
      succeed;
    case PL_ACTION_FLUSH:
      pl_flush();
      succeed;
    default:
      sysError("PL_action(): Illegal action: %d", action);
      /*NOTREACHED*/
      fail;
  }
}

		/********************************
		*         QUERY PROLOG          *
		*********************************/

static int c_argc = -1;
static char **c_argv;

static void
init_c_args()
{ if ( c_argc == -1 )
  { int i;

    c_argv = alloc_heap(mainArgc * sizeof(char *));
    c_argv[0] = mainArgv[0];
    c_argc = 1;

    for(i=1; i<mainArgc; i++)
    { if ( mainArgv[i][0] == '-' )
      { switch(mainArgv[i][1])
	{ case 'x':
	  case 'g':
	  case 'd':
	  case 'f':
	  case 't':
	    i++;
	    continue;
	  case 'B':
	  case 'L':
	  case 'G':
	  case 'O':
	  case 'T':
	  case 'A':
	    continue;
	}
      }
      c_argv[c_argc++] = mainArgv[i];
    }
  }
}


long
PL_query(int query)
{ switch(query)
  { case PL_QUERY_ARGC:
      init_c_args();
      return (long) c_argc;
    case PL_QUERY_ARGV:
      init_c_args();
      return (long) c_argv;
    case PL_QUERY_SYMBOLFILE:
      if ( !getSymbols() )
	return (long) NULL;
      return (long) stringAtom(loaderstatus.symbolfile);
    case PL_QUERY_ORGSYMBOLFILE:
      if ( getSymbols() == FALSE )
	return (long) NULL;
      return (long) stringAtom(loaderstatus.orgsymbolfile);
    case PL_QUERY_MAX_INTEGER:
      return PLMAXINT;
    case PL_QUERY_MIN_INTEGER:
      return PLMININT;
    case PL_QUERY_MAX_TAGGED_INT:
      return PLMAXTAGGEDINT;
    case PL_QUERY_MIN_TAGGED_INT:
      return PLMINTAGGEDINT;
    case PL_QUERY_GETC:
      PopTty(&ttytab);			/* restore terminal mode */
      return (long) Sgetchar();		/* normal reading */
    default:
      sysError("PL_query: Illegal query: %d", query);
      /*NOTREACHED*/
      fail;
  }
}

