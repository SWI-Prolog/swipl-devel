/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*#define O_SECURE 1*/
/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-ctype.h"
#include <errno.h>

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
valHandle__LD(term_t r ARG_LD)
{ Word p = valTermRef(r);

  deRef(p);
  return *p;
}

#define valHandle(r) valHandle__LD(r PASS_LD)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deduce the value to store a copy of the  contents of p. This is a *very*
frequent operation. There  are  a  couple   of  options  to  realise it.
Basically, we can choose between simple  dereferencing and returning the
value or create a new reference.  In the latter case, we are a bit unlucky,
as we could also have returned the last reference.

Second, we can opt  for  inlining  or   not.  Especially  in  the latter
variation, which is a bit longer, a function might actually be faster.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
linkVal__LD(Word p ARG_LD)
{ word w = *p;

  if ( needsRef(w) )
    return makeRef(p);

  while( isRef(w) )
  { p = unRef(w);
    if ( needsRef(*p) )
      return w;
    w = *p;
  }

  return w;
}

term_t
wordToTermRef(Word p)
{ GET_LD

  if ( p > (Word) lBase )
    return p - (Word)lBase;
  else
  { term_t t = PL_new_term_ref();

    setHandle(t, linkVal(p));
    return t;
  }
}


		 /*******************************
		 *	   CREATE/RESET		*
		 *******************************/

term_t
PL_new_term_refs__LD(int n ARG_LD)
{ Word t;
  term_t r;
  int i;

  requireStack(local, sizeof(word)*n);
  t = (Word)lTop;
  r = consTermRef(t);

  for(i=0; i<n; i++)
    setVar(*t++);
  lTop = (LocalFrame)t;
  fli_context->size += n;

  return r;
}


term_t
PL_new_term_ref__LD(ARG1_LD)
{ Word t;
  term_t r;
  FliFrame fr;

  requireStack(local, sizeof(word));
  t = (Word)lTop;
  r = consTermRef(t);
  SECURE(assert(*t != QID_MAGIC));
  setVar(*t);

  lTop = (LocalFrame)(t+1);
  fr = fli_context;
  fr->size++;
  SECURE({ int s = (Word) lTop - (Word)(fr+1);
	   assert(s == fr->size);
	 });

  return r;
}


#undef PL_new_term_ref
#undef PL_new_term_refs

term_t
PL_new_term_refs(int n)
{ GET_LD

  return PL_new_term_refs__LD(n PASS_LD);
}


term_t
PL_new_term_ref()
{ GET_LD

  return PL_new_term_ref__LD(PASS_LD1);
}

#define PL_new_term_ref()	PL_new_term_ref__LD(PASS_LD1)
#define PL_new_term_refs(n)	PL_new_term_refs__LD(n PASS_LD)


void
PL_reset_term_refs(term_t r)
{ GET_LD
  FliFrame fr = fli_context;

  lTop = (LocalFrame) valTermRef(r);
  fr->size = (Word) lTop - (Word)addPointer(fr, sizeof(struct fliFrame));
  SECURE(if ( fr->size < 0 || fr->size > 100 )
	   Sdprintf("Suspect foreign frame size: %d\n", fr->size));
}


term_t
PL_copy_term_ref(term_t from)
{ GET_LD
  Word t, p2;
  term_t r;
  FliFrame fr;

  requireStack(local, sizeof(word));

  t  = (Word)lTop;
  r  = consTermRef(t);
  p2 = valHandleP(from);

  *t = linkVal(p2);
  lTop = (LocalFrame)(t+1);
  fr = fli_context;
  fr->size++;
  SECURE({ int s = (Word) lTop - (Word)(fr+1);
	   assert(s == fr->size);
	 });

  return r;
}

		 /*******************************
		 *	    UNIFICATION		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unifyAtomic(p, a) unifies a term, represented by  a pointer to it, with
an atomic value. It is intended for foreign language functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
unifyAtomic(term_t t, word w ARG_LD)
{ Word p = valHandleP(t);

  for(;;)
  { if ( canBind(*p) )
    { bindConst(p, w);
      succeed;
    }

    if ( isRef(*p) )
    { p = unRef(*p);
      continue;
    }

    if ( *p == w )
      succeed;

    if ( isIndirect(w) && isIndirect(*p) )
      return equalIndirect(w, *p);

    fail;
  }
}

		 /*******************************
		 *	       ATOMS		*
		 *******************************/

atom_t
PL_new_atom(const char *s)
{ if ( !GD->initialised )
    initAtoms();

  return (atom_t) lookupAtom(s, strlen(s));
}


atom_t
PL_new_atom_nchars(unsigned int len, const char *s)
{ if ( !GD->initialised )
    initAtoms();

  return (atom_t) lookupAtom(s, len);
}


const char *
PL_atom_chars(atom_t a)
{ return (const char *) stringAtom(a);
}


const char *
PL_atom_nchars(atom_t a, unsigned int *len)
{ Atom x = atomValue(a);

  if ( len )
    *len = x->length;

  return x->name;
}


functor_t
PL_new_functor(atom_t f,  int a)
{ return lookupFunctorDef(f, a);
}


atom_t
PL_functor_name(functor_t f)
{ return nameFunctor(f);
}


int
PL_functor_arity(functor_t f)
{ return arityFunctor(f);
}


		 /*******************************
		 *    WIDE CHARACTER SUPPORT	*
		 *******************************/

static PL_blob_t ucs_atom =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE|PL_BLOB_TEXT,		/* unique representation of text */
  "ucs_text"
};

#define TEXT_REPRESENTATION_MASK 0xff

int
PL_unify_wchars(term_t t, int flags, unsigned int len, const pl_wchar_t *text)
{ const pl_wchar_t *p = text, *end = &text[len];
  char *small, *s;
  int malloced;

  if ( len < 4096 )
  { small = alloca(len);
    malloced = FALSE;
  } else
  { small = PL_malloc(len);
    malloced = TRUE;
  }

  for(s=small; p<end && *p <= 0xff; p++)
    *s++ = *p;

  if ( p == end )
  { int rval;

    switch(flags&TEXT_REPRESENTATION_MASK)
    { case PL_ATOM:
	rval = PL_unify_atom_nchars(t, len, small);
        break;
      case PL_STRING:
	rval = PL_unify_string_nchars(t, len, small);
        break;
      case PL_CODE_LIST:
	rval = PL_unify_list_ncodes(t, len, small);
        break;
      case PL_CHAR_LIST:
	rval = PL_unify_list_nchars(t, len, small);
        break;
      default:
	rval = PL_warning("PL_unify_wchars(): bad flags");
    }

    if ( malloced )
      PL_free(small);

    return rval;
  } else				/* wide character version */
  { if ( malloced )
      PL_free(small);

    switch(flags&TEXT_REPRESENTATION_MASK)
    { case PL_ATOM:
      { GET_LD
	atom_t a;
	int new, rval;

	a = lookupBlob((const char *)text, len*sizeof(pl_wchar_t),
		       &ucs_atom, &new);
	rval = unifyAtomic(t, a PASS_LD);
	PL_unregister_atom(a);
	return rval;
      }
      case PL_STRING:
      case PL_CODE_LIST:
      case PL_CHAR_LIST:
	return PL_warning("PL_unify_wchars(): TBD");
      default:
	return PL_warning("PL_unify_wchars(): bad flags");
    }
  }
}


		 /*******************************
		 *    QUINTUS WRAPPER SUPPORT   *
		 *******************************/

bool
PL_cvt_i_integer(term_t p, long *c)
{ GET_LD
  return PL_get_long(p, c);
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
{ GET_LD
  return PL_get_atom(p, c);
}


bool
PL_cvt_o_integer(long c, term_t p)
{ GET_LD
  return PL_unify_integer(p, c);
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
{ GET_LD
  return PL_unify_atom(p, c);
}


		 /*******************************
		 *	      COMPARE		*
		 *******************************/

int
PL_compare(term_t t1, term_t t2)
{ GET_LD
  Word p1 = valHandleP(t1);
  Word p2 = valHandleP(t2);

  return compareStandard(p1, p2, FALSE PASS_LD);	/* -1, 0, 1 */
}


int
PL_same_compound(term_t t1, term_t t2)
{ GET_LD
  word w1 = valHandle(t1);
  word w2 = valHandle(t2);

 return isTerm(w1) && w1==w2 ? TRUE : FALSE;
}


		 /*******************************
		 *	      INTEGERS		*
		 *******************************/

static inline word
__makeNum(long i ARG_LD)
{ word w = consInt(i);

  if ( valInt(w) == i )
    return w;

  return globalLong(i PASS_LD);
}

word
makeNum__LD(long i ARG_LD)
{ return __makeNum(i PASS_LD);
}

#undef makeNum
#define makeNum(i) __makeNum(i PASS_LD)


		 /*******************************
		 *	       CONS-*		*
		 *******************************/

static inline void
bindConsVal(Word to, Word p ARG_LD)
{ deRef(p);

  if ( canBind(*p) )
  { if ( to < p && !isAttVar(*p) )
    { setVar(*to);
      *p = makeRefG(to);
    } else
      *to = makeRef(p);
  } else
    *to = *p;
}


void
PL_cons_functor(term_t h, functor_t fd, ...)
{ GET_LD
  int arity = arityFunctor(fd);

  if ( arity == 0 )
  { setHandle(h, nameFunctor(fd));
  } else
  { Word a = allocGlobal(1 + arity);
    Word t = a;
    va_list args;

    va_start(args, fd);
    *a = fd;
    while( --arity >= 0 )
    { term_t r = va_arg(args, term_t);

      bindConsVal(++a, valHandleP(r) PASS_LD);
    }
    setHandle(h, consPtr(t, TAG_COMPOUND|STG_GLOBAL));
    va_end(args);
  }
}


void
PL_cons_functor_v(term_t h, functor_t fd, term_t a0)
{ GET_LD
  int arity = arityFunctor(fd);

  if ( arity == 0 )
  { setHandle(h, nameFunctor(fd));
  } else
  { Word t  = allocGlobal(1 + arity);
    Word a  = t;
    Word ai = valHandleP(a0);

    *a = fd;
    while( --arity >= 0 )
      bindConsVal(++a, ai++ PASS_LD);

    setHandle(h, consPtr(t, TAG_COMPOUND|STG_GLOBAL));
  }
}


void
PL_cons_list__LD(term_t l, term_t head, term_t tail ARG_LD)
{ Word a = allocGlobal(3);
  
  a[0] = FUNCTOR_dot2;
  bindConsVal(&a[1], valHandleP(head) PASS_LD);
  bindConsVal(&a[2], valHandleP(tail) PASS_LD);

  setHandle(l, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
}


#undef PL_cons_list
void
PL_cons_list(term_t l, term_t head, term_t tail)
{ GET_LD
  PL_cons_list__LD(l, head, tail PASS_LD);
}
#define PL_cons_list(l, h, t) PL_cons_list__LD(l, h, t PASS_LD)


		 /*******************************
		 *     POINTER <-> PROLOG INT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pointers are not a special type in Prolog. Instead, they are represented
by an integer. The funtions below convert   integers  such that they can
normally be expressed as a tagged  integer: the heap_base is subtracted,
it is divided by 4 and the low 2   bits  are placed at the top (they are
normally 0). longToPointer() does the inverse operation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline ulong
pointerToLong(void *ptr)
{ ulong p   = (ulong) ptr;
  ulong low = p & 0x3L;

  p -= heap_base;
  p >>= 2;
  p |= low<<(sizeof(ulong)*8-2);
  
  return p;
}


static inline void *
longToPointer(ulong p)
{ ulong low = p >> (sizeof(ulong)*8-2);

  p <<= 2;
  p |= low;
  p += heap_base;

  return (void *) p;
}


		 /*******************************
		 *	      GET-*		*
		 *******************************/

static const int type_map[8] = { PL_VARIABLE,
				 PL_VARIABLE,  /* attributed variable */
			         PL_FLOAT,
				 PL_INTEGER,
				 PL_ATOM,
				 PL_STRING,
				 PL_TERM,	/* TAG_COMPOUND */
				 -1		/* TAG_REFERENCE */
			       };

int
PL_get_term_value(term_t t, term_value_t *val)
{ GET_LD
  word w = valHandle(t);
  int rval = type_map[tag(w)];

  switch(rval)
  { case PL_VARIABLE:
      break;
    case PL_INTEGER:
      val->i = valInteger(w);
      break;
    case PL_FLOAT:
      val->f = valReal(w);
      break;
    case PL_ATOM:
      val->a = (atom_t)w;
      break;
    case PL_STRING:
      val->s = valString(w);
      break;
    case PL_TERM:
    { FunctorDef fd = valueFunctor(functorTerm(w));
      val->t.name  = fd->name;
      val->t.arity = fd->arity;
      break;
    }
    default:
      assert(0);
  }

  return rval;
}


int
PL_get_bool(term_t t, int *b)
{ GET_LD
  word w = valHandle(t);

  if ( isAtom(w) )
  { if ( w == ATOM_true || w == ATOM_on )
    { *b = TRUE;
      succeed;
    } else if ( w == ATOM_false || w == ATOM_off )
    { *b = FALSE;
      succeed;
    }
  }

  fail;
}


int
PL_get_atom__LD(term_t t, atom_t *a ARG_LD)
{ word w = valHandle(t);

  if ( isAtom(w) )
  { *a = (atom_t) w;
    succeed;
  }
  fail;
}


#undef PL_get_atom
int
PL_get_atom(term_t t, atom_t *a)
{ GET_LD
  return PL_get_atom__LD(t, a PASS_LD);
}
#define PL_get_atom(t, a) PL_get_atom__LD(t, a PASS_LD)


int
PL_get_atom_chars(term_t t, char **s)
{ GET_LD
  word w = valHandle(t);

  if ( isAtom(w) )
  { Atom a = atomValue(w);

    if ( true(a->type, PL_BLOB_TEXT) )
    { *s = a->name;
      succeed;
    }
  }

  fail;
}


int
PL_get_atom_nchars(term_t t, unsigned int *len, char **s)
{ GET_LD
  word w = valHandle(t);

  if ( isAtom(w) )
  { Atom a = atomValue(w);

    if ( true(a->type, PL_BLOB_TEXT) )
    { *s   = a->name;
      *len = a->length;

      succeed;
    }
  }

  fail;
}


#ifdef O_STRING
int
PL_get_string(term_t t, char **s, unsigned int *len)
{ GET_LD
  word w = valHandle(t);

  if ( isString(w) )
  { *s = valString(w);
    *len = sizeString(w);
    succeed;
  }
  fail;
}
#endif

#define discardable_buffer 	(LD->fli._discardable_buffer)
#define buffer_ring		(LD->fli._buffer_ring)
#define current_buffer_id	(LD->fli._current_buffer_id)

static Buffer
findBuffer(int flags)
{ GET_LD
  Buffer b;

  if ( flags & BUF_RING )
  { if ( ++current_buffer_id == BUFFER_RING_SIZE )
      current_buffer_id = 0;
    b = &buffer_ring[current_buffer_id];
  } else
    b = &discardable_buffer;

  if ( !b->base )
    initBuffer(b);

  emptyBuffer(b);
  return b;
}


char *
buffer_string(const char *s, int flags)
{ Buffer b = findBuffer(flags);
  int l = strlen(s) + 1;

  addMultipleBuffer(b, s, l, char);

  return baseBuffer(b, char);
}


static int
unfindBuffer(int flags)
{ GET_LD
  if ( flags & BUF_RING )
  { if ( --current_buffer_id <= 0 )
      current_buffer_id = BUFFER_RING_SIZE-1;
  }

  fail;
}


int
PL_get_list_nchars(term_t l,
		   unsigned int *length, char **s, unsigned int flags)
{ GET_LD
  Buffer b;
  word list = valHandle(l);
  Word arg, tail;
  char *r;
  unsigned int len;
  enum { CHARS, CODES } type;

  if ( isList(list) )
  { arg = argTermP(list, 0);
    deRef(arg);
    if ( isTaggedInt(*arg) )
    { int i = valInt(*arg);
      if ( i >= 0 && i < 256 )
      { type = CODES;
	goto ok;
      }
    } else if ( isAtom(*arg) )
    { Atom a = atomValue(*arg);

      if ( a->length == 1 && true(a->type, PL_BLOB_TEXT) )
      { type = CHARS;
	goto ok;
      }
    }
  } else if ( isNil(list) )
  { b = findBuffer(flags);
    goto empty;
  }

  fail;

ok:
  b = findBuffer(flags);

  while( isList(list) )
  { int c = -1;

    arg = argTermP(list, 0);
    deRef(arg);

    switch(type)
    { case CODES:
	if ( isTaggedInt(*arg) )
	{ int i = valInt(*arg);
	  if ( i >= 0 && i < 256 )
	    c = i;
	}
        break;
      case CHARS:
	if ( isAtom(*arg) )
	{ Atom a = atomValue(*arg);

	  if ( a->length == 1 && true(a->type, PL_BLOB_TEXT) )
	    c = a->name[0] & 0xff;

	  break;
	}
    }

    if ( c == -1 )
      return unfindBuffer(flags);

    addBuffer(b, c, char);
    tail = argTermP(list, 1);
    deRef(tail);
    list = *tail;
  }
  if ( !isNil(list) )
    return unfindBuffer(flags);

empty:
  len = entriesBuffer(b, char);

  if ( length )
    *length = len;
  addBuffer(b, EOS, char);
  r = baseBuffer(b, char);

  if ( flags & BUF_MALLOC )
  { *s = xmalloc(len+1);
    memcpy(*s, r, len+1);
    unfindBuffer(flags);
  } else
    *s = r;

  succeed;
}


int
PL_get_list_chars(term_t l, char **s, unsigned flags)
{ return PL_get_list_nchars(l, NULL, s, flags);
}


int
PL_get_nchars(term_t l, unsigned int *length, char **s, unsigned flags)
{ GET_LD
  word w = valHandle(l);
  char tmp[100];
  char *r;
  int type;
  unsigned int len = ~(unsigned int)0;
  int malloced = FALSE;

  DEBUG(7, pl_write(l); pl_nl());

  if ( (flags & CVT_ATOM) && isAtom(w) )
  { Atom a = atomValue(w);
    if ( false(a->type, PL_BLOB_TEXT) )
      fail;				/* non-textual atom */
    type = PL_ATOM;
    r = a->name;
    len = a->length;
  } else if ( (flags & CVT_INTEGER) && isInteger(w) )
  { type = PL_INTEGER;
    Ssprintf(tmp, "%ld", valInteger(w) );
    r = tmp;
  } else if ( (flags & CVT_FLOAT) && isReal(w) )
  { char *q;
    type = PL_FLOAT;
    Ssprintf(tmp, LD->float_format, valReal(w) );
    r = tmp;

    q = tmp;				/* See also writePrimitive() */
    if ( *q == '-' )
      q++;
    for(; *q; q++)
    { if ( !isDigit(*q) )
	break;
    }
    if ( !*q )
    { *q++ = '.';
      *q++ = '0';
      *q = EOS;
    }
#ifdef O_STRING
  } else if ( (flags & CVT_STRING) && isString(w) )
  { type = PL_STRING;
    flags |= BUF_RING;			/* always buffer strings */
    r = valString(w);
    len = sizeString(w);
#endif
  } else if ( (flags & CVT_LIST) &&
	      (isList(w) || isNil(w)) &&
	      PL_get_list_nchars(l, length, s, flags) )
  { DEBUG(7, Sdprintf("--> %s\n", *s));
    succeed;
  } else if ( (flags & CVT_VARIABLE) && isVar(w) )
  { type = PL_VARIABLE;
    r = varName(l, tmp);
  } else if ( (flags & CVT_WRITE) )
  { int size;
    IOSTREAM *fd;
    
    type = PL_STRING;			/* hack to get things below ok */
    flags |= BUF_RING;

    r = tmp;
    size = sizeof(tmp);
    fd = Sopenmem(&r, &size, "w");
    PL_write_term(fd, l, 1200, 0);
    Sputc(EOS, fd);
    Sflush(fd);
    len = size-1;			/* need to flush first */
    Sclose(fd);
    if ( r != tmp )
      malloced = TRUE;
  } else
  { DEBUG(7, Sdprintf("--> fail\n"));
    fail;
  }
    
  DEBUG(7, Sdprintf("--> %s\n", r));
  if ( len == ~(unsigned int)0 )
    len = strlen(r);
  if ( length )
    *length = len;

  if ( flags & BUF_MALLOC )
  { *s = xmalloc(len+1);
    memcpy(*s, r, len+1);
  } else if ( ((flags & BUF_RING) && type != PL_ATOM) || /* never atoms */
	      r == tmp )		/* always buffer tmp */
  { Buffer b = findBuffer(flags);

    addMultipleBuffer(b, r, len+1, char);
    *s = baseBuffer(b, char);
  } else
    *s = r;

  if ( malloced )
    free(r);

  succeed;
}


int
PL_get_chars(term_t t, char **s, unsigned flags)
{ return PL_get_nchars(t, NULL, s, flags);
}


char *
PL_quote(int chr, const char *s)
{ Buffer b = findBuffer(BUF_RING);
  
  addBuffer(b, chr, char);
  for(; *s; s++)
  { if ( *s == chr )
      addBuffer(b, chr, char);
    addBuffer(b, *s, char);
  }
  addBuffer(b, chr, char);
  addBuffer(b, EOS, char);
  
  return baseBuffer(b, char);
}


int
PL_get_integer__LD(term_t t, int *i ARG_LD)
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
    int l;

#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( !((f >= PLMININT) && (f <= PLMAXINT)) )
      fail;
#endif

    l = (int)f;
    if ( (real)l == f )
    { *i = l;
      succeed;
    }
  }
  fail;
} 


#undef PL_get_integer
int
PL_get_integer(term_t t, int *i)
{ GET_LD
  return PL_get_integer__LD(t, i PASS_LD);
}
#define PL_get_integer(t, i) PL_get_integer__LD(t, i PASS_LD)


int
PL_get_long__LD(term_t t, long *i ARG_LD)
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
    long l;
    
#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( !((f >= PLMININT) && (f <= PLMAXINT)) )
      fail;
#endif

    l = (long) f;
    if ( (real)l == f )
    { *i = l;
      succeed;
    }
  }
  fail;
} 


#undef PL_get_long
int
PL_get_long(term_t t, long *i)
{ GET_LD
  return PL_get_long__LD(t, i PASS_LD);
}
#define PL_get_long(t, i) PL_get_long__LD(t, i PASS_LD)


int
PL_is_inf(term_t t)
{ GET_LD
  atom_t a;

  if ( PL_get_atom(t, &a) &&
       (a == ATOM_inf || a == ATOM_infinite) )
    succeed;

  fail;
}


int
PL_get_float(term_t t, double *f)
{ GET_LD
  word w = valHandle(t);
  
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
PL_get_pointer__LD(term_t t, void **ptr ARG_LD)
{ long p;

  if ( PL_get_long(t, &p) )
  { *ptr = longToPointer((ulong)p);

    succeed;
  }

  fail;
} 


#undef PL_get_pointer
int
PL_get_pointer(term_t t, void **ptr)
{ GET_LD
  return PL_get_pointer__LD(t, ptr PASS_LD);
}
#define PL_get_pointer(t, ptr) PL_get_pointer__LD(t, ptr PASS_LD)



int
PL_get_name_arity(term_t t, atom_t *name, int *arity)
{ GET_LD
  word w = valHandle(t);

  if ( isTerm(w) )
  { FunctorDef fd = valueFunctor(functorTerm(w));

    *name =  fd->name;
    *arity = fd->arity;
    succeed;
  }
  if ( isTextAtom(w) )
  { *name = (atom_t)w;
    *arity = 0;
    succeed;
  }

  fail;
}


int
PL_get_functor__LD(term_t t, functor_t *f ARG_LD)
{ word w = valHandle(t);

  if ( isTerm(w) )
  { *f = functorTerm(w);
    succeed;
  }
  if ( isTextAtom(w) )
  { *f = lookupFunctorDef(w, 0);
    succeed;
  }

  fail;
}


#undef PL_get_functor
int
PL_get_functor(term_t t, functor_t *f)
{ GET_LD
  return PL_get_functor__LD(t, f PASS_LD);
}
#define PL_get_functor(t, f) PL_get_functor__LD(t, f PASS_LD)

int
PL_get_module(term_t t, module_t *m)
{ GET_LD
  atom_t a;

  if ( PL_get_atom(t, &a) )
  { *m = lookupModule(a);
    succeed;
  }

  fail;
}


#undef _PL_get_arg			/* undo global definition */
void
_PL_get_arg(int index, term_t t, term_t a)
{ GET_LD
  word w = valHandle(t);
  Functor f = (Functor)valPtr(w);
  Word p = &f->arguments[index-1];

  setHandle(a, linkVal(p));
}
#define _PL_get_arg(i, t, a) _PL_get_arg__LD(i, t, a PASS_LD)


void
_PL_get_arg__LD(int index, term_t t, term_t a ARG_LD)
{ word w = valHandle(t);
  Functor f = (Functor)valPtr(w);
  Word p = &f->arguments[index-1];

  setHandle(a, linkVal(p));
}


int
PL_get_arg(int index, term_t t, term_t a)
{ GET_LD
  word w = valHandle(t);

  if ( isTerm(w) && index > 0 )
  { Functor f = (Functor)valPtr(w);
    int arity = arityFunctor(f->definition);

    if ( --index < arity )
    { Word p = &f->arguments[index];

      setHandle(a, linkVal(p));
      succeed;
    }
  }

  fail;
}


#ifdef O_ATTVAR
int
PL_get_attr(term_t t, term_t a)
{ GET_LD
  word w = valHandle(t);

  if ( isAttVar(w) )
  { Word p = valPAttVar(w);

    setHandle(a, makeRef(p));		/* reference, so we can assign */
    succeed;
  }

  fail;
}
#endif


int
PL_get_list__LD(term_t l, term_t h, term_t t ARG_LD)
{ word w = valHandle(l);

  if ( isList(w) )
  { Word a = argTermP(w, 0);

    setHandle(h, linkVal(a++));
    setHandle(t, linkVal(a));

    succeed;
  }

  fail;
}


#undef PL_get_list
int
PL_get_list(term_t l, term_t h, term_t t)
{ GET_LD
  return PL_get_list__LD(l, h, t PASS_LD);
}
#define PL_get_list(l, h, t) PL_get_list__LD(l, h, t PASS_LD)


int
PL_get_head(term_t l, term_t h)
{ GET_LD
  word w = valHandle(l);

  if ( isList(w) )
  { Word a = argTermP(w, 0);
    setHandle(h, linkVal(a));
    succeed;
  }

  fail;
}


int
PL_get_tail(term_t l, term_t t)
{ GET_LD
  word w = valHandle(l);

  if ( isList(w) )
  { Word a = argTermP(w, 1);
    setHandle(t, linkVal(a));
    succeed;
  }
  fail;
}


int
PL_get_nil(term_t l)
{ GET_LD
  word w = valHandle(l);

  if ( isNil(w) )
    succeed;

  fail;
}


int
_PL_get_xpce_reference(term_t t, xpceref_t *ref)
{ GET_LD
  word w = valHandle(t);
  functor_t fd;

  if ( !isTerm(w) )
    fail;

  fd = valueTerm(w)->definition;
  if ( fd == FUNCTOR_xpceref1 )		/* @ref */
  { Word p = argTermP(w, 0);

    do
    { if ( isTaggedInt(*p) )
      { ref->type    = PL_INTEGER;
	ref->value.i = valInt(*p);

	goto ok;
      } 
      if ( isTextAtom(*p) )
      { ref->type    = PL_ATOM;
	ref->value.a = (atom_t) *p;

	goto ok;
      }
      if ( isBignum(*p) )
      { ref->type    = PL_INTEGER;
	ref->value.i = valBignum(*p);

	goto ok;
      }
    } while(isRef(*p) && (p = unRef(*p)));

    return -1;				/* error! */

  ok:
    succeed;
  }

  fail;
}


		 /*******************************
		 *		IS-*		*
		 *******************************/

int
PL_is_variable__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  return canBind(w) ? TRUE : FALSE;
}


#undef PL_is_variable
int
PL_is_variable(term_t t)
{ GET_LD
  word w = valHandle(t);

  return canBind(w) ? TRUE : FALSE;
}
#define PL_is_variable(t) PL_is_variable__LD(t PASS_LD)


int
PL_is_atom__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  if ( isTextAtom(w) )
    return TRUE;

  return FALSE;
}


#undef PL_is_atom
int
PL_is_atom(term_t t)
{ GET_LD
  
  return PL_is_atom__LD(t PASS_LD);
}
#define PL_is_atom(t) PL_is_atom__LD(t PASS_LD)


int
PL_is_blob(term_t t, PL_blob_t **type)
{ GET_LD
  word w = valHandle(t);

  if ( isAtom(w) )
  { if ( type )
    { Atom a = atomValue(w);
      *type = a->type;
    }

    return TRUE;
  }

  return FALSE;
}


int
PL_is_attvar(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isAttVar(w) ? TRUE : FALSE;
}


int
PL_is_integer(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isInteger(w) ? TRUE : FALSE;
}


int
PL_is_float(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isReal(w) ? TRUE : FALSE;
}


int
PL_is_compound(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isTerm(w) ? TRUE : FALSE;
}


int
PL_is_functor__LD(term_t t, functor_t f ARG_LD)
{ word w = valHandle(t);

  if ( hasFunctor(w, f) )
    succeed;

  fail;
}


#undef PL_is_functor
int
PL_is_functor(term_t t, functor_t f)
{ GET_LD
  word w = valHandle(t);

  if ( hasFunctor(w, f) )
    succeed;

  fail;
}
#define PL_is_functor(t, f) PL_is_functor__LD(t, f PASS_LD)


int
PL_is_list(term_t t)
{ GET_LD
  word w = valHandle(t);

  return (isList(w) || isNil(w)) ? TRUE : FALSE;
}


int
PL_is_atomic__LD(term_t t ARG_LD)
{ word w = valHandle(t);

  return isAtomic(w) ? TRUE : FALSE;
}


#undef PL_is_atomic
int
PL_is_atomic(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isAtomic(w) ? TRUE : FALSE;
}
#define PL_is_atomic(t) PL_is_atomic__LD(t PASS_LD)


int
PL_is_number(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isNumber(w) ? TRUE : FALSE;
}


#ifdef O_STRING
int
PL_is_string(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isString(w) ? TRUE : FALSE;
}

int
PL_unify_string_chars(term_t t, const char *s)
{ GET_LD
  word str = globalString((char *)s);

  return unifyAtomic(t, str PASS_LD);
}

int
PL_unify_string_nchars(term_t t, unsigned int len, const char *s)
{ GET_LD
  word str = globalNString(len, s);

  return unifyAtomic(t, str PASS_LD);
}

#endif

		 /*******************************
		 *             PUT-*  		*
		 *******************************/

void
PL_put_variable(term_t t)
{ GET_LD
  Word p = allocGlobal(1);

  setVar(*p);
  setHandle(t, consPtr(p, TAG_REFERENCE|STG_GLOBAL)); /* = makeRef */
}


void
PL_put_atom__LD(term_t t, atom_t a ARG_LD)
{ setHandle(t, a);
}


#undef PL_put_atom
void
PL_put_atom(term_t t, atom_t a)
{ GET_LD
  setHandle(t, a);
}
#define PL_put_atom(t, a) PL_put_atom__LD(t, a PASS_LD)


void
PL_put_atom_chars(term_t t, const char *s)
{ GET_LD
  atom_t a = lookupAtom(s, strlen(s));

  setHandle(t, a);
  PL_unregister_atom(a);
}


void
PL_put_atom_nchars(term_t t, unsigned int len, const char *s)
{ GET_LD
  atom_t a = lookupAtom(s, len);

  setHandle(t, a);
  PL_unregister_atom(a);
}


void
PL_put_string_chars(term_t t, const char *s)
{ GET_LD
  word w = globalString(s);

  setHandle(t, w);
}


void
PL_put_string_nchars(term_t t, unsigned int len, const char *s)
{ GET_LD
  word w = globalNString(len, s);

  setHandle(t, w);
}


void
PL_put_list_ncodes(term_t t, unsigned int len, const char *chars)
{ GET_LD
  
  if ( len == 0 )
  { setHandle(t, ATOM_nil);
  } else
  { Word p = allocGlobal(len*3);
    setHandle(t, consPtr(p, TAG_COMPOUND|STG_GLOBAL));

    for( ; len-- != 0; chars++)
    { *p++ = FUNCTOR_dot2;
      *p++ = consInt((long)*chars & 0xff);
      *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);
      p++;
    }
    p[-1] = ATOM_nil;
  }
}


void
PL_put_list_codes(term_t t, const char *chars)
{ PL_put_list_ncodes(t, strlen(chars), chars);
}


void
PL_put_list_nchars(term_t t, unsigned int len, const char *chars)
{ GET_LD
  
  if ( len == 0 )
  { setHandle(t, ATOM_nil);
  } else
  { Word p = allocGlobal(len*3);
    setHandle(t, consPtr(p, TAG_COMPOUND|STG_GLOBAL));

    for( ; len-- != 0 ; chars++)
    { *p++ = FUNCTOR_dot2;
      *p++ = codeToAtom(*chars & 0xff);
      *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);
      p++;
    }
    p[-1] = ATOM_nil;
  }
}


void
PL_put_list_chars(term_t t, const char *chars)
{ PL_put_list_nchars(t, strlen(chars), chars);
}


void
PL_put_integer__LD(term_t t, long i ARG_LD)
{ setHandle(t, makeNum(i));
}


#undef PL_put_integer
void
PL_put_integer(term_t t, long i)
{ GET_LD
  setHandle(t, makeNum(i));
}
#define PL_put_integer(t, i) PL_put_integer__LD(t, i PASS_LD)


void
_PL_put_number__LD(term_t t, Number n ARG_LD)
{ if ( intNumber(n) )
    PL_put_integer(t, n->value.i);
  else
    PL_put_float(t, n->value.f);
}


void
PL_put_pointer(term_t t, void *ptr)
{ GET_LD
  PL_put_integer(t, pointerToLong(ptr));
}


void
PL_put_float(term_t t, double f)
{ GET_LD
  setHandle(t, globalReal(f));
}


void
PL_put_functor(term_t t, functor_t f)
{ GET_LD
  int arity = arityFunctor(f);

  if ( arity == 0 )
  { setHandle(t, nameFunctor(f));
  } else
  { Word a = allocGlobal(1 + arity);

    setHandle(t, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
    *a++ = f;
    while(arity-- > 0)
      setVar(*a++);
  }
}


void
PL_put_list(term_t l)
{ GET_LD
  Word a = allocGlobal(3);

  setHandle(l, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
  *a++ = FUNCTOR_dot2;
  setVar(*a++);
  setVar(*a);
}


void
PL_put_nil(term_t l)
{ GET_LD
  setHandle(l, ATOM_nil);
}


void
PL_put_term__LD(term_t t1, term_t t2 ARG_LD)
{ Word p2 = valHandleP(t2);

  setHandle(t1, linkVal(p2));
}


#undef PL_put_term
void
PL_put_term(term_t t1, term_t t2)
{ GET_LD
  Word p2 = valHandleP(t2);

  setHandle(t1, linkVal(p2));
}
#define PL_put_term(t1, t2) PL_put_term__LD(t1, t2 PASS_LD)


void
_PL_put_xpce_reference_i(term_t t, unsigned long r)
{ GET_LD
  Word a = allocGlobal(2);

  setHandle(t, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
  *a++ = FUNCTOR_xpceref1;
  *a++ = makeNum(r);
}


void
_PL_put_xpce_reference_a(term_t t, atom_t name)
{ GET_LD
  Word a = allocGlobal(2);

  setHandle(t, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
  *a++ = FUNCTOR_xpceref1;
  *a++ = name;
}


		 /*******************************
		 *	       UNIFY		*
		 *******************************/

int
PL_unify_atom__LD(term_t t, atom_t a ARG_LD)
{ return unifyAtomic(t, a PASS_LD);
}

#undef PL_unify_atom
int
PL_unify_atom(term_t t, atom_t a)
{ GET_LD
  return unifyAtomic(t, a PASS_LD);
}
#define PL_unify_atom(t, a) PL_unify_atom__LD(t, a PASS_LD)


int
PL_unify_functor(term_t t, functor_t f)
{ GET_LD
  Word p = valHandleP(t);
  int arity = arityFunctor(f);

  deRef(p);
  if ( isVar(*p) )
  { if ( arity == 0 )
    { *p = nameFunctor(f);
    } else
    { 
#ifdef O_SHIFT_STACKS
      if ( roomStack(global) < (1+arity) * (long)sizeof(word) )
      { growStacks(environment_frame, NULL, NULL, FALSE, TRUE, FALSE);
	p = valHandleP(t);
	deRef(p);
      }
#else 
      requireStack(global, sizeof(word)*(1+arity));
#endif

      { Word a = gTop;
	gTop += 1+arity;

	*p = consPtr(a, TAG_COMPOUND|STG_GLOBAL);
	*a = f;
	while( --arity >= 0 )
	  setVar(*++a);
      }
    }

    Trail(p);
    succeed;
  } else
  { if ( arity == 0  )
    { if ( *p == nameFunctor(f) )
	succeed;
    } else
    { if ( hasFunctor(*p, f) )
	succeed;
    }

    fail;
  }
}


int
PL_unify_atom_chars(term_t t, const char *chars)
{ GET_LD
  atom_t a = lookupAtom(chars, strlen(chars));
  int rval = unifyAtomic(t, a PASS_LD);

  PL_unregister_atom(a);

  return rval;
}


int
PL_unify_atom_nchars(term_t t, unsigned int len, const char *chars)
{ GET_LD
  atom_t a = lookupAtom(chars, len);
  int rval = unifyAtomic(t, a PASS_LD);

  PL_unregister_atom(a);

  return rval;
}


atom_t
codeToAtom(int code)
{ atom_t a;

  if ( code == EOF )
    return ATOM_end_of_file;

  code &= 0xff;				/* mask for signedness */

  if ( !(a=GD->atoms.for_code[code]) )
  { char tmp[1];

    tmp[0] = code;
    a = lookupAtom(tmp, 1);
    GD->atoms.for_code[code] = a;
  }
  
  return a;
}


int
PL_unify_list_ncodes(term_t l, unsigned int len, const char *chars)
{ GET_LD
  if ( PL_is_variable(l) )
  { term_t tmp = PL_new_term_ref();

    PL_put_list_ncodes(tmp, len, chars);
    return PL_unify(l, tmp);
  } else
  { term_t head = PL_new_term_ref();
    term_t t    = PL_copy_term_ref(l);
    int rval;
  
    for( ; len-- != 0; chars++ )
    { if ( !PL_unify_list(t, head, t) ||
	   !PL_unify_integer(head, (int)*chars & 0xff) )
	fail;
    }
  
    rval = PL_unify_nil(t);
    PL_reset_term_refs(head);
  
    return rval;
  }
}


int
PL_unify_list_codes(term_t l, const char *chars)
{ return PL_unify_list_ncodes(l, strlen(chars), chars);
}


int
PL_unify_list_nchars(term_t l, unsigned int len, const char *chars)
{ GET_LD
  if ( PL_is_variable(l) )
  { term_t tmp = PL_new_term_ref();

    PL_put_list_nchars(tmp, len, chars);
    return PL_unify(l, tmp);
  } else
  { term_t head = PL_new_term_ref();
    term_t t    = PL_copy_term_ref(l);
    int rval;
  
    for( ; len-- != 0; chars++ )
    { if ( !PL_unify_list(t, head, t) ||
	   !PL_unify_atom(head, codeToAtom(*chars & 0xff)) )
	fail;
    }
  
    rval = PL_unify_nil(t);
    PL_reset_term_refs(head);
  
    return rval;
  }
}


int
PL_unify_list_chars(term_t l, const char *chars)
{ return PL_unify_list_nchars(l, strlen(chars), chars);
}


int
PL_unify_integer__LD(term_t t, long i ARG_LD)
{ return unifyAtomic(t, makeNum(i) PASS_LD);
}


#undef PL_unify_integer
int
PL_unify_integer(term_t t, long i)
{ GET_LD
  return unifyAtomic(t, makeNum(i) PASS_LD);
}
#define PL_unify_integer(t, i)	PL_unify_integer__LD(t, i PASS_LD)


int
PL_unify_pointer__LD(term_t t, void *ptr ARG_LD)
{ word w = makeNum(pointerToLong(ptr));

  return unifyAtomic(t, w PASS_LD);
}


#undef PL_unify_pointer
int
PL_unify_pointer(term_t t, void *ptr)
{ GET_LD

  return PL_unify_pointer__LD(t, ptr PASS_LD);
}
#define PL_unify_pointer(t, ptr) PL_unify_pointer__LD(t, ptr PASS_LD)


int
PL_unify_float(term_t t, double f)
{ GET_LD
  word w = globalReal(f);

  return unifyAtomic(t, w PASS_LD);
}


int
PL_unify_arg(int index, term_t t, term_t a)
{ GET_LD
  word w = valHandle(t);

  if ( isTerm(w) &&
       index > 0 &&
       index <= (int)arityFunctor(functorTerm(w)) )
  { Word p = argTermP(w, index-1);
    Word p2 = valHandleP(a);

    return unify_ptrs(p, p2 PASS_LD);
  }

  fail;
}


int
PL_unify_list__LD(term_t l, term_t h, term_t t ARG_LD)
{ Word p = valHandleP(l);

  deRef(p);

  if ( isVar(*p) )
  { Word a;

#ifdef O_SHIFT_STACKS
    if ( roomStack(global) < (long)(3 * sizeof(word)) )
    { growStacks(environment_frame, NULL, NULL, FALSE, TRUE, FALSE);
      p = valHandleP(t);
      deRef(p);
    }
#else 
    requireStack(global, sizeof(word)*3);
#endif
    a = gTop;
    gTop += 3;

    *p = consPtr(a, TAG_COMPOUND|STG_GLOBAL);
    *a++ = FUNCTOR_dot2;
    setVar(*a);
    setHandle(h, makeRefG(a));
    setVar(*++a);
    setHandle(t, makeRefG(a));

    Trail(p);
  } else if ( isList(*p) )
  { Word a = argTermP(*p, 0);

    setHandle(h, linkVal(a++));
    setHandle(t, linkVal(a));
  } else
    fail;

  succeed;
}


#undef PL_unify_list
int
PL_unify_list(term_t l, term_t h, term_t t)
{ GET_LD

  return PL_unify_list__LD(l, h, t PASS_LD);
}
#define PL_unify_list(l, h, t) PL_unify_list__LD(l, h, t PASS_LD)


int
PL_unify_nil(term_t l)
{ GET_LD
  return unifyAtomic(l, ATOM_nil PASS_LD);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_unify_termv(term_t t, va_list args)

This is really complicated. There appears to be no portable way to write
a recursive function using va_list as   argument, each call pulling some
arguments from the list, as va_list can  be any type, including an array
of dynamic unspecified size. So, our only   option is to avoid recursion
and do everything by hand. Luckily  I   was  raised in the days Dijkstra
couldn't cope with recursion and explained you could always translate it
into normal loops :-)

Best implementation for the agenda would   be alloca(), but alloca() has
several portability problems of its own, so we will go for using buffers
as defined in pl-buffer.h.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ enum
  { w_term,				/* Agenda is a term */
    w_list				/* agenda is a list */
  } type;
  union
  { struct
    { term_t term;			/* term for which to do work on */
      int    arity;			/* arity of the term */
      int    arg;			/* current argument */
    } term;
    struct
    { term_t tail;			/* tail of list */
      int    len;			/* elements left */
    } list;
  } value;
} work;


int
PL_unify_termv(term_t t, va_list args)
{ GET_LD
  term_t tsave = PL_new_term_refs(0);	/* save for reclaim */
  tmp_buffer buf;
  int tos = 0;				/* Top-of-stack */
  int rval;

  t = PL_copy_term_ref(t);
  initBuffer(&buf);
  
cont:
  switch(va_arg(args, int))
  { case PL_VARIABLE:
      rval = TRUE;
      break;
    case PL_ATOM:
      rval = PL_unify_atom(t, va_arg(args, atom_t));
      break;
    case PL_BOOL:
    { int v = va_arg(args, int);
      rval = PL_unify_atom(t, v ? ATOM_true : ATOM_false);
      break;
    }
    case PL_SHORT:
    case PL_INT:
      rval = PL_unify_integer(t, va_arg(args, int));
      break;
    case PL_INTEGER:
    case PL_LONG:
      rval = PL_unify_integer(t, va_arg(args, long));
      break;
    case PL_POINTER:
      rval = PL_unify_pointer(t, va_arg(args, void *));
      break;
    case PL_FLOAT:
    case PL_DOUBLE:
      rval = PL_unify_float(t, va_arg(args, double));
      break;
    case PL_STRING:
      rval = PL_unify_string_chars(t, va_arg(args, const char *));
      break;
    case PL_TERM:
      rval = PL_unify(t, va_arg(args, term_t));
      break;
    case PL_CHARS:
      rval = PL_unify_atom_chars(t, va_arg(args, const char *));
      break;
    case PL_NCHARS:
    { unsigned int len = va_arg(args, unsigned int);
      const char *s = va_arg(args, const char *);

      rval = PL_unify_atom_nchars(t, len, s);
      break;
    }
  { functor_t ft;
    int arity;

    case PL_FUNCTOR_CHARS:
    { const char *s = va_arg(args, const char *);

      arity = va_arg(args, int);
      ft = PL_new_functor(PL_new_atom(s), arity);
      goto common_f;
    }
    case PL_FUNCTOR:
    { work w;

      ft = va_arg(args, functor_t);
      arity = arityFunctor(ft);

    common_f:
      if ( !PL_unify_functor(t, ft) )
	goto failout;

      w.type  = w_term;
      w.value.term.term  = PL_copy_term_ref(t);
      w.value.term.arg   = 0;
      w.value.term.arity = arity;
      addBuffer(&buf, w, work);
      tos++;

      rval = TRUE;
      break;
    }
  }
    case PL_LIST:
    { work w;
      
      w.type = w_list;
      w.value.list.tail = PL_copy_term_ref(t);
      w.value.list.len  = va_arg(args, int);

      addBuffer(&buf, w, work);
      tos++;
      
      rval = TRUE;
      break;
    }
    case _PL_PREDICATE_INDICATOR:
    { predicate_t proc = va_arg(args, predicate_t);

      return unify_definition(t, proc->definition,
			      0, GP_HIDESYSTEM|GP_NAMEARITY);
    }
    default:
      PL_warning("Format error in PL_unify_term()");
      goto failout;
  }

  if ( rval )
  { while( tos > 0 )
    { work *w = &baseBuffer(&buf, work)[tos-1];

      switch( w->type )
      { case w_term:
	  if ( w->value.term.arg < w->value.term.arity )
	  { _PL_get_arg(++w->value.term.arg,
			w->value.term.term, t);
	    goto cont;
	  } else
	  { tos--;
	    seekBuffer(&buf, tos, work);
	    break;
	  }
	case w_list:
	{ if ( w->value.list.len > 0 )
	  { if ( PL_unify_list(w->value.list.tail, t, w->value.list.tail) )
	    { w->value.list.len--;
	      goto cont;
	    }
	  } else if ( PL_unify_nil(w->value.list.tail) )
	  { tos--;
	    seekBuffer(&buf, tos, work);
	  } else
	    goto failout;
	}
      }
    }

    PL_reset_term_refs(tsave);
    discardBuffer(&buf);
    return TRUE;
  }

failout:
  PL_reset_term_refs(tsave);
  discardBuffer(&buf);

  return FALSE;
}


int
PL_unify_term(term_t t, ...)
{ va_list args;
  int rval;

  va_start(args, t);
  rval = PL_unify_termv(t, args);
  va_end(args);

  return rval;
}


int
_PL_unify_xpce_reference(term_t t, xpceref_t *ref)
{ GET_LD
  Word p = valHandleP(t);

  do
  { if ( isVar(*p) )
    { Word a = allocGlobal(2);
  
      *p = consPtr(a, TAG_COMPOUND|STG_GLOBAL);
      Trail(p);
      *a++ = FUNCTOR_xpceref1;
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
      if ( *a == v )
	succeed;
      if ( isVar(*a) )
      { *a = v;
        Trail(a);
	succeed;
      }
      if ( isIndirect(v) )
	return equalIndirect(v, *a);
      fail;
    }
  } while ( isRef(*p) && (p = unRef(*p)) );

  fail;
}


		 /*******************************
		 *       ATOMIC (INTERNAL)	*
		 *******************************/

PL_atomic_t
_PL_get_atomic(term_t t)
{ GET_LD
  return valHandle(t);
}


int
_PL_unify_atomic(term_t t, PL_atomic_t a)
{ GET_LD
  return unifyAtomic(t, a PASS_LD);
}


void
_PL_put_atomic(term_t t, PL_atomic_t a)
{ GET_LD
  setHandle(t, a);
}


void
_PL_copy_atomic(term_t t, PL_atomic_t arg) /* internal one */
{ GET_LD
  word a;

  if ( isIndirect(arg) )
    a = globalIndirect(arg);
  else
    a = arg;
  
  setHandle(t, a);
}


		 /*******************************
		 *	       BLOBS		*
		 *******************************/

int
PL_unify_blob(term_t t, void *blob, unsigned int len, PL_blob_t *type)
{ GET_LD
  int new;
  atom_t a = lookupBlob(blob, len, type, &new);
  int rval = unifyAtomic(t, a PASS_LD);

  PL_unregister_atom(a);

  return rval;
}


int
PL_put_blob(term_t t, void *blob, unsigned int len, PL_blob_t *type)
{ GET_LD
  int new;
  atom_t a = lookupBlob(blob, len, type, &new);

  setHandle(t, a);
  PL_unregister_atom(a);

  return new;
}


int
PL_get_blob(term_t t, void **blob, unsigned int *len, PL_blob_t **type)
{ GET_LD
  word w = valHandle(t);

  if ( isAtom(w) )
  { Atom a = atomValue(w);

    if ( blob )
      *blob = a->name;
    if ( len )
      *len  = a->length;
    if ( type )
      *type = a->type;

    succeed;
  }

  fail;
}


void *
PL_blob_data(atom_t a, unsigned int *len, PL_blob_t **type)
{ Atom x = atomValue(a);

  if ( len )
    *len = x->length;
  if ( type )
    *type = x->type;

  return x->name;
}


		 /*******************************
		 *	       TYPE		*
		 *******************************/


int
PL_term_type(term_t t)
{ GET_LD
  word w = valHandle(t);

  return type_map[tag(w)];
}

		 /*******************************
		 *	      UNIFY		*
		 *******************************/



int
PL_unify__LD(term_t t1, term_t t2 ARG_LD)
{ Word p1 = valHandleP(t1);
  Word p2 = valHandleP(t2);
  mark m;

  Mark(m);
  if ( !unify(p1, p2 PASS_LD) )
  { Undo(m);
    fail;
  }

  succeed;
}


#undef PL_unify

int
PL_unify(term_t t1, term_t t2)
{ GET_LD

  return PL_unify__LD(t1, t2 PASS_LD);
}

#define PL_unify(t1, t2) PL_unify__LD(t1, t2 PASS_LD)


		 /*******************************
		 *	       MODULES		*
		 *******************************/

int
PL_strip_module__LD(term_t raw, module_t *m, term_t plain ARG_LD)
{ Word p = valTermRef(raw);
  
  deRef(p);
  if ( hasFunctor(*p, FUNCTOR_colon2) )
  { p = stripModule(p, m PASS_LD);
    setHandle(plain, linkVal(p));
  } else
  { if ( *m == NULL )
      *m = environment_frame ? contextModule(environment_frame)
			     : MODULE_user;
    setHandle(plain, needsRef(*p) ? makeRef(p) : *p);
  }

  succeed;
}

#undef PL_strip_module
int
PL_strip_module(term_t raw, module_t *m, term_t plain)
{ GET_LD
  return PL_strip_module__LD(raw, m, plain PASS_LD);
}
#define PL_strip_module(q, m, t) PL_strip_module__LD(q, m, t PASS_LD)

module_t
PL_context()
{ GET_LD
  return environment_frame ? contextModule(environment_frame)
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
{ Module m;
  atom_t a    = lookupAtom(name, strlen(name));
  functor_t f = lookupFunctorDef(a, arity);

  PL_unregister_atom(a);

  if ( module )
  { a = lookupAtom(module, strlen(module));
    m = lookupModule(a);
    PL_unregister_atom(a);
  } else
    m = PL_context();

  return PL_pred(f, m);
}


predicate_t
_PL_predicate(const char *name, int arity, const char *module,
	      predicate_t *bin)
{ if ( !*bin )
    *bin = PL_predicate(name, arity, module);

  return *bin;
}


int
PL_predicate_info(predicate_t pred, atom_t *name, int *arity, module_t *m)
{ if ( pred->type == PROCEDURE_TYPE )
  { Definition def = pred->definition;

    *name  = def->functor->name;
    *arity = def->functor->arity;
    *m     = def->module;

    succeed;
  }

  fail;
}


		 /*******************************
		 *	       CALLING		*
		 *******************************/

int
PL_call_predicate(Module ctx, int flags, predicate_t pred, term_t h0)
{ int rval;

  qid_t qid = PL_open_query(ctx, flags, pred, h0);
  rval = PL_next_solution(qid);
  PL_cut_query(qid);

  return rval;
}


bool
PL_call(term_t t, Module m)
{ return callProlog(m, t, PL_Q_NORMAL, NULL);
}  


		/********************************
		*	 FOREIGNS RETURN        *
		********************************/

foreign_t
_PL_retry(long v)
{ ForeignRedoInt(v);
}


foreign_t
_PL_retry_address(void *v)
{ if ( (ulong)v & FRG_REDO_MASK )
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


static QueryFrame
find_query(LocalFrame fr)
{ if ( fr )
  { QueryFrame qf;

    while(fr->parent)
      fr = fr->parent;
    
    qf = (QueryFrame)addPointer(fr, -(long)offsetof(struct queryFrame, frame));

    return qf;
  }

  return NULL;
}


int
PL_raise_exception(term_t exception)
{ GET_LD
  PL_put_term(exception_bin, exception);
  exception_term = exception_bin;

  fail;
}


int
PL_throw(term_t exception)
{ GET_LD
  QueryFrame QF = find_query(environment_frame);

  PL_put_term(exception_bin, exception);
  exception_term = exception_bin;
  assert(exception_term);

  if ( QF )
    longjmp(QF->exception_jmp_env, exception_term);

  fail;
}

		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

static void
notify_registered_foreign(functor_t fd, Module m)
{ if ( GD->initialised )
  { GET_LD
    fid_t cid = PL_open_foreign_frame();
    term_t argv = PL_new_term_refs(2);
    predicate_t pred = _PL_predicate("$foreign_registered", 2, "system",
				     &GD->procedures.foreign_registered2);

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
  functor_t fdef;
  atom_t aname;
  const char *s;

  if ( !GD->initialised )
    initModules();			/* Before PL_initialise()! */

					/* check for module:name */
  for(s=name; isAlpha(*s); s++)
    ;

  if ( *s == ':' )
  { m = PL_new_module(PL_new_atom_nchars(s-name, name));
    aname = PL_new_atom(s+1);
  } else
  { GET_LD
    aname = PL_new_atom(name);
    m = (LD && environment_frame ? contextModule(environment_frame)
			         : MODULE_user);
  }

  fdef = lookupFunctorDef(aname, arity);
  proc = lookupProcedure(fdef, m);
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

  if ( m == MODULE_system )
    set(def, FOREIGN|SYSTEM|TRACE_ME);
  else
  { GET_LD
    
    if ( SYSTEM_MODE )
      set(def, FOREIGN|SYSTEM|TRACE_ME);
    else
      set(def, FOREIGN|TRACE_ME);
  }

  if ( (flags & PL_FA_NOTRACE) )	  clear(def, TRACE_ME);
  if ( (flags & PL_FA_TRANSPARENT) )	  set(def, METAPRED);
  if ( (flags & PL_FA_NONDETERMINISTIC) ) set(def, NONDETERMINISTIC);
  if ( (flags & PL_FA_VARARGS) )	  set(def, P_VARARG);

  notify_registered_foreign(fdef, m);

  succeed;
}  


void
PL_load_extensions(const PL_extension *ext)
{ GET_LD
  const PL_extension *e;
  Module m;

  m = (environment_frame ? contextModule(environment_frame)
			 : MODULE_system);

  for(e = ext; e->predicate_name; e++)
  { unsigned long flags = TRACE_ME;
    Definition def;
    Procedure proc;
    atom_t a = PL_new_atom(e->predicate_name);
    functor_t fd = lookupFunctorDef(a, e->arity);

    PL_unregister_atom(a);

    if ( e->flags & PL_FA_NOTRACE )	     flags &= ~TRACE_ME;
    if ( e->flags & PL_FA_TRANSPARENT )	     flags |= METAPRED;
    if ( e->flags & PL_FA_NONDETERMINISTIC ) flags |= NONDETERMINISTIC;
    if ( e->flags & PL_FA_VARARGS )	     flags |= P_VARARG;

    proc = lookupProcedure(fd, m);
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

    notify_registered_foreign(def->functor->functor, m);
  }    
}

		 /*******************************
		 *	 EMBEDDING PROLOG	*
		 *******************************/

int
PL_toplevel(void)
{ atom_t a = PL_new_atom("$toplevel");
  int rval = prologToplevel(a);

  PL_unregister_atom(a);

  return rval;
}


int
PL_halt(int status)
{ if ( !PL_cleanup(status) )
    fail;

  exit(status);
}


		 /*******************************
		 *	    RESOURCES		*
		 *******************************/


IOSTREAM *
PL_open_resource(Module m,
		 const char *name, const char *rc_class,
		 const char *mode)
{ GET_LD
  IOSTREAM *s = NULL;
  fid_t fid = PL_open_foreign_frame();
  static predicate_t MTOK_pred;
  term_t t0 = PL_new_term_refs(4);

  if ( !m )
    m = MODULE_user;

  if ( !MTOK_pred )
    MTOK_pred = PL_predicate("open_resource", 4, "system");

  PL_put_atom_chars(t0+0, name);

  if ( rc_class )
    PL_put_atom_chars(t0+1, rc_class);
  PL_put_atom_chars(t0+2, mode[0] == 'r' ? "read" : "write");
  
  if ( !PL_call_predicate(m, PL_Q_CATCH_EXCEPTION, MTOK_pred, t0) ||
       !PL_get_stream_handle(t0+3, &s) )
    errno = ENOENT;

  PL_discard_foreign_frame(fid);
  return s;
}


		/********************************
		*            SIGNALS            *
		*********************************/

int
PL_raise(int sig)
{ GET_LD
  if ( sig > 0 && sig <= MAXSIGNAL && LD )
  { LD->pending_signals |= (1L << (sig-1));
    return TRUE;
  }

  return FALSE;
}


		/********************************
		*         RESET (ABORTS)	*
		********************************/

struct abort_handle
{ AbortHandle	  next;			/* Next handle */
  PL_abort_hook_t function;		/* The handle itself */
};

#define abort_head (LD->fli._abort_head)
#define abort_tail (LD->fli._abort_tail)

void
PL_abort_hook(PL_abort_hook_t func)
{ GET_LD
  AbortHandle h = (AbortHandle) allocHeap(sizeof(struct abort_handle));
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
{ GET_LD
  AbortHandle h = abort_head;

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
{ GET_LD
  AbortHandle h = abort_head;

  for(; h; h = h->next)
    if ( h->function )
      (*h->function)();
}


		/********************************
		*        FOREIGN INITIALISE	*
		********************************/

struct initialise_handle
{ InitialiseHandle	  next;			/* Next handle */
  PL_initialise_hook_t function;		/* The handle itself */
};

#define initialise_head (GD->foreign.initialise_head)
#define initialise_tail (GD->foreign.initialise_tail)

void
PL_initialise_hook(PL_initialise_hook_t func)
{ InitialiseHandle h = initialise_head;

  for(; h; h = h->next)
  { if ( h->function == func )
      return;				/* already there */
  }

  h = malloc(sizeof(struct initialise_handle));
  if ( !h )
    outOfCore();

  h->next = NULL;
  h->function = func;

  if ( initialise_head == NULL )
  { initialise_head = initialise_tail = h;
  } else
  { initialise_tail->next = h;
    initialise_tail = h;
  }
}


void
initialiseForeign(int argc, char **argv)
{ InitialiseHandle h = initialise_head;

  for(; h; h = h->next)
    (*h->function)(argc, argv);
}


void
cleanupInitialiseHooks(void)
{ InitialiseHandle h, next;

  for(h=initialise_head; h; h=next)
  { next = h->next;
    free(h);
  }

  initialise_head = initialise_tail = NULL;
}



		 /*******************************
		 *	      PROMPT		*
		 *******************************/

void
PL_prompt1(const char *s)
{ prompt1((char *) s);
}


int
PL_ttymode(IOSTREAM *s)
{ GET_LD

  if ( s == Suser_input )
  { if ( !trueFeature(TTY_CONTROL_FEATURE) ) /* -tty in effect */
      return PL_NOTTY;
    if ( ttymode == TTY_RAW )		/* get_single_char/1 and friends */
      return PL_RAWTTY;
    return PL_COOKEDTTY;		/* cooked (readline) input */
  } else
    return PL_NOTTY;
}


void
PL_prompt_next(int fd)
{ GET_LD

  if ( fd == 0 )
    LD->prompt.next = TRUE;
}


char *
PL_prompt_string(int fd)
{ if ( fd == 0 )
    return PrologPrompt();

  return "";
}


void
PL_add_to_protocol(const char *buf, int n)
{ protocol(buf, n);
}


		 /*******************************
		 *	   DISPATCHING		*
		 *******************************/

#define dispatch_events (LD->fli._dispatch_events)

PL_dispatch_hook_t
PL_dispatch_hook(PL_dispatch_hook_t hook)
{ GET_LD
  PL_dispatch_hook_t old = dispatch_events;

  dispatch_events = hook;
  return old;
}


#if defined(HAVE_SELECT) && !defined(WIN32)

#ifdef WIN32
#include <winsock2.h>
#endif

static int
input_on_fd(int fd)
{ fd_set rfds;
  struct timeval tv;

  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  return select(fd+1, &rfds, NULL, NULL, &tv) != 0;
}

#else
#define input_on_fd(fd) 1
#endif


int
PL_dispatch(int fd, int wait)
{ GET_LD

  if ( wait == PL_DISPATCH_INSTALLED )
    return dispatch_events ? TRUE : FALSE;

  if ( dispatch_events )
  { if ( wait == PL_DISPATCH_WAIT )
    { while( !input_on_fd(fd) )
      { if ( PL_handle_signals() < 0 )
	  return FALSE;
	(*dispatch_events)(fd);
      }
    } else
    { (*dispatch_events)(fd);
      if ( PL_handle_signals() < 0 )
	  return FALSE;
    }
  }

  return TRUE;
}


		 /*******************************
		 *	RECORDED DATABASE	*
		 *******************************/

record_t
PL_record(term_t t)
{ GET_LD

  return compileTermToHeap(t, R_DUPLICATE);
}


void
PL_recorded(record_t r, term_t t)
{ GET_LD

  copyRecordToGlobal(t, r PASS_LD);
}


void
PL_erase(record_t r)
{ GET_LD

  freeRecord(r);
}


record_t
PL_duplicate_record(record_t r)
{ if ( true(r, R_DUPLICATE) )
  { r->references++;
    return r;
  } else
    return NULL;
}


		 /*******************************
		 *	    FEATURES		*
		 *******************************/

int
PL_set_feature(const char *name, int type, ...)
{ va_list args;
  int rval = TRUE;

  initFeatureTable();

  va_start(args, type);
  switch(type)
  { case PL_BOOL:
    { int val = va_arg(args, int);
      
      defFeature(name, FT_BOOL, val, 0);
      break;
    }
    case PL_ATOM:
    { const char *v = va_arg(args, const char *);
      if ( !GD->initialised )
	initAtoms();
      defFeature(name, FT_ATOM, v);
      break;
    }
    case PL_INTEGER:
    { long v = va_arg(args, long);
      defFeature(name, FT_INTEGER, v);
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

int
PL_action(int action, ...)
{ int rval;
  va_list args;

  va_start(args, action);

  switch(action)
  { case PL_ACTION_TRACE:
      rval = pl_trace();
      break;
    case PL_ACTION_DEBUG:
      debugmode(DBG_ALL, NULL);
      rval = TRUE;
      break;
    case PL_ACTION_BACKTRACE:
#ifdef O_DEBUGGER
    { GET_LD
      int a = va_arg(args, int);
      int om;

      if ( gc_status.active )
      { Sfprintf(Serror,
		 "\n[Cannot print stack while in %ld-th garbage collection]\n",
		 gc_status.collections);
	fail;
      }
      if ( GD->bootsession || !GD->initialised )
      { Sfprintf(Serror,
		 "\n[Cannot print stack while initialising]\n");
	fail;
      }
      om = systemMode(TRUE);		/* Also show hidden frames */
      backTrace(environment_frame, a);
      systemMode(om);
      rval = TRUE;
    }
#else
      warning("No Prolog backtrace in runtime version");
      rval = FALSE;
#endif
      break;
    case PL_ACTION_BREAK:
      rval = pl_break();
      break;
    case PL_ACTION_HALT:
    { int a = va_arg(args, int);

      PL_halt(a);
      rval = FALSE;
      break;
    }
    case PL_ACTION_ABORT:
      rval = pl_abort(ABORT_NORMAL);
      break;
    case PL_ACTION_GUIAPP:
    { int guiapp = va_arg(args, int);
      GD->os.gui_app = guiapp;
      rval = TRUE;
      break;
    }
    case PL_ACTION_WRITE:
    { GET_LD
      char *s = va_arg(args, char *);
      rval = Sfputs(s, Scurout) < 0 ? FALSE : TRUE;
      break;
    }
    case PL_ACTION_FLUSH:
    { GET_LD
      rval = Sflush(Scurout);
      break;
    }
    case PL_ACTION_ATTACH_CONSOLE:
    {
#ifdef O_PLMT
      return attachConsole();
#else
      return FALSE;
#endif
    }
    default:
      sysError("PL_action(): Illegal action: %d", action);
      /*NOTREACHED*/
      rval = FALSE;
  }

  va_end(args);

  return rval;
}

		/********************************
		*         QUERY PROLOG          *
		*********************************/

#define c_argc (GD->cmdline._c_argc)
#define c_argv (GD->cmdline._c_argv)

static void
init_c_args()
{ if ( c_argc == -1 )
  { GET_LD
    int i;
    int argc    = GD->cmdline.argc;
    char **argv = GD->cmdline.argv;

    c_argv = allocHeap(argc * sizeof(char *));
    c_argv[0] = argv[0];
    c_argc = 1;

    for(i=1; i<argc; i++)
    { if ( argv[i][0] == '-' )
      { switch(argv[i][1])
	{ case 'x':
	  case 'g':
	  case 'd':
	  case 'f':
	  case 's':
	  case 't':
	    i++;
	    continue;
	  case 'B':
	  case 'L':
	  case 'G':
	  case 'O':
	  case 'T':
	  case 'A':
	  case 'q':
	    continue;
	}
      }
      c_argv[c_argc++] = argv[i];
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
    case PL_QUERY_MAX_INTEGER:
      return PLMAXINT;
    case PL_QUERY_MIN_INTEGER:
      return PLMININT;
    case PL_QUERY_MAX_TAGGED_INT:
      return PLMAXTAGGEDINT;
    case PL_QUERY_MIN_TAGGED_INT:
      return PLMINTAGGEDINT;
    case PL_QUERY_GETC:
      PopTty(Sinput, &ttytab);		/* restore terminal mode */
      return (long) Sgetchar();		/* normal reading */
    case PL_QUERY_VERSION:
      return PLVERSION;
    case PL_QUERY_MAX_THREADS:
#ifdef O_PLMT
      return MAX_THREADS;
#else
      return 1;
#endif
    default:
      sysError("PL_query: Illegal query: %d", query);
      /*NOTREACHED*/
      fail;
  }
}


		 /*******************************
		 *	      LICENSE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Register the current module using the license restrictions that apply for
it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct license
{ char *license_id;
  char *module_id;
  struct license *next;
} *pre_registered;


void
PL_license(const char *license, const char *module)
{ GET_LD

  if ( GD->initialised )
  { fid_t fid = PL_open_foreign_frame();
    predicate_t pred = PL_predicate("license", 2, "system");
    term_t av = PL_new_term_refs(2);

    PL_put_atom_chars(av+0, license);
    PL_put_atom_chars(av+1, module);

    PL_call_predicate(NULL, PL_Q_NORMAL, pred, av);
    
    PL_discard_foreign_frame(fid);
  } else
  { struct license *l = allocHeap(sizeof(*l));

    l->license_id = store_string(license);
    l->module_id  = store_string(module);
    l->next = pre_registered;
    pre_registered = l;
  }
}


void
registerForeignLicenses(void)
{ GET_LD
  struct license *l, *next;

  for(l=pre_registered; l; l=next)
  { next = l->next;

    PL_license(l->license_id, l->module_id);
    remove_string(l->license_id);
    remove_string(l->module_id);
    freeHeap(l, sizeof(*l));
  }

  pre_registered = NULL;
}
