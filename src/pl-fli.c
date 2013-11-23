/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "os/pl-ctype.h"
#include "os/pl-utf8.h"
#include "pl-codelist.h"
#include <errno.h>

#include <limits.h>
#if !defined(LLONG_MAX)
#define LLONG_MAX 9223372036854775807LL
#define LLONG_MIN (-LLONG_MAX - 1LL)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog  new-style  foreign-language  interface.   This  new  foreign
interface is a mix of the old  interface using the ideas on term-handles
from Quintus Prolog. Term-handles are   integers (uintptr_t), describing
the offset of the term-location relative to the base of the local stack.

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

#if O_DEBUG || defined(O_MAINTENANCE)
#ifndef O_CHECK_TERM_REFS
#define O_CHECK_TERM_REFS 1
#endif
#endif

#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)

static int	unify_int64_ex__LD(term_t t, int64_t i, int ex ARG_LD);

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

  DEBUG(CHK_SECURE, assert(w != ATOM_garbage_collected));

  return w;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
term_t pushWordAsTermRef(Word p)
       popTermRef()

These two functions are used to create a term-ref from a `Word'. This is
typically needed for calling  PL_error().  In   many  cases  there is no
foreign  environment  around,  which   makes    that   we   cannot  call
PL_new_term_ref(). These functions use the   tmp-references, shared with
PushPtr()/PopPtr() (see pl-incl.h).  Push and pop *must* match.

Note that this protects creating a term-ref  if there is no environment.
However, the function called still must   either not use term-references
or must create an environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

term_t
pushWordAsTermRef__LD(Word p ARG_LD)
{ int i = LD->tmp.top++;
  term_t t = LD->tmp.h[i];

  assert(i<TMP_PTR_SIZE);
  setHandle(t, linkVal(p));

  return t;
}

void
popTermRef__LD(ARG1_LD)
{ int i = --LD->tmp.top;

  assert(i>=0);
  setVar(*valTermRef(LD->tmp.h[i]));
}


		 /*******************************
		 *	   CREATE/RESET		*
		 *******************************/

term_t
PL_new_term_refs__LD(int n ARG_LD)
{ Word t;
  term_t r;
  int i;
  FliFrame fr;

  if ( addPointer(lTop, n*sizeof(word)) > (void*) lMax )
  { int rc = ensureLocalSpace(n*sizeof(word), ALLOW_SHIFT);

    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      return 0;
    }
  }

  t = (Word)lTop;
  r = consTermRef(t);

  for(i=0; i<n; i++)
    setVar(*t++);
  lTop = (LocalFrame)t;
  fr = fli_context;
  fr->size += n;
#ifdef O_CHECK_TERM_REFS
  { int s = (int)((Word) lTop - (Word)(fr+1));
    assert(s == fr->size);
  }
#endif

  return r;
}


static inline term_t
new_term_ref(ARG1_LD)
{ Word t;
  term_t r;
  FliFrame fr;

  t = (Word)lTop;
  r = consTermRef(t);
  setVar(*t++);

  lTop = (LocalFrame)t;
  fr = fli_context;
  fr->size++;
#ifdef O_CHECK_TERM_REFS
  { int s = (int)((Word) lTop - (Word)(fr+1));
    assert(s == fr->size);
  }
#endif

  return r;
}


term_t
PL_new_term_ref__LD(ARG1_LD)
{ if ( addPointer(lTop, sizeof(word)) > (void*) lMax )
  { int rc = ensureLocalSpace(sizeof(word), ALLOW_SHIFT);

    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      return 0;
    }
  }

  return new_term_ref(PASS_LD1);
}


term_t
PL_new_term_ref_noshift__LD(ARG1_LD)
{ if ( addPointer(lTop, sizeof(word)) > (void*) lMax )
    return 0;
  return new_term_ref(PASS_LD1);
}


#undef PL_new_term_ref
#undef PL_new_term_refs

term_t
PL_new_term_refs(int n)
{ GET_LD

  if ( (void*)fli_context <= (void*)environment_frame )
    fatalError("PL_new_term_refs(): No foreign environment");

  return PL_new_term_refs__LD(n PASS_LD);
}


term_t
PL_new_term_ref()
{ GET_LD

  if ( (void*)fli_context <= (void*)environment_frame )
    fatalError("PL_new_term_ref(): No foreign environment");

  return PL_new_term_ref__LD(PASS_LD1);
}


/* PL_new_nil_ref() is for compatibility with SICStus and other
   prologs that create the initial term-reference as [] instead of
   using a variable.
*/

term_t
PL_new_nil_ref(void)
{ GET_LD
  term_t t;

  if ( (void*)fli_context <= (void*)environment_frame )
    fatalError("PL_new_term_ref(): No foreign environment");

  if ( (t=PL_new_term_ref__LD(PASS_LD1)) )
    setHandle(t, ATOM_nil);

  return t;
}


#define PL_new_term_ref()	PL_new_term_ref__LD(PASS_LD1)
#define PL_new_term_refs(n)	PL_new_term_refs__LD(n PASS_LD)


void
PL_reset_term_refs(term_t r)
{ GET_LD
  FliFrame fr = fli_context;

  lTop = (LocalFrame) valTermRef(r);
  fr->size = (int)((Word) lTop - (Word)addPointer(fr, sizeof(struct fliFrame)));
  DEBUG(CHK_SECURE, if ( fr->size < 0 || fr->size > 100 )
		      Sdprintf("Suspect foreign frame size: %d\n", fr->size));
}


term_t
PL_copy_term_ref(term_t from)
{ GET_LD
  Word t, p2;
  term_t r;
  FliFrame fr;

  if ( addPointer(lTop, sizeof(word)) > (void*) lMax )
  { int rc = ensureLocalSpace(sizeof(word), ALLOW_SHIFT);

    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      return 0;
    }
  }

  t  = (Word)lTop;
  r  = consTermRef(t);
  p2 = valHandleP(from);

  *t = linkVal(p2);
  lTop = (LocalFrame)(t+1);
  fr = fli_context;
  fr->size++;
  DEBUG(CHK_SECURE,
	{ int s = (Word) lTop - (Word)(fr+1);
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

May call GC/SHIFT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
unifyAtomic(term_t t, word w ARG_LD)
{ Word p = valHandleP(t);

  for(;;)
  { if ( canBind(*p) )
    { if ( !hasGlobalSpace(0) )
      { int rc;

	if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
	  return raiseStackOverflow(rc);
	p = valHandleP(t);
	deRef(p);
      }

      bindConst(p, w);
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
PL_new_atom_nchars(size_t len, const char *s)
{ if ( !GD->initialised )
    initAtoms();

  if ( len == (size_t)-1 )
    len = strlen(s);

  return (atom_t) lookupAtom(s, len);
}


functor_t
PL_new_functor(atom_t f,  int a)
{ if ( !GD->initialised )
    initFunctors();

  return lookupFunctorDef(f, a);
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

static int	compareUCSAtom(atom_t h1, atom_t h2);
static int	saveUCSAtom(atom_t a, IOSTREAM *fd);
static atom_t	loadUCSAtom(IOSTREAM *fd);

static PL_blob_t ucs_atom =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE|PL_BLOB_TEXT|PL_BLOB_WCHAR,
					/* unique representation of text */
  "ucs_text",
  NULL,					/* release */
  compareUCSAtom,			/* compare */
  writeUCSAtom,				/* write */
  NULL,					/* acquire */
  saveUCSAtom,				/* save load to/from .qlf files */
  loadUCSAtom
};


static void
initUCSAtoms(void)
{ PL_register_blob_type(&ucs_atom);
}


int
isUCSAtom(Atom a)
{ return a->type == &ucs_atom;
}


atom_t
lookupUCSAtom(const pl_wchar_t *s, size_t len)
{ int new;

  return lookupBlob((const char *)s, len*sizeof(pl_wchar_t),
		    &ucs_atom, &new);
}


atom_t
PL_new_atom_wchars(size_t len, const wchar_t *s)
{ PL_chars_t txt;

  if ( !GD->initialised )
    initAtoms();

  if ( len == (size_t)-1 )
    len = wcslen(s);

  txt.text.w    = (wchar_t*)s;
  txt.length    = len;
  txt.encoding  = ENC_WCHAR;
  txt.storage   = PL_CHARS_HEAP;
  txt.canonical = FALSE;

  return textToAtom(&txt);
}


int
get_atom_ptr_text(Atom a, PL_chars_t *text)
{ if ( false(a->type, PL_BLOB_TEXT) )
    fail;				/* non-textual atom */
  if ( a->type == &ucs_atom )
  { text->text.w   = (pl_wchar_t *) a->name;
    text->length   = a->length / sizeof(pl_wchar_t);
    text->encoding = ENC_WCHAR;
  } else
  { text->text.t   = a->name;
    text->length   = a->length;
    text->encoding = ENC_ISO_LATIN_1;
  }
  text->storage   = PL_CHARS_HEAP;
  text->canonical = TRUE;

  succeed;
}


int
get_atom_text(atom_t atom, PL_chars_t *text)
{ Atom a = atomValue(atom);

  return get_atom_ptr_text(a, text);
}


int
get_string_text(word w, PL_chars_t *text ARG_LD)
{ if ( isBString(w) )
  { text->text.t   = getCharsString(w, &text->length);
    text->encoding = ENC_ISO_LATIN_1;
  } else
  { text->text.w   = getCharsWString(w, &text->length);
    text->encoding = ENC_WCHAR;
  }
  text->storage   = PL_CHARS_STACK;
  text->canonical = TRUE;

  succeed;
}


static int
compareUCSAtom(atom_t h1, atom_t h2)
{ Atom a1 = atomValue(h1);
  Atom a2 = atomValue(h2);
  const pl_wchar_t *s1 = (const pl_wchar_t*)a1->name;
  const pl_wchar_t *s2 = (const pl_wchar_t*)a2->name;
  size_t len = a1->length < a2->length ? a1->length : a2->length;

  len /= sizeof(pl_wchar_t);

  for( ; len-- > 0; s1++, s2++)
  { if ( *s1 != *s2 )
    { int d = *s1 - *s2;

      return d<0 ? CMP_LESS : d>0 ? CMP_GREATER : CMP_EQUAL;
    }
  }

  return a1->length >  a2->length ? CMP_GREATER :
	 a1->length == a2->length ? CMP_EQUAL : CMP_LESS;
}


static int
saveUCSAtom(atom_t atom, IOSTREAM *fd)
{ Atom a = atomValue(atom);
  const pl_wchar_t *s = (const pl_wchar_t*)a->name;
  size_t len = a->length/sizeof(pl_wchar_t);

  wicPutStringW(s, len, fd);

  return TRUE;
}


static atom_t
loadUCSAtom(IOSTREAM *fd)
{ pl_wchar_t buf[256];
  pl_wchar_t *w;
  size_t len;
  atom_t a;

  w = wicGetStringUTF8(fd, &len, buf, sizeof(buf)/sizeof(pl_wchar_t));
  a = lookupUCSAtom(w, len);

  if ( w != buf )
    PL_free(w);

  return a;
}


int
PL_unify_wchars(term_t t, int flags, size_t len, const pl_wchar_t *s)
{ PL_chars_t text;
  int rc;

  if ( len == (size_t)-1 )
    len = wcslen(s);

  text.text.w    = (pl_wchar_t *)s;
  text.encoding  = ENC_WCHAR;
  text.storage   = PL_CHARS_HEAP;
  text.length    = len;
  text.canonical = FALSE;

  rc = PL_unify_text(t, 0, &text, flags);
  PL_free_text(&text);

  return rc;
}


int
PL_unify_wchars_diff(term_t t, term_t tail, int flags,
		     size_t len, const pl_wchar_t *s)
{ PL_chars_t text;
  int rc;

  if ( len == (size_t)-1 )
    len = wcslen(s);

  text.text.w    = (pl_wchar_t *)s;
  text.encoding  = ENC_WCHAR;
  text.storage   = PL_CHARS_HEAP;
  text.length    = len;
  text.canonical = FALSE;

  rc = PL_unify_text(t, tail, &text, flags);
  PL_free_text(&text);

  return rc;
}


size_t
PL_utf8_strlen(const char *s, size_t len)
{ return utf8_strlen(s, len);
}


		 /*******************************
		 *	  GET ATOM TEXT		*
		 *******************************/

const char *
PL_atom_chars(atom_t a)
{ return (const char *) stringAtom(a);
}


const char *
PL_atom_nchars(atom_t a, size_t *len)
{ Atom x = atomValue(a);

  if ( x->type != &ucs_atom )
  { if ( len )
      *len = x->length;

    return x->name;
  } else
    return NULL;
}


const wchar_t *
PL_atom_wchars(atom_t a, size_t *len)
{ Atom x = atomValue(a);

  if ( x->type == &ucs_atom )
  { if ( len )
      *len = x->length / sizeof(pl_wchar_t);

    return (const wchar_t *)x->name;
  } else if ( true(x->type, PL_BLOB_TEXT) )
  { Buffer b = findBuffer(BUF_RING);
    const char *s = (const char*)x->name;
    const char *e = &s[x->length];

    for(; s<e; s++)
    { addBuffer(b, *s, wchar_t);
    }
    addBuffer(b, 0, wchar_t);

    if ( len )
      *len = x->length;

    return baseBuffer(b, const wchar_t);
  } else
    return NULL;
}


int
charCode(word w)
{ if ( isAtom(w) )
  { Atom a = atomValue(w);

    if ( a->length == 1 && true(a->type, PL_BLOB_TEXT) )
      return a->name[0] & 0xff;
    if ( a->length == sizeof(pl_wchar_t) && a->type == &ucs_atom )
    { pl_wchar_t *p = (pl_wchar_t*)a->name;

      return p[0];
    }
  }

  return -1;
}


		 /*******************************
		 *    QUINTUS/SICSTUS WRAPPER   *
		 *******************************/

static int sp_encoding = REP_UTF8;

void
SP_set_state(int state)
{ GET_LD

  LD->fli.SP_state = state;
}


int
SP_get_state(void)
{ GET_LD

  return LD->fli.SP_state;
}


int
PL_cvt_encoding(void)
{ return sp_encoding;
}

int
PL_cvt_set_encoding(int enc)
{ switch(enc)
  { case REP_ISO_LATIN_1:
    case REP_UTF8:
    case REP_MB:
      sp_encoding = enc;
      return TRUE;
  }

  return FALSE;
}

#define REP_SP (sp_encoding)

bool
PL_cvt_i_int(term_t p, int *c)
{ return PL_get_integer_ex(p, c);
}


bool
PL_cvt_i_long(term_t p, long *c)
{ return PL_get_long_ex(p, c);
}


bool
PL_cvt_i_size_t(term_t p, size_t *c)
{ return PL_get_size_ex(p, c);
}


bool
PL_cvt_i_float(term_t p, double *c)
{ return PL_get_float_ex(p, c);
}


bool
PL_cvt_i_single(term_t p, float *c)
{ double f;

  if ( PL_get_float_ex(p, &f) )
  { *c = (float)f;
    succeed;
  }

  fail;
}


bool
PL_cvt_i_string(term_t p, char **c)
{ return PL_get_chars(p, c, CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_SP);
}


bool
PL_cvt_i_codes(term_t p, char **c)
{ return PL_get_chars(p, c, CVT_LIST|CVT_EXCEPTION|REP_SP);
}


bool
PL_cvt_i_atom(term_t p, atom_t *c)
{ GET_LD

  return PL_get_atom_ex(p, c);
}


bool
PL_cvt_i_address(term_t p, void *address)
{ void **addrp = address;

  return PL_get_pointer_ex(p, addrp);
}


bool
PL_cvt_o_int64(int64_t c, term_t p)
{ GET_LD
  return unify_int64_ex__LD(p, c, TRUE PASS_LD);
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
{ return PL_unify_chars(p, PL_ATOM|REP_SP, (size_t)-1, c);
}


bool
PL_cvt_o_codes(const char *c, term_t p)
{ return PL_unify_chars(p, PL_CODE_LIST|REP_SP, (size_t)-1, c);
}


bool
PL_cvt_o_atom(atom_t c, term_t p)
{ GET_LD
  return PL_unify_atom(p, c);
}


bool
PL_cvt_o_address(void *address, term_t p)
{ GET_LD
  return PL_unify_pointer(p, address);
}


		 /*******************************
		 *	      COMPARE		*
		 *******************************/

int					/* TBD: how to report error? */
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


int
PL_cons_functor(term_t h, functor_t fd, ...)
{ GET_LD
  int arity = arityFunctor(fd);

  if ( arity == 0 )
  { setHandle(h, nameFunctor(fd));
  } else
  { va_list args;
    Word a, t;

    if ( !hasGlobalSpace(1+arity) )
    { int rc;

      if ( (rc=ensureGlobalSpace(1+arity, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
    }

    a = t = gTop;
    gTop += 1+arity;
    va_start(args, fd);
    *a = fd;
    while( --arity >= 0 )
    { term_t r = va_arg(args, term_t);

      bindConsVal(++a, valHandleP(r) PASS_LD);
    }
    setHandle(h, consPtr(t, TAG_COMPOUND|STG_GLOBAL));
    va_end(args);
  }

  return TRUE;
}


int
PL_cons_functor_v(term_t h, functor_t fd, term_t a0)
{ GET_LD
  int arity = arityFunctor(fd);

  if ( arity == 0 )
  { setHandle(h, nameFunctor(fd));
  } else
  { Word t, a, ai;

    if ( !hasGlobalSpace(1+arity) )
    { int rc;

      if ( (rc=ensureGlobalSpace(1+arity, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
    }

    a = t = gTop;
    gTop += 1+arity;

    ai = valHandleP(a0);
    *a = fd;
    while( --arity >= 0 )
      bindConsVal(++a, ai++ PASS_LD);

    setHandle(h, consPtr(t, TAG_COMPOUND|STG_GLOBAL));
  }

  return TRUE;
}


int
PL_cons_list__LD(term_t l, term_t head, term_t tail ARG_LD)
{ Word a;

  if ( !hasGlobalSpace(3) )
  { int rc;

    if ( (rc=ensureGlobalSpace(3, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  a = gTop;
  gTop += 3;
  a[0] = FUNCTOR_dot2;
  bindConsVal(&a[1], valHandleP(head) PASS_LD);
  bindConsVal(&a[2], valHandleP(tail) PASS_LD);

  setHandle(l, consPtr(a, TAG_COMPOUND|STG_GLOBAL));

  return TRUE;
}


#undef PL_cons_list
int
PL_cons_list(term_t l, term_t head, term_t tail)
{ GET_LD
  return PL_cons_list__LD(l, head, tail PASS_LD);
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

static inline uintptr_t
pointerToInt(void *ptr)
{ uintptr_t p   = (uintptr_t) ptr;
  uintptr_t low = p & 0x3L;

  p -= GD->heap_base;
  p >>= 2;
  p |= low<<(sizeof(uintptr_t)*8-2);

  return p;
}


static inline void *
intToPointer(uintptr_t p)
{ uintptr_t low = p >> (sizeof(uintptr_t)*8-2);

  p <<= 2;
  p |= low;
  p += GD->heap_base;

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
      val->i = valInteger(w);		/* TBD: Handle MPZ integers? */
      break;
    case PL_FLOAT:
      val->f = valFloat(w);
      break;
    case PL_ATOM:
      val->a = (atom_t)w;
      break;
    case PL_STRING:
      val->s = getCharsString(w, NULL);
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
PL_get_atom_nchars(term_t t, size_t *len, char **s)
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
PL_get_string(term_t t, char **s, size_t *len)
{ GET_LD
  word w = valHandle(t);

  if ( isString(w) )
  { char *tmp = getCharsString(w, len);

    if ( tmp )
    { *s = tmp;
      succeed;
    }					/* fails on wide-character string */
  }
  fail;
}
#endif


int
PL_get_list_nchars(term_t l, size_t *length, char **s, unsigned int flags)
{ Buffer b;
  CVT_result result;

  if ( (b = codes_or_chars_to_buffer(l, flags, FALSE, &result)) )
  { char *r;
    size_t len = entriesBuffer(b, char);

    if ( length )
      *length = len;
    addBuffer(b, EOS, char);
    r = baseBuffer(b, char);

    if ( flags & BUF_MALLOC )
    { *s = PL_malloc(len+1);
      memcpy(*s, r, len+1);
      unfindBuffer(flags);
    } else
      *s = r;

    succeed;
  }

  fail;
}


int
PL_get_list_chars(term_t l, char **s, unsigned flags)
{ return PL_get_list_nchars(l, NULL, s, flags);
}


int
PL_get_wchars(term_t l, size_t *length, pl_wchar_t **s, unsigned flags)
{ GET_LD
  PL_chars_t text;

  if ( !PL_get_text(l, &text, flags) )
    return FALSE;

  PL_promote_text(&text);
  PL_save_text(&text, flags);

  if ( length )
    *length = text.length;
  *s = text.text.w;

  return TRUE;
}


int
PL_get_nchars(term_t l, size_t *length, char **s, unsigned flags)
{ GET_LD
  PL_chars_t text;

  if ( !PL_get_text(l, &text, flags) )
    return FALSE;

  if ( PL_mb_text(&text, flags) )
  { PL_save_text(&text, flags);

    if ( length )
      *length = text.length;
    *s = text.text.t;

    return TRUE;
  } else
  { PL_free_text(&text);

    return FALSE;
  }
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
  { intptr_t val = valInt(w);

    if ( val > INT_MAX || val < INT_MIN )
      fail;
    *i = (int)val;
    succeed;
  }
  if ( isBignum(w) )
  { int64_t val = valBignum(w);

    if ( val > INT_MAX || val < INT_MIN )
      fail;

    *i = (int)val;
    succeed;
  }
  if ( isFloat(w) )
  { double f = valFloat(w);
    int l;

#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( f > (double)INT_MAX || f < (double)INT_MIN )
      fail;
#endif

    l = (int)f;
    if ( (double)l == f )
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
  { intptr_t val = valInt(w);

    if ( val > LONG_MAX || val < LONG_MIN )
      fail;
    *i = (long)val;
    succeed;
  }
  if ( isBignum(w) )
  { int64_t val = valBignum(w);

    if ( val > LONG_MAX || val < LONG_MIN )
      fail;

    *i = (long)val;
    succeed;
  }
  if ( isFloat(w) )
  { double f = valFloat(w);
    long l;

#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( f > (double)LONG_MAX || f < (double)LONG_MIN )
      fail;
#endif

    l = (long) f;
    if ( (double)l == f )
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
PL_get_int64__LD(term_t t, int64_t *i ARG_LD)
{ word w = valHandle(t);

  if ( isTaggedInt(w) )
  { *i = valInt(w);
    succeed;
  }
  if ( isBignum(w) )
  { *i = valBignum(w);
    succeed;
  }
  if ( isFloat(w) )
  { double f = valFloat(w);
    int64_t l;

#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( !((f >= LLONG_MAX) && (f <= LLONG_MIN)) )
      fail;
#endif

    l = (int64_t) f;
    if ( (double)l == f )
    { *i = l;
      succeed;
    }
  }

  fail;
}


#undef PL_get_int64
int
PL_get_int64(term_t t, int64_t *i)
{ GET_LD
  return PL_get_int64__LD(t, i PASS_LD);
}
#define PL_get_int64(t, i) PL_get_int64__LD(t, i PASS_LD)


int
PL_get_intptr(term_t t, intptr_t *i)
{ GET_LD
#if SIZEOF_LONG != SIZEOF_VOIDP && SIZEOF_VOIDP == 8
   return PL_get_int64(t, i);
#else
   return PL_get_long(t, (long*)i);
#endif
}


int
PL_get_uintptr(term_t t, size_t *i)
{ GET_LD
  int64_t val;

  if ( !PL_get_int64(t, &val) )
    return FALSE;

  if ( val < 0 )
    return FALSE;
#if SIZEOF_VOIDP < 8
#if SIZEOF_LONG == SIZEOF_VOIDP
  if ( val > (int64_t)ULONG_MAX )
    return FALSE;
#endif
#endif

  *i = (size_t)val;

  return TRUE;
}


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

  if ( isFloat(w) )
  { *f = valFloat(w);
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


#ifdef _MSC_VER
#define ULL(x) x ## ui64
#else
#define ULL(x) x ## ULL
#endif

int
PL_get_pointer__LD(term_t t, void **ptr ARG_LD)
{ int64_t p;

  if ( PL_get_int64(t, &p) )
  {
#if SIZEOF_VOIDP == 4
    if ( p & ULL(0xffffffff00000000) )
      fail;
#endif

    *ptr = intToPointer((uintptr_t)p);

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

    if ( name )
      *name =  fd->name;
    if ( arity )
      *arity = fd->arity;
    succeed;
  }
  if ( isTextAtom(w) )
  { if ( name )
      *name = (atom_t)w;
    if ( arity )
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
  return PL_get_attr__LD(t, a PASS_LD);
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
PL_skip_list(term_t list, term_t tail, size_t *len)
{ GET_LD
  intptr_t length;
  Word l = valTermRef(list);
  Word t;

  length = skip_list(l, &t PASS_LD);
  if ( len )
    *len = length;
  if ( tail )
  { Word t2 = valTermRef(tail);

    setVar(*t2);
    unify_ptrs(t2, t, 0 PASS_LD);
  }

  if ( isNil(*t) )
    return PL_LIST;
  else if ( isVar(*t) )
    return PL_PARTIAL_LIST;
  else if ( isList(*t) )
    return PL_CYCLIC_TERM;
  else
    return PL_NOT_A_LIST;
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
	ref->value.i = (intptr_t)valBignum(*p);

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

  return isFloat(w) ? TRUE : FALSE;
}


static inline int
isRational(word w ARG_LD)
{ if ( isTerm(w) )
  { Functor f = valueTerm(w);

    if ( f->definition == FUNCTOR_rdiv2 )
    { Word p;

      deRef2(&f->arguments[0], p);
      if ( !isInteger(*p) )
	fail;
      deRef2(&f->arguments[1], p);
      if ( !isInteger(*p) )
	fail;
      if ( *p == consInt(0) )
	fail;

      return TRUE;
    }
  }
  if ( isInteger(w) )
    return TRUE;

  return FALSE;
}


int
PL_is_rational(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isRational(w PASS_LD);
}


int
PL_is_compound(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isTerm(w) ? TRUE : FALSE;
}


int
PL_is_callable(term_t t)
{ GET_LD
  word w = valHandle(t);

  return (isTerm(w) || isTextAtom(w)) ? TRUE : FALSE;
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
PL_is_pair(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isList(w) ? TRUE : FALSE;
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

  if ( isInteger(w) ||
       isFloat(w) )
    return TRUE;

  return FALSE;
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
  word str = globalString(strlen(s), (char *)s);

  if ( str )
    return unifyAtomic(t, str PASS_LD);

  return FALSE;
}

int
PL_unify_string_nchars(term_t t, size_t len, const char *s)
{ GET_LD
  word str = globalString(len, s);

  if ( str )
    return unifyAtomic(t, str PASS_LD);

  return FALSE;
}
#endif /*O_STRING*/


		 /*******************************
		 *             PUT-*		*
		 *******************************/

int
PL_put_variable__LD(term_t t ARG_LD)
{ Word p = valTermRef(t);

  setVar(*p);
  return TRUE;
}


#undef PL_put_variable
int
PL_put_variable(term_t t)
{ GET_LD

  return PL_put_variable__LD(t PASS_LD);
}
#define PL_put_variable(t) PL_put_variable__LD(t PASS_LD)


int
PL_put_atom__LD(term_t t, atom_t a ARG_LD)
{ setHandle(t, a);
  return TRUE;
}


#undef PL_put_atom
int
PL_put_atom(term_t t, atom_t a)
{ GET_LD
  setHandle(t, a);
  return TRUE;
}
#define PL_put_atom(t, a) PL_put_atom__LD(t, a PASS_LD)


int
PL_put_bool(term_t t, int val)
{ GET_LD

  PL_put_atom__LD(t, val ? ATOM_true : ATOM_false PASS_LD);
  return TRUE;
}


int
PL_put_atom_chars(term_t t, const char *s)
{ GET_LD
  atom_t a = lookupAtom(s, strlen(s));

  setHandle(t, a);
  PL_unregister_atom(a);

  return TRUE;
}


int
PL_put_atom_nchars(term_t t, size_t len, const char *s)
{ GET_LD
  atom_t a = lookupAtom(s, len);

  if ( len == (size_t)-1 )
    len = strlen(s);

  setHandle(t, a);
  PL_unregister_atom(a);

  return TRUE;
}


int
PL_put_string_chars(term_t t, const char *s)
{ GET_LD
  word w = globalString(strlen(s), s);

  if ( w )
  { setHandle(t, w);
    return TRUE;
  }

  return FALSE;
}


int
PL_put_string_nchars(term_t t, size_t len, const char *s)
{ GET_LD
  word w = globalString(len, s);

  if ( w )
  { setHandle(t, w);
    return TRUE;
  }

  return FALSE;
}


int
PL_put_list_ncodes(term_t t, size_t len, const char *chars)
{ GET_LD

  if ( len == 0 )
  { setHandle(t, ATOM_nil);
  } else
  { Word p = allocGlobal(len*3);

    if ( !p )
      return FALSE;

    setHandle(t, consPtr(p, TAG_COMPOUND|STG_GLOBAL));

    for( ; len-- != 0; chars++)
    { *p++ = FUNCTOR_dot2;
      *p++ = consInt((intptr_t)*chars & 0xff);
      *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);
      p++;
    }
    p[-1] = ATOM_nil;
  }

  return TRUE;
}


int
PL_put_list_codes(term_t t, const char *chars)
{ return PL_put_list_ncodes(t, strlen(chars), chars);
}


int
PL_put_list_nchars(term_t t, size_t len, const char *chars)
{ GET_LD

  if ( len == 0 )
  { setHandle(t, ATOM_nil);
  } else
  { Word p = allocGlobal(len*3);

    if ( !p )
      return FALSE;

    setHandle(t, consPtr(p, TAG_COMPOUND|STG_GLOBAL));

    for( ; len-- != 0 ; chars++)
    { *p++ = FUNCTOR_dot2;
      *p++ = codeToAtom(*chars & 0xff);
      *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);
      p++;
    }
    p[-1] = ATOM_nil;
  }

  return TRUE;
}


int
PL_put_list_chars(term_t t, const char *chars)
{ return PL_put_list_nchars(t, strlen(chars), chars);
}


int
PL_put_int64__LD(term_t t, int64_t i ARG_LD)
{ word w = consInt(i);

  if ( valInt(w) != i &&
       put_int64(&w, i, ALLOW_GC PASS_LD) != TRUE )
    return FALSE;

  setHandle(t, w);
  return TRUE;
}


int
PL_put_integer__LD(term_t t, long i ARG_LD)
{ return PL_put_int64__LD(t, i PASS_LD);
}


int
PL_put_intptr__LD(term_t t, intptr_t i ARG_LD)
{ return PL_put_int64__LD(t, i PASS_LD);
}


int
PL_put_int64(term_t t, int64_t i)
{ GET_LD

  return PL_put_int64__LD(t, i PASS_LD);
}


#undef PL_put_integer
int
PL_put_integer(term_t t, long i)
{ GET_LD
  return PL_put_int64__LD(t, i PASS_LD);
}
#define PL_put_integer(t, i) PL_put_integer__LD(t, i PASS_LD)


int
_PL_put_number__LD(term_t t, Number n ARG_LD)
{ word w;
  int rc;

  if ( (rc=put_number(&w, n, ALLOW_GC PASS_LD)) == TRUE )
  { setHandle(t, w);
    return TRUE;
  } else
  { return raiseStackOverflow(rc);
  }
}


int
PL_put_pointer(term_t t, void *ptr)
{ GET_LD
  uint64_t i = pointerToInt(ptr);

  return PL_put_int64__LD(t, (int64_t)i PASS_LD);
}


int
PL_put_float(term_t t, double f)
{ GET_LD
  word w;
  int rc;

  if ( (rc=put_double(&w, f, ALLOW_GC PASS_LD)) == TRUE )
  { setHandle(t, w);
    return TRUE;
  }

  return raiseStackOverflow(rc);
}


int
PL_put_functor(term_t t, functor_t f)
{ GET_LD
  int arity = arityFunctor(f);

  if ( arity == 0 )
  { setHandle(t, nameFunctor(f));
  } else
  { Word a = allocGlobal(1 + arity);

    if ( !a )
      return FALSE;
    setHandle(t, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
    *a++ = f;
    while(arity-- > 0)
      setVar(*a++);
  }

  return TRUE;
}


int
PL_put_list(term_t l)
{ GET_LD
  Word a = allocGlobal(3);

  if ( a )
  { setHandle(l, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
    *a++ = FUNCTOR_dot2;
    setVar(*a++);
    setVar(*a);
    return TRUE;
  }

  return FALSE;
}


int
PL_put_nil(term_t l)
{ GET_LD
  setHandle(l, ATOM_nil);

  return TRUE;
}


int
PL_put_term__LD(term_t t1, term_t t2 ARG_LD)
{ Word p2 = valHandleP(t2);

  setHandle(t1, linkVal(p2));
  return TRUE;
}


#undef PL_put_term
int
PL_put_term(term_t t1, term_t t2)
{ GET_LD
  Word p2 = valHandleP(t2);

  setHandle(t1, linkVal(p2));
  return TRUE;
}
#define PL_put_term(t1, t2) PL_put_term__LD(t1, t2 PASS_LD)


int
_PL_put_xpce_reference_i(term_t t, uintptr_t i)
{ GET_LD
  Word p;
  word w;

  if ( !hasGlobalSpace(2+2+WORDS_PER_INT64) )
  { int rc;

    if ( (rc=ensureGlobalSpace(2+2+WORDS_PER_INT64, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  w = consInt(i);
  if ( valInt(w) != i )
    put_int64(&w, i, 0 PASS_LD);

  p = gTop;
  gTop += 2;
  setHandle(t, consPtr(p, TAG_COMPOUND|STG_GLOBAL));
  *p++ = FUNCTOR_xpceref1;
  *p++ = w;

  return TRUE;
}


int
_PL_put_xpce_reference_a(term_t t, atom_t name)
{ GET_LD
  Word a = allocGlobal(2);

  if ( a )
  { setHandle(t, consPtr(a, TAG_COMPOUND|STG_GLOBAL));
    *a++ = FUNCTOR_xpceref1;
    *a++ = name;
    return TRUE;
  }
  return FALSE;
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
  if ( canBind(*p) )
  { size_t needed = (1+arity);

    if ( !hasGlobalSpace(needed) )
    { int rc;

      if ( (rc=ensureGlobalSpace(needed, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
      p = valHandleP(t);		/* reload: may have shifted */
      deRef(p);
    }

    if ( arity == 0 )
    { word name = nameFunctor(f);
      bindConst(p, name);
    } else
    { Word a = gTop;
      word to = consPtr(a, TAG_COMPOUND|STG_GLOBAL);

      gTop += 1+arity;
      *a = f;
      while( --arity >= 0 )
	setVar(*++a);

      bindConst(p, to);
    }

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
PL_unify_atom_nchars(term_t t, size_t len, const char *chars)
{ GET_LD
  atom_t a = lookupAtom(chars, len);
  int rval = unifyAtomic(t, a PASS_LD);

  PL_unregister_atom(a);

  return rval;
}


static atom_t
uncachedCodeToAtom(int chrcode)
{ if ( chrcode < 256 )
  { char tmp[1];

    tmp[0] = chrcode;
    return lookupAtom(tmp, 1);
  } else
  { pl_wchar_t tmp[1];
    int new;

    tmp[0] = chrcode;

    return lookupBlob((const char *)tmp, sizeof(pl_wchar_t),
		      &ucs_atom, &new);
  }
}


atom_t
codeToAtom(int chrcode)
{ atom_t a;

  if ( chrcode == EOF )
    return ATOM_end_of_file;

  assert(chrcode >= 0);

  if ( chrcode < (1<<15) )
  { int page  = chrcode / 256;
    int entry = chrcode % 256;
    atom_t *pv;

    if ( !(pv=GD->atoms.for_code[page]) )
    { pv = PL_malloc(256*sizeof(atom_t));

      memset(pv, 0, 256*sizeof(atom_t));
      GD->atoms.for_code[page] = pv;
    }

    if ( !(a=pv[entry]) )
    { a = pv[entry] = uncachedCodeToAtom(chrcode);
    }
  } else
  { a = uncachedCodeToAtom(chrcode);
  }

  return a;
}


void
cleanupCodeToAtom(void)
{ int page;
  atom_t **pv;

  for(page=0, pv=GD->atoms.for_code; page<256; page++)
  { if ( *pv )
    { void *ptr = *pv;
      *pv = NULL;
      PL_free(ptr);
    }
  }
}


int
PL_unify_list_ncodes(term_t l, size_t len, const char *chars)
{ GET_LD
  if ( PL_is_variable(l) )
  { term_t tmp = PL_new_term_ref();

    return (PL_put_list_ncodes(tmp, len, chars) &&
	    PL_unify(l, tmp));
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
PL_unify_list_nchars(term_t l, size_t len, const char *chars)
{ GET_LD
  if ( PL_is_variable(l) )
  { term_t tmp = PL_new_term_ref();

    return (PL_put_list_nchars(tmp, len, chars) &&
	    PL_unify(l, tmp));
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
flags: bitwise or of type and representation

	Types:		PL_ATOM, PL_STRING, PL_CODE_LIST, PL_CHAR_LIST
	Representation: REP_ISO_LATIN_1, REP_UTF8, REP_MB
	Extra:		PL_DIFF_LIST
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_unify_chars(term_t t, int flags, size_t len, const char *s)
{ PL_chars_t text;
  term_t tail;
  int rc;

  if ( len == (size_t)-1 )
    len = strlen(s);

  text.text.t    = (char *)s;
  text.encoding  = ((flags&REP_UTF8) ? ENC_UTF8 : \
		    (flags&REP_MB)   ? ENC_ANSI : ENC_ISO_LATIN_1);
  text.storage   = PL_CHARS_HEAP;
  text.length    = len;
  text.canonical = FALSE;

  flags &= ~(REP_UTF8|REP_MB|REP_ISO_LATIN_1);

  if ( (flags & PL_DIFF_LIST) )
  { tail = t+1;
    flags &= (~PL_DIFF_LIST);
  } else
  { tail = 0;
  }

  rc = PL_unify_text(t, tail, &text, flags);
  PL_free_text(&text);

  return rc;
}


static int
unify_int64_ex__LD(term_t t, int64_t i, int ex ARG_LD)
{ word w = consInt(i);
  Word p = valHandleP(t);

  deRef(p);

  if ( canBind(*p) )
  { if ( !hasGlobalSpace(2+WORDS_PER_INT64) )
    { int rc;

      if ( (rc=ensureGlobalSpace(2+WORDS_PER_INT64, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
      p = valHandleP(t);
      deRef(p);
    }

    if ( valInt(w) != i )
      put_int64(&w, i, 0 PASS_LD);

    bindConst(p, w);
    succeed;
  }

  if ( w == *p && valInt(w) == i )
    succeed;

  if ( isBignum(*p) )
    return valBignum(*p) == i;

  if ( ex && !isInteger(*p) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);

  fail;
}


int
PL_unify_int64_ex__LD(term_t t, int64_t i ARG_LD)
{ return unify_int64_ex__LD(t, i, TRUE PASS_LD);
}


int
PL_unify_int64__LD(term_t t, int64_t i ARG_LD)
{ return unify_int64_ex__LD(t, i, FALSE PASS_LD);
}


int
PL_unify_integer__LD(term_t t, intptr_t i ARG_LD)
{ return unify_int64_ex__LD(t, i, FALSE PASS_LD);
}


#undef PL_unify_integer
int
PL_unify_integer(term_t t, intptr_t i)
{ GET_LD
  return unify_int64_ex__LD(t, i, FALSE PASS_LD);
}
#define PL_unify_integer(t, i)	PL_unify_integer__LD(t, i PASS_LD)

#undef PL_unify_int64
int
PL_unify_int64(term_t t, int64_t i)
{ GET_LD

  return unify_int64_ex__LD(t, i, FALSE PASS_LD);
}
#define PL_unify_int64(t, i)	PL_unify_int64__LD(t, i PASS_LD)

int
PL_unify_pointer__LD(term_t t, void *ptr ARG_LD)
{ uint64_t i = pointerToInt(ptr);

  return unify_int64_ex__LD(t, (int64_t)i, FALSE PASS_LD);
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
  Word p = valHandleP(t);

  deRef(p);

  if ( canBind(*p) )
  { word w;

    if ( !hasGlobalSpace(2+WORDS_PER_DOUBLE) )
    { int rc;

      if ( (rc=ensureGlobalSpace(2+WORDS_PER_DOUBLE, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
      p = valHandleP(t);
      deRef(p);
    }

    put_double(&w, f, ALLOW_CHECKED PASS_LD);
    bindConst(p, w);
    succeed;
  }

  if ( isFloat(*p) && valFloat(*p) == f )
    succeed;

  fail;
}


int
PL_unify_bool(term_t t, int val)
{ GET_LD

  return PL_unify_atom(t, val ? ATOM_true : ATOM_false);
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

    return unify_ptrs(p, p2, ALLOW_GC|ALLOW_SHIFT PASS_LD);
  }

  fail;
}


int
PL_unify_list__LD(term_t l, term_t h, term_t t ARG_LD)
{ Word p = valHandleP(l);

  deRef(p);

  if ( canBind(*p) )
  { Word a;
    word c;

    if ( !hasGlobalSpace(3) )
    { int rc;

      if ( (rc=ensureGlobalSpace(3, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
      p = valHandleP(l);		/* reload: may have shifted */
      deRef(p);
    }

    a = gTop;
    gTop += 3;

    c = consPtr(a, TAG_COMPOUND|STG_GLOBAL);
    *a++ = FUNCTOR_dot2;
    setVar(*a);
    setHandle(h, makeRefG(a));
    setVar(*++a);
    setHandle(t, makeRefG(a));

    bindConst(p, c);
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
  int op;

  if ( !(t = PL_copy_term_ref(t)) )
    return FALSE;
  initBuffer(&buf);

cont:
  switch((op=va_arg(args, int)))
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
    case PL_INT64:
      rval = PL_unify_int64(t, va_arg(args, int64_t));
      break;
    case PL_INTPTR:
    { int64_t i = va_arg(args, intptr_t);
      rval = PL_unify_int64(t, i);
      break;
    }
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
    { size_t len = va_arg(args, size_t);
      const char *s = va_arg(args, const char *);

      rval = PL_unify_atom_nchars(t, len, s);
      break;
    }
    case PL_UTF8_CHARS:
    case PL_UTF8_STRING:
    { PL_chars_t txt;

      txt.text.t    = va_arg(args, char *);
      txt.length    = strlen(txt.text.t);
      txt.storage   = PL_CHARS_HEAP;
      txt.encoding  = ENC_UTF8;
      txt.canonical = FALSE;

      rval = PL_unify_text(t, 0, &txt,
			   op == PL_UTF8_STRING ? PL_STRING : PL_ATOM);
      PL_free_text(&txt);

      break;
    }
    case PL_NUTF8_CHARS:
    case PL_NUTF8_CODES:
    case PL_NUTF8_STRING:
    { PL_chars_t txt;

      txt.length    = va_arg(args, size_t);
      txt.text.t    = va_arg(args, char *);
      txt.storage   = PL_CHARS_HEAP;
      txt.encoding  = ENC_UTF8;
      txt.canonical = FALSE;

      rval = PL_unify_text(t, 0, &txt,
			   op == PL_NUTF8_CHARS ? PL_ATOM :
			   op == PL_NUTF8_CODES ? PL_CODE_LIST :
						  PL_STRING);
      PL_free_text(&txt);

      break;
    }
    case PL_NWCHARS:
    case PL_NWCODES:
    case PL_NWSTRING:
    { PL_chars_t txt;

      txt.length    = va_arg(args, size_t);
      txt.text.w    = va_arg(args, wchar_t *);
      txt.storage   = PL_CHARS_HEAP;
      txt.encoding  = ENC_WCHAR;
      txt.canonical = FALSE;

      if ( txt.length == (size_t)-1 )
	txt.length = wcslen(txt.text.w );

      rval = PL_unify_text(t, 0, &txt,
			   op == PL_NWCHARS ? PL_ATOM :
			   op == PL_NWCODES ? PL_CODE_LIST :
					      PL_STRING);
      PL_free_text(&txt);

      break;
    }
    case PL_MBCHARS:
    case PL_MBCODES:
    case PL_MBSTRING:
    { PL_chars_t txt;

      txt.text.t    = va_arg(args, char *);
      txt.length    = strlen(txt.text.t);
      txt.storage   = PL_CHARS_HEAP;
      txt.encoding  = ENC_ANSI;
      txt.canonical = FALSE;

      rval = PL_unify_text(t, 0, &txt,
			   op == PL_MBCHARS ? PL_ATOM :
			   op == PL_MBCODES ? PL_CODE_LIST :
					      PL_STRING);
      PL_free_text(&txt);

      break;
    }
  { functor_t ft;
    int arity;

    case PL_FUNCTOR_CHARS:
    { const char *s = va_arg(args, const char *);
      atom_t a = PL_new_atom(s);

      arity = va_arg(args, int);
      ft = PL_new_functor(a, arity);
      PL_unregister_atom(a);
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
      if ( !(w.value.term.term  = PL_copy_term_ref(t)) )
	return FALSE;
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
      if ( !(w.value.list.tail = PL_copy_term_ref(t)) )
	return FALSE;
      w.value.list.len  = va_arg(args, int);

      addBuffer(&buf, w, work);
      tos++;

      rval = TRUE;
      break;
    }
    case _PL_PREDICATE_INDICATOR:
    { predicate_t proc = va_arg(args, predicate_t);

      return unify_definition(MODULE_user, t, proc->definition,
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


static inline word
put_xpce_ref_arg(xpceref_t *ref ARG_LD)
{ if ( ref->type == PL_INTEGER )
  { word w = consInt(ref->value.i);

    if ( valInt(w) != ref->value.i )
      put_int64(&w, ref->value.i, 0 PASS_LD);

    return w;
  }

  return ref->value.a;
}


int
_PL_unify_xpce_reference(term_t t, xpceref_t *ref)
{ GET_LD
  Word p;

  if ( !hasGlobalSpace(2+2+WORDS_PER_INT64) )
  { int rc;

    if ( (rc=ensureGlobalSpace(2+2+WORDS_PER_INT64, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  p = valHandleP(t);

  do
  { if ( canBind(*p) )
    { Word a;
      word c;

      a = gTop;
      gTop += 2;
      c = consPtr(a, TAG_COMPOUND|STG_GLOBAL);

      *a++ = FUNCTOR_xpceref1;
      *a++ = put_xpce_ref_arg(ref PASS_LD);

      bindConst(p, c);
      succeed;
    }
    if ( hasFunctor(*p, FUNCTOR_xpceref1) )
    { Word a = argTermP(*p, 0);

      deRef(a);
      if ( canBind(*a) )
      { word c = put_xpce_ref_arg(ref PASS_LD);

	bindConst(a, c);
	succeed;
      } else
      { if ( ref->type == PL_INTEGER )
	  return ( isInteger(*a) &&
		   valInteger(*a) == ref->value.i );
	else
	  return *a == ref->value.a;
      }
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


		 /*******************************
		 *	       BLOBS		*
		 *******************************/

int
PL_unify_blob(term_t t, void *blob, size_t len, PL_blob_t *type)
{ GET_LD
  int new;
  atom_t a = lookupBlob(blob, len, type, &new);
  int rval = unifyAtomic(t, a PASS_LD);

  PL_unregister_atom(a);

  return rval;
}


int
PL_put_blob(term_t t, void *blob, size_t len, PL_blob_t *type)
{ GET_LD
  int new;
  atom_t a = lookupBlob(blob, len, type, &new);

  setHandle(t, a);
  PL_unregister_atom(a);

  return new;
}


int
PL_get_blob(term_t t, void **blob, size_t *len, PL_blob_t **type)
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
PL_blob_data(atom_t a, size_t *len, PL_blob_t **type)
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

  return unify_ptrs(p1, p2, ALLOW_GC|ALLOW_SHIFT PASS_LD);
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
    if ( raw != plain )
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_strip_module_ex() is similar to  PL_strip_module(),   but  returns an
error if it encounters a term <m>:<t>, where <m> is not an atom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_strip_module_ex__LD(term_t raw, module_t *m, term_t plain ARG_LD)
{ Word p = valTermRef(raw);

  deRef(p);
  if ( hasFunctor(*p, FUNCTOR_colon2) )
  { p = stripModule(p, m PASS_LD);
    if ( hasFunctor(*p, FUNCTOR_colon2) )
    { Word a1 = argTermP(*p, 0);
      deRef(a1);
      setHandle(plain, needsRef(*a1) ? makeRef(a1) : *a1);
      return PL_type_error("module", plain);
    }
    setHandle(plain, linkVal(p));
  } else
  { if ( *m == NULL )
      *m = environment_frame ? contextModule(environment_frame)
			     : MODULE_user;
    setHandle(plain, needsRef(*p) ? makeRef(p) : *p);
  }

  return TRUE;
}

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

int
PL_qualify(term_t raw, term_t qualified)
{ GET_LD
  Module m = NULL;
  term_t mname;

  if ( !(mname = PL_new_term_ref()) ||
       !PL_strip_module(raw, &m, qualified) )
    return FALSE;

  setHandle(mname, m->name);

  return PL_cons_functor(qualified, FUNCTOR_colon2, mname, qualified);
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
{ Definition def = pred->definition;

  if ( name )
    *name  = def->functor->name;
  if ( arity )
    *arity = def->functor->arity;
  if ( m )
    *m     = def->module;

  return TRUE;
}


		 /*******************************
		 *	       CALLING		*
		 *******************************/

int
PL_call_predicate(Module ctx, int flags, predicate_t pred, term_t h0)
{ int rval;
  qid_t qid;

  if ( (qid = PL_open_query(ctx, flags, pred, h0)) )
  { rval = PL_next_solution(qid);
    PL_cut_query(qid);
  } else
    rval = FALSE;

  return rval;
}


int
PL_call(term_t t, Module m)
{ return callProlog(m, t, PL_Q_NORMAL, NULL);
}


		/********************************
		*	 FOREIGNS RETURN        *
		********************************/

foreign_t
_PL_retry(intptr_t v)
{ ForeignRedoInt(v);
}


foreign_t
_PL_retry_address(void *v)
{ if ( (uintptr_t)v & FRG_REDO_MASK )
    PL_fatal_error("PL_retry_address(%p): bad alignment", v);

  ForeignRedoPtr(v);
}


intptr_t
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


static int
is_resource_error(term_t ex)
{ GET_LD
  Word p = valTermRef(ex);

  deRef(p);
  if ( hasFunctor(*p, FUNCTOR_error2) )
  { p = argTermP(*p, 0);
    deRef(p);

    return hasFunctor(*p, FUNCTOR_resource_error1);
  }

  return FALSE;
}


int
PL_raise_exception(term_t exception)
{ GET_LD

  if ( is_resource_error(exception) )
    save_backtrace("exception");

  if ( PL_is_variable(exception) )
    fatalError("Cannot throw variable exception");

  LD->exception.processing = TRUE;
  if ( !PL_same_term(exception, exception_bin) ) /* re-throwing */
  { setVar(*valTermRef(exception_bin));
    if ( !duplicate_term(exception, exception_bin PASS_LD) )
      fatalError("Failed to copy exception term");
    freezeGlobal(PASS_LD1);
  }
  exception_term = exception_bin;

  fail;
}


int
PL_throw(term_t exception)
{ GET_LD

  PL_raise_exception(exception);
  if ( LD->exception.throw_environment )
    longjmp(LD->exception.throw_environment->exception_jmp_env, 1);

  fail;
}


int
PL_rethrow(void)
{ GET_LD

  if ( LD->exception.throw_environment )
    longjmp(LD->exception.throw_environment->exception_jmp_env, 1);

  fail;
}


void
PL_clear_exception(void)
{ GET_LD

  if ( exception_term )
  { exception_term = 0;
    setVar(*valTermRef(LD->exception.bin));
    setVar(*valTermRef(LD->exception.printed));
    setVar(*valTermRef(LD->exception.pending));
  }

  LD->exception.processing = FALSE;
}


void
PL_clear_foreign_exception(LocalFrame fr)
{ term_t ex = PL_exception(0);

  Sdprintf("Foreign predicate %s did not clear exception: ",
	   predicateName(fr->predicate));
  PL_write_term(Serror, ex, 1200, 0);
  Sdprintf("\n");
  if ( is_resource_error(ex) )
    print_backtrace_named("exception");

  PL_clear_exception();
}



		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

#define extensions_loaded	(GD->foreign._loaded)

static void
notify_registered_foreign(functor_t fd, Module m)
{ if ( GD->initialised )
  { GET_LD
    fid_t cid;

    if ( (cid = PL_open_foreign_frame()) )
    { term_t argv = PL_new_term_refs(2);
      predicate_t pred = _PL_predicate("$foreign_registered", 2, "system",
				       &GD->procedures.foreign_registered2);

      PL_put_atom(argv+0, m->name);
      if ( !(PL_put_functor(argv+1, fd) &&
	     PL_call_predicate(MODULE_system, PL_Q_NODEBUG, pred, argv)) )
	; /*Sdprintf("Failed to notify new foreign predicate\n");*/
	  /*note that the hook may not be defined*/
      PL_discard_foreign_frame(cid);
    }
  }
}


static predicate_t
bindForeign(Module m, const char *name, int arity, Func f, int flags)
{ GET_LD
  Procedure proc;
  Definition def;
  functor_t fdef;
  atom_t aname;

  aname = PL_new_atom(name);

  fdef = lookupFunctorDef(aname, arity);
  if ( !(proc = lookupProcedureToDefine(fdef, m)) )
  { warning("PL_register_foreign(): attempt to redefine "
	    "a system predicate: %s:%s",
	    PL_atom_chars(m->name), functorName(fdef));
    return NULL;
  }
  def = proc->definition;
  if ( def->module != m || def->impl.any )
  { DEBUG(MSG_PROC, Sdprintf("Abolish %s from %s\n",
			     procedureName(proc), PL_atom_chars(m->name)));
    abolishProcedure(proc, m);
    def = proc->definition;
  }

  if ( def->impl.any )
    PL_linger(def->impl.any);
  def->impl.function = f;
  def->flags &= ~(P_DYNAMIC|P_THREAD_LOCAL|P_TRANSPARENT|P_NONDET|P_VARARG);
  def->flags |= (P_FOREIGN|TRACE_ME);

  if ( m == MODULE_system || SYSTEM_MODE )
    set(def, P_LOCKED|HIDE_CHILDS);

  if ( (flags & PL_FA_NOTRACE) )	  clear(def, TRACE_ME);
  if ( (flags & PL_FA_TRANSPARENT) )	  set(def, P_TRANSPARENT);
  if ( (flags & PL_FA_NONDETERMINISTIC) ) set(def, P_NONDET);
  if ( (flags & PL_FA_VARARGS) )	  set(def, P_VARARG);

  createForeignSupervisor(def, f);
  notify_registered_foreign(fdef, m);

  return proc;
}


static Module
resolveModule(const char *module)
{ if ( !GD->initialised )      /* Before PL_initialise()! */
    initModules();

  if (module)
    return PL_new_module(PL_new_atom(module));
  else
  { GET_LD
    return (LD && environment_frame ? contextModule(environment_frame)
	                            : MODULE_user);
  }
}

void
bindExtensions(const char *module, const PL_extension *ext)
{ Module m = resolveModule(module);

  for(; ext->predicate_name; ext++)
  { bindForeign(m, ext->predicate_name, ext->arity,
		ext->function, ext->flags);
  }
}

void
PL_register_extensions_in_module(const char *module, const PL_extension *e)
{ if ( extensions_loaded )
    bindExtensions(module, e);
  else
    rememberExtensions(module, e);
}


void
PL_register_extensions(const PL_extension *e)
{ PL_register_extensions_in_module(NULL, e);
}


static int
register_foreignv(const char *module,
		  const char *name, int arity, Func f, int flags,
		  va_list args)
{ if ( extensions_loaded )
  { Module m = resolveModule(module);
    predicate_t p = bindForeign(m, name, arity, f, flags);

    if ( p && (flags&PL_FA_META) )
      PL_meta_predicate(p, va_arg(args, char*));

    return (p != NULL);
  } else
  { PL_extension ext[2];
    ext->predicate_name = (char *)name;
    ext->arity = arity;
    ext->function = f;
    ext->flags = flags;
    ext[1].predicate_name = NULL;
    rememberExtensions(module, ext);

    return TRUE;
  }
}


int
PL_register_foreign_in_module(const char *module,
			      const char *name, int arity, Func f, int flags, ...)
{ va_list args;
  int rc;

  va_start(args, flags);
  rc = register_foreignv(module, name, arity, f, flags, args);
  va_end(args);

  return rc;
}


int
PL_register_foreign(const char *name, int arity, Func f, int flags, ...)
{ va_list args;
  int rc;

  va_start(args, flags);
  rc = register_foreignv(NULL, name, arity, f, flags, args);
  va_end(args);

  return rc;
}

		    /* deprecated */
void
PL_load_extensions(const PL_extension *ext)
{ PL_register_extensions_in_module(NULL, ext);
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
{ int reclaim_memory = FALSE;

#if defined(GC_DEBUG) || defined(O_DEBUG)
  reclaim_memory = TRUE;
#endif

  if ( cleanupProlog(status, reclaim_memory) )
  { run_on_halt(&GD->os.exit_hooks, status);
    exit(status);
  }

  return FALSE;
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
  fid_t fid;
  static predicate_t MTOK_pred;
  term_t t0;

  if ( !m )
    m = MODULE_user;

  if ( !MTOK_pred )
    MTOK_pred = PL_predicate("open_resource", 4, "system");

  if ( !(fid = PL_open_foreign_frame()) )
  { errno = ENOENT;
    return s;
  }
  t0 = PL_new_term_refs(4);
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

  return raiseSignal(LD, sig);
}


int
PL_pending__LD(int sig ARG_LD)
{ if ( sig > 0 && sig <= MAXSIGNAL && LD )
    return (LD->signal.pending & ((int64_t)1 << (sig-1))) ? TRUE : FALSE;

  return -1;
}


int
PL_clearsig__LD(int sig ARG_LD)
{ if ( sig > 0 && sig <= MAXSIGNAL && LD )
  { simpleMutexLock(&LD->signal.sig_lock);
    LD->signal.pending &= ~((int64_t)1 << (sig-1));
    simpleMutexUnlock(&LD->signal.sig_lock);
    updateAlerted(LD);
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
  AbortHandle h = (AbortHandle) allocHeapOrHalt(sizeof(struct abort_handle));
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
{ prompt1(lookupAtom(s, strlen(s)));
}


int
PL_ttymode(IOSTREAM *s)
{ GET_LD

  if ( s == Suser_input )
  { if ( !truePrologFlag(PLFLAG_TTY_CONTROL) ) /* -tty in effect */
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
  { atom_t a = PrologPrompt();		/* TBD: deal with UTF-8 */

    if ( a )
    { PL_chars_t txt;

      if ( get_atom_text(a, &txt) )
      { if ( txt.encoding == ENC_ISO_LATIN_1 )
	  return txt.text.t;
      }
    }
  }

  return NULL;
}


void
PL_add_to_protocol(const char *buf, size_t n)
{ protocol(buf, n);
}


		 /*******************************
		 *	   DISPATCHING		*
		 *******************************/

PL_dispatch_hook_t
PL_dispatch_hook(PL_dispatch_hook_t hook)
{ PL_dispatch_hook_t old = GD->foreign.dispatch_events;

  GD->foreign.dispatch_events = hook;
  return old;
}


#if defined(HAVE_SELECT) && !defined(__WINDOWS__)
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that this is  used  to   integrate  X11  event-dispatching into the
SWI-Prolog  toplevel.  Integration  of  event-handling   in  Windows  is
achieved through the plterm DLL (see  win32/console). For this reason we
do never want this code in Windows.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
{ if ( wait == PL_DISPATCH_INSTALLED )
    return GD->foreign.dispatch_events ? TRUE : FALSE;

  if ( GD->foreign.dispatch_events && PL_thread_self() == 1 )
  { if ( wait == PL_DISPATCH_WAIT )
    { while( !input_on_fd(fd) )
      { if ( PL_handle_signals() < 0 )
	  return FALSE;
	(*GD->foreign.dispatch_events)(fd);
      }
    } else
    { (*GD->foreign.dispatch_events)(fd);
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


int
PL_recorded(record_t r, term_t t)
{ GET_LD

  return copyRecordToGlobal(t, r, ALLOW_GC PASS_LD) == TRUE;
}


void
PL_erase(record_t r)
{ freeRecord(r);
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
		 *	   PROLOG FLAGS		*
		 *******************************/

int
PL_set_prolog_flag(const char *name, int type, ...)
{ va_list args;
  int rval = TRUE;
  int flags = (type & FF_MASK);

  va_start(args, type);
  switch(type & ~FF_MASK)
  { case PL_BOOL:
    { int val = va_arg(args, int);

      setPrologFlag(name, FT_BOOL|flags, val, 0);
      break;
    }
    case PL_ATOM:
    { const char *v = va_arg(args, const char *);
      if ( !GD->initialised )
	initAtoms();
      setPrologFlag(name, FT_ATOM|flags, v);
      break;
    }
    case PL_INTEGER:
    { intptr_t v = va_arg(args, intptr_t);
      setPrologFlag(name, FT_INTEGER|flags, v);
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

int
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
{ int rval = TRUE;
  va_list args;

  va_start(args, action);

  switch(action)
  { case PL_ACTION_TRACE:
      rval = (int)pl_trace();
      break;
    case PL_ACTION_DEBUG:
      debugmode(DBG_ALL, NULL);
      break;
    case PL_ACTION_BACKTRACE:
#ifdef O_DEBUGGER
    { GET_LD
      int a = va_arg(args, int);

      if ( gc_status.active )
      { Sfprintf(Serror,
		 "\n[Cannot print stack while in %ld-th garbage collection]\n",
		 gc_status.collections);
	rval = FALSE;
	break;
      }
      if ( GD->bootsession || !GD->initialised )
      { Sfprintf(Serror,
		 "\n[Cannot print stack while initialising]\n");
	rval = FALSE;
	break;
      }
      PL_backtrace(a, 0);
    }
#else
      warning("No Prolog backtrace in runtime version");
      rval = FALSE;
#endif
      break;
    case PL_ACTION_BREAK:
      rval = (int)pl_break();
      break;
    case PL_ACTION_HALT:
    { int a = va_arg(args, int);

      PL_halt(a);
      rval = FALSE;
      break;
    }
    case PL_ACTION_ABORT:
      rval = (int)abortProlog();
      break;
    case PL_ACTION_GUIAPP:
    { int guiapp = va_arg(args, int);
      GD->os.gui_app = guiapp;
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
      rval = attachConsole();
#else
      rval = FALSE;
#endif
      break;
    }
    case PL_GMP_SET_ALLOC_FUNCTIONS:
    {
#ifdef O_GMP
      int set = va_arg(args, int);

      if ( !GD->gmp.initialised )
      { GD->gmp.keep_alloc_functions = !set;
	initGMP();
      } else
      { rval = FALSE;
      }
#else
      rval = FALSE;
#endif
      break;
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

intptr_t
PL_query(int query)
{ switch(query)
  { case PL_QUERY_ARGC:
      return (intptr_t) GD->cmdline.appl_argc;
    case PL_QUERY_ARGV:
      return (intptr_t) GD->cmdline.appl_argv;
    case PL_QUERY_MAX_INTEGER:
    case PL_QUERY_MIN_INTEGER:
      fail;				/* cannot represent (anymore) */
    case PL_QUERY_MAX_TAGGED_INT:
      return PLMAXTAGGEDINT;
    case PL_QUERY_MIN_TAGGED_INT:
      return PLMINTAGGEDINT;
    case PL_QUERY_GETC:
      PopTty(Sinput, &ttytab, FALSE);		/* restore terminal mode */
      return (intptr_t) Sgetchar();		/* normal reading */
    case PL_QUERY_VERSION:
      return PLVERSION;
    case PL_QUERY_MAX_THREADS:
#ifdef O_PLMT
      Sdprintf("PL_query(PL_QUERY_MAX_THREADS) is no longer supported\n");
      return 100000;
#else
      return 1;
#endif
    case PL_QUERY_ENCODING:
    { GET_LD

      if ( LD )
	return LD->encoding;
      return PL_local_data.encoding;	/* Default: of main thread? */
    }
    case PL_QUERY_USER_CPU:		/* User CPU in milliseconds */
    { double cpu = CpuTime(CPU_USER);
      return (intptr_t)(cpu*1000.0);
    }
    case PL_QUERY_HALTING:
    { return (GD->cleaning == CLN_NORMAL ? FALSE : TRUE);
    }
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
  { struct license *l = allocHeapOrHalt(sizeof(*l));

    l->license_id = store_string(license);
    l->module_id  = store_string(module);
    l->next = pre_registered;
    pre_registered = l;
  }
}


void
registerForeignLicenses(void)
{ struct license *l, *next;

  for(l=pre_registered; l; l=next)
  { next = l->next;

    PL_license(l->license_id, l->module_id);
    remove_string(l->license_id);
    remove_string(l->module_id);
    freeHeap(l, sizeof(*l));
  }

  pre_registered = NULL;
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

void
initForeign(void)
{ initUCSAtoms();
}
