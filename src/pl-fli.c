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

/*#define O_DEBUG 1*/
#define EMIT_FLI_INLINES 1
#include "pl-fli.h"

#include "os/pl-ctype.h"
#include "os/pl-utf8.h"
#include "os/pl-text.h"
#include "os/pl-cstack.h"
#include "os/pl-prologflag.h"
#include "pl-codelist.h"
#include "pl-dict.h"
#include "pl-arith.h"
#include "pl-wrap.h"
#include "pl-comp.h"
#include "pl-gc.h"
#include "pl-attvar.h"
#include "pl-funct.h"
#include "pl-write.h"
#include "pl-wic.h"
#include "pl-prims.h"
#include "pl-modul.h"
#include "pl-proc.h"
#include "pl-pro.h"
#include "pl-gvar.h"
#include "pl-util.h"
#include "pl-supervisor.h"
#include "pl-ext.h"
#include "pl-rec.h"
#include "pl-trace.h"
#include <errno.h>

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/lsan_interface.h>
#endif

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

#define VALID_INT_ARITY(a) \
	{ if ( arity < 0 || arity > INT_MAX ) \
	    fatalError("Arity out of range: %lld", (int64_t)arity); \
	} while(0);

#define VALID_TERM_ARITY(arity) \
	do { if ( (ssize_t)arity < 0 ) \
	       return raiseStackOverflow(GLOBAL_OVERFLOW); } while(0)

#if USE_LD_MACROS
#define	unify_int64_ex(t, i, ex)	LDFUNC(unify_int64_ex, t, i, ex)
#define	PL_get_uint(t, i)		LDFUNC(PL_get_uint, t, i)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static int	unify_int64_ex(term_t t, int64_t i, int ex);
static int	PL_get_uint(term_t t, unsigned int *i);

#undef LDFUNC_DECLARATIONS

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deduce the value to store a copy of the  contents of p. This is a *very*
frequent operation. There  are  a  couple   of  options  to  realise it.
Basically, we can choose between simple  dereferencing and returning the
value or create a new reference.  In the latter case, we are a bit unlucky,
as we could also have returned the last reference.

Second, we can opt  for  inlining  or   not.  Especially  in  the latter
variation, which is a bit longer, a function might actually be faster.

The general version here is no longer used  as we typically need to deal
with linking to a local stack variable  in a dedicated way. Therefore we
have:

  - linkValI()
    Is an inlined version where the caller must guarantee we should
    not make a reference to a local variable.
  - linkValG()
    May allocate a global stack variable and GC/SHIFT.  Caller must
    guarantee this poses no problems.
  - linkValNoG()
    May be used if in the case the argument is a local stack variable
    we can simply pass it as an unlinked variable.  Typically the case
    if a variable is an error anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if 0
word
linkVal(DECL_LD Word p)
{ word w = *p;

  while( isRef(w) )
  { p = unRef(w);
    if ( needsRef(*p) )
      return w;
    w = *p;
  }

  if ( unlikely(needsRef(w)) )
  { if ( unlikely(p > (Word)lBase) )
    { Word v = gTop++;

#ifdef O_DEBUG
      Sdprintf("linkVal() needs to globalize\n");
      trap_gdb();
#endif
      assert(gTop < gMax);		/* TBD: ensure in caller */
      setVar(*v);
      w = makeRefG(v);
      Trail(p, w);
      return w;
    }
    return makeRefG(p);
  }

  DEBUG(CHK_ATOM_GARBAGE_COLLECTED, assert(w != ATOM_garbage_collected));

  return w;
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If `p` ultimately is a  variable  on   the  local  stack  this creates a
variable on the global stack and links  both variables to this location.
Note that this may cause global and   trail stack overflows and thus may
cause a stack shift, garbage collection or fail (returning 0).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
linkValG(DECL_LD Word p)
{ word w;

retry:
  w = *p;
  while( isRef(w) )
  { p = unRef(w);
    if ( needsRef(*p) )
      return w;
    w = *p;
  }

  if ( unlikely(needsRef(w)) )
  { if ( unlikely(p > (Word)lBase) )
    { Word v;

      if ( !hasGlobalSpace(1) )
      { int rc; PushPtr(p);
	rc = makeMoreStackSpace(GLOBAL_OVERFLOW, ALLOW_GC);
	PopPtr(p);
	if ( !rc )
	  return 0;
	goto retry;
      }

      v = gTop++;
      setVar(*v);
      w = makeRefG(v);
      Trail(p, w);
      return w;
    }
    return makeRefG(p);
  }

  DEBUG(CHK_ATOM_GARBAGE_COLLECTED, assert(w != ATOM_garbage_collected));

  return w;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This version always succeeds, but returns   a non-linked variable if the
argument is a plain variable on the local   stack.  This is fine for use
cases where a variable is an error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
linkValNoG(DECL_LD Word p)
{ word w = *p;

  while(isRef(w))
  { p = unRef(w);
    w = *p;
  }

  if ( needsRef(w) )
  { if ( p < (Word)lBase )
      w = makeRefG(p);
  }

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

Note  that  if  `p`  ultimately  is  a   variable  on  the  local  stack
linkValNoG() will return a non-linked variable. This should be ok as for
all use cases passing a variable is an error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

term_t
pushWordAsTermRef(DECL_LD Word p)
{ int i = LD->tmp.top++;
  term_t t = LD->tmp.h[i];

  assert(i<TMP_PTR_SIZE);
  setHandle(t, linkValNoG(p));

  return t;
}

void
popTermRef(DECL_LD)
{ int i = --LD->tmp.top;

  assert(i>=0);
  setVar(*valTermRef(LD->tmp.h[i]));
}


/* bArgVar(Word ap, Word vp) unifies a pointer into a struct with a
   pointer to a value.  This is the same as the B_ARGVAR instruction
   and used to push terms for e.g., A_ADD_FC
*/

void
bArgVar(DECL_LD Word ap, Word vp)
{ deRef(vp);

  if ( isVar(*vp) )
  { if ( ap < vp )
    { setVar(*ap);
      Trail(vp, makeRefG(ap));
    } else
    { *ap = makeRefG(vp);
    }
  } else if ( isAttVar(*vp) )
  { *ap = makeRefG(vp);
  } else
  { *ap = *vp;
  }
}

		 /*******************************
		 *	   CREATE/RESET		*
		 *******************************/

term_t
PL_new_term_refs(DECL_LD int n)
{ Word t;
  term_t r;
  int i;
  FliFrame fr;

  if ( !ensureLocalSpace(n*sizeof(word)) )
    return 0;

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


#define new_term_ref(_) LDFUNC(new_term_ref, _)
static inline term_t
new_term_ref(DECL_LD)
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
PL_new_term_ref(DECL_LD)
{ if ( !ensureLocalSpace(sizeof(word)) )
    return 0;

  return new_term_ref();
}


term_t
PL_new_term_ref_noshift(DECL_LD)
{ if ( unlikely(addPointer(lTop, sizeof(word)) > (void*) lMax) )
    return 0;
  return new_term_ref();
}



API_STUB(term_t)
(PL_new_term_refs)(int n)
( if ( (void*)fli_context <= (void*)environment_frame )
    fatalError("PL_new_term_refs(): No foreign environment");

  return PL_new_term_refs(n);
)


API_STUB(term_t)
(PL_new_term_ref)()
( if ( (void*)fli_context <= (void*)environment_frame )
    fatalError("PL_new_term_ref(): No foreign environment");

  return PL_new_term_ref();
)


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

  if ( (t=PL_new_term_ref()) )
    setHandle(t, ATOM_nil);

  return t;
}


int
globalizeTermRef(DECL_LD term_t t)
{ Word p;

retry:
  p = valTermRef(t);
  if ( unlikely(isVar(*p)) )
  { Word v;

    if ( !hasGlobalSpace(1) )
    { int rc;

      if ( (rc=ensureGlobalSpace(1, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
      goto retry;
    }
    v = gTop++;
    setVar(*v);
    Trail(p, makeRefG(v));
  }

  return TRUE;
}





void
PL_reset_term_refs(DECL_LD term_t r)
{ FliFrame fr = fli_context;

  lTop = (LocalFrame) valTermRef(r);
  fr->size = (int)((Word) lTop - (Word)addPointer(fr, sizeof(struct fliFrame)));
  DEBUG(0, assert(fr->size >= 0));
}

term_t
PL_copy_term_ref(DECL_LD term_t from)
{ Word t, p2;
  term_t r;
  FliFrame fr;

  if ( !ensureLocalSpace(sizeof(word)) ||
       !globalizeTermRef(from) )
    return 0;

  t  = (Word)lTop;
  r  = consTermRef(t);
  p2 = valHandleP(from);
  *t = linkValI(p2);
  lTop = (LocalFrame)(t+1);
  fr = fli_context;
  fr->size++;
  DEBUG(CHK_SECURE,
	{ int s = (Word) lTop - (Word)(fr+1);
	  assert(s == fr->size);
	});

  return r;
}


API_STUB(void)
(PL_reset_term_refs)(term_t r)
( PL_reset_term_refs(r); )

API_STUB(term_t)
(PL_copy_term_ref)(term_t from)
( return PL_copy_term_ref(from); )



		 /*******************************
		 *	    UNIFICATION		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unifyAtomic(p, a) unifies a term, represented by  a pointer to it, with
an atomic value. It is intended for foreign language functions.

May call GC/SHIFT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define unifyAtomic(t, w) LDFUNC(unifyAtomic, t, w)
static bool
unifyAtomic(DECL_LD term_t t, word w)
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


atom_t
PL_new_atom_mbchars(int flags, size_t len, const char *s)
{ PL_chars_t text;
  atom_t a;

  if ( len == (size_t)-1 )
    len = strlen(s);

  text.text.t    = (char*)s;
  text.encoding  = ((flags&REP_UTF8) ? ENC_UTF8 : \
		    (flags&REP_MB)   ? ENC_ANSI : ENC_ISO_LATIN_1);
  text.length    = len;
  text.canonical = FALSE;
  text.storage   = PL_CHARS_HEAP;

  a = textToAtom(&text);
  PL_free_text(&text);

  return a;
}



functor_t
PL_new_functor_sz(atom_t f, size_t arity)
{ if ( !GD->initialised )
    initFunctors();

  return lookupFunctorDef(f, arity);
}

functor_t
(PL_new_functor)(atom_t f, int arity)
{ if ( arity >= 0 )
    return PL_new_functor_sz(f, arity);
  fatalError("Arity out of range: %d", arity);
  return 0;
}


atom_t
PL_functor_name(functor_t f)
{ return nameFunctor(f);
}


size_t
PL_functor_arity_sz(functor_t f)
{ return arityFunctor(f);
}

int
(PL_functor_arity)(functor_t f)
{ size_t arity = arityFunctor(f);

  VALID_INT_ARITY(arity);
  return (int)arity;
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
  atom_t a;

  if ( !GD->initialised )
    initAtoms();

  if ( len == (size_t)-1 )
    len = wcslen(s);

  txt.text.w    = (wchar_t*)s;
  txt.length    = len;
  txt.encoding  = ENC_WCHAR;
  txt.storage   = PL_CHARS_HEAP;
  txt.canonical = FALSE;

  a = textToAtom(&txt);
  PL_free_text(&txt);

  return a;
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
get_string_text(DECL_LD word w, PL_chars_t *text)
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
  { Buffer b = findBuffer(BUF_STACK);
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

#ifndef SCHAR_MIN
#define SCHAR_MIN -128
#define SCHAR_MAX 127
#endif
#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif

#ifndef SHORT_MIN
#define SHORT_MIN -32768
#define SHORT_MAX 32767
#define USHORT_MAX (SHORT_MAX*2+1)
#endif

static bool
_PL_cvt_i_char(term_t p, char *c, int mn, int mx)
{ GET_LD
  int i;
  PL_chars_t txt;

  if ( PL_get_integer(p, &i) && i >= mn && i <= mx )
  { *c = (char)i;
    return TRUE;
  } else if ( PL_get_text(p, &txt, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { if ( txt.length == 1 && txt.encoding == ENC_ISO_LATIN_1 )
    { *c = txt.text.t[0];
      return TRUE;			/* can never be allocated */
    }
    PL_free_text(&txt);
  }

  if ( PL_is_integer(p) )
    return PL_representation_error(mn < 0 ? "char" : "uchar");

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, p);
}


bool
PL_cvt_i_schar(term_t p, signed char *c)
{ return _PL_cvt_i_char(p, (char *)c, SCHAR_MIN, SCHAR_MAX);
}

bool
PL_cvt_i_uchar(term_t p, unsigned char *c)
{ return _PL_cvt_i_char(p, (char *)c, 0, UCHAR_MAX);
}


bool
PL_cvt_i_char(term_t p, char *c)
{ return ((char) 255 == -1) ? PL_cvt_i_schar(p, (signed char*)c)
			    : PL_cvt_i_uchar(p, (unsigned char *)c);
}

static bool
_PL_cvt_i_short(term_t p, short *s, int mn, int mx)
{ GET_LD
  int i;

  if ( PL_get_integer(p, &i) &&
       i >= mn && i <= mx )
  { *s = (short)i;
    return TRUE;
  }

  if ( PL_is_integer(p) )
    return PL_representation_error(mn < 0 ? "short" : "ushort");

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, p);
}

bool
PL_cvt_i_short(term_t p, short *s)
{ return _PL_cvt_i_short(p, s, SHORT_MIN, SHORT_MAX);
}

bool
PL_cvt_i_ushort(term_t p, unsigned short *s)
{ return _PL_cvt_i_short(p, (short *)s, 0, USHORT_MAX);
}

bool
PL_cvt_i_int(term_t p, int *c)
{ return PL_get_integer_ex(p, c);
}

bool
PL_cvt_i_uint(term_t t, unsigned int *c)
{ GET_LD

  if ( PL_get_uint(t, c) )
    return TRUE;

  if ( PL_is_integer(t) )
    return PL_representation_error("uint");

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}

bool
PL_cvt_i_long(term_t p, long *c)
{ return PL_get_long_ex(p, c);
}

bool
PL_cvt_i_ulong(term_t p, unsigned long *c)
{
#if SIZEOF_LONG == 8
  return PL_cvt_i_uint64(p, (uint64_t *)c);
#else
  return PL_cvt_i_uint(p, (unsigned int*)c);
#endif
}

bool
PL_cvt_i_int64(term_t p, int64_t *c)
{ return PL_get_int64_ex(p, c);
}

bool
PL_cvt_i_uint64(term_t p, uint64_t *c)
{ GET_LD
  return PL_get_uint64_ex(p, c);
}

bool
PL_cvt_i_size_t(term_t p, size_t *c)
{ GET_LD
  return PL_get_size_ex(p, c);
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
  return unify_int64_ex(p, c, TRUE);
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

  return compareStandard(p1, p2, FALSE);	/* -1, 0, 1 */
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

/* `to` is a pointer into a (new) compound
   `p` may be anything.
*/

#define bindConsVal(to, p) LDFUNC(bindConsVal, to, p)
static inline void
bindConsVal(DECL_LD Word to, Word p)
{ deRef(p);

  if ( canBind(*p) )
  { if ( to < p && !isAttVar(*p) )
    { setVar(*to);
      *p = makeRefG(to);
    } else
      *to = makeRefG(p);
  } else
    *to = *p;
}


int
PL_cons_functor(term_t h, functor_t fd, ...)
{ GET_LD
  size_t arity = arityFunctor(fd);

  if ( arity == 0 )
  { setHandle(h, nameFunctor(fd));
  } else
  { va_list args;
    Word a, t;

    VALID_TERM_ARITY(arity);

    if ( !hasGlobalSpace(1+arity) )
    { int rc;

      if ( (rc=ensureGlobalSpace(1+arity, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
    }

    a = t = gTop;
    gTop += 1+arity;
    va_start(args, fd);
    *a = fd;
    while( arity-- > 0 )
    { term_t r = va_arg(args, term_t);

      bindConsVal(++a, valHandleP(r));
    }
    setHandle(h, consPtr(t, TAG_COMPOUND|STG_GLOBAL));
    va_end(args);
  }

  return TRUE;
}


int
PL_cons_functor_v(term_t h, functor_t fd, term_t a0)
{ GET_LD
  size_t arity = arityFunctor(fd);

  if ( arity == 0 )
  { setHandle(h, nameFunctor(fd));
  } else
  { Word t, a, ai;

    VALID_TERM_ARITY(arity);

    if ( !hasGlobalSpace(1+arity) )
    { int rc;

      if ( (rc=ensureGlobalSpace(1+arity, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
    }

    a = t = gTop;
    gTop += 1+arity;

    ai = valHandleP(a0);
    *a = fd;
    while( arity-- > 0 )
      bindConsVal(++a, ai++);

    setHandle(h, consPtr(t, TAG_COMPOUND|STG_GLOBAL));
  }

  return TRUE;
}


int
PL_cons_list(DECL_LD term_t l, term_t head, term_t tail)
{ Word a;

  if ( !hasGlobalSpace(3) )
  { int rc;

    if ( (rc=ensureGlobalSpace(3, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  a = gTop;
  gTop += 3;
  a[0] = FUNCTOR_dot2;
  bindConsVal(&a[1], valHandleP(head));
  bindConsVal(&a[2], valHandleP(tail));

  setHandle(l, consPtr(a, TAG_COMPOUND|STG_GLOBAL));

  return TRUE;
}


API_STUB(int)
(PL_cons_list)(term_t l, term_t head, term_t tail)
( return PL_cons_list(l, head, tail); )

/* PL_cons_list_v() creates a list from a vector of term-references
*/

int
PL_cons_list_v(term_t list, size_t count, term_t elems)
{ GET_LD

  if ( count > 0 )
  { Word p;

    if ( !hasGlobalSpace(3*count) )
    { int rc;

      if ( (rc=ensureGlobalSpace(3*count, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
    }

    p = gTop;
    for( ; count-- > 0; p += 3, elems++ )
    { p[0] = FUNCTOR_dot2;
      bindConsVal(&p[1], valHandleP(elems));
      if ( count > 0 )
      { p[2] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
      } else
      { p[2] = ATOM_nil;
      }
    }

    setHandle(list, consPtr(gTop, TAG_COMPOUND|STG_GLOBAL));
    gTop = p;
  } else
  { setHandle(list, ATOM_nil);
  }

  return TRUE;
}

		 /*******************************
		 *	      GET-*		*
		 *******************************/

static const int type_map[8] = { PL_VARIABLE,
				 PL_VARIABLE,  /* attributed variable */
			         PL_FLOAT,
				 PL_INTEGER,
				 PL_STRING,
				 PL_ATOM,
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
      if ( !isTextAtom(val->a) )
      { if ( val->a == ATOM_nil )
	  return PL_NIL;
	else
	  return PL_BLOB;
      }
      break;
    case PL_STRING:
      val->s = getCharsString(w, NULL);
      break;
    case PL_TERM:
    { FunctorDef fd = valueFunctor(functorTerm(w));
      val->t.name  = fd->name;
      val->t.arity = fd->arity;
      if ( fd->functor == FUNCTOR_dot2 )
	return PL_LIST_PAIR;
      if ( val->t.name == ATOM_dict )
	return PL_DICT;
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


/* PL_get_atom(DECL_LD term_t t, atom_t *a) moved to pl-fli.h */

API_STUB(int)
(PL_get_atom)(term_t t, atom_t *a)
( return PL_get_atom(t, a); )


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
      unfindBuffer(b, flags);
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


int
PL_get_text_as_atom(term_t t, atom_t *a, int flags)
{ GET_LD
  word w = valHandle(t);
  PL_chars_t text;

  if ( isAtom(w) )
  { *a = (atom_t) w;
    return TRUE;
  }

  if ( PL_get_text(t, &text, flags) )
  { atom_t ta = textToAtom(&text);

    PL_free_text(&text);
    if ( ta )
    { *a = ta;
      return TRUE;
    }
  }

  return FALSE;
}



char *
PL_quote(int chr, const char *s)
{ Buffer b = findBuffer(BUF_STACK);

  addBuffer(b, (char)chr, char);
  for(; *s; s++)
  { if ( *s == chr )
      addBuffer(b, (char)chr, char);
    addBuffer(b, *s, char);
  }
  addBuffer(b, (char)chr, char);
  addBuffer(b, EOS, char);

  return baseBuffer(b, char);
}


int
PL_get_integer(DECL_LD term_t t, int *i)
{ word w = valHandle(t);

  if ( isTaggedInt(w) )
  { intptr_t val = valInt(w);

    if ( val > INT_MAX || val < INT_MIN )
      fail;
    *i = (int)val;
    succeed;
  }
#if SIZEOF_VOIDP < 8
  if ( isBignum(w) )
  { int64_t val = valBignum(w);

    if ( val > INT_MAX || val < INT_MIN )
      fail;

    *i = (int)val;
    succeed;
  }
#endif
#ifndef O_GMP
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
#endif
  fail;
}


API_STUB(int)
(PL_get_integer)(term_t t, int *i)
( return PL_get_integer(t, i); )


static int
PL_get_uint(DECL_LD term_t t, unsigned int *i)
{ word w = valHandle(t);

  if ( isTaggedInt(w) )
  { intptr_t val = valInt(w);

    if ( val < 0 || val > UINT_MAX )
      fail;
    *i = (unsigned int)val;
    succeed;
  }
#if SIZEOF_VOIDP < 8
  if ( isBignum(w) )
  { int64_t val = valBignum(w);

    if ( val < 0 || val > UINT_MAX )
      fail;

    *i = (unsigned int)val;
    succeed;
  }
#endif
#ifndef O_GMP
  if ( isFloat(w) )
  { double f = valFloat(w);
    unsigned int l;

#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( f > (double)UINT_MAX || f < 0.0 )
      fail;
#endif

    l = (unsigned int)f;
    if ( (double)l == f )
    { *i = l;
      succeed;
    }
  }
#endif
  fail;
}


int
PL_get_long(DECL_LD term_t t, long *i)
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


API_STUB(int)
(PL_get_long)(term_t t, long *i)
( return PL_get_long(t, i); )


int
PL_get_int64(DECL_LD term_t t, int64_t *i)
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


API_STUB(int)
(PL_get_int64)(term_t t, int64_t *i)
( return PL_get_int64(t, i); )

int
PL_get_uint64(term_t t, uint64_t *i)
{ GET_LD

  return pl_get_uint64(t, i, FALSE);
}

API_STUB(int)
(PL_get_uint64_ex)(term_t t, uint64_t *i)
( return pl_get_uint64(t, i, TRUE); )

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
    return TRUE;
  }
  if ( isRational(w) )
  { number n;
    int rc;

    get_rational(w, &n);
    if ( (rc=promoteToFloatNumber(&n)) )
      *f = n.value.f;
    else
      PL_clear_exception();

    clearNumber(&n);

    return rc;
  }

  return FALSE;
}


#ifdef _MSC_VER
#define ULL(x) x ## ui64
#else
#define ULL(x) x ## ULL
#endif

int
PL_get_pointer(DECL_LD term_t t, void **ptr)
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


API_STUB(int)
(PL_get_pointer)(term_t t, void **ptr)
( return PL_get_pointer(t, ptr); )



int
PL_get_name_arity_sz(DECL_LD term_t t, atom_t *name, size_t *arity)
{ word w = valHandle(t);

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


API_STUB(int)
(PL_get_name_arity_sz)(term_t t, atom_t *name, size_t *arity)
( return PL_get_name_arity_sz(t, name, arity); )


int
PL_get_compound_name_arity_sz(term_t t, atom_t *name, size_t *arity)
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

  fail;
}

int
(PL_get_name_arity)(term_t t, atom_t *name, int *arityp)
{ GET_LD
  size_t arity;

  if ( !PL_get_name_arity_sz(t, name, &arity) )
    return FALSE;
  VALID_INT_ARITY(arity);
  *arityp = (int)arity;
  return TRUE;
}

int
(PL_get_compound_name_arity)(term_t t, atom_t *name, int *arityp)
{ size_t arity;

  if ( !PL_get_compound_name_arity_sz(t, name, &arity) )
    return FALSE;
  VALID_INT_ARITY(arity);
  *arityp = (int)arity;
  return TRUE;
}


int
PL_get_functor(DECL_LD term_t t, functor_t *f)
{ word w = valHandle(t);

  if ( isTerm(w) )
  { *f = functorTerm(w);
    succeed;
  }
  if ( isCallableAtom(w) || isReservedSymbol(w) )
  { *f = lookupFunctorDef(w, 0);
    succeed;
  }

  fail;
}


API_STUB(int)
(PL_get_functor)(term_t t, functor_t *f)
( return PL_get_functor(t, f); )

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


int
_PL_get_arg_sz(size_t index, term_t t, term_t a)
{ GET_LD
  word w = valHandle(t);
  Functor f = (Functor)valPtr(w);
  Word p = &f->arguments[index-1];

  setHandle(a, linkValI(p));
  return TRUE;
}
int
(_PL_get_arg)(int index, term_t t, term_t a)
{ if ( index >= 0 )
  { _PL_get_arg_sz(index, t, a);
    return TRUE;
  } else
    fatalError("Arity out of range: %d", a);
}


int
_PL_get_arg(DECL_LD size_t index, term_t t, term_t a)
{ word w = valHandle(t);
  Functor f = (Functor)valPtr(w);
  Word p = &f->arguments[index-1];

  setHandle(a, linkValI(p));
  return TRUE;
}


int
PL_get_arg_sz(size_t index, term_t t, term_t a)
{ GET_LD
  word w = valHandle(t);

  if ( isTerm(w) && index > 0 )
  { Functor f = (Functor)valPtr(w);
    size_t arity = arityFunctor(f->definition);

    if ( --index < arity )
    { Word p = &f->arguments[index];

      setHandle(a, linkValI(p));
      succeed;
    }
  }

  fail;
}

int
(PL_get_arg)(int index, term_t t, term_t a)
{ if ( index >= 0 )
    return PL_get_arg_sz(index, t, a);
  fatalError("Index out of range: %d", index);
  return FALSE;
}

#ifdef O_ATTVAR
API_STUB(int)
(PL_get_attr)(term_t t, term_t a)
( return PL_get_attr(t, a); )
#endif


int
PL_get_list(DECL_LD term_t l, term_t h, term_t t)
{ word w = valHandle(l);

  if ( isList(w) )
  { Word a = argTermP(w, 0);

    setHandle(h, linkValI(a++));	/* safe: `a` is on global stack */
    setHandle(t, linkValI(a));

    succeed;
  }

  fail;
}


API_STUB(int)
(PL_get_list)(term_t l, term_t h, term_t t)
( return PL_get_list(l, h, t); )


int
PL_get_head(term_t l, term_t h)
{ GET_LD
  word w = valHandle(l);

  if ( isList(w) )
  { Word a = argTermP(w, 0);
    setHandle(h, linkValI(a));	/* safe: `a` is on global stack */
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
    setHandle(t, linkValI(a));	/* safe: `a` is on global stack */
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

  length = skip_list(l, &t);
  if ( len )
    *len = length;
  if ( tail )
  { Word t2 = valTermRef(tail);

    setVar(*t2);
    unify_ptrs(t2, t, 0);
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

/* PL_is_variable(DECL_LD term_t t) moved to pl-fli.h */

API_STUB(int)
(PL_is_variable)(term_t t)
( word w = valHandle(t);

  return canBind(w) ? TRUE : FALSE;
)


/* PL_is_atom(DECL_LD term_t t) moved to pl-fli.h */


API_STUB(int)
(PL_is_atom)(term_t t)
( return PL_is_atom(t); )


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


int
PL_is_rational(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isRational(w);
}


int
PL_is_compound(term_t t)
{ GET_LD
  word w = valHandle(t);

  return isTerm(w) ? TRUE : FALSE;
}


int
isCallable(DECL_LD word w)
{ if ( isTerm(w) )
  { Functor f = valueTerm(w);
    FunctorDef fd = valueFunctor(f->definition);
    Atom ap = atomValue(fd->name);

    if ( true(ap->type, PL_BLOB_TEXT) || fd->name == ATOM_nil )
      return TRUE;
    if ( ap->type == &_PL_closure_blob )
    { closure *c = (closure*)ap->name;

      if ( c->def.functor->arity == fd->arity )
	return TRUE;
    }

    return FALSE;
  }

  return isTextAtom(w) != 0;
}

int
PL_is_callable(term_t t)
{ GET_LD

  return isCallable(valHandle(t));
}

/* PL_is_functor(DECL_LD term_t t, functor_t f) moved to pl-fli.h */

API_STUB(int)
(PL_is_functor)(term_t t, functor_t f)
( word w = valHandle(t);

  if ( hasFunctor(w, f) )
    succeed;

  fail;
)


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

  return !!isList(w);
}


/* PL_is_atomic(DECL_LD term_t t) moved to pl-fli.h */


API_STUB(int)
(PL_is_atomic)(term_t t)
( word w = valHandle(t);

  return !!isAtomic(w);
)


int
PL_is_number(term_t t)
{ GET_LD
  word w = valHandle(t);

  return !!isNumber(w);
}


#ifdef O_STRING
int
PL_is_string(term_t t)
{ GET_LD
  word w = valHandle(t);

  return !!isString(w);
}

int
PL_unify_string_chars(term_t t, const char *s)
{ GET_LD
  word str = globalString(strlen(s), (char *)s);

  if ( str )
    return unifyAtomic(t, str);

  return FALSE;
}

int
PL_unify_string_nchars(term_t t, size_t len, const char *s)
{ GET_LD
  word str = globalString(len, s);

  if ( str )
    return unifyAtomic(t, str);

  return FALSE;
}
#endif /*O_STRING*/


		 /*******************************
		 *             PUT-*		*
		 *******************************/



API_STUB(int)
(PL_put_variable)(term_t t)
( return PL_put_variable(t); )


/* PL_put_atom(DECL_LD term_t t, atom_t a) moved to pl-fli.h */

API_STUB(int)
(PL_put_atom)(term_t t, atom_t a)
( setHandle(t, a);
  return TRUE;
)


int
PL_put_bool(term_t t, int val)
{ GET_LD

  PL_put_atom(t, val ? ATOM_true : ATOM_false);
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
PL_put_chars(term_t t, int flags, size_t len, const char *s)
{ GET_LD
  PL_chars_t text;
  word w = 0;
  int rc = FALSE;

  if ( len == (size_t)-1 )
    len = strlen(s);

  text.text.t    = (char*)s;
  text.encoding  = ((flags&REP_UTF8) ? ENC_UTF8 : \
		    (flags&REP_MB)   ? ENC_ANSI : ENC_ISO_LATIN_1);
  text.length    = len;
  text.canonical = FALSE;
  text.storage   = PL_CHARS_HEAP;

  flags &= ~(REP_UTF8|REP_MB|REP_ISO_LATIN_1);

  if ( flags == PL_ATOM )
    w = textToAtom(&text);
  else if ( flags == PL_STRING )
    w = textToString(&text);
  else if ( flags == PL_CODE_LIST || flags == PL_CHAR_LIST )
  { PL_put_variable(t);
    rc = PL_unify_text(t, 0, &text, flags);
  } else
    assert(0);

  if ( w )
  { setHandle(t, w);
    rc = TRUE;
  }

  PL_free_text(&text);

  return rc;
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


/* PL_put_int64(DECL_LD term_t t, int64_t i) moved to pl-fli.h */
/* PL_put_integer(DECL_LD term_t t, long i) moved to pl-fli.h */
/* PL_put_intptr(DECL_LD term_t t, intptr_t i) moved to pl-fli.h */


API_STUB(int)
(PL_put_int64)(term_t t, int64_t i)
( return PL_put_int64(t, i); )

int
PL_put_uint64(term_t t, uint64_t i)
{ GET_LD
  word w;
  int rc;

  switch ( (rc=put_uint64(&w, i, ALLOW_GC)) )
  { case TRUE:
      return setHandle(t, w);
    case LOCAL_OVERFLOW:
      return PL_representation_error("uint64_t");
    default:
      return raiseStackOverflow(rc);
  }
}


API_STUB(int)
(PL_put_integer)(term_t t, long i)
( return PL_put_int64(t, i); )


int
_PL_put_number(DECL_LD term_t t, Number n)
{ word w;
  int rc;

  if ( (rc=put_number(&w, n, ALLOW_GC)) == TRUE )
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

  return PL_put_int64(t, (int64_t)i);
}


int
PL_put_float(term_t t, double f)
{ GET_LD
  word w;
  int rc;

  if ( (rc=put_double(&w, f, ALLOW_GC)) == TRUE )
  { setHandle(t, w);
    return TRUE;
  }

  return raiseStackOverflow(rc);
}


int
PL_put_functor(term_t t, functor_t f)
{ GET_LD
  size_t arity = arityFunctor(f);

  if ( arity == 0 )
  { setHandle(t, nameFunctor(f));
  } else
  { Word a;

    VALID_TERM_ARITY(arity);
    if ( !(a = allocGlobal(1 + arity)) )
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
PL_put_term(DECL_LD term_t t1, term_t t2)
{ if ( globalizeTermRef(t2) )
  { Word p2 = valHandleP(t2);
    setHandle(t1, linkValI(p2));
    return TRUE;
  }

  return FALSE;
}


API_STUB(int)
(PL_put_term)(term_t t1, term_t t2)
( return PL_put_term(t1, t2); )


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
    put_int64(&w, i, 0);

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
PL_unify_atom(DECL_LD term_t t, atom_t a)
{ return unifyAtomic(t, a);
}

API_STUB(int)
(PL_unify_atom)(term_t t, atom_t a)
( return unifyAtomic(t, a); )


int
PL_unify_compound(term_t t, functor_t f)
{ GET_LD
  Word p = valHandleP(t);
  size_t arity = arityFunctor(f);

  deRef(p);
  if ( canBind(*p) )
  { size_t needed = (1+arity);
    Word a;
    word to;

    VALID_TERM_ARITY(arity);
    if ( !hasGlobalSpace(needed) )
    { int rc;

      if ( (rc=ensureGlobalSpace(needed, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
      p = valHandleP(t);		/* reload: may have shifted */
      deRef(p);
    }

    a = gTop;
    to = consPtr(a, TAG_COMPOUND|STG_GLOBAL);

    gTop += 1+arity;
    *a = f;
    while( arity-- > 0 )
      setVar(*++a);

    bindConst(p, to);

    succeed;
  } else
  { return hasFunctor(*p, f);
  }
}


int
PL_unify_functor(DECL_LD term_t t, functor_t f)
{ Word p = valHandleP(t);
  size_t arity = arityFunctor(f);

  deRef(p);
  if ( canBind(*p) )
  { size_t needed = (1+arity);

    VALID_TERM_ARITY(arity);
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
      while( arity-- > 0 )
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



API_STUB(int)
(PL_unify_functor)(term_t t, functor_t f)
( return PL_unify_functor(t, f); )


int
PL_unify_atom_chars(term_t t, const char *chars)
{ GET_LD
  atom_t a = lookupAtom(chars, strlen(chars));
  int rval = unifyAtomic(t, a);

  PL_unregister_atom(a);

  return rval;
}


int
PL_unify_atom_nchars(term_t t, size_t len, const char *chars)
{ GET_LD
  atom_t a = lookupAtom(chars, len);
  int rval = unifyAtomic(t, a);

  PL_unregister_atom(a);

  return rval;
}


static atom_t
uncachedCodeToAtom(int chrcode)
{ if ( chrcode < 256 )
  { char tmp[1];

    tmp[0] = (char)chrcode;
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
unify_int64_ex(DECL_LD term_t t, int64_t i, int ex)
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
      put_int64(&w, i, 0);

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
PL_unify_int64_ex(DECL_LD term_t t, int64_t i)
{ return unify_int64_ex(t, i, TRUE);
}

int
PL_unify_int64(DECL_LD term_t t, int64_t i)
{ return unify_int64_ex(t, i, FALSE);
}

int
PL_unify_uint64(term_t t, uint64_t i)
{ GET_LD

  if ( (int64_t)i >= 0 )
  { return unify_int64_ex(t, i, TRUE);
  } else if ( PL_is_variable(t) )
  { word w;
    int rc;

    switch ( (rc=put_uint64(&w, i, ALLOW_GC)) )
    { case TRUE:
	return unifyAtomic(t, w);
      case LOCAL_OVERFLOW:
	return PL_representation_error("uint64_t");
      default:
	return raiseStackOverflow(rc);
    }
  } else
  { number n;

    if ( PL_get_number(t, &n) )
    { switch(n.type)
      { case V_INTEGER:
	  return FALSE;			/* we have a too big integer */
#ifdef O_GMP
	case V_MPZ:
	{ uint64_t v;

	  if ( mpz_to_uint64(n.value.mpz, &v) == 0 )
	    return v == i;
	}
#endif
	default:
	  break;
      }
    }

    return FALSE;
  }
}

int
PL_unify_integer(DECL_LD term_t t, intptr_t i)
{ word w = consInt(i);

  if ( valInt(w) == i )
    return unifyAtomic(t, w);

  return unify_int64_ex(t, i, FALSE);
}


API_STUB(int)
(PL_unify_integer)(term_t t, intptr_t i)
( return unify_int64_ex(t, i, FALSE); )

API_STUB(int)
(PL_unify_int64)(term_t t, int64_t i)
( return unify_int64_ex(t, i, FALSE); )

int
PL_unify_pointer(DECL_LD term_t t, void *ptr)
{ uint64_t i = pointerToInt(ptr);

  return unify_int64_ex(t, (int64_t)i, FALSE);
}


API_STUB(int)
(PL_unify_pointer)(term_t t, void *ptr)
( return PL_unify_pointer(t, ptr); )


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

    put_double(&w, f, ALLOW_CHECKED);
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
PL_unify_arg_sz(size_t index, term_t t, term_t a)
{ GET_LD
  word w = valHandle(t);

  if ( isTerm(w) &&
       index > 0 &&
       index <=	arityFunctor(functorTerm(w)) )
  { Word p = argTermP(w, index-1);
    Word p2 = valHandleP(a);

    return unify_ptrs(p, p2, ALLOW_GC|ALLOW_SHIFT);
  }

  fail;
}

int
(PL_unify_arg)(int index, term_t t, term_t a)
{ if ( index >= 0 )
    return PL_unify_arg_sz(index, t, a);
  fatalError("Index out of range: %d", index);
  return FALSE;
}

int
PL_unify_list(DECL_LD term_t l, term_t h, term_t t)
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

    setHandle(h, linkValI(a++));	/* safe: `a` is on global stack */
    setHandle(t, linkValI(a));
  } else
    fail;

  succeed;
}


API_STUB(int)
(PL_unify_list)(term_t l, term_t h, term_t t)
( return PL_unify_list(l, h, t); )


int
PL_unify_nil(term_t l)
{ GET_LD
  return unifyAtomic(l, ATOM_nil);
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
      size_t arity;			/* arity of the term */
      size_t arg;			/* current argument */
    } term;
    struct
    { term_t tail;			/* tail of list */
      int    len;			/* elements left */
    } list;
  } value;
} work;


int
PL_unify_termv(DECL_LD term_t t, va_list args)
{ term_t tsave = PL_new_term_refs(0);	/* save for reclaim */
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
    size_t arity;

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

      rval = unify_definition(MODULE_user, t, proc->definition,
			      0, GP_HIDESYSTEM|GP_NAMEARITY);
      break;
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

API_STUB(int)
(PL_unify_termv)(term_t t, va_list args)
( return PL_unify_termv(t, args); )


int
PL_unify_term(DECL_LD term_t t, ...)
{ va_list args;
  int rval;

  va_start(args, t);
  rval = PL_unify_termv(t, args);
  va_end(args);

  return rval;
}


API_STUB(int)
(PL_unify_term)(term_t t, ...)
( va_list args;
  int rval;

  va_start(args, t);
  rval = PL_unify_termv(t, args);
  va_end(args);

  return rval;
)

#define put_xpce_ref_arg(ref) LDFUNC(put_xpce_ref_arg, ref)
static inline word
put_xpce_ref_arg(DECL_LD xpceref_t *ref)
{ if ( ref->type == PL_INTEGER )
  { word w = consInt(ref->value.i);

    if ( valInt(w) != ref->value.i )
      put_int64(&w, ref->value.i, 0);

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
      *a++ = put_xpce_ref_arg(ref);

      bindConst(p, c);
      succeed;
    }
    if ( hasFunctor(*p, FUNCTOR_xpceref1) )
    { Word a = argTermP(*p, 0);

      deRef(a);
      if ( canBind(*a) )
      { word c = put_xpce_ref_arg(ref);

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


API_STUB(int)
(_PL_unify_atomic)(term_t t, PL_atomic_t a)
( return unifyAtomic(t, a); )


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
  int rval = unifyAtomic(t, a);

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
  if ( unlikely(x->type == ATOM_TYPE_INVALID) )
  { if ( type )
      *type = NULL;
    return NULL;
  }

  if ( type )
    *type = x->type;

  return x->name;
}


		 /*******************************
		 *	       DICT		*
		 *******************************/

int
PL_put_dict(term_t t, atom_t tag,
	    size_t len, const atom_t *keys, term_t values)
{ GET_LD
  Word p, p0;
  size_t size = len*2+2;
  size_t i;

  for(i=0; i<len; i++)
  { if ( !globalizeTermRef(values+i) )
      return FALSE;
  }

  if ( (p0=p=allocGlobal(size)) )
  { *p++ = dict_functor(len);
    if ( tag )
    { if ( isAtom(tag) )
      { *p++ = tag;
      } else
      { invalid:
	gTop -= size;
	return -1;
      }
    } else
    { setVar(*p++);
    }

    for(; len-- > 0; keys++, values++)
    { *p++ = linkValI(valTermRef(values));
      if ( is_dict_key(*keys) )
	*p++ = *keys;
      else
	goto invalid;
    }

    if ( dict_order(p0, NULL) == TRUE )
    { setHandle(t, consPtr(p0, TAG_COMPOUND|STG_GLOBAL));
      DEBUG(CHK_SECURE, checkStacks(NULL));
      return TRUE;
    }

    gTop -= size;
    return -2;
  }

  return FALSE;
}


		 /*******************************
		 *	       TYPE		*
		 *******************************/


int
PL_term_type(term_t t)
{ GET_LD
  word w = valHandle(t);
  int t0 = type_map[tag(w)];

  switch(t0)
  { case PL_ATOM:
    { if ( isTextAtom(w) )
	return t0;
      if ( w == ATOM_nil )
	return PL_NIL;
      return PL_BLOB;
    }
    case PL_INTEGER:
    { return (isInteger(w) ? PL_INTEGER : PL_RATIONAL);
    }
    case PL_TERM:
    { functor_t f = valueTerm(w)->definition;
      FunctorDef fd = valueFunctor(f);

      if ( f == FUNCTOR_dot2 )
	return PL_LIST_PAIR;
      if ( fd->name == ATOM_dict )
	return PL_DICT;
    }
    /*FALLTHROUGH*/
    default:
      return t0;
  }
}


		 /*******************************
		 *	      UNIFY		*
		 *******************************/



int
PL_unify(DECL_LD term_t t1, term_t t2)
{ Word p1 = valHandleP(t1);
  Word p2 = valHandleP(t2);

  return unify_ptrs(p1, p2, ALLOW_GC|ALLOW_SHIFT);
}


API_STUB(int)
(PL_unify)(term_t t1, term_t t2)
( return PL_unify(t1, t2); )


/*
 * Unify an output argument.  Only deals with the simple case
 * where the output argument is unbound and the value is bound.
 */

int
PL_unify_output(DECL_LD term_t t1, term_t t2)
{ Word p1 = valHandleP(t1);
  Word p2 = valHandleP(t2);

  deRef(p1);
  deRef(p2);
  if ( canBind(*p1) && !canBind(*p2) &&
       hasGlobalSpace(0) )
  { bindConst(p1, *p2);
    return TRUE;
  } else
  { return unify_ptrs(p1, p2, ALLOW_GC|ALLOW_SHIFT);
  }
}



		 /*******************************
		 *	       MODULES		*
		 *******************************/

int
PL_strip_module_flags(DECL_LD term_t raw, module_t *m, term_t plain, int flags)
{ Word p = valTermRef(raw);

  deRef(p);
  if ( hasFunctor(*p, FUNCTOR_colon2) )
  { if ( !(p = stripModule(p, m, flags)) )
      return FALSE;
    setHandle(plain, linkValI(p));
  } else
  { if ( *m == NULL )
      *m = environment_frame ? contextModule(environment_frame)
			     : MODULE_user;
    if ( raw != plain )
    { word w = linkValG(p);

      if ( w )
	setHandle(plain, w);
      else
	return FALSE;
    }
  }

  return TRUE;
}

API_STUB(int)
(PL_strip_module)(term_t raw, module_t *m, term_t plain)
( return PL_strip_module_flags(raw, m, plain, 0); )

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_strip_module_ex() is similar to  PL_strip_module(),   but  returns an
error if it encounters a term <m>:<t>, where <m> is not an atom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_strip_module_ex(DECL_LD term_t raw, module_t *m, term_t plain)
{ Word p;

  globalizeTermRef(raw);
  p = valTermRef(raw);
  deRef(p);
  if ( hasFunctor(*p, FUNCTOR_colon2) )
  { if ( !(p = stripModule(p, m, 0)) )
      return FALSE;
    if ( hasFunctor(*p, FUNCTOR_colon2) )
    { Word a1 = argTermP(*p, 0);
      deRef(a1);
      setHandle(plain, needsRef(*a1) ? makeRefG(a1) : *a1);
      return PL_type_error("module", plain);
    }
    setHandle(plain, linkValI(p));
  } else
  { word w;

    if ( *m == NULL )
      *m = environment_frame ? contextModule(environment_frame)
			     : MODULE_user;
    if ( (w=linkValG(p)) )
      setHandle(plain, w);
    else
      return FALSE;
  }

  return TRUE;
}

module_t
PL_context(void)
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
{ GET_LD
  return lookupModule(name);
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
  { GET_LD
    a = lookupAtom(module, strlen(module));
    m = lookupModule(a);
    PL_unregister_atom(a);
  } else
    m = PL_context();

  return PL_pred(f, m);
}


/* _PL_predicate(const char *name, int arity, const char *module, moved to pl-fli.h */


int
PL_predicate_info(predicate_t pred, atom_t *name, size_t *arity, module_t *m)
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
  { int r1 = PL_next_solution(qid);
    int r2 = PL_cut_query(qid);

    rval = (r1 && r2);	/* do not inline; we *must* execute PL_cut_query() */
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

foreign_t
_PL_yield_address(void *v)
{ if ( (uintptr_t)v & FRG_REDO_MASK )
    PL_fatal_error("PL_yield_address(%p): bad alignment", v);

  ForeignYieldPtr(v);
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

predicate_t				/* = Procedure */
PL_foreign_context_predicate(control_t h)
{ GET_LD
  Definition def = h->predicate;

  return isCurrentProcedure(def->functor->functor, def->module);
}

int
has_emergency_space(void *sv, size_t needed)
{ Stack s = (Stack) sv;
  ssize_t lacking = ((char*)s->top + needed) - (char*)s->max;

  if ( lacking <= 0 )
    return TRUE;
  if ( lacking < s->spare )
  { s->max    = (char*)s->max + lacking;
    s->spare -= lacking;
    return TRUE;
  }

  return FALSE;
}


#define copy_exception(ex, bin) LDFUNC(copy_exception, ex, bin)
static int
copy_exception(DECL_LD term_t ex, term_t bin)
{ fid_t fid;

  if ( (fid=PL_open_foreign_frame()) )
  { if ( duplicate_term(ex, bin) )
    { ok:
      PL_close_foreign_frame(fid);
      return TRUE;
    } else
    { PL_rewind_foreign_frame(fid);
      PL_clear_exception();
      LD->exception.processing = TRUE;

      if ( PL_is_functor(ex, FUNCTOR_error2) )
      { term_t arg, av;

	if ( (arg = PL_new_term_ref()) &&
	     (av  = PL_new_term_refs(2)) &&
	     PL_get_arg(1, ex, arg) &&
	     duplicate_term(arg, av+0) &&
	     PL_cons_functor_v(bin, FUNCTOR_error2, av) )
	{ Sdprintf("WARNING: Removed error context due to stack overflow\n");
	  goto ok;
	}
      } else if ( has_emergency_space(&LD->stacks.global, 5*sizeof(word)) )
      { Word p = gTop;

	Sdprintf("WARNING: cannot raise exception; raising global overflow\n");
	p[0] = FUNCTOR_error2;			/* see (*) above */
	p[1] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
	p[2] = ATOM_global;
	p[3] = FUNCTOR_resource_error1;
	p[4] = ATOM_stack;
	gTop += 5;

	*valTermRef(bin) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
	goto ok;
      }
    }
    PL_close_foreign_frame(fid);
  }

  Sdprintf("WARNING: mapped exception to abort due to stack overflow\n");
  PL_put_atom(bin, ATOM_aborted);
  return TRUE;
}


except_class
classify_exception_p(DECL_LD Word p)
{ deRef(p);
  if ( isVar(*p) )
  { return EXCEPT_NONE;
  } else if ( isAtom(*p) )
  { if ( *p == ATOM_aborted )
      return EXCEPT_ABORT;
    if ( *p == ATOM_time_limit_exceeded )
      return EXCEPT_TIMEOUT;
  } else if ( hasFunctor(*p, FUNCTOR_error2) )
  { p = argTermP(*p, 0);
    deRef(p);

    if ( isAtom(*p) )
    { if ( *p == ATOM_resource_error )
	return EXCEPT_RESOURCE;
    }

    return EXCEPT_ERROR;
  }

  return EXCEPT_OTHER;
}


/* classify_exception(DECL_LD term_t exception) moved to pl-fli.h */


int
PL_raise_exception(term_t exception)
{ GET_LD

  assert(valTermRef(exception) < (Word)lTop);

  if ( PL_is_variable(exception) )	/* internal error */
    fatalError("Cannot throw variable exception");

#if O_DEBUG
  save_backtrace("exception");
#endif

  LD->exception.processing = TRUE;
  if ( !PL_same_term(exception, exception_bin) ) /* re-throwing */
  { except_class co = classify_exception(exception_bin);
    except_class cn = classify_exception(exception);

    if ( cn >= co )
    { if ( cn == EXCEPT_RESOURCE )
	enableSpareStacks();
      setVar(*valTermRef(exception_bin));
      copy_exception(exception, exception_bin);
      if ( !PL_is_atom(exception_bin) )
	freezeGlobal();
    }
  }
  exception_term = exception_bin;

  return FALSE;
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
  { resumeAfterException(TRUE, LD->outofstack);
    LD->outofstack = NULL;
  }
}


void
PL_clear_foreign_exception(LocalFrame fr)
{ GET_LD
  term_t ex = PL_exception(0);
  fid_t fid;

#ifdef O_PLMT
{ int tid = PL_thread_self();
  atom_t alias;
  const pl_wchar_t *name = L"";

  if ( PL_get_thread_alias(tid, &alias) )
    name = PL_atom_wchars(alias, NULL);

  Sdprintf("Thread %d (%Ws): foreign predicate %s did not clear exception: \n\t",
	   tid, name, predicateName(fr->predicate));
#if O_DEBUG
  print_backtrace_named("exception");
#endif
}
#else
  Sdprintf("Foreign predicate %s did not clear exception: ",
	   predicateName(fr->predicate));
#endif

  if ( (fid=PL_open_foreign_frame()) )
  { PL_write_term(Serror, ex, 1200, PL_WRT_NEWLINE);
    PL_close_foreign_frame(fid);
  }

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
  if ( def->module != m || def->impl.any.defined )
  { DEBUG(MSG_PROC, Sdprintf("Abolish %s from %s\n",
			     procedureName(proc), PL_atom_chars(m->name)));
    abolishProcedure(proc, m);
    def = proc->definition;
  }

  if ( def->impl.any.defined )
    PL_linger(def->impl.any.defined);	/* Dubious: what if a clause list? */
  def->impl.foreign.function = f;
  def->flags &= ~(P_DYNAMIC|P_THREAD_LOCAL|P_TRANSPARENT|P_NONDET|P_VARARG);
  def->flags |= (P_FOREIGN|TRACE_ME);

  if ( m == MODULE_system || SYSTEM_MODE )
    set(def, P_LOCKED|HIDE_CHILDS);

  if ( (flags & PL_FA_NOTRACE) )	  clear(def, TRACE_ME);
  if ( (flags & PL_FA_TRANSPARENT) )	  set(def, P_TRANSPARENT);
  if ( (flags & PL_FA_NONDETERMINISTIC) ) set(def, P_NONDET);
  if ( (flags & PL_FA_VARARGS) )	  set(def, P_VARARG);
  if ( (flags & PL_FA_SIG_ATOMIC) )	  set(def, P_SIG_ATOMIC);

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
    return (HAS_LD && environment_frame ? contextModule(environment_frame)
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
    ext->arity = (short)arity;
    ext->function = f;
    ext->flags = (short)flags;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The    system    may     be      compiled     using     AddressSanitizer
(https://github.com/google/sanitizers/wiki/AddressSanitizer)  which   is
supported by GCC and Clang. Do do so, use

    cmake -DCMAKE_BUILD_TYPE=Sanitize

See cmake/BuildType.cmake for details.

Currently SWI-Prolog does not reclaim all memory   on  edit, even not if
cleanupProlog() is called with reclaim_memory set to TRUE. The docs says
we can use __lsan_disable() just before exit   to  avoid the leak check,
but this doesn't seem to work (Ubuntu 18.04). What does work is defining
__asan_default_options(), providing an alternative   to  the environment
variable LSAN_OPTIONS=.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
haltProlog(int status)
{ status |= PL_CLEANUP_NO_RECLAIM_MEMORY;

#if defined(GC_DEBUG) || defined(O_DEBUG) || defined(__SANITIZE_ADDRESS__)
  status &= ~PL_CLEANUP_NO_RECLAIM_MEMORY;
#endif

  switch( PL_cleanup(status) )
  { case PL_CLEANUP_CANCELED:
    case PL_CLEANUP_RECURSIVE:
      return FALSE;
    default:
      run_on_halt(&GD->os.exit_hooks, status);
      return TRUE;
  }
}

int
PL_halt(int status)
{ if ( haltProlog(status) )
    exit(status);

  return FALSE;
}

#ifndef SIGABRT
#define SIGABRT 6			/* exit 134 --> aborted */
#endif

void
PL_abort_process(void)
{ haltProlog(128+SIGABRT);
  abort();
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
  predicate_t pred;
  term_t t0;

  (void)rc_class;

  if ( !m )
    m = MODULE_user;
  pred = _PL_predicate("c_open_resource", 3, "$rc",
		       &GD->procedures.c_open_resource3);

  if ( !(fid = PL_open_foreign_frame()) )
  { errno = ENOENT;
    return s;
  }
  t0 = PL_new_term_refs(3);
  PL_put_atom_chars(t0+0, name);
  PL_put_atom_chars(t0+1, mode);

  if ( !PL_call_predicate(m, PL_Q_CATCH_EXCEPTION, pred, t0) ||
       !PL_get_stream_handle(t0+2, &s) )
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


/* PL_pending(DECL_LD int sig) moved to pl-fli.h */


int
PL_clearsig(DECL_LD int sig)
{ if ( IS_VALID_SIGNAL(sig) && HAS_LD )
  { WSIGMASK_CLEAR(LD->signal.pending, sig);
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


void
cleanAbortHooks(PL_local_data_t *ld)
{ WITH_LD(ld)
  { AbortHandle next;

    for(AbortHandle h = abort_head; h; h=next)
    { next = h->next;
      freeHeap(h, sizeof(*h));
    }
    abort_head = abort_tail = NULL;
  }
}

int
PL_abort_unhook(PL_abort_hook_t func)
{ GET_LD
  AbortHandle h = abort_head;
  AbortHandle prev = NULL;

  for(; h; h = h->next)
  { if ( h->function == func )
    { h->function = NULL;
      if ( prev )
	prev->next = h->next;
      else
	abort_head = h->next;
      if ( !h->next )
	abort_tail = prev;
      freeHeap(h, sizeof(*h));
      return TRUE;
    }
    prev = h;
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

int
PL_ttymode(IOSTREAM *s)
{ GET_LD

  if ( s == Suser_input )
  { if ( !truePrologFlag(PLFLAG_TTY_CONTROL) ) /* -tty in effect */
      return PL_NOTTY;
    if ( Sttymode(s) == TTY_RAW )	/* get_single_char/1 and friends */
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
{ GET_LD
  IOSTREAM *s;

  if ( (s=Suser_input) && fd == Sfileno(s) )
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
#if defined(HAVE_POLL_H) && defined(HAVE_POLL)
#include <poll.h>
#elif defined(HAVE_SYS_SELECT_H)
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
{
#ifdef HAVE_POLL
  struct pollfd fds[1];

  fds[0].fd = fd;
  fds[0].events = POLLIN;

  return poll(fds, 1, 0) != 0;
#else
  fd_set rfds;
  struct timeval tv;

#if defined(FD_SETSIZE) && !defined(__WINDOWS__)
  if ( fd >= FD_SETSIZE )
  { Sdprintf("input_on_fd(%d) > FD_SETSIZE\n", fd);
    return 1;
  }
#endif

  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  return select(fd+1, &rfds, NULL, NULL, &tv) != 0;
#endif
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

  return copyRecordToGlobal(t, r, ALLOW_GC) == TRUE;
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
{ GET_LD
  va_list args;
  int rval = TRUE;
  int flags = (type & FF_MASK);
  fid_t fid;
  term_t av;

  va_start(args, type);
  if ( HAS_LD &&
       GD->io_initialised &&			/* setupProlog() finished */
       (fid = PL_open_foreign_frame()) &&
       (av  = PL_new_term_refs(2)) )
  { PL_put_atom_chars(av+0, name);
    switch(type & ~FF_MASK)
    { case PL_BOOL:
      { int val = va_arg(args, int);

	rval = ( PL_put_bool(av+1, val) &&
		 set_prolog_flag(av+0, av+1, FT_BOOL|flags) );
	break;
      }
      case PL_ATOM:
      { const char *v = va_arg(args, const char *);

	rval = ( PL_put_atom_chars(av+1, v) &&
		 set_prolog_flag(av+0, av+1, FT_ATOM|flags) );
	break;
      }
      case PL_INTEGER:
      { intptr_t v = va_arg(args, intptr_t);

	rval = ( PL_put_integer(av+1, v) &&
		 set_prolog_flag(av+0, av+1, FT_INTEGER|flags) );
	break;
      }
      default:
	rval = FALSE;
    }
    PL_close_foreign_frame(fid);
  } else
  { initPrologThreads();

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
		 LD->gc.stats.totals.collections);
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
    case PL_ACTION_TRADITIONAL:
      setTraditional();
      break;
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

      if ( HAS_LD )
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
		 *	      VERSION		*
		 *******************************/

unsigned int
PL_version_info(int which)
{ switch(which)
  { case PL_VERSION_SYSTEM:	return PLVERSION;
    case PL_VERSION_FLI:	return PL_FLI_VERSION;
    case PL_VERSION_REC:	return PL_REC_VERSION;
    case PL_VERSION_QLF:	return PL_QLF_VERSION;
    case PL_VERSION_QLF_LOAD:	return PL_QLF_LOADVERSION;
    case PL_VERSION_VM:		return VM_SIGNATURE;
    case PL_VERSION_BUILT_IN:	return GD->foreign.signature;
    default:			return 0;
  }
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

void
initForeign(void)
{ initUCSAtoms();
}
