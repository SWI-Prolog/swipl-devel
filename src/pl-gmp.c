/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2024, University of Amsterdam
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
#include <math.h>
#include <fenv.h>
#include <float.h>
#include "pl-gmp.h"
#include "pl-arith.h"
#include "pl-pro.h"
#include "pl-fli.h"
#include "pl-gc.h"
#include "pl-attvar.h"
#include "pl-inline.h"
#include "pl-comp.h"
#undef LD
#define LD LOCAL_LD

typedef union
{ double  d;
  int64_t i;
} fpattern;

#ifdef O_BIGNUM				/* Upto the end of this file */

static mpz_t MPZ_MIN_TAGGED;		/* Prolog tagged integers */
static mpz_t MPZ_MAX_TAGGED;
#ifndef O_BF
static mpz_t MPZ_MIN_INT64;		/* int64_t integers */
static mpz_t MPZ_MAX_INT64;
#endif
static mpz_t MPZ_MAX_UINT64;
#if SIZEOF_LONG	< SIZEOF_WORD
static mpz_t MPZ_MIN_LONG;		/* Prolog int64_t integers */
static mpz_t MPZ_MAX_LONG;
#endif

#define abs(v) ((v) < 0 ? -(v) : (v))

		 /*******************************
		 *	 MEMORY MANAGEMENT	*
		 *******************************/

#if O_MY_GMP_ALLOC
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GMP doesn't (yet) allow for handling  memory overflows. You can redefine
the allocation handles, but you are not  allowed to return NULL or abort
the execution using longjmp(). As our  normal   GMP  numbers live on the
global stack, we however can  cleanup   the  temporary  numbers that are
created during the Prolog function evaluation  and use longjmp() through
STACK_OVERFLOW_THROW.   Patrick Pelissier acknowledged this should work.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void *(*smp_alloc)(size_t);
static void *(*smp_realloc)(void *, size_t, size_t);
static void  (*smp_free)(void *, size_t);

#define NOT_IN_PROLOG_ARITHMETIC() \
	(LD == NULL || LD->gmp.context == NULL)

#define ROUND_SIZE(n) (((n) + (sizeof(size_t) - 1))/sizeof(size_t))

typedef enum
{ GMO_TB_OK = 0,
  GMP_TB_RESTRAINT,
  GMP_TB_STACK,
  GMP_TB_MALLOC
} gmp_tb;

#define gmp_too_big(why)      LDFUNC(gmp_too_big, why)
#define gmp_check_size(bytes) LDFUNC(gmp_check_size, bytes)

static void*			/* Actually, does not return */
gmp_too_big(DECL_LD gmp_tb why)
{ DEBUG(MSG_GMP_OVERFLOW, Sdprintf("Signalling GMP overflow\n"));

  switch(why)
  { case GMP_TB_RESTRAINT:
    { number max = { .type = V_INTEGER };
      max.value.i = LD->gmp.max_integer_size;
      PL_error(NULL, 0, "requires more than max_integer_size bytes",
	       ERR_AR_TRIPWIRE, ATOM_max_integer_size, &max);
      PL_rethrow();
    }
    case GMP_TB_STACK:
      outOfStack((Stack)&LD->stacks.global, STACK_OVERFLOW_THROW);
      break;
    case GMP_TB_MALLOC:
    default:
      PL_no_memory();
      PL_rethrow();
  }

  abortProlog();		/* Just in case the above fails */
  PL_rethrow();
  return false;
}


static int
gmp_check_size(DECL_LD size_t bytes)
{ gmp_tb why = GMO_TB_OK;

  if ( bytes <= 1000 )
    return true;
  if ( bytes > LD->gmp.max_integer_size )
    why = GMP_TB_RESTRAINT;
  else if ( bytes > (size_t)globalStackLimit())
    why = GMP_TB_STACK;
  else
    return true;

  gmp_too_big(why);
  return false;
}


#define TOO_BIG_GMP(n) ((n) > 1000 &&			   \
			((n) > LD->gmp.max_integer_size || \
			 (n) > (size_t)globalStackLimit()))

static void *
mp_alloc(size_t bytes)
{ GET_LD
  ar_context *ctx;
  mp_mem_header *mem;

  if ( NOT_IN_PROLOG_ARITHMETIC() )
    return smp_alloc(bytes);

  if ( !gmp_check_size(bytes) )
    return NULL;

#if O_BF
  if ( bytes == 0 )
    return NULL;
#endif
  ctx = LD->gmp.context;

  size_t fastunits = ROUND_SIZE(bytes)+1;
  if ( ctx->allocated+fastunits <= GMP_STACK_ALLOC )
  { size_t *data = &ctx->alloc_buf[ctx->allocated];
    *data++ = fastunits;
    ctx->allocated += fastunits;
    DEBUG(MSG_GMP_ALLOC, Sdprintf("GMP: from stack %zd@%p\n", bytes, data));
    return data;
  }

  if ( (mem = tmp_malloc(sizeof(mp_mem_header)+bytes)) )
  { mem->next = NULL;
    if ( ctx->tail )
    { mem->prev = ctx->tail;
      ctx->tail->next = mem;
      ctx->tail = mem;
    } else
    { mem->prev = NULL;
      ctx->head = ctx->tail = mem;
    }
    DEBUG(MSG_GMP_ALLOC, Sdprintf("GMP: malloc %zd@%p\n", bytes, &mem[1]));

    return &mem[1];
  } else
    return gmp_too_big(GMP_TB_MALLOC);
}


static inline size_t *
mp_on_stack(ar_context *ctx, void *ptr)
{ if ( ptr > (void*)ctx->alloc_buf && ptr < (void*)&ctx->alloc_buf[GMP_STACK_ALLOC] )
    return &((size_t*)ptr)[-1];

  return NULL;
}


static void
mp_free(void *ptr, size_t size)
{ GET_LD
  ar_context *ctx;
  mp_mem_header *mem;

  if ( NOT_IN_PROLOG_ARITHMETIC() )
  { smp_free(ptr, size);
    return;
  }

#if O_BF
  if ( !ptr )
    return;
#endif
  ctx = LD->gmp.context;
  size_t *base = mp_on_stack(ctx, ptr);
  if ( base )
  { if ( (base-ctx->alloc_buf) + base[0] == ctx->allocated )
      ctx->allocated -= base[0];
    return;
  }

  mem = ((mp_mem_header*)ptr)-1;

  if ( mem == ctx->head )
  { ctx->head = ctx->head->next;
    if ( ctx->head )
      ctx->head->prev = NULL;
    else
      ctx->tail = NULL;
  } else if ( mem == ctx->tail )
  { ctx->tail = ctx->tail->prev;
    ctx->tail->next = NULL;
  } else
  { mem->prev->next = mem->next;
    mem->next->prev = mem->prev;
  }

  tmp_free(mem);
  DEBUG(MSG_GMP_ALLOC, Sdprintf("GMP: free: %zd@%p\n", size, ptr));
}


static void *
mp_realloc(void *ptr, size_t oldsize, size_t newsize)
{ GET_LD
  ar_context *ctx;
  mp_mem_header *oldmem, *newmem;

  if ( NOT_IN_PROLOG_ARITHMETIC() )
    return smp_realloc(ptr, oldsize, newsize);

#if O_BF
  if ( !ptr )
    return mp_alloc(newsize);
  if ( !newsize )
  { mp_free(ptr, oldsize);
    return NULL;
  }
#endif

  if ( newsize > oldsize && !gmp_check_size(newsize) )
    return NULL;

  ctx = LD->gmp.context;
  size_t *base = mp_on_stack(ctx, ptr);
  if ( base )
  { size_t fastunits = ROUND_SIZE(newsize)+1;
    size_t alloc0 = base-ctx->alloc_buf;

    if ( alloc0 + base[0] == ctx->allocated ) /* at the top */
    { if ( alloc0+fastunits-1 <= GMP_STACK_ALLOC )
      { base[0] = fastunits;		      /* and still fits */
	ctx->allocated = alloc0+fastunits;
	return ptr;
      }
    } else if ( fastunits <= base[0] )	      /* shrink */
    { base[0] = fastunits;
      return ptr;
    }

    void *new = mp_alloc(newsize);
    if ( new )
    { size_t cp = base[0]*sizeof(size_t);
      if ( newsize < cp )
	cp = newsize;
      memcpy(new, ptr, cp);
    }
    return new;
  }

  oldmem = ((mp_mem_header*)ptr)-1;
  if ( (newmem = tmp_realloc(oldmem, sizeof(mp_mem_header)+newsize)) )
  { if ( oldmem != newmem )		/* re-link if moved */
    { if ( newmem->prev )
	newmem->prev->next = newmem;
      else
	ctx->head = newmem;

      if ( newmem->next )
	newmem->next->prev = newmem;
      else
	ctx->tail = newmem;
    }

    DEBUG(MSG_GMP_ALLOC, Sdprintf("GMP: realloc %zd@%p --> %zd@%p\n", oldsize, ptr, newsize, &newmem[1]));
    return &newmem[1];
  } else
    return gmp_too_big(GMP_TB_MALLOC);
}


void
mp_cleanup(ar_context *ctx)
{ mp_mem_header *mem, *next;

  for(mem=ctx->head; mem; mem=next)
  { next = mem->next;
    DEBUG(MSG_GMP_ALLOC, Sdprintf("GMP: cleanup of %p\n", &mem[1]));
    mp_free(&mem[1], 0);
  }
}

#ifdef O_DEBUG
static int
mp_test_alloc(void)
{ GET_LD
  AR_CTX;

  AR_BEGIN();

  char *first  = mp_alloc(8);
  strcpy(first, "hello");
  char *second = mp_alloc(8);
  assert(second-first == sizeof(size_t)+8);
  /* realloc top */
  char *ext = mp_realloc(second, 8, 12);
  assert(ext == second);
  assert(__PL_ar_ctx.allocated == ROUND_SIZE(8)+1+ROUND_SIZE(12)+1);
  /* free top */
  mp_free(ext, 12);
  assert(__PL_ar_ctx.allocated == ROUND_SIZE(8)+1);
  /* re-add second */
  second = mp_alloc(8);
  assert(second-first == sizeof(size_t)+8);
  /* realloc non-first (move) */
  ext = mp_realloc(first, 8, 25);
  assert(ext-second == sizeof(size_t)+8);
  assert(strcmp(ext, "hello") == 0);
  /* shrink last */
  char *ext2 = mp_realloc(ext, 25, 8);
  assert(ext == ext2);
  assert(mp_on_stack(&__PL_ar_ctx, ext2)[0] == ROUND_SIZE(8)+1);

  AR_END();

  return true;
}
#endif

#endif /*O_MY_GMP_ALLOC*/


#ifdef __WINDOWS__
#undef isascii
int
isascii(int c)				/* missing from gmp.lib */
{ return c >= 0 && c < 128;
}
#endif

		 /*******************************
		 *	STACK MANAGEMENT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An MPZ number on the stack is represented as:

  - Indirect Header
  - Size shifted left by 1, MP_RAT_MASK=0, negative size is nagative MPZ
if O_BF
  - exponent
endif
  - Limbs
  - Indirect Header

An MPQ number on the stack is represented as:
  - Indirect Header
  - Size numerator shifted left by 1, MP_RAT_MASK=0, negative size is nagative MPZ
  - Size denominator shifted left by 1, MP_RAT_MASK=0, negative size is nagative MPZ
if O_BF
  - exponent numerator
  - exponent denominator
endif
  - Limbs numerator
  - Limbs denominator
  - Indirect Header
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#if O_GMP

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mpz_wsize() returns the size needed to stores the limbs on the stack and
stores the not-rounded size over `s`.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
mpz_wsize(const mpz_t mpz, size_t *s)
{ DEBUG(0, assert(sizeof(mpz->_mp_size) == sizeof(int)));
  size_t size = sizeof(mp_limb_t)*abs(mpz->_mp_size);
  size_t wsz  = (size+sizeof(word)-1)/sizeof(word);

  if ( s )
    *s = size;

  return wsz;
}

#elif O_BF

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An MPZ when emulated using LibBF is represented as:

  - Indirect Header
  - Size shifted left by 1, MP_RAT_MASK=0, negative size is nagative MPZ
  - Exponent
  - Limbs
  - Indirect Header
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static size_t
mpz_wsize(const mpz_t mpz, size_t *s)
{ const bf_t *bf = mpz;
  size_t size = sizeof(*bf->tab) * bf->len;
  size_t wsz  = (size+sizeof(word)-1)/sizeof(word);

  if ( s )
    *s = size;

  return wsz;
}

#endif /*O_GMP||O_BF*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
globalMPZ() pushes an mpz type GMP  integer   onto  the local stack. The
saved version is the _mp_size field, followed by the limps.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define globalMPZ(at, mpz, flags) LDFUNC(globalMPZ, at, mpz, flags)
static int
globalMPZ(DECL_LD Word at, mpz_t mpz, int flags)
{ DEBUG(CHK_SECURE, assert(!onStackArea(global, at) && !onStackArea(local, at)));

  if ( !MPZ_ON_STACK(mpz) )
  { size_t size, wsz;
    Word p;
    word m;

  copy:
    wsz = mpz_wsize(mpz, &size);
    m   = mkIndHdr(wsz+MPZ_STACK_EXTRA, TAG_INTEGER);

    if ( wsizeofInd(m) != wsz+MPZ_STACK_EXTRA )
    { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_integer);
      return 0;
    }

    if ( !hasGlobalSpace(wsz+MPZ_STACK_EXTRA+2) )
    { int rc = ensureGlobalSpace(wsz+MPZ_STACK_EXTRA+2, flags);

      if ( rc != true )
	return rc;
    }
    p = gTop;
    gTop += wsz+MPZ_STACK_EXTRA+2;

    *at = consPtr(p, TAG_INTEGER|STG_GLOBAL);

    *p++     = m;
    p[wsz+MPZ_STACK_EXTRA-1] = 0L;	/* pad out */
    p[wsz+MPZ_STACK_EXTRA] = m;
    *p++     = mpz_size_stack(MPZ_LIMB_SIZE(mpz));
#if O_BF
    *p++     = mpz->expn;
#endif
    memcpy(p, MPZ_LIMBS(mpz), size);
  } else				/* already on the stack */
  { Word p = (Word)MPZ_LIMBS(mpz) - MPZ_STACK_EXTRA - 1;
    if ( !onStack(global, p) )
      goto copy;
#ifndef NDEBUG
    size_t size;
    size_t wsz = mpz_wsize(mpz, &size);
    assert(p[0] == mkIndHdr(wsz+MPZ_STACK_EXTRA, TAG_INTEGER));
#endif
    *at = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  }

  return true;
}


#define globalMPQ(at, mpq, flags) LDFUNC(globalMPQ, at, mpq, flags)
static int
globalMPQ(DECL_LD Word at, mpq_t mpq, int flags)
{ mpz_t num, den;			/* num/den */

  num[0] = *mpq_numref(mpq);
  den[0] = *mpq_denref(mpq);

  if ( !MPZ_ON_STACK(num) || !MPZ_ON_STACK(den) )
  { size_t num_size, den_size, num_wsz, den_wsz;
    Word p;
    word m;

  copy:
    num_wsz = mpz_wsize(num, &num_size);
    den_wsz = mpz_wsize(den, &den_size);
    m       = mkIndHdr(num_wsz+den_wsz+2*MPZ_STACK_EXTRA, TAG_INTEGER);

    if ( wsizeofInd(m) != num_wsz+den_wsz+2*MPZ_STACK_EXTRA )
    { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_rational);
      return 0;
    }

    if ( !hasGlobalSpace(num_wsz+den_wsz+2+2*MPZ_STACK_EXTRA) )
    { int rc = ensureGlobalSpace(num_wsz+den_wsz+2+2*MPZ_STACK_EXTRA, flags);

      if ( rc != true )
	return rc;
    }
    p = gTop;
    gTop += num_wsz+den_wsz+2+2*MPZ_STACK_EXTRA;

    *at = consPtr(p, TAG_INTEGER|STG_GLOBAL);
    *p++ = m;
    *p++ = mpq_size_stack(MPZ_LIMB_SIZE(num));
#if O_BF
    *p++ = num->expn;
#endif
    *p++ = mpq_size_stack(MPZ_LIMB_SIZE(den));
#if O_BF
    *p++ = den->expn;
#endif
    p[num_wsz-1] = 0L;				/* pad out */
    memcpy(p, MPZ_LIMBS(num), num_size);
    p += num_wsz;
    p[den_wsz-1] = 0L;				/* pad out */
    memcpy(p, MPZ_LIMBS(den), den_size);
    p += den_wsz;
    *p = m;
    assert(p==gTop-1);
  } else					/* already on the stack */
  { Word p = (Word)MPZ_LIMBS(num)-1-2*MPZ_STACK_EXTRA;
    if ( !onStack(global, p) )
      goto copy;
    DEBUG(CHK_SECURE,
	  { size_t num_size;
	    size_t den_size;
	    size_t num_wsz = mpz_wsize(num, &num_size);
	    size_t den_wsz = mpz_wsize(den, &den_size);
	    assert(p[0] == mkIndHdr(num_wsz+den_wsz+2+2*MPZ_STACK_EXTRA, TAG_INTEGER));
	    assert((Word)MPZ_LIMBS(den) == (Word)MPZ_LIMBS(num) + num_wsz);
	  });
    *at = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  }

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_bigint()  fetches  the value  of  a  Prolog  term  known to  be  a
non-inlined integer  into a number structure.   If the value is  a MPZ
number, it must be handled as read-only and it only be used as long as
no calls are made that may force a relocation or garbage collection on
the global stack.

Normally called through the inline get_integer() function.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
get_mpz_from_stack(Word p, mpz_t mpz)
{
#if O_GMP
  mpz->_mp_size  = mpz_stack_size(*p++);
  mpz->_mp_alloc = 0;
  mpz->_mp_d     = (mp_limb_t*) p;
#elif O_BF
  slimb_t len = mpz_stack_size(*p++);
  mpz->ctx	 = NULL;
  mpz->expn = (slimb_t)*p++;
  mpz->sign = len < 0;
  mpz->len  = abs(len);
  mpz->tab  = (limb_t*)p;
#endif
}

static void
get_mpq_from_stack(Word p, mpq_t mpq)
{ mpz_t num, den;
  size_t num_size;

#if O_GMP
  num->_mp_size  = mpz_stack_size(*p++);
  num->_mp_alloc = 0;
  num->_mp_d     = (mp_limb_t*) (p+1);
  num_size       = mpz_wsize(num, NULL);
  den->_mp_size  = mpz_stack_size(*p++);
  den->_mp_alloc = 0;
  den->_mp_d     = (mp_limb_t*) (p+num_size);
#elif O_BF
  slimb_t len = mpz_stack_size(*p++);
  num->ctx    = NULL;
  num->alloc  = 0;
  num->expn   = (slimb_t)*p++;
  num->sign   = len < 0;
  num->len    = abs(len);
  num->tab    = (limb_t*)(p+2);
  num_size    = mpz_wsize(num, NULL);
  den->ctx    = NULL;
  den->alloc  = 0;
  den->sign   = 0;			/* canonical MPQ */
  den->len    = mpz_stack_size(*p++);
  den->expn   = (slimb_t)*p++;
  den->tab    = (limb_t*) (p+num_size);
#endif
  *mpq_numref(mpq) = num[0];
  *mpq_denref(mpq) = den[0];
}

void
get_bigint(word w, Number n)
{ Word p = addressIndirect(w);

  DEBUG(0, assert(storage(w) != STG_INLINE));

  p++;				/* bits of indirect */
  n->type = V_MPZ;
  get_mpz_from_stack(p, n->value.mpz);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get a rational (int or non-int-rational) from a Prolog word, knowing the
word is not an inlined (tagged) integer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
get_rational_no_int(DECL_LD word w, Number n)
{ Word p = addressIndirect(w);

  p++;
  if ( (*p&MP_RAT_MASK) )
  { n->type = V_MPQ;
    get_mpq_from_stack(p, n->value.mpq);
  } else
  { n->type = V_MPZ;
    get_mpz_from_stack(p, n->value.mpz);
  }
}


Code
get_mpz_from_code(Code pc, mpz_t mpz)
{ word m;
  Word data;
  pc = code_get_indirect(pc, &m, &data);

  get_mpz_from_stack(data, mpz);

  return pc;
}


Code
get_mpq_from_code(Code pc, mpq_t mpq)
{ word m;
  Word data;
  pc = code_get_indirect(pc, &m, &data);

  get_mpq_from_stack(data, mpq);

  return pc;
}


bool
get_int64(DECL_LD word w, int64_t *ip)
{ if ( tagex(w) == (TAG_INTEGER|STG_INLINE) )
  { *ip = valInt(w);
    return true;
  } else if ( tagex(w) == (TAG_INTEGER|STG_GLOBAL) )
  { number n;

    get_bigint(w, &n);
    return mpz_to_int64(n.value.mpz, ip);
  } else
    return false;
}



		 /*******************************
		 *	  IMPORT/EXPORT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addMPZToBuffer(Buffer b, mpz_t mpz)
	Add mpz in a machine independent representation to the given buffer.
	The data is stored in limps of 1 byte, preceded by the byte-count
	as 4-byte big-endian number;

addMPQToBuffer(Buffer b, mpq_t mpq)
	Similar to mpz, but first writes the two headers and then the
	two bit patterns.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
add_mpz_size_buffer(Buffer b, mpz_t mpz, size_t size)
{ ssize_t hdrsize;

  if ( mpz_sgn(mpz) < 0 )
    hdrsize = -(ssize_t)size;
  else
    hdrsize = (ssize_t)size;

  *b->top++ = (char)((hdrsize>>24)&0xff);
  *b->top++ = (char)((hdrsize>>16)&0xff);
  *b->top++ = (char)((hdrsize>> 8)&0xff);
  *b->top++ = (char)((hdrsize    )&0xff);
}

static void
add_mpz_bits_buffer(Buffer b, mpz_t mpz, size_t size)
{ size_t count;

  mpz_export(b->top, &count, 1, 1, 1, 0, mpz);
  assert(count == size);
  b->top = b->top + size;
}

void
addMPZToBuffer(Buffer b, mpz_t mpz)
{ size_t size = (mpz_sizeinbase(mpz, 2)+7)/8;

  if ( !growBuffer(b, size+4) )
    outOfCore();
  add_mpz_size_buffer(b, mpz, size);
  add_mpz_bits_buffer(b, mpz, size);
}

void
addMPQToBuffer(Buffer b, mpq_t mpq)
{ mpz_t num, den;			/* num/den */
  size_t num_size, den_size;

  num[0] = *mpq_numref(mpq);
  den[0] = *mpq_denref(mpq);
  num_size = (mpz_sizeinbase(num, 2)+7)/8;
  den_size = (mpz_sizeinbase(den, 2)+7)/8;

  if ( !growBuffer(b, num_size+den_size+8) )
    outOfCore();

  add_mpz_size_buffer(b, num, num_size);
  add_mpz_size_buffer(b, den, den_size);
  add_mpz_bits_buffer(b, num, num_size);
  add_mpz_bits_buffer(b, den, den_size);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
loadMPZFromCharp() loads an MPZ number directly back to the global stack
from a char *  as  filled   by  addMPZToBuffer().  Memory  allocation is
avoided by creating a dummy mpz that looks big enough to mpz_import().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SHIFTSIGN32 ((sizeof(int)-4)*8)

static char *
load_mpz_size(const char *data, int *szp)
{ const unsigned char *udata = (const unsigned char*)data;
  uint32_t usize = 0;
  int32_t size;

  usize |= ((uint32_t)udata[0])<<24;
  usize |= ((uint32_t)udata[1])<<16;
  usize |= ((uint32_t)udata[2])<<8;
  usize |= ((uint32_t)udata[3]);
  size = (int32_t)usize;

  *szp = size;
  data += 4;

  return (char *)data;
}

static char *
load_abs_mpz_size(const char *data, int *szp, int *neg)
{ int size;

  data = load_mpz_size(data, &size);
  if ( neg )
  { if ( size < 0 )
    { size = -size;
      *neg = true;
    } else
    { *neg = false;
    }
  } else if ( size < 0 )
    size = -size;
  *szp = size;

  return (char *)data;
}

static char *
load_mpz_bits(const char *data, size_t size, size_t limpsize, int neg, Word p)
{ mpz_t mpz;

#if O_GMP
  mpz->_mp_size  = limpsize;
  mpz->_mp_alloc = limpsize;
  mpz->_mp_d     = (mp_limb_t*)p;

  mpz_import(mpz, size, 1, 1, 1, 0, data);
  assert((Word)mpz->_mp_d == p);	/* check no (re-)allocation is done */
#elif O_BF
  mpz->len = limpsize;
  mpz->tab = (mp_limb_t*)p;
  mpz_import(mpz, size, 1, 1, 1, 0, data);
#endif

  return (char*)data+size;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load a bit integer from data as stored   in  records. A bigint is stored
using 4 bytes in big endian notation   do represent the length, followed
by N bytes in big endian notation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
loadMPZFromCharp(const char *data, Word r, Word *store)
{ int size = 0;
  size_t limbsize;
  size_t wsize;
  int neg;
  Word p;
  word m;

  data = load_abs_mpz_size(data, &size, &neg);
#if O_BF
  bf_t bf;
  bf_import_dimension(&bf, (const unsigned char*)data, size);
  limbsize = bf.len;
#else
  limbsize = (size+sizeof(mp_limb_t)-1)/sizeof(mp_limb_t);
#endif

  wsize = (limbsize*sizeof(mp_limb_t)+sizeof(word)-1)/sizeof(word);
  p = *store;
  *store += (wsize+2+MPZ_STACK_EXTRA);
  *r = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  m = mkIndHdr(wsize+MPZ_STACK_EXTRA, TAG_INTEGER);
  *p++ = m;
  p[wsize+MPZ_STACK_EXTRA-1] = 0L;	/* pad out */
  p[wsize+MPZ_STACK_EXTRA] = m;
  *p++ = mpz_size_stack(neg ? -limbsize : limbsize);
#if O_BF
  *p++ = bf.expn;
#endif

  return load_mpz_bits(data, size, limbsize, neg, p);
}

char *
loadMPQFromCharp(const char *data, Word r, Word *store)
{ int num_size;
  int den_size;
  size_t num_limbsize, num_wsize;
  size_t den_limbsize, den_wsize;
  int num_neg, den_neg;
  size_t wsize;
  Word p;
  word m;

  data = load_abs_mpz_size(data, &num_size, &num_neg);
  data = load_abs_mpz_size(data, &den_size, &den_neg);

#if O_BF
  bf_t num_bf, den_bf;
  bf_import_dimension(&num_bf, (const unsigned char*)data, num_size);
  num_limbsize = num_bf.len;
  bf_import_dimension(&den_bf, (const unsigned char*)data+num_size, den_size);
  den_limbsize = den_bf.len;
#else
  num_limbsize = (num_size+sizeof(mp_limb_t)-1)/sizeof(mp_limb_t);
  den_limbsize = (den_size+sizeof(mp_limb_t)-1)/sizeof(mp_limb_t);
#endif

  num_wsize = (num_limbsize*sizeof(mp_limb_t)+sizeof(word)-1)/sizeof(word);
  den_wsize = (den_limbsize*sizeof(mp_limb_t)+sizeof(word)-1)/sizeof(word);
  wsize = num_wsize+den_wsize;

  p = *store;
  *store += wsize+2+2*MPZ_STACK_EXTRA;
  *r = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  m = mkIndHdr(wsize+2*MPZ_STACK_EXTRA, TAG_INTEGER);
  *p++ = m;
  *p++ = mpq_size_stack(num_neg ? -num_limbsize : num_limbsize);
#if O_BF
  *p++ = num_bf.expn;
#endif
  *p++ = mpq_size_stack(den_neg ? -den_limbsize : den_limbsize);
#if O_BF
  *p++ = den_bf.expn;
#endif
  p[num_wsize-1] = 0;
  data = load_mpz_bits(data, num_size, num_limbsize, num_neg, p);
  p += num_wsize;
  p[den_wsize-1] = 0;
  data = load_mpz_bits(data, den_size, den_limbsize, den_neg, p);
  p += den_wsize;
  *p++ = m;
  assert(p == *store);

  return (char *)data;
}

char *
skipMPZOnCharp(const char *data)
{ int size;

  data = load_abs_mpz_size(data, &size, NULL);

  return (char *)data + size;
}

char *
skipMPQOnCharp(const char *data)
{ int num_size;
  int den_size;

  data = load_abs_mpz_size(data, &num_size, NULL);
  data = load_abs_mpz_size(data, &den_size, NULL);
  data += num_size;
  data += den_size;

  return (char *)data;
}

#undef SHIFTSIGN32


		 /*******************************
		 *	     CONVERSION		*
		 *******************************/

#ifdef WORDS_BIGENDIAN
#define ORDER 1
#else
#define ORDER -1
#endif

#if O_GMP
void
mpz_init_set_si64(mpz_t mpz, int64_t i)
{
#if SIZEOF_LONG == 8
  mpz_init_set_si(mpz, (long)i);
#else
  DEBUG(2, Sdprintf("Converting %" PRId64 " to MPZ\n", i));

  if ( i >= LONG_MIN && i <= LONG_MAX )
  { mpz_init_set_si(mpz, (long)i);
  } else
  { mpz_init(mpz);
    if ( i >= 0 )
    { mpz_import(mpz, sizeof(i), ORDER, 1, 0, 0, &i);
    } else
    { i = -i;
      mpz_import(mpz, sizeof(i), ORDER, 1, 0, 0, &i);
      mpz_neg(mpz, mpz);
    }
  }
  DEBUG(2, gmp_printf("\t--> %Zd\n", mpz));
#endif
}

#endif /*O_GMP*/

static void
mpz_init_set_uint64(mpz_t mpz, uint64_t i)
{
#if O_GMP
#if SIZEOF_LONG == 8
  mpz_init_set_ui(mpz, (unsigned long)i);
#else
  mpz_init(mpz);
  mpz_import(mpz, sizeof(i), ORDER, 1, 0, 0, &i);
#endif
#elif O_BF
  mpz_init(mpz);
  bf_set_ui(mpz, i);
#else
#error "No implementation for mpz_init_set_uint64()"
#endif
}

static void
mpz_init_max_uint(mpz_t mpz, int bits)
{ mpz_init_set_si(mpz, 1);
  mpz_mul_2exp(mpz, mpz, bits);
  mpz_sub_ui(mpz, mpz, 1);
}


int
promoteToMPZNumber(number *n)
{ switch(n->type)
  { case V_INTEGER:
      mpz_init_set_si64(n->value.mpz, n->value.i);
      n->type = V_MPZ;
      break;
    case V_MPZ:
      break;
    case V_MPQ:
    { mpz_t mpz;

      mpz_init(mpz);
      mpz_tdiv_q(mpz,
		 mpq_numref(n->value.mpq),
		 mpq_denref(n->value.mpq));
      clearNumber(n);
      n->type = V_MPZ;
      n->value.mpz[0] = mpz[0];
      break;
    }
    case V_FLOAT:
      mpz_init_set_d(n->value.mpz, n->value.f);
      n->type = V_MPZ;
      break;
  }

  return true;
}


int
promoteToMPQNumber(number *n)
{ switch(n->type)
  { case V_INTEGER:
      promoteToMPZNumber(n);
      /*FALLTHOURGH*/
    case V_MPZ:
    { *mpq_numref(n->value.mpq) = n->value.mpz[0];
      mpz_init_set_ui(mpq_denref(n->value.mpq), 1L);
      n->type = V_MPQ;
      break;
    }
    case V_MPQ:
      break;
    case V_FLOAT:
    { double v = n->value.f;

      switch(fpclassify(v))
      { case FP_NAN:
	  return PL_error(NULL, 0, NULL, ERR_AR_UNDEF);
	case FP_INFINITE:
	  return PL_error(NULL, 0, NULL, ERR_AR_RAT_OVERFLOW);
      }

      n->type = V_MPQ;
      mpq_init(n->value.mpq);
      mpq_set_d(n->value.mpq, v);
      break;
    }
  }

  return true;
}


		 /*******************************
		 *		RW		*
		 *******************************/

void
ensureWritableNumber(Number n)
{ switch(n->type)
  { case V_MPZ:
      if ( MPZ_ON_STACK(n->value.mpz) )
      { mpz_t tmp;

	tmp[0] = n->value.mpz[0];
	mpz_init_set(n->value.mpz, tmp);
	break;
      }
    case V_MPQ:
    { if ( MPZ_ON_STACK(mpq_numref(n->value.mpq)) )
      { mpz_t tmp;

	tmp[0] = mpq_numref(n->value.mpq)[0];
	mpz_init_set(mpq_numref(n->value.mpq), tmp);
      }
	if ( MPZ_ON_STACK(mpq_denref(n->value.mpq)) )
      { mpz_t tmp;

	tmp[0] = mpq_denref(n->value.mpq)[0];
	mpz_init_set(mpq_denref(n->value.mpq), tmp);
      }
      break;
    }
    default:
      break;
  }
}



		 /*******************************
		 *	       CLEAR		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Numbers may contain two type of MPZ   numbers.  Ones that are created by
the GMP library and must be cleared,   and  ones that have their `limbs'
stored somewhere in the Prolog memory. These  may only be used read-only
and their _mp_alloc field  is  set   to  0.  clearNumber()  discards MPZ
numbers that are created by GMP only.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
clearGMPNumber(Number n)
{ switch(n->type)
  { case V_MPZ:
      if ( !MPZ_ON_STACK(n->value.mpz) )
	mpz_clear(n->value.mpz);
      break;
    case V_MPQ:
      if ( !MPZ_ON_STACK(mpq_numref(n->value.mpq)) )
	mpz_clear(mpq_numref(n->value.mpq));
      if ( !MPZ_ON_STACK(mpq_denref(n->value.mpq)) )
	mpz_clear(mpq_denref(n->value.mpq));
      break;
    default:
      break;
  }
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

void
initGMP(void)
{ if ( !GD->gmp.initialised )
  { GD->gmp.initialised = true;

#if O_BF
    initBF();
#endif

    mpz_init_set_si64(MPZ_MIN_TAGGED, PLMINTAGGEDINT);
    mpz_init_set_si64(MPZ_MAX_TAGGED, PLMAXTAGGEDINT);
#ifndef O_BF
    mpz_init_set_si64(MPZ_MIN_INT64,  INT64_MIN);
    mpz_init_set_si64(MPZ_MAX_INT64,  INT64_MAX);
#endif
    mpz_init_max_uint(MPZ_MAX_UINT64, 64);
#if SIZEOF_LONG < SIZEOF_WORD
    mpz_init_set_si64(MPZ_MIN_LONG, LONG_MIN);
    mpz_init_set_si64(MPZ_MAX_LONG, LONG_MAX);
#endif
#ifdef O_MY_GMP_ALLOC
    if ( !GD->gmp.keep_alloc_functions )
    { mp_get_memory_functions(&smp_alloc, &smp_realloc, &smp_free);
      mp_set_memory_functions(mp_alloc, mp_realloc, mp_free);
    }
    DEBUG(0, mp_test_alloc());
#endif

#if O_GMP
#if __GNU_MP__ > 3 && __GNU_MP__ < 6
    PL_license("lgplv3", "libgmp");
#else
    PL_license("lgplv2+", "libgmp");
#endif
#endif
  }
}


void
cleanupGMP(void)
{ if ( GD->gmp.initialised )
  { GD->gmp.initialised = false;

#ifdef O_MY_GMP_ALLOC
    if ( !GD->gmp.keep_alloc_functions )
      mp_set_memory_functions(smp_alloc, smp_realloc, smp_free);
#endif
    mpz_clear(MPZ_MIN_TAGGED);
    mpz_clear(MPZ_MAX_TAGGED);
#ifndef O_BF
    mpz_clear(MPZ_MIN_INT64);
    mpz_clear(MPZ_MAX_INT64);
#endif
    mpz_clear(MPZ_MAX_UINT64);
#if SIZEOF_LONG < SIZEOF_VOIDP
    mpz_clear(MPZ_MIN_LONG);
    mpz_clear(MPZ_MAX_LONG);
#endif
  }
}

		 /*******************************
		 *	     INDEXING		*
		 *******************************/

/* given a pointer to the indirect header of an integer, return a hash
   as used  for clause indexing.   If the integer  is huge, we  do not
   want to use the whole thing.   Instead, we pick the dimensions, the
   first two and last limb of the content.

   Note: p might be aligned at Code rather than Word.
 */

word
bignum_index(const word *p)
{ word m = *p++;
  size_t n = wsizeofInd(m);

  if ( n <= KEY_INDEX_MAX )
  { return murmur_key(p, n*sizeof(*p));
  } else if ( (p[0]&MP_RAT_MASK) )
  { word data[4];
#if O_GMP
    data[0] = p[0]^p[1]<<(sizeof(p[0])*4);
    data[1] = p[2];
    data[2] = p[3];
#elif O_BF
    data[0] = p[0]^p[1]<<(sizeof(p[0])*4)^p[2]^p[3]<<(sizeof(p[0])*4);
    data[1] = p[4];
    data[2] = p[5];
#endif
    data[3] = p[n-1];
    return murmur_key(data, sizeof(data));
  } else
  { word data[4];
#if O_GMP
    data[0] = p[0];
    data[1] = p[1];
    data[2] = p[2];
#elif O_BF
    data[0] = p[0]^p[1]<<(sizeof(p[0])*4);
    data[1] = p[2];
    data[2] = p[3];
#else
#error("No GMP or BF")
#endif
    data[3] = p[n-1];
    return murmur_key(data, sizeof(data));
  }
}


		 /*******************************
		 *	   NUMBER HANDLING      *
		 *******************************/

#if O_GMP

int
mpz_to_int64(mpz_t mpz, int64_t *i)
{ if ( mpz_cmp(mpz, MPZ_MIN_INT64) >= 0 &&
       mpz_cmp(mpz, MPZ_MAX_INT64) <= 0 )
  { uint64_t v;

    mpz_export(&v, NULL, ORDER, sizeof(v), 0, 0, mpz);
    DEBUG(2,
	  { char buf[256];
	    Sdprintf("Convert %s --> %I64d\n",
		     mpz_get_str(buf, 10, mpz), v);
	  });

    if ( mpz_sgn(mpz) < 0 )
      *i = -(int64_t)(v - 1) - 1;
    else
      *i = (int64_t)v;

    return true;
  }

  return false;
}

/* return: <0:              -1
	   >MPZ_UINT64_MAX:  1
	   (ok)		     0
*/

int
mpz_to_uint64(mpz_t mpz, uint64_t *i)
{ if ( mpz_sgn(mpz) < 0 )
    return -1;

  if ( mpz_cmp(mpz, MPZ_MAX_UINT64) <= 0 )
  { uint64_t v;

    mpz_export(&v, NULL, ORDER, sizeof(v), 0, 0, mpz);
    *i = v;

    return 0;
  }

  return 1;
}


#elif O_BF

int
mpz_to_int64(mpz_t mpz, int64_t *i)
{ return bf_get_int64(i, mpz, 0) == 0;
}

int
mpz_to_uint64(mpz_t mpz, uint64_t *i)
{ if ( mpz_sgn(mpz) < 0 )
    return -1;

  if ( mpz_cmp(mpz, MPZ_MAX_UINT64) <= 0 )
  { int64_t v;

    if ( bf_get_int64(&v, mpz, BF_RNDZ|BF_GET_INT_MOD) == 0 )
    { *i = (uint64_t)v;
      return 0;
    }
  }

  return 1;
}

#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_number() transforms a number into a Prolog  term. Note that this may
allocate on the global stack. Please note   that  this function uses the
most compact representation, which is  essential   to  make unify() work
without any knowledge of the represented data.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define put_mpz(at, mpz, flags) LDFUNC(put_mpz, at, mpz, flags)
static int
put_mpz(DECL_LD Word at, mpz_t mpz, int flags)
{ DEBUG(2,
	{ char buf[256];
	  Sdprintf("put_mpz(%s)\n",
		   mpz_get_str(buf, 10, mpz));
	});

#if SIZEOF_LONG < SIZEOF_WORD
  if ( mpz_cmp(mpz, MPZ_MIN_LONG) >= 0 &&
       mpz_cmp(mpz, MPZ_MAX_LONG) <= 0 )
#else
  if ( mpz_cmp(mpz, MPZ_MIN_TAGGED) >= 0 &&
       mpz_cmp(mpz, MPZ_MAX_TAGGED) <= 0 )
#endif
  { long v = mpz_get_si(mpz);

    if ( !hasGlobalSpace(0) )		/* ensure we have room for bindConst */
    { int rc = ensureGlobalSpace(0, flags);

      if ( rc != true )
	return rc;
    }

    *at = consInt(v);
    assert(valInt(*at) == v);
    return true;
  } else
  { return globalMPZ(at, mpz, flags);
  }
}

#endif /*O_GMP*/

/* returns one of

  true: ok
  false: some error
  GLOBAL_OVERFLOW: no space
  LOCAL_OVERFLOW: cannot represent (no GMP)
*/

int
put_int64(DECL_LD Word at, int64_t l, int flags)
{ word r;

  r = consInt(l);
  if ( valInt(r) == l )
  { *at = r;
    return true;
  } else
  {
#ifdef O_BIGNUM
    mpz_t mpz;

    mpz_init_set_si64(mpz, l);
    return globalMPZ(at, mpz, flags);
#else
    return LOCAL_OVERFLOW;
#endif
  }
}


int
put_uint64(DECL_LD Word at, uint64_t l, int flags)
{ if ( (int64_t)l >= 0 )
  { return put_int64(at, l, flags);
  } else
  {
#ifdef O_BIGNUM
    mpz_t mpz;

    mpz_init_set_uint64(mpz, l);
    return globalMPZ(at, mpz, flags);
#else
    return LOCAL_OVERFLOW;
#endif
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_number()   translates   a   number   structure   into   its   Prolog
representation and ensures there  is  enough   space  for  a  subsequent
bindConst() call. Note that `at' must point   to  an address that is not
affected by GC/shift.  The intented scenario is:

  { word c;

    if ( (rc=put_number(&c, n, ALLOW_GC)) == true )
      bindConst(<somewhere>, c);
    ...
  }
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int				/* true, false, _*OVERFLOW */
put_number(DECL_LD Word at, Number n, int flags)
{ switch(n->type)
  { case V_INTEGER:
    { word w = consInt(n->value.i);

      if ( valInt(w) == n->value.i )
      { if ( !hasGlobalSpace(0) )
	{ int rc = ensureGlobalSpace(0, flags);

	  if ( rc != true )
	    return rc;
	}

	*at = w;
	return true;
      }

      return put_int64(at, n->value.i, flags);
    }
#ifdef O_BIGNUM
    case V_MPZ:
      return put_mpz(at, n->value.mpz, flags);
    case V_MPQ:
    { if ( mpz_cmp_ui(mpq_denref(n->value.mpq), 1L) == 0 )
	return put_mpz(at, mpq_numref(n->value.mpq), flags);
      else
	return globalMPQ(at, n->value.mpq, flags);
    }
#endif
    case V_FLOAT:
      return put_double(at, n->value.f, flags);
  }

  assert(0);
  return false;
}


int
PL_unify_number(DECL_LD term_t t, Number n)
{ Word p = valTermRef(t);
  word w;

  deRef(p);

  if ( isVar(*p) &&
       n->type == V_INTEGER &&
       valInt(w=consInt(n->value.i)) == n->value.i )
    return varBindConst(p, w);

  if ( canBind(*p) )
  { int rc;

    if ( (rc=put_number(&w, n, ALLOW_GC)) != true )
      return raiseStackOverflow(rc);

    p = valTermRef(t);			/* put_number can shift the stacks */
    deRef(p);

    bindConst(p, w);
    return true;
  }

  switch(n->type)
  { case V_INTEGER:
      if ( isTaggedInt(*p) )
	return valInt(*p) == n->value.i;
      /*FALLTHOURGH*/
#ifdef O_BIGNUM
    case V_MPZ:
#endif
      if ( isInteger(*p) )
      { number n2;
	int rc;

	get_integer(*p, &n2);
	rc = (cmpNumbers(n, &n2) == CMP_EQUAL);
	clearNumber(&n2);

	return rc;
      }
      break;
#ifdef O_BIGNUM
    case V_MPQ:
    { if ( isRational(*p) )
      { number n2;
	int rc;

	get_rational(*p, &n2);
	rc = (cmpNumbers(n, &n2) == CMP_EQUAL);
	clearNumber(&n2);

	return rc;
      }
      break;
    }
#endif
    case V_FLOAT:
      if ( isFloat(*p) )
      {	Word ap = valIndirectP(*p);

	return memcmp((char*)&n->value.f, ap, sizeof(n->value.f)) == 0;
      }
      break;
  }

  fail;
}


int
PL_put_number(DECL_LD term_t t, Number n)
{ word w;
  int rc;

  if ( (rc=put_number(&w, n, ALLOW_GC)) != true )
    return raiseStackOverflow(rc);

  *valTermRef(t) = w;

  return true;
}


void
get_number(DECL_LD word w, Number n)
{ if ( isRational(w) )
  { get_rational(w, n);
  } else
  { n->type = V_FLOAT;
    n->value.f = valFloat(w);
  }
}


int
PL_get_number(DECL_LD term_t t, Number n)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isRational(*p) )
  { get_rational(*p, n);
    succeed;
  }
  if ( isFloat(*p) )
  { n->value.f = valFloat(*p);
    n->type = V_FLOAT;
    succeed;
  }

  fail;
}

API_STUB(int)
(PL_get_number)(term_t t, Number n)
( return PL_get_number(t, n); )


		 /*******************************
		 *	     PROMOTION		*
		 *******************************/

bool
promoteToFloatNumber(Number n)
{ switch(n->type)
  { case V_INTEGER:
      n->value.f = (double)n->value.i;
      n->type = V_FLOAT;
      break;
#ifdef O_BIGNUM
    case V_MPZ:
    { double val = mpz_to_double(n->value.mpz);

      clearNumber(n);
      n->value.f = val;
      n->type = V_FLOAT;
      break;
    }
    case V_MPQ:
    { double val = mpq_to_double(n->value.mpq);

      clearNumber(n);
      n->value.f = val;
      n->type = V_FLOAT;
      break;
    }
#endif
    case V_FLOAT:
      return true;
  }

  return check_float(n);
}


bool
promoteNumber(Number n, numtype t)
{ switch(t)
  { case V_INTEGER:
      return true;
#ifdef O_BIGNUM
    case V_MPZ:
      return promoteToMPZNumber(n);
    case V_MPQ:
      return promoteToMPQNumber(n);
#endif
    case V_FLOAT:
      return promoteToFloatNumber(n);
    default:
      assert(0);
      return false;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
make_same_type_numbers(n1, n2)
    Upgrade both numbers to the `highest' type of both. Number types are
    defined in the enum-type numtype, which is supposed to define a
    total ordering between the number types.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
make_same_type_numbers(Number n1, Number n2)
{ if ( (int)n1->type > (int)n2->type )
    return promoteNumber(n2, n1->type);
  else
    return promoteNumber(n1, n2->type);
}


		 /*******************************
		 *	    OPERATIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cmpNumbers() compares two numbers. First, both   numbers are promoted to
the lowest (in V_* ordering) common type, after which they are compared.
Note that if the common type is V_FLOAT, but not both are V_FLOAT we can
run into troubles because big  integers  may   be  out  of range for the
double representation. We trust mpz_get_d()   and mpq_to_double() return
+/- float infinity and this compares  correctly with the other argument,
which is guaranteed to be a valid float.

(*) Similar  to mpq_to_double(), we must  compare at 64 bit  level and
not the extended level.  First we compare the bits.  If this is equal,
we are done.   Otherwise we use the usual float  comparison, but as we
have accessed the bits, the compiler must  have put the double in a 64
bit register.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
cmpFloatNumbers(Number n1, Number n2)
{ if ( n1->type == V_FLOAT )
  { fpattern d2;

    if ( isnan(n1->value.f) )
      return CMP_NOTEQ;

    switch(n2->type)
    { case V_INTEGER:
	d2.d = (double)n2->value.i;
	break;
#ifdef O_BIGNUM
      case V_MPZ:
	d2.d = mpz_to_double(n2->value.mpz);
	break;
      case V_MPQ:
	d2.d = mpq_to_double(n2->value.mpq);
	break;
#endif
      default:
	assert(0);
	d2.d = 0.0;
    }

    fpattern f1 = { .d = n1->value.f };
    return f1.i == d2.i ? CMP_EQUAL : /* see (*) */
           f1.d == d2.d ? CMP_EQUAL :
           f1.d  < d2.d ? CMP_LESS :
                          CMP_GREATER;
  } else
  { assert(n2->type == V_FLOAT);
    if ( isnan(n2->value.f) )	/* CMP_NOTEQ != -CMP_NOTEQ :( */
      return CMP_NOTEQ;
    return -cmpFloatNumbers(n2, n1);
  }
}


int
cmpNumbers(Number n1, Number n2)
{ if ( unlikely(n1->type != n2->type) )
  { int rc;

    if ( n1->type == V_FLOAT || n2->type == V_FLOAT )
      return cmpFloatNumbers(n1, n2);
    rc = make_same_type_numbers(n1, n2);
    assert(rc != CMP_ERROR);
    (void)rc;
  }

  switch(n1->type)
  { case V_INTEGER:
      return SCALAR_TO_CMP(n1->value.i, n2->value.i);
#ifdef O_BIGNUM
    case V_MPZ:
    { int rc = mpz_cmp(n1->value.mpz, n2->value.mpz);

      return SCALAR_TO_CMP(rc, 0);
    }
    case V_MPQ:
    { int rc = mpq_cmp(n1->value.mpq, n2->value.mpq);

      return SCALAR_TO_CMP(rc, 0);
    }
#endif
    case V_FLOAT:
    { if ( n1->value.f == n2->value.f )
	return CMP_EQUAL;

      int lt = n1->value.f  < n2->value.f;
      int gt = n1->value.f  > n2->value.f;

      if ( !lt && !gt )		/* either is NaN */
	return CMP_NOTEQ;	/* as SCALAR_TO_CMP() */
      return gt-lt;
    }
    default:
      assert(0);
      return CMP_EQUAL;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cmpReals()   compares  two   real  numbers   (any  sub-type)   and  is
mathematically correct.   Floats and 64  bit integers can  be compared
without  using  extended  arithmetic.    All  other  comparisons  with
integers  or rationals  are  done using  the  relevant BIGNUM  library
compare  functions.  cmpReals() itself  is  just  a large  4x4  switch
statement  which  uses  the  targeted auxiliary  compare  function  to
compute the return value.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
cmp_i_i(int64_t i1, int64_t i2)
{ return (i1 == i2) ? CMP_EQUAL : ((i1 < i2) ? CMP_LESS : CMP_GREATER);
}

static int
cmp_f_f(double d1, double d2)
{ if ( isnan(d1) || isnan(d2) )
    return CMP_NOTEQ;
  else
    return (d1 == d2) ? CMP_EQUAL : ((d1 < d2) ? CMP_LESS : CMP_GREATER);
}

/* See https://stackoverflow.com/questions/58734034/ */
static int
cmp_i_f(int64_t i1, double d2)
{ if ( isnan(d2) )
  { return CMP_NOTEQ;
  } else
  { double d1_lo, d1_hi;
#define TOD(i) ((double)((int64_t)(i)))
    if ( i1 >= 0 )
    { d1_lo = TOD(i1 & 0x00000000FFFFFFFF);
      d1_hi = TOD(i1 & 0xFFFFFFFF00000000);
    } else
    { d1_lo = TOD(i1 | 0xFFFFFFFF00000000);
      d1_hi = TOD(i1 | 0x00000000FFFFFFFF)+1.0;
    }
#undef TOD
    return SCALAR_TO_CMP(d1_lo, d2-d1_hi);
  }
}

#ifdef O_BIGNUM

static int
cmp_z_z(mpz_t z1, mpz_t z2)
{ int t = mpz_cmp(z1,z2);
  return (t < 0) ? CMP_LESS : (t > 0);
}

static int
cmp_q_q(mpq_t q1, mpq_t q2)
{ int t = mpq_cmp(q1,q2);
  return (t < 0) ? CMP_LESS : (t > 0);
}

static int
cmp_z_i(mpz_t z1, int64_t i2)
{
#if SIZEOF_LONG == 8
  int t = mpz_cmp_si(z1,i2);
  return (t < 0) ? CMP_LESS : (t > 0);
#else
  if ( i2 >= LONG_MIN && i2 <= LONG_MAX )
  { int t = mpz_cmp_si(z1,(long)i2);
    return (t < 0) ? CMP_LESS : (t > 0);
  } else
  { mpz_t a;
    mpz_init_set_si64(a, i2);
    int t = cmp_z_z(z1, a);
    mpz_clear(a);
    return t;
  }
#endif
}

static int
cmp_q_i(mpq_t q1, int64_t i2)
{
#if SIZEOF_LONG == 8
  int t = mpq_cmp_si(q1,i2,1);
  return (t < 0) ? CMP_LESS : (t > 0);
#else
  if ( i2 >= LONG_MIN && i2 <= LONG_MAX )
  { int t = mpq_cmp_si(q1,(long)i2,1);
    return (t < 0) ? CMP_LESS : (t > 0);
  } else
  { mpq_t qa;
    mpz_t za;
    mpq_init(qa);
    mpz_init_set_si64(za, i2);
    mpq_set_z(qa, za);
    int t = cmp_q_q(q1, qa);
    mpz_clear(za);
    mpq_clear(qa);
    return t;
  }
#endif
}

static int
cmp_z_f(mpz_t z1, double d2)
{ if (isnan(d2)) return CMP_NOTEQ;
  else {
    int t = mpz_cmp_d(z1,d2);        // mpz_cmp_d handles infinities
    return (t < 0) ? CMP_LESS : (t > 0);
  }
}

static int
cmp_f_q(double d1, mpq_t q2)
{ if      (isnan(d1))       return CMP_NOTEQ;
  else if (d1 ==  INFINITY) return CMP_GREATER;
  else if (d1 == -INFINITY) return CMP_LESS;
  else
  { mpq_t q1;
    mpq_init(q1);
    mpq_set_d(q1,d1);
    int t = cmp_q_q(q1,q2);
    mpq_clear(q1);
    return t;
  }
}

static int
cmp_q_z(mpq_t q1, mpz_t z2)
{ int t = mpq_cmp_z(q1,z2);
  return (t < 0) ? CMP_LESS : (t > 0);
}

#endif // O_BIGNUM

/* Note that we can use reversed arguments and negation for all functions
   except those involving floats because -CMP_NOTEQ is wrong
*/

int
cmpReals(Number n1, Number n2)
{ int rc;

  switch(n1->type)
  { case V_INTEGER:
      switch(n2->type)
      { case V_INTEGER: return  cmp_i_i(n1->value.i,n2->value.i);
        case V_FLOAT:   return  cmp_i_f(n1->value.i,n2->value.f);
#ifdef O_BIGNUM
        case V_MPZ:     return -cmp_z_i(n2->value.mpz,n1->value.i);
        case V_MPQ:     return -cmp_q_i(n2->value.mpq,n1->value.i);
#endif
      }
    case V_FLOAT:
      switch(n2->type)
      { case V_INTEGER: rc =    cmp_i_f(n2->value.i,n1->value.f);
                        return  (rc == CMP_NOTEQ) ? rc : -rc;
        case V_FLOAT:   return  cmp_f_f(n1->value.f,n2->value.f);
#ifdef O_BIGNUM
        case V_MPZ:     rc =    cmp_z_f(n2->value.mpz,n1->value.f);
                        return  (rc == CMP_NOTEQ) ? rc : -rc;
        case V_MPQ:     return  cmp_f_q(n1->value.f,n2->value.mpq);
#endif
      }
#ifdef O_BIGNUM
    case V_MPZ:
      switch(n2->type)
      { case V_INTEGER: return  cmp_z_i(n1->value.mpz,n2->value.i);
        case V_FLOAT:   return  cmp_z_f(n1->value.mpz,n2->value.f);
        case V_MPZ:     return  cmp_z_z(n1->value.mpz,n2->value.mpz);
        case V_MPQ:     return -cmp_q_z(n2->value.mpq,n1->value.mpz);
      }
    case V_MPQ:
      switch(n2->type)
      { case V_INTEGER: return  cmp_q_i(n1->value.mpq,n2->value.i);
        case V_FLOAT:   rc =    cmp_f_q(n2->value.f,n1->value.mpq);
                        return  (rc == CMP_NOTEQ) ? rc : -rc;
        case V_MPZ:     return  cmp_q_z(n1->value.mpq,n2->value.mpz);
        case V_MPQ:     return  cmp_q_q(n1->value.mpq,n2->value.mpq);
      }
#endif // O_BIGNUM
  }
  return CMP_NOTEQ;  // unrecognized type?, treat as nan
}

void
cpNumber(Number to, Number from)
{ to->type = from->type;

  switch(from->type)
  { case V_INTEGER:
      to->value.i = from->value.i;
      break;
#ifdef O_BIGNUM
    case V_MPZ:
      mpz_init(to->value.mpz);
      mpz_set(to->value.mpz, from->value.mpz);
      break;
    case V_MPQ:
      mpq_init(to->value.mpq);
      mpq_set(to->value.mpq, from->value.mpq);
      break;
#endif
    case V_FLOAT:
      to->value.f = from->value.f;
  }
}

#ifdef O_BIGNUM

		 /*******************************
		 *	 FLOAT <-> RATIONAL	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
From   https://gmplib.org/list-archives/gmp-devel/2013-April/003223.html
Changed interface to be compatible to  the   previous  code we used from
ECLiPSe.  Whereas  the  ECLiPSe  truncates,  this  code  rounds  towards
nearest.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static double
mpz_fdiv(mpz_t a, mpz_t b)
{ size_t sa = mpz_sizeinbase(a, 2);
  size_t sb = mpz_sizeinbase(b, 2);
  size_t na, nb;
  mpz_t aa, bb;
  double d;

  /* easy case: |a|, |b| < 2^53, no overflow nor underflow can occur */
  if ( sa <= 53 && sb <= 53 )
    return mpz_get_d(a) / mpz_get_d(b);

  /* same if a = m*2^e with m representable on 53 bits, idem for b, but beware
     that both a and b do not give an overflow */
  na = sa - mpz_scan1(a, 0);
  nb = sb - mpz_scan1(b, 0);
  if (sa <= 1024 && na <= 53 && sb <= 1024 && nb <= 53)
    return mpz_get_d(a) / mpz_get_d(b);

  /* hard case */
  mpz_init(aa);
  mpz_init(bb);

  if (sa >= sb)
  { mpz_set(aa, a);
    mpz_mul_2exp(bb, b, sa - sb);
  } else
  { mpz_mul_2exp(aa, a, sb - sa);
    mpz_set(bb, b);
  }

  /* q = aa/bb*2^(sa-sb) */

  if ( mpz_cmpabs(aa, bb) >= 0 )
  { mpz_mul_2exp(bb, bb, 1);
    sa++;
  }

  mpz_mul_2exp(aa, aa, 54);
  sb += 54;

  mpz_tdiv_qr(aa, bb, aa, bb);

  /* the quotient aa should have exactly 54 bits */

  switch(fegetround())
  { case FE_TONEAREST:
      if ( mpz_tstbit(aa, 0) == 0 )
      {
      } else if ( mpz_sgn(bb) != 0 )
      { if ( mpz_sgn(aa) > 0 )
	  mpz_add_ui(aa, aa, 1);
	else
	  mpz_sub_ui(aa, aa, 1);
      } else /* mid case: round to even */
      { if (mpz_tstbit(aa, 1) == 0)
	{ if (mpz_sgn(aa) > 0)
	    mpz_sub_ui(aa, aa, 1);
	  else
	    mpz_add_ui(aa, aa, 1);
	} else
	{ if (mpz_sgn(aa) > 0)
	    mpz_add_ui(aa, aa, 1);
	  else
	    mpz_sub_ui(aa, aa, 1);
	}
      }
      break;
    case FE_UPWARD:        // negative aa defers to truncate (mpz_get_d)
      if (mpz_sgn(aa) > 0 && (mpz_sgn(bb) != 0 || mpz_tstbit(aa, 0) == 1))
	mpz_add_ui(aa, aa, 2);
      break;
    case FE_DOWNWARD:      // positive aa defers to truncate (mpz_get_d)
      if (mpz_sgn(aa) < 0 && (mpz_sgn(bb) != 0 || mpz_tstbit(aa, 0) == 1))
	mpz_sub_ui(aa, aa, 2);
      break;
    case FE_TOWARDZERO:    // truncation performed by mpz_get_d
      break;

  }  /* switch(fegetround()) */

  d = mpz_get_d(aa); /* exact */
  mpz_clear(aa);
  mpz_clear(bb);

  return ldexp(d, (long)sa - (long)sb);
}

/* Convert MPZ to a double by appropriate rounding of result from mpz_get_d */

double
mpz_to_double(mpz_t a)
{ double d = mpz_get_d(a);  // truncated, note: a != 0
  size_t sa = mpz_sizeinbase(a, 2);
  size_t na = mpz_scan1(a, 0);
  int bit54, trailing_zeros;

  /* float overflow check */
  if ( isinf(d) )
    return d;

  /* if a = m*2^e with m representable on 53 bits */
  if ( (sa-na) <= 53 )
    return d;  // truncated value is accurate

  /* round d outwards as determined by rounding mode */
  /* bit54 is true if rounding compensation required */
  /* trailing_zeros is true if remaining trailing bits are zero */

  bit54 = mpz_tstbit(a, sa-54);
  trailing_zeros = (na >= sa-54);

  switch(fegetround())  /* Note: all uses of nexttoward can overflow */
  { case FE_TONEAREST:
      if ( d > 0 )
      { if ( !bit54 )                           // d is positive
	{
	} else if ( !trailing_zeros || mpz_tstbit(a, sa-53) == 1 )
	{ d = nexttoward(d,  INFINITY);
	}
      } else
      { if ( bit54 && !trailing_zeros )		// d is negative
	{
	} else if ( !trailing_zeros || mpz_tstbit(a, sa-53) == 0 )
	{ d = nexttoward(d, -INFINITY);
	}
      }
      break;
    case FE_UPWARD:
      if ( d > 0 && (!trailing_zeros || bit54) )
	d = nexttoward(d,  INFINITY);
      break;
    case FE_DOWNWARD:
      if ( d < 0 && (!trailing_zeros || bit54) )
	d = nexttoward(d, -INFINITY);
      break;
    case FE_TOWARDZERO:    // truncation already performed by mpz_get_d
      break;
  }

  return d;
}

double
mpq_to_double(mpq_t q)
{ return mpz_fdiv(mpq_numref(q), mpq_denref(q));
}

/*
 * Try  to compute  a "nice"  rational from  a float  using continued
 * fractions and rational arithmetic.  Stop when rational converts
 * (using mpz_fdiv) back into the original float.
 *
 * Prolog implementation:

rat_rationalize(Flt, Rat) :-
   R is rational(Flt), rational(R, N, D),
   rat_iter((N,D), (1,0), (0,1), Flt, Rat).

rat_iter((V,W), (M,N), (P,Q), Flt, Rat) :-
	divmod(V, W, D, U),
	A is D*M+P,  %  A = Pnxt
	B is D*N+Q,  %  B = Qnxt
	Try is A rdiv B,
	( (float(Try) =:= Flt ; U == 0)  % terminating conditions
	  -> Rat = Try
	  ;  rat_iter((W,U), (A,B), (M,N), Flt, Rat)
	).

 * (*) We are  done if both 64-bit doubles have  the same bit pattern.
 * We must  notably avoid  that we compare  the _extended  width float
 * register_ found in e.g. x87 hardware.   One way to force this is to
 * place the temporary  result in memory, but unless  we use something
 * the compiler doesn't  know about or the  compiler cannot gurarantee
 * it is used elsewhere may cause  inlining.  Therefore we use a union
 * and compare the integer component.
 */

void
mpq_set_double(mpq_t r, double f)	/* float -> nice rational */
{ mpz_t m; mpz_init_set_si(m, 1);   // (m,n) = (1,0)
  mpz_t n; mpz_init_set_si(n, 0);
  mpz_t p; mpz_init_set_si(p, 0);   // (p,q) = (0,1)
  mpz_t q; mpz_init_set_si(q, 1);

  mpq_set_d(r, f);
  mpz_t v; mpz_init(v); mpq_get_num(v,r);  // (v,w) == (r.num, r.den)
  mpz_t w; mpz_init(w); mpq_get_den(w,r);

  mpz_t d; mpz_init(d);
  mpz_t u; mpz_init(u);

  fpattern fp = { .d = f };	/* see (*) */
  fpattern rp;
  for(;;)
  { mpz_fdiv_qr(d,u,v,w);
    mpz_addmul(p,d,m);
    mpz_addmul(q,d,n);
    rp.d = mpz_fdiv(p,q);
    if ((rp.i == fp.i) || (mpz_sgn(u) == 0)) // terminating conditions
    { mpq_set_num(r,p);			     // final answer, p & q are co-prime
      mpq_set_den(r,q);
      break;
    } else {
      mpz_set(v,w);  mpz_set(w,u);
      mpz_swap(m,p); mpz_swap(n,q);
    }
  }

  mpz_clear(m);
  mpz_clear(n);
  mpz_clear(p);
  mpz_clear(q);
  mpz_clear(v);
  mpz_clear(w);
  mpz_clear(d);
  mpz_clear(u);
}


		 /*******************************
		 *	 PUBLIC INTERFACE	*
		 *******************************/

bool
PL_get_mpz(term_t t, mpz_t mpz)
{ GET_LD
  Word p = valTermRef(t);

  deRef(p);
  if ( isInteger(*p) )
  { number n;

    get_integer(*p, &n);
    switch(n.type)
    { case V_INTEGER:
	promoteToMPZNumber(&n);
	mpz_set(mpz, n.value.mpz);
	clearNumber(&n);
	break;
      case V_MPZ:
	mpz_set(mpz, n.value.mpz);
	break;
      default:
	assert(0);
    }

    return true;
  }

  return false;
}


bool
PL_get_mpq(term_t t, mpq_t mpq)
{ if ( PL_is_rational(t) )
  { GET_LD
    number n;
    Word p = valTermRef(t);

    deRef(p);
    get_rational(*p, &n);
    switch(n.type)
    { case V_INTEGER:
	if ( n.value.i >= LONG_MIN && n.value.i <= LONG_MAX )
	{ mpq_set_si(mpq, (long)n.value.i, 1L);
	  return true;
	}
	promoteToMPZNumber(&n);
	/*FALLTHROUGH*/
      case V_MPZ:
	mpq_set_z(mpq, n.value.mpz);
	clearNumber(&n);
	return true;
      case V_MPQ:
	mpq_set(mpq, n.value.mpq);
	clearNumber(&n);
	return true;
      default:
	;
    }
  }

  return false;
}


bool
PL_unify_mpz(term_t t, mpz_t mpz)
{ GET_LD
  number n;
  int rc;

  n.type = V_MPZ;
  mpz_init(n.value.mpz);
  mpz_set(n.value.mpz, mpz);

  rc = PL_unify_number(t, &n);
  clearNumber(&n);

  return rc;
}


bool
PL_unify_mpq(term_t t, mpq_t mpq)
{ GET_LD
  number n;
  int rc;

  n.type = V_MPQ;
  mpq_init(n.value.mpq);
  mpq_set(n.value.mpq, mpq);

  rc = PL_unify_number(t, &n);
  clearNumber(&n);

  return rc;
}

		 /*******************************
		 *               WIN64		*
		 *******************************/


#if defined(WIN64) && _MSC_VER <= 1400 && !defined(_CRT_ASSEMBLY_VERSION)
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
"#if (_MSC_VER <= 1400)" should suffice,   but  although both the VS2005
(VC8) and the Microsoft Server 2003   R2 (VC8 SDK) define _MSC_VER=1400,
VC8 SDK does not define the below functions, while VC8 does... The macro
below distinguishes the two.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

size_t
strnlen(const char *s, size_t maxlen)
{ size_t len = 0;

  while(*s++ && maxlen-- > 0)
    len++;

  return len;
}

void
__GSHandlerCheck()
{
}
#endif

#endif /*O_BIGNUM*/
