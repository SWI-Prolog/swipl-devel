/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2022, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
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
#undef LD
#define LD LOCAL_LD

#ifdef O_BIGNUM				/* Upto the end of this file */

static mpz_t MPZ_MIN_TAGGED;		/* Prolog tagged integers */
static mpz_t MPZ_MAX_TAGGED;
static mpz_t MPZ_MIN_PLINT;		/* Prolog int64_t integers */
static mpz_t MPZ_MAX_PLINT;
static mpz_t MPZ_MAX_UINT64;
#if SIZEOF_LONG	< SIZEOF_VOIDP
static mpz_t MPZ_MIN_LONG;		/* Prolog int64_t integers */
static mpz_t MPZ_MAX_LONG;
#endif

#define abs(v) ((v) < 0 ? -(v) : (v))

typedef union
{ double  d;
  int64_t i;
} fpattern;


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
	(LD == NULL || LD->gmp.context == NULL || LD->gmp.persistent)

static int
gmp_too_big(void)
{ GET_LD

  DEBUG(1, Sdprintf("Signalling GMP overflow\n"));

  return (int)outOfStack((Stack)&LD->stacks.global, STACK_OVERFLOW_THROW);
}

#define TOO_BIG_GMP(n) ((n) > 1000 && (n) > (size_t)globalStackLimit())

static void *
mp_alloc(size_t bytes)
{ GET_LD
  mp_mem_header *mem;

  if ( NOT_IN_PROLOG_ARITHMETIC() )
    return smp_alloc(bytes);

  if ( TOO_BIG_GMP(bytes) ||
       !(mem = malloc(sizeof(mp_mem_header)+bytes)) )
  { gmp_too_big();
    abortProlog();
    PL_rethrow();
    return NULL;			/* make compiler happy */
  }

#if O_BF
  if ( bytes == 0 )
    return NULL;
#endif

  GMP_LEAK_CHECK(LD->gmp.allocated += bytes);

  mem->next = NULL;
  mem->context = LD->gmp.context;
  if ( LD->gmp.tail )
  { mem->prev = LD->gmp.tail;
    LD->gmp.tail->next = mem;
    LD->gmp.tail = mem;
  } else
  { mem->prev = NULL;
    LD->gmp.head = LD->gmp.tail = mem;
  }
  DEBUG(9, Sdprintf("GMP: alloc %ld@%p\n", bytes, &mem[1]));

  return &mem[1];
}


static void
mp_free(void *ptr, size_t size)
{ GET_LD
  mp_mem_header *mem;

  if ( NOT_IN_PROLOG_ARITHMETIC() )
  { smp_free(ptr, size);
    return;
  }

#if O_BF
  if ( !ptr )
    return;
#endif

  mem = ((mp_mem_header*)ptr)-1;

  if ( mem == LD->gmp.head )
  { LD->gmp.head = LD->gmp.head->next;
    if ( LD->gmp.head )
      LD->gmp.head->prev = NULL;
    else
      LD->gmp.tail = NULL;
  } else if ( mem == LD->gmp.tail )
  { LD->gmp.tail = LD->gmp.tail->prev;
    LD->gmp.tail->next = NULL;
  } else
  { mem->prev->next = mem->next;
    mem->next->prev = mem->prev;
  }

  free(mem);
  DEBUG(9, Sdprintf("GMP: free: %ld@%p\n", size, ptr));
  GMP_LEAK_CHECK(LD->gmp.allocated -= size);
}


static void *
mp_realloc(void *ptr, size_t oldsize, size_t newsize)
{ GET_LD
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

  oldmem = ((mp_mem_header*)ptr)-1;
  if ( TOO_BIG_GMP(newsize) ||
       !(newmem = realloc(oldmem, sizeof(mp_mem_header)+newsize)) )
  { gmp_too_big();
    abortProlog();
    PL_rethrow();
    return NULL;			/* make compiler happy */
  }

  if ( oldmem != newmem )		/* re-link if moved */
  { if ( newmem->prev )
      newmem->prev->next = newmem;
    else
      LD->gmp.head = newmem;

    if ( newmem->next )
      newmem->next->prev = newmem;
    else
      LD->gmp.tail = newmem;
  }

  GMP_LEAK_CHECK(LD->gmp.allocated -= oldsize;
		 LD->gmp.allocated += newsize);
  DEBUG(9, Sdprintf("GMP: realloc %ld@%p --> %ld@%p\n", oldsize, ptr, newsize, &newmem[1]));

  return &newmem[1];
}


void
mp_cleanup(ar_context *ctx)
{ GET_LD
  mp_mem_header *mem, *next;

  if ( LD->gmp.context )
  { for(mem=LD->gmp.head; mem; mem=next)
    { next = mem->next;
      if ( mem->context == LD->gmp.context )
      { DEBUG(9, Sdprintf("GMP: cleanup of %p\n", &mem[1]));
	mp_free(&mem[1], 0);
      }
    }
  }

  LD->gmp.context = ctx->parent;
}
#endif


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

      if ( rc != TRUE )
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

  return TRUE;
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

      if ( rc != TRUE )
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

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_integer() fetches the value of a Prolog  term known to be an integer
into a number structure. If the  value  is   a  MPZ  number,  it must be
handled as read-only and it only be used   as  intptr_t as no calls are made
that may force a relocation or garbage collection on the global stack.

The version without O_GMP is a macro defined in pl-gmp.h
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
get_integer(word w, Number n)
{ if ( storage(w) == STG_INLINE )
  { n->type = V_INTEGER,
    n->value.i = valInt(w);
  } else
  { GET_LD
    Word p = addressIndirect(w);
    size_t wsize = wsizeofInd(*p);

    p++;
    if ( wsize == WORDS_PER_INT64 )
    { n->type = V_INTEGER;
      memcpy(&n->value.i, p, sizeof(int64_t));
    } else
    { n->type = V_MPZ;

#if O_GMP
      n->value.mpz->_mp_size  = mpz_stack_size(*p++);
      n->value.mpz->_mp_alloc = 0;
      n->value.mpz->_mp_d     = (mp_limb_t*) p;
#elif O_BF
      slimb_t len = mpz_stack_size(*p++);
      n->value.mpz->ctx	 = NULL;
      n->value.mpz->expn = (slimb_t)*p++;
      n->value.mpz->sign = len < 0;
      n->value.mpz->len  = abs(len);
      n->value.mpz->tab  = (limb_t*)p;
#endif
    }
  }
}


void
get_rational(DECL_LD word w, Number n)
{ if ( storage(w) == STG_INLINE )
  { n->type = V_INTEGER,
    n->value.i = valInt(w);
  } else
  { Word p = addressIndirect(w);
    size_t wsize = wsizeofInd(*p);

    p++;
    if ( wsize == WORDS_PER_INT64 )
    { n->type = V_INTEGER;
      memcpy(&n->value.i, p, sizeof(int64_t));
    } else if ( (*p&MP_RAT_MASK) )
    { mpz_t num, den;
      size_t num_size;

      n->type = V_MPQ;
#if O_GMP
      num->_mp_size  = mpz_stack_size(*p++);
      num->_mp_alloc = 0;
      num->_mp_d     = (mp_limb_t*) (p+1);
      num_size = mpz_wsize(num, NULL);
      den->_mp_size  = mpz_stack_size(*p++);
      den->_mp_alloc = 0;
      den->_mp_d     = (mp_limb_t*) (p+num_size);
#elif O_BF
      slimb_t len = mpz_stack_size(*p++);
      num->ctx	= NULL;
      num->expn = (slimb_t)*p++;
      num->sign = len < 0;
      num->len  = abs(len);
      num->tab  = (limb_t*)(p+2);
      num_size  = mpz_wsize(num, NULL);
      den->ctx  = NULL;
      den->sign = 0;			/* canonical MPQ */
      den->len  = mpz_stack_size(*p++);
      den->expn = (slimb_t)*p++;
      den->tab  = (limb_t*) (p+num_size);
#endif
      *mpq_numref(n->value.mpq) = num[0];
      *mpq_denref(n->value.mpq) = den[0];
    } else
    { n->type = V_MPZ;

#if O_GMP
      n->value.mpz->_mp_size  = mpz_stack_size(*p++);
      n->value.mpz->_mp_alloc = 0;
      n->value.mpz->_mp_d     = (mp_limb_t*) p;
#elif O_BF
      slimb_t len = mpz_stack_size(*p++);
      n->value.mpz->ctx	 = NULL;
      n->value.mpz->expn = (slimb_t)*p++;
      n->value.mpz->sign = len < 0;
      n->value.mpz->len  = abs(len);
      n->value.mpz->tab  = (limb_t*)p;
#endif
    }
  }
}


Code
get_mpz_from_code(Code pc, mpz_t mpz)
{ size_t wsize = wsizeofInd(*pc);

  pc++;
#if O_GMP
  mpz->_mp_size  = mpz_stack_size(*pc);
  mpz->_mp_alloc = 0;
  mpz->_mp_d     = (mp_limb_t*)(pc+1);
#elif O_BF
  slimb_t len = mpz_stack_size(*pc);
  mpz->ctx    = NULL;
  mpz->expn   = (slimb_t)pc[1];
  mpz->sign   = len < 0;
  mpz->len    = abs(len);
  mpz->tab    = (limb_t*)pc+2;
#endif

  return pc+wsize;
}

Code
get_mpq_from_code(Code pc, mpq_t mpq)
{ Word p = pc;
  size_t wsize = wsizeofInd(*p);
  p++;
  int num_size = mpz_stack_size(*p++);
  int den_size = mpz_stack_size(*p++);
  size_t limpsize = sizeof(mp_limb_t) * abs(num_size);
  mpz_t num, den;

#if O_GMP
  num->_mp_size   = num_size;
  den->_mp_size   = den_size;
  num->_mp_alloc  = 0;
  den->_mp_alloc  = 0;
  num->_mp_d = (mp_limb_t*)p;
  p += (limpsize+sizeof(word)-1)/sizeof(word);
  den->_mp_d = (mp_limb_t*)p;
#elif O_BF
  num->ctx = NULL;
  num->expn = *p++;
  den->ctx = NULL;
  den->expn = *p++;
  num->sign = num_size < 0;
  num->len  = abs(num_size);
  den->sign = den_size < 0;
  den->len  = abs(den_size);
  num->tab = (mp_limb_t*)p;
  p += (limpsize+sizeof(word)-1)/sizeof(word);
  den->tab = (mp_limb_t*)p;
#endif
  *mpq_numref(mpq) = num[0];
  *mpq_denref(mpq) = den[0];

  return pc+wsize+1;
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
{ int size = 0;

  size |= (data[0]&0xff)<<24;
  size |= (data[1]&0xff)<<16;
  size |= (data[2]&0xff)<<8;
  size |= (data[3]&0xff);
  size = (size << SHIFTSIGN32)>>SHIFTSIGN32;	/* sign extend */

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
      *neg = TRUE;
    } else
    { *neg = FALSE;
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
{ GET_LD
  int size = 0;
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
{ GET_LD
  int num_size;
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

  return TRUE;
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

  return TRUE;
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
  { GD->gmp.initialised = TRUE;

#if O_BF
    initBF();
#endif

    mpz_init_set_si64(MPZ_MIN_TAGGED, PLMINTAGGEDINT);
    mpz_init_set_si64(MPZ_MAX_TAGGED, PLMAXTAGGEDINT);
    mpz_init_set_si64(MPZ_MIN_PLINT, PLMININT);
    mpz_init_set_si64(MPZ_MAX_PLINT, PLMAXINT);
    mpz_init_max_uint(MPZ_MAX_UINT64, 64);
#if SIZEOF_LONG < SIZEOF_VOIDP
    mpz_init_set_si64(MPZ_MIN_LONG, LONG_MIN);
    mpz_init_set_si64(MPZ_MAX_LONG, LONG_MAX);
#endif
#ifdef O_MY_GMP_ALLOC
    if ( !GD->gmp.keep_alloc_functions )
    { mp_get_memory_functions(&smp_alloc, &smp_realloc, &smp_free);
      mp_set_memory_functions(mp_alloc, mp_realloc, mp_free);
    }
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
  { GD->gmp.initialised = FALSE;

#ifdef O_MY_GMP_ALLOC
    if ( !GD->gmp.keep_alloc_functions )
      mp_set_memory_functions(smp_alloc, smp_realloc, smp_free);
#endif
    mpz_clear(MPZ_MIN_TAGGED);
    mpz_clear(MPZ_MAX_TAGGED);
    mpz_clear(MPZ_MIN_PLINT);
    mpz_clear(MPZ_MAX_PLINT);
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
{ if ( mpz_cmp(mpz, MPZ_MIN_PLINT) >= 0 &&
       mpz_cmp(mpz, MPZ_MAX_PLINT) <= 0 )
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

    return TRUE;
  }

  return FALSE;
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
{ int64_t v;

  DEBUG(2,
	{ char buf[256];
	  Sdprintf("put_mpz(%s)\n",
		   mpz_get_str(buf, 10, mpz));
	});

#if SIZEOF_LONG < SIZEOF_VOIDP
  if ( mpz_cmp(mpz, MPZ_MIN_LONG) >= 0 &&
       mpz_cmp(mpz, MPZ_MAX_LONG) <= 0 )
#else
  if ( mpz_cmp(mpz, MPZ_MIN_TAGGED) >= 0 &&
       mpz_cmp(mpz, MPZ_MAX_TAGGED) <= 0 )
#endif
  { long v = mpz_get_si(mpz);

    if ( !hasGlobalSpace(0) )		/* ensure we have room for bindConst */
    { int rc = ensureGlobalSpace(0, flags);

      if ( rc != TRUE )
	return rc;
    }

    *at = consInt(v);
    return TRUE;
  } else if ( mpz_to_int64(mpz, &v) )
  { return put_int64(at, v, flags);
  } else
  { return globalMPZ(at, mpz, flags);
  }
}

#endif /*O_GMP*/

/* returns one of

  TRUE: ok
  FALSE: some error
  GLOBAL_OVERFLOW: no space
  LOCAL_OVERFLOW: cannot represent (no GMP)
*/

int
put_uint64(DECL_LD Word at, uint64_t l, int flags)
{ if ( (int64_t)l >= 0 )
  { return put_int64(at, l, flags);
  } else
  {
#ifdef O_BIGNUM
    mpz_t mpz;

    mpz_init_set_uint64(mpz, l);
    return put_mpz(at, mpz, flags);
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

    if ( (rc=put_number(&c, n, ALLOW_GC)) == TRUE )
      bindConst(<somewhere>, c);
    ...
  }
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
put_number(DECL_LD Word at, Number n, int flags)
{ switch(n->type)
  { case V_INTEGER:
    { word w = consInt(n->value.i);

      if ( valInt(w) == n->value.i )
      { if ( !hasGlobalSpace(0) )
	{ int rc = ensureGlobalSpace(0, flags);

	  if ( rc != TRUE )
	    return rc;
	}

	*at = w;
	return TRUE;
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
  return FALSE;
}


int
PL_unify_number(DECL_LD term_t t, Number n)
{ Word p = valTermRef(t);

  deRef(p);

  if ( canBind(*p) )
  { word w;
    int rc;

    if ( (rc=put_number(&w, n, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);

    p = valTermRef(t);			/* put_number can shift the stacks */
    deRef(p);

    bindConst(p, w);
    succeed;
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

  if ( (rc=put_number(&w, n, ALLOW_GC)) != TRUE )
    return raiseStackOverflow(rc);

  *valTermRef(t) = w;

  return TRUE;
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

int
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
      break;
  }

  return check_float(n);
}


int
promoteNumber(Number n, numtype t)
{ switch(t)
  { case V_INTEGER:
      return TRUE;
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
      return FALSE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
same_type_numbers(n1, n2)
    Upgrade both numbers to the `highest' type of both. Number types are
    defined in the enum-type numtype, which is supposed to define a
    total ordering between the number types.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
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
{ if ( n1->type != n2->type )
  { int rc;

    if ( n1->type == V_FLOAT || n2->type == V_FLOAT )
      return cmpFloatNumbers(n1, n2);
    rc = make_same_type_numbers(n1, n2);
    assert(rc != CMP_ERROR);
    (void)rc;
  }

  switch(n1->type)
  { case V_INTEGER:
      return n1->value.i  < n2->value.i ? CMP_LESS :
	     n1->value.i == n2->value.i ? CMP_EQUAL : CMP_GREATER;
#ifdef O_BIGNUM
    case V_MPZ:
    { int rc = mpz_cmp(n1->value.mpz, n2->value.mpz);

      return rc < 0 ? CMP_LESS : rc == 0 ? CMP_EQUAL : CMP_GREATER;
    }
    case V_MPQ:
    { int rc = mpq_cmp(n1->value.mpq, n2->value.mpq);

      return rc < 0 ? CMP_LESS : rc == 0 ? CMP_EQUAL : CMP_GREATER;
    }
#endif
    case V_FLOAT:
      return n1->value.f  < n2->value.f ? CMP_LESS :
	     n1->value.f == n2->value.f ? CMP_EQUAL :
	     n1->value.f  > n2->value.f ? CMP_GREATER : CMP_NOTEQ;
  }

  assert(0);
  return CMP_EQUAL;
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

int
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

    return TRUE;
  }

  return FALSE;
}


int
PL_get_mpq(term_t t, mpq_t mpq)
{ if ( PL_is_rational(t) )
  { GET_LD
    number n;

    get_rational(t, &n);
    switch(n.type)
    { case V_INTEGER:
	if ( n.value.i >= LONG_MIN && n.value.i <= LONG_MAX )
	{ mpq_set_si(mpq, (long)n.value.i, 1L);
	  return TRUE;
	}
	promoteToMPZNumber(&n);
	/*FALLTHROUGH*/
      case V_MPZ:
	mpq_set_z(mpq, n.value.mpz);
	clearNumber(&n);
	return TRUE;
      case V_MPQ:
	mpq_set(mpq, n.value.mpq);
	clearNumber(&n);
	return TRUE;
      default:
	;
    }
  }

  return FALSE;
}


int
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


int
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
