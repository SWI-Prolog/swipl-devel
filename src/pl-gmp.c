/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2020, University of Amsterdam
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

#ifdef O_GMP

static mpz_t MPZ_MIN_TAGGED;		/* Prolog tagged integers */
static mpz_t MPZ_MAX_TAGGED;
static mpz_t MPZ_MIN_PLINT;		/* Prolog int64_t integers */
static mpz_t MPZ_MAX_PLINT;
static mpz_t MPZ_MAX_UINT64;
#if SIZEOF_LONG	< SIZEOF_VOIDP
static mpz_t MPZ_MIN_LONG;		/* Prolog int64_t integers */
static mpz_t MPZ_MAX_LONG;
#endif


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

static int
gmp_too_big()
{ GET_LD

  DEBUG(1, Sdprintf("Signalling GMP overflow\n"));

  return (int)outOfStack((Stack)&LD->stacks.global, STACK_OVERFLOW_THROW);
}

#define TOO_BIG_GMP(n) ((n) > 1000 && (n) > (size_t)globalStackLimit())

static void *
mp_alloc(size_t bytes)
{ GET_LD
  mp_mem_header *mem;

  if ( LD->gmp.persistent )
    return malloc(bytes);

  if ( TOO_BIG_GMP(bytes) ||
       !(mem = malloc(sizeof(mp_mem_header)+bytes)) )
  { gmp_too_big();
    abortProlog();
    PL_rethrow();
    return NULL;			/* make compiler happy */
  }

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


static void *
mp_realloc(void *ptr, size_t oldsize, size_t newsize)
{ GET_LD
  mp_mem_header *oldmem, *newmem;

  if ( LD->gmp.persistent )
    return realloc(ptr, newsize);

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


static void
mp_free(void *ptr, size_t size)
{ GET_LD
  mp_mem_header *mem;

  if ( LD->gmp.persistent )
  { free(ptr);
    return;
  }

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
globalMPZ() pushes an mpz type GMP  integer   onto  the local stack. The
saved version is the _mp_size field, followed by the limps.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
mpz_wsize(mpz_t mpz, size_t *s)
{ DEBUG(0, assert(sizeof(mpz->_mp_size) == sizeof(int)));
  size_t size = sizeof(mp_limb_t)*abs(mpz->_mp_size);
  size_t wsz  = (size+sizeof(word)-1)/sizeof(word);

  if ( s )
    *s = size;

  return wsz;
}


#define globalMPZ(at, mpz, flags) LDFUNC(globalMPZ, at, mpz, flags)
static int
globalMPZ(DECL_LD Word at, mpz_t mpz, int flags)
{ DEBUG(CHK_SECURE, assert(!onStackArea(global, at) && !onStackArea(local, at)));

  if ( mpz->_mp_alloc )
  { size_t size, wsz;
    Word p;
    word m;

  copy:
    wsz = mpz_wsize(mpz, &size);
    m   = mkIndHdr(wsz+1, TAG_INTEGER);

    if ( wsizeofInd(m) != wsz+1 )
    { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_integer);
      return 0;
    }

    if ( !hasGlobalSpace(wsz+3) )
    { int rc = ensureGlobalSpace(wsz+3, flags);

      if ( rc != TRUE )
	return rc;
    }
    p = gTop;
    gTop += wsz+3;

    *at = consPtr(p, TAG_INTEGER|STG_GLOBAL);

    *p++     = m;
    p[wsz]   = 0L;			/* pad out */
    p[wsz+1] = m;
    *p++     = mpz_size_stack(mpz->_mp_size);
    memcpy(p, mpz->_mp_d, size);
  } else				/* already on the stack */
  { Word p = (Word)mpz->_mp_d - 2;
    if ( !onStack(global, p) )
      goto copy;
#ifndef NDEBUG
    size_t size;
    size_t wsz = mpz_wsize(mpz, &size);
    assert(p[0] == mkIndHdr(wsz+1, TAG_INTEGER));
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

  if ( num->_mp_alloc || den->_mp_alloc )
  { size_t num_size, den_size, num_wsz, den_wsz;
    Word p;
    word m;

  copy:
    num_wsz = mpz_wsize(num, &num_size);
    den_wsz = mpz_wsize(den, &den_size);
    m       = mkIndHdr(num_wsz+den_wsz+2, TAG_INTEGER);

    if ( wsizeofInd(m) != num_wsz+den_wsz+2 )
    { PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_rational);
      return 0;
    }

    if ( !hasGlobalSpace(num_wsz+den_wsz+4) )
    { int rc = ensureGlobalSpace(num_wsz+den_wsz+4, flags);

      if ( rc != TRUE )
	return rc;
    }
    p = gTop;
    gTop += num_wsz+den_wsz+4;

    *at = consPtr(p, TAG_INTEGER|STG_GLOBAL);
    *p++ = m;
    *p++ = mpq_size_stack(num->_mp_size);
    *p++ = mpq_size_stack(den->_mp_size);
    p[num_wsz-1] = 0L;				/* pad out */
    memcpy(p, num->_mp_d, num_size);
    p += num_wsz;
    p[den_wsz-1] = 0L;				/* pad out */
    memcpy(p, den->_mp_d, den_size);
    p += den_wsz;
    *p = m;
  } else					/* already on the stack */
  { Word p = (Word)num->_mp_d - 3;
    if ( !onStack(global, p) )
      goto copy;
    DEBUG(CHK_SECURE,
	  { size_t num_size;
	    size_t den_size;
	    size_t num_wsz = mpz_wsize(num, &num_size);
	    size_t den_wsz = mpz_wsize(den, &den_size);
	    assert(p[0] == mkIndHdr(num_wsz+den_wsz+2, TAG_INTEGER));
	    assert((Word)den->_mp_d == (Word)num->_mp_d + num_wsz);
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

      n->value.mpz->_mp_size  = mpz_stack_size(*p++);
      n->value.mpz->_mp_alloc = 0;
      n->value.mpz->_mp_d     = (mp_limb_t*) p;
    }
  }
}


void
get_rational(word w, Number n)
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
    } else if ( (*p&MP_RAT_MASK) )
    { mpz_t num, den;
      size_t num_size;

      n->type = V_MPQ;
      num->_mp_size  = mpz_stack_size(*p++);
      num->_mp_alloc = 0;
      num->_mp_d     = (mp_limb_t*) (p+1);
      num_size = mpz_wsize(num, NULL);
      den->_mp_size  = mpz_stack_size(*p++);
      den->_mp_alloc = 0;
      den->_mp_d     = (mp_limb_t*) (p+num_size);

      *mpq_numref(n->value.mpq) = num[0];
      *mpq_denref(n->value.mpq) = den[0];
    } else
    { n->type = V_MPZ;

      n->value.mpz->_mp_size  = mpz_stack_size(*p++);
      n->value.mpz->_mp_alloc = 0;
      n->value.mpz->_mp_d     = (mp_limb_t*) p;
    }
  }
}


Code
get_mpz_from_code(Code pc, mpz_t mpz)
{ size_t wsize = wsizeofInd(*pc);

  pc++;
  mpz->_mp_size  = mpz_stack_size(*pc);
  mpz->_mp_alloc = 0;
  mpz->_mp_d     = (mp_limb_t*)(pc+1);

  return pc+wsize;
}

Code
get_mpq_from_code(Code pc, mpq_t mpq)
{ Word p = pc;
  size_t wsize = wsizeofInd(*p);
  p++;
  int num_size = mpz_stack_size(*p++);
  int den_size = mpz_stack_size(*p++);
  size_t limpsize;

  mpq_numref(mpq)->_mp_size  = num_size;
  mpq_denref(mpq)->_mp_size  = den_size;
  mpq_numref(mpq)->_mp_alloc = 0;
  mpq_denref(mpq)->_mp_alloc = 0;
  mpq_numref(mpq)->_mp_d     = (mp_limb_t*)p;
  limpsize = sizeof(mp_limb_t) * abs(num_size);
  p += (limpsize+sizeof(word)-1)/sizeof(word);
  mpq_denref(mpq)->_mp_d     = (mp_limb_t*)p;

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

  mpz->_mp_size  = limpsize;
  mpz->_mp_alloc = limpsize;
  mpz->_mp_d     = (mp_limb_t*)p;

  mpz_import(mpz, size, 1, 1, 1, 0, data);
  assert((Word)mpz->_mp_d == p);	/* check no (re-)allocation is done */

  return (char*)data+size;
}

char *
loadMPZFromCharp(const char *data, Word r, Word *store)
{ GET_LD
  int size = 0;
  size_t limpsize;
  size_t wsize;
  int neg;
  Word p;
  word m;

  data = load_abs_mpz_size(data, &size, &neg);

  limpsize = (size+sizeof(mp_limb_t)-1)/sizeof(mp_limb_t);
  wsize = (limpsize*sizeof(mp_limb_t)+sizeof(word)-1)/sizeof(word);
  p = *store;
  *store += (wsize+3);
  *r = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  m = mkIndHdr(wsize+1, TAG_INTEGER);
  *p++ = m;
  p[wsize] = 0L;			/* pad out */
  p[wsize+1] = m;
  *p++ = mpz_size_stack(neg ? -limpsize : limpsize);

  return load_mpz_bits(data, size, limpsize, neg, p);
}

char *
loadMPQFromCharp(const char *data, Word r, Word *store)
{ GET_LD
  int num_size;
  int den_size;
  size_t num_limpsize, num_wsize;
  size_t den_limpsize, den_wsize;
  int num_neg, den_neg;
  size_t wsize;
  Word p;
  word m;

  data = load_abs_mpz_size(data, &num_size, &num_neg);
  data = load_abs_mpz_size(data, &den_size, &den_neg);

  num_limpsize = (num_size+sizeof(mp_limb_t)-1)/sizeof(mp_limb_t);
  num_wsize = (num_limpsize*sizeof(mp_limb_t)+sizeof(word)-1)/sizeof(word);
  den_limpsize = (den_size+sizeof(mp_limb_t)-1)/sizeof(mp_limb_t);
  den_wsize = (den_limpsize*sizeof(mp_limb_t)+sizeof(word)-1)/sizeof(word);
  wsize = num_wsize+den_wsize;

  p = *store;
  *store += (wsize+4);
  *r = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  m = mkIndHdr(wsize+2, TAG_INTEGER);
  *p++ = m;
  *p++ = mpq_size_stack(num_neg ? -num_limpsize : num_limpsize);
  *p++ = mpq_size_stack(den_neg ? -den_limpsize : den_limpsize);
  p[num_wsize-1] = 0;
  data = load_mpz_bits(data, num_size, num_limpsize, num_neg, p);
  p += num_wsize;
  p[den_wsize-1] = 0;
  data = load_mpz_bits(data, den_size, den_limpsize, den_neg, p);
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


static void
mpz_init_set_uint64(mpz_t mpz, uint64_t i)
{
#if SIZEOF_LONG == 8
  mpz_init_set_ui(mpz, (unsigned long)i);
#else
  mpz_init(mpz);
  mpz_import(mpz, sizeof(i), ORDER, 1, 0, 0, &i);
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
    { n->value.mpq->_mp_num = n->value.mpz[0];
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
      if ( !n->value.mpz->_mp_alloc )
      { mpz_t tmp;

	tmp[0] = n->value.mpz[0];
	mpz_init_set(n->value.mpz, tmp);
	break;
      }
    case V_MPQ:
    { if ( !mpq_numref(n->value.mpq)->_mp_alloc )
      { mpz_t tmp;

	tmp[0] = mpq_numref(n->value.mpq)[0];
	mpz_init_set(mpq_numref(n->value.mpq), tmp);
      }
      if ( !mpq_denref(n->value.mpq)->_mp_alloc )
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
      if ( n->value.mpz->_mp_alloc )
	mpz_clear(n->value.mpz);
      break;
    case V_MPQ:
      if ( mpq_numref(n->value.mpq)->_mp_alloc )
	mpz_clear(mpq_numref(n->value.mpq));
      if ( mpq_denref(n->value.mpq)->_mp_alloc )
	mpz_clear(mpq_denref(n->value.mpq));
      break;
    default:
      break;
  }
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

static void *(*smp_alloc)(size_t);
static void *(*smp_realloc)(void *, size_t, size_t);
static void  (*smp_free)(void *, size_t);

void
initGMP(void)
{ if ( !GD->gmp.initialised )
  { GD->gmp.initialised = TRUE;

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

#if __GNU_MP__ > 3 && __GNU_MP__ < 6
    PL_license("lgplv3", "libgmp");
#else
    PL_license("lgplv2+", "libgmp");
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
		 *	   NUMBER HANDLING      *
		 *******************************/

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
#ifdef O_GMP
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
#ifdef O_GMP
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
#ifdef O_GMP
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
#ifdef O_GMP
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The GMP functions which convert  mpz's   and  mpq's to double's truncate
(round to zero) if necessary ignoring the current IEEE rounding mode. To
correct this incorrect  rounding  when  the   mode  is  `to_postive`  or
`to_negative` all calls  to  mpX_get_d()  should   be  wrapped  in  this
function. Note that this function has no GMP dependencies.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

double
mpX_round(double d) {
  switch (fegetround()) {
    case FE_UPWARD  : return (d>=0) ? nexttoward(d, INFINITY) : d;
    case FE_DOWNWARD: return (d<=0) ? nexttoward(d,-INFINITY) : d;
    default: return d;
  }
}

int
promoteToFloatNumber(Number n)
{ switch(n->type)
  { case V_INTEGER:
      n->value.f = (double)n->value.i;
      n->type = V_FLOAT;
      break;
#ifdef O_GMP
    case V_MPZ:
    { double val = mpX_round(mpz_get_d(n->value.mpz));

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
#ifdef O_GMP
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
cmpFloatNumbers(Number n1, Number n2)
{ if ( n1->type == V_FLOAT )
  { double d2;

    if ( isnan(n1->value.f) )
      return CMP_NOTEQ;

    switch(n2->type)
    { case V_INTEGER:
	d2 = (double)n2->value.i;
	break;
#ifdef O_GMP
      case V_MPZ:
	d2 = mpX_round(mpz_get_d(n2->value.mpz));
	break;
      case V_MPQ:
	d2 = mpq_to_double(n2->value.mpq);
	break;
#endif
      default:
	assert(0);
	d2 = 0.0;
    }

    return n1->value.f  < d2 ? CMP_LESS :
	   n1->value.f == d2 ? CMP_EQUAL : CMP_GREATER;
  } else
  { double d1;

    assert(n2->type == V_FLOAT);

    if ( isnan(n2->value.f) )
      return CMP_NOTEQ;

    switch(n1->type)
    { case V_INTEGER:
	d1 = (double)n1->value.i;
	break;
#ifdef O_GMP
      case V_MPZ:
	d1 = mpX_round(mpz_get_d(n1->value.mpz));
	break;
      case V_MPQ:
	d1 = mpq_to_double(n1->value.mpq);
	break;
#endif
      default:
	assert(0);
	d1 = 0.0;
    }

    return n2->value.f  < d1 ? CMP_GREATER :
	   n2->value.f == d1 ? CMP_EQUAL : CMP_LESS;
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
#ifdef O_GMP
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
#ifdef O_GMP
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

#ifdef O_GMP

		 /*******************************
		 *	 FLOAT <-> RATIONAL	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code is copied from ECLiPSe  7.0_53.   This  code is covered by the
CMPL 1.1 (Cisco-style Mozilla Public License  Version 1.1), available at
www.eclipse-clp.org/license.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Divide two bignums giving a double float result. The naive solution
 *	return mpz_to_double(num) / mpz_to_double(den);
 * suffers from floating point overflows when the numbers are huge
 * and is inefficient because it looks at unnecessarily many digits.
 *
 * IEEE double precision is 53 bits mantissa and 12 bits signed exponent.
 * So the largest integer representable with doubles is 1024 bits wide,
 * of which the first 53 are ones, i.e. it lies between 2^1023 and 2^1024.
 * If the dividend's MSB is more than 1024 bits higher than the divisor's,
 * the result will always be floating point infinity (no need to divide).
 * If we do divide, we first drop excess integer precision by keeping only
 * DBL_PRECISION_LIMBS and ignoring the lower limbs for both operands
 * (i.e. we effectively scale the integers down, or right-shift them).
 */

#define MIN_LIMB_DIFF (2+1024/GMP_NUMB_BITS)
#define DBL_PRECISION_LIMBS (2+53/GMP_NUMB_BITS)
#define MAX_ULONG_DBL ((double)~0L+1.0)

static double
mpz_fdiv(mpz_t num, mpz_t den)
{ mp_ptr longer_d, shorter_d;
  mp_size_t shorter_size, longer_size, ignored_limbs = 0;
  int negative, swapped;
  /* By declaring res volatile we make sure that the result is rounded
   * to double precision instead of being returned with extended precision
   * in a floating point register, which can have confusing consequences */
  volatile double res;

  shorter_size = num->_mp_size;
  longer_size = den->_mp_size;
  negative = 0;

  if ( shorter_size < 0 )
  { shorter_size = -shorter_size;
    negative = !negative;
  }
  if ( longer_size < 0 )
  { longer_size = -longer_size;
    negative = !negative;
  }
  if ( shorter_size > longer_size )
  { longer_size = shorter_size;
    longer_d = num->_mp_d;
    shorter_size = (den->_mp_size >= 0 ? den->_mp_size : -den->_mp_size);
    shorter_d = den->_mp_d;
    swapped = 1;			/* abs(res) > 1 */
  } else
  { longer_d = den->_mp_d;
    shorter_d = num->_mp_d;
    swapped = 0;			/* abs(res) < 1 */
  }

  if ( longer_size - shorter_size > MIN_LIMB_DIFF )
  { res = swapped ? HUGE_VAL : 0.0;
  } else
  { double l,s;
    long int le, se;
    mpz_t li, si;
    int r_mode;

    /* we ignore limbs that are not significant for the result */
    if ( longer_size > MIN_LIMB_DIFF )	/* more can't be represented */
    { ignored_limbs = longer_size - MIN_LIMB_DIFF;
      longer_size -= ignored_limbs;
      shorter_size -= ignored_limbs;
    }
    if ( shorter_size > DBL_PRECISION_LIMBS )	/* more exceeds the precision */
    { ignored_limbs += shorter_size - DBL_PRECISION_LIMBS;
      longer_size -= shorter_size - DBL_PRECISION_LIMBS;
      shorter_size = DBL_PRECISION_LIMBS;
    }
    longer_d += ignored_limbs;
    shorter_d += ignored_limbs;
    li->_mp_alloc = li->_mp_size = longer_size; li->_mp_d = longer_d;
    si->_mp_alloc = si->_mp_size = shorter_size; si->_mp_d = shorter_d;

    l = mpz_get_d_2exp(&le, li);
    s = mpz_get_d_2exp(&se, si);

    /* if result negative, some rounding modes must be swapped;
       avoid if unnecessary */
    if ( negative )
    { r_mode = fegetround();

      switch (r_mode)
      { case FE_UPWARD:   fesetround(FE_DOWNWARD); break;
	case FE_DOWNWARD: fesetround(FE_UPWARD);
      }
    }

    if ( swapped )
      res = (l/s) * pow(2.0, le-se);
    else
      res = (s/l) * pow(2.0, se-le);

    if ( negative )
      fesetround(r_mode);
  }

  return negative ? -res : res;
}

double
mpq_to_double(mpq_t q)
{ return mpz_fdiv(mpq_numref(q), mpq_denref(q));
}


/*
 * Try to compute a "nice" rational from a float, using continued fractions.
 * Stop when the rational converts back into the original float exactly.
 * If the process doesn't converge (due to numeric problems), or produces
 * a result longer than the fast bitwise conversion from the float mantissa,
 * then fall back to the bitwise conversion.
 */

void
mpq_set_double(mpq_t q, double f)	/* float -> nice rational */
{ double fabs = (f < 0.0) ? -f : f;	/* get rid of the sign */
  double x = fabs;
  mpq_t b, c;
  MP_INT *pna = mpq_numref(q);		/* use output q for a directly */
  MP_INT *pda = mpq_denref(q);
  MP_INT *pnb = mpq_numref(b);
  MP_INT *pdb = mpq_denref(b);
  MP_INT *pnc = mpq_numref(c);
  MP_INT *pdc = mpq_denref(c);
  mpz_t big_xi;
  int half_exp;                       /* predict bitwise conversion size */
  double fr = frexp(fabs, &half_exp);
  int bitwise_denominator_size = DBL_MANT_DIG-(half_exp-1);

  (void)fr;
  if ( bitwise_denominator_size < 1 )
    bitwise_denominator_size = 1;

  mpz_set_ui(pna, 1L);                /* a = q = 1/0 */
  mpz_set_ui(pda, 0L);
  mpq_init(b);			      /* b = 0/1 */
  mpq_init(c);			      /* auxiliary */
  mpz_init(big_xi);                   /* auxiliary */

  while ((mpz_fdiv(pna, pda)) != fabs)
  { /* infinite x indicates failure to converge */
    if ( !isfinite(x) )
      goto _bitwise_conversion_;

    double xi = floor(x);
    double xf = x - xi;

      /* compute a = a*xi + b for both numerator and denominator */
    mpq_swap(q, b);
    if ( x < (double)LONG_MAX )
    { unsigned long int_xi = (unsigned long) xi;
      mpz_mul_ui(pnc, pnb, int_xi);
      mpz_mul_ui(pdc, pdb, int_xi);
    } else
    { mpz_set_d(big_xi, xi);
      mpz_mul(pnc, pnb, big_xi);
      mpz_mul(pdc, pdb, big_xi);
    }
    mpz_add(pna, pna, pnc);
    mpz_add(pda, pda, pdc);

    /* if it gets too long, fall back to bitwise conversion */
    if (mpz_sizeinbase(pda, 2) > bitwise_denominator_size)
      goto _bitwise_conversion_;

    x = 1.0/xf;
  }

  if ( f < 0.0 )
    mpq_neg(q, q);
  mpq_canonicalize(q);                /* normally not be necessary */

  goto _cleanup_;

_bitwise_conversion_:
  mpq_set_d(q, f);                    /* bitwise conversion */

_cleanup_:
  mpz_clear(big_xi);
  mpq_clear(c);
  mpq_clear(b);
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

    if ( valueExpression(t, &n) )
    { switch(n.type)
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
      clearNumber(&n);
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

#endif /*O_GMP*/
