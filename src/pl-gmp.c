/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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
#include "pl-inline.h"
#undef LD
#define LD LOCAL_LD

#ifdef O_GMP

static mpz_t MPZ_MIN_TAGGED;		/* Prolog tagged integers */
static mpz_t MPZ_MAX_TAGGED;
static mpz_t MPZ_MIN_PLINT;		/* Prolog int64_t integers */
static mpz_t MPZ_MAX_PLINT;
#if SIZEOF_LONG	< SIZEOF_VOIDP
static mpz_t MPZ_MIN_LONG;		/* Prolog int64_t integers */
static mpz_t MPZ_MAX_LONG;
#endif


		 /*******************************
		 *	MEMMORY MANAGEMENT	*
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

#define TOO_BIG_GMP(n) ((n) > 1000 && (n) > (size_t)limitStack(global))

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
{ size_t size = sizeof(mp_limb_t)*abs(mpz->_mp_size);
  size_t wsz  = (size+sizeof(word)-1)/sizeof(word);

  if ( s )
    *s = size;

  return wsz;
}


static int
globalMPZ(Word at, mpz_t mpz, int flags ARG_LD)
{ Word p;
  size_t size;
  size_t wsz = mpz_wsize(mpz, &size);
  word m     = mkIndHdr(wsz+1, TAG_INTEGER);

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
  *p++     = (word)mpz->_mp_size;
  memcpy(p, mpz->_mp_d, size);

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

      n->value.mpz->_mp_size  = (int)*p++;
      n->value.mpz->_mp_alloc = 0;
      n->value.mpz->_mp_d     = (mp_limb_t*) p;
    }
  }
}


		 /*******************************
		 *	  IMPORT/EXPORT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addMPZToBuffer(Buffer b, mpz_t mpz)
	Add mpz in a machine independent representation to the given buffer.
	The data is stored in limps of 1 byte, preceeded by the byte-count
	as 4-byte big-endian number;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
addMPZToBuffer(Buffer b, mpz_t mpz)
{ size_t size = (mpz_sizeinbase(mpz, 2)+7)/8;
  long hdrsize;
  size_t count;

  if ( !growBuffer(b, size+4) )
    outOfCore();
  if ( mpz_sgn(mpz) < 0 )
    hdrsize = -(long)size;
  else
    hdrsize = (long)size;
  DEBUG(1, Sdprintf("addMPZToBuffer(): Added %d bytes\n", size));

  *b->top++ = (char)((hdrsize>>24)&0xff);
  *b->top++ = (char)((hdrsize>>16)&0xff);
  *b->top++ = (char)((hdrsize>> 8)&0xff);
  *b->top++ = (char)((hdrsize    )&0xff);

  mpz_export(b->top, &count, 1, 1, 1, 0, mpz);
  assert(count == size);
  b->top = b->top + size;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
loadMPZFromCharp() loads an MPZ number directly back to the global stack
from a char *  as  filled   by  addMPZToBuffer().  Memory  allocation is
avoided by creating a dummy mpz that looks big enough to mpz_import().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SHIFTSIGN32 ((sizeof(int)-4)*8)

char *
loadMPZFromCharp(const char *data, Word r, Word *store)
{ GET_LD
  int size = 0;
  int limpsize;
  int wsize;
  int neg;
  mpz_t mpz;
  Word p;
  word m;

  size |= (data[0]&0xff)<<24;
  size |= (data[1]&0xff)<<16;
  size |= (data[2]&0xff)<<8;
  size |= (data[3]&0xff);
  size = (size << SHIFTSIGN32)>>SHIFTSIGN32;	/* sign extend */
  data += 4;

  DEBUG(1, Sdprintf("loadMPZFromCharp(): size = %d bytes\n", size));

  if ( size < 0 )
  { neg = TRUE;
    size = -size;
  } else
    neg = FALSE;

  limpsize = (size+sizeof(mp_limb_t)-1)/sizeof(mp_limb_t);
  wsize = (limpsize*sizeof(mp_limb_t)+sizeof(word)-1)/sizeof(word);
  p = *store;
  *store += (wsize+3);
  *r = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  m = mkIndHdr(wsize+1, TAG_INTEGER);
  *p++ = m;
  p[wsize] = 0L;			/* pad out */
  p[wsize+1] = m;
  *p++ = (neg ? -limpsize : limpsize);
  mpz->_mp_size  = limpsize;
  mpz->_mp_alloc = limpsize;
  mpz->_mp_d     = (mp_limb_t*)p;

  mpz_import(mpz, size, 1, 1, 1, 0, data);
  assert((Word)mpz->_mp_d == p);		/* check no (re-)allocation is done */

  return (char *)data+size;
}


char *
skipMPZOnCharp(const char *data)
{ int size = 0;

  size |= (data[0]&0xff)<<24;
  size |= (data[1]&0xff)<<16;
  size |= (data[2]&0xff)<<8;
  size |= (data[3]&0xff);
  size = (size << SHIFTSIGN32)>>SHIFTSIGN32;	/* sign extend */
  data += 4;

  if ( size < 0 )
    size = -size;

  return (char *)data + size;
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

static void
mpz_init_set_si64(mpz_t mpz, int64_t i)
{
#if SIZEOF_LONG == 8
  mpz_init_set_si(mpz, (long)i);
#else
  DEBUG(2, Sdprintf("Converting " INT64_FORMAT " to MPZ\n", i));

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


void
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
}


void
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

      n->type = V_MPQ;
      mpq_init(n->value.mpq);
      mpq_set_d(n->value.mpq, v);
      break;
    }
  }
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

void
initGMP()
{ if ( !GD->gmp.initialised )
  { GD->gmp.initialised = TRUE;

    mpz_init_set_si64(MPZ_MIN_TAGGED, PLMINTAGGEDINT);
    mpz_init_set_si64(MPZ_MAX_TAGGED, PLMAXTAGGEDINT);
    mpz_init_set_si64(MPZ_MIN_PLINT, PLMININT);
    mpz_init_set_si64(MPZ_MAX_PLINT, PLMAXINT);
#if SIZEOF_LONG < SIZEOF_VOIDP
    mpz_init_set_si64(MPZ_MIN_LONG, LONG_MIN);
    mpz_init_set_si64(MPZ_MAX_LONG, LONG_MAX);
#endif
#ifdef O_MY_GMP_ALLOC
    if ( !GD->gmp.keep_alloc_functions )
      mp_set_memory_functions(mp_alloc, mp_realloc, mp_free);
#endif
  }
}


void
cleanupGMP()
{ if ( GD->gmp.initialised )
  { GD->gmp.initialised = FALSE;

#ifdef O_MY_GMP_ALLOC
    if ( !GD->gmp.keep_alloc_functions )
      mp_set_memory_functions(NULL, NULL, NULL);
#endif
    mpz_clear(MPZ_MIN_TAGGED);
    mpz_clear(MPZ_MAX_TAGGED);
    mpz_clear(MPZ_MIN_PLINT);
    mpz_clear(MPZ_MAX_PLINT);
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
  { int64_t v;

    mpz_export(&v, NULL, ORDER, sizeof(v), 0, 0, mpz);
    DEBUG(2,
	  { char buf[256];
	    Sdprintf("Convert %s --> %I64d\n",
		     mpz_get_str(buf, 10, mpz), v);
	  });

    if ( mpz_sgn(mpz) < 0 )
      v = -v;

    *i = v;
    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_number() transforms a number into a Prolog  term. Note that this may
allocate on the global stack. Please note   that  this function uses the
most compact representation, which is  essential   to  make unify() work
without any knowledge of the represented data.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_mpz(Word at, mpz_t mpz, int flags ARG_LD)
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
  { return put_int64(at, v, flags PASS_LD);
  } else
  { return globalMPZ(at, mpz, flags PASS_LD);
  }
}

#endif /*O_GMP*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_number()   translates   a   number   structure   into   its   Prolog
representation and ensures there  is  enough   space  for  a  subsequent
bindConst() call. Note that `at' must point   to  an address that is not
affected by GC/shift.  The intented scenario is:

  { word c;

    if ( (rc=put_number(&c, n, ALLOW_GC PASS_LD)) == TRUE )
      bindConst(<somewhere>, c);
    ...
  }
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
put_number(Word at, Number n, int flags ARG_LD)
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

      return put_int64(at, n->value.i, flags PASS_LD);
    }
#ifdef O_GMP
    case V_MPZ:
      return put_mpz(at, n->value.mpz, flags PASS_LD);
    case V_MPQ:
    { if ( mpz_cmp_ui(mpq_denref(n->value.mpq), 1L) == 0 )
      { return put_mpz(at, mpq_numref(n->value.mpq), flags PASS_LD);
      } else
      { word num, den;
	Word p;
	size_t req = ( mpz_wsize(mpq_numref(n->value.mpq), NULL)+3 +
		       mpz_wsize(mpq_denref(n->value.mpq), NULL)+3 + 3 );

	if ( !hasGlobalSpace(req) )
	{ int rc = ensureGlobalSpace(req, flags);

	  if ( rc != TRUE )
	    return rc;
	}

	if ( !(put_mpz(&num, mpq_numref(n->value.mpq), 0 PASS_LD)) ||
	     !(put_mpz(&den, mpq_denref(n->value.mpq), 0 PASS_LD)) )
	  fail;

	p = gTop;
	gTop += 3;
	assert(gTop <= gMax);


	p[0] = FUNCTOR_rdiv2;
	p[1] = num;
	p[2] = den;

	*at = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
	return TRUE;
      }
    }
#endif
    case V_FLOAT:
      return put_double(at, n->value.f, flags PASS_LD);
  }

  assert(0);
  return FALSE;
}


int
PL_unify_number__LD(term_t t, Number n ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);

  if ( canBind(*p) )
  { word w;
    int rc;

    if ( (rc=put_number(&w, n, ALLOW_GC PASS_LD)) != TRUE )
      return raiseStackOverflow(rc);

    p = valTermRef(t);			/* put_number can shift the stacks */
    deRef(p);

    bindConst(p, w);
    succeed;
  }

  switch(n->type)
  { case V_INTEGER:
#ifdef O_GMP
    case V_MPZ:
#endif
      if ( isInteger(*p) )
      { number n2;
	int rc;

	get_integer(*p, &n2);
	same_type_numbers(n, &n2);
	rc = ar_compare_eq(n, &n2);
	clearNumber(&n2);

	return rc;
      }
      break;
#ifdef O_GMP
    case V_MPQ:
    { word w;
      term_t tmp;
      int rc;

      if ( !(tmp=PL_new_term_ref()) )
	return FALSE;
      if ( (rc=put_number(&w, n, ALLOW_GC PASS_LD)) != TRUE )
	return raiseStackOverflow(rc);

      *valTermRef(tmp) = w;
      return PL_unify(t, tmp);
    }
#endif
    case V_FLOAT:
      if ( isFloat(*p) )
	return n->value.f == valFloat(*p);
      break;
  }

  fail;
}


void
get_number(word w, Number n ARG_LD)
{ if ( isInteger(w) )
  { get_integer(w, n);
  } else
  { n->type = V_FLOAT;
    n->value.f = valFloat(w);
  }
}


int
PL_get_number(term_t t, Number n)
{ GET_LD
  Word p = valTermRef(t);

  deRef(p);
  if ( isInteger(*p) )
  { get_integer(*p, n);
    succeed;
  }
  if ( isFloat(*p) )
  { n->value.f = valFloat(*p);
    n->type = V_FLOAT;
    succeed;
  }

  fail;
}


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
#ifdef O_GMP
    case V_MPZ:
    { double val = mpz_get_d(n->value.mpz);

      if ( !check_float(val) )
	return FALSE;

      clearNumber(n);
      n->value.f = val;
      n->type = V_FLOAT;
      break;
    }
    case V_MPQ:
    { double val = mpq_get_d(n->value.mpq);

      if ( !check_float(val) )
	return FALSE;

      clearNumber(n);
      n->value.f = val;
      n->type = V_FLOAT;
      break;
    }
#endif
    case V_FLOAT:
      break;
  }

  return TRUE;
}


void
promoteNumber(Number n, numtype t)
{ switch(t)
  { case V_INTEGER:
      break;
#ifdef O_GMP
    case V_MPZ:
      promoteToMPZNumber(n);
      break;
    case V_MPQ:
      promoteToMPQNumber(n);
      break;
#endif
    case V_FLOAT:
      promoteToFloatNumber(n);
      break;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
same_type_numbers(n1, n2)
    Upgrade both numbers to the `highest' type of both. Number types are
    defined in the enum-type numtype, which is supposed to define a
    total ordering between the number types.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
make_same_type_numbers(Number n1, Number n2)
{ if ( (int)n1->type > (int)n2->type )
    promoteNumber(n2, n1->type);
  else
    promoteNumber(n1, n2->type);
}


		 /*******************************
		 *	    OPERATIONS		*
		 *******************************/

					/* TBD: share with pl-prims.c!! */
#define LESS    -1			/* n1 < n2 */
#define EQUAL    0
#define GREATER  1

int
cmpNumbers(Number n1, Number n2)
{ same_type_numbers(n1, n2);

  switch(n1->type)
  { case V_INTEGER:
      return n1->value.i  < n2->value.i ? LESS :
	     n1->value.i == n2->value.i ? EQUAL : GREATER;
#ifdef O_GMP
    case V_MPZ:
    { int rc = mpz_cmp(n1->value.mpz, n2->value.mpz);

      return rc < 0 ? LESS : rc == 0 ? EQUAL : GREATER;
    }
    case V_MPQ:
    { int rc = mpq_cmp(n1->value.mpq, n2->value.mpq);

      return rc < 0 ? LESS : rc == 0 ? EQUAL : GREATER;
    }
#endif
    case V_FLOAT:
      return n1->value.f  < n2->value.f ? LESS :
	     n1->value.f == n2->value.f ? EQUAL : GREATER;
  }

  assert(0);
  return EQUAL;
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


		 /*******************************
		 *	 PUBLIC INTERFACE	*
		 *******************************/

#ifdef O_GMP

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

    if ( valueExpression(t, &n PASS_LD) )
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
