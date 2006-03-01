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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#undef LD
#define LD LOCAL_LD

#ifdef O_GMP

static mpz_t MPZ_MIN_TAGGED;		/* Prolog tagged integers */
static mpz_t MPZ_MAX_TAGGED;
static mpz_t MPZ_MIN_INT;		/* Prolog int64_t integers */
static mpz_t MPZ_MAX_INT;


#if 0
		 /*******************************
		 *	MEMMORY MANAGEMENT	*
		 *******************************/

/*See also clearNumber()*/

static void *
mp_alloc(size_t bytes)
{
}


static void *
mp_realloc(void *ptr, size_t oldsize, size_t newsize)
{
}


static void
free(void *ptr, size_t size)
{
}
#endif

#ifdef WIN32
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

int
wordSizeofMPZ(mpz_t mpz)
{ int size = sizeof(mp_limb_t)*abs(mpz->_mp_size);
  
  return (size+sizeof(word)-1)/sizeof(word);
}


static word
globalMPZ(mpz_t mpz)
{ GET_LD

  int size = sizeof(mp_limb_t)*abs(mpz->_mp_size);
  int wsz  = (size+sizeof(word)-1)/sizeof(word);
  Word p   = allocGlobal(wsz+3);
  word r   = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  word m   = mkIndHdr(wsz+1, TAG_INTEGER);

  *p++     = m;
  p[wsz]   = 0L;			/* pad out */
  p[wsz+1] = m;
  *p++     = (word)mpz->_mp_size;
  memcpy(p, mpz->_mp_d, size);
  
  return r;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_integer() fetches the value of a Prolog  term known to be an integer
into a number structure. If the  value  is   a  MPZ  number,  it must be
handled as read-only and it only be used   as  long as no calls are made
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
    int wsize = wsizeofInd(*p);

    p++;
    if ( wsize == sizeof(int64_t)/sizeof(word) )
    { int64_t *ip = (int64_t *)p;

      n->type = V_INTEGER;
      n->value.i = *ip;
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
{ int size = (mpz_sizeinbase(mpz, 2)+7)/8;
  int hdrsize;
  size_t count;

  growBuffer(b, size+4);
  if ( mpz_sgn(mpz) < 0 )
    hdrsize = -size;
  else
    hdrsize = size;
  DEBUG(1, Sdprintf("addMPZToBuffer(): Added %d bytes\n", size));

  *b->top++ = (hdrsize>>24)&0xff;
  *b->top++ = (hdrsize>>16)&0xff;
  *b->top++ = (hdrsize>> 8)&0xff;
  *b->top++ = (hdrsize    )&0xff;

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
  assert(mpz->_mp_d == p);		/* check no (re-)allocation is done */

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
{ DEBUG(2, Sdprintf("Converting " INT64_FORMAT " to MPZ\n", i));

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
    case V_REAL:
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
    case V_REAL:
    { double v = n->value.f;

      n->type = V_MPQ;
      mpq_init(n->value.mpq);
      mpq_set_d(n->value.mpq, v);
      break;
    }
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
clearNumber(Number n)
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
{ mpz_init_set_si(MPZ_MIN_TAGGED, PLMINTAGGEDINT);
  mpz_init_set_si(MPZ_MAX_TAGGED, PLMAXTAGGEDINT);
  mpz_init_set_si64(MPZ_MIN_INT, PLMININT);
  mpz_init_set_si64(MPZ_MAX_INT, PLMAXINT);
}

		 /*******************************
		 *	   NUMBER HANDLING      *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_number() transforms a number into a Prolog  term. Note that this may
allocate on the global stack. Please note   that  this function uses the
most compact representation, which is  essential   to  make unify() work
without any knowledge of the represented data.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
put_mpz(mpz_t mpz)
{ if ( mpz_cmp(mpz, MPZ_MIN_TAGGED) >= 0 &&
       mpz_cmp(mpz, MPZ_MAX_TAGGED) <= 0 )
  { long v = mpz_get_si(mpz);

    return consInt(v);
  } else if ( mpz_cmp(mpz, MPZ_MIN_INT) >= 0 &&
	      mpz_cmp(mpz, MPZ_MAX_INT) <= 0 )
  { GET_LD
    int64_t v;

    mpz_export(&v, NULL, ORDER, sizeof(v), 0, 0, mpz);
    if ( mpz_sgn(mpz) < 0 )
      v = -v;
    
    return globalLong(v PASS_LD);
  } else
  { return globalMPZ(mpz);
  }
}

#endif /*O_GMP*/

word
put_number(Number n)
{ GET_LD

  switch(n->type)
  { case V_INTEGER:
    { word w = consInt(n->value.i);

      if ( valInt(w) == n->value.i )
	return w;

      return globalLong(n->value.i PASS_LD);
    }
#ifdef O_GMP
    case V_MPZ:
      return put_mpz(n->value.mpz);
    case V_MPQ:
    { if ( mpz_cmp_ui(mpq_denref(n->value.mpq), 1L) == 0 )
      { return put_mpz(mpq_numref(n->value.mpq));
      } else
      { Word p = allocGlobal(3);
	
	p[0] = FUNCTOR_rdiv2;
	p[1] = put_mpz(mpq_numref(n->value.mpq));
	p[2] = put_mpz(mpq_denref(n->value.mpq));

	return consPtr(p, TAG_COMPOUND|STG_GLOBAL);
      }
    }
#endif
    case V_REAL:
      return globalReal(n->value.f);
  }

  assert(0);
  return 0L;
}


int
PL_unify_number(term_t t, Number n)
{ GET_LD
  Word p = valTermRef(t);

  deRef(p);

  if ( canBind(*p) )
  { word w = put_number(n);

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

	get_integer(*p, &n2);
	
	return ar_compare(n, &n2, EQ);
      }
      break;
#ifdef O_GMP
    case V_MPQ:
    { term_t q = PL_new_term_ref();
      
      *valTermRef(q) = put_number(n);
      return PL_unify(t, q);
    }
#endif
    case V_REAL:
      if ( isReal(*p) )
	return n->value.f == valReal(*p);
      break;
  }

  fail;
}


void
get_number(word w, Number n ARG_LD)
{ if ( isInteger(w) )
  { get_integer(w, n);
  } else
  { n->type = V_REAL;
    n->value.f = valReal(w);
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
  if ( isReal(*p) )
  { n->value.f = valReal(*p);
    n->type = V_REAL;
    succeed;
  }

  fail;
}


		 /*******************************
		 *	     PROMOTION		*
		 *******************************/

void
promoteToRealNumber(Number n)
{ switch(n->type)
  { case V_INTEGER:
      n->value.f = (double)n->value.i;
      n->type = V_REAL;
      break;
#ifdef O_GMP
    case V_MPZ:
    { double val = mpz_get_d(n->value.mpz);

      clearNumber(n);
      n->value.f = val;
      n->type = V_REAL;
      break;
    }
    case V_MPQ:
    { double val = mpq_get_d(n->value.mpq);

      clearNumber(n);
      n->value.f = val;
      n->type = V_REAL;
      break;
    }
#endif
    case V_REAL:
      break;
  }
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
    case V_REAL:
      promoteToRealNumber(n);
      break;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
same_type_numbers(n1, n2)
    Upgrade both numbers to the `highest' type of both. Number types are
    defined in the enum-type numtype, which is supposed to define a
    total ordering between the number types.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef max
#define max(a,b) ((a) >= (b) ? (a) : (b))
#endif

void
same_type_numbers(Number n1, Number n2)
{ if ( n1->type != n2->type )
  { if ( (int)n1->type > (int)n2->type )
      promoteNumber(n2, n1->type);
    else
      promoteNumber(n1, n2->type);
  }
}


		 /*******************************
		 *	    OPERATIONS		*
		 *******************************/

#define LESS    -1			/* TBD: share with pl-prims.c!! */
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
    case V_REAL:
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
    case V_REAL:
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
    promoteToMPZNumber(&n);
    mpz_set(mpz, n.value.mpz);
    clearNumber(&n);

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
	  { mpq_set_si(mpq, n.value.i, 1L);
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
{ number n;
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
{ number n;
  int rc;

  n.type = V_MPQ;
  mpq_init(n.value.mpq);
  mpq_set(n.value.mpq, mpq);

  rc = PL_unify_number(t, &n);
  clearNumber(&n);

  return rc;
}

#endif /*O_GMP*/
