/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, University of Amsterdam
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

#define _CRT_SECURE_NO_WARNINGS 1	/* Avoid strcpy() warning */
#include <sys/types.h>			/* get ssize_t */
#include <string.h>
#include <stdio.h>
#include "bf_gmp.h"
#include <stdlib.h>
#include "cutils.h"
//#include "../os/SWI-Stream.h"		/* For Sdprintf() debugging */

#define STEIN 1

void
bf_print_i(const char *msg, const bf_t *i)
{ printf("%s=%s\n",
	 msg,
	 bf_ftoa(NULL, i, 10, 0, BF_RNDZ|BF_FTOA_FORMAT_FRAC));
}


/* Note that the caller must make sure `STR` is large enough!
   Dubious GMP API ...
*/

char *
mpz_get_str(char *STR, int BASE, const mpz_t OP)
{ const bf_t *op = OP;
  bf_t copy;

  if ( !op->ctx )
  { copy = OP[0];
    copy.ctx = &alloc_wrapper.bf_context;
    op = &copy;
  }

  char *s = bf_ftoa(NULL, op, BASE, 0, BF_RNDZ|BF_FTOA_FORMAT_FRAC);
  strcpy(STR, s);
  bf_free(op->ctx, s);

  return STR;
}


/* multiply 'r' by 2^e */
// shift in place (copied from libbf.c with rounding removed)
static inline void
mul_2exp(bf_t *r, slimb_t e)
{
    slimb_t e_max;
    if (r->len != 0)
    { e_max = ((limb_t)1 << BF_EXT_EXP_BITS_MAX) - 1;
      e = bf_max(e, -e_max);
      e = bf_min(e, e_max);
      r->expn += e;
    }
    return;
}

#if STEIN

// adapted from
// https://stackoverflow.com/questions/63604914/how-can-i-speed-up-the-binary-gcd-algorithm-using-builtin-ctz
static uint64_t
i64_gcd(uint64_t u, uint64_t v) {
    uint64_t t = u | v;

    if(u == 0 || v == 0) return t; /* return (v) or (u), resp. */

    int g = (int)__builtin_ctzll(t);

    while(u != 0) {
        u >>= __builtin_ctzll(u);
        v >>= __builtin_ctzll(v);

        if(u >= v)
            u = (u - v) / 2;
        else
            v = (v - u) / 2;
    }

    return (v << g); /* scale by common factor. */
}

#define INT64BITSIZE		(8 * sizeof(int64_t))  // from pl-incl.h

void
mpz_gcd(mpz_t r, const mpz_t n1, const mpz_t n2)
{ mpz_t a, b;
  mp_bitcnt_t als1, bls1, k;
  int r_sgn;

  if ( mpz_sgn(n1) == 0 )
  { mpz_abs(r, n2);
    return;
  }
  if ( mpz_sgn(n2) == 0 )
  { mpz_abs(r, n1);
    return;
  }

  mpz_init(a);
  mpz_init(b);
  // gcd is always positive
  mpz_abs(a, n1);
  mpz_abs(b, n2);

  while ( llabs(a->expn - b->expn) > 5 )  // if large difference between a and b
  { mpz_tdiv_r(r, a, b);  // reduce somewhat with Euclidean
    if ( mpz_sgn(r) == 0 )  // if remainder is 0, answer is b
    { mpz_swap(b, r);
      mpz_clear(a);
      mpz_clear(b);
      return;
    }
    mpz_swap(a, b);
    mpz_swap(b, r);
  }

  // reduce a and b to odd
  als1 = mpz_scan1(a, 0);
  if ( als1 > 0 ) mul_2exp(a, UNEG(als1));
  bls1 = mpz_scan1(b, 0);
  if ( bls1 > 0 ) mul_2exp(b, UNEG(bls1));
  k = (als1 > bls1) ? bls1 : als1;  // k = min(als1,bls1)

  while (1)     // now use Stein
  { // a and b always odd at start of loop
    if ((a->expn < INT64BITSIZE) && (b->expn <INT64BITSIZE))
    { // both fit in 64 bit integers; get int64 values and use int64 gcd
      mpz_set_ui64(r, i64_gcd(mpz_get_si64(a), mpz_get_si64(b)));
      break;             // we're done, exit while loop
    }
    mpz_sub(r,a,b);      // a-b -> r is now even, b still odd
    r_sgn = mpz_sgn(r);
    if ( r_sgn == 0 )
    { mpz_swap(r, a);    // a==b, a -> r
      break;             // we're done, exit while loop
    } else if ( r_sgn == -1 )  // test a<b
    { mpz_swap(b, a);    // a -> b
      mpz_abs(r, r);     // |a-b| -> r
    }
    mpz_swap(a, r);      // |a-b| -> a
    mul_2exp(a, -(slimb_t)mpz_scan1(a, 0));  // make a odd again
  }

  mpz_mul_2exp(r, r, k); // r*2^d -> r (final answer)
  mpz_clear(a);
  mpz_clear(b);
}


#else

void
mpz_gcd(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_t a, b, t;

  if ( bf_is_zero(n1) )
  { bf_set(r, n2);
    return;
  }
  if ( bf_is_zero(n2) )
  { bf_set(r, n1);
    return;
  }

  bf_init(&alloc_wrapper.bf_context, &a);
  bf_init(&alloc_wrapper.bf_context, &b);
  bf_init(&alloc_wrapper.bf_context, &t);

  bf_set(r, n1);
  bf_set(&b, n2);

  while( !bf_is_zero(&b) )
  { bf_set(&t, &b);
    bf_rem(&a, r, &b, BF_PREC_INF, 0, BF_RNDN);
    bf_set(&b, &a);
    bf_set(r, &t);
  }

  bf_delete(&a);
  bf_delete(&b);
  bf_delete(&t);
}

#endif

void
mpz_lcm(mpz_t r, const mpz_t n1, const mpz_t n2)
{ mpz_t gcd;
  mpz_t prod;

  mpz_init(gcd);
  mpz_init(prod);
  mpz_mul(prod, n1, n2);
  mpz_gcd(gcd, n1, n2);
  mpz_divexact(r, prod, gcd);
  mpz_clear(prod);
  mpz_clear(gcd);
}


void
mpz_pow_ui(mpz_t r, const mpz_t n, unsigned long x)
{ mpz_t N;

  mpz_init_set(N, n);
  mpz_set_ui(r, 1);

  while ( x )
  { if ( x & 0x1 )
    { mpz_mul(r, r, N);
      x -= 1;
    }
    x /= 2;
    mpz_mul(N, N, N);
  }

  mpz_clear(N);
}

void
mpz_ui_pow_ui(mpz_t r, unsigned long n, unsigned long x)
{ uint64_t N = n;
  uint64_t R = 1;
  mpz_t Nz;

  while ( x )
  { uint64_t N1, R1=R;
    unsigned long x1 = x;

    if ( x & 0x1 )
    { if ( __builtin_mul_overflow(R,N,&R1) )
	goto overflow;
      x1 -= 1;
    }
    x1 /= 2;
    if ( __builtin_mul_overflow(N,N,&N1) )
      goto overflow;
    x = x1;
    R = R1;
    N = N1;
  }
  mpz_set_ui64(r, R);

 overflow:
  mpz_init_set_ui64(Nz, N);
  mpz_set_ui64(r, R);

  while ( x )
  { if ( x & 0x1 )
    { mpz_mul(r, r, Nz);
      x -= 1;
    }
    x /= 2;
    mpz_mul(Nz, Nz, Nz);
  }

  mpz_clear(Nz);
}


/* r is base**exp % mod

   This is a simple implementation that merely keeps the operant sizes small
   by performing the modulo during the computation.
*/

void
mpz_powm(mpz_t r, const mpz_t base, const mpz_t exp, const mpz_t mod)
{ mpz_t N;
  mpz_t x;

  mpz_init_set(N, base);
  mpz_init_set(x, exp);
  mpz_set_ui(r, 1);

  while ( !bf_is_zero(x) )
  { if ( mpz_scan1(x, 0) == 0 )	/* can be more efficient */
    { mpz_mul(r, r, N);
      mpz_fdiv_r(r, r, mod);
      mpz_sub_ui(x, x, 1);
    }
    mpz_fdiv_q_ui(x, x, 2);
    mpz_mul(N, N, N);
    mpz_fdiv_r(N, N, mod);
  }

  mpz_clear(N);
  mpz_clear(x);
}


// see https://stackoverflow.com/questions/72659156/convert-double-to-integer-mantissa-and-exponents
void
mpq_set_d(mpq_t r, double f)
{ double m;
  int exp;
  int64_t man;

  m = frexp(f, &exp);
  man = (int64_t)scalbn(m, 53);
  exp -= 53;
  bf_set_si(mpq_numref(r), man);
  bf_set_si(mpq_denref(r), 1);
  if ( exp > 0 )
    mul_2exp(mpq_numref(r), exp);
  else
    mul_2exp(mpq_denref(r), -exp);

  mpq_canonicalize(r);
}


void
mpq_canonicalize(mpq_t q)
{ mpz_t gcd;
  mpz_t tmp;

  mpz_init(gcd);
  mpz_init(tmp);
  mpz_gcd(gcd, mpq_numref(q), mpq_denref(q));
  mpz_divexact(tmp, mpq_numref(q), gcd);
  mpz_set(mpq_numref(q), tmp);
  mpz_divexact(tmp, mpq_denref(q), gcd);
  mpz_set(mpq_denref(q), tmp);
  if ( q[1].sign )
  { bf_neg(&q[0]);
    q[1].sign = 0;
  }
  mpz_clear(gcd);
  mpz_clear(tmp);
}


int
mpq_cmp(const mpq_t q1, const mpq_t q2)
{ mpz_t numa, numb;
  int rc;

  mpz_init(numa);
  mpz_init(numb);
  mpz_mul(numa, mpq_cnumref(q1), mpq_cdenref(q2));
  mpz_mul(numb, mpq_cnumref(q2), mpq_cdenref(q1));

  rc = mpz_cmp(numa, numb);

  mpz_clear(numa);
  mpz_clear(numb);

  return rc;
}


int
mpq_cmp_ui(const mpq_t q1, unsigned long n, unsigned long d)
{ mpq_t q2;
  int rc;

  mpq_init(q2);
  mpq_set_ui(q2, n, d);
  rc = mpq_cmp(q1, q2);
  mpq_clear(q2);

  return rc;
}


int
mpq_cmp_si(const mpq_t q1, long n, unsigned long d)
{ mpq_t q2;
  int rc;

  mpq_init(q2);
  mpq_set_si(q2, n, d);
  rc = mpq_cmp(q1, q2);
  mpq_clear(q2);

  return rc;
}


int
mpq_cmp_z(const mpq_t q1, const mpz_t z2)
{ mpz_t num;
  int rc;
  mpz_init(num);
  mpz_mul(num, mpq_cdenref(q1), z2);
  rc = mpz_cmp(mpq_cnumref(q1), num);
  mpz_clear(num);

  return rc;
}


static void
mpq_addsub(mpq_t r, const mpq_t q1, const mpq_t q2, int add)
{ mpz_t numa, numb;

  mpz_init(numa);
  mpz_init(numb);
  mpz_mul(mpq_denref(r), mpq_cdenref(q1), mpq_cdenref(q2));
  mpz_mul(numa, mpq_cnumref(q1), mpq_cdenref(q2));
  mpz_mul(numb, mpq_cnumref(q2), mpq_cdenref(q1));
  if ( add )
    mpz_add(mpq_numref(r), numa, numb);
  else
    mpz_sub(mpq_numref(r), numa, numb);
  mpz_clear(numa);
  mpz_clear(numb);
  mpq_canonicalize(r);
}

void
mpq_add(mpq_t r, const mpq_t q1, const mpq_t q2)
{ mpq_addsub(r, q1, q2, 1);
}

void
mpq_sub(mpq_t r, const mpq_t q1, const mpq_t q2)
{ mpq_addsub(r, q1, q2, 0);
}

void
mpq_mul(mpq_t r, const mpq_t q1, const mpq_t q2)
{ mpz_mul(mpq_numref(r), mpq_cnumref(q1), mpq_cnumref(q2));
  mpz_mul(mpq_denref(r), mpq_cdenref(q1), mpq_cdenref(q2));
  mpq_canonicalize(r);
}

void
mpq_div(mpq_t r, const mpq_t q1, const mpq_t q2)
{ mpz_mul(mpq_numref(r), mpq_cnumref(q1), mpq_cdenref(q2));
  mpz_mul(mpq_denref(r), mpq_cdenref(q1), mpq_cnumref(q2));
  mpq_canonicalize(r);
}


void
gmp_randseed(gmp_randstate_t state, const mpz_t seed)
{ mt_randseed(state, (const uint32_t*)seed->tab,
	      (seed->len*sizeof(limb_t))/sizeof(uint32_t));
}

void
gmp_randseed_ui(gmp_randstate_t state, unsigned long seed)
{ mt_randseed(state, (const uint32_t*)&seed, sizeof(seed)/sizeof(uint32_t));
}


static int
bf_is_exp_2(const bf_t *n)
{ return ( n->len == 1 && __builtin_popcountll(n->tab[0]) == 1 );
}


static double
random_double(gmp_randstate_t state)
{ uint64_t l = mt_rand_u32(state);
  uint64_t h = mt_rand_u32(state);
  uint64_t i = (l<<32) | h;

  return (double)i/(double)0xffffffffffffffff;
}

/* TBD: Currently ignores `bits`.  Guess we can resize, fill the `tab`
   and set ->expn to 1?
 */

void
mpf_urandomb(mpf_t r, gmp_randstate_t state, mp_bitcnt_t bits)
{ double rnd = random_double(state);

  bf_set_float64(r, rnd);
}


static void
mpz_urandom_2exp(mpz_t r, gmp_randstate_t state, const mp_bitcnt_t szbits)
{ size_t szbytes = (szbits+7)/8;
  unsigned char buf[256];
  unsigned char *data = szbytes <= sizeof(buf) ? buf : malloc(szbytes);
  int byte = 0;
  uint32_t rnd;

  for(int i=0; i<szbytes; i++)
  { if ( byte == 0 )
    { rnd = mt_rand_u32(state);
      byte = 3;
    }
    data[i] = (rnd>>(byte*8))&0xff;
    byte--;
  }

  int bits_in_high_byte = szbits % 8;
  if ( bits_in_high_byte != 0 )
  { int mask = (1<<(bits_in_high_byte-1))-1;
    data[0] &= mask;
  }

  mpz_import(r, szbytes, 1, 1, 1, 0, data);

  if ( data != buf )
    free(data);
}

/* See https://www.pcg-random.org/posts/bounded-rands.html */

void
mpz_urandomm(mpz_t r, gmp_randstate_t state, const mpz_t N)
{ if ( bf_is_zero(N) )
  { bf_set_si(r, 0);
  } else if ( bf_is_exp_2(N) )
  { mpz_urandom_2exp(r, state, (mp_bitcnt_t)mpz_sizeinbase(N,2));
  } else
  { do
    { mpz_urandom_2exp(r, state, (mp_bitcnt_t)mpz_sizeinbase(N,2)+1);
    } while( mpz_cmp(r, N) >= 0 );
  }
}

#ifndef offsetof
#define offsetof(structure, field) ((int) &(((structure *)NULL)->field))
#endif

int
bf_set_randstate(gmp_randstate_t state, const mpz_t n)
{ size_t size = offsetof(MTState, MT_TEMPERED);
  size_t bytes = (mpz_sizeinbase(n, 2)+7)/8;
  size_t count;

  if ( bytes != size )
    return -1;

  mpz_export((unsigned char*)state, &count, 1, 1, 1, 0, n);
  assert(count == bytes);

  mt_temper(state);
  return 0;
}

void
bf_get_randstate(mpz_t n, const gmp_randstate_t state)
{ size_t size = offsetof(MTState, MT_TEMPERED);

  mpz_import(n, size, 1, 1, 1, 0, state);
}


/* When `n` is negative we use "Working from LSB towards MSB"  from
   https://en.wikipedia.org/wiki/Two%27s_complement#Converting_to_two's_complement_representation
*/

int
mpz_tstbit(const mpz_t n, mp_bitcnt_t i)
{ int set;

  if ( (slimb_t)i >= n->expn )
  { set = 0;
  } else
  { limb_t boffset = n->expn-1 - i; /* offset from msb */
    limb_t loffset = boffset/(sizeof(limb_t)*8);
    if ( loffset >= n->len )
      set = 0;

    /* bit is 0..63, counting from lsb */
    int bit = sizeof(limb_t)*8 - 1 - boffset % (sizeof(limb_t)*8);
    set = !!(n->tab[n->len-1-loffset] & ((limb_t)1<<bit));
  }

  if ( n->sign )
  { mp_bitcnt_t lsb = mpz_scan1(n, 0);
    if ( i > lsb+1 )
      set = !set;
  }

  return set;
}


mp_bitcnt_t			/* is lsb, counting bits from 0 */
mpz_scan1(const mpz_t n, mp_bitcnt_t start)
{ if ( bf_is_zero(n) )
  { return ~(mp_bitcnt_t)0;
  } else
  { mp_bitcnt_t msb  = n->expn-1;
    mp_bitcnt_t from = msb - n->len*sizeof(limb_t)*8;
    const limb_t *lp = &n->tab[0];

    if ( start > msb )
      return ~(mp_bitcnt_t)0;
    if ( start < from )
      start = from;

    for(;;)
    { if ( *lp )
	return start + __builtin_ffsll(*lp);
      start += sizeof(limb_t)*8;
      lp++;
    }
  }
}


/* Binary complement.   This would  be easy, but  we must  recall that
   least significant 0-limbs are not  in tab.  These should become all
   1's.  So, we need to extend the  tab, shift the bits we have to the
   most significant place, fill the new least significant segment with
   1s   and    complement   the    old   libs.    Finally    we   need
   bf_normalize_and_round() to  adjust for 0s  we may now have  at the
   most significant end.
*/

void
mpz_com(mpz_t r, const mpz_t n)
{ if ( r != n )
    mpz_set(r, n);

  size_t alllimbs = (r->expn+8*sizeof(limb_t)-1)/(8*sizeof(limb_t));
  size_t len0 = r->len;
  if ( len0 < alllimbs )
  { bf_resize(r, (limb_t)alllimbs);
    memmove(&r->tab[alllimbs-len0], &r->tab[0], len0*sizeof(limb_t));
    for(size_t i=0; i<alllimbs-len0; i++)
      r->tab[i] = ~(limb_t)0;
  }

  for(size_t i = alllimbs-len0; i<alllimbs; i++)
    r->tab[i] ^= ~(limb_t)0;

  bf_normalize_and_round(r, BF_PREC_INF, BF_RNDN);
}


/* OP is rop**n + rem */

void
mpz_rootrem(mpz_t rop, mpz_t rem, const mpz_t OP, unsigned long int n)
{ mpz_t cn, nxt, x, tmp;
  const MP_INT *op;
  int op_sgn;

  if (bf_is_zero(OP))
  { mpz_set(rop, OP);               // nth root of zero is zero
    mpz_set(rem, OP);
    return;
  }

  if ( mpz_sizeinbase(OP, 2) < n )  // if n > bit size, answer is +/- 1
  { op_sgn = mpz_sgn(OP);
    mpz_add_si(rem, OP, -op_sgn);
    mpz_set_si(rop, op_sgn);
    return;
  }

  if ( rop == OP )
  { mpz_init_set(tmp, OP);
    op = &tmp[0];
  } else
    op = &OP[0];

  mpz_init_set_ui(cn, n);     // const n
  mpz_init_set(nxt, op);
  mpz_init(x);

  do  // using rop and rem as temporaries
  { mpz_set(x, nxt);                   // x = nxt
    mpz_pow_ui(rop, x, n-1);           // rop = pow(x, n-1)
    mpz_tdiv_qr(rop, rem, op, rop);    // rop = op // rop
    mpz_mul_ui(rem, x, n-1);           // rem = x * (n-1)
    mpz_add(rop, rop, rem);            // rop = rop + rem
    mpz_tdiv_qr(nxt, rem, rop, cn);    // nxt = rop // n
  } while (mpz_cmp(x, nxt) == mpz_sgn(x));

  mpz_set(rop, x);

  mpz_pow_ui(x, rop, n);               // x = pow(rop, n)
  mpz_sub(rem, op, x);                 // rem = op - x

  mpz_clear(cn);
  mpz_clear(nxt);
  mpz_clear(x);
  if ( rop == OP )
    mpz_clear(tmp);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the  exponent and len given  a bigint represented as  a series of
bytes.  Note  that LibBF  does not include  0-limbs.  This  implies we
must compute the number  of bits between the msb and  lsb and round it
to limbs.  Note that in normalized form the high bit of the first limb
is always 1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
bf_import_dimension(bf_t *r, const unsigned char *data, size_t len)
{ size_t bits;
  const unsigned char *d;

  while( len > 0 && *data == 0 )
  { len--;
    data++;
  }
  if ( len == 0 )
  { r->len = 0;
    r->expn = BF_EXP_ZERO;
    return;
  }

  unsigned int i = data[0];
  r->expn = (slimb_t)(len*8 - (__builtin_clz(i) - (sizeof(i)*8 - 8)));
  bits = r->expn;
  for(d=&data[len-1]; ;d--)
  { if ( *d )
    { bits -= __builtin_ffs(*d)-1;
      break;
    }
    bits -= 8;
  }

  r->len = (limb_t)((bits+sizeof(limb_t)*8-1)/(sizeof(limb_t)*8));
}

void
mpz_import(mpz_t ROP, size_t COUNT, int ORDER,
	   size_t SIZE, int ENDIAN, size_t NAILS, const void *OP)
{ if ( SIZE == 1 )
  { bf_t bf;
    size_t byte = sizeof(limb_t)-1;
    ssize_t bytes = COUNT;
    limb_t *lt;
    limb_t l = 0;
    const unsigned char *data = OP;

    bf_import_dimension(&bf, OP, COUNT);

    assert(NAILS==0 && ORDER==1);

    bf_resize(ROP, bf.len);
    ROP->sign = 0;
    ROP->expn = bf.expn;
    if ( bf.expn == BF_EXP_ZERO )
      return;

    int shift = (int)(COUNT*8-bf.expn);
    limb_t mask = ((limb_t)1<<shift)-1;

    lt = &ROP->tab[bf.len-1];
    while(bytes-->0)
    { l |= (limb_t)*data++ << byte*8;
      if ( byte == 0 )
      { byte =  sizeof(limb_t)-1;
	if ( shift )
	{ l <<= shift;
	  if ( bytes > 0 )
	    l |= (data[0] >> (8-shift))&mask;
	}
	*lt = l;
	if ( lt == ROP->tab )
	  return;			/* rest is all zero */
	lt--;
	l = 0;
      } else
	byte--;
    }
    if ( shift )
      l <<= shift;
    *lt = l;
    assert(lt == ROP->tab);
  } else
  { bf_not_implemented("mpz_import for SIZE != 1");
  }
}

void *
mpz_export(void *ROP, size_t *COUNTP, int ORDER,
	   size_t SIZE, int ENDIAN, size_t NAILS, const mpz_t OP)
{ if ( SIZE == 1 )			/* export per byte */
  { if ( OP->expn == 0 )
    { *COUNTP = 0;
      return ROP;
    } else
    { size_t bytes = (OP->expn+7)/8;
      limb_t *lt = &OP->tab[OP->len-1];
      int byte = sizeof(limb_t)-1;
      unsigned char *out = ROP;
      limb_t l = *lt;
      int shift = (int)(bytes*8-OP->expn);
      limb_t mask = (1<<shift)-1;
      limb_t low = l&mask;
      limb_t high = low<<(sizeof(limb_t)*8-shift);
      l >>= shift;

      *COUNTP = bytes;
      while(bytes-->0)
      { *out++ = (l>>(8*byte))&0xff;
	if ( byte == 0 )
	{ byte = sizeof(limb_t)-1;
	  if ( lt == OP->tab )
	  { l = high;
	    high = 0;
	  } else
	  { l = *--lt;
	    low = l&mask;
	    l>>=shift;
	    l |= high;
	    high = low<<(sizeof(limb_t)*8-shift);
	  }
	} else
	  byte--;
      }
      assert(out == (unsigned char*)ROP+*COUNTP);

      return ROP;
    }
  }

  bf_not_implemented("mpz_export for SIZE != 1");
  return NULL;
}


int
gmp_snprintf(char *BUF, size_t SIZE, const char *FMT, ...)
{ bf_not_implemented("gmp_snprintf");
  return 0;
}


#if 0
static inline void
print_bytes(const unsigned char *data, size_t len)
{ for(size_t i=0; i<len; i++)
  { int byte = data[i];
    int upper = (byte>>4)&0xf;
    int c1 = upper < 10 ? '0'+upper : 'a'+upper-10;
    int lower = byte&0xf;
    int c2 = lower < 10 ? '0'+lower : 'a'+lower-10;
    fprintf(stderr, "%c%c", c1, c2);
  }
  fprintf(stderr, "\n");
}
#endif
