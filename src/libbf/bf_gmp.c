#include <sys/types.h>			/* get ssize_t */
#include <string.h>
#include <stdio.h>
#include "bf_gmp.h"
#include <stdlib.h>

#define STEIN 1

void
bf_print_i(const char *msg, const bf_t *i)
{ printf("%s=%s\n",
	 msg,
	 bf_ftoa(NULL, i, 10, 0, BF_RNDZ|BF_FTOA_FORMAT_FRAC));
}


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

/* multiply 'r' by 2^e */
// shift in place (copied from libbf.c with rounding removed)
void
mpz_gcd(mpz_t r, const mpz_t n1, const mpz_t n2)
{ mpz_t a, b;
  mp_bitcnt_t als1, bls1, k;

  if ( bf_is_zero(n1) )
  { mpz_abs(r, n2);
    return;
  }
  if ( bf_is_zero(n2) )
  { mpz_abs(r, n1);
    return;
  }

  mpz_init(a);
  mpz_init(b);
  // gcd is always positive
  mpz_abs(a, n1);
  mpz_abs(b, n2);
  int d = 0;

  while (mpz_cmp(a, b) != 0)
  { als1 = mpz_scan1(a, 0);
    bls1 = mpz_scan1(b, 0);

    switch ((als1 == 0)*2 +(bls1 == 0))
    { case 0: // both even
	k = -bf_min(als1,bls1);
	mul_2exp(a, k);
	mul_2exp(b, k);
	d = d-k;  // add offset (k is negative)
	break;
      case 1: // a even, b odd
	mul_2exp(a, -als1);
	break;
      case 2: // a odd, b even
	mul_2exp(b, -bls1);
	break;
      case 3: // both odd, use r as temporary for swapping
	mpz_sub(r, a, b);
	if (r->sign)
	{ mpz_swap(b, a);  // a < b --> a0    -> b1
	  bf_neg(r);
	  mpz_set(a, r);   //           b0-a0 -> a1
	} else
	{ mpz_swap(a, b);  // a > b --> b0    -> a1
	  mpz_set(b, r);   //           a0-b0 -> b1
	}
	break;
    }
  }

  mul_2exp(a, d);   // a==b, so we're done, a*2^d -> r
  mpz_set(r, a);
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
}

void
mpq_div(mpq_t r, const mpq_t q1, const mpq_t q2)
{ mpz_mul(mpq_numref(r), mpq_cnumref(q1), mpq_cdenref(q2));
  mpz_mul(mpq_denref(r), mpq_cdenref(q1), mpq_cnumref(q2));
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
  { mpz_urandom_2exp(r, state, mpz_sizeinbase(N,2));
  } else
  { do
    { mpz_urandom_2exp(r, state, mpz_sizeinbase(N,2)+1);
    } while( mpz_cmp(r, N) >= 0 );
  }
}

void
bf_set_randstate(gmp_randstate_t state, const mpz_t n)
{ size_t bytes = (mpz_sizeinbase(n, 2)+7)/8;
  size_t count;

  memset(state, 0, sizeof(state[0]));
  mpz_export((unsigned char*)state+sizeof(state[0])-bytes,
	     &count, 1, 1, 1, 0, n);
  assert(count == bytes);
}

void
bf_get_randstate(mpz_t n, const gmp_randstate_t state)
{ mpz_import(n, sizeof(state[0]), 1, 1, 1, 0, state);
}


int
mpz_tstbit(const mpz_t n, mp_bitcnt_t i)
{ if ( i >= n->expn )
    return 0;

  limb_t boffset = n->expn-1 - i; /* offset from msb */
  limb_t loffset = boffset/(sizeof(limb_t)*8);
  if ( loffset >= n->len )
    return 0;

  /* bit is 0..63, counting from lsb */
  int bit = sizeof(limb_t)*8 - 1 - boffset % (sizeof(limb_t)*8);

  return !!(n->tab[n->len-1-loffset] & ((limb_t)1<<bit));
}


mp_bitcnt_t
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
  { bf_resize(r, alllimbs);
    memmove(&r->tab[alllimbs-len0], &r->tab[0], len0*sizeof(limb_t));
    for(size_t i=0; i<alllimbs-len0; i++)
      r->tab[i] = ~(limb_t)0;
  }

  for(size_t i = alllimbs-len0; i<alllimbs; i++)
    r->tab[i] ^= ~(limb_t)0;

  bf_normalize_and_round(r, BF_PREC_INF, BF_RNDN);
}


void
mpz_rootrem(mpz_t rop, mpz_t rem, const mpz_t OP, unsigned long int n)
{ mpz_t cn, cn1, nxt, x, tmp;
  const MP_INT *op;

  if ( rop == OP )
  { mpz_init_set(tmp, OP);
    op = &tmp[0];
  } else
    op = &OP[0];

  mpz_init_set_ui(cn, n);     // const n
  mpz_init_set_ui(cn1, n-1);  // const n-1
  mpz_init_set(nxt, op);
  mpz_init(x);

  do  // using rop and rem as temporaries
  { mpz_set(x, nxt);                  // x = nxt
    //    mpz_pow_ui(rop, x, n-1);          // rop = pow(x, n-1)        PLUS 1
    bf_pow(rop, x, cn1, BF_PREC_INF, BF_RNDN);  // rop = pow(x, n-1)
    mpz_fdiv_qr(rop, rem, op, rop);     // rop = op // rop
    //    mpz_addmul_ui(rop, &s, n-1);       // rop = rop + x * (n-1)    PLUS 1
    mpz_mul_ui(rem, x, n-1);           // rem = x * (n-1)
    mpz_add(rop, rop, rem);             // rop = rop + rem
    mpz_fdiv_qr(nxt, rem, rop, cn);   // nxt = rop // n
  } while (mpz_cmp(nxt, x) < 0);

  mpz_set(rop, x);

  bf_pow(rem, rop, cn, BF_PREC_INF, BF_RNDN);  // rem = pow(rop, n)
  bf_sub(rem, op, rem, BF_PREC_INF, BF_RNDN);  // rem = op - rem

  mpz_clear(cn);
  mpz_clear(cn1);
  mpz_clear(nxt);
  mpz_clear(x);
  if ( rop == OP )
    mpz_clear(tmp);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the exponent and len given a bigint represented as a series of
bytes.  Note that LibBF does not include 0-limbs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
bf_import_dimension(bf_t *r, const unsigned char *data, size_t len)
{ unsigned int i = data[0];
  size_t  limbs = (len+sizeof(limb_t)-1)/sizeof(limb_t);
  const unsigned char *ll = data + (len/sizeof(limb_t))*sizeof(limb_t);
  const unsigned char *end = &data[len-1];
  int byte = sizeof(limb_t);

  r->expn = len*8 - (__builtin_clz(i) - (sizeof(i)*8 - 8));
  r->len  = limbs;
  if ( end >= ll )
  { for(; end >= ll; end-- )
    { if ( *end )
	return;				/* last limb non-zero */
    }
    r->len--;
  }
  while(!*end--)
  { if ( --byte == 0 )
    { r->len--;
      byte = sizeof(limb_t);
    }
  }

  if ( r->len == 0 )
    r->expn = BF_EXP_ZERO;
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

    int shift = COUNT*8-bf.expn;
    limb_t mask = ((limb_t)1<<shift)-1;

    lt = &ROP->tab[bf.len-1];
    while(bytes-->0)
    { l |= (limb_t)*data++ << byte*8;
      if ( byte == 0 )
      { byte =  sizeof(limb_t)-1;
	if ( shift )
	{ l <<= shift;
	  if ( bytes >= 0 )
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
      int shift = bytes*8-OP->expn;
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
