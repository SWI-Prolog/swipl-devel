#include <sys/types.h>			/* get ssize_t */
#include <string.h>
#include <stdio.h>
#include "bf_gmp.h"
#include <stdlib.h>

void
bf_print_i(const char *msg, const bf_t *i)
{ printf("%s=%s\n",
	 msg,
	 bf_ftoa(NULL, i, 10, 0, BF_RNDZ|BF_FTOA_FORMAT_FRAC));
}


void
mpz_gcd(mpz_t r, const mpz_t n1, mpz_t n2)
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

void
mpz_lcm(mpz_t r, const mpz_t n1, mpz_t n2)
{ bf_not_implemented("mpz_lcm");
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
{ bf_not_implemented("mpq_cmp");
  return 0;
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


void
mpz_urandomm(mpz_t r, gmp_randstate_t state, const mpz_t N)
{ if ( bf_is_zero(N) )
  { bf_set_si(r, 0);
  } else if ( bf_is_exp_2(N) )
  { mpz_urandom_2exp(r, state, mpz_sizeinbase(N,2));
  } else
  { bf_not_implemented("mpz_urandomm for N != 2^M");
  }
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
