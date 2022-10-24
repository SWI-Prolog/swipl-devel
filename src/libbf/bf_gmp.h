#ifndef BF_GMP_H_INCLUDED
#define BF_GMP_H_INCLUDED

#include "bf_gmp_types.h"
#include <assert.h>

typedef void *(*mp_malloc_t)(size_t);
typedef void *(*mp_realloc_t)(void *, size_t old, size_t newsize);
typedef void  (*mp_free_t)(void *, size_t size);

typedef struct mp_alloc_wrapper
{ bf_context_t bf_context;
  mp_realloc_t realloc_func;
} mp_alloc_wrapper;

extern mp_alloc_wrapper alloc_wrapper;
extern void bf_not_implemented(const char *func);


		 /*******************************
		 *	      CONTEXT		*
		 *******************************/

static inline void
mp_get_memory_functions(mp_malloc_t *m, mp_realloc_t *r, mp_free_t *f)
{ *m = NULL;
  *r = alloc_wrapper.realloc_func;
  *f = NULL;
}

static inline void
mp_set_memory_functions(mp_malloc_t m, mp_realloc_t r, mp_free_t f)
{ alloc_wrapper.realloc_func = r;
}

		 /*******************************
		 *	       MPZ		*
		 *******************************/

// Allow/free

static inline void
mpz_init(mpz_t i)
{ bf_init(&alloc_wrapper.bf_context, i);
}

static inline void
mpz_clear(mpz_t i)
{ bf_delete(i);
}

static inline void
mpz_init_set(mpz_t r, const mpz_t n)
{ bf_init(&alloc_wrapper.bf_context, r);
  bf_set(r, n);
}

static inline void
mpz_init_set_si(mpz_t r, long n)
{ bf_init(&alloc_wrapper.bf_context, r);
  bf_set_si(r, n);
}

static inline void
mpz_init_set_ui(mpz_t r, unsigned long n)
{ assert((int64_t)n >= 0);
  bf_init(&alloc_wrapper.bf_context, r);
  bf_set_si(r, n);
}

static inline void
mpz_init_set_si64(mpz_t r, int64_t n)
{ bf_init(&alloc_wrapper.bf_context, r);
  bf_set_si(r, n);
}

static inline void
mpz_init_set_d(mpz_t r, double n)
{ bf_init(&alloc_wrapper.bf_context, r);
  bf_not_implemented("mpz_init_set_d");
}

// Copy
static inline void
mpz_set(mpz_t r, const mpz_t n)
{ bf_set(r, n);
}

static inline void
mpz_set_ui(mpz_t r, unsigned long n)
{ assert((int64_t)n >= 0);
  bf_set_si(r, n);
}

static inline void
mpz_set_d(mpz_t r, double d)
{ bf_not_implemented("mpz_set_d");
}

static inline void
mpz_set_q(mpz_t ROP, const mpq_t OP)
{ bf_not_implemented("mpz_set_q");
}

static inline long
mpz_get_si(const mpz_t n)
{ int64_t nv;

  if ( bf_get_int64(&nv, n, BF_RNDN) == 0 )
    return nv;

  assert(0);				/* TBD: return least significant bits */
  return 0;
}

static inline long
mpz_get_ui(const mpz_t n)
{ int64_t nv;

  if ( bf_get_int64(&nv, n, BF_RNDN) == 0 )
  { if ( nv < 0 )
      nv = -nv;
    return nv;
  }

  assert(0);				/* TBD: return least significant bits */
  return 0;
}

static inline double
mpz_get_d(const mpz_t n)
{ bf_not_implemented("mpz_get_d");
  return 0.0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the exponent and len given a bigint represented as a series of
bytes.  Note that LibBF does not include 0-limbs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
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
  }
  while(!*end--)
  { if ( --byte == 0 )
    { r->len--;
      byte = sizeof(limb_t);
    }
  }
}



static inline void
mpz_import(mpz_t ROP, size_t COUNT, int ORDER,
	   size_t SIZE, int ENDIAN, size_t NAILS, const void *OP)
{ if ( SIZE == 1 )
  { bf_t bf;
    size_t byte = sizeof(limb_t)-1;
    size_t bytes = COUNT;
    limb_t *lt;
    limb_t l = 0;
    const unsigned char *data = OP;

    bf_import_dimension(&bf, OP, COUNT);
    lt = &ROP->tab[bf.len-1];

    assert(NAILS==0 && ORDER==1 && ENDIAN==1);
    assert(bf.len == ROP->len);

    ROP->sign = 0;
    ROP->expn = bf.expn;

    while(bytes-->0)
    { l |= (limb_t)*data++ << bytes*8;
      if ( byte == 0 )
      { byte =  sizeof(limb_t)-1;
	if ( lt == ROP->tab )
	  return;			/* rest is all zero */
	*lt-- = l;
	l = 0;
      } else
	byte--;
    }
    *lt = l;
    assert(lt == ROP->tab);
  } else
  { bf_not_implemented("mpz_import for SIZE != 1");
  }
}

static inline void *
mpz_export(void *ROP, size_t *COUNTP, int ORDER,
	   size_t SIZE, int ENDIAN, size_t NAILS, const mpz_t OP)
{ if ( SIZE == 1 )			/* export per byte */
  { if ( OP->expn == 0 )
    { *COUNTP = 0;
      return ROP;
    } else
    { size_t bytes = (OP->expn+7)/8;
      limb_t *lt = &OP->tab[OP->len-1];
      limb_t l = *lt;
      int byte = sizeof(limb_t)-1;
      char *out = ROP;

      *COUNTP = bytes;
      while(bytes-->0)
      { *out++ = (l>>(8*byte))&0xff;
	if ( byte == 0 )
	{ byte = sizeof(limb_t)-1;
	  if ( lt == OP->tab )
	    l = 0;
	  else
	    l = *--lt;
	} else
	  byte--;
      }
      return ROP;
    }
  }

  bf_not_implemented("mpz_export for SIZE != 1");
  return NULL;
}

// Signs

static inline int
mpz_sgn(const mpz_t n)
{ if ( n[0].sign == 0 )
    return bf_is_zero(n) ? 0 : 1;
  return -1;
}

static inline void
mpz_neg(mpz_t r, const mpz_t n)
{ if ( r != n )
    mpz_init_set(r, n);

  bf_neg(r);
}

static inline void
mpz_abs(mpz_t r, const mpz_t n)
{ if ( r != n )
    mpz_init_set(r, n);

  r[0].sign = 0;
}

// Bit operations
// Offsets are 0-based

static inline int
mpz_tstbit(const mpz_t n, mp_bitcnt_t i)
{ return bf_tstbit(n, i);
}

static inline mp_bitcnt_t
mpz_popcount(const mpz_t n)
{ bf_not_implemented("mpz_popcount");
  return 0;
}

static inline void
mpz_com(mpz_t r, const mpz_t n)
{ bf_not_implemented("mpz_com");
}

static inline void
mpz_ior(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_not_implemented("mpz_ior");
}

static inline void
mpz_and(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_not_implemented("mpz_ior");
}

static inline void
mpz_xor(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_not_implemented("mpz_ior");
}

static inline mp_bitcnt_t
mpz_scan1(const mpz_t n, mp_bitcnt_t start)
{ bf_not_implemented("mpz_scan1");
  return 0;
}

static inline size_t
mpz_sizeinbase (const mpz_t n, int base)
{ if ( base == 2 )
    return n->expn;
  bf_not_implemented("mpz_sizeinbase with base != 2");
  return 0;
}

static inline int
mpz_cmp(const mpz_t n1, const mpz_t n2)
{ return bf_cmp(n1, n2);
}

static inline int
mpz_cmpabs(const mpz_t n1, const mpz_t n2)
{ return bf_cmpu(n1, n2);
}

static inline int
mpz_cmp_ui(const mpz_t n, unsigned long i)
{ int64_t nv;

  if ( bf_get_int64(&nv, n, BF_RNDN) == 0 )
    return i == nv ? 0 : i > nv ? 1 : -1;
  return n->sign ? 1 : -1;
}

static inline int
mpz_cmp_si(const mpz_t n, long i)
{ int64_t nv;

  if ( bf_get_int64(&nv, n, BF_RNDN) == 0 )
    return i == nv ? 0 : i > nv ? 1 : -1;
  return n->sign ? 1 : -1;
}

// Basic arithmetic

static inline void
mpz_add(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_add(r, n1, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_add_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ assert((int64_t)n2 >= 0);
  bf_add_si(r, n1, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_add_si(mpz_t r, const mpz_t n1, long n2)
{ bf_add_si(r, n1, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_sub(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_sub(r, n1, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_sub_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ assert((int64_t)n2 >= 0);
  bf_add_si(r, n1, -(int64_t)n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_mul(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_mul(r, n1, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_mul_si(mpz_t r, const mpz_t n1, long n2)
{ bf_mul_si(r, n1, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_mul_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ bf_mul_ui(r, n1, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_mul_2exp(mpz_t r, const mpz_t n1, mp_bitcnt_t n2)
{ bf_set(r, n1);
  bf_mul_2exp(r, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_addmul_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ bf_not_implemented("mpz_addmul_ui");
}

static inline void
mpz_submul_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ bf_not_implemented("mpz_submul_ui");
}

static inline void
mpz_divexact(mpz_t Q, const mpz_t N, const mpz_t D)
{
}

static inline void
mpz_fdiv_r(mpz_t R, const mpz_t N, const mpz_t D)
{ bf_not_implemented("mpz_fdiv_r");
}

static inline void
mpz_fdiv_qr(mpz_t Q, mpz_t R, const mpz_t N, const mpz_t D)
{ bf_not_implemented("mpz_fdiv_qr");
}

static inline unsigned long
mpz_fdiv_ui(const mpz_t n1, unsigned long n2)
{ bf_not_implemented("mpz_fdiv_ui");
  return 0;
}

static inline void
mpz_fdiv_q_2exp(mpz_t Q, const mpz_t N, mp_bitcnt_t B)
{ bf_not_implemented("mpz_fdiv_q_2exp");
}

static inline void
mpz_tdiv_q(mpz_t Q, const mpz_t N, const mpz_t D)
{ bf_not_implemented("mpz_tdiv_q");
}

static inline void
mpz_fdiv_q(mpz_t Q, const mpz_t N, const mpz_t D)
{ bf_not_implemented("mpz_fdiv_q");
}

static inline void
mpz_tdiv_qr(mpz_t Q, mpz_t R, const mpz_t N, const mpz_t D)
{ bf_not_implemented("mpz_tdiv_qr");
}

static inline void
mpz_tdiv_r(mpz_t R, const mpz_t N, const mpz_t D)
{
}

static inline int
mpz_divisible_p(const mpz_t N, const mpz_t D)
{ bf_not_implemented("mpz_divisible_p");
  return 0;
}

static inline void
mpz_gcd(mpz_t r, const mpz_t n1, mpz_t n2)
{ bf_not_implemented("mpz_gcd");
}

static inline void
mpz_lcm(mpz_t r, const mpz_t n1, mpz_t n2)
{ bf_not_implemented("mpz_lcm");
}

static inline int
mpz_root(mpz_t ROP, const mpz_t OP, unsigned long int N)
{ bf_not_implemented("mpz_root");
  return 0;
}

static inline void
mpz_rootrem(mpz_t ROOT, mpz_t REM, const mpz_t U, unsigned long int N)
{ bf_not_implemented("mpz_rootrem");
}

static inline void
mpz_ui_pow_ui(mpz_t r, unsigned long x, unsigned y)
{ bf_not_implemented("mpz_ui_pow_ui");
}

static inline void
mpz_powm(mpz_t r, const mpz_t base, const mpz_t exp, const mpz_t mod)
{ bf_not_implemented("mpz_ui_powm");
}

static inline void
mpz_pow_ui(mpz_t r, const mpz_t x, unsigned y)
{ bf_not_implemented("mpz_pow_ui");
}

static inline char *
mpz_get_str(char *STR, int BASE, const mpz_t OP)
{ const bf_t *op = OP;
  bf_t copy;

  if ( !op->ctx )
  { copy = OP[0];
    copy.ctx = &alloc_wrapper.bf_context;
    op = &copy;
  }

  strcpy(STR, bf_ftoa(NULL, op, BASE, 0, BF_RNDZ|BF_FTOA_FORMAT_FRAC));

  return STR;
}


		 /*******************************
		 *	       MPQ		*
		 *******************************/

static inline void
mpq_init(mpq_t q)
{ bf_init(&alloc_wrapper.bf_context, &q[0]);
  bf_init(&alloc_wrapper.bf_context, &q[1]);
}

static inline void
mpq_clear(mpq_t q)
{ bf_delete(&q[0]);
  bf_delete(&q[1]);
}

static inline void
mpq_swap(mpq_t q1, mpq_t q2)
{ mpq_t tmp;

  memcpy(tmp, q1, sizeof(mpq_t));
  memcpy(q1,  q2, sizeof(mpq_t));
  memcpy(q2, tmp, sizeof(mpq_t));
}


static inline void
mpq_set(mpq_t r, const mpq_t q)
{ bf_set(&r[0], &q[0]);
  bf_set(&r[1], &q[1]);
}

static inline void
mpq_set_d(mpq_t r, double)
{ bf_not_implemented("mpq_set_d");
}

static inline void
mpq_set_z(mpq_t r, const mpz_t n)
{ bf_set(&r[0], n);
  bf_set_si(&r[1], 1);
}

static inline void
mpq_set_ui(mpq_t r, unsigned long n, unsigned long d)
{ assert((int64_t)n >= 0);
  assert((int64_t)d >= 0);
  bf_set_si(&r[0], n);
  bf_set_si(&r[1], d);
}

static inline void
mpq_set_si(mpq_t r, long n, long d)
{ bf_set_si(&r[0], n);
  bf_set_si(&r[1], d);
}

static inline MP_INT*
mpq_numref(mpq_t q)
{ return &q[0];
}

static inline MP_INT*
mpq_denref(mpq_t q)
{ return &q[1];
}

static inline int
mpq_sgn(const mpq_t q)
{ int s0 = mpz_sgn(&q[0]);

  return q[1].sign ? -s0 : s0;
}

static inline void
mpq_abs(mpq_t r, const mpq_t q)
{ mpq_init(r);
  mpq_set(r, q);
  r[0].sign = 0;
  r[1].sign = 0;
}

static inline void
mpq_neg(mpq_t r, const mpq_t q)
{ mpq_init(r);
  mpq_set(r, q);
  bf_neg(&r[0]);
}

static inline void
mpq_canonicalize(mpq_t q)
{ bf_not_implemented("mpq_canonicalize");
}

static inline void
mpq_inv(mpq_t r, mpq_t q)
{ mpq_init(r);
  bf_set(&r[0], &q[1]);
  bf_set(&r[1], &q[0]);
}

static inline int
mpq_cmp(const mpq_t q1, const mpq_t q2)
{ bf_not_implemented("mpq_cmp");
  return 0;
}

static inline void
mpq_add(mpq_t r, const mpq_t q1, const mpq_t q2)
{ bf_not_implemented("mpq_add");
}

static inline void
mpq_sub(mpq_t r, const mpq_t q1, const mpq_t q2)
{ bf_not_implemented("mpq_sub");
}

static inline void
mpq_mul(mpq_t r, const mpq_t q1, const mpq_t q2)
{ bf_not_implemented("mpq_mul");
}

static inline void
mpq_div(mpq_t r, const mpq_t q1, const mpq_t q2)
{ bf_not_implemented("mpq_div");
}

		 /*******************************
		 *	        MPF		*
		 *******************************/

static inline void
mpf_init2(mpf_t r, mp_bitcnt_t bits)
{ bf_init(&alloc_wrapper.bf_context, r);
}

static inline void
mpf_clear(mpf_t f)
{ bf_delete(f);
}

static inline double
mpf_get_d(const mpf_t f)
{ double d;

  if ( bf_get_float64(f, &d, BF_RNDN) == 0 )
    return d;

  assert(0);
  return 0.0;
}

static inline void
mpf_urandomb(mpf_t r, gmp_randstate_t state, mp_bitcnt_t bits)
{ bf_not_implemented("mpf_urandomb");
}



		 /*******************************
		 *	       RANDOM		*
		 *******************************/

static inline void
mpz_urandomm(mpz_t r, gmp_randstate_t state, const mpz_t N)
{ bf_not_implemented("mpz_urandomm");
}

static inline void
gmp_randinit_default(gmp_randstate_t state)
{ bf_not_implemented("gmp_randinit_default");
}

static inline void
gmp_randseed(gmp_randstate_t state, const mpz_t seed)
{ bf_not_implemented("gmp_randseed");
}

static inline void
gmp_randseed_ui(gmp_randstate_t state, unsigned long seed)
{ bf_not_implemented("gmp_randseed_ui");
}

static inline void
gmp_randclear(gmp_randstate_t state)
{ bf_not_implemented("gmp_randclear");
}

#endif /*BF_GMP_H_INCLUDED*/
