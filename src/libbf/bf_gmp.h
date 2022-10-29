#ifndef BF_GMP_H_INCLUDED
#define BF_GMP_H_INCLUDED

#include "bf_gmp_types.h"
#include <math.h>
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

/* Actually we mpq_set_double(), which we might be able to do simply
   by selecting the right rounding mode.
 */

static inline void
mpz_set_d(mpz_t r, double d)
{ bf_set_float64(r, d);
  bf_rint(r, BF_RNDZ);
}

static inline void
mpz_init_set_d(mpz_t r, double f)
{ mpz_init(r);
  mpz_set_d(r, f);
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
{ double d;

  if ( bf_get_float64(n, &d, BF_RNDZ) != 0 )
    d = NAN;

  return d;
}


void bf_import_dimension(bf_t *r, const unsigned char *data, size_t len);
void mpz_import(mpz_t ROP, size_t COUNT, int ORDER,
		size_t SIZE, int ENDIAN, size_t NAILS, const void *OP);
void *mpz_export(void *ROP, size_t *COUNTP, int ORDER,
		 size_t SIZE, int ENDIAN, size_t NAILS, const mpz_t OP);



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
{ mp_bitcnt_t cnt = 0;

  for(size_t i=0; i<n->len; i++)
    cnt += __builtin_popcountll(n->tab[i]);

  return cnt;
}

// Should complement the ->tab, shift leading zeros and adjust expn
// Maybe we can reuse bf_logic_xor()?
static inline void
mpz_com(mpz_t r, const mpz_t n)
{ bf_not_implemented("mpz_com");
}

static inline void
mpz_ior(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_logic_or(r, n1, n2);
}

static inline void
mpz_and(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_logic_and(r, n1, n2);
}

static inline void
mpz_xor(mpz_t r, const mpz_t n1, const mpz_t n2)
{ bf_logic_xor(r, n1, n2);
}

mp_bitcnt_t	mpz_scan1(const mpz_t n, mp_bitcnt_t start);

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
{ if ( r != n1 )
    bf_set(r, n1);
  bf_mul_2exp(r, n2, BF_PREC_INF, BF_RNDN);
}

static inline void
mpz_addmul_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ mpz_t add;

  mpz_init(add);
  bf_mul_ui(add, n1, n2, BF_PREC_INF, BF_RNDN);
  bf_add(r, n1, add, BF_PREC_INF, BF_RNDN);
  mpz_clear(add);
}

static inline void
mpz_submul_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ mpz_t sub;

  mpz_init(sub);
  bf_mul_ui(sub, n1, n2, BF_PREC_INF, BF_RNDN);
  bf_sub(r, n1, sub, BF_PREC_INF, BF_RNDN);
  mpz_clear(sub);
}

static inline void
mpz_divexact(mpz_t Q, const mpz_t N, const mpz_t D)
{ bf_t rem;
  bf_init(&alloc_wrapper.bf_context, &rem);
  bf_divrem(Q, &rem, N, D, BF_PREC_INF, 0, BF_RNDN);
  assert(bf_is_zero(&rem));
  bf_delete(&rem);
}

/* _fdiv_ and _tdiv_ functions.  Both compute the quatient and
   remainder and for all holds N=Q*D+R and 0<=abs(R)<abs(D).  The two
   families differ in the rounding.

   - The _fdiv_ (floor div) family rounds Q to -infinity and R has the
     same sign as D.
   - the _tdiv_ (truncate div) family rounds Q towards 0 and R has the
     same sign as N
 */

static inline void
mpz_fdiv_q(mpz_t Q, const mpz_t N, const mpz_t D)
{ bf_t rem;
  bf_init(&alloc_wrapper.bf_context, &rem);
  bf_divrem(Q, &rem, N, D, BF_PREC_INF, 0, BF_RNDD);
  bf_delete(&rem);
}

static inline void
mpz_fdiv_r(mpz_t R, const mpz_t N, const mpz_t D)
{ bf_rem(R, N, D, BF_PREC_INF, 0, BF_RNDD);
}

static inline void
mpz_fdiv_qr(mpz_t Q, mpz_t R, const mpz_t N, const mpz_t D)
{ bf_divrem(Q, R, N, D, BF_PREC_INF, 0, BF_RNDD);
}

static inline unsigned long	/* remainder of N/d */
mpz_fdiv_ui(const mpz_t N, unsigned long d)
{ mpz_t D, Q;
  bf_t rem;
  int64_t r;

  mpz_init_set_ui(D, d);
  mpz_init(Q);
  bf_init(&alloc_wrapper.bf_context, &rem);
  bf_divrem(Q, &rem, N, D, BF_PREC_INF, 0, BF_RNDD);
  bf_get_int64(&r, &rem, BF_RNDN);
  mpz_clear(Q);
  bf_delete(&rem);
  return r;
}

static inline void
mpz_fdiv_q_2exp(mpz_t Q, const mpz_t N, mp_bitcnt_t B)
{ bf_t rem;
  mpz_t D;

  mpz_init_set_ui(D, 1);
  mpz_mul_2exp(D, D, B);
  bf_init(&alloc_wrapper.bf_context, &rem);
  bf_divrem(Q, &rem, N, D, BF_PREC_INF, 0, BF_RNDD);
  bf_delete(&rem);
}

static inline void
mpz_tdiv_q(mpz_t Q, const mpz_t N, const mpz_t D)
{ bf_t rem;
  bf_init(&alloc_wrapper.bf_context, &rem);
  bf_divrem(Q, &rem, N, D, BF_PREC_INF, 0, BF_RNDZ);
  bf_delete(&rem);
}

static inline void
mpz_tdiv_qr(mpz_t Q, mpz_t R, const mpz_t N, const mpz_t D)
{ bf_divrem(Q, R, N, D, BF_PREC_INF, 0, BF_RNDZ);
}

static inline void
mpz_tdiv_r(mpz_t R, const mpz_t N, const mpz_t D)
{ bf_rem(R, N, D, BF_PREC_INF, 0, BF_RNDZ);
}

static inline unsigned long
mpz_tdiv_q_ui(mpz_t Q, const mpz_t N, unsigned long d)
{ mpz_t D;
  bf_t rem;
  int64_t r;

  mpz_init_set_ui(D, d);
  bf_init(&alloc_wrapper.bf_context, &rem);
  bf_divrem(Q, &rem, N, D, BF_PREC_INF, 0, BF_RNDZ);
  bf_get_int64(&r, &rem, BF_RNDN);
  bf_delete(&rem);
  return r;
}


static inline int
mpz_divisible_p(const mpz_t N, const mpz_t D)
{ bf_t rem;
  int rc;

  bf_init(&alloc_wrapper.bf_context, &rem);
  bf_rem(&rem, N, D, BF_PREC_INF, 0, BF_RNDN);
  rc = bf_is_zero(&rem);
  bf_delete(&rem);

  return rc;
}

void mpz_gcd(mpz_t r, const mpz_t n1, const mpz_t n2);
void mpz_lcm(mpz_t r, const mpz_t n1, const mpz_t n2);

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
{ bf_t X, Y;

  bf_init(&alloc_wrapper.bf_context, &X); /* TBD: Provide statically allocated versions */
  bf_init(&alloc_wrapper.bf_context, &Y);
  bf_set_si(&X, x);
  bf_set_si(&Y, y);
  bf_pow(r, &X, &Y, BF_PREC_INF, BF_RNDN);
  bf_delete(&X);
  bf_delete(&Y);
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
mpq_set_d(mpq_t r, double f)
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

static inline const MP_INT*
mpq_cnumref(const mpq_t q)
{ return &q[0];
}

static inline const MP_INT*
mpq_cdenref(const mpq_t q)
{ return &q[1];
}

static inline int
mpq_sgn(const mpq_t q)
{ int s0 = mpz_sgn(&q[0]);

  return q[1].sign ? -s0 : s0;
}

static inline void
mpq_abs(mpq_t r, const mpq_t q)
{ if ( r != q )
  { mpq_init(r);
    mpq_set(r, q);
  }
  r[0].sign = 0;
  r[1].sign = 0;
}

static inline void
mpq_neg(mpq_t r, const mpq_t q)
{ if ( r != q )
  { mpq_init(r);
    mpq_set(r, q);
  }
  bf_neg(&r[0]);
}

void mpq_canonicalize(mpq_t q);

static inline void
mpq_inv(mpq_t r, mpq_t q)
{ mpq_init(r);
  bf_set(&r[0], &q[1]);
  bf_set(&r[1], &q[0]);
}

static inline void
mpz_set_q(mpz_t ROP, const mpq_t OP)
{ bf_t rem;

  bf_init(&alloc_wrapper.bf_context, &rem);
  bf_divrem(ROP, &rem, mpq_cnumref(OP), mpq_cdenref(OP),
	    BF_PREC_INF, 0, BF_RNDZ);
  bf_delete(&rem);
}

int	mpq_cmp(const mpq_t q1, const mpq_t q2);
int	mpq_cmp_ui(const mpq_t q1, unsigned long n, unsigned long d);
void	mpq_add(mpq_t r, const mpq_t q1, const mpq_t q2);
void	mpq_sub(mpq_t r, const mpq_t q1, const mpq_t q2);
void	mpq_mul(mpq_t r, const mpq_t q1, const mpq_t q2);
void	mpq_div(mpq_t r, const mpq_t q1, const mpq_t q2);


		 /*******************************
		 *		MPF		*
		 *******************************/

static inline void
mpf_init2(mpf_t r, mp_bitcnt_t bits)
{ bf_init(&alloc_wrapper.bf_context, r);
}

static inline void
mpf_clear(mpf_t f)
{ bf_delete(f);
}

static inline void
mpf_set_z(mpf_t f, mpz_t n)
{ bf_not_implemented("mpf_set_z");
}

static inline void
mpf_set_q(mpf_t f, mpq_t q)
{ bf_not_implemented("mpf_set_q");
}

static inline double
mpf_get_d(const mpf_t f)
{ double d;

  if ( bf_get_float64(f, &d, BF_RNDN) == 0 )
    return d;

  assert(0);
  return 0.0;
}

void	mpf_urandomb(mpf_t r, gmp_randstate_t state, mp_bitcnt_t bits);


		 /*******************************
		 *	       RANDOM		*
		 *******************************/

#define HAVE_GMP_RANDINIT_MT 1

void	mpz_urandomm(mpz_t r, gmp_randstate_t state, const mpz_t N);

static inline void
gmp_randinit_mt(gmp_randstate_t state)
{ memset(state, 0, sizeof(state[0]));
}

void	gmp_randseed(gmp_randstate_t state, const mpz_t seed);
void	gmp_randseed_ui(gmp_randstate_t state, unsigned long seed);

static inline void
gmp_randclear(gmp_randstate_t state)
{ (void) state;
}

void	bf_set_randstate(gmp_randstate_t state, const mpz_t n);
void	bf_get_randstate(mpz_t n, const gmp_randstate_t state);

int	gmp_snprintf(char *BUF, size_t SIZE, const char *FMT, ...);

#endif /*BF_GMP_H_INCLUDED*/
