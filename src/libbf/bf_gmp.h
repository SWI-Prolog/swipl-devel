/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

#ifndef BF_GMP_H_INCLUDED
#define BF_GMP_H_INCLUDED

#include "bf_gmp_types.h"
#include <math.h>
#ifdef NO_ASSERT_H
#define assert(c) (void)0
#else
#include <assert.h>
#endif

#ifdef _MSC_VER
static inline size_t
__builtin_popcountll(long long sz)
{ return __popcnt64(sz);
}
#endif

typedef void *(*mp_malloc_t)(size_t);
typedef void *(*mp_realloc_t)(void *, size_t old, size_t newsize);
typedef void  (*mp_free_t)(void *, size_t size);

typedef struct mp_alloc_wrapper
{ bf_context_t bf_context;
  mp_realloc_t realloc_func;
  mp_free_t free_func;
} mp_alloc_wrapper;

extern mp_alloc_wrapper alloc_wrapper;

extern void bf_not_implemented(const char *func);

void	bf_print_i(const char *msg, const bf_t *i);


		 /*******************************
		 *	      CONTEXT		*
		 *******************************/

static inline void
mp_get_memory_functions(mp_malloc_t *m, mp_realloc_t *r, mp_free_t *f)
{ if(m) *m = NULL;
  if(r) *r = alloc_wrapper.realloc_func;
  if(f) *f = alloc_wrapper.free_func;
}

static inline void
mp_set_memory_functions(mp_malloc_t m, mp_realloc_t r, mp_free_t f)
{ alloc_wrapper.realloc_func = r;
  alloc_wrapper.free_func = f;
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
{ bf_init(&alloc_wrapper.bf_context, r);
  bf_set_ui(r, n);
}

static inline void
mpz_init_set_ui64(mpz_t r, uint64_t n)
{ bf_init(&alloc_wrapper.bf_context, r);
  bf_set_ui(r, n);
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
{ bf_set_ui(r, n);
}

static inline void
mpz_set_ui64(mpz_t r, int64_t n)
{ bf_set_ui(r, n);
}

static inline void
mpz_set_si(mpz_t r, long n)
{ bf_set_si(r, n);
}

static inline void
mpz_swap(mpz_t a, mpz_t b)
{ mpz_t tmp;

  tmp[0] = a[0];
  a[0] = b[0];
  b[0] = tmp[0];
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
    return (long)nv;

  assert(0);				/* TBD: return least significant bits */
  return 0;
}

static inline int64_t
mpz_get_si64(const mpz_t n)
{ int64_t nv;

  if ( bf_get_int64(&nv, n, BF_RNDN) == 0 )
    return nv;

  assert(0);				/* TBD: return least significant bits */
  return 0;
}

static inline unsigned long
mpz_get_ui(const mpz_t n)
{ int64_t v;

  if ( bf_get_int64(&v, n, BF_RNDZ|BF_GET_INT_MOD) == 0 )
    return (unsigned long)v;

  assert(0);				/* TBD: return least significant bits */
  return 0;
}

static inline double
mpz_get_d(const mpz_t n)
{ double d;
  const bf_t *op = n;
  bf_t copy;
  int rc;

  if ( !op->ctx )
  { copy = n[0];
    copy.ctx = &alloc_wrapper.bf_context;
    op = &copy;
  }

  rc = bf_get_float64(op, &d, BF_RNDZ);
  if ( (rc & ~BF_ST_INEXACT) == 0 )
    return d;
  if ( rc & BF_ST_OVERFLOW )
    return n->sign ? -INFINITY : INFINITY;

  assert(0);
  return NAN;
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

int	mpz_tstbit(const mpz_t n, mp_bitcnt_t i);

static inline mp_bitcnt_t
mpz_popcount(const mpz_t n)
{ mp_bitcnt_t cnt = 0;

  for(size_t i=0; i<n->len; i++)
    cnt += (mp_bitcnt_t)__builtin_popcountll(n->tab[i]);

  return cnt;
}

void	mpz_com(mpz_t r, const mpz_t n);

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
{ if ( bf_is_zero(n) )
    return 1;
  if ( base == 2 )
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
    return i == nv ? 0 : nv > i ? 1 : -1;
  return n->sign ? -1 : 1;
}

static inline int
mpz_cmp_si(const mpz_t n, long i)
{ int64_t nv;

  if ( bf_get_int64(&nv, n, BF_RNDN) == 0 )
    return i == nv ? 0 : nv > i ? 1 : -1;
  return n->sign ? -1 : 1;
}

static inline int
mpz_cmp_d(const mpz_t n, double f)
{ mpz_t tmp;
  int rc ;

  if      ( f ==  INFINITY ) return -1;
  else if ( f == -INFINITY ) return  1;
  else
  {
    mpz_init(tmp);
    mpz_set_d(tmp, f);
    rc = mpz_cmp(n, tmp);
    mpz_clear(tmp);

    return rc;
  }
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
{ if ( r == n1 || r == n2 )
  { mpz_t tmp;

    mpz_init(tmp);
    bf_mul(tmp, n1, n2, BF_PREC_INF, BF_RNDN);
    mpz_set(r, tmp);
    mpz_clear(tmp);
  } else
  { bf_mul(r, n1, n2, BF_PREC_INF, BF_RNDN);
  }
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
mpz_addmul(mpz_t r, const mpz_t n1, const mpz_t n2)
{ mpz_t tmp; mpz_init_set(tmp,r);
  mpz_t acc; mpz_init(acc);

  bf_mul(acc, n1, n2, BF_PREC_INF, BF_RNDN);
  bf_add(r, tmp, acc, BF_PREC_INF, BF_RNDN);

  mpz_clear(tmp);
  mpz_clear(acc);
}

static inline void
mpz_addmul_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ mpz_t tmp; mpz_init_set(tmp,r);
  mpz_t acc; mpz_init(acc);

  bf_mul_ui(acc, n1, n2, BF_PREC_INF, BF_RNDN);
  bf_add(r, tmp, acc, BF_PREC_INF, BF_RNDN);

  mpz_clear(tmp);
  mpz_clear(acc);
}

static inline void
mpz_submul_ui(mpz_t r, const mpz_t n1, unsigned long n2)
{ mpz_t tmp; mpz_init_set(tmp,r);
  mpz_t acc; mpz_init(acc);

  bf_mul_ui(acc, n1, n2, BF_PREC_INF, BF_RNDN);
  bf_sub(r, tmp, acc, BF_PREC_INF, BF_RNDN);

  mpz_clear(tmp);
  mpz_clear(acc);
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
{ if ( R == N || R == D )
  { mpz_t tmp;

    mpz_init(tmp);
    bf_rem(tmp, N, D, BF_PREC_INF, 0, BF_RNDD);
    mpz_set(R, tmp);
    mpz_clear(tmp);
  } else
    bf_rem(R, N, D, BF_PREC_INF, 0, BF_RNDD);
}

static inline void
mpz_fdiv_qr(mpz_t Q, mpz_t R, const mpz_t N, const mpz_t D)
{ if ( Q == D )
  { mpz_t tmp;

    mpz_init(tmp);
    bf_divrem(tmp, R, N, D, BF_PREC_INF, 0, BF_RNDD);
    mpz_set(Q, tmp);
    mpz_clear(tmp);
  } else
  { bf_divrem(Q, R, N, D, BF_PREC_INF, 0, BF_RNDD);
  }
}

static inline unsigned long	/* remainder of N/d */
mpz_fdiv_ui(const mpz_t N, unsigned long d)
{ mpz_t D, Q;
  mpz_t rem;
  unsigned long r;

  mpz_init_set_ui(D, d);
  mpz_init(Q);
  mpz_init(rem);
  bf_divrem(Q, rem, N, D, BF_PREC_INF, 0, BF_RNDD);
  r = mpz_get_ui(rem);
  mpz_clear(Q);
  mpz_clear(D);
  mpz_clear(rem);

  return r;
}

static inline unsigned long	/* remainder of N/d */
mpz_fdiv_q_ui(mpz_t Q, const mpz_t N, unsigned long d)
{ mpz_t D, rem;
  unsigned long r;

  mpz_init_set_ui(D, d);
  mpz_init(rem);

  if ( Q == N )
  { mpz_t tmp;

    mpz_init(tmp);
    bf_divrem(tmp, rem, N, D, BF_PREC_INF, 0, BF_RNDD);
    mpz_set(Q, tmp);
    mpz_clear(tmp);
  } else
  { bf_divrem(Q, rem, N, D, BF_PREC_INF, 0, BF_RNDD);
  }
  r = mpz_get_ui(rem);
  mpz_clear(rem);
  mpz_clear(D);

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
{ mpz_t rem;

  mpz_init(rem);
  if ( Q == N )
  { mpz_t q;
    mpz_init(q);
    bf_divrem(q, rem, N, D, BF_PREC_INF, 0, BF_RNDZ);
    mpz_set(Q, q);
    mpz_clear(q);
  } else
    bf_divrem(Q, rem, N, D, BF_PREC_INF, 0, BF_RNDZ);
  mpz_clear(rem);
}

static inline void
mpz_tdiv_qr(mpz_t Q, mpz_t R, const mpz_t N, const mpz_t D)
{ if ( Q == N || R == D || Q == D )
  { mpz_t q, r;
    mpz_init(q);
    mpz_init(r);
    bf_divrem(q, r, N, D, BF_PREC_INF, 0, BF_RNDZ);
    mpz_set(Q, q);
    mpz_set(R, r);
    mpz_clear(q);
    mpz_clear(r);
  } else
    bf_divrem(Q, R, N, D, BF_PREC_INF, 0, BF_RNDZ);
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
  return (unsigned long)r;
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

void	mpz_rootrem(mpz_t ROOT, mpz_t REM, const mpz_t U, unsigned long int N);

static inline int
mpz_root(mpz_t ROP, const mpz_t OP, unsigned long int N)
{ mpz_t rem;
  int rc;

  mpz_init(rem);
  mpz_rootrem(ROP, rem, OP, N);
  rc = bf_is_zero(rem);
  mpz_clear(rem);

  return rc;
}

void	mpz_pow_ui(mpz_t r, const mpz_t x, unsigned long y);
void	mpz_ui_pow_ui(mpz_t r, unsigned long x, unsigned long y);
void	mpz_powm(mpz_t r, const mpz_t base, const mpz_t exp, const mpz_t mod);
char   *mpz_get_str(char *STR, int BASE, const mpz_t OP);


		 /*******************************
		 *	       MPQ		*
		 *******************************/

static inline void
mpq_init(mpq_t q)
{ bf_init(&alloc_wrapper.bf_context, &q[0]);
  bf_init(&alloc_wrapper.bf_context, &q[1]);
  bf_set_si(&q[1], 1);
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

void	mpq_set_d(mpq_t r, double f);

static inline void
mpq_set_z(mpq_t r, const mpz_t n)
{ bf_set(&r[0], n);
  bf_set_ui(&r[1], 1);
}

static inline void
mpq_set_ui(mpq_t r, unsigned long n, unsigned long d)
{ assert((int64_t)n >= 0);
  assert((int64_t)d >= 0);
  bf_set_ui(&r[0], n);
  bf_set_ui(&r[1], d);
}

static inline void
mpq_set_si(mpq_t r, long n, unsigned long d)
{ bf_set_si(&r[0], n);
  bf_set_ui(&r[1], d);
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

static inline void
mpq_get_num(mpz_t n, const mpq_t r)
{ bf_set(n, &r[0]);
}

static inline void
mpq_get_den(mpz_t d, const mpq_t r)
{ bf_set(d, &r[1]);
}

static inline void
mpq_set_num(mpq_t r, const mpz_t n)
{ bf_set(&r[0], n);
}

static inline void
mpq_set_den(mpq_t r, const mpz_t d)
{ bf_set(&r[1], d);
}

static inline int
mpq_sgn(const mpq_t q)
{ if ( bf_is_zero(&q[0]) )
    return 0;
  return q[0].sign ? -1 : 1;
}

static inline void
mpq_abs(mpq_t r, const mpq_t q)
{ if ( r != q )
  { mpq_init(r);
    mpq_set(r, q);
  }
  r[0].sign = 0;
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
mpq_inv(mpq_t r, const mpq_t q)
{ if ( r == q )
  { mpz_swap(mpq_numref(r), mpq_denref(r));
  } else
  { bf_set(&r[0], &q[1]);
    bf_set(&r[1], &q[0]);
  }
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
int	mpq_cmp_si(const mpq_t q1, long n, unsigned long d);
int	mpq_cmp_z(const mpq_t q1, const mpz_t z2);
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

int	bf_set_randstate(gmp_randstate_t state, const mpz_t n);
void	bf_get_randstate(mpz_t n, const gmp_randstate_t state);

int	gmp_snprintf(char *BUF, size_t SIZE, const char *FMT, ...);

#endif /*BF_GMP_H_INCLUDED*/
