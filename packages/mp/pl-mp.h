#include "gmp.h"
#include <SWI-Prolog.h>

/* if GMP multiplication proves to be more expensive than addition,
   it is wise to enable FAST_COMPLEX_MUL */
#define FAST_COMPLEX_MUL

#define max(n, m) (n >= m ? n : m)
#define abs(n) (n >= 0 ? n : -n)

#define MPE (0)	/* error: not an MP type */
#define MPZ (1)	/* MP integer */
#define MPQ (2)	/* MP rational */
#define MPF (3)	/* MP float */
#define MPC (4)	/* MP complex */
#define MP0 (5)	/* MP division by zero */

typedef int mp_tp;	/* MP types as defined above */

typedef struct {
	mpf_t r;
	mpf_t i;
} mpc_t;		/* MP complex number */

typedef struct {
	mp_tp t;
	union {
		mpz_t z;	/* GMP integer */
		mpq_t q;	/* GMP rational */
		mpf_t f;	/* GMP float */
		mpc_t c;	/* complex */
	} n;
} mp_t;			/* MP number */

void mp_install();

extern PL_extension mp_predicates[];

