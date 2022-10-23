#ifndef BF_GMP_TYPES_H_INCLUDED
#define BF_GMP_TYPES_H_INCLUDED

#include "libbf.h"

typedef bf_t MP_INT;
typedef bf_t mpz_t[1];
typedef bf_t mpq_t[2];			/* Numerator/Denumerator */
typedef bf_t mpf_t[1];
typedef limb_t mp_limb_t;
typedef limb_t mp_bitcnt_t;
typedef int gmp_randstate_t;		/* dummy */

#endif /*BF_GMP_TYPES_H_INCLUDED*/
