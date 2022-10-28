/*
 * The Mersenne Twister pseudo-random number generator (PRNG)
 *
 * This is an implementation of fast PRNG called MT19937, meaning it has a
 * period of 2^19937-1, which is a Mersenne prime.
 *
 * This PRNG is fast and suitable for non-cryptographic code.  For instance, it
 * would be perfect for Monte Carlo simulations, etc.
 *
 * For all the details on this algorithm, see the original paper:
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/mt.pdf
 *
 * Written by Christian Stigen Larsen
 * Distributed under the modified BSD license.
 * 2015-02-17, 2017-12-06
 */

#ifndef MERSENNE_TWISTER_H
#define MERSENNE_TWISTER_H

#define __STDC_LIMIT_MACROS
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Extract a pseudo-random unsigned 32-bit integer in the range 0 ... UINT32_MAX
 */
uint32_t rand_u32();

/*
 * Initialize Mersenne Twister with given seed value.
 */
void seed(uint32_t seed_value);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // MERSENNE_TWISTER_H
