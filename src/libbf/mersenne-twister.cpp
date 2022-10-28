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

#include <stdio.h>
#include "mersenne-twister.h"

// Better on older Intel Core i7, but worse on newer Intel Xeon CPUs (undefine
// it on those).
//#define MT_UNROLL_MORE

/*
 * We have an array of 624 32-bit values, and there are 31 unused bits, so we
 * have a seed value of 624*32-31 = 19937 bits.
 */
static const size_t SIZE   = 624;
static const size_t PERIOD = 397;
static const size_t DIFF   = SIZE - PERIOD;

static const uint32_t MAGIC = 0x9908b0df;

// State for a singleton Mersenne Twister. If you want to make this into a
// class, these are what you need to isolate.
struct MTState {
  uint32_t MT[SIZE];
  uint32_t MT_TEMPERED[SIZE];
  size_t index = SIZE;
};

static MTState state;

#define M32(x) (0x80000000 & x) // 32nd MSB
#define L31(x) (0x7FFFFFFF & x) // 31 LSBs

#define UNROLL(expr) \
  y = M32(state.MT[i]) | L31(state.MT[i+1]); \
  state.MT[i] = state.MT[expr] ^ (y >> 1) ^ (((int32_t(y) << 31) >> 31) & MAGIC); \
  ++i;

static void generate_numbers()
{
  /*
   * For performance reasons, we've unrolled the loop three times, thus
   * mitigating the need for any modulus operations. Anyway, it seems this
   * trick is old hat: http://www.quadibloc.com/crypto/co4814.htm
   */

  size_t i = 0;
  uint32_t y;

  // i = [0 ... 226]
  while ( i < DIFF ) {
    /*
     * We're doing 226 = 113*2, an even number of steps, so we can safely
     * unroll one more step here for speed:
     */
    UNROLL(i+PERIOD);

#ifdef MT_UNROLL_MORE
    UNROLL(i+PERIOD);
#endif
  }

  // i = [227 ... 622]
  while ( i < SIZE -1 ) {
    /*
     * 623-227 = 396 = 2*2*3*3*11, so we can unroll this loop in any number
     * that evenly divides 396 (2, 4, 6, etc). Here we'll unroll 11 times.
     */
    UNROLL(i-DIFF);

#ifdef MT_UNROLL_MORE
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
    UNROLL(i-DIFF);
#endif
  }

  {
    // i = 623, last step rolls over
    y = M32(state.MT[SIZE-1]) | L31(state.MT[0]);
    state.MT[SIZE-1] = state.MT[PERIOD-1] ^ (y >> 1) ^ (((int32_t(y) << 31) >>
          31) & MAGIC);
  }

  // Temper all numbers in a batch
  for (size_t i = 0; i < SIZE; ++i) {
    y = state.MT[i];
    y ^= y >> 11;
    y ^= y << 7  & 0x9d2c5680;
    y ^= y << 15 & 0xefc60000;
    y ^= y >> 18;
    state.MT_TEMPERED[i] = y;
  }

  state.index = 0;
}

extern "C" void seed(uint32_t value)
{
  /*
   * The equation below is a linear congruential generator (LCG), one of the
   * oldest known pseudo-random number generator algorithms, in the form
   * X_(n+1) = = (a*X_n + c) (mod m).
   *
   * We've implicitly got m=32 (mask + word size of 32 bits), so there is no
   * need to explicitly use modulus.
   *
   * What is interesting is the multiplier a.  The one we have below is
   * 0x6c07865 --- 1812433253 in decimal, and is called the Borosh-Niederreiter
   * multiplier for modulus 2^32.
   *
   * It is mentioned in passing in Knuth's THE ART OF COMPUTER
   * PROGRAMMING, Volume 2, page 106, Table 1, line 13.  LCGs are
   * treated in the same book, pp. 10-26
   *
   * You can read the original paper by Borosh and Niederreiter as well.  It's
   * called OPTIMAL MULTIPLIERS FOR PSEUDO-RANDOM NUMBER GENERATION BY THE
   * LINEAR CONGRUENTIAL METHOD (1983) at
   * http://www.springerlink.com/content/n7765ku70w8857l7/
   *
   * You can read about LCGs at:
   * http://en.wikipedia.org/wiki/Linear_congruential_generator
   *
   * From that page, it says: "A common Mersenne twister implementation,
   * interestingly enough, uses an LCG to generate seed data.",
   *
   * Since we're using 32-bits data types for our MT array, we can skip the
   * masking with 0xFFFFFFFF below.
   */

  state.MT[0] = value;
  state.index = SIZE;

  for ( uint_fast32_t i=1; i<SIZE; ++i )
    state.MT[i] = 0x6c078965*(state.MT[i-1] ^ state.MT[i-1]>>30) + i;
}

extern "C" uint32_t rand_u32()
{
  if ( state.index == SIZE ) {
    generate_numbers();
    state.index = 0;
  }

  return state.MT_TEMPERED[state.index++];
}
