#include "tai.h"

void tai_pack(char *s, struct tai *t)
{
  uint64_t x;

  x = t->x;
  /* JW: Cast to int need to get MSVC6 silent */
  s[7] = (char)(x & 255); x >>= 8;
  s[6] = (char)(x & 255); x >>= 8;
  s[5] = (char)(x & 255); x >>= 8;
  s[4] = (char)(x & 255); x >>= 8;
  s[3] = (char)(x & 255); x >>= 8;
  s[2] = (char)(x & 255); x >>= 8;
  s[1] = (char)(x & 255); x >>= 8;
  s[0] = (char)x;
}
