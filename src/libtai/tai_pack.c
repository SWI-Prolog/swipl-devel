#include "tai.h"

void tai_pack(char *s, struct tai *t)
{
  uint64_t x;

  x = t->x;
  /* JW: Cast to int need to get MSVC6 silent */
  s[7] = (int)x & 255; x >>= 8;
  s[6] = (int)x & 255; x >>= 8;
  s[5] = (int)x & 255; x >>= 8;
  s[4] = (int)x & 255; x >>= 8;
  s[3] = (int)x & 255; x >>= 8;
  s[2] = (int)x & 255; x >>= 8;
  s[1] = (int)x & 255; x >>= 8;
  s[0] = (int)x;
}
