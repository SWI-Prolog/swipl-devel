#include "taia.h"

void taia_pack(char *s, struct taia *t)
{
  unsigned long x;

  tai_pack(s,&t->sec);
  s += 8;

  x = t->atto;
  s[7] = (int)x & 255; x >>= 8;
  s[6] = (int)x & 255; x >>= 8;
  s[5] = (int)x & 255; x >>= 8;
  s[4] = (int)x;
  x = t->nano;
  s[3] = (int)x & 255; x >>= 8;
  s[2] = (int)x & 255; x >>= 8;
  s[1] = (int)x & 255; x >>= 8;
  s[0] = (int)x;
}
