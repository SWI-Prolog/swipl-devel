#include "taia.h"

unsigned int taia_fmtfrac(char *s, struct taia *t)
{
  unsigned long x;

  if (s) {
    x = t->atto;
    s[17] = '0' + (int)(x % 10); x /= 10;
    s[16] = '0' + (int)(x % 10); x /= 10;
    s[15] = '0' + (int)(x % 10); x /= 10;
    s[14] = '0' + (int)(x % 10); x /= 10;
    s[13] = '0' + (int)(x % 10); x /= 10;
    s[12] = '0' + (int)(x % 10); x /= 10;
    s[11] = '0' + (int)(x % 10); x /= 10;
    s[10] = '0' + (int)(x % 10); x /= 10;
    s[9] = '0' + (int)(x % 10);
    x = t->nano;
    s[8] = '0' + (int)(x % 10); x /= 10;
    s[7] = '0' + (int)(x % 10); x /= 10;
    s[6] = '0' + (int)(x % 10); x /= 10;
    s[5] = '0' + (int)(x % 10); x /= 10;
    s[4] = '0' + (int)(x % 10); x /= 10;
    s[3] = '0' + (int)(x % 10); x /= 10;
    s[2] = '0' + (int)(x % 10); x /= 10;
    s[1] = '0' + (int)(x % 10); x /= 10;
    s[0] = '0' + (int)(x % 10);
  }

  return 18;
}
