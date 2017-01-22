#include "taia.h"

unsigned int taia_fmtfrac(char *s, struct taia *t)
{
  unsigned long x;

#define NEXTDIGIT(x) (char)('0' + (x % 10)); x /= 10
  if (s) {
    x = t->atto;
    s[17] = NEXTDIGIT(x);
    s[16] = NEXTDIGIT(x);
    s[15] = NEXTDIGIT(x);
    s[14] = NEXTDIGIT(x);
    s[13] = NEXTDIGIT(x);
    s[12] = NEXTDIGIT(x);
    s[11] = NEXTDIGIT(x);
    s[10] = NEXTDIGIT(x);
    s[9]  = NEXTDIGIT(x);
    x = t->nano;
    s[8] = NEXTDIGIT(x);
    s[7] = NEXTDIGIT(x);
    s[6] = NEXTDIGIT(x);
    s[5] = NEXTDIGIT(x);
    s[4] = NEXTDIGIT(x);
    s[3] = NEXTDIGIT(x);
    s[2] = NEXTDIGIT(x);
    s[1] = NEXTDIGIT(x);
    s[0] = NEXTDIGIT(x);
  }

  return 18;
}
