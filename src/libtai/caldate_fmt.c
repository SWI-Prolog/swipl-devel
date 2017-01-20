#include "tai.h"
#include "caldate.h"

unsigned int caldate_fmt(char *s, struct caldate *cd)
{
  long x;
  int i = 0;

  x = cd->year; if (x < 0) x = -x; do { ++i; x /= 10; } while(x);

#define NEXTDIGIT(x) (char)('0'+(x%10)); x /= 10
  if (s) {
    x = cd->year;
    if (x < 0) { x = -x; *s++ = '-'; }
    s += i; do { *--s = NEXTDIGIT(x); } while(x); s += i;

    x = cd->month;
    s[0] = '-'; s[2] = NEXTDIGIT(x); s[1] = NEXTDIGIT(x);

    x = cd->day;
    s[3] = '-'; s[5] = NEXTDIGIT(x); s[4] = NEXTDIGIT(x);
  }

  return (cd->year < 0) + i + 6;
}
