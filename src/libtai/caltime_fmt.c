#include "tai.h"
#include "caldate.h"
#include "caltime.h"

unsigned int caltime_fmt(char *s, struct caltime *ct)
{
  unsigned int result;
  long x;

  result = caldate_fmt(s,&ct->date);

#define NEXTDIGIT(x) (char)('0' + (x % 10)); x /= 10
#define NEXT6DIGIT(x) (char)('0' + (x % 6)); x /= 6
  if (s) {
    s += result;

    x = ct->hour;
    s[0] = ' ';
    s[2] = NEXTDIGIT(x);
    s[1] = NEXTDIGIT(x);
    s += 3;

    x = ct->minute;
    s[0] = ':';
    s[2] = NEXTDIGIT(x);
    s[1] = NEXTDIGIT(x);
    s += 3;

    x = ct->second;
    s[0] = ':';
    s[2] = NEXTDIGIT(x);
    s[1] = NEXTDIGIT(x);
    s += 3;

    s[0] = ' ';
    x = ct->offset;
    if (x < 0) { s[1] = '-'; x = -x; } else s[1] = '+';

    s[5] = NEXTDIGIT(x);
    s[4] = NEXT6DIGIT(x);
    s[3] = NEXTDIGIT(x);
    s[2] = NEXTDIGIT(x);
  }

  return result + 15;
}
