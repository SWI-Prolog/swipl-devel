#include "tai.h"
#include "caldate.h"

void caldate_easter(struct caldate *cd)
{
  long c;
  long t;
  long j;
  long n;
  long y;

  y = cd->year;

  c = (y / 100) + 1;
  t = 210 - (((c * 3) / 4) % 210);
  j = y % 19;
  n = 57 - ((14 + j * 11 + (c * 8 + 5) / 25 + t) % 30);
  if ((n == 56) && (j > 10)) --n;
  if (n == 57) --n;
  n -= ((((y % 28) * 5) / 4 + t + n + 2) % 7);

  if (n < 32) { cd->month = 3; cd->day = n; }
  else { cd->month = 4; cd->day = n - 31; }
}
