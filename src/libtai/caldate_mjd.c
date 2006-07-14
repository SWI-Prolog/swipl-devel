#include "tai.h"
#include "caldate.h"

static unsigned long times365[4] = { 0, 365, 730, 1095 } ;
static unsigned long times36524[4] = { 0, 36524UL, 73048UL, 109572UL } ;
static unsigned long montab[12] =
{ 0, 31, 61, 92, 122, 153, 184, 214, 245, 275, 306, 337 } ;
/* month length after february is (306 * m + 5) / 10 */

long caldate_mjd(struct caldate *cd)
{
  long y;
  long m;
  long d;

  d = cd->day - 678882L;
  m = cd->month - 1;
  y = cd->year;

  d += 146097L * (y / 400);
  y %= 400;

  if (m >= 2) m -= 2; else { m += 10; --y; }

  y += (m / 12);
  m %= 12;
  if (m < 0) { m += 12; --y; }

  d += montab[m];

  d += 146097L * (y / 400);
  y %= 400;
  if (y < 0) { y += 400; d -= 146097L; }

  d += times365[y & 3];
  y >>= 2;

  d += 1461L * (y % 25);
  y /= 25;

  d += times36524[y & 3];

  return d;
}
