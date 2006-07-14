#include "tai.h"
#include "caldate.h"

void caldate_frommjd(struct caldate *cd, int64_t day, int *pwday, int *pyday)
{
  long year;
  long month;
  int yday;

  year = (long)(day / LL(146097));
  day %= LL(146097);
  day += LL(678881);
  while (day >= LL(146097)) { day -= LL(146097); ++year; }

  /* year * 146097 + day - 678881 is MJD; 0 <= day < 146097 */
  /* 2000-03-01, MJD 51604, is year 5, day 0 */

  if (pwday) *pwday = (int)((day + 3) % 7);

  year *= 4;
  if (day == LL(146096)) { year += 3; day = 36524L; }
  else { year += (long)(day / LL(36524)); day %= LL(36524); }
  year *= 25;
  year += (long)(day / 1461);
  day %= 1461;
  year *= 4;

  yday = (day < 306);
  if (day == 1460) { year += 3; day = 365; }
  else { year += (long)(day / 365); day %= 365; }
  yday += (long)day;

  day *= 10;
  month = (long)((day + 5) / 306);
  day = (day + 5) % 306;
  day /= 10;
  if (month >= 10) { yday -= 306; ++year; month -= 10; }
  else { yday += 59; month += 2; }

  cd->year = year;
  cd->month = month + 1;
  cd->day = (long)(day + 1);

  if (pyday) *pyday = yday;
}
