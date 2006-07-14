#include "tai.h"
#include "caldate.h"

void caldate_frommjd(struct caldate *cd, int64_t day, int *pwday, int *pyday)
{
  long year;
  long month;
  int yday;

  year = day / 146097LL;
  day %= 146097LL;
  day += 678881LL;
  while (day >= 146097LL) { day -= 146097LL; ++year; }

  /* year * 146097 + day - 678881 is MJD; 0 <= day < 146097 */
  /* 2000-03-01, MJD 51604, is year 5, day 0 */

  if (pwday) *pwday = (day + 3) % 7;

  year *= 4;
  if (day == 146096LL) { year += 3; day = 36524L; }
  else { year += day / 36524LL; day %= 36524LL; }
  year *= 25;
  year += day / 1461;
  day %= 1461;
  year *= 4;

  yday = (day < 306);
  if (day == 1460) { year += 3; day = 365; }
  else { year += day / 365; day %= 365; }
  yday += day;

  day *= 10;
  month = (day + 5) / 306;
  day = (day + 5) % 306;
  day /= 10;
  if (month >= 10) { yday -= 306; ++year; month -= 10; }
  else { yday += 59; month += 2; }

  cd->year = year;
  cd->month = month + 1;
  cd->day = day + 1;

  if (pyday) *pyday = yday;
}
