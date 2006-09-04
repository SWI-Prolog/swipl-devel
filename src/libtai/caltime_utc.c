#include "tai.h"
#include "leapsecs.h"
#include "caldate.h"
#include "caltime.h"

/* XXX: breaks tai encapsulation */

void caltime_utc(struct caltime *ct, struct tai *t, int *pwday, int *pyday)
{
  struct tai t2 = *t;
  uint64_t u;
  int leap;
  long s;

  /* XXX: check for overfow? */

  leap = leapsecs_sub(&t2);
  u = t2.x;

  u += 58486;
  s = (long)(u % ULL(86400));

  ct->second = (s % 60) + leap; s /= 60;
  ct->minute = s % 60; s /= 60;
  ct->hour = s;

  u /= ULL(86400);
  caldate_frommjd(&ct->date, (int64_t)u - LL(53375995543064), pwday, pyday);

  ct->offset = 0;
}
