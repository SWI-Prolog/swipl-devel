#include "tai.h"
#include "leapsecs.h"

/* XXX: breaks tai encapsulation */

int leapsecs_sub(struct tai *t)
{
  int i;
  uint64_t u;
  int s;

  if (leapsecs_init() == -1) return 0;

  u = t->x;
  s = 0;

  for (i = 0;i < leapsecs_num;++i) {
    if (u < leapsecs[i].x) break;
    ++s;
    if (u == leapsecs[i].x) { t->x = u - s; return 1; }
  }

  t->x = u - s;
  return 0;
}
