#include "tai.h"
#include "leapsecs.h"

/* XXX: breaks tai encapsulation */

void leapsecs_add(struct tai *t, int hit)
{
  int i;
  uint64_t u;

  if (leapsecs_init() == -1) return;

  u = t->x;

  for (i = 0;i < leapsecs_num;++i) {
    if (u < leapsecs[i].x) break;
    if (!hit || (u > leapsecs[i].x)) ++u;
  }

  t->x = u;
}
