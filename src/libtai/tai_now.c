#include <time.h>
#include "tai.h"

void tai_now(struct tai *t)
{
  t->x = ULL(4611686018427387914) + (uint64_t) time((time_t *) 0);
}
