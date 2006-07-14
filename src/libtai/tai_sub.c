#include "tai.h"

void tai_sub(struct tai *t, struct tai *u, struct tai *v)
{
  t->x = u->x - v->x;
}
