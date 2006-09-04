#include "tai.h"

void tai_add(struct tai *t, struct tai *u, struct tai *v)
{
  t->x = u->x + v->x;
}
