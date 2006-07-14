#include "taia.h"

void taia_tai(struct taia *ta, struct tai *t)
{
  *t = ta->sec;
}
