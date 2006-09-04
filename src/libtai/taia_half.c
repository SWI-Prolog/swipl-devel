#include "taia.h"

/* XXX: breaks tai encapsulation */

void taia_half(struct taia *t, struct taia *u)
{
  t->atto = u->atto >> 1;
  if (u->nano & 1) t->atto += 500000000UL;
  t->nano = u->nano >> 1;
  if (u->sec.x & 1) t->nano += 500000000UL;
  t->sec.x = u->sec.x >> 1;
}
