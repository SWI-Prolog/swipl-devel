#include "taia.h"

/* XXX: breaks tai encapsulation */

int taia_less(struct taia *t, struct taia *u)
{
  if (t->sec.x < u->sec.x) return 1;
  if (t->sec.x > u->sec.x) return 0;
  if (t->nano < u->nano) return 1;
  if (t->nano > u->nano) return 0;
  return t->atto < u->atto;
}
