#include "taia.h"

double taia_frac(struct taia *t)
{
  return ((double)t->atto * 0.000000001 + (double)t->nano) * 0.000000001;
}
