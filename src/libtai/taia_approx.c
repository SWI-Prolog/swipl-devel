#include "taia.h"

double taia_approx(struct taia *t)
{ return tai_approx(&t->sec) + taia_frac(t);
}
