#include <sys/types.h>
#include <sys/time.h>
#include "taia.h"

/* XXX: breaks tai encapsulation */

void taia_now(struct taia *t)
{
  struct timeval now;
  gettimeofday(&now,(struct timezone *) 0);
  t->sec.x = 4611686018427387914ULL + (uint64_t) now.tv_sec;
  t->nano = 1000 * now.tv_usec + 500;
  t->atto = 0;
}
