#include "tai.h"
#include "leapsecs.h"

static int flaginit = 0;

int leapsecs_init(void)
{
  if (flaginit) return 0;
  if (leapsecs_read("/etc/leapsecs.dat") == -1) return -1;
  flaginit = 1;
  return 0;
}
