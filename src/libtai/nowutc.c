#include <stdio.h>
#include <stdlib.h>
#include "tai.h"
#include "leapsecs.h"
#include "taia.h"
#include "caltime.h"

struct taia now;
struct tai sec;
struct caltime ct;

char x[TAIA_FMTFRAC];

int
main(int argc, char **argv)
{
  if (leapsecs_init() == -1) {
    fprintf(stderr,"utcnow: fatal: unable to init leapsecs\n");
    exit(111);
  }

  taia_now(&now);
  x[taia_fmtfrac(x,&now)] = 0;

  taia_tai(&now,&sec);
  caltime_utc(&ct,&sec,(int *) 0,(int *) 0);

  printf("%ld-%02d-%02d %02d:%02d:%02d.%s\n"
    ,ct.date.year
    ,ct.date.month
    ,ct.date.day
    ,ct.hour
    ,ct.minute
    ,ct.second
    ,x
    );

  exit(0);
}
