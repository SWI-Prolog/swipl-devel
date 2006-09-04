#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "tai.h"
#include "leapsecs.h"
#include "caltime.h"

char line[100];

char *dayname[7] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" } ;

char out[101];
char x[TAI_PACK];

int
main(int argc, char **argv)
{
  struct tai t;
  struct tai t2;
  struct caltime ct;
  struct caltime ct2;
  int weekday;
  int yearday;
  int i;
  double packerr;

  if (leapsecs_init() == -1)
    printf("unable to init leapsecs\n");

  while (fgets(line,sizeof line,stdin))
    if (!caltime_scan(line,&ct))
      printf("unable to parse\n");
    else {
      caltime_tai(&ct,&t);
      caltime_utc(&ct2,&t,&weekday,&yearday);
      tai_pack(x,&t);
      tai_unpack(x,&t2);
      tai_sub(&t2,&t2,&t);
      packerr = tai_approx(&t2);
      for (i = 0;i < TAI_PACK;++i)
        printf("%2.2lx",(unsigned long) (unsigned char) x[i]);
      if (packerr)
        printf(" packerr=%f",packerr);
      printf(" %03d  %s",yearday,dayname[weekday]);
      if (caltime_fmt((char *) 0,&ct2) + 1 < sizeof out) {
        out[caltime_fmt(out,&ct2)] = 0;
        printf(" %s",out);
      }
      printf("\n");
    }
  exit(0);
}
