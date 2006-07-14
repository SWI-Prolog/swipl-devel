#include <stdio.h>
#include <stdlib.h>
#include "caldate.h"

char *montab[] = {
  "January"
, "February"
, "March"
, "April"
, "May"
, "June"
, "July"
, "August"
, "September"
, "October"
, "November"
, "December"
} ;

int main(int argc, char **argv)
{
  int year;
  long daystart;
  long dayend;
  long day;
  int weekday;
  struct caldate cd;

  while (*++argv) {
    year = atoi(*argv);

    cd.year = year;
    cd.month = 1;
    cd.day = 1;
    daystart = caldate_mjd(&cd);
    cd.year = year + 1;
    dayend = caldate_mjd(&cd);

    while ((daystart + 3) % 7) --daystart;
    while ((dayend + 3) % 7) ++dayend;

    for (day = daystart;day < dayend;++day) {
      caldate_frommjd(&cd,day,&weekday,(int *) 0);
      if (cd.year != year)
	printf("   ");
      else {
	if (cd.month & 1)
	  if (cd.day < 10)
	    printf(" %d%c%d ",cd.day % 10,8,cd.day % 10);
	  else
	    printf("%d%c%d%d%c%d ",cd.day / 10,8,cd.day / 10,cd.day % 10,8,cd.day % 10);
	else
	  printf("%2d ",cd.day);
        if (weekday == 6) {
	  if ((cd.day >= 15) && (cd.day < 22))
	    printf(" %s %d\n",montab[cd.month - 1],year);
	  else
	    printf("\n");
        }
      }
    }
    printf("\n");
  }

  exit(0);
}
