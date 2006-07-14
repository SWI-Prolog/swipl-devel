#ifndef CALTIME_H
#define CALTIME_H

#include "caldate.h"

struct caltime {
  struct caldate date;
  int hour;
  int minute;
  int second;
  long offset;
} ;

extern void caltime_tai(struct caltime *ct, struct tai *t);
extern void caltime_utc(struct caltime *ct, struct tai *t, int *pwday, int *pyday);

extern unsigned int caltime_fmt(char *s, struct caltime *ct);
extern unsigned int caltime_scan(char *s, struct caltime *ct);

#endif
