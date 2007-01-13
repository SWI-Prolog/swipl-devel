#include "tai.h"
#include "caltime.h"

unsigned int caltime_scan(char *s, struct caltime *ct)
{
  char *t = s;
  unsigned long z;
  unsigned long c;
  int sign;

  t += caldate_scan(t,&ct->date);

  while ((*t == ' ') || (*t == '\t') || (*t == 'T')) ++t;
  z = 0; while ((c = (unsigned char) (*t - '0')) <= 9) { z = z * 10 + c; ++t; }
  ct->hour = z;

  if (*t++ != ':') return 0;
  z = 0; while ((c = (unsigned char) (*t - '0')) <= 9) { z = z * 10 + c; ++t; }
  ct->minute = z;

  if (*t != ':')
    ct->second = 0;
  else {
    ++t;
    z = 0; while ((c = (unsigned char) (*t - '0')) <= 9) { z = z * 10 + c; ++t; }
    ct->second = z;
  }

  while ((*t == ' ') || (*t == '\t')) ++t;
  if (*t == '+') sign = 1; else if (*t == '-') sign = -1; else return 0;
  ++t;
  c = (unsigned char) (*t++ - '0'); if (c > 9) return 0; z = c;
  c = (unsigned char) (*t++ - '0'); if (c > 9) return 0; z = z * 10 + c;
  c = (unsigned char) (*t++ - '0'); if (c > 9) return 0; z = z * 6 + c;
  c = (unsigned char) (*t++ - '0'); if (c > 9) return 0; z = z * 10 + c;
  ct->offset = z * sign;

  return (unsigned int)(t - s);
}
