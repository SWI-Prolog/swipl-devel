#include <math.h>

double d = -0.0;
double i;

int
main(int argc, char **argv)
{ 
  /* IEEE754: check if the fractional part of -0.0 is negative */
  if ( copysign(1.0, modf(d, &i)) == -1.0 )
    return 0;

  /* positive (violating the standard) */
  return 1;
}
