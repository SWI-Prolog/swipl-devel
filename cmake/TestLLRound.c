#include <math.h>

float d = 0.5;

int
main(int argc, char **argv)
{ if ( llround(nexttoward(d, -10)) == 0 )
    return 0;
  return 1;
}
