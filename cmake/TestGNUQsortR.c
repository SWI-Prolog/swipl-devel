#define _GNU_SOURCE
#include <stdlib.h>

static int
mycompare(const void *p1, const void *p2, void *c)
{ const int *s1 = p1;
  const int *s2 = p2;
  int *ip = c;

  if ( *ip != 1 )
    exit(1);

  return *s1 - *s2;
}

int
main(int argc, char**argv)
{ int data[] = {0,1,2,3,4,5,6,7,8,9};
  int ctx = 1;

  qsort_r(data, 10, sizeof(int), mycompare, (void*)&ctx);
  return 0;
}
