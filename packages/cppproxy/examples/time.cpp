#include "SWI-proxy.cpp"
#include <sys/time.h>

#ifndef PORT
#define PORT 4224
#endif

#include "time_proxy.h"

static double
now()
{ struct timeval tv;

  gettimeofday(&tv, NULL);
  return (double)tv.tv_sec + (double)tv.tv_usec/1000000.0;
}


static void
time_test(TimeProxy *proxy, long n)
{ double t0 = now();

  try
  { for(long i=0; i<n; i++)
    { long i;
      proxy->version(i);
    }
  } catch ( PlException &ex )
  { cerr << ex << endl;
  }

  double(t1) = now();
  cout << "time_test(" << n << "): " << t1-t0 << endl;
}


static void
time_test_nondet(TimeProxy *proxy, long n)
{ double t0 = now();

  try
  { between q(proxy);
    long x;
    while(q.next_solution(1,n,x))
      ;
  } catch ( PlException &ex )
  { cerr << ex << endl;
  }

  double(t1) = now();
  cout << "time_test_nondet(" << n << "): " << t1-t0 << endl;
}


int
main(int argc, char **argv)
{ TimeProxy proxy("localhost", PORT);
  long n = 1000;

  if ( argc == 2 )
    n = atoi(argv[1]);

  cout << "Timing test: " << n << " runs" << endl;

  time_test(&proxy, 1000);
  time_test_nondet(&proxy, 1000);

  return 0;
}
