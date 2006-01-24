#include "SWI-proxy.cpp"

#ifndef PORT
#define PORT 4224
#endif

#include "sqrt_proxy.h"

int
main(int argc, char **argv)
{ SqrtProxy proxy("localhost", PORT);

  try
  { between q(&proxy);
    long l = 1;
    long h = 100;
    long i;

    while ( q.next_solution(l, h, i) )
    { double ifloat = (double)i;
      double rval;

      proxy.sqrt(ifloat, rval);
      cout << "sqrt(" << i << ") = " << rval << endl;
    }
  } catch ( PlException &ex )
  { cerr << ex;
  }

  return 0;
}
