#include "cppapi.h"
#include <iostream.h>

int
main(int argc, char **argv)
{ PlEngine e(argv[0]);
  PlTermv av(1);
  PlTail l(av[0]);

  for(int i=0; i<argc; i++)
    l.append(argv[i]);
  l.close();

  PlQuery q("entry", av);
  try
  { return q.next_solution() ? 0 : 1;
  } catch ( PlException &ex )
  { cerr << (char *)ex << endl;
  }
}
