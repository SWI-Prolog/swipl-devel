#include "../SWI-proxy.cpp"

#ifndef PORT
#define PORT 4224
#endif

class person
{ 
public:
  char *first_name;
  char *last_name;
  int age;

  person()
  { first_name = NULL;
    last_name = NULL;
    age = -1;
  };
  ~person()
  { if ( first_name ) free(first_name);
    if ( last_name ) free(last_name);
  }

  char *get_first_name() const { return first_name; }
  char *get_last_name() const  { return last_name; }
  long  get_age() const  { return age; }

  void initialize(string fn, string ln, long years)
  { if ( first_name ) free(first_name);
    if ( last_name  ) free(last_name);

    first_name = strdup(fn.c_str());
    last_name  = strdup(ln.c_str());
    age = years;
  }
};

#include "person_proxy.h"

int
main(int argc, char **argv)
{ PersonProxy proxy("localhost", PORT);
  long maxage;
  
  if ( argc == 2 )
  { maxage = atoi(argv[1]);
  } else
  { maxage = 50;
  }

  try
  { person p;
    p.first_name = "Jan";
    p.last_name = "Wielemaker";
    p.age = 45;

    proxy.add_person(p);
  } catch ( PlException &ex )
  { cerr << ex << endl;
    return 1;
  }

  try
  { find_person_younger_than q(&proxy);
    person p;

    while( q.next_solution(maxage, p) )
      cout << p.first_name << " " << p.last_name << " " << p.age << endl;
  } catch ( PlException &ex )
  { cerr << ex << endl;
    return 1;
  }

  return 0;
}
