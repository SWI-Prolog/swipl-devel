/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amsterdam. All rights reserved.
*/


#include "SWI-cpp.h"
#include <iostream.h>

/* Usage:

   likes			prints who likes what
   likes x			prints what is liked by x
   likes x y			Test whether x likes y
   likes -happy			See who is happy

   Compile using:

   plld -o likes -ld g++ -goal true likes.cpp likes.pl
*/

int
body(int argc, char **argv)
{ if ( argc == 1 )
  { if ( strcmp(argv[0], "-happy") == 0 )
    { PlTermv av(1);			/* likes - happy */

      cout << "Happy people:" << endl;
      PlQuery q("happy", av);
      while( q.next_solution() )
	cout << "\t" << (char *)av[0] << endl;
    } else
    { PlTermv av(2);

      cout << argv[0] << " likes:" << endl;
      av[0] = argv[0];
      PlQuery q("likes", av);
      while( q.next_solution() )
	cout << "\t" << (char *)av[1] << endl;
    }
  } else if ( argc == 2 )
  { PlTermv av(2);

    av[0] = argv[0];
    av[1] = argv[1];
    if ( PlCall("likes", av) )
      cout << "yes" << endl;
    else
      cout << "no" << endl;
  } else
    cout << "Usage: likes x [y] or likes -happy" << endl;

  return 0;
}


int
main(int argc, char **argv)
{ PlEngine e(argv[0]);

  try 
  { return body(argc-1, argv+1);
  } catch ( PlException &ex )
  { cerr << (char *) ex << endl;
    exit(1);
  }
}
 

