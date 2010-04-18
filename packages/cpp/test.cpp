/*  $Id$

    Part of SWI-Prolog

    This example code is in the public domain
*/

#define PROLOG_MODULE "user"
#include "SWI-cpp.h"
#include <iostream>
#include <math.h>
using namespace std;

PREDICATE(hello, 1)
{ cout << "Hello " << (char *)A1 << endl;

  return TRUE;
}


PREDICATE(add, 3)
{ return A3 = (long)A1 + (long)A2;
}


PREDICATE(name_arity, 1)
{ cout << "name = " << A1.name() << ", arity = " << A1.arity() << endl;

  return TRUE;
}


PREDICATE(list_modules, 0)
{ PlTermv av(1);

  PlQuery q("current_module", av);
  while( q.next_solution() )
    cout << (char *)av[0] << endl;

  return TRUE;
}


PREDICATE(average, 3)			/* average(+Templ, :Goal, -Average) */
{ long sum = 0;
  long n = 0;

  PlTermv av(A2);			/* could be inlined in next call */
  PlQuery q("call", av);		/* but MSVC has a bug here */
  while( q.next_solution() )
  { sum += (long)A1;
    n++;
  }
  return A3 = (double)sum/(double)n;
}


PREDICATE(hello, 0)
{ PlQuery q("write", PlTermv("hello world\n"));
  return q.next_solution();
}


PREDICATE(term, 1)
{ return A1 = PlCompound("hello", PlTermv("world"));
}

PlAtom ATOM_atom("atom");

PREDICATE(term, 2)
{ PlAtom a(A1);

  if ( a == ATOM_atom )
    return A2 = PlTerm("hello world");
  if ( A1 == "string" )
    return A2 = PlString("hello world");
  if ( A1 == "code_list" )
    return A2 = PlCodeList("hello world");
  if ( A1 == "char_list" )
    return A2 = PlCharList("hello world");
  if ( A1 == "term" )
    return A2 = PlCompound("hello(world)");

  throw PlDomainError("type", A1);
}


PREDICATE(can_unify, 2)
{ PlFrame fr;

  int rval = (A1=A2);
  fr.rewind();
  return rval;
}


PREDICATE(write_list, 1)
{ PlTail tail(A1);
  PlTerm e;

  while(tail.next(e))
    cout << (char *)e << endl;

  return TRUE;
}


PREDICATE(cappend, 3)
{ PlTail l1(A1);
  PlTail l3(A3);
  PlTerm e;

  while(l1.next(e))
  { if ( !l3.append(e) )
      return FALSE;
  }

  return A2 = l3;
}


PREDICATE(call_atom, 1)
{ try
  { return PlCall((char *)A1);
  } catch ( PlTypeError &ex )
  { cerr << "Type Error caugth in C++" << endl;
    cerr << "Message: \"" << (char *)ex << "\"" << endl;
    return FALSE;
  }
}


/* The purpose of this predicate is mostly to show that
   resource errors are dealt with appropriately: with large
   enough argument, this will overflow the stacks.  The Prolog
   error is mapped to a C++ exception and back again when
   control is passed back to Prolog.  So this is just fine:

   ?- square_roots(1000000000, L)
   ERROR: Out of global stack
*/

PREDICATE(square_roots, 2)
{ int end = A1;
  PlTail list(A2);

  for(int i=0; i<end; i++)
    list.append(sqrt((double)i));

  return list.close();
}
