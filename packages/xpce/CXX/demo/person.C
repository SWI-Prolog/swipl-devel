/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/And.h>
#include <pce/Button.h>
#include <pce/Call.h>
#include <pce/Dialog.h>
#include <pce/Message.h>
#include <pce/TextItem.h>
#include <iostream.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This appliction show how a real C++  object   can  use XPCE and have its
native  C++  methods  called  directly  from   XPCE  objects  using  the
PceMethodCall mechanism.

A C++ method can only be called  if   it  returns  a PceStatus and takes
PceArg arguments. The pointer to the implementation is passed as a (void
*), because full typing is impossible as we   donot know the type of the
receiver. This only works because

	PceStatus (Class::*)(PceArg, ...)

is supposed to be the same as

	PceStatus (*)(void *this, PceArg, ...)

E.i. the receiver is simply pushes  as   first  argument to an otherwise
normal C-function. This is  true  for  GCC,   but  not  part  of the C++
standard definition. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

class person
{ char *name;
  int   age;

public:
  person(char *nm, int a)
  { name = nm;
    age = a;
  }

  PceStatus setName(PceArg a) { name = a; return SUCCEED; }
  PceStatus setAge(PceArg a)  { age  = a; return SUCCEED; }
  PceStatus print() { cout << "Person " << name << ", age " << age << endl; }


  edit()
  { PceDialog d(PceObject("string", "Edit person"));

    d.send("append",
	   PceTextItem("name", name,
		       PceMethodCall(this, (void *)&setName, TheArg1)));
    d.send("append",
	   PceTextItem("age", age,
		       PceMethodCall(this, (void *)&setAge, TheArg1)));
    d.send("append",
	   PceButton("quit",
		     PceAnd(PceMessage(d, "apply"),
			    PceMethodCall(this, (void *)&print),
			    PceMessage(d, "destroy"))));

    d.send("open");
  }
};


PceStatus
pceInitApplication(int argc, char *argv[])
{ person *p = new person ("gnat", 527);

  p->edit();

  return SUCCEED;
}
