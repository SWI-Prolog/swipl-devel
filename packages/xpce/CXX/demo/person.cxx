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
#include <pce/Pointer.h>
#include <iostream.h>
#include <stdio.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This appliction show how  a  C++   object  that  defines person::edit(),
exploiting XPCE behaviour to modify the C++ object.

XPCE cannot call C++ memberfunctions directly, so a wrapper is needed to
make this work.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

class person
{ 
public:
  char *name;
  int   age;

  person(char *nm, int a)
  { name = nm;
    age = a;
  }

  void setName(char *a) { name = a; }
  void setAge(int a)  { age  = a; }
  void edit();
};

static PceStatus
setNamePerson(PceArg person, PceArg name)
{ class person *p = (class person *)person.pointer();

  p->setName(name);

  return SUCCEED;
}

static PceStatus
setAgePerson(PceArg person, PceArg age)
{ class person *p = (class person *)person.pointer();

  p->setAge(age);

  return SUCCEED;
}

static PceStatus
printPerson(PceArg person)
{ class person *p = (class person *)person.pointer();

  cout << p->name << " is " << p->age << " years old" << endl;
  return SUCCEED;
}


void
person::edit()
{ PceDialog d(PceObject("string", "Edit person"));

  d.send("append",
	 PceTextItem("name", name,
		     PceCall(setNamePerson, PcePointer(this), TheArg1)));
  d.send("append",
	 PceTextItem("age", age,
		     PceCall(setAgePerson, PcePointer(this), TheArg1)));
  d.send("append",
	 PceButton("quit",
		   PceAnd(PceMessage(d, "apply"),
			  PceCall(printPerson, PcePointer(this)),
			  PceMessage(d, "destroy"))));

  d.send("open");
}


PceStatus
pceInitApplication(int argc, char *argv[])
{ person *p = new person ("gnat", 527);

  p->edit();

  return SUCCEED;
}
