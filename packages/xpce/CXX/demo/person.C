/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Call.h>
#include <pce/String.h>
#include <pce/Dialog.h>
#include <pce/TextItem.h>
#include <pce/Button.h>
#include <pce/Message.h>

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

  edit()
  { PceDialog d(PceObject("string", "Edit person"));

    d.send("append",
	   PceTextItem("name", name,
		       PceMethodCall(this, &setName, TheArg1)));
    d.send("append",
	   PceTextItem("age", age,
		       PceMethodCall(this, &setAge, TheArg1)));
    d.send("append", PceButton("quit", PceMessage(d, "destroy")));

    d.send("open");
  }
};


PceStatus
pceInitApplication(int argc, char *argv[])
{ person *p = new person ("gnat", 527);

  p->edit();

  return SUCCEED;
}
