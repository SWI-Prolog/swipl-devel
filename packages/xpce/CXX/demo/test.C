/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <pce/Pce.h>
#include <pce/Call.h>
#include <pce/Class.h>
#include <pce/And.h>
#include <pce/Button.h>
#include <pce/Dialog.h>
#include <pce/Menu.h>
#include <pce/Message.h>
#include <pce/TextItem.h>
#include <pce/String.h>

PceStatus
hello(PceArg text)
{ TheDisplay.send("inform",
		  PceString("I am XPCE version %s \n"
			    "And I have %d classes"),
		  ThePce.get("version"),
		  PceGlobal("classes").get("size"));  // G++

  return TRUE;
}


class person
{ char *name;
  char *sex;
  int  age;

public:
  person(char *n, char *s, int a)
  { name = n;
    sex = s;
    age = a;
  }

  PceStatus setName(PceArg a) 	{ name = a; return TRUE; }
  PceStatus setSex(PceArg a)	{ sex = a; return TRUE; }
  PceStatus setAge(PceArg a)	{ age = a; return TRUE; }

  PceStatus print(void)
  { TheDisplay.send("inform", "Name: %s; sex: %s, age: %s", name, sex, age);

    return TRUE;
  }

  PceStatus makeDialog(void)
  { printf("Making dialog ... \n");
    PceDialog d;
    PceObject m;
    
    d.send("append", PceTextItem("name", name,
				 PceMethodCall(this, &person::setName, TheArg1)));
    d.send("append",
	   m = PceMenu("sex", "cycle",
		       PceMethodCall(this, &person::setSex, TheArg1)));
    m.send("append", "male");
    m.send("append", "female");
    m.send("selection", sex);
    d.send("append", PceTextItem("age",  age,
				 PceMethodCall(this, &person::setAge, TheArg1)));
    d.send("append", PceButton(PceArg("apply")));
    d.send("append", PceButton("print", PceMethodCall(this, &person::print)));
    d.send("append", PceButton("hello", PceCall(hello, "gnu")));
    d.send("append", PceButton("quit", PceMessage(d, "destroy")));
    d.send("default_button", "apply");
    d.send("open");

    return TRUE;
  }
};


		 /*******************************
		 *	     CLASSES		*
		 *******************************/

static PceVariable *name_slot;

static PceStatus
nameNamed(PceReceiver n, PceArg value)
{ n.store(name_slot, value);

  return TRUE;
}


static PceStatus
benchNamed(PceReceiver n, PceArg times)
{ printf("%d times\n", (int) times);

  while( --times >= 0 )
    n.send("pce_succeed");
  
  return TRUE;
}


static PceStatus
pceSucceedNamed(PceReceiver n)
{ return ThePce.send("succeed");
}


static PceStatus
succeedNamed(PceReceiver n)
{ return TRUE;
}


static PceStatus
succeed1Named(PceReceiver n, PceArg a1)
{ return TRUE;
}


static PceStatus
vaSucceedNamed(PceReceiver n, PceArg args)
{ return TRUE;
}


static PceArg
getLoopNamed(PceReceiver n, PceArg times)
{ int done = 0;

  while(times-- > 0)
    done++;

  return done;
}


static PceArg
getQuickLoopNamed(PceReceiver n, PceArg times)
{ int done = 0;
  int t = times;

  while(t-- > 0)
    done++;

  return done;
}

static PceArg
getLabelNamed(PceReceiver n)
{ return n[name_slot]["label_name"];
  /* or: return n.fetch(name_slot).get("label_name"); ?? */
}


static PceArg
getAddNamed(PceReceiver n, PceArg a1, PceArg a2)
{ return a1 + a2;
}


void
makeClassNamed(void)
{ PceClass cl("named", "object", "Named object");

  name_slot = cl.defvar("name", "naming", "Name of the object",
			"name", "get", "unnamed");

  cl.defsendmethod("name", "naming", "Set the name",
		   nameNamed, "name");
  cl.defsendmethod("bench", "statistics", "n-times @pce ->succeed",
		   benchNamed, "int");
  cl.defsendmethod("pce_succeed", "statistics", "invoke @pce ->succeed",
		   pceSucceedNamed);
  cl.defsendmethod("succeed", "statistics", "just succeed",
		   succeedNamed);
  cl.defsendmethod("succeed1", "statistics", "just succeed",
		   succeed1Named, "any");
  cl.defsendmethod("va_succeed", "statistics", "just succeed",
		   vaSucceedNamed, "any ...");

  cl.defgetmethod("loop", "statistics", "test integer arithmetic",
		   "int", getLoopNamed, "int");
  cl.defgetmethod("quick_loop", "statistics", "test integer arithmetic",
		   "int", getQuickLoopNamed, "int");

  cl.defgetmethod("label", "naming", "Fetch capitalised name",
		  "name", getLabelNamed);
  cl.defgetmethod("add", "arithmetic", "Add arguments",
		  "int", getAddNamed, "int", "int");
}


		 /*******************************
		 *	      ENTRY		*
		 *******************************/

PceStatus
pceInitApplication(int argc, char *argv[])
{ person *me = new person("gnu", "male", 5217);

  makeClassNamed();
  me->makeDialog();

  PceDialog d("XPCE console");
  d.send("append", PceButton("trace",
			     PceAnd(PceMessage(PceClass("vmi"), "trace", "on"),
				    PceMessage(ThePce, "trace", "user"))));

  d.send("append", PceButton("info", PceMessage(ThePce, "info")));
  d.send("append", PceButton("quit", PceMessage(ThePce, "die")));
  d.send("open");

  return TRUE;
}


