/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Call.h>
#include <pce/Chain.h>
#include <pce/Dialog.h>
#include <pce/Message.h>
#include <pce/TextItem.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple application to show the number of   instances created for an XPCE
class. The association of  a  function   as  value-set  to the text-item
illustrates C++ functions returning an XPCE object, much like ?(@prolog,
predicate, Args).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


PceStatus
showInstances(PceArg cl)
{ PceArg c;

  if ( (c = ThePce.get("convert", cl, "class")) )
  { TheDisplay.send("inform", "Class %s has %d instances",
		    c.get("name"), c.get("no_created") -
		    c.get("no_freed"));
  }

  return SUCCEED;
}


PceArg
collectClasses()
{ PceObject ch("chain");

  PceGlobal("classes").send("for_all",
			    PceMessage(ch, "append", TheArg1));

  return ch;
}


PceStatus
pceInitApplication(int argc, char *argv[])
{ PceDialog d("Show #Instances");
  PceArg ti;

  d.send("append", ti=PceTextItem("class", "",
				  PceCall(showInstances, TheArg1)));
  ti.send("value_set", PceFuncall(collectClasses));
  return d.send("open");
}
