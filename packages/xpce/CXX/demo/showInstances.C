/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Call.h>
#include <pce/Dialog.h>
#include <pce/TextItem.h>

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


PceStatus
pceInitApplication(int argc, char *argv[])
{ PceDialog d("Show #Instances");

  d.send("append", PceTextItem("class", "",
			       PceCall(showInstances, TheArg1)));
  return d.send("open");
}
