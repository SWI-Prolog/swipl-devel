/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>

void
hello()
{ PceObject d("dialog");	// Create an instance of dialog

  d.send("append", PceObject("label", "message", "Hello World"));
  d.send("open");
}


PceStatus
pceInitApplication(int argc, char *argv[])
{ int i;

  hello();

  return SUCCEED;
}
