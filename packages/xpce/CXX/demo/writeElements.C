/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Chain.h>
#include <pce/Message.h>
#include <iostream.h>

void
writeElements(PceChain ch)
{ PceCell c = ch.head();
  int i = 0;

  for(; c; c++, i++)
    cout << "Element " << i << ": " << (char *)c.value() << "\n";
}


PceStatus
pceInitApplication(int argc, char *argv[])
{ PceChain ch("gnat");

  PceGlobal("classes").send("for_all", PceMessage(ch, "append", TheArg1));
  ch.send("sort");
  
  writeElements(ch);

  return SUCCEED;
}
