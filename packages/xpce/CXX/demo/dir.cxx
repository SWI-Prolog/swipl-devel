/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <stdlib.h>
#include <pce/Pce.h>
#include <pce/Chain.h>
#include <pce/Directory.h>
#include <pce/Message.h>
#include <iostream.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The application `dir' lists the directories   and  files in all argument
directories. Usage: dir directory ...

It illustrates two ways of enumerating the   elements  of an XPCE chain.
The first uses the XPCE method   `chain->for_all', while the latter uses
the PceChain::head() and PceCell C++ class   to  follow the linked list.
Not that the first  is  safe  for  changes   to  the  chin  while  it is
enumerated, while the latter is not.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PceStatus
pceInitApplication(int argc, char *argv[])
{ int i;

  for(i=1; i < argc; i++)
  { PceDirectory d(argv[i]);

    PceChain dirs = AsChain(d.get("directories"));
    cout << "sub directories of " << argv[i] << endl;
    dirs.send("for_all", PceMessage(ThePce, "write_ln", TheArg1));

    PceChain files = AsChain(d.get("files"));
    cout << "files in " << argv[i] << endl;
    PceCell cell;

    for( cell = files.head(); cell; )
      cout << (char *)(++cell).value() << endl;
  }

  ThePce.send("die");
}
