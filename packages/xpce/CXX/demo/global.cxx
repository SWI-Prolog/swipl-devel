/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Very simple demo, illustrating a single global C++ object referencing an
XPCE object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <pce/Pce.h>
#include <pce/Picture.h>

PcePicture p("Hello World");

PceStatus
pceInitApplication(int argc, char *argv[])
{ p.send("open");

  return SUCCEED;
}
