/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

#include <stdlib.h>
#include <pce/Pce.h>
#include <pce/Button.h>
#include <pce/Dialog.h>
#include <pce/Message.h>
#include <pce/TextItem.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Very simple tool to print XPCE's notion  on its home directory. The home
directory  contains  the  X11  resource  files,  standard  bitmaps,  the
PostScript header file, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PceStatus
pceInitApplication(int argc, char *argv[])
{ int i;
  PceDialog d("XPCE");
  PceTextItem ti("PCE Home directory", ThePce.get("home"));
  PceButton b("ok", PceMessage(ThePce, "die"));

  ti.send("editable", TheOff);
  d.send("append", ti);
  d.send("append", b);
  d.send("open");

  return SUCCEED;
}
