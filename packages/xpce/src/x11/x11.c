/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/interface.h>
#include "include.h"


void
ws_initialise(int argc, char **argv)
{
}


int
ws_version()
{ return XT_VERSION;
}


int
ws_revision()
{ return XT_REVISION;
}


status
ws_expose_console()
{ fail;
}
 

status
ws_iconify_console()
{ fail;
}
 

status
ws_console_label(CharArray label)
{ char *t = getenv("TERM");

  if ( t && streq(t, "xterm") )
  { char buf[1000];

    sprintf(buf, "\033]2;%s\007", strName(label));
    hostAction(HOST_WRITE, buf);
    hostAction(HOST_FLUSH);
  }

  succeed;
}
