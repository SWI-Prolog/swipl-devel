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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


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

  if ( t && streq(t, "xterm") && isatty(2) )
  { char buf[256];

    sprintf(buf, "\033]2;%s\007", strName(label));
    write(2, buf, strlen(buf));
  }

  succeed;
}


Int
ws_default_scrollbar_width()
{ return toInt(16);
}
