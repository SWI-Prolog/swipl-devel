/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
ws_show_console(Name how)
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
