/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
