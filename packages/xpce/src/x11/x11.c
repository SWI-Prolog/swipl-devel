/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"


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
ws_console_label(CharArray label)
{ fail;
}
