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

#include "md.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <h/interface.h>

extern int pceInitApplication(int argc, char **argv);

int
main(int argc, char* argv[])
{ if ( !pceInitialise(0, NULL, argc, argv) )
  { Cprintf("Sorry, failed to initialise XPCE\n");
    exit(1);
  }
  
  if ( !pceInitApplication(argc, argv) )
  { Cprintf("Failed to run pceInitApplication()\n");
    exit(1);
  }

  for(;;)
    pceDispatch(0, 1000);
}
