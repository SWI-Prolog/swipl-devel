/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Chain.h>

static PceArg PcNframes("frames");
static PceArg PcNkind("kind");
static PceArg PcNtoplevel("toplevel");

extern "C" {
int	pceInitialise(int handles, char *home, int argc, char **argv);
int	pceDispatch(int fd, int timeout);
void	Cprintf(const char *fmt, ...);
}

int
main(int argc, char* argv[])
{ int frames = TRUE;

  if ( !pceInitialise(0, (char *)0, argc, argv) )
  { Cprintf("Sorry, failed to initialise XPCE\n");
    exit(1);
  }
  
  if ( !pceInitApplication(argc, argv) )
  { Cprintf("Failed to run pceInitApplication()\n");
    exit(1);
  }

  while(frames)
  { PceCell cell;

    pceDispatch(0, 1000);

    for(frames = FALSE, cell = AsChain(TheDisplay.get(PcNframes)).head();
	cell;
	++cell)
    { if ( cell.value().get(PcNkind) == PcNtoplevel )
      { frames = TRUE;
	break;
      }
    }
  }

  exit(0);
}
