/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
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
