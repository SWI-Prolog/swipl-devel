/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dtd.h"
#include "prolog.h"

#define streq(s,q) strcmp((s), (q)) == 0

char *program;

static void
usage()
{ fprintf(stderr, "Usage: %s [-xml|sgml] file.dtd\n", program);
}

int
main(int argc, char **argv)
{ dtd_dialect dialect = DL_SGML;

  program = argv[0];
  argv++;
  argc--;
  
  while(argc > 0 && argv[0][0] == '-')
  { if ( streq(argv[0], "-xml") )
    { dialect = DL_XML;
      argc--;
      argv++;
    } else if ( streq(argv[0], "-sgml") )
    { dialect = DL_SGML;
      argc--;
      argv++;
    } else
    { usage();
      exit(1);
    }
  }

  if ( argc == 1 )
  { dtd *dtd = file_to_dtd(argv[0], "test", dialect);

    if ( dtd )
    { prolog_print_dtd(dtd, PL_PRINT_ALL & ~PL_PRINT_PENTITIES);
      return 0;
    }
  }

  usage();
  return 1;
}





