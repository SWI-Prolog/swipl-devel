/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "dtd.h"
#include "prolog.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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





