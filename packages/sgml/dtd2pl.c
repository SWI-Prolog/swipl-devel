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

int
main(int argc, char **argv)
{ if ( argc == 2 )
  { dtd *dtd = file_to_dtd(argv[1], "test");

    if ( dtd )
    { prolog_print_dtd(dtd, PL_PRINT_ALL & ~PL_PRINT_PENTITIES);
      return 0;
    }
  }

  return 1;
}
