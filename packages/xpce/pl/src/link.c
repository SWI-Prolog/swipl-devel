/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <SWI-Prolog.h>

extern foreign_t pl_pce_init(term_t h);

int
main(int argc, char **argv)
{ if ( !PL_initialise(argc, argv) )
    PL_halt(1);

  PL_install_readline();		/* delete if you don't want readline */

  PL_halt(PL_toplevel() ? 0 : 1);

  return 1;
}
