/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Skeleton for extensions
*/

#include <stdio.h>
#include "pl-itf.h"

#define READLINE 1			/* use readline interface */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C-extensions can either be loaded through the foreign language interface
using load_foreign_library/[1,2] or they can   be statically linked with
the Prolog kernel.

In the latter case, proceed as follows:

  1) Make a copy of this file.  In this copy:
  2) Fill the table below for adding foreign functions as predicates
  3) Replace the main() below with your own main().  You can pass
     the main arguments or your own argument (using the systems
     conventions).  Make sure to pass the name of the program as
     first argument.
  4) Normally #undef READLINE (above), unless your embedded version
     provided access to the Prolog toplevel for which you want to
     use readline.
  5) Use plld(1) to link this file, and possible Prolog sources to
     a single executable:

	plld -o myprog myprog.c myprog.pl

     See main plld and/or the SWI-Prolog manuals for further details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PL_extension PL_extensions [] =
{
/*{ "name",	arity,  function,	PL_FA_<flags> },*/

  { NULL,	0, 	NULL,		0 }	/* terminating line */
};


#ifdef READLINE
static void
install_readline(int argc, char**argv)
{ PL_install_readline();
}
#endif

int
main(int argc, char **argv)
{

#ifdef READLINE
  PL_initialise_hook(install_readline);
#endif

  if ( !PL_initialise(argc, argv) )
    PL_halt(1);

  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


