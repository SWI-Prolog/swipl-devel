/*	main.c

	The new SWI-Prolog main for the MP library interface for SWI-Prolog,
	version 1.3
	Needs installation of GMP: the GNU Multiple Precision arithmetic library
	version 3.0.1

	Copyright (c) 1999 Robert A. van Engelen, Florida State University
	engelen@cs.fsu.edu. All rights reserved.
*/

#include <stdio.h>
#include "SWI-Prolog.h"
#include "pl-mp.h"
#define READLINE 1                      /* use readline interface */


#ifdef READLINE
static void
install_readline(int argc, char **argv)
{	PL_install_readline();
}
#endif

static void
install_mp(int argc, char **argv)
{	mp_install();
}

int main(int argc, char **argv)
{
#ifdef READLINE
	PL_initialise_hook(install_readline);
#endif
	PL_initialise_hook(install_mp);

	/* This is the only PL_ call allowed */
        /* before PL_initialise().  It */
        /* ensures the foreign predicates */
        /* are available before loading */
        /* Prolog code */

	if ( !PL_initialise(argc, argv) )
		PL_halt(1);

	PL_halt(PL_toplevel() ? 0 : 1);

	return 0;
}

