/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <SWI-Prolog.h>
#include <stdio.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This little file servers a similar role for incremental linking of C(++)
code with SWI-Prolog as /lib/crt0.o plays  for normal executables: it is
prepended for the object files (see   pceload/[1,2])  and deals with the
initialisation of loaded code.

First, the C++ constructors are   called.   Next pceInitApplication() is
called with the original main argc/argv   pair.  Finally the symboltable
is restored so that further incrementally linked packages donot conflict
with the symbols loaded from this session.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef void (*func_ptr) ();		/* for the ctor handling */
extern	func_ptr __CTOR_LIST__[];

#if defined(__GNUC__)
static void
do_ctors(func_ptr *p)
{ for (p++; *p; )			/* skip the first */
  { (*p++) ();
  }
}
#endif


void
__pl_start()
{ 
#if defined(__GNUC__)
  do_ctors(__CTOR_LIST__);
#endif

  pceInitApplication((int) PL_query(PL_QUERY_ARGC),
		     (char **) PL_query(PL_QUERY_ARGV));

  PL_action(PL_ACTION_SYMBOLFILE,
	    (void *)PL_query(PL_QUERY_ORGSYMBOLFILE));
}
