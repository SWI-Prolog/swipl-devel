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

#if defined(__sun__) && !defined(__svr4__) && defined(__GNUC__)
#define GCC_DOTORS
#endif

#ifdef GCC_DOTORS
extern	func_ptr __CTOR_LIST__[];
extern	func_ptr __DTOR_LIST__[];
#endif

#if defined(GCC_DTORS)
static void
do_ctors(func_ptr *p)
{ for (p++; *p; )			/* skip the first */
  { (*p++) ();
  }
}
#endif


install_t
install()
{ 
#if defined(GCC_DTORS)
  do_ctors(__CTOR_LIST__);
#endif

  pceInitApplication((int) PL_query(PL_QUERY_ARGC),
		     (char **) PL_query(PL_QUERY_ARGV));
}


install_t
uninstall()
{
#if defined(GCC_DTORS)
  do_ctors(__DTOR_LIST__);
#endif
}
