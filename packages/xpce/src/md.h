/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
autoconf/config.h based machine-binding file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef _MD_INCLUDED
#define _MD_INCLUDED
#include "../config.h"

		 /*******************************
		 *	      ALLOCA		*
		 *******************************/

/* AIX requires this to be the first thing in the file.  */
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not __GNUC__ */
#if HAVE_ALLOCA_H
#include <alloca.h>
#else /* not HAVE_ALLOCA_H */
#ifdef _AIX
#pragma alloca
#else /* not _AIX */
char *alloca ();
#endif /* not _AIX */
#endif /* not HAVE_ALLOCA_H */
#endif /* not __GNUC__ */


		 /*******************************
		 *          STDC_HEADERS	*
		 *******************************/

#if STDC_HEADERS || HAVE_STRING_H
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !STDC_HEADERS && HAVE_MEMORY_H
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#ifndef HAVE_MEMMOVE
#define memmove(to, from, size)	bcopy(from, to, size)
#endif

		 /*******************************
		 *	SOME SYSTEM STUFF	*
		 *******************************/

#if defined(sun) && !defined(HAVE_LIBELF) /* i.e. not solaris */
#define SOME_MISSING_LIB_PROTOTYPES 1
#define O_EXTRA_SYSTEM_TYPES <h/sunlib.h>
#endif

#endif /*_MD_INCLUDED*/
