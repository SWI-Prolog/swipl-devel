/*  md-sun4.h,v 1.1.1.1 1992/06/02 16:22:30 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <alloca.h>

#define _POSIX_SOURCE		1	/* demand posix source */
#define O_ULONG_PREDEFINED	1	/* type ulong is predefined */
#define O_NOTIMELOCAL		1	/* timelocal() not in library */
#define O_SIG_AUTO_RESET	1	/* reinstall signal handlers */

#define O_CPLUSPLUS		1	/* include C++ interface */
#define HAVE_on_exit		1	/* provides atexit() */

#define OS	"linux"


