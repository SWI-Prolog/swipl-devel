/*  md-solaris.h,v 1.1.1.1 1992/06/02 16:22:30 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <alloca.h>

#define solaris			1
#define USG			1

#define	O_NOVSSCANF		1
#define O_NOTIMELOCAL		1
#define O_ULONG_PREDEFINED	1
#define DIRENT			1	/* for gnu-system.h */
#define FTIME_MISSING		1	/* for gnu-getdate.y */
#define HAVE_on_exit		1	/* on_exit() is around */
#define O_CPLUSPLUS		1	/* include C++ interface */

#define OS	"solaris"


