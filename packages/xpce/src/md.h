/*  md-sun4.h,v 1.1.1.1 1992/06/02 16:22:30 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <alloca.h>

#define	O_NOVSSCANF	1
#define memmove(to, from, size)	bcopy(from, to, size)

#if GCC_BEFORE_250
extern double strtod(const char *, char**, int);
#define StrTod(s, e)	strtod(s, e, 10)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set O_CONVERT_SUNVIEW_IMAGES to  1  to   include  automatic  reading  of
SunView image files.  Requires the   -lpixrect SunView library installed
and added to Makefile-sparc-sunos-4.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define O_CONVERT_SUNVIEW_IMAGES	0

#define O_CPLUSPLUS		1	/* include C++ interface */
#define HAVE_on_exit		1	/* on_exit() is around */
#define O_EXTRA_SYSTEM_TYPES	<h/sunlib.h>

#define OS	"sun_os"
