/*  md-rs6000.h,v 1.3 1993/01/08 13:47:54 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define OS			"aix"

/* C-compiler */

#ifndef __GNUC__
#pragma alloca				/* Use built-in alloca */
#define O_NO_TAGGED_LVALUE	1	/* No (tag)var = value */
#endif
#define O_SYSTEM_V		1	/* System-V calls (?) */
#define O_ULONG_PREDEFINED	1	/* ulong is built-in */
#define O_UCHAR_PREDEFINED	1	/* uchar is built-in */
#define SIGN_EXTEND_CHAR(x) ((((int)(x)) << 24) >> 24) /* gnu-regex.c */
#define FTIME_MISSING		1	/* gnu-getdate.y */

/* Libraries */

#define O_MOTIF			1	/* AIX uses Motif */
#define O_NOTIMELOCAL		1	/* timelocal() not in library */
#define O_WAIT_UNION		0	/* waitpid(): union wait */
#define O_NOVSSCANF		1	/* library lacks vsscanf() */

/* Machine parameters */

#define POINTER_OFFSET	(0x20000000L)
#define TEXT_OFFSET	(0x10000000L)

/* X11 parameters */

/*typedef char * XtPointer; */		/* Not defined on X11R3 */
#define O_NO_XT_POPUP_SPRING_LOADED 1	/* Not defined */
