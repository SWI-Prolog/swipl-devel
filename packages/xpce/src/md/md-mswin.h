/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define msdos			1
#define __msdos__		1
#define NEED_USHORT		1
#define HAVE_SYS_FILE_H		0
#define HAVE_PWD_H		0
#define HAVE_SYS_TIMES_H	0
#define HAVE_SYS_PARAM_H	0
#define HAVE_MALLOC_H		1
#define TIME_H			<time.h>
#define STDC_HEADERS		1
#define POSIX			1
#define DIRECT			1
#define O_NO_POPEN		1       /* popen() is not supported */
#define O_NO_SELECT		1 	/* select() is not supported */
#define O_NOVSSCANF		1
#define O_STRERROR		1
#define O_NOTIMELOCAL		1
#define O_DOSFILENAMES		1
#define O_GETCWD		1
#define O_XOS			1
#define TEXT_PTR_ALIGNMENT	1	/* unaligned or sizeof(short)? */
#ifdef M_I386
#define __i386__		1
#endif

#define O_CPLUSPLUS		0	/* include C++ interface */

#define OS	"ms-windows"


