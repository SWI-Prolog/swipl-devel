/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define msdos			1
#define __msdos__		1
#define NEED_USHORT		1
#undef HAVE_SYS_FILE_H
#undef HAVE_PWD_H
#undef HAVE_SYS_TIMES_H
#undef HAVE_SYS_PARAM_H
#define HAVE_MALLOC_H		1
#define STDC_HEADERS		1
#define POSIX			1
#define DIRECT			1
#undef  HAVE_POPEN
#undef  HAVE_SELECT
#undef  HAVE_SSCANF		
#undef  HAVE_TIMELOCAL		
#define HAVE_STRERROR		1
#define O_DOSFILENAMES		1
#define HAVE_GETCWD		1
#define O_XOS			1
#define TEXT_PTR_ALIGNMENT	1	/* unaligned or sizeof(short)? */
#ifdef M_I386
#define __i386__		1
#endif

#define O_CPLUSPLUS		0	/* include C++ interface */

#define OS	"ms-windows"


