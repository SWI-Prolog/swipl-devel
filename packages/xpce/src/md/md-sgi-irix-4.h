/*  md-sgi.h,v 1.1.1.1 1992/06/02 16:22:30 jan Exp

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define OS      "irix"

#define O_ULONG_PREDEFINED	1
#define SIGN_EXTEND_CHAR(x) ((((int)(x)) << 24) >> 24) /* gnu-regex.c */

#define TIMEB_H_MISSING		1
#define O_NOTIMELOCAL		1       /* timelocal() not in library */
#define XTSTRINGDEFINES		1

#define POINTER_OFFSET		(0x10000000L)
#define TEXT_OFFSET		(0x400000L)
