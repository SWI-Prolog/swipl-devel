/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for IBM RISC/6000, AIX 3.1 (rios)
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a partial port for  the  IBM-RISC/6000 (RS6000, rios) machine.
All functionality, except for dynamic stacks is ported.

NOTES:	

The  interface for   loading foreign   (C)  code differs from  the one
supported for SUN and documented in the manual.

This version of this file is based on   gcc rather than cc.  Tested with
gcc-2.4.5 on AIX 3.2.  GCC seems  to   like  -static  so we finally have
proper saved states on the RS6000!

Thanks to Olle Ollson at SICS for giving me access to their hardware.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			gcc
/*#define M_OPTIMIZE		-g -DO_DEBUG*/
#define M_OPTIMIZE		-O2
#define M_LDFLAGS		-static
#define M_CFLAGS		
#define M_LIBS		        -lreadline -lm -ltermcap -lbsd

			/* prolog part */

#define FOREIGN_PL		aix_foreign.pl

			/* compiler */
#ifndef __GNUC__
#pragma alloca
#endif
#define etext _etext
#define unix			1
#define _BSD			1	/* BSD compatibility mode */
#define ANSI			1
#define PROTO			1
#define O_NO_LEFT_CAST		1
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_ULONG_PREDEFINED	1
#define O_UCHAR_PREDEFINED	1

			/* C-header-files */
#define TIME_INCLUDE		<time.h>

			/* Operating system */
#define O_DATA_AT_0X2		1 /* data space at 0x20000000 - 0x2fffffff */
#define O_VMCODE_IS_ADDRESS	0
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	0
#define O_AIX_FOREIGN		1
#define O_SAVE			1
#define TEXT_START		0x10000000
#define DATA_START		0x20000000
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local/bin:.:";
#define SRANDOM(t)		srandom((long)t)
#define RANDOM()		random()

#define vfork			fork

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1
#define O_XWINDOWS		0

#define MACHINE			"rs6000"
#define OPERATING_SYSTEM	"aix"
