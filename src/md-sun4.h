/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for SUN-4 (SPARC)
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
As SWI-Prolog is ported to ANSI-C, it can no longer be compiled using
SunOS cc.  
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			gcc
#define M_OPTIMIZE	        -O2
#define M_LDFLAGS		-static
/*#define M_CFLAGS		-ansi -pedantic -Wall -funsigned-char*/
#define M_CFLAGS		-Wall -funsigned-char

/*#define M_OPTIMIZE		-g -DO_DEBUG*/

#define M_LIBS			-lm -ltermcap -lreadline

/* #define O_VMCODE_IS_ADDRESS	0	temporary */

			/* compiler */
#define sun			1
#define ANSI			1
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
			/* Operating system */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		1

#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(512 * 1024 *1024)
#define O_FOREIGN		1
#ifndef USE_CC				/* just the default ld for cc */
#define LD_COMMAND		"gcc"
#endif
#define LD_OPT_ADDR		"-T %lx"
#define O_NOENTRY		0	/* ld -e doesn't work */
#define O_SAVE			1
#define DEFAULT_PATH		":.:/usr/ucb:/bin:/usr/bin:";
#define SRANDOM(t)		srandom((long)t)
#define RANDOM()		random()
#define O_EXTRA_SYSTEM_TYPES	"sun-types.h"

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1

#define MACHINE			"sun4"
#define OPERATING_SYSTEM	"sunos"
