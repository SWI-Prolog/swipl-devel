/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for SUN-3
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: when using gcc, make sure you have patched the SUN include files
using the shell script fixincludes  provided with gcc.   If you don't,
the terminal interface does not operate properly.

Note: From SunOs 4.1.1 this fix appears no longer necessary.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#define M_CC			gcc
#define M_OPTIMIZE	        -O -finline-functions -fomit-frame-pointers
#define M_LDFLAGS		-static
#define M_CFLAGS		-Wall
#define M_LIBS			-lm -ltermcap

#ifndef SUNOS_3		/* how to find out? */
#define SUNOS_4 1
#else
#define SUNOS_4 0
#endif

			/* compiler */
#define ANSI			0
#define PROTO			0
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
			/* Operating system */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		SUNOS_4	/* NOTE: set to 0 when running */
					/* SunOs 4.0.0 on a SUN-3; mmap() */
					/* is broken on this OS */
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 *1024)
#define O_FOREIGN		1
#define O_STORE_PROGRAM		1
#define DEFAULT_PATH		":.:/usr/ucb:/bin:/usr/bin:";
#ifdef SUNOS_3
#define SIGNAL_HANDLER_TYPE	int
#define DESCRIPTOR_TABLE_SIZE	32
#define O_STRUCT_DIRECT		1
#define DIR_INCLUDE		<sys/dir.h>
#endif

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1

#define MACHINE			"sun3"
#define OPERATING_SYSTEM	"sunos"
