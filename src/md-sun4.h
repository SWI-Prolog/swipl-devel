/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for SUN-4 (SPARC)
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: when using gcc, make sure you have patched the SUN include files
using the shell script fixincludes  provided with gcc.   If you don't,
the terminal interface does not operate properly.   To fix this either
run fixincludes or compile  pl-os.c using cc: `cc  -c -O pl-os.c'  and
run make again.

When using SUN cc, make sure to  use  the  BSD compiler, include files
and libraries.  If you are not sure, run `which cc' which should yield
/usr/ucb/cc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef USE_CC				/* Sun cc (slower) */
#define M_CC			cc
#define M_OPTIMIZE		-O	/* O2 hardly helps */
#define M_LDFLAGS		-Bstatic -L/usr/local/lib
#define M_CFLAGS		
#include <alloca.h>
#else					/* gcc (preferred) */
#define M_CC			gcc
#define M_OPTIMIZE	        -O2
#define M_LDFLAGS		-static
#define M_CFLAGS		-funsigned-char
#endif

/*#define M_OPTIMIZE		-g*/

#define M_LIBS			-lm -ltermcap -lreadline

/* #define O_VMCODE_IS_ADDRESS	0	/* temporary */

			/* compiler */
#define ANSI			__GNUC__
#define O_NO_LEFT_CAST		0
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
#define O_NOENTRY		1	/* ld -e doesn't work */
#define O_SAVE			1
#define DEFAULT_PATH		":.:/usr/ucb:/bin:/usr/bin:";
#define SRANDOM(t)		srandom((long)t)
#define RANDOM()		random()

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1

#define MACHINE			"sun4"
#define OPERATING_SYSTEM	"sunos"
