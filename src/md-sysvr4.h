/*  $Id$

    Copyright (c) 1993 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for System V Release 4.0 on i486
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Port to System V Release 4.0 by Eric S. Raymond <esr@snark.thyrsus.com>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			gcc
#define M_OPTIMIZE	        -O6
/*#define M_OPTIMIZE		-g*/
#define M_LDFLAGS		-static
#define M_CFLAGS		-funsigned-char
#define M_LIBS			-lm -ltermcap /usr/lib/libucb.a

#define v7			1	/* Mostly v7 unix */

			/* compiler */
#define ANSI			1
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_UCHAR_PREDEFINED	0	/* type uchar is predefined */
#define O_ULONG_PREDEFINED	1	/* type ulong is predefined */

			/* Operating system */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	1
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		1
#define O_SHIFT_STACKS		0
#define O_NO_SEGV_ADDRESS	1
#define MAX_VIRTUAL_ADDRESS     (220*1024*1024) /* not sure, but it will do */
#define O_FOREIGN		0
#define O_SAVE			1
#define FIRST_DATA_SYMBOL	etext
#define DEFAULT_PATH		":.:/bin:/usr/bin:/usr/local/bin:";
#define SIGNAL_HANDLER_TYPE	void
#define O_STRUCT_DIRECT		0
#define DIR_INCLUDE		<sys/dir.h>
#define DIR_INCLUDE2		<dirent.h>
#define TERMIO_INCLUDE		<termio.h>

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1

#define MACHINE			"i486"
#define OPERATING_SYSTEM  	"sysvr4"


		/********************************
		*      COMPATIBILITY MACROS	*
		********************************/

#define bzero(t, l)	memset(t, 0, l)
#define bcopy(s, t, n)	memcpy(t, s, n)
