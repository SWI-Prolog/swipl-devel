/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine Description for Silicon Graphics (sgi)
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This port was made by Fred Kwakkel from CWI, the Netherlands.

It appears `O_FOREIGN 1' gives problems on some SGI archiectures. It you
are not a C-hacker change this into O_FOREIGN 0. Otherwise you might
want to start debugging.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			gcc
#define M_OPTIMIZE		-O2
#define M_LDFLAGS		
#define M_CFLAGS
#define M_LIBS			-lm -ltermcap


			/* compiler */
#define ANSI			1
#define PROTO			1
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_ULONG_PREDEFINED	1

/* Operating system */

#define sgi			1
#undef mips
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0
#define O_SHM_ALIGN_FAR_APART   0
#define O_CAN_MAP		0

#define O_LABEL_ADDRESSES       0
#define O_VMCODE_IS_ADDRESS     0

#define O_NO_SEGV_ADDRESS       1

#define COFF                    1
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 * 1024)
#define TEXT_START		0x400000
#define DATA_START		0x10000000
#define O_DATA_AT_OX1		1
#define O_FOREIGN		1
#define O_STORE_PROGRAM		0
#define O_SAVE		        1
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local:.:"

#define vfork()			fork()

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0


			/* Interfaces */
#define O_PCE 			1

#define MACHINE			"sgi"
#define OPERATING_SYSTEM	"sgi"
