/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: VAX Ultrix 32 GCC-1.37 compiler machine description file
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: SWI-Prolog 1.4 has been ported by Marcel van Teeuwen to the VAX.
I tried to incorporate the changes  into 1.5, but  as the organisation
of 1.5 is a bit different this md-file is not guarantied to work.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			cc
#define M_OPTIMIZE	        -O
#define M_LDFLAGS		
#define M_CFLAGS		
#define M_LIBS			-lm -ltermcap

#define MACHINE			"vaxp"
#define OPERATING_SYSTEM	"ultrix"

			/* Compiler */
#define ANSI			0
#define PROTO			0
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_ASM_SWITCH		0
			/* Operating system */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0	/* does not work properly when 1 */
#define O_CAN_MAP		0	/* SIGSEGV otherwise */
#define O_NO_SEGV_ADDRESS	1
#define MAX_VIRTUAL_ADDRESS	(8 * 1024 * 1024) /* actual limit is 21Mb */
#define O_FOREIGN		1
#define O_STORE_PROGRAM		1
#define O_ASM_SWITCH		0
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local/bin:.:";
#define STREAM_OPEN_BIN_READ	"r"
#define STREAM_OPEN_BIN_WRITE	"w"
			/* terminal driver */
#define O_TERMIOS		1	/* 1 suggested */
#define O_EXTEND_ATOMS		1
#define O_LINE_EDIT		1	/* 1 gives extra echo */
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD			0
			/* Interfaces */
#define O_PCE			0
