/*  md-mips.h,v 1.4 1993/04/28 15:25:04 jan Exp

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for DEC MIPS station
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Received this updated version 2/7/93 from David Morley <morley@aaii.oz.au>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			gcc
#define M_OPTIMIZE	        -O2
#define M_LDFLAGS		
#define M_CFLAGS		-funsigned-char
#define M_LIBS			-lm -ltermcap

			/* compiler */
#define ANSI			__GNUC__
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0

			/* Operating system */
#define O_SAVE			0
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	1
#define O_SHARED_MEMORY		0
#define O_SHM_ALIGN_FAR_APART	1
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	1
#define MAX_VIRTUAL_ADDRESS	(0x10000000 + 100 * 1024 * 1024)
#define O_FOREIGN		0
#define O_STORE_PROGRAM		0
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local:.:";
#define STREAM_OPEN_BIN_READ	"r"
#define STREAM_OPEN_BIN_WRITE	"w"
#define UNEXEC_SOURCE		"gnu/unexmips.c"
#define TEXT_START		0x400000
#define DATA_START		0x10000000
#define O_DATA_AT_OX1		1
#define O_VMCODE_IS_ADDRESS	0

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1

#define MACHINE			"mips"
#define OPERATING_SYSTEM	"ultrix"

