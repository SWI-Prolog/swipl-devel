/*  md-386bsd.h

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for NetBSD

    Adapted from md-freebsd.h with addition of O_TERMIOS/BSD_TERMIOS.
    Probably works for FreeBSD.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compiler flags for making `makefile' from `Makefile' using cpp
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			cc
#define M_OPTIMIZE	        -O
#define M_LDFLAGS		
#define M_CFLAGS		-I/usr/src/gnu/usr.bin/gdb #-g -DO_DEBUG
#define M_LIBS			-lm -ltermcap -L/usr/src/gnu/usr.bin/gdb/readline -lreadline

			/* compiler */
#define ANSI			1
#define PROTO			1
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_ASM_SWITCH		0
			/* Operating system */
#define O_PROFILE		0
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 * 1024)
#define O_FOREIGN		1
#define DEFAULT_PATH		"/bin:/usr/bin:/usr/local/bin:."
#define DESCRIPTOR_TABLE_SIZE   32
#define O_SAVE                  1
#define O_STORE_PROGRAM         0
			/* terminal driver */
#define O_TERMIOS 		1
#define BSD_TERMIOS		1
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD 			0
			/* Interfaces */
#define O_PCE 			0


#undef i386
#define i386 			1
#define MACHINE 		"i386"
#define OPERATING_SYSTEM        "NetBSD"
#define LD_OPT_OPTIONS          "-Bstatic -N"
#define LD_OPT_ADDR		"-T 0x%x"
#define O_READLINE              1
#define O_RL12                  1
#define O_SAVE_STDIO            1
