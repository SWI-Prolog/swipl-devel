/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Machine description for the NeXT
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Created by Luc Peerdeman, luc@ailab.eur.nl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compiler flags for making `makefile' from `Makefile' using cpp
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			cc	/* == gcc on NeXT */
/* #define M_OPTIMIZE -g -DO_DEBUG */
#define M_OPTIMIZE -O -finline-functions
#define M_LDFLAGS
#define M_CFLAGS -Wall
#define M_LIBS			-lm -ltermcap 


			/* compiler */
#define ANSI			1
#define PROTO			1
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_ASM_SWITCH		0
			/* Operating system */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	0

#define O_SHARED_MEMORY		0
/* Not documented on NeXT... but present in library. At least, mmap() is.
 * Not a trace of munmap() however, and the definition of MAP_FIXED is also
 * missing from <sys/mmap.h>. Let's wait until NeXTstep 3.0 is out.
 */
#define O_CAN_MAP		0 

#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 * 1024) /* Not checked */

#define O_FOREIGN 		0
#define O_AIX_FOREIGN		0
#define O_MACH_FOREIGN		1

#define O_STORE_PROGRAM		0
#define O_SAVE			1	/* Might work: not tested (JW) */
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local:.:"
#define DIR_INCLUDE		<sys/dir.h>
#define DIR_INCLUDE2		<sys/dirent.h>

			/* terminal driver */
#define O_TERMIOS 		0
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD 			0
			/* Interfaces */
#define O_PCE 			0

#define MACHINE			"next"
#define OPERATING_SYSTEM	"mach"
