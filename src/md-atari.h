/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for ATARI-ST, turbo-C compiler
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE:  This port is not yet complete.  Known problems:

  * System() does not work properly
  * Setenv() does not work properly
  * Profiling possibly can be ported
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			tcc
#define M_OPTIMIZE	        -O
#define M_LDFLAGS		
#define M_CFLAGS		
#define M_LIBS			-lm -ltermcap

#define tos			1
#define ANSI			1
#define PROTO			1
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		1
#define O_ASM_SWITCH		0
#define O_16_BITS		1

			/* Operating system */
#define O_PROFILE		0
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 *1024)
#define O_FOREIGN		0
#define O_STORE_PROGRAM		0
#define DEFAULT_PATH		""
#define DESCRIPTOR_TABLE_SIZE	OPEN_MAX

			/* terminal driver */
#define O_TERMIOS 		0
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			0

#include <ext.h>

#define SYSTEMHOME		"c:/pl"
#define SYSTEMSTATE		"pl.wic"
#define DEFSTARTUP		"pl.rc"
#define DEFLOCAL		200
#define DEFGLOBAL		100
#define DEFTRAIL	 	50
#define DEFARGUMENT	  	5
#define DEFLOCK		  	5
#define MACHINE			"atari_st"
#define OPERATING_SYTEM		"gemdos"
