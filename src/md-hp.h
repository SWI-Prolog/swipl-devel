/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for HP9000, HPUX 6.5
*/

#define M_CC			cc
#define M_OPTIMIZE	        -O
#define M_LDFLAGS		
#define M_CFLAGS		
#define M_LIBS			-lm -ltermcap

#define ANSI			0
#define PROTO			0
#define O_NO_LEFT_CAST		1
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_ASM_SWITCH		0
			/* Operating system */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	1
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 *1024)
#define O_FOREIGN		1
#define O_STORE_PROGRAM		1
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local:.:";
			/* terminal driver */
#define O_TERMIOS 		1
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD 			0
			/* Interfaces */
#define O_PCE 			0

#define MACHINE			"hp"
#define OPERATING_SYSTEM	"hpux"
