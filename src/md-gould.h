/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for GOULD PN9000, UTX 2.1
*/

#define M_CC			cc
#define M_OPTIMIZE	        -O
#define M_LDFLAGS		
#define M_CFLAGS		
#define M_LIBS			-lm -ltermcap

#define ANSI			0
#define PROTO			0
#define O_NO_LEFT_CAST		1
#define O_NO_VOID_POINTER	1
#define O_SHORT_SYMBOLS		0
			/* Operating system */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		1
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	1
#define MAX_VIRTUAL_ADDRESS	(15 * 1024 *1024)
#define O_FOREIGN		0
#define O_STORE_PROGRAM		0
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local:.:";
#define DIR_INCLUDE		<dir.h>
#define O_STRUCT_DIRECT		1

#define vfork()			fork()

			/* terminal driver */
#define O_TERMIOS 		1
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD 			0
			/* Interfaces */
#define O_PCE 			0

#define MACHINE			"gould"
#define OPERATING_SYSTEM	"utx"
