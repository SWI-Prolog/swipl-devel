/*  $Id$

    Copyright (c) 1992 Jan Wielemaker/Pieter Olivier. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for Linux
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The   port to LINUX   was made  by   Pieter  Olivier.   It   has  been
incorperated  into the most  recent   version  (1.5.5) of  the  common
sources, but not tested afterwards.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			cc
#define M_OPTIMIZE	        -O
#define M_LDFLAGS		
#define M_CFLAGS		
#define M_LIBS			

#define LINUX			1	/* Remaining #if's for linux */
#define v7			1	/* Mostly v7 unix */

			/* compiler */
#define ANSI			0
#define PROTO			0
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_UCHAR_PREDEFINED	1	/* type uchar is predefined */
#define O_ULONG_PREDEFINED	1	/* type ulong is predefined */

			/* Operating system */
#define O_PROFILE		0
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS     (220*1024*1024) /* I don't know how bit it is */
#define O_FOREIGN		1
#define O_STORE_PROGRAM		0
#define DEFAULT_PATH		":.:/bin:/usr/bin:/usr/local/bin:";
#define SIGNAL_HANDLER_TYPE	int
#define DESCRIPTOR_TABLE_SIZE   32
#define O_STRUCT_DIRECT		0
#define DIR_INCLUDE		<sys/dir.h>
#define DIR_INCLUDE2		<dirent.h>
#define TERMIO_INCLUDE		<termio.h>
			/* terminal driver */
#define O_TERMIOS 		1
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_FOLD 		 	79	
			/* Interfaces */
#define O_PCE 			0

#define MACHINE			"Intel-386"
#define OPERATING_SYSTEM  	"Linux"


		/********************************
		*      COMPATIBILITY MACROS	*
		********************************/

#define bzero(t, l)	memset(t, 0, l)
#define vfork()		fork()
