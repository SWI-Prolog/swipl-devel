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

Updated version 1.6.8 after an important fix sent to me by Peter Barth
(barth@mpi-sb.mpg.de)   and   minor   fixes    from    Philip   Perucc  
(dsc3pzp@nmrdc1.nmrdc.nnmc.navy.mil).  LINUX versions:

	gcc 2.3.3, libc 4.2, Linux 0.99pl2:

Updated version  1.6.14 by  Jan Wielemaker  (got  my own  now).  Fixed 
save_program/1 and included dynamic stacks.  Versions:

	gcc 2.3.3, libc 4.3, Linux 0.99pl7:

Fixed load_foreign/[1,2] and dropped -static from the LDFLAGS and this
appears not to be necessary with linux.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			gcc
/*#define M_OPTIMIZE	        -O6*/
#define M_OPTIMIZE		-g
#define M_LDFLAGS		
#define M_CFLAGS		-funsigned-char
#define M_LIBS			-lm -ltermcap -lreadline

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
#define O_FOREIGN		1
#define LD_OPT_ADDR		"-T 0x%x"
#define O_SAVE			1
#define FIRST_DATA_SYMBOL	etext
#define DEFAULT_PATH		":.:/bin:/usr/bin:/usr/local/bin:";
#define SIGNAL_HANDLER_TYPE	void
#define O_STRUCT_DIRECT		0
#define DIR_INCLUDE		<sys/dir.h>
#define DIR_INCLUDE2		<dirent.h>
#define TERMIO_INCLUDE		<termio.h>
#define O_GETCWD		1

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1

#define MACHINE			"i386"
#define OPERATING_SYSTEM  	"linux"


		/********************************
		*      COMPATIBILITY MACROS	*
		********************************/

#define bzero(t, l)	memset(t, 0, l)
#define vfork()		fork()
