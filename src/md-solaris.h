/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for Solaris 2.2 (running on SPARC)
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE:	/lri2/jan/{lib,include} hold the GNU readline library.  Modify
	to fit your system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			gcc
#define M_OPTIMIZE	        -O2
#define M_LDFLAGS		-L/lri2/jan/lib
#define M_CFLAGS		-Wall -funsigned-char -I/lri2/jan/include
#define M_LIBS			-lm -ltermcap -lreadline -lelf

			/* compiler */
#define solaris			1
#define USG			1
#define ANSI			__GNUC__
#define O_ULONG_PREDEFINED	1

			/* Operating system */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	1
#define O_CAN_MAP		1
#define O_VMCODE_IS_ADDRESS	0
#define O_NO_SEGV_ADDRESS	1
#define MAX_VIRTUAL_ADDRESS	(512 * 1024 *1024)
#define O_FOREIGN		0
#define O_SAVE			1
#define O_ELF			1
#define O_SAVE_STDIO		1
#define DEFAULT_PATH		":.:/usr/ucb:/bin:/usr/bin:";

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1

#define MACHINE			"sparc"
#define OPERATING_SYSTEM	"solaris"
#define MACHINE_ID		"solaris"

