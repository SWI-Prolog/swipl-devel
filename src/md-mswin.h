/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for ATARI-ST, turbo-C compiler
*/

#pragma warning 203 9;

#define __MSDOS__		1
#define O_NO_LEFT_CAST		1
#define TIME_INCLUDE		<time.h>
#define DIR_INCLUDE		<direct.h>
#define MAXPATHLEN		512

			/* Operating system */
#define O_SIGNAL		1
#define O_SIG_AUTO_RESET	1
#define O_XOS			1
#define O_NOGETTIMEOFDAY	1
#define O_UNIXLIB		1
#define O_PUTENV		1
#define O_NOSELECT		1
#define O_NOGETW		1
#define O_GETCWD		1
#define O_STRERROR		1
#define O_PROFILE		0
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 *1024)
#define O_FOREIGN		0
#define O_STORE_PROGRAM		0
#define DEFAULT_PATH		""
#define IS_DIR_SEPARATOR(c)	((c) == '\\' || (c) == '/')
#define DESCRIPTOR_TABLE_SIZE	20

			/* Windows interprocess stuff */
#define O_DDE			1

			/* terminal driver */
#define O_TERMIOS 		0	/* Won't work!  Update! */
#define O_READLINE		1
#define O_RL12			1
#define O_FOLD 			0
#define O_RLC			1	/* Window readline console */

			/* Interfaces */
#define O_PCE 			1

#define SYSTEMHOME		"/c:/jan/src/pl"
#define DEFSTARTUP		"_plrc"
#define MACHINE			"PC"
#define OPERATING_SYSTEM	"ms-windows"
