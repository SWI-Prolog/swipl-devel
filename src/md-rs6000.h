/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Machine description for IBM RISC/6000, AIX 3.1 (rios)
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is a partial port for  the  IBM-RISC/6000 (RS6000, rios) machine.
All functionality, except for dynamic stacks is ported.

NOTES:	

The  interface for   loading foreign   (C)  code differs from  the one
supported for SUN and documented in the manual.

There  are  porblems with  the interaction   between  load_foreign and
save_program.  See pl-load.c and pl-dump.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			cc
/*#define M_OPTIMIZE		-g -DO_DEBUG*/
#define M_OPTIMIZE		-O
#define M_LDFLAGS		-bE:$(EXPORTS)
#define M_CFLAGS		
#define M_LIBS			-lm -ltermcap -lbsd
#define O_INSTALL_DONOT_STORE	1

			/* prolog part */

#define FOREIGN_PL		aix_foreign.pl

			/* compiler */
#pragma alloca
#define etext _etext
#define unix			1
#define _BSD			1	/* BSD compatibility mode */
#define ANSI			1
#define PROTO			1
#define O_NO_LEFT_CAST		1
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_ASM_SWITCH		0
#define O_ULONG_PREDEFINED	1
#define O_UCHAR_PREDEFINED	1

			/* C-header-files */
#define TIME_INCLUDE		<time.h>

			/* Operating system */
#define O_DATA_AT_0X2		1 /* data space at 0x20000000 - 0x2fffffff */
#define O_PROFILE		1
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0	/* Is available but not suitable */
#define O_SHM_ALIGN_FAR_APART   1	/* redundant */
#define O_CAN_MAP		0	/* Neither suitable */
#define O_NO_SEGV_ADDRESS	1	/* redundant */
#define MAX_VIRTUAL_ADDRESS	(512 * 1024 *1024) /* redundant */
#define O_FOREIGN		0
#define O_AIX_FOREIGN		1
#define O_SAVE			1
#define UNEXEC_SOURCE		"gnu/unexaix.c"
#define TEXT_START		0x10000000
#define DATA_START		0x20000000
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local/bin:.:";

#define vfork			fork

			/* terminal driver */
#define O_TERMIOS 		1
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			1
#define O_XWINDOWS		0

#define MACHINE			"rs6000"
#define OPERATING_SYSTEM	"aix"
