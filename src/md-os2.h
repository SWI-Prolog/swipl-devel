/*  md-os2.h,v 1.0 1993/02/16 

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Copyright (c) 1993 Andreas T"onne. All rights reserved.

    Purpose: Machine description for OS/2 & EMX (PC)
*/


#define M_CC			gcc
#define M_OPTIMIZE	        -O2
#define M_LDFLAGS	     
#define M_CFLAGS		-Wall -DOS2 -DEMX
#define M_LIBS			-lm -ltermc -los2
#define M_SEP       		\\


			/* compiler */
#define ANSI			1
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
			/* Operating system */
#define O_PROFILE		0
#define O_SIG_AUTO_RESET	1
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 * 1024)
#define O_FOREIGN		0
#define O_SAVE			1
#define DEFAULT_PATH		"C:\\usr\\local\\cmd;.;"
#define IS_DIR_SEPARATOR	((c) == '\\' || (c) == '/')
#define DESCRIPTOR_TABLE_SIZE   4096
#define FIRST_DATA_SYMBOL       _data   /* valid for EMX under OS/2 only */
#define LAST_DATA_SYMBOL        _end    /* valid for EMX under OS/2 only */
#define HEAP_START		_heap_start
#define SIGNAL_CONTEXT_TYPE	void *
#define O_VMCODE_IS_ADDRESS	0
typedef unsigned short		ushort;

			/* terminal driver */
#define O_TERMIOS 		1
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD 			0
			/* Interfaces */
#define O_PCE 			0

                        /* Make */
#define OS2MAKE                 1

#define MACHINE			"PC"
#define OPERATING_SYSTEM	"OS/2 2.0"
