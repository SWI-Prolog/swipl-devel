/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


#ifndef GIFHDRH
#define GIFHDRH

typedef unsigned int PIXEL;		/* for X11 compatibility */

typedef int (*GIFAllocColor)(int index,
			     int r, int g, int b,
			     void *closure);
typedef int (*GIFAllocColorTable)(int size, void *closure);
typedef int (*GIFDoExtension)(int ext, void *data, void *closure);

#define GIFEXT_TRANSPARENT 0		/* data = colour index */

#define GIF_OK		0
#define GIF_NOMEM	1
#define GIF_INVALID	2

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define MAXCOLORMAPSIZE	256
#define CM_RED	0
#define CM_GREEN 1
#define CM_BLUE 2
#define UCHAR unsigned char
#define MAX_LZW_BITS	12

extern int GIFReadFD(IOSTREAM *fd,
		     PIXEL **data, int *width, int *height,
		     GIFAllocColorTable at,
		     GIFAllocColor ac,
		     GIFDoExtension doext,
		     void *closure);
extern const char  *GIFError(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First include <X11/xpm.h> to get  this   prototype.  The  function is in
giftoxpm.c. The usage in x11/xconvert.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef XPM_h
extern int XpmReadGIF(IOSTREAM *fd, XpmImage *image);
#endif

#endif /*GIFHDRH*/
