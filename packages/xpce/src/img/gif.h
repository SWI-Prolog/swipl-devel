/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
