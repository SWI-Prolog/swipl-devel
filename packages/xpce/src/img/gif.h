/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (note this is different from the rest of XPCE):
    This is free to use and modify provided proper credit is given

    See gifread for further comments on this file.
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
