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

#ifndef _PCE_X11_INCLUDED
#define _PCE_X11_INCLUDED

#define O_XDND 1			/* include Gnome/KDE drag-and-drop */

#ifdef HAVE_XMISMOTIFWMRUNNING
#define O_MOTIF 1
#endif

#ifndef O_PPM
#define O_PPM 1

#define PNM_PNM	0			/* Portable aNy Map */
#define PNM_PBM	1			/* Portable BitMap */
#define PNM_PGM	2			/* Portable GreyMap */
#define PNM_PPM	3			/* Portable PixMap */

#define PNM_ASCII   0
#define PNM_RAWBITS 3
#define PNM_RUNLEN  6
#endif

#define O_GIFWRITE 1

#define String XString
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>		/* XtConfigureWidget() prototype */
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#undef Bool				/* X11 defines this too */
#undef index				/* X11 defines this too */
#undef String

#ifndef XMalloc
#define XMalloc(size) pceMalloc(size)
#define XFree(ptr)    pceFree(ptr)
#endif

#if XT_VERSION == 10
error XPCE cannot be build for X version 10.  Sorry.
#endif

#ifdef O_XDND
#include "xdnd.h"
#endif


		 /*******************************
		 *	      IMAGES		*
		 *******************************/

typedef enum 
{ IMG_OK,				/* Image loaded fine */
  IMG_UNRECOGNISED,			/* Routine didn't recognise it */
  IMG_NOMEM,				/* Not enough memory */
  IMG_INVALID,				/* Something wrong with the image */
  IMG_NO_STATIC_COLOUR			/* true colour routine can't handle */
} img_status;

img_status staticColourReadJPEGFile(Image image, IOSTREAM *fd, XImage **ret);


		 /*******************************
		 *	      FRAME		*
		 *******************************/

typedef struct
{ Widget	widget;
  Window	busy_window;
  int		win_gravity;
} frame_ws_ref, *FrameWsRef;


		 /*******************************
		 *	     DISPLAY		*
		 *******************************/

typedef struct
{ Display      *display_xref;		/* X-windows reference */
  Widget	shell_xref;		/* Top of widget tree */
  Pixmap	root_bitmap;		/* Bitmap for GC creation */
  DrawContext	pixmap_context;		/* Context for pixmap operations */
  DrawContext	bitmap_context;		/* Context for bitmap operations */
  int		depth;			/* depth of display */
  unsigned long white_pixel;		/* a white pixel */
  unsigned long black_pixel;		/* a black pixel */
  unsigned long foreground_pixel;	/* foreground pixel */
  unsigned long background_pixel;	/* background pixel */
  Colormap	colour_map;		/* Colourmap of the display */
#ifdef O_XDND
  DndClass     *dnd;			/* The DND handler */
  Atom		XdndTextUriList;	/* "text/uri-list" */
#endif
} display_ws_ref, *DisplayWsXref;

#define display_x11_ref(d, field) (((DisplayWsXref)((d)->ws_ref))->field)


		 /*******************************
		 *	      FONTS		*
		 *******************************/

typedef char cwidth;

typedef struct xpce_font_info *XpceFontInfo;

struct xpce_font_info
{ XFontStruct  *info;			/* the X info structure */
  cwidth       *widths;			/* array of ints for the widths */
};

		/********************************
		*        GRAPHICS CONTEXT	*
		********************************/

struct draw_context
{ Name		kind;			/* Kind of device */

  GC		workGC;			/* Dynamic GC for simple graphicals */
  GC		clearGC;		/* Write current background */
  GC		andGC;			/* Stipple background */
  GC		fillGC;			/* Tile/Stipple area */
  GC		complementGC;		/* Complement an area */
  GC		bitmapGC;		/* Paint bitmap to pixmap */
  GC		copyGC;			/* Simple pixmap copy */
  GC		opGC;			/* Logical area operations */
  GC		shadowGC;		/* Paint shadows */
  GC		reliefGC;		/* Paint opposite of shadow */
  
  int		pen;			/* Current pen */
  int		depth;			/* #bits per pixel */
  Name		dash;			/* Current dash pattern */
  Name		arcmode;		/* Current arc mode (filling arcs) */
  Any		fill;			/* Image or Colour */
  Image		and_pattern;		/* Current andpattern */
  FontObj	font;			/* Current font */
  XFontStruct * font_info;		/* X-font for this display */
  cwidth      * char_widths;		/* array with widths of characters */
  Any		colour;			/* Current colour */
  Any		background;		/* Current background colour */
  unsigned long foreground_pixel;	/* X pixel value of foreground */
  unsigned long background_pixel;	/* X pixel value of background */
  Bool		subwindow_mode;		/* Draw in subwindows too */
  Bool		invert_mode;		/* Just invert */
  Elevation	elevation;		/* 3-d elevation GC's */
};


		 /*******************************
		 *	    PROTOTYPES		*
		 *******************************/

/* x11-common.c */
Widget		widgetWindow(PceWindow sw);
Widget		widgetFrame(FrameObj fr);
Atom		DisplayAtom(DisplayObj d, Name name);
char *		DisplayAtomToString(DisplayObj d, Atom a);
Atom		FrameAtom(FrameObj fr, Name name);
char *		FrameAtomToString(FrameObj fr, Atom a);
Atom		WmProtocols(FrameObj fr);
XtAppContext	pceXtAppContext(XtAppContext ctx);
EventObj	CtoEvent(Any window, XEvent *event);
unsigned long   getPixelColour(Colour c, DisplayObj d);
void		setXImageImage(Image image, XImage *i);
int		shift_for_mask(unsigned long mask);
status		postscriptXImage(XImage *im,
				 int x, int y, int w, int h,
				 Display *disp, Colormap cmap,
				 int depth);
int		intensityXColor(XColor *c);
void		x11_set_gc_foreground(DisplayObj d, Any fg, int gcs, GC *gc);
#ifdef O_XDND
status		setDndAwareFrame(FrameObj fr);
#endif

/* x11-conversion.c */
XImage *	readImageFile(Image image, IOSTREAM *fd);
XImage *	CreateXImageFromData(unsigned char *data, int w, int h);
XImage *	read_ppm_file(Display *disp, Colormap map,
			      int depth, IOSTREAM *fd);
int		write_pnm_file(IOSTREAM *fd, XImage *img,
			       Display *disp, Colormap cmap,
			       int scale, int fmt, int asascii);
int		write_jpeg_file(IOSTREAM *fd, XImage *img,
				Display *disp, Colormap cmap, Image image);
int		write_gif_file(IOSTREAM *fd, XImage *img, XImage *msk,
			       Display *disp, Colormap cmap);

/* xcolour.c */
status		allocNearestColour(Display *display, Colormap map,
				   int depth, Name vt, XColor *c);

extern XtAppContext ThePceXtAppContext;	/* X toolkit application context */

#endif /*_PCE_X11_INCLUDED*/

