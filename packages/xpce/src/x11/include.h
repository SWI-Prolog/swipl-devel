/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_X11_INCLUDED
#define _PCE_X11_INCLUDED

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

		 /*******************************
		 *	      FRAME		*
		 *******************************/

typedef struct
{ Widget	widget;
  Window	busy_window;
} frame_ws_ref;


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
  ulong		foreground_pixel;	/* X pixel value of foreground */
  ulong		background_pixel;	/* X pixel value of background */
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
ulong		getPixelColour(Colour c, DisplayObj d);
Window		getWMFrameFrame(FrameObj fr);
void		setXImageImage(Image image, XImage *i);
status		postscriptXImage(XImage *im,
				 int x, int y, int w, int h,
				 Display *disp, Colormap cmap,
				 int depth);
XColor **	makeSparceCInfo(Display *disp, Colormap cmap,
				XImage *img, int *ncolours);
void		greySparceCInfo(XColor **cinfo, int depth);
void		freeSparceCInfo(XColor **table, int depth);
void		x11_set_gc_foreground(DisplayObj d, Any fg, int gcs, GC *gc);

/* x11-conversion.c */
XImage *	readImageFile(Image image, FILE *fd);
XImage *	CreateXImageFromData(unsigned char *data, int w, int h);
XImage *	read_ppm_file(Display *disp, Colormap map,
			      int depth, FILE *fd);
int		write_pnm_file(FILE *fd, XImage *img,
			       Display *disp, Colormap cmap,
			       int scale, int fmt, int asascii);

/* xcolour.c */
status		findNearestColour(Display *display, Colormap map,
				  int depth, Name vt, XColor *c);
int		intensityXColor(XColor *c);

extern XtAppContext ThePceXtAppContext;	/* X toolkit application context */

/* xmenu.c */
Any		ws_3d_grey(void);

#endif /*_PCE_X11_INCLUDED*/

