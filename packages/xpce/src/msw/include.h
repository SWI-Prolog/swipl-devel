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

#undef  O_IMGLIB  			/* Generic IMGLIB support */
#undef  O_GIFREAD 			/* The GIF library */
#define O_XPM	  1			/* use Xpm library */
#define O_GIF	  1			/* use our GIF loader */
#define O_GIFWRITE 1			/* allow writing GIF files */

#define RedrawWindow WinRedrawWindow
#include <windows.h>
#ifndef WM_MOUSEWHEEL			/* sometimes not defined */
#define WM_MOUSEWHEEL 0x020A
#endif
#undef RedrawWindow
#undef hyper				/* don't need this */
#undef islower				/* we have these ourselves */
#undef isupper
#undef isdigit
#undef isalnum
#undef isalpha
#undef iscntrl
#undef isprint
#undef isspace
#undef ispunct
#undef isxdigit

#define PceHInstance ThePceHInstance

#define Ellipse PceEllipse
#define Arc PceArc
#include <h/kernel.h>
#include <h/graphics.h>
#undef Arc
#undef Ellipse

#define APIError() WinStrError(GetLastError())

#include "xpcemh.h"

typedef struct win_mf *WinMF;		/* win_metafile pointer type */

extern HINSTANCE ThePceHInstance;	/* HINSTANCE from pceDLLEntry() */

typedef char	cwidth;			/* width of a character */

typedef struct
{ HFONT		hfont;			/* Windows font handle */
  cwidth *	widths;			/* Character widths */
  int		ascent;			/* tmAscent + tmExternalLeading */
  int		descent;		/* tmDescent */
  int		from_stock;		/* TRUE: from GetStockObject() */
} ws_font, *WsFont;


typedef struct
{ int w;				/* width of image */
  int h;				/* height of image */
  BITMAPINFO *msw_info;			/* MS-Windows info structure */
  void *data;				/* the data */
  HICON icon;				/* Icon we've created for it */
} ws_image, *WsImage;


typedef struct
{ HWND		hwnd;
  HCURSOR	hcursor;		/* current cursor handle */
  WNDPROC	saved_window_procedure;	/* For refinement */
  unsigned capture    : 1;		/* has capture */
  unsigned open	      : 1;		/* window is opened */
} ws_window, *WsWindow;


typedef struct
{ HWND		hwnd;			/* Windows handle */
  int		placed;			/* Explicit placement? */
  HCURSOR	hbusy_cursor;		/* handle for busy cursor */
  HCURSOR	hcursor;		/* cursor displayed */
  int		bb;			/* has bounding box */
  int		bbx, bby, bbw, bbh;	/* BB coordinates */
} ws_frame, *WsFrame;

		 /*******************************
		 *	     COLOURMAP		*
		 *******************************/

typedef struct jpeg_colour_map *JpegColourMap; /* JPEG <-> DIB colourmap */

typedef struct ws_cmdata		/* colour_map data */
{ HPALETTE      hpal;			/* Windows palette */
  JpegColourMap jpeg_cmap;		/* JPEG <-> DIB colourmap */
} *WsCmdata;

extern WsCmdata getWsCmdata(ColourMap cm);
extern void	free_jpeg_cmap(JpegColourMap map);

#define EXACT_COLOUR_MASK 0x80000000L	/* Avoid rounding the colour */

		 /*******************************
		 *	    PROTOTYPES		*
		 *******************************/

#define getDisplayWindow(sw) getDisplayGraphical((Graphical)(sw))

void		ws_renderall(void);
int		ws_provide_selection(int format);
HWND		PceHiddenWindow(void);
HBITMAP		read_ppm_file(IOSTREAM *fd, Name *kind);
int		write_pnm_file(IOSTREAM *fd, HBITMAP bm,
			       int scale, int fmt, int encode);
unsigned char *	read_bitmap_data(IOSTREAM *fd, int *w, int *h);
void		assocObjectToHWND(HWND hwnd, Any obj);
Any		getObjectFromHWND(HWND hwnd);
HWND		getHwndFrame(FrameObj fr);
void		setHwndFrame(FrameObj fr, HWND ref);
HWND		getHwndWindow(PceWindow sw);
void		setHwndWindow(PceWindow sw, HWND ref);
Any		messageToKeyId(UINT message, UINT wParam, LONG lParam,
			       unsigned long *bmask);
EventObj	messageToEvent(HWND hwnd, UINT msg, UINT wParam, LONG lParam);
status		d_mswindow(PceWindow sw, IArea a, int clear);
void		d_hdc(HDC hdc, Colour fg, Colour bg);
void		initDraw(void);
void		exitDraw(int rval);
void *		ws_image_bits(Image image);
void *		ws_image_bits_for_cursor(Image image, Name kind, int w, int h);
HICON		ws_icon_from_image(Image img);
void		ws_create_image_from_x11_data(Image image, unsigned char *data,
					      int w, int h);
PceWindow	get_window_holding_point(FrameObj fr, POINT *pt);
status		move_big_cursor(void);
status		exit_big_cursor(void);
status		start_big_cursor(CursorObj c);
void		PceWhDeleteWindow(HWND win);
void		PceEventInWindow(HWND win);

void		ws_system_colours(DisplayObj d);
Colour		ws_system_colour(DisplayObj d, const char *name, COLORREF rgb);
status		ws_has_colourmap(DisplayObj d);
void		declareWindowsBrush(Any obj, HBRUSH brush);
status		d_winmf(const char *fn,
			int x, int y, int w, int h,
			const char *descr);
HENHMETAFILE	d_winmfdone(void);
void		r_winmf(HENHMETAFILE hmf, int x, int y, int w, int h);
void		ws_init_loc_still_timer(void);
void		setPaletteColourMap(ColourMap cm, HPALETTE hpal);
HPALETTE	getPaletteColourMap(ColourMap cm);
HDC		d_current_hdc(void);
status		postscriptDC(HDC hdc, int fx, int fy, int w, int h, int depth);
HBRUSH		standardWindowsBrush(Any obj);
void		r_3d_rectangle(int x, int y, int w, int h,
			       int z, COLORREF *colours);
status		ws_on_clipboard_metafile(WinMF mf, Name type);
WinMF		CtoWinMetafile(HENHMETAFILE hmf);
COLORREF	cref_colour(Colour c);
COLORREF	ws_3d_grey_rgb();
HPALETTE	window_palette(PceWindow sw);
HPALETTE	frame_palette(FrameObj fr);
int		write_jpeg_file(IOSTREAM *fd, Image image, HBITMAP bm);
status		read_jpeg_file(IOSTREAM *fd, Image image);
int		write_gif_file(IOSTREAM *fd, Image image, HBITMAP bm);
HWND		HostConsoleHWND();
Name		getWinFileNameDisplay(DisplayObj d,
				      Name mode,
				      Chain filters,
				      CharArray title,
				      CharArray file,
				      Directory dir,
				      Any owner);

#include "msproto.h"

		 /*******************************
		 *	    PNM FORMATS		*
		 *******************************/

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

		 /*******************************
		 *	  DEBUGGING MACROS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Debugging  the  management  of  Microsofts   graphical  resources.  When
activated,  XPCE's  low-level  graphics  will  maintain  the  hash-table
@ms_objects, containing all currently allocated   graphical  objects. It
will also check the return value of various of the graphical calls.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*#define DEBUG_GRAPHICS 1*/

#ifdef DEBUG_GRAPHICS

#ifndef NAME_msObjects
#define NAME_msObjects CtoName("ms_objects")
#endif
#ifndef NAME_solidBrush
#define NAME_solidBrush CtoName("solid_brush")
#endif
#ifndef NAME_patternBrush
#define NAME_patternBrush CtoName("pattern_brush")
#endif

static int _dobj;
static HCURSOR _hcur;
static HBRUSH _hbrush;
static HBITMAP _hbitmap;
static HPEN _hpen;
static HRGN _hrgn;

extern HashTable	MsObjectTable(void);

#define ZDeleteFromRegistry(obj) \
	(!deleteHashTable(MsObjectTable(), toInt(obj)) \
	 ?(void)Cprintf("%s:%d obj not in registry: 0x%x\n",\
			__FILE__,__LINE__, obj):(void)0)

#define ZSelectObject(hdc, obj)	\
	(assert(obj), SelectObject(hdc, obj))

#define ZDeleteObject(obj) \
	(ZDeleteFromRegistry(obj), \
	 _dobj = DeleteObject(obj), \
	 (!_dobj?(void)Cprintf("%s:%d: DeleteObject(0x%x) failed\n",\
			       __FILE__,__LINE__,obj):(void)0),\
	 _dobj)

#define ZSetCursor(h) \
	(_hcur = SetCursor(h), \
	 (PCEdebugging && memberChain(PCEdebugSubjects, NAME_cursor)) ? \
	     printf("SetCursor(0x%04x)\n") : 1, _hcur)

		 /*******************************
		 *	     CREATION		*
		 *******************************/

#define ZCreatePatternBrush(bm) \
	( _hbrush = CreatePatternBrush(bm), \
	  appendHashTable(MsObjectTable(),toInt(_hbrush),NAME_patternBrush), \
	  _hbrush)
#define ZCreateSolidBrush(rgb) \
	( _hbrush = CreateSolidBrush(rgb), \
	  appendHashTable(MsObjectTable(), toInt(_hbrush), NAME_solidBrush), \
	  _hbrush)
#define ZCreateCompatibleBitmap(hdc, w, h) \
	( _hbitmap = CreateCompatibleBitmap(hdc, w, h), \
	  DEBUGGING(NAME_msObjects)?(void) \
		Cprintf("%s:%d: ZCreateCompatibleBitmap(%d x %d) --> 0x%x\n", \
			__FILE__, __LINE__, w, h, _hbitmap):(void)0, \
	  assert(_hbitmap), \
	  appendHashTable(MsObjectTable(), toInt(_hbitmap), NAME_bitmap), \
	  _hbitmap)
#define ZCreateDIBitmap(a, b, c, d, e, f) \
	( _hbitmap = CreateDIBitmap(a, b, c, d, e, f), \
	  appendHashTable(MsObjectTable(), toInt(_hbitmap), NAME_bitmap), \
	  _hbitmap)
#define ZCreateBitmap(a, b, c, d, e) \
	( _hbitmap = CreateBitmap(a, b, c, d, e), \
	  appendHashTable(MsObjectTable(), toInt(_hbitmap), NAME_bitmap), \
	  _hbitmap)
#define ZCreateRectRgn(x, y, w, h) \
	( _hrgn = CreateRectRgn(x, y, w, h), \
	  appendHashTable(MsObjectTable(), toInt(_hrgn), NAME_region), \
	  _hrgn)
#define ZCreateRectRgnIndirect(r) \
	( _hrgn = CreateRectRgnIndirect(r), \
	  appendHashTable(MsObjectTable(), toInt(_hrgn), NAME_region), \
	  _hrgn)
#define ZCreatePen(s, t, c) \
	( _hpen = CreatePen(s, t, c), \
	  appendHashTable(MsObjectTable(), toInt(_hpen), NAME_pen), \
	  _hpen)
#define ZExtCreatePen(s, w, b, n, p) \
	( _hpen = ExtCreatePen(s, w, b, n, p), \
	  appendHashTable(MsObjectTable(), toInt(_hpen), NAME_pen), \
	  _hpen)

#else /*DEBUG_GRAPHICS*/

#define ZSelectObject(hdc, obj)	SelectObject(hdc, obj)	
#define ZDeleteObject(obj) DeleteObject(obj) 
#define ZSetCursor(h) SetCursor(h) 
#define ZCreatePatternBrush(bm) CreatePatternBrush(bm) 
#define ZCreateSolidBrush(rgb) CreateSolidBrush(rgb) 
#define ZCreateCompatibleBitmap(hdc, w, h) CreateCompatibleBitmap(hdc, w, h) 
#define ZCreateDIBitmap(a, b, c, d, e, f) CreateDIBitmap(a, b, c, d, e, f) 
#define ZCreateBitmap(a, b, c, d, e) CreateBitmap(a, b, c, d, e) 
#define ZCreateRectRgn(x, y, w, h) CreateRectRgn(x, y, w, h) 
#define ZCreateRectRgnIndirect(r) CreateRectRgnIndirect(r) 
#define ZCreatePen(s, t, c) CreatePen(s, t, c) 
#define ZExtCreatePen(s, w, b, n, p) ExtCreatePen(s, w, b, n, p)

#endif /*DEBUG_GRAPHICS*/
