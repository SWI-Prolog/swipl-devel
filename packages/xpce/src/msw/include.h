/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#define RedrawWindow WinRedrawWindow
#include <windows.h>
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

#include "pcewh.h"

#ifdef __WIN32__
#define CONSOLE_PROVIDE_WINMAIN 1	/* rlc_hinstance() and rlc_hwnd() */
#include <console.h>
#define WinAPI		int PASCAL
#define MK_FP32(x) x
#else /*__WIN32__*/

#include "..\..\..\readline\console.h"
#define WinAPI		long FAR PASCAL _export
#endif /*__WIN32__*/

#define PceHInstance	rlc_hinstance()

#define GWL_DATA	(0)		/* client-handle offset */

#define Ellipse PceEllipse
#define Arc PceArc
#include <h/kernel.h>
#include <h/graphics.h>
#undef Arc
#undef Ellipse

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
} ws_image, *WsImage;


typedef struct
{ HWND		hwnd;
  HCURSOR	hcursor;		/* current cursor handle */
  unsigned capture : 1;			/* has capture */
} ws_window, *WsWindow;


typedef struct
{ HWND		hwnd;			/* Windows handle */
  int		placed;			/* Explicit placement? */
  HCURSOR	hbusy_cursor;		/* handle for busy cursor */
} ws_frame, *WsFrame;


#define getDisplayWindow(sw) getDisplayGraphical((Graphical)(sw))


unsigned char *	read_bitmap_data(FILE *fd, int *w, int *h);
HWND		getHwndFrame(FrameObj fr);
void		setHwndFrame(FrameObj fr, HWND ref);
HWND		getHwndWindow(PceWindow sw);
void		setHwndWindow(PceWindow sw, HWND ref);
EventObj	messageToEvent(HWND hwnd, UINT msg, UINT wParam, LONG lParam);
status		d_mswindow(PceWindow sw, IArea a, int clear);
void		d_hdc(HDC hdc, Colour fg, Colour bg);
void		initDraw(void);
void		exitDraw(void);
void *		ws_image_bits(Image image);
void *		ws_image_bits_for_cursor(Image image, Name kind, int w, int h);
PceWindow	get_window_holding_point(FrameObj fr, POINT *pt);
status		move_big_cursor(void);
status		exit_big_cursor(void);
status		start_big_cursor(CursorObj c);

		 /*******************************
		 *	  DEBUGGING MACROS	*
		 *******************************/

static int _dobj;
static HCURSOR _hcur;

#define ZSelectObject(hdc, obj)	\
	(assert(obj), SelectObject(hdc, obj))
#define ZDeleteObject(obj) \
	(_dobj = DeleteObject(obj), \
	 (!_dobj?(void)Cprintf("%s:%d: DeleteObject(0x%x) failed\n",\
			       __FILE__,__LINE__,obj):(void)0),\
	 _dobj)
#define ZSetCursor(h) \
	(_hcur = SetCursor(h), \
	 (PCEdebugging && memberChain(PCEdebugSubjects, NAME_cursor)) ? \
	     printf("SetCursor(0x%04x)\n") : 1, _hcur)
