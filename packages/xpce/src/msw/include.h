/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#define  RedrawWindow WinRedrawWindow
#include <windows.h>
#undef RedrawWindow

#include "pcewh.h"
#include "..\..\..\readline\console.h"

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
  unsigned short *data;			/* the data */
} ws_bits, *WsBits;


typedef struct
{ HWND		hwnd;
  HCURSOR	hcursor;		/* current cursor handle */
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
void		initDraw(void);
void *		ws_image_bits(Image image);
void *		ws_image_bits_for_cursor(Image image, Name kind, int w, int h);
PceWindow	get_window_holding_point(FrameObj fr, POINT *pt);
status		move_big_cursor(void);
status		exit_big_cursor(void);
status		start_big_cursor(CursorObj c);
