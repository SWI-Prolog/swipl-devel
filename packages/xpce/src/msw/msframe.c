/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "include.h"

static int WINAPI frame_wnd_proc(HWND win, UINT msg, UINT wP, LONG lP);
static status     keyboard_event_frame(FrameObj fr, Any id,
				       UINT wParam, LONG lParam,
				       unsigned long bmask);
static void       paint_icon(FrameObj fr);

#define MainWindow(fr)	     ( isNil(fr->members->head) ? (Any) fr : \
			       fr->members->head->value )

static FrameObj current_frame;		/* hack for timing problem */

static char *
WinFrameClass()
{ static Name winclassname = NULL;
  static WNDCLASS wndClass;

  if ( !winclassname )
  { char buf[50];

    sprintf(buf, "PceFrame%d", PceHInstance);
    winclassname = CtoName(buf);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) frame_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= PceHInstance;
    wndClass.hIcon		= NULL; /*LoadIcon(NULL, IDI_APPLICATION);*/
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= strName(winclassname);

    RegisterClass(&wndClass);
  }

  return strName(winclassname);
}


static char *
WinPopupFrameClass()
{ static Name winclassname = NULL;
  static WNDCLASS wndClass;

  if ( !winclassname )
  { char buf[50];

    sprintf(buf, "PcePopupFrame%d", PceHInstance);
    winclassname = CtoName(buf);

    wndClass.style		= /*CS_HREDRAW|CS_VREDRAW|*/CS_SAVEBITS;
    wndClass.lpfnWndProc	= (LPVOID) frame_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= PceHInstance;
    wndClass.hIcon		= NULL; /*LoadIcon(NULL, IDI_APPLICATION);*/
    wndClass.hCursor		= LoadCursor(NULL, IDC_ARROW);
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= strName(winclassname);

    RegisterClass(&wndClass);
  }

  return strName(winclassname);
}


HPALETTE
frame_palette(FrameObj fr)
{ ColourMap cm = fr->colour_map;

  if ( isDefault(cm) && notNil(fr->display) )
    cm = fr->display->colour_map;

  if ( isNil(cm) || isDefault(cm) )
    return NULL;
  else
    return getPaletteColourMap(cm);
}


static int
do_frame_wnd_proc(FrameObj fr,
		  HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ DEBUG(NAME_event,
	Cprintf("%s(0x%04x): MS-Windows event 0x%04x with 0x%04x/0x%08lx\n",
		pp(fr), hwnd, message, wParam, lParam));

  switch(message)
  { case WM_CREATE:
    { DragAcceptFiles(hwnd, TRUE);
      break;
    }

#if 0					/* does not work in Windows 95! */
    case WM_WINDOWPOSCHANGED:
    { LPWINDOWPOS wpos = (LPWINDOWPOS) lParam;

      if ( (wpos->flags & SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER) ==
						   SWP_NOSIZE|SWP_NOMOVE )
	send(fr, NAME_exposed, EAV);
/*
      Cprintf("hwnd = 0x%x, insertAfter = 0x%x, flags = 0x%x\n",
	      wpos->hwnd, wpos->hwndInsertAfter,
	      wpos->flags);
*/
      break;
    }
#endif

    case WM_SIZE:			/* frame resized */
    { int w = LOWORD(lParam);
      int h = HIWORD(lParam);

      DEBUG(NAME_frame, Cprintf("Resized %s to %d x %d\n", pp(fr), w, h));
      assign(fr->area, w, toInt(w));
      assign(fr->area, h, toInt(h));

      switch( wParam )
      { case SIZE_MINIMIZED:
	{ Cell cell;

	  SetWindowText(hwnd, strName(getIconLabelFrame(fr)));
	  assign(fr, status, NAME_iconic);
	  for_cell(cell, fr->members)
	    DisplayedGraphical(cell->value, OFF);
	  break;
	}
	case SIZE_RESTORED:
	case SIZE_MAXIMIZED:
	  if ( IsWindowVisible(hwnd) )
	  { Cell cell;

	    send(fr, NAME_resize, EAV);
	    SetWindowText(hwnd, strName(fr->label));
	    assign(fr, status, wParam == SIZE_MAXIMIZED ? NAME_fullScreen
		   					: NAME_window);
	    for_cell(cell, fr->members)
	      DisplayedGraphical(cell->value, ON);
	  }
	  break;
      }

      goto repaint;
    }

    case WM_MOVE:			/* frame moved */
    { POINTS pt = MAKEPOINTS(lParam);

      DEBUG(NAME_frame, Cprintf("Moved %s to %d, %d\n", pp(fr), pt.x, pt.y));
      assign(fr->area, x, toInt(pt.x));
      assign(fr->area, y, toInt(pt.y));

      return 0;
    }

    case WM_SHOWWINDOW:
    { HWND hwnd;
      Cell cell;

      if ( !wParam && (hwnd = getHwndFrame(fr)) )
      { Cell cell;

	for_cell(cell, fr->members)
	{ HWND subhwnd = getHwndWindow(cell->value);
	  
	  if ( subhwnd )
	    PceWhDeleteWindow(subhwnd);
	}

	PceWhDeleteWindow(hwnd);
      }

 
      if ( wParam )			/* show on */
      { for_cell(cell, fr->members)
	{ extern void unlink_changes_data_window(PceWindow sw);

	  send(cell->value, NAME_displayed, ON, EAV);
	  ComputeGraphical(cell->value);
	  unlink_changes_data_window(cell->value);
	}

	send(fr, NAME_mapped, ON, EAV);

	assign(fr, status, NAME_window); /* Or full_screen? */
	ws_set_icon_frame(fr);
      } else				/* show off */
      { for_cell(cell, fr->members)
	{ if ( !onFlag(cell->value, F_FREED|F_FREEING) )
	    send(cell->value, NAME_displayed, OFF, EAV);
	}

	if ( !isFreedObj(fr) || isFreeingObj(fr) )
	  send(fr, NAME_mapped, OFF, EAV);

	assign(fr, status, NAME_hidden);
      }

      goto repaint;
    }

    case WM_SETFOCUS:
    case WM_KILLFOCUS:
    { Bool val = (message == WM_SETFOCUS ? ON : OFF);

      send(fr, NAME_inputFocus, val, EAV);
      goto repaint;
    }

    case WM_QUERYNEWPALETTE:
    case_query:
    { HPALETTE hpal = frame_palette(fr);
      if ( hpal )
      { HDC hdc = GetDC(hwnd);
	int i = 0;

	hpal = SelectPalette(hdc, hpal, FALSE);
	i = RealizePalette(hdc);
	SelectPalette(hdc, hpal, TRUE);
	RealizePalette(hdc);
	ReleaseDC(hwnd, hdc);

	if ( i > 0 )
	{ forwardColourMapChangeFrame(fr);
	  return TRUE;
	}
      }

      return FALSE;
    }
    case WM_PALETTECHANGED:
      if ( (HWND)wParam != hwnd )
	goto case_query;

      return FALSE;

    case WM_ERASEBKGND:			/* TODO: Add colourmap code */
    { HDC hdc = (HDC) wParam;
      RECT rect;
      COLORREF rgb = (COLORREF) getXrefObject(fr->background, fr->display);
      HBRUSH hbrush;
      
      rgb = GetNearestColor(hdc, rgb);
      hbrush = ZCreateSolidBrush(rgb);
      GetClipBox(hdc, &rect);
      FillRect(hdc, &rect, hbrush);
      ZDeleteObject(hbrush);

      DEBUG(NAME_redraw, Cprintf("Cleared background %d %d %d %d of %s\n",
				 rect.left, rect.top,
				 rect.right - rect.left,
				 rect.bottom - rect.top,
				 pp(fr)));

      return 1;				/* non-zero: I've erased it */
    }

    case WM_PAINT:
      if ( IsIconic(hwnd) )
      { paint_icon(fr);
	return 0;
      } else
        goto win_default;
      
#ifdef WM_MOUSEWHEEL			/* distributed as key-event */
    case WM_MOUSEWHEEL:
      DEBUG(NAME_wheel, Cprintf("Got WM_MOUSEWHEEL on %s\n", pp(fr)));
#endif
    case WM_KEYDOWN:			/* Named keys */
    case WM_SYSCHAR:			/* ALT-commands */
    case WM_CHAR:			/* Printable keys */
    { unsigned long bmask;
      Any id = messageToKeyId(message, wParam, lParam, &bmask);

      DEBUG(NAME_wheel,
	    if ( id == NAME_wheel )
	      Cprintf("Translated to wheel-event\n"));

      if ( id && keyboard_event_frame(fr, id, wParam, lParam, bmask) )
	return 0;

      break;
    }
    case WM_SYSCOMMAND:			/* prevent loosing the mouse on ALT */
      if ( (wParam & 0xfff0) == SC_KEYMENU )
	return 0;
      break;

    case WM_CLOSE:
    { Code msg;

      if ( (msg = checkType(getValueSheet(fr->wm_protocols,
					  CtoName("WM_DELETE_WINDOW")),
			    TypeCode, fr)) )
      { DEBUG(NAME_close, Cprintf("Running WM_DELETE_WINDOW message %s\n",
				  pp(msg)));
	forwardReceiverCode(msg, fr, MainWindow(fr), EAV);
	DEBUG(NAME_close, Cprintf("Finished WM_DELETE_WINDOW. fr=%s, msg=%s\n",
				  pp(fr), pp(msg)));
      }

      return 0;
    }

    case WM_DESTROY:
    { HWND hwnd = getHwndFrame(fr);

      DEBUG(NAME_window, Cprintf("WM_DESTROY on %s, hwnd 0x%x\n",
				 pp(fr), hwnd)); 
      if ( hwnd )
      { DragAcceptFiles(hwnd, FALSE);
	setHwndFrame(fr, 0);
	assocObjectToHWND(hwnd, NIL);
	freeObject(fr);
      }

      return 0;
    }

    case WM_SETCURSOR:
    { if ( LOWORD(lParam) == HTCLIENT )
      { WsFrame f = fr->ws_ref;

	if ( f )
	{ if ( !f->hcursor )
	    f->hcursor = LoadCursor(NULL, IDC_ARROW);

	  ZSetCursor(f->hcursor);
	}

	return 1;
      }

      break;
    }

#if 0
    case WM_PARENTNOTIFY:
    { int  fwEvent = LOWORD(wParam);
      HWND child   = HIWORD(wParam);

      if ( fwEvent == WM_DESTROY )
      { DEBUG(NAME_window, Cprintf("%s: child 0x%x destroyed\n",
				   pp(fr), hwnd));
	return 0;
      }

      break;
    }
#endif
  }

  { EventObj ev;
    AnswerMark mark;
    status rval = FALSE;
    markAnswerStack(mark);
  
    if ( (ev = messageToEvent(hwnd, message, wParam, lParam)) )
    { if ( message != WM_WINENTER && message != WM_WINEXIT )
	PceEventInWindow(hwnd);

      addCodeReference(ev);
      rval = send(fr, NAME_event, ev, EAV);
      delCodeReference(ev);
      freeableObj(ev);
    }
    rewindAnswerStack(mark, NIL);

    if ( rval )
    { RedrawDisplayManager(TheDisplayManager());
      return 0;
    }
  }

repaint:
  RedrawDisplayManager(TheDisplayManager());

win_default:
  return DefWindowProc(hwnd, message, wParam, lParam);
}


static int
service_frame(FrameObj fr)
{ Application app = fr->application;

  return (notNil(app) && app->kind == NAME_service ? PCE_EXEC_SERVICE
						   : PCE_EXEC_USER);
}


static int WINAPI
frame_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ FrameObj fr = getObjectFromHWND(hwnd);
  int rval;

  if ( !fr )
  { fr = current_frame;
    if ( !fr )
      return DefWindowProc(hwnd, message, wParam, lParam);
  }
  assert(isProperObject(fr));

  ServiceMode(service_frame(fr),
	      rval = do_frame_wnd_proc(fr, hwnd, message, wParam, lParam));

  return rval;
}


static void
paint_icon(FrameObj fr)
{ if ( notNil(fr->icon_image) )
  { HWND hwnd = getHwndFrame(fr);
    PAINTSTRUCT ps;
    HDC  hdc  = BeginPaint(hwnd, &ps);
    HBITMAP bm = (HBITMAP) getXrefObject(fr->icon_image, fr->display);
    Size is = fr->icon_image->size;
    HDC mhdc = CreateCompatibleDC(hdc);
    HBITMAP obm;

    obm = SelectObject(mhdc, bm);
    StretchBlt(hdc, 0, 0, valInt(fr->area->w), valInt(fr->area->h),
	       mhdc, 0, 0, valInt(is->w), valInt(is->h), SRCCOPY);
    SelectObject(mhdc, obm);
    DeleteDC(mhdc);

    EndPaint(hwnd, &ps);
  }
}


static void
get_point_frame(FrameObj fr, POINT *pt)
{ GetCursorPos(pt);
  pt->x -= valInt(fr->area->x);
  pt->y -= valInt(fr->area->y);
}


PceWindow
get_window_holding_point(FrameObj fr, POINT *pt)
{ HWND win;
  PceWindow sw;

  if ( (win = ChildWindowFromPoint(getHwndFrame(fr), *pt)) &&
       (sw  = getObjectFromHWND(win)) &&
       instanceOfObject(sw, ClassWindow) )
    return sw;

  fail;
}


static status
keyboard_event_frame(FrameObj fr, Any id,
		     UINT wParam, LONG lParam,
		     unsigned long bmask)
{ PceWindow sw;
  POINT pt;
  EventObj ev;
  AnswerMark mark;
  status rval = FALSE;
  Any receiver;
  unsigned long m;

  get_point_frame(fr, &pt);
  if ( !(sw = get_window_holding_point(fr, &pt)) &&
       !(sw = getKeyboardFocusFrame(fr)) )
    receiver = fr;
  else
    receiver = sw;

  markAnswerStack(mark);

  if ( sw )
  { pt.x -= valInt(sw->area->x) + valInt(sw->pen);
    pt.y -= valInt(sw->area->y) + valInt(sw->pen);
  }
  ev = answerObject(ClassEvent, id, receiver, toInt(pt.x), toInt(pt.y), EAV);
  m = valInt(ev->buttons);
  m &= ~(BUTTON_shift|BUTTON_control|BUTTON_meta);
  m |= bmask;
  assign(ev, buttons, toInt(m));

  if ( id == NAME_wheel )
  { short a = (short)HIWORD(wParam);
    Any angle = toInt(a);

    attributeObject(ev, NAME_rotation, angle);
    DEBUG(NAME_wheel,
	  Cprintf("Posting wheel %s degrees to %s\n",
		  pp(angle), pp(receiver)));
  }

  addCodeReference(ev);
  rval = postEvent(ev, receiver, DEFAULT);
  delCodeReference(ev);
  freeableObj(ev);

  rewindAnswerStack(mark, NIL);

  RedrawDisplayManager(TheDisplayManager());

  succeed;
}


PceWindow
ws_window_holding_point_frame(FrameObj fr)
{ POINT pt;

  get_point_frame(fr, &pt);
  return userWindow(get_window_holding_point(fr, &pt));
}


status
ws_created_frame(FrameObj fr)
{ if ( getHwndFrame(fr) )
    succeed;

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ws_uncreate_frame(FrameObj fr) is called  by   `frame  ->uncreate'.   It
calls  DestroyWindow(),  which  in  turn   will  destroy  the  MS-Window
subwindows, causing the WM_DESTROY action on these windows.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_uncreate_frame(FrameObj fr)
{ HWND hwnd = getHwndFrame(fr);

  if ( hwnd )
  { Cell cell;

    setHwndFrame(fr, 0);
    assocObjectToHWND(hwnd, NIL);
    PceWhDeleteWindow(hwnd);

    for_cell(cell, fr->members)
    { HWND subhwnd = getHwndWindow(cell->value);
      if ( subhwnd )
	PceWhDeleteWindow(subhwnd);
    }

    DestroyWindow(hwnd);
  }
}


static void
outer_frame_area(FrameObj fr, int *x, int *y, int *w, int *h, int limit)
{ Area a = fr->area;
  int dw, th;

  *x = valInt(a->x);
  *y = valInt(a->y);
  *w = valInt(a->w);
  *h = valInt(a->h);

  if ( fr->kind == NAME_toplevel )
  { dw = 4;				/* decoration width */
    th = 19;				/* title hight */
  } else if ( fr->kind == NAME_transient )
  { dw = 4;
    if ( getClassVariableValueObject(fr, NAME_decorateTransient) == ON )
      th = 19;
    else
      th = 0;
  } else
    dw = th = 0;

  *x -= dw;
  *w += dw * 2;
  *y -= dw + th;
  *h += dw * 2 + th;

  if ( limit && *x < 0 )
    *x = 0;
  if ( limit && *y < 0 )
    *y = 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: transients are a bit complicated. I'd   like to have a window with
just a title and a close-button. The  below appears to achieve that. The
icon_image must be set to @nil. 

If we are a transient window we must set the parent handle. This ensures
we stay on top of the parent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_create_frame(FrameObj fr)
{ HWND ref;
  HWND owner = NULL;
  DWORD style = WS_CLIPCHILDREN; 
  DWORD exstyle = 0;
  int x, y, w, h;

  if ( fr->kind == NAME_popup )
  { style |= WS_POPUP;
    if ( fr->border != ZERO )
      style |= WS_BORDER;
    exstyle |= WS_EX_TOOLWINDOW;
  } else
  { if ( fr->kind == NAME_toplevel )
    { style |= WS_OVERLAPPEDWINDOW;
    } else if ( fr->kind == NAME_transient )
    { int dec;

      dec = (getClassVariableValueObject(fr, NAME_decorateTransient) == ON);
      if ( notNil(fr->transient_for) )
	owner = getHwndFrame(fr->transient_for);

      { style = WS_POPUP;
	if ( dec )
	{ style	|= WS_POPUPWINDOW|WS_CAPTION;
	  exstyle |= WS_EX_DLGMODALFRAME;
	} else
	  style |= WS_DLGFRAME;
      }
    }

    if ( fr->can_resize == ON )
    { style |= (WS_MAXIMIZEBOX|WS_MINIMIZEBOX|WS_SIZEBOX);
    } else
    { style &= ~(WS_MAXIMIZEBOX|WS_MINIMIZEBOX|WS_SIZEBOX);
    }
  }
    
  outer_frame_area(fr, &x, &y, &w, &h, TRUE);

  current_frame = fr;
  ref = CreateWindowEx(exstyle,
		       fr->kind == NAME_popup ? WinPopupFrameClass()
					      : WinFrameClass(),
		       strName(fr->label),
		       style,
		       x, y, w, h,
		       owner,
		       NULL,		/* menu */
		       PceHInstance,
		       NULL);		/* Creation data */
		     
  if ( !ref )
    return errorPce(fr, NAME_xOpen, fr->display);

  setHwndFrame(fr, ref);
  assocObjectToHWND(ref, fr);
  current_frame = NULL;

  succeed;
}


void
ws_realise_frame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;
    
    ShowWindow(getHwndWindow(sw), SW_SHOW);
  }
}

void
ws_frame_border(FrameObj fr, int *xb, int *yb, int *ycap)
{ *xb   = GetSystemMetrics(SM_CXBORDER);
  *yb   = GetSystemMetrics(SM_CYBORDER);
  *ycap = GetSystemMetrics(SM_CYCAPTION);
}

#define PLACE_MARGIN 30			/* don't place on the border */
#define PLACE_X_OFFSET 20		/* offsets */
#define PLACE_Y_OFFSET 30

static void
ws_place_frame(FrameObj fr)
{ static int last_x = 0, last_y = 0;
  static int placed = 0;
  int dw = valInt(getWidthDisplay(fr->display));
  int dh = valInt(getHeightDisplay(fr->display));
  int fw = valInt(fr->area->w);
  int fh = valInt(fr->area->h);
  int xborder, yborder, ycap;

  ws_frame_border(fr, &xborder, &yborder, &ycap);
  yborder += ycap;
  
  if ( !placed++ )
  { last_x = rand() % (dw-fw-2*PLACE_MARGIN);
    last_y = rand() % (dh-fh-2*PLACE_MARGIN);
  } else
  { last_x += PLACE_X_OFFSET;
    last_y += PLACE_Y_OFFSET;
  }

  if ( last_x + fw > dw - PLACE_MARGIN )
  { int xborder = GetSystemMetrics(SM_CXBORDER);

    last_x = PLACE_MARGIN;
    if ( last_x + fw > dw )
      last_x = 0;
  }
  if ( last_y + fh > dh - PLACE_MARGIN )
  { last_y = PLACE_MARGIN;
    if ( last_y + fh > dh )
      last_y = 0;
  }

  last_x = max(xborder, last_x);
  last_y = max(yborder, last_y);

  send(fr, NAME_set, toInt(last_x), toInt(last_y), EAV);
}


void
ws_raise_frame(FrameObj fr)
{ BringWindowToTop(getHwndFrame(fr));
}


void
ws_lower_frame(FrameObj fr)
{ SetWindowPos(getHwndFrame(fr),
	       HWND_BOTTOM,
	       0, 0, 0, 0,
	       SWP_NOMOVE|SWP_NOSIZE);
}


void
ws_topmost_frame(FrameObj fr, Bool topmost)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) )
  { SetWindowPos(hwnd,
		 topmost == ON ? HWND_TOPMOST : HWND_NOTOPMOST,
		 0, 0, 0, 0,
		 SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE);
  }
}


status
ws_attach_wm_prototols_frame(FrameObj fr)
{ succeed;
}


status
ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h)
{ if ( getHwndFrame(fr) )
  { RECT rect;

    GetWindowRect(getHwndFrame(fr), &rect);
    *x = rect.left;
    *y = rect.top;
    *w = rect.right - rect.left;
    *h = rect.bottom - rect.top;

    succeed;
  }

  fail;
}


void
ws_x_geometry_frame(FrameObj fr, Name spec)
{ char *s = strName(spec);
  UINT flags = SWP_NOACTIVATE|SWP_NOZORDER;
  int x, y, w, h;
  char signx[10], signy[10];
  int ok=0;
  WsFrame f = fr->ws_ref;

  outer_frame_area(fr, &x, &y, &w, &h, FALSE);

  switch(sscanf(s, "%dx%d%[+-]%d%[+-]%d", &w, &h, signx, &x, signy, &y))
  { case 2:
      flags |= SWP_NOMOVE;
      ok++;
      break;
    case 6:
      if ( signx[0] == '-' )
	x = valInt(getWidthDisplay(fr->display)) - x - w;
      if ( signy[0] == '-' )
	y = valInt(getHeightDisplay(fr->display)) - y - h;
      ok++;
      break;
    default:				/* [<Sign>]X<Sign>Y */
      if ( sscanf(s, "%[+-]%d%[+-]%d", signx, &x, signy, &y) != 4 )
      { signx[0] = '+';
	if ( sscanf(s, "%d%[+-]%d", signx, &x, signy, &y) != 3 )
	  break;
      }

      flags |= SWP_NOSIZE;
      if ( signx[0] == '-' )
	x = valInt(getWidthDisplay(fr->display)) - x - w;
      if ( signy[0] == '-' )
	y = valInt(getHeightDisplay(fr->display)) - y - h;
      ok++;
      break;
  }
  
  if ( f && ok )
  { SetWindowPos(f->hwnd,
		 HWND_TOP,		/* ignored */
		 x, y, w, h,
		 flags);
    f->placed = TRUE;
  }
}


void
ws_geometry_frame(FrameObj fr, Int px, Int py, Int pw, Int ph)
{ WsFrame f = fr->ws_ref;

  if ( f )
  { int x, y, w, h;
    UINT flags = SWP_NOACTIVATE|SWP_NOZORDER;

    outer_frame_area(fr, &x, &y, &w, &h, FALSE);

    if ( isDefault(pw) && isDefault(ph) )
      flags |= SWP_NOSIZE;
    else
      f->placed = TRUE;
    if ( isDefault(px) && isDefault(py) )
      flags |= SWP_NOMOVE;

    SetWindowPos(f->hwnd,
		 HWND_TOP,		/* ignored */
		 x, y, w, h,
		 flags);
  }
}


void
ws_frame_background(FrameObj fr, Any c)
{ Cprintf("ws_frame_background(%s, %s)\n", pp(fr), pp(c));
}


void
ws_border_frame(FrameObj fr, int b)
{
}


void
ws_busy_cursor_frame(FrameObj fr, CursorObj c)
{ WsFrame r = fr->ws_ref;

  if ( r )
  { POINT pt;
    HWND hwnd;
    Any obj;
    int setcursor;

    GetCursorPos(&pt);
    if ( (hwnd = WindowFromPoint(pt)) &&
	 (obj = getObjectFromHWND(hwnd)) &&
	 isProperObject(obj) &&
	 hasGetMethodObject(obj, NAME_frame) &&
	 (fr == qadGetv(obj, NAME_frame, 0, NULL)) )
      setcursor = TRUE;
    else
    { obj = NULL;
      setcursor = FALSE;
    }

    if ( isNil(c) )
    { if ( setcursor )
      { WsWindow ref;

	if ( obj && instanceOfObject(obj, ClassWindow) &&
	     (ref = ((PceWindow)obj)->ws_ref) &&
	     ref->hcursor )
	{ ZSetCursor(ref->hcursor);
	} else
	  ZSetCursor(LoadCursor(NULL, IDC_ARROW));
      }

      r->hbusy_cursor = NULL;
    } else
    { if ( isDefault(c) )
	c = getClassVariableValueObject(fr, NAME_busyCursor);

      if ( c )
      { if ( setcursor )
	{ r->hbusy_cursor = (HCURSOR)getXrefObject(c, fr->display);
	  ZSetCursor(r->hbusy_cursor);
	}
      }
    }
  }
}


void
ws_set_icon_frame(FrameObj fr)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) && fr->status != NAME_unlinking )
  { HICON icon;

    if ( notNil(fr->icon_image) )
    { if ( (icon = ws_icon_from_image(fr->icon_image)) )
      { SendMessage(hwnd,
		    WM_SETICON,
		    (WPARAM)ICON_SMALL,
		    (LPARAM)icon);
      }
    } else
    { SendMessage(hwnd,
		  WM_SETICON,
		  (WPARAM)ICON_SMALL,
		  (LPARAM)NULL);
    }

    if ( IsIconic(hwnd) )
      InvalidateRect(hwnd, NULL, TRUE);
  }
}


void
ws_set_icon_label_frame(FrameObj fr)
{
}


void
ws_set_icon_position_frame(FrameObj fr, int x, int y)
{
}


status
ws_get_icon_position_frame(FrameObj fr, int *x, int *y)
{ fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Enable/disable the windows we are modal too.   This is not really ok. If
frame fr1 is application modal and  opens fr2 which is application-modal
too, releasing fr2 will enable the other windows ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_enable_modal(FrameObj fr, Bool val)
{ BOOL enable = (val == ON ? TRUE : FALSE);

  if ( fr->modal == NAME_transient && notNil(fr->transient_for) )
  { HWND owner = getHwndFrame(fr->transient_for);

    DEBUG(NAME_modal, Cprintf("ws_enable_modal() %s %s\n",
			      pp(fr), enable ? "TRUE" : "FALSE"));
    EnableWindow(owner, enable);
  } else if ( fr->modal == NAME_application && notNil(fr->application) )
  { Cell cell;

    for_cell(cell, fr->application->members)
    { FrameObj appfr = cell->value;
      HWND hwnd;

      if ( appfr != fr && (hwnd = getHwndFrame(appfr)) )
	EnableWindow(hwnd, enable);
    }
  }
}


void
ws_status_frame(FrameObj fr, Name stat)
{ WsFrame f = fr->ws_ref;

  if ( stat == NAME_window || stat == NAME_fullScreen )
  { int how;

    if ( !f->placed )
    { ws_place_frame(fr);
      f->placed = TRUE;
    }

    if ( fr->kind == NAME_popup )
    { how = SW_SHOWNOACTIVATE; /*SW_SHOWNA;*/
    } else
    { if ( stat == NAME_window )
	how = SW_RESTORE;
      else
	how = SW_MAXIMIZE;
    }

    ShowWindow(f->hwnd, how);
    UpdateWindow(f->hwnd);
    ws_enable_modal(fr, OFF);
  } else
  { if ( stat == NAME_iconic )
    { ShowWindow(f->hwnd, SW_MINIMIZE);
    } else if ( stat == NAME_hidden )
    { if ( f->hwnd )
	ShowWindow(f->hwnd, SW_HIDE);
    }
    ws_enable_modal(fr, ON);
  }
}


void
ws_set_label_frame(FrameObj fr)
{ HWND hwnd = getHwndFrame(fr);

  if ( hwnd )
    SetWindowText(hwnd, strName(fr->label));
}


Image
ws_image_of_frame(FrameObj fr)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) )
  { HDC hdc = GetDC(NULL);
    RECT rect;
    Image image;
    int w, h;
    HBITMAP obm, bm;
    HDC hdcimg;
    Size size = getSizeDisplay(fr->display);

    GetWindowRect(hwnd, &rect);
    if ( rect.left < 0 ) rect.left = 0;
    if ( rect.top < 0 )  rect.top  = 0;
    if ( rect.bottom > valInt(size->h) ) rect.bottom = valInt(size->h);
    if ( rect.right >  valInt(size->w) ) rect.right  = valInt(size->w);
    
    w = rect.right - rect.left;
    h = rect.bottom - rect.top;

    DEBUG(NAME_image, Cprintf("hdc = %d, size = %dx%d\n", (int) hdc, w, h));
    image = answerObject(ClassImage, NIL,
			 toInt(w), toInt(h), NAME_pixmap, EAV);
    assign(image, display, fr->display);
    bm = ZCreateCompatibleBitmap(hdc, w, h);
    hdcimg = CreateCompatibleDC(hdc);
    obm = SelectObject(hdcimg, bm);

    BitBlt(hdcimg, 0, 0, w, h, hdc, rect.left, rect.top, SRCCOPY);

    SelectObject(hdcimg, obm);
    ZDeleteObject(hdcimg);
    ReleaseDC(hwnd, hdc);

    registerXrefObject(image, image->display, (void *) bm);
    return image;
  }

  fail;
}


void
ws_transient_frame(FrameObj fr, FrameObj fr2)
{
}


status
ws_postscript_frame(FrameObj fr)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) )
  { HDC hdc = GetDC(NULL);
    RECT rect;
    int w, h;
    int depth = GetDeviceCaps(hdc, BITSPIXEL);

    if ( depth >= 4 )
      depth = 4;
    else if ( depth == 3 )
      depth = 2;

    GetWindowRect(hwnd, &rect);
    w = rect.right - rect.left;
    h = rect.bottom - rect.top;

    ps_output("0 0 ~D ~D ~D greymap\n", w, h, depth);
    postscriptDC(hdc, rect.left, rect.top, rect.right, rect.bottom, depth);
    ps_output("\n");

    succeed;
  } else
    return errorPce(fr, NAME_mustBeOpenBeforePostscript);
}


void
ws_frame_cursor(FrameObj fr, CursorObj cursor)
{ WsFrame f = fr->ws_ref;

  if ( f )
  { if ( isDefault(cursor) )
      f->hcursor = LoadCursor(NULL, IDC_ARROW);
    else
      f->hcursor = (HCURSOR)getXrefObject(cursor, fr->display);

    ZSetCursor(f->hcursor);
  }
}


void
ws_grab_frame_pointer(FrameObj fr, Bool grab, CursorObj cursor)
{ HWND win;

  if ( (win = getHwndFrame(fr)) )
  { if ( grab == ON )
    { ws_frame_cursor(fr, cursor);
      SetCapture(win);
    } else
      ReleaseCapture();
  }
}



