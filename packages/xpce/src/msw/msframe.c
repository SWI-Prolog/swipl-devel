/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"

static int WINAPI frame_wnd_proc(HWND win, UINT msg, UINT wP, LONG lP);
static status     keyboard_event_frame(FrameObj fr, Any id);
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

    wndClass.style		= CS_HREDRAW|CS_VREDRAW;
    wndClass.lpfnWndProc	= (LPVOID) frame_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= sizeof(long);
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

    wndClass.style		= CS_HREDRAW|CS_VREDRAW|CS_SAVEBITS;
    wndClass.lpfnWndProc	= (LPVOID) frame_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= sizeof(long);
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


static int
IsDownKey(code)
{ int mask = GetKeyState(code);

  DEBUG(NAME_key, Cprintf("IsDownKey(%d): mask = 0x%x\n", code, mask));

  return mask & ~0xff;
}


static int
IsDownMeta(LONG lParam)
{ DEBUG(NAME_key, Cprintf("IsDownMeta(0x%lx)\n", lParam));

  return lParam & (1L << (30-1));	/* bit-29 is 1 if ALT is depressed */
					/* test tells me it is bit 30 ??? */
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

	    send(fr, NAME_resize, 0);
	    SetWindowText(hwnd, strName(fr->label));
	    assign(fr, status, NAME_open);
	    for_cell(cell, fr->members)
	      DisplayedGraphical(cell->value, ON);
	  }
	  break;
      }

      return 0;
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
	  send(cell->value, NAME_displayed, ON, 0);

	send(fr, NAME_mapped, ON, 0);

	assign(fr, status, NAME_open);
      } else
      { for_cell(cell, fr->members)
	{ if ( !onFlag(cell->value, F_FREED|F_FREEING) )
	    send(cell->value, NAME_displayed, OFF, 0);
	}

	if ( !isFreedObj(fr) || isFreeingObj(fr) )
	  send(fr, NAME_mapped, OFF, 0);

	assign(fr, status, NAME_hidden);
      }

      goto repaint;
    }

    case WM_SETFOCUS:
      send(fr, NAME_inputFocus, ON, 0);
      goto repaint;

    case WM_KILLFOCUS:
      send(fr, NAME_inputFocus, OFF, 0);
      goto repaint;

    case WM_QUERYNEWPALETTE:
    case WM_PALETTECHANGED:
      if ( instanceOfObject(fr->colour_map, ClassColourMap) &&
	   fr->colour_map->ws_ref )
	forwardColourMapChangeFrame(fr);
      break;

    case WM_ERASEBKGND:
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
	break;

    case WM_KEYDOWN:
    { Any id = NIL;

      switch((int) wParam)
      { case VK_DELETE:		id = toInt(127);	break;
        case VK_LEFT:		id = NAME_cursorLeft;	break;
        case VK_RIGHT:		id = NAME_cursorRight;	break;
        case VK_UP:		id = NAME_cursorUp;	break;
        case VK_DOWN:		id = NAME_cursorDown;	break;
        case VK_HOME:		id = NAME_cursorHome;	break;
	case VK_PRIOR:		id = NAME_pageUp;	break;
	case VK_NEXT:		id = NAME_pageDown;	break;
	case VK_END:		id = NAME_end;		break;
	
	case VK_SELECT:		id = NAME_select;	break;
	case VK_PRINT:		id = NAME_print;	break;
	case VK_EXECUTE:	id = NAME_execute;	break;
	case VK_INSERT:		id = NAME_insert;	break;
	case VK_HELP:		id = NAME_help;		break;
	case VK_MENU:		id = NAME_menu;		break;

        case VK_F1:		id = NAME_keyTop_1;	break;
        case VK_F2:		id = NAME_keyTop_2;	break;
        case VK_F3:		id = NAME_keyTop_3;	break;
        case VK_F4:		id = NAME_keyTop_4;	break;
        case VK_F5:		id = NAME_keyTop_5;	break;
        case VK_F6:		id = NAME_keyTop_6;	break;
        case VK_F7:		id = NAME_keyTop_7;	break;
        case VK_F8:		id = NAME_keyTop_8;	break;
        case VK_F9:		id = NAME_keyTop_9;	break;
        case VK_F10:		id = NAME_keyTop_10;	break;
        case '2':			/* ^@ */
	  if ( IsDownKey(VK_CONTROL) )
	    id = ZERO;
	  break;
	case 0xbd:			/* OEM specific Control('_') ??? */
	  if ( IsDownKey(VK_CONTROL) && !IsDownKey(VK_SHIFT) )
	    id = toInt(Control('_'));
	  break;
	case 0x56:			/* OEM specific 'V' ??? */
	  if ( IsDownKey(VK_CONTROL) && IsDownMeta(lParam) )
	    id = toInt(Control('V') + META_OFFSET);
	  break;
	case 0x49:			/* OEM specific 'I' ??? */
	  if ( IsDownKey(VK_CONTROL) && IsDownMeta(lParam) )
	    id = toInt(Control('I') + META_OFFSET);
	  break;
      }
      
      if ( notNil(id) && keyboard_event_frame(fr, id) )
	return 0;

      DEBUG(NAME_key, Cprintf("WM_KEYUP with key=0x%x\n", (int)wParam));

      break;
    }
    case WM_SYSCHAR:			/* handle ALT keys myself */
      if ( keyboard_event_frame(fr, toInt(wParam + META_OFFSET)) )
	return 0;
      break;

    case WM_CHAR:
    { Any id = toInt(wParam);

      if ( wParam == ' ' && IsDownKey(VK_CONTROL) )
        id = ZERO;			/* ^-space --> ^@ */
      else if ( wParam == 8 && !IsDownKey(VK_CONTROL) )
	id = NAME_backspace;

      if ( keyboard_event_frame(fr, id) )
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
	forwardReceiverCode(msg, fr, MainWindow(fr), 0);
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
      rval = send(fr, NAME_event, ev, 0);
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
{ FrameObj fr = (FrameObj) GetWindowLong(hwnd, GWL_DATA);
  int rval;

  if ( !fr )
    fr = current_frame;
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
       (sw  = (PceWindow) GetWindowLong(win, GWL_DATA)) &&
       instanceOfObject(sw, ClassWindow) )
    return sw;

  fail;
}


static status
keyboard_event_frame(FrameObj fr, Any id)
{ PceWindow sw;
  POINT pt;
  EventObj ev;
  AnswerMark mark;
  status rval = FALSE;
  Any receiver;

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
  ev = answerObject(ClassEvent, id, receiver, toInt(pt.x), toInt(pt.y), 0);

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


status
ws_create_frame(FrameObj fr)
{ HWND ref;
  DWORD style, exstyle = 0;
  int x, y, w, h;

  if ( fr->kind == NAME_toplevel )
  { style = WS_OVERLAPPEDWINDOW;
  } else if ( fr->kind == NAME_transient )
  { style = WS_DLGFRAME|WS_POPUP;
  } else /* popup */
  { style = WS_POPUP;
    if ( fr->border != ZERO )
      style |= WS_BORDER;
    exstyle |= WS_EX_TOOLWINDOW;
  }
    
  outer_frame_area(fr, &x, &y, &w, &h, TRUE);

  current_frame = fr;
  ref = CreateWindowEx(exstyle,
		       fr->kind == NAME_popup ? WinPopupFrameClass()
		       			      : WinFrameClass(),
		       strName(getIconLabelFrame(fr)),
		       style,
		       x, y, w, h,
		       NULL, NULL, PceHInstance, NULL);
		     
  if ( !ref )
    return errorPce(fr, NAME_xOpen, fr->display);

  setHwndFrame(fr, ref);
  SetWindowLong(ref, GWL_DATA, (LONG) fr);

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
  
  if ( !placed++ )
  { last_x = rand() % (dw-fw-2*PLACE_MARGIN);
    last_y = rand() % (dh-fh-2*PLACE_MARGIN);
  } else
  { last_x += PLACE_X_OFFSET;
    last_y += PLACE_Y_OFFSET;
  }

  if ( last_x + fw > dw - PLACE_MARGIN )
  { last_x = PLACE_MARGIN;
    if ( last_x + fw > dw )
      last_x = GetSystemMetrics(SM_CXBORDER);
  }
  if ( last_y + fh > dh - PLACE_MARGIN )
  { last_y = PLACE_MARGIN;
    if ( last_y + fh > dh )
      last_y = GetSystemMetrics(SM_CYCAPTION) +
	       GetSystemMetrics(SM_CYBORDER);
  }

  send(fr, NAME_set, toInt(last_x), toInt(last_y), 0);
}


void
ws_show_frame(FrameObj fr, Bool grab)
{ WsFrame f = fr->ws_ref;

  if ( f )
  { if ( !f->placed )
    { ws_place_frame(fr);

      f->placed = TRUE;
    }

    ShowWindow(f->hwnd, fr->kind == NAME_popup ? SW_SHOWNA : SW_RESTORE);
    UpdateWindow(f->hwnd);
  }
}


void
ws_unshow_frame(FrameObj fr)
{ HWND hwnd = getHwndFrame(fr);

  if ( hwnd )
    ShowWindow(hwnd, SW_HIDE);
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
  char signx[1], signy[1];
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
  { if ( isNil(c) )
    { POINT pt;
      HWND hwnd;
      PceWindow win;
      WsWindow ref;

      GetCursorPos(&pt);
      if ( (hwnd = WindowFromPoint(pt)) &&
#ifdef __WIN32__
	   (HANDLE) GetWindowLong(hwnd, GWL_HINSTANCE) == PceHInstance &&
#else
	   GetWindowWord(hwnd, GWW_HINSTANCE) == PceHInstance &&
#endif
	   (win = (PceWindow)GetWindowLong(hwnd, GWL_DATA)) &&
	   isProperObject(win) &&
	   instanceOfObject(win, ClassWindow) &&
	   (ref = win->ws_ref) &&
	   ref->hcursor )
      { ZSetCursor(ref->hcursor);
      } else
	ZSetCursor(LoadCursor(NULL, IDC_ARROW));

      r->hbusy_cursor = NULL;
      
    } else
    { if ( isDefault(c) )
	c = getResourceValueObject(fr, NAME_busyCursor);

      if ( c )
      { r->hbusy_cursor = (HCURSOR)getXrefObject(c, fr->display);

	ZSetCursor(r->hbusy_cursor);
      }
    }
  }
}


void
ws_set_icon_frame(FrameObj fr)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) &&
       IsIconic(hwnd) &&
       fr->destroying == OFF )
    InvalidateRect(hwnd, NULL, TRUE);
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


void
ws_iconify_frame(FrameObj fr)
{ ShowWindow(getHwndFrame(fr), SW_MINIMIZE);
}


void
ws_deiconify_frame(FrameObj fr)
{ ShowWindow(getHwndFrame(fr), SW_RESTORE);
}


void
ws_set_label_frame(FrameObj fr)
{ HWND hwnd = getHwndFrame(fr);

  if ( hwnd )
    SetWindowText(hwnd, strName(getIconLabelFrame(fr)));
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
			 toInt(w), toInt(h), NAME_pixmap, 0);
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



