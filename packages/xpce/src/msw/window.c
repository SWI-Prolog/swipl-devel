/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"

static PceWindow current_window; /* hack to avoid Windows timing problem */

static long FAR PASCAL _export window_wnd_proc(HWND win, UINT msg,
					       UINT wP, LONG lP);

static int clearing_update;		/* from ws_redraw_window() */

static char *
WinWindowClass()
{ static Name winclassname = NULL;
  static WNDCLASS wndClass;

  if ( !winclassname )
  { char buf[50];

    sprintf(buf, "PceWindow%d", PceHInstance);
    winclassname = CtoName(buf);

    wndClass.style		= CS_HREDRAW|CS_VREDRAW;
    wndClass.lpfnWndProc	= (LPVOID) window_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= sizeof(long);
    wndClass.hInstance		= PceHInstance;
    wndClass.hIcon		= NULL; /*LoadIcon(NULL, IDI_APPLICATION); */
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= CreateSolidBrush(COLOR_WINDOW + 1);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= strName(winclassname);

    RegisterClass(&wndClass);
  }

  return strName(winclassname);
}


static long FAR PASCAL _export
window_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ HDC hdc;
  PceWindow sw = (PceWindow) GetWindowLong(hwnd, GWL_DATA);
  FrameObj fr;
  WsFrame wfr;

  if ( !sw )
    sw = current_window;

  switch(message)
  { case WM_CREATE:
      break;

    case WM_SIZE:			/* window changed size */
    { int w = LOWORD(lParam);
      int h = HIWORD(lParam);

      if ( notNil(sw->device) )		/* subwindow */
      { int p2 = valInt(sw->pen) * 2;

	w += p2;
	h += p2;
      }

      assign(sw->area, w, toInt(w));
      assign(sw->area, h, toInt(h));
      qadSendv(sw, NAME_resize, 0, NULL);
      changedUnionWindow(sw, sw->area);
      return 0;
    }

    case WM_MOVE:			/* window moved */
    { int x = LOWORD(lParam);
      int y = HIWORD(lParam);

      assign(sw->area, x, toInt(x));
      assign(sw->area, y, toInt(y));

      return 0;
    }

    case WM_SETFOCUS:
      DEBUG(NAME_focus, printf("Received FocusIn on %s\n", pp(sw)));
      assign(sw, input_focus, ON);
      return 0;

    case WM_KILLFOCUS:
      DEBUG(NAME_focus, printf("Received FocusOut on %s\n", pp(sw)));
      assign(sw, input_focus, OFF);
      return 0;

    case WM_ERASEBKGND:
    { HDC hdc = (HDC) wParam;
      RECT rect;
      COLORREF rgb = (COLORREF) getXrefObject(sw->background,
					      getDisplayWindow(sw));
      HBRUSH hbrush = CreateSolidBrush(rgb);
      
      DEBUG(NAME_redraw, printf("Clearing background %d %d %d %d of %s\n",
				rect.left, rect.top,
				rect.right - rect.left, rect.bottom - rect.top,
				pp(sw)));

      GetClipBox(hdc, &rect);
      FillRect(hdc, &rect, hbrush);
      DeleteObject(hbrush);

      return 1;				/* non-zero: I've erased it */
    }

    case WM_PAINT:
    { RECT rect;
      struct iarea a;
      int clear;

      DEBUG(NAME_redraw, printf("%s (%ld) received WM_PAINT\n",
				pp(sw), (long)hwnd));

      if ( sw->displayed == OFF )
	send(sw, NAME_displayed, ON, 0);

      if ( d_mswindow(sw, &a, clearing_update) )
	RedrawAreaWindow(sw, &a, clearing_update);
      d_done();

      return 0;
    }

    case WM_DESTROY:
      setHwndWindow(sw, 0);
      break;
  }

  if ( (fr = getFrameWindow(sw)) &&
       (wfr = fr->ws_ref) &&
       wfr->hbusy_cursor )
  { SetCursor(wfr->hbusy_cursor);
  } else
  { EventObj ev;
    AnswerMark mark;
    status rval = FALSE;
    markAnswerStack(mark);
  
    if ( (ev = messageToEvent(hwnd, message, wParam, lParam)) )
    { addCodeReference(ev);
      if ( isDownEvent(ev) )
	ws_grab_pointer_window(sw, ON);	/* needs counting? */
      if ( isUpEvent(ev) )
	ws_grab_pointer_window(sw, OFF);
      rval = postEvent(ev, (Graphical) sw, DEFAULT);
      delCodeReference(ev);
      freeableObj(ev);
    }
    rewindAnswerStack(mark, NIL);

    if ( ev )				/* rval won't update on failing */
      RedrawDisplayManager(TheDisplayManager());

    { WsWindow w = sw->ws_ref;

      if ( w )
      { if ( w->hcursor && GetCursor() != w->hcursor )
	  SetCursor(w->hcursor);
	else
	  move_big_cursor();
      }
    }

    if ( rval )
      return 0;
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}


status
ws_created_window(PceWindow sw)
{ if ( getHwndWindow(sw) )
    succeed;

  fail;
}


void
ws_uncreate_window(PceWindow sw)
{ if ( ws_created_window(sw) )
  { DestroyWindow(getHwndWindow(sw));
    setHwndWindow(sw, 0);
  }
}


status
ws_create_window(PceWindow sw, PceWindow parent)
{ HWND ref;
  HWND parent_handle;
  DWORD style = WS_CHILD|WS_CLIPCHILDREN|WS_CLIPSIBLINGS|WS_VISIBLE;

  DEBUG(NAME_window, printf("ws_create_window(%s %s)\n", pp(sw), pp(parent)));

  if ( isDefault(parent) )		/* window in frame */
  { parent_handle = getHwndFrame(sw->frame);
  } else				/* sub-window */
  { parent_handle = getHwndWindow(parent);
  }

  if ( sw->pen != ZERO )
    style |= WS_BORDER;

  current_window = sw;		/* hack to avoid timing problem! */

  ref = CreateWindow(WinWindowClass(),
		     strName(sw->name),
		     style,
		     valInt(sw->area->x), valInt(sw->area->y),
		     valInt(sw->area->w), valInt(sw->area->h),
		     parent_handle, NULL, PceHInstance, NULL);
		     
  if ( !ref )
    return errorPce(sw, NAME_createFailed);

  DEBUG(NAME_window, printf("Windows ref = %ld\n", (long) ref));

  setHwndWindow(sw, ref);
  SetWindowLong(ref, GWL_DATA, (LONG) sw);

  if ( notDefault(parent) )		/* make a sub-window */
    send(sw, NAME_displayed, ON, 0);

  succeed;
}


void
ws_manage_window(PceWindow sw)
{
}


void
ws_unmanage_window(PceWindow sw)
{
}


void
ws_reassociate_ws_window(PceWindow from, PceWindow to)
{ HWND win = getHwndWindow(from);

  setHwndWindow(to, win);
  SetWindowLong(win, GWL_DATA, (LONG) to);
  setHwndWindow(from, NULL);
}


void
ws_geometry_window(PceWindow sw, int x, int y, int w, int h, int pen)
{ SetWindowPos(getHwndWindow(sw),
	       HWND_TOP,		/* ignored */
	       x, y, w, h,
	       SWP_NOACTIVATE|SWP_NOZORDER);
}


void
ws_invalidate_window(PceWindow sw, Area a)
{ int clear = FALSE;

  clearing_update = FALSE;

  if ( isDefault(a) )
    InvalidateRect(getHwndWindow(sw), NULL, TRUE);
  else					/* actually not used ... */
  { RECT rect;

    rect.left   = valInt(a->x) + valInt(sw->scroll_offset->x);
    rect.right  = rect.left    + valInt(a->w);
    rect.top    = valInt(a->y) + valInt(sw->scroll_offset->y);
    rect.bottom = rect.top     + valInt(a->h);
  
    DEBUG(NAME_redraw, printf("Invalidating %s: (%d %d %d %d)\n",
			      pp(sw), rect.left, rect.top,
			      rect.right - rect.left,
			      rect.bottom - rect.top));

    InvalidateRect(getHwndWindow(sw), &rect, clear);
  }
}


void
ws_redraw_window(PceWindow sw, IArea a, int clear)
{ HWND hwnd = getHwndWindow(sw);

  if ( hwnd )
  { RECT rect;

    rect.left   = a->x      + valInt(sw->scroll_offset->x);
    rect.right  = rect.left + a->w;
    rect.top    = a->y      + valInt(sw->scroll_offset->y);
    rect.bottom = rect.top  + a->h;

    InvalidateRect(hwnd, &rect, FALSE);
    clearing_update = clear;
    UpdateWindow(hwnd);			/* will start WM_PAINT */
    clearing_update = FALSE;		/* ok for normal WM_PAINT call */
  }
}


void
ws_scroll_window(PceWindow sw, int dx, int dy)
{ HWND hwnd;

  if ( (hwnd = getHwndWindow(sw)) )
    ScrollWindowEx(hwnd, dx, dy, NULL, NULL, NULL, NULL,
		   SW_ERASE|SW_INVALIDATE);
}


void
ws_grab_keyboard_window(PceWindow sw, Bool val)
{
}


void
ws_grab_pointer_window(PceWindow sw, Bool val)
{ HWND win;

  if ( (win = getHwndWindow(sw)) )
  { if ( val == ON )
      SetCapture(win);
    else
      ReleaseCapture();
  }
}


void
ws_ungrab_all(void)
{ ReleaseCapture();
}


void
ws_move_pointer(PceWindow sw, int x, int y)
{ if ( ws_created_window(sw) )
  { int ox, oy;
    FrameObj fr;

    if ( frame_offset_window(sw, &fr, &ox, &oy) )
    { x += ox + valInt(fr->area->x);
      y += oy + valInt(fr->area->y);

      SetCursorPos(x, y);
    }
  }
}


void
ws_window_cursor(PceWindow sw, CursorObj c)
{ if ( ws_created_window(sw) )
  { WsWindow w = sw->ws_ref;
    
    exit_big_cursor();			/* should there be one */

    if ( notNil(c->image) &&
	 (valInt(c->image->size->w) > 32 || valInt(c->image->size->h) > 32) &&
	 sw->focus_cursor == c )
    { start_big_cursor(c);
      w->hcursor = NULL;
    } else
    { w->hcursor = (HCURSOR)getXrefObject(c,
					  getDisplayGraphical((Graphical)sw));
      SetCursor(w->hcursor);
    }
  }
}


void
ws_window_background(PceWindow sw, Colour c)
{ HWND hwnd;

  if ( (hwnd = getHwndWindow(sw)) )
  { DEBUG(NAME_background, printf("Invalidating %s for clear\n", pp(sw)));
    InvalidateRect(hwnd, NULL, TRUE);
  }
}


void
ws_raise_window(PceWindow sw)
{ ShowWindow(getHwndWindow(sw), SW_SHOW);
}


void
ws_lower_window(PceWindow sw)
{ ShowWindow(getHwndWindow(sw), SW_HIDE);
}
