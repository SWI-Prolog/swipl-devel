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

#include "include.h"

static PceWindow current_window; /* hack to avoid Windows timing problem */

static int WINAPI window_wnd_proc(HWND win, UINT msg, UINT wP, LONG lP);

static int clearing_update;		/* from ws_redraw_window() */
static int invert_window = FALSE;	/* invert the window */

#ifndef MAXPATHLEN
#define MAXPATHLEN 512			/* drag-and-drop */
#endif

static char *
WinWindowClass()
{ static Name winclassname = NULL;
  static WNDCLASS wndClass;

  if ( !winclassname )
  { char buf[50];

    sprintf(buf, "PceWindow%ld", (long)PceHInstance);
    winclassname = CtoName(buf);

    wndClass.style		= 0/*CS_HREDRAW|CS_VREDRAW*/;
    wndClass.lpfnWndProc	= (LPVOID) window_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= PceHInstance;
    wndClass.hIcon		= NULL; /*LoadIcon(NULL, IDI_APPLICATION); */
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= strName(winclassname);

    RegisterClass(&wndClass);
  }

  return strName(winclassname);
}


HPALETTE
window_palette(PceWindow sw)
{ FrameObj fr = getFrameWindow(sw, DEFAULT); 

  if ( fr )
    return frame_palette(fr);

  return NULL;
}


static FrameObj
getExistingFrameWindow(PceWindow sw)
{ PceWindow root = (PceWindow) getRootGraphical((Graphical) sw);
  
  if ( instanceOfObject(root, ClassWindow) &&
       instanceOfObject(root->frame, ClassFrame) )
    answer(root->frame);

  fail;
}



static int WINAPI
window_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ PceWindow sw = getObjectFromHWND(hwnd);
  FrameObj fr;
  WsFrame wfr;
  WsWindow wsw;

  DEBUG(NAME_event,
	Cprintf("%s(0x%04x): MS-Windows event 0x%04x with 0x%04x/0x%08lx\n",
		pp(sw), hwnd, message, wParam, lParam));

  if ( !sw ) 
  { if ( !(sw = current_window) )
      return DefWindowProc(hwnd, message, wParam, lParam);
  }
  wsw = sw->ws_ref;

  switch(message)
  { case WM_CREATE:
      ServiceMode(is_service_window(sw),
		  if ( hasSendMethodObject(sw, NAME_dropFiles) )
		    DragAcceptFiles(hwnd, TRUE));
      goto cascade;

    case WM_DROPFILES:
    { HDROP hdrop = (HDROP) wParam;
      POINT pt;
      int nfiles;

      if ( DragQueryPoint(hdrop, &pt)  &&
	   (nfiles = DragQueryFile(hdrop, (UINT)-1, NULL, 0)) >= 0 )
      { Chain files;
      	Point pos;
	char buf[MAXPATHLEN];
	AnswerMark mark;
	int i;

	ServiceMode(is_service_window(sw),
		    { markAnswerStack(mark);
		      files = answerObject(ClassChain, EAV);
		      pos   = answerObject(ClassPoint,
					   toInt(pt.x), toInt(pt.y), EAV);

		      for(i=0; i<nfiles; i++)
		      { int namlen;

			namlen = DragQueryFile(hdrop, i, buf, sizeof(buf)-1);
			buf[namlen] = EOS;
			appendChain(files, CtoName(buf));
		      }
			  
		      DragFinish(hdrop);		/* reclaims memory */
	
		      send(sw, NAME_dropFiles, files, pos, EAV);
		      RedrawDisplayManager(TheDisplayManager());
		      rewindAnswerStack(mark, NIL);
		    })
      }

      return 0;
    }

    case WM_SIZE:			/* window changed size */
    { int w = LOWORD(lParam);
      int h = HIWORD(lParam);
      Area a = sw->area;
      Int ow = a->w, oh = a->h;
      
      if ( notNil(sw->device) )		/* subwindow */
      { int p2 = valInt(sw->pen) * 2;

	w += p2;
	h += p2;
      }

      ServiceMode(is_service_window(sw),
		  assign(a, w, toInt(w));
		  assign(a, h, toInt(h));
		  qadSendv(sw, NAME_resize, 0, NULL);
		  changedUnionWindow(sw, a->x, a->y, ow, oh));

      return 0;
    }

    case WM_MOVE:			/* window moved */
    { int x = LOWORD(lParam);
      int y = HIWORD(lParam);

      if ( isNil(sw->device) )		/* Window is not used as graphical */
      { ServiceMode(is_service_window(sw),
		    assign(sw->area, x, toInt(x));
		    assign(sw->area, y, toInt(y)));
      }

      return 0;
    }

    case WM_SHOWWINDOW:
    { HWND hwnd;

      if ( !wParam && (hwnd = getHwndWindow(sw)) )
      {	WsWindow wsw = sw->ws_ref;

	if ( wsw )
	{ PceWhDeleteWindow(wsw->hwnd);
	  wsw->open = FALSE;
	}
      }

      goto cascade;
    }

    case WM_SETFOCUS:
    case WM_KILLFOCUS:
    { Bool val = (message == WM_SETFOCUS ? ON : OFF);

      ServiceMode(is_service_window(sw),
		  DEBUG(NAME_focus,
			Cprintf("Received Focus %s on %s\n", pp(val), pp(sw)));
		  send(sw, NAME_inputFocus, val, EAV);
		  RedrawDisplayManager(TheDisplayManager()));
		  /*assign(sw, input_focus, val)*/

      DEBUG(NAME_focus, Cprintf("\tFocus Request Handled!\n"));

      goto cascade;
    }

    case WM_ERASEBKGND:
    { HDC hdc = (HDC) wParam;
      RECT rect;

#if 1
      d_hdc(hdc, NIL, sw->background);
      GetClipBox(hdc, &rect);
      r_clear(rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top);
      d_done();
#else
      HBRUSH hbrush;
      HPALETTE hpal = window_palette(sw), ohpal = NULL;
      COLORREF rgb = (COLORREF) getXrefObject(sw->background,
					      getDisplayWindow(sw));

      if ( hpal )
      { int n;

	ohpal = SelectPalette(hdc, hpal, FALSE);
	RealizePalette(hdc);
	n = GetNearestPaletteIndex(hpal, rgb);
	rgb = PALETTEINDEX(n);
      } else
	rgb = GetNearestColor(hdc, rgb);

      hbrush = ZCreateSolidBrush(rgb);
      GetClipBox(hdc, &rect);
      FillRect(hdc, &rect, hbrush);
      ZDeleteObject(hbrush);

      if ( ohpal )
	SelectPalette(hdc, ohpal, FALSE);

      DEBUG(NAME_redraw, Cprintf("Cleared background %d %d %d %d of %s\n",
				 rect.left, rect.top,
				 rect.right - rect.left,
				 rect.bottom - rect.top,
				 pp(sw)));
#endif /*1*/

      return 1;				/* non-zero: I've erased it */
    }

    case WM_PAINT:
    if ( invert_window )		/* see ws_flash_window() */
    { HWND hwnd = getHwndWindow(sw);
      PAINTSTRUCT ps;
      HDC hdc = BeginPaint(hwnd, &ps);

      InvertRect(hdc, &ps.rcPaint);

      EndPaint(hwnd, &ps);
    } else
    { iarea a;
      
      if ( !clearing_update )
      { WsWindow wsw;

	if ( (wsw = sw->ws_ref) )
	  wsw->open = TRUE;
      }

      ServiceMode(is_service_window(sw),
		  DEBUG(NAME_redraw,
			Cprintf("%s (0x%04x) received WM_PAINT (%s clear)\n",
				pp(sw), (long)hwnd,
				clearing_update ? "" : "no"));

		  if ( sw->displayed == OFF )
		    send(sw, NAME_displayed, ON, EAV);

		  if ( d_mswindow(sw, &a, clearing_update) )
		  { DEBUG(NAME_redraw,
			  Cprintf("Redrawing %d %d %d %d\n",
				  a.x, a.y, a.w, a.h));
		    RedrawAreaWindow(sw, &a, clearing_update);
		  } else
		  { DEBUG(NAME_redraw,
			  Cprintf("d_mswindow() failed: empty area\n"));
		  }
		  d_done());
    }
    return 0;

    case WM_DESTROY:
    { HWND hwnd = getHwndWindow(sw);

      if ( hwnd )
      { WNDPROC oproc = wsw->saved_window_procedure;

	ServiceMode(is_service_window(sw),
		    DEBUG(NAME_window,
			  Cprintf("WM_DESTROY on %s, hwnd 0x%x\n",
				  pp(sw), hwnd)); 
		    if ( hasSendMethodObject(sw, NAME_dropFiles) )
		      DragAcceptFiles(hwnd, FALSE);
		    PceWhDeleteWindow(hwnd);
		    setHwndWindow(sw, 0);
		    assocObjectToHWND(hwnd, NIL));
	
	if ( oproc )			/* refining alien window */
					/* see winHandleWindow() below */
	{ SetWindowLong(hwnd, GWL_WNDPROC, (LONG) oproc);
	  return CallWindowProc(oproc, hwnd, message, wParam, lParam);
	}

	return 0;
      }

      return DefWindowProc(hwnd, message, wParam, lParam);
    }

 
    case WM_SETCURSOR:
    { WsWindow w;
      WsFrame wfr;

      if ( (fr = getExistingFrameWindow(sw)) &&
	   (wfr = fr->ws_ref) &&
	   wfr->hbusy_cursor )
      { ZSetCursor(wfr->hbusy_cursor);
      } else
      { if ( (w = sw->ws_ref) )
	{ if ( !w->hcursor )
	    w->hcursor = LoadCursor(NULL, IDC_ARROW);
	  ZSetCursor(w->hcursor);
	}
      }

      return 1;
    }
  }

  if ( (fr = getExistingFrameWindow(sw)) &&
       (wfr = fr->ws_ref) &&
       wfr->hbusy_cursor )
  { ZSetCursor(wfr->hbusy_cursor);
  } else
  { EventObj ev;
    AnswerMark mark;
    status rval = FALSE;
    FrameObj bfr;
  
    if ( sw->sensitive == OFF )
      goto cascade;

    if ( (bfr=blockedByModalFrame(fr)) )
    { switch(message)
      {	case WM_KEYDOWN:
	case WM_SYSCHAR:
	case WM_CHAR:
	case WM_LBUTTONUP:
	case WM_MBUTTONUP:
	case WM_RBUTTONUP:
	case WM_LBUTTONDOWN:
	case WM_MBUTTONDOWN:
	case WM_RBUTTONDOWN:
	  send(fr, NAME_bell, EAV);
#ifdef WM_MOUSEWHEEL
	case WM_MOUSEWHEEL:
	  send(bfr, NAME_expose, EAV);
#endif
      }

      goto cascade;
    }

    ServiceMode(is_service_window(sw),
		{ markAnswerStack(mark);

		  if ( (ev = messageToEvent(hwnd, message, wParam, lParam)) )
		  { WsWindow w = sw->ws_ref;
  
		    if ( message != WM_WINENTER && message != WM_WINEXIT )
		      PceEventInWindow(hwnd);
  
		    addCodeReference(ev);
		    if ( isDownEvent(ev) && !w->capture )
		      SetCapture(hwnd);
		    else if ( isUpEvent(ev) && !w->capture )
		      ReleaseCapture();
		    rval = postEvent(ev, (Graphical) sw, DEFAULT);
		    delCodeReference(ev);
		    freeableObj(ev);
		  }

		  rewindAnswerStack(mark, NIL);

		  if ( ev )		/* rval will not update on failing */
		    RedrawDisplayManager(TheDisplayManager());
		});

    move_big_cursor();			/* only if we have one */

    if ( rval )
      return 0;
  }

cascade:
  if ( wsw && wsw->saved_window_procedure )
    return CallWindowProc(wsw->saved_window_procedure,
			  hwnd, message, wParam, lParam);
  else
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
{ HWND hwnd;

  if ( (hwnd = getHwndWindow(sw)) )
  { DEBUG(NAME_window,
	  Cprintf("ws_uncreate_window(%s) (=0x%04x)\n", pp(sw), hwnd));
    setHwndWindow(sw, 0);
    assocObjectToHWND(hwnd, NIL);
    PceWhDeleteWindow(hwnd);
    DestroyWindow(hwnd);
    DEBUG(NAME_window,
	  Cprintf("ws_uncreate_window(%s) (=0x%04x) completed\n",
		  pp(sw), hwnd));
  }
}


status
ws_create_window(PceWindow sw, PceWindow parent)
{ HWND ref;
  HWND parent_handle;
  DWORD style = WS_CHILD|WS_CLIPCHILDREN|WS_CLIPSIBLINGS|WS_VISIBLE;

  DEBUG(NAME_window, Cprintf("ws_create_window(%s %s)\n", pp(sw), pp(parent)));

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

  DEBUG(NAME_window, Cprintf("Windows ref = %ld\n", (long) ref));

  setHwndWindow(sw, ref);
  assocObjectToHWND(ref, sw);
  current_window = NULL;

  if ( notDefault(parent) )		/* make a sub-window */
    send(sw, NAME_displayed, ON, EAV);

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
  if ( win )
    assocObjectToHWND(win, to);

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
ws_topmost_window(PceWindow sw, Bool topmost)
{ HWND hwnd;

  if ( (hwnd = getHwndWindow(sw)) )
  {
#if 0					/* somehow doesn't work */
    SetWindowPos(hwnd,
		 topmost == ON ? HWND_TOPMOST : HWND_NOTOPMOST,
		 0, 0, 0, 0,
		 SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE);
#else
    if ( topmost == ON )
      BringWindowToTop(hwnd);
#endif
  }
}


void
ws_invalidate_window(PceWindow sw, Area a)
{ int clear = FALSE;
  HWND hwnd = getHwndWindow(sw);

  clearing_update = FALSE;

  if ( hwnd && IsWindowVisible(hwnd) )
  { if ( isDefault(a) )
      InvalidateRect(hwnd, NULL, TRUE);
    else				/* actually not used ... */
    { RECT rect;

      rect.left   = valInt(a->x) + valInt(sw->scroll_offset->x);
      rect.right  = rect.left    + valInt(a->w);
      rect.top    = valInt(a->y) + valInt(sw->scroll_offset->y);
      rect.bottom = rect.top     + valInt(a->h);
  
      InvalidateRect(hwnd, &rect, clear);
    }
  }
}


void
ws_redraw_window(PceWindow sw, IArea a, int clear)
{ WsWindow wsw = sw->ws_ref;

  if ( wsw && wsw->hwnd && wsw->open )
  { RECT rect;

    rect.left   = a->x      + valInt(sw->scroll_offset->x);
    rect.right  = rect.left + a->w;
    rect.top    = a->y      + valInt(sw->scroll_offset->y);
    rect.bottom = rect.top  + a->h;

    InvalidateRect(wsw->hwnd, &rect, FALSE);
    clearing_update = clear;
    UpdateWindow(wsw->hwnd);		/* will start WM_PAINT */
    clearing_update = FALSE;		/* ok for normal WM_PAINT call */
  }
}


void
ws_flash_window(PceWindow sw, int msecs)
{ HWND hwnd = getHwndWindow(sw);

  if ( hwnd && sw->displayed == ON )
  { invert_window = TRUE;
    InvalidateRect(hwnd, NULL, FALSE);
    UpdateWindow(hwnd);
    msleep(msecs);
    InvalidateRect(hwnd, NULL, FALSE);
    UpdateWindow(hwnd);
    invert_window = FALSE;
  }
}


void
ws_flash_area_window(PceWindow sw, int x, int y, int w, int h, int msecs)
{ HWND hwnd = getHwndWindow(sw);

  if ( hwnd && sw->displayed == ON )
  { RECT rect;

    rect.left   = x + valInt(sw->scroll_offset->x);
    rect.right  = rect.left + w;
    rect.top    = y + valInt(sw->scroll_offset->y);
    rect.bottom = rect.top  + h;
    
    invert_window = TRUE;
    InvalidateRect(hwnd, &rect, FALSE);
    UpdateWindow(hwnd);
    msleep(msecs);
    InvalidateRect(hwnd, &rect, FALSE);
    UpdateWindow(hwnd);
    invert_window = FALSE;
  }
}


void
ws_scroll_window(PceWindow sw, int dx, int dy)
{ HWND hwnd;

  if ( (hwnd = getHwndWindow(sw)) && sw->displayed == ON )
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
  { WsWindow w = sw->ws_ref;

    if ( val == ON )
      SetCapture(win);
    else
      ReleaseCapture();

    w->capture = (val == ON);
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

    if ( isNil(c) )
      c = getClassVariableValueObject(sw, NAME_cursor);

    if ( notNil(c->image) &&
	 (valInt(c->image->size->w) > 32 || valInt(c->image->size->h) > 32) &&
	 sw->focus_cursor == c )
    { start_big_cursor(c);
      w->hcursor = NULL;
    } else
    { POINT pt;

      w->hcursor = (HCURSOR)getXrefObject(c,
					  getDisplayGraphical((Graphical)sw));

      GetCursorPos(&pt);
      if ( getHwndWindow(sw) == WindowFromPoint(pt) )
	ZSetCursor(w->hcursor);
    }
  }
}


void
ws_window_background(PceWindow sw, Any c)
{ HWND hwnd;

  if ( (hwnd = getHwndWindow(sw)) && sw->displayed == ON )
  { DEBUG(NAME_background, Cprintf("Invalidating %s for clear\n", pp(sw)));
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


		 /*******************************
		 *     EMBEDDING WINDOWS	*
		 *******************************/

Int
getWinHandleWindow(PceWindow sw)
{ HWND hwnd;

  if ( (hwnd = getHwndWindow(sw)) )
    answer(toInt(hwnd));

  fail;
}


status
winHandleWindow(PceWindow sw, Int handle)
{ HWND hwnd = (HWND)valInt(handle);
  WsWindow w;
  RECT rect;
  
  while( notNil(sw->decoration) )
    sw = sw->decoration;

  setHwndWindow(sw, hwnd);
  assocObjectToHWND(hwnd, sw);
  w = sw->ws_ref;
  w->saved_window_procedure = (WNDPROC)GetWindowLong(hwnd, GWL_WNDPROC);
  SetWindowLong(hwnd, GWL_WNDPROC, (LONG) window_wnd_proc);
  GetWindowRect(hwnd, &rect);
  ServiceMode(is_service_window(sw),
	      { Area a = sw->area;
		int p = 1;		/* TBD */
		int w = rect.right - rect.left;
		int h = rect.bottom - rect.top;
		int x = rect.left;
		int y = rect.top;
		Int ow = a->w;
		Int oh = a->h;

		x += p;
		y += p;
		w -= 2*p;
		h -= 2*p;
		assign(a, x, toInt(x));
		assign(a, y, toInt(y));
		assign(a, w, toInt(w));
		assign(a, h, toInt(h));
		qadSendv(sw, NAME_resize, 0, NULL);
		changedUnionWindow(sw, a->x, a->y, ow, oh);
	      });

  send(sw, NAME_displayed, ON, EAV);

  succeed;
}
