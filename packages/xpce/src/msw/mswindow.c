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

#define WM_PCE_REDRAW (WM_USER+25)

static int WINAPI
do_window_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ PceWindow sw = getObjectFromHWND(hwnd);
  FrameObj fr;
  WsFrame wfr;
  WsWindow wsw;

  DEBUG(NAME_event,
	Cprintf("%s(0x%04x): MS-Windows event 0x%04x with 0x%04x/0x%08lx\n",
		pp(sw), hwnd, message, wParam, lParam));

  if ( !sw ) 
    return DefWindowProc(hwnd, message, wParam, lParam);
  wsw = sw->ws_ref;

  switch(message)
  { case WM_PCE_REDRAW:
      RedrawWindow(sw);
      return 0;

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
    { if ( !wParam && wsw && wsw->hwnd )
	PceWhDeleteWindow(wsw->hwnd);

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

      d_hdc(hdc, NIL, sw->background);
      GetClipBox(hdc, &rect);
      r_clear(rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top);
      d_done();

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

	DEBUG(NAME_window,
	      Cprintf("WM_DESTROY on %s, hwnd 0x%x\n",
		      pp(sw), hwnd)); 

	if ( wsw->drop )
	  DragAcceptFiles(hwnd, FALSE);
	PceWhDeleteWindow(hwnd);
	setHwndWindow(sw, 0);
	assocObjectToHWND(hwnd, NIL);
	
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
      { DEBUG(NAME_busyCursor,
	      Cprintf("Setting busy cursor for %s to %p\n",
		      pp(fr), wfr->hbusy_cursor));
	ZSetCursor(wfr->hbusy_cursor);
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
		    rval = postNamedEvent(ev, (Graphical) sw, DEFAULT, NAME_postEvent);
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


static int WINAPI
window_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ int rval;

  if ( InSendMessage() )
  { if ( pceMTTryLock(LOCK_PCE) )
    { rval = do_window_wnd_proc(hwnd, message, wParam, lParam);
      pceMTUnlock(LOCK_PCE);
    } else
    { PceWindow sw = getObjectFromHWND(hwnd);
      DEBUG(NAME_thread,
	    Cprintf("[Thread 0x%x] %s: message 0x%04x: could not lock\n",
		    GetCurrentThreadId(), pp(sw), message));
      return DefWindowProc(hwnd, message, wParam, lParam);
    }
  } else
  { pceMTLock(LOCK_PCE);
    rval = do_window_wnd_proc(hwnd, message, wParam, lParam);
    pceMTUnlock(LOCK_PCE);
  }

  return rval;
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
{ HWND hwnd;
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
  if ( sw->sensitive == OFF )
    style |= WS_DISABLED;

  hwnd = CreateWindow(WinWindowClass(),
		      strName(sw->name),
		      style,
		      valInt(sw->area->x), valInt(sw->area->y),
		      valInt(sw->area->w), valInt(sw->area->h),
		      parent_handle, NULL, PceHInstance, NULL);
  if ( !hwnd )
    return errorPce(sw, NAME_createFailed);

  DEBUG(NAME_window, Cprintf("Windows hwnd = %ld\n", (long) hwnd));

  setHwndWindow(sw, hwnd);
  assocObjectToHWND(hwnd, sw);

  if ( hasSendMethodObject(sw, NAME_dropFiles) )
  { WsWindow wsw = sw->ws_ref;

    wsw->drop = TRUE;
    DragAcceptFiles(hwnd, TRUE);
  };
		     
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
  HWND hwnd;

  pceMTLock(LOCK_PCE);
  hwnd = getHwndWindow(sw);
  clearing_update = FALSE;

  if ( hwnd && IsWindowVisible(hwnd) )
  { DEBUG(NAME_redraw, Cprintf("%s: InvalidateRect(%p)\n", pp(sw), hwnd));

    if ( isDefault(a) )
      InvalidateRect(hwnd, NULL, TRUE);
    else				/* actually not used ... */
    { RECT rect;

      rect.left   = valInt(a->x) + valInt(sw->scroll_offset->x);
      rect.right  = rect.left    + valInt(a->w);
      rect.top    = valInt(a->y) + valInt(sw->scroll_offset->y);
      rect.bottom = rect.top     + valInt(a->h);
  
      InvalidateRect(hwnd, &rect, clear);
    }
  } else
  { DEBUG(NAME_redraw, Cprintf("%s: hwnd %p invisible\n", pp(sw), hwnd));
  }
  pceMTUnlock(LOCK_PCE);
}


Int
ws_window_thread(PceWindow sw)
{ HWND hwnd;

  if ( (hwnd = getHwndWindow(sw)) )
  { DWORD owner = GetWindowThreadProcessId(hwnd, NULL);

    return toInt(owner);
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called from RedrawWindow(). If the window is owned by another thread, we
post a message to this thread asking for the the redraw.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ws_delayed_redraw_window(PceWindow sw)
{
#ifdef _REENTRANT
  HWND hwnd;
  
  if ( (hwnd = getHwndWindow(sw)) )
  { DWORD owner = GetWindowThreadProcessId(hwnd, NULL);
    DWORD me = GetCurrentThreadId();

    if ( owner == me )
      return FALSE;

    DEBUG(NAME_thread,
	  Cprintf("Delaying redraw of %s from thread 0x%x (me=0x%x)\n",
		  pp(sw), owner, me));

    PostMessage(hwnd, WM_PCE_REDRAW, 0, 0L);
    return TRUE;
  }
#endif

  return FALSE;
}


void
ws_redraw_window(PceWindow sw, IArea a, int clear)
{ WsWindow wsw = sw->ws_ref;

  if ( wsw && wsw->hwnd && IsWindowVisible(wsw->hwnd) )
  { RECT rect;

    rect.left   = a->x      + valInt(sw->scroll_offset->x);
    rect.right  = rect.left + a->w;
    rect.top    = a->y      + valInt(sw->scroll_offset->y);
    rect.bottom = rect.top  + a->h;

    if ( has_big_cursor() )
      restore_big_cursor_background();
    InvalidateRect(wsw->hwnd, &rect, FALSE);
    clearing_update = clear;
    UpdateWindow(wsw->hwnd);		/* will start WM_PAINT */
    clearing_update = FALSE;		/* ok for normal WM_PAINT call */
    if ( has_big_cursor() )
    { save_big_cursor_background();
      paint_big_cursor();
    }
  } else
  { DEBUG(NAME_redraw, Cprintf("ws_redraw_window(%s): invisible\n", pp(sw)));
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


static void
do_grab_window(PceWindow sw, Bool val)
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
ws_grab_pointer_window(PceWindow sw, Bool val)
{ if (  getHwndWindow(sw) )
  { if ( val == ON )
    { if ( getHeadChain(grabbedWindows) != sw )
      { do_grab_window(sw, ON);
	prependChain(grabbedWindows, sw);
      }
    } else
    { do_grab_window(sw, OFF);
      deleteChain(grabbedWindows, sw);
      if ( notNil(grabbedWindows->head) )
        do_grab_window(grabbedWindows->head->value, ON);
    }
  }
}


void
ws_ungrab_all(void)
{ if ( grabbedWindows )
    clearChain(grabbedWindows);
  
  ReleaseCapture();
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
    static int cursor_width = 0;
    static int cursor_height = 0;

    if ( !cursor_width )
    { cursor_width  = GetSystemMetrics(SM_CXCURSOR);
      cursor_height = GetSystemMetrics(SM_CYCURSOR);
    }

    exit_big_cursor();			/* should there be one */

    if ( isNil(c) )
      c = getClassVariableValueObject(sw, NAME_cursor);

    if ( notNil(c->image) &&
	 (valInt(c->image->size->w) > cursor_width ||
	  valInt(c->image->size->h) > cursor_height) &&
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


int
ws_enable_window(PceWindow sw, int enable)
{ HWND hwnd;

  if ( (hwnd=getHwndWindow(sw)) )
  { if ( EnableWindow(hwnd, enable) )
      succeed;
  }

  fail;
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
