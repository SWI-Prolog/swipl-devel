/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"


HWND
getHwndFrame(FrameObj fr)
{ WsFrame f;

  if ( (f = fr->ws_ref) )
  { DEBUG(NAME_window, printf("HWND of %s --> 0x%04x\n",
			      pp(fr), f->hwnd));
    return f->hwnd;
  }

  return 0;
}


void
setHwndFrame(FrameObj fr, HWND ref)
{ WsFrame f;

  if ( ref )
  { if ( !(f=fr->ws_ref) )
    { fr->ws_ref = alloc(sizeof(ws_frame));
      f = fr->ws_ref;
    }

    f->hwnd         = ref;
    f->hbusy_cursor = NULL;
    f->placed       = FALSE;
  } else
  { if ( fr->ws_ref )
    { unalloc(sizeof(ws_frame), fr->ws_ref);
      fr->ws_ref = NULL;
    }
  }
}


HWND
getHwndWindow(PceWindow sw)
{ WsWindow w;

  if ( (w = sw->ws_ref) )
  { DEBUG(NAME_window, printf("HWND of %s --> 0x%04x\n",
			      pp(sw), w->hwnd));
    return w->hwnd;
  }

  return 0;
}


void
setHwndWindow(PceWindow sw, HWND ref)
{ WsWindow w;

  if ( ref )
  { if ( !(w=sw->ws_ref) )
    { sw->ws_ref = alloc(sizeof(ws_window));
      w = sw->ws_ref;
    }

    w->hwnd = ref;
    w->hcursor = 0;
    w->capture = 0;
  } else
  { if ( sw->ws_ref )
    { unalloc(sizeof(ws_window), sw->ws_ref);
      sw->ws_ref = NULL;
    }
    assign(sw, displayed, OFF);
  }
}


		 /*******************************
		 *	  EVENT HANDLING	*
		 *******************************/

EventObj
messageToEvent(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ Any id = NIL;
  Int x = DEFAULT, y = DEFAULT;
  Int buttons = DEFAULT;
  Any window = (Any)GetWindowLong(hwnd, GWL_DATA);
  int mouse_ev = FALSE;

  switch(message)
  { case WM_CHAR:
      id = toInt((unsigned char)wParam);
      break;
					/* BEGIN MOUSE STUFF */
    case WM_LBUTTONUP:
      id = NAME_msLeftUp;
      mouse_ev++;
      break;
    case WM_LBUTTONDOWN:
      id = NAME_msLeftDown;
      mouse_ev++;
      break;
    case WM_MBUTTONUP:
      id = NAME_msMiddleUp;
      mouse_ev++;
      break;
    case WM_MBUTTONDOWN:
      id = NAME_msMiddleDown;
      mouse_ev++;
      break;
    case WM_RBUTTONUP:
      id = NAME_msRightUp;
      mouse_ev++;
      break;
    case WM_RBUTTONDOWN:
      id = NAME_msRightDown;
      mouse_ev++;
      break;
    case WM_MOUSEMOVE:
    { if ( wParam & MK_LBUTTON )
	id = NAME_msLeftDrag;
      else if ( wParam & MK_MBUTTON )
	id = NAME_msMiddleDrag;
      else if ( wParam & MK_RBUTTON )
	id = NAME_msRightDrag;
      else
	id = NAME_locMove;

      mouse_ev++;
      break;
    }
					/* END MOUSE STUFF */
    case WM_WINENTER:
      id = NAME_areaEnter;
      mouse_ev++;
      break;

    case WM_WINEXIT:
      id = NAME_areaExit;
      mouse_ev++;
      break;
  }

  if ( mouse_ev )
  { int state = 0;
    POINT pt = MAKEPOINT(lParam);

    x = toInt(pt.x);
    y = toInt(pt.y);

    if ( wParam & MK_CONTROL )
      state |= BUTTON_control;
    if ( wParam & MK_SHIFT )
      state |= BUTTON_shift;
    if ( wParam & MK_LBUTTON )
      state |= BUTTON_ms_left;
    if ( wParam & MK_MBUTTON )
      state |= BUTTON_ms_middle;
    if ( wParam & MK_RBUTTON )
      state |= BUTTON_ms_right;

    buttons = toInt(state);
  }

  if ( notNil(id) )
  { setLastEventTime((ulong) GetTickCount());

    return answerObject(ClassEvent,
			id, 
			window,
			x, y,
			buttons,
			0);
  } else
    fail;
}
