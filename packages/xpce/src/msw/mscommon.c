/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"


		 /*******************************
		 *	     DEBUGGING		*
		 *******************************/

HashTable
MsObjectTable()
{ static HashTable table;

  if ( !table )
    table = globalObject(CtoName("ms_objects"), ClassHashTable, 0);

  return table;
}


		 /*******************************
		 *	INPUT (MODAL LOOP)	*
		 *******************************/

void
ws_discard_input(const char *msg)
{ char buf[1024];

  Cprintf("%s; discarding input ...", msg);
  Cgetline(buf, sizeof(buf));
  Cprintf("ok\n");
}

		 /*******************************
		 *	       FRAME		*
		 *******************************/

HWND
getHwndFrame(FrameObj fr)
{ WsFrame f;

  if ( (f = fr->ws_ref) )
  { DEBUG(NAME_window, Cprintf("HWND of %s --> 0x%04x\n",
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
    f->hcursor	    = NULL;
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
  { /*DEBUG(NAME_window, Cprintf("HWND of %s --> 0x%04x\n",
			      pp(sw), w->hwnd));*/
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

    w->hwnd       = ref;
    w->hcursor    = 0;
    w->capture    = 0;
    w->haspalette = 0;
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

static int	emulate_three_buttons;
static HWND	emu_hwnd;		/* Emulating for this window */

int
ws_emulate_three_buttons(int time)
{ int old = emulate_three_buttons;

  if ( time >= 0 )
    emulate_three_buttons = time;

  return old;
}


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
      if ( emu_hwnd == hwnd )
      { if ( (wParam & MK_RBUTTON) )
	{ id = NAME_msMiddleUp;
	  mouse_ev++;
	} else
	{ emu_hwnd = 0;
	}
      } else
      { id = NAME_msLeftUp;
	mouse_ev++;
	emu_hwnd = 0;
      }

      break;
    case WM_LBUTTONDOWN:
      id = NAME_msLeftDown;

      if ( emulate_three_buttons )
      { MSG msg;

	Sleep(emulate_three_buttons);
        if ( PeekMessage(&msg, hwnd,
			 WM_RBUTTONDOWN, WM_RBUTTONDOWN, PM_REMOVE) )
	{ id = NAME_msMiddleDown;
	  emu_hwnd = hwnd;
	}
      }

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
      if ( emu_hwnd == hwnd )
      { if ( (wParam & MK_LBUTTON) )
	{ id = NAME_msMiddleUp;
	  mouse_ev++;
	} else
	{ emu_hwnd = 0;
	}
      } else
      { id = NAME_msRightUp;
	mouse_ev++;
	emu_hwnd = 0;
      }

      break;
    case WM_RBUTTONDOWN:
      id = NAME_msRightDown;

      if ( emulate_three_buttons )
      { MSG msg;

	Sleep(emulate_three_buttons);
        if ( PeekMessage(&msg, hwnd,
			 WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE) )
	{ id = NAME_msMiddleDown;
	  emu_hwnd = hwnd;
	}
      }

      mouse_ev++;
      break;
    case WM_MOUSEMOVE:
    { if ( emu_hwnd == hwnd )
      { id = NAME_msMiddleDrag;
      } else
      { if ( wParam & MK_LBUTTON )
	  id = NAME_msLeftDrag;
	else if ( wParam & MK_MBUTTON )
	  id = NAME_msMiddleDrag;
	else if ( wParam & MK_RBUTTON )
	  id = NAME_msRightDrag;
	else
	  id = NAME_locMove;
      }

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
    POINTS pt = MAKEPOINTS(lParam);

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
