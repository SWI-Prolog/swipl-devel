/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"

void
resetDispatch()
{
}

#ifdef USE_RLC_FUNCTIONS

status
ws_dispatch(Int FD, Int timeout)
{ static RlcQueue discard_queue = NULL;

  if ( !discard_queue )
    discard_queue = rlc_make_queue(100);

  rlc_dispatch(discard_queue);
  if ( !rlc_is_empty_queue(discard_queue) )
  { rlc_empty_queue(discard_queue);
    Cprintf("Confirmer running (dicarding input)\n");
  }

  fail;					/* signal no input */
}


#else /*USE_RLC_FUNCTIONS*/

status
ws_dispatch(Int FD, Int timeout)
{ MSG msg;
  
  if ( GetMessage(&msg, NULL, 0, 0) )
  { TranslateMessage(&msg);
    DispatchMessage(&msg);

    fail;				/* signal no input? */
  }

  ExitProcess(0);			/* WM_QUIT received */
  fail;					/* make compiler happy */
}

#endif /*USE_RLC_FUNCTIONS*/


Any
ws_event_in_subwindow(EventObj ev, Any root)
{ DisplayObj d = getDisplayEvent(ev);
  Int ex, ey;
  POINT pt;
    
  if ( isDefault(root) )
    root = d;

  get_xy_event(ev, root, ON, &ex, &ey);
  pt.x = valInt(ex);
  pt.y = valInt(ey);
  DEBUG(NAME_drag, Cprintf("Point at %d,%d to %s\n", pt.x, pt.y, pp(root)));

  if ( instanceOfObject(root, ClassDisplay) )
  { HWND win = WindowFromPoint(pt);
    Any obj;

    if ( (obj=getObjectFromHWND(win)) )
      return get(obj, NAME_frame, 0);
  } else if ( instanceOfObject(root, ClassFrame) )
  { PceWindow sw = get_window_holding_point(root, &pt);

    if ( sw && instanceOfObject(sw, ClassWindowDecorator) )
      return ws_event_in_subwindow(ev, sw);

    return sw;
  } else /*if ( instanceOfObject(root, ClassWindow) )*/
  { HWND win;
    PceWindow sw;

    if ( (win = ChildWindowFromPoint(getHwndWindow(root), pt)) &&
	 (sw  = getObjectFromHWND(win)) &&
	 instanceOfObject(sw, ClassWindow) )
      return sw;
  }

  fail;
}


		 /*******************************
		 *	       LOC-STILL	*
		 *******************************/

static VOID CALLBACK
locStillTimer(HWND hwnd, UINT msg, UINT id, DWORD now)
{ DEBUG(NAME_locStill, Cprintf("locStillTimer() called\n"));
  considerLocStillEvent();

  /*ws_init_loc_still_timer();*/
}


void
ws_init_loc_still_timer()
{ if ( !SetTimer(NULL, 0, (UINT)(250), (TIMERPROC) locStillTimer) )
    Cprintf("SetTimer() failed\n");
}
