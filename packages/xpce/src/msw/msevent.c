/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"

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

#ifdef __WIN32__
    if ( win && (HANDLE)GetWindowLong(win, GWL_HINSTANCE) == PceHInstance )
#else
    if ( win && GetWindowWord(win, GWW_HINSTANCE) == PceHInstance )
#endif
    { Any obj = (Any)GetWindowLong(win, GWL_DATA);

      if ( isProperObject(obj) )	/* may return a subwindow */
	return get(obj, NAME_frame, 0);
    }
  } else if ( instanceOfObject(root, ClassFrame) )
  { PceWindow sw = get_window_holding_point(root, &pt);

    if ( sw && instanceOfObject(sw, ClassWindowDecorator) )
      return ws_event_in_subwindow(ev, sw);

    return sw;
  } else /*if ( instanceOfObject(root, ClassWindow) )*/
  { HWND win;
    PceWindow sw;

    if ( (win = ChildWindowFromPoint(getHwndWindow(root), pt)) &&
	 (sw  = (PceWindow) GetWindowLong(win, GWL_DATA)) &&
	 instanceOfObject(sw, ClassWindow) )
      return sw;
  }

  fail;
}


