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

void
resetDispatch()
{
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There is little reason for timeout here. This function returns everytime
the loc_still timer expires (250 milliseconds).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_dispatch(Int FD, Any timeout)
{ MSG msg;

  if ( GetMessage(&msg, NULL, 0, 0) )
  { TranslateMessage(&msg);
    DispatchMessage(&msg);

    succeed;				/* processed an event */
  }

  ExitProcess(0);			/* WM_QUIT received */
  fail;					/* make compiler happy */
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
    Any obj;

    if ( (obj=getObjectFromHWND(win)) )
      return get(obj, NAME_frame, EAV);
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Wait for 250 milliseconds to see whether another key is pressed. This is
used to merge CUA accelerators C-x  and   C-c  with the Emacs ones. Note
that we check for key-down as there will be an up waiting for us.

In practice C-x C-x is only trick handled through this code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ws_wait_for_key(int maxwait)
{ MSG msg;

  msleep(maxwait);

  if ( PeekMessage(&msg, NULL, WM_KEYDOWN, WM_KEYDOWN, PM_NOREMOVE) )
  { succeed;
  } else
  { fail;
  }   
}
