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

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"

static XtIntervalId
getIdTimer(Timer tm)
{ return (XtIntervalId) tm->ws_ref;
}


static void
setIdTimer(Timer tm, XtIntervalId id)
{ tm->ws_ref = (WsRef) id;
}


static void
trapTimer(XtPointer xtm, XtIntervalId *id)
{ Timer tm = (Timer) xtm;

  if ( getIdTimer(tm) == *id )
  { setIdTimer(tm, 0);

    executeTimer(tm);

    if ( tm->status == NAME_repeat )
    { long msec = (long) (valReal(tm->interval) * 1000.0);
      XtIntervalId id;

      id = XtAppAddTimeOut(pceXtAppContext(NULL),
			   msec,
			   trapTimer,
			   (XtPointer) tm);
      setIdTimer(tm, id);
    }

    if ( tm->status == NAME_once )
      assign(tm, status, NAME_idle);
  }
}


void
ws_status_timer(Timer tm, Name status)
{ XtIntervalId id;

  if ( (id = getIdTimer(tm)) )
  { setIdTimer(tm, 0);
    XtRemoveTimeOut(id);
  }

  if ( status != NAME_idle )
  { long msec = (long) (valReal(tm->interval) * 1000.0);
    XtIntervalId nid;

    nid = XtAppAddTimeOut(pceXtAppContext(NULL),
			  msec,
			  trapTimer,
			  (XtPointer) tm);
    setIdTimer(tm, nid);
  }
}


#ifdef O_LICENCE
#include "../../../licence/xtimeout.c"
#endif
