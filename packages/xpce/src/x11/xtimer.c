/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
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
