/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"

static HashTable TimerTable;

static UINT
getIdTimer(Timer tm)
{ return (UINT) tm->ws_ref;
}


static void
setIdTimer(Timer tm, UINT id)
{ DEBUG(NAME_timer, Cprintf("setIdTimer(%s, %d)\n", pp(tm), id));
  tm->ws_ref = (WsRef) id;
}


VOID CALLBACK
timer_proc(HWND hwnd, UINT msg, UINT id, DWORD now)
{ DEBUG(NAME_timer, Cprintf("Fireing timer %d\n", id));

  if ( TimerTable )
  { Timer tm;

    if ( (tm = getMemberHashTable(TimerTable, toInt(id))) )
    { executeTimer(tm);
      RedrawDisplayManager(TheDisplayManager());

      if ( tm->status != NAME_repeat )
      { KillTimer(NULL, id);
	deleteHashTable(TimerTable, toInt(id));
	assign(tm, status, NAME_idle);
      }

      return;
    }
  }

  KillTimer(NULL, id);			/* Unexpected timer.  Get rid of it */
}


void
ws_status_timer(Timer tm, Name status)
{ UINT id;

  if ( (id = getIdTimer(tm)) )
  { KillTimer(NULL, id);
    deleteHashTable(TimerTable, toInt(id));
    setIdTimer(tm, 0);
  }

  if ( status != NAME_idle )
  { long msec = (long) (valReal(tm->interval) * 1000.0);
    
    if ( (id = SetTimer(NULL, 0, (UINT)msec, (TIMERPROC) timer_proc)) )
    { if ( !TimerTable )
      { TimerTable = globalObject(CtoName("active_timers"),
				  ClassHashTable, 0);
	assign(TimerTable, refer, OFF);
      }
      appendHashTable(TimerTable, toInt(id), tm);
      setIdTimer(tm, id);
      DEBUG(NAME_timer, Cprintf("Created timer of %d milliseconds (id = %d)\n",
				msec, id));
    } else
      Cprintf("Failed SetTimer()\n");
  }
}


#ifdef O_LICENCE
#include "../../../licence/mstimeout.c"
#endif
