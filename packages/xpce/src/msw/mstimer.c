/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
    { if ( tm->status != NAME_repeat )
      { KillTimer(NULL, id);
	deleteHashTable(TimerTable, toInt(id));
	assign(tm, status, NAME_idle);
      }

      executeTimer(tm);

      RedrawDisplayManager(TheDisplayManager());

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
				  ClassHashTable, EAV);
	assign(TimerTable, refer, NAME_none);
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
