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
{ DEBUG(NAME_timer, printf("setIdTimer(%s, %d)\n", pp(tm), id));
  tm->ws_ref = (WsRef) id;
}


static void
timer_proc(int id)
{ DEBUG(NAME_timer, printf("Fireing timer %d\n", id));

  if ( TimerTable )
  { Timer tm;

    if ( (tm = getMemberHashTable(TimerTable, toInt(id))) )
    { executeTimer(tm);
      RedrawDisplayManager(TheDisplayManager());

      if ( tm->status == NAME_once )
      { KillTimer(rlc_hwnd(), id);
	deleteHashTable(TimerTable, toInt(id));
	assign(tm, status, NAME_idle);
      }
    }
  }
}


int
new_timer_id()
{ int id = RLC_APPTIMER_ID;

  while( TimerTable && getMemberHashTable(TimerTable, toInt(id)) )
    id++;

  return id;
}


void
ws_status_timer(Timer tm, Name status)
{ UINT id;

  if ( (id = getIdTimer(tm)) )
  { KillTimer(rlc_hwnd(), id);
    deleteHashTable(TimerTable, toInt(id));
  }

  if ( status != NAME_idle )
  { long msec = (long) (tm->interval->value * 1000.0);
    
    id = new_timer_id();

    if ( (SetTimer(rlc_hwnd(), id, (UINT)msec, NULL)) )
    { if ( !TimerTable )
      { rlc_timer_hook = timer_proc;
	TimerTable = globalObject(CtoName("active_timers"),
				  ClassHashTable, 0);
      }
      appendHashTable(TimerTable, toInt(id), tm);
      setIdTimer(tm, id);
      DEBUG(NAME_timer, printf("Created timer of %d milliseconds (id = %d)\n",
			       msec, id));
    } else
      printf("Failed SetTimer()\n");
  }
}
