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


static void
do_timer_proc(Timer tm, UINT id)
{ if ( tm->status != NAME_repeat )
  { KillTimer(NULL, id);
    deleteHashTable(TimerTable, toInt(id));
    assign(tm, status, NAME_idle);
  }

  executeTimer(tm);
  
  RedrawDisplayManager(TheDisplayManager());
}


VOID CALLBACK
timer_proc(HWND hwnd, UINT msg, UINT id, DWORD now)
{ DEBUG(NAME_timer, Cprintf("Firing timer %d\n", id));

  if ( TimerTable )
  { Timer tm;

    pceMTLock(LOCK_PCE);
    if ( (tm = getMemberHashTable(TimerTable, toInt(id))) )
    { if ( tm->service == ON )
      { ServiceMode(PCE_EXEC_SERVICE, do_timer_proc(tm, id));
      } else
	do_timer_proc(tm, id);

      pceMTUnlock(LOCK_PCE);

      return;
    }
    pceMTUnlock(LOCK_PCE);
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
