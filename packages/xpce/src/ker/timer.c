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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static status	runningTimer(Timer tm, Bool val);

static status
initialiseTimer(Timer tm, Real interval, Code msg)
{ if ( isDefault(msg) )
    msg = NIL;
  
  assign(tm, interval, CtoReal(0.0));
  assign(tm, message,  msg);
  assign(tm, status,   NAME_idle);

  intervalTimer(tm, interval);

  succeed;
}


static status
unlinkTimer(Timer tm)
{ runningTimer(tm, OFF);

  succeed;
}


status
intervalTimer(Timer tm, Real interval)
{ if ( valReal(interval) == valReal(tm->interval) )
    succeed;

  assign(tm, interval, interval);
  if ( tm->status == NAME_repeat )
    statusTimer(tm, NAME_repeat);

  succeed;
}


status
executeTimer(Timer tm)
{ if ( notNil(tm->message) )
    return forwardReceiverCode(tm->message, tm, EAV);

  fail;
}


status
statusTimer(Timer tm, Name stat)
{ ws_status_timer(tm, stat);

  assign(tm, status, stat);
  succeed;
}


static status
delayTimer(Timer tm)
{ DisplayObj d = CurrentDisplay(NIL);

  statusTimer(tm, NAME_once);
  synchroniseDisplay(d);
  while( tm->status == NAME_once )
  { if ( dispatchDisplay(d) )
      ws_discard_input("Timer running");
  }

  succeed;
}


status
startTimer(Timer tm, Name mode)
{ if ( isDefault(mode) )
    mode = NAME_repeat;

  return statusTimer(tm, mode);
}


status
stopTimer(Timer tm)
{ return statusTimer(tm, NAME_idle);
}


static status
runningTimer(Timer tm, Bool val)
{ return (val == ON ? startTimer(tm, NAME_repeat) : stopTimer(tm));
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "interval=real", "message=[code]*" };

/* Instance Variables */

static vardecl var_timer[] =
{ SV(NAME_interval, "real", IV_GET|IV_STORE, intervalTimer,
     NAME_time, "Interval between messages in seconds"),
  IV(NAME_message, "code*", IV_BOTH,
     NAME_action, "Code executed each time"),
  SV(NAME_status, "{idle,repeat,once}", IV_GET|IV_STORE, statusTimer,
     NAME_status, "Status of timer"),
  IV(NAME_wsRef, "alien:WsRef", IV_GET,
     NAME_internal, "Window System Reference")
};

/* Send Methods */

static senddecl send_timer[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseTimer,
     DEFAULT, "Create for interval and message"),
  SM(NAME_unlink, 0, NULL, unlinkTimer,
     DEFAULT, "Destroy X-timer"),
  SM(NAME_execute, 0, NULL, executeTimer,
     NAME_action, "Fire the timer right now"),
  SM(NAME_delay, 0, NULL, delayTimer,
     NAME_status, "Delay for <-interval"),
  SM(NAME_running, 1, "running=bool", runningTimer,
     NAME_status, "Start/stop the timer in `repeat' mode"),
  SM(NAME_start, 1, "how=[{repeat,once}]", startTimer,
     NAME_status, "Equivalent to ->status: [repeat]"),
  SM(NAME_stop, 0, NULL, stopTimer,
     NAME_status, "Equivalent to ->status: idle")
};

/* Get Methods */

#define get_timer NULL
/*
static getdecl get_timer[] =
{ 
};
*/

/* Resources */

#define rc_timer NULL
/*
static classvardecl rc_timer[] =
{ 
};
*/

/* Class Declaration */

static Name timer_termnames[] = { NAME_interval, NAME_message };

ClassDecl(timer_decls,
          var_timer, send_timer, get_timer, rc_timer,
          2, timer_termnames,
          "$Rev$");

status
makeClassTimer(Class class)
{ return declareClass(class, &timer_decls);
}

