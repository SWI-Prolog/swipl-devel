/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <unistd.h>

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
{ if ( interval->value == tm->interval->value )
    succeed;

  assign(tm, interval, interval);
  if ( tm->status == NAME_repeat )
    statusTimer(tm, NAME_repeat);

  succeed;
}


status
executeTimer(Timer tm)
{ if ( notNil(tm->message) )
    return forwardReceiverCode(tm->message, tm, 0);

  fail;
}


status
statusTimer(Timer tm, Name stat)
{ ws_status_timer(tm, stat);

  assign(tm, status, stat);
  succeed;
}


status
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


status
makeClassTimer(Class class)
{ sourceClass(class, makeClassTimer, __FILE__, "$Revision$");

  localClass(class, NAME_interval, NAME_time, "real", NAME_get,
	     "Interval between messages in seconds");
  localClass(class, NAME_message, NAME_action, "code*", NAME_both,
	     "Code executed each time");
  localClass(class, NAME_status, NAME_status, "{idle,repeat,once}", NAME_get,
	     "Status of timer");
  localClass(class, NAME_wsRef, NAME_internal, "alien:WsRef", NAME_get,
	     "Window System Reference");

  termClass(class, "timer", 2, NAME_interval, NAME_message);

  storeMethod(class, NAME_interval, intervalTimer);
  storeMethod(class, NAME_status, statusTimer);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "interval=real", "message=[code]*",
	     "Create for interval and message",
	     initialiseTimer);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Destroy X-timer",
	     unlinkTimer);
  sendMethod(class, NAME_execute, NAME_action, 0,
	     "Fire the timer right now",
	     executeTimer);
  sendMethod(class, NAME_start, NAME_status, 1, "how=[{repeat,once}]",
	     "Equivalent to ->status: [repeat]",
	     startTimer);
  sendMethod(class, NAME_stop, NAME_status, 0,
	     "Equivalent to ->status: idle",
	     stopTimer);
  sendMethod(class, NAME_running, NAME_status, 1, "running=bool",
	     "Start/stop the timer in `repeat' mode",
	     runningTimer);
  sendMethod(class, NAME_delay, NAME_status, 0,
	     "Delay for <-interval",
	     delayTimer);

  succeed;
}

