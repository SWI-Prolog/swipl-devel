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

#include "md.h"
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <signal.h>
#include <h/interface.h>
#include "stub.h"

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#ifdef SYSLIB_H
#include SYSLIB_H
#endif

typedef void (*VoidFunc)();
typedef void (*sig_handler_t)(int);
typedef void (*atexit_hook_t)(void);
typedef void (*onexit_hook_t)(int, void *);

static char * host_action_names[] =
{ "HOST_QUERY",
  "HOST_TRACE",
  "HOST_BACKTRACE",
  "HOST_HALT",
  "HOST_BREAK",
  "<unused (5)>",
  "HOST_ABORT",
  "HOST_SIGNAL",
  "HOST_RECOVER_FROM_FATAL_ERROR",
  "HOST_ATEXIT",
  "HOST_CONSOLE",
  "HOST_CHECK_INTERRUPT"
};

#define HIGHEST_HOST_ACTION_NAME (sizeof(host_action_names) / sizeof(char *))

int
Stub__HostActionv(int action, va_list args)
{ int rval = PCE_SUCCEED;

  switch(action)
  { case HOST_ATEXIT:
#if HAVE_ON_EXIT
    { onexit_hook_t func = va_arg(args, onexit_hook_t);

      on_exit(func, NULL);
      break;
    }
#else
#if HAVE_ATEXIT
    { atexit_hook_t func = va_arg(args, atexit_hook_t);

      atexit(func);
      break;
    }
#endif
#endif
    case HOST_TRACE:
    case HOST_BACKTRACE:
    case HOST_BREAK:
    case HOST_ABORT:
    case HOST_RECOVER_FROM_FATAL_ERROR:
      Cprintf("hostAction(%d (=%s)) not supported for C++-interface\n",
	      action, host_action_names[action]);
      rval = PCE_FAIL;
      break;
    case HOST_HALT:
      exit(va_arg(args, int));
      break;
    case HOST_SIGNAL:
      signal(va_arg(args, int), va_arg(args, sig_handler_t));
      break;
    case HOST_CHECK_INTERRUPT:
      return PCE_FAIL;
    default:
      Cprintf("Unknown action request from PCE: %d\n", action);
      rval = PCE_FAIL;
  }

  return rval;
}


int
Stub__HostQuery(int what, PceCValue *value)
{ switch(what)
  { case HOST_CONSOLE:
      return PCE_FAIL;
    default:
      Cprintf("Unknown query from PCE: %d\n", what);
      return PCE_FAIL;
  }
}


int
Stub__HostSend(PceObject prolog, PceName sel, int argc, PceObject *argv)
{ Cprintf("hostSend() not implemented.  See class `c'\n");

  return PCE_FAIL;
}


PceObject
Stub__HostGet(PceObject prolog, PceName sel, int argc, PceObject *argv)
{ Cprintf("hostGet() not implemented.  See class `c'\n");

  return PCE_FAIL;
}


int
Stub__HostCall(PceGoal goal)
{ Cprintf("hostCall() not implemented\n");

  return PCE_FAIL;
}

