/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
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

int
Stub__HostActionv(int action, va_list args)
{ int rval = PCE_SUCCEED;

  switch(action)
  { case HOST_ATEXIT:
#if HAVE_ATEXIT
    { atexit_hook_t func = va_arg(args, atexit_hook_t);

      atexit(func);
      break;
    }
#endif
    case HOST_TRACE:
    case HOST_BACKTRACE:
    case HOST_BREAK:
    case HOST_ABORT:
    case HOST_RECOVER_FROM_FATAL_ERROR:
      fprintf(stderr,
	      "hostAction(%d) not supported for C++-interface\n", action);
      rval = PCE_FAIL;
      break;
    case HOST_HALT:
      exit(va_arg(args, int));
      break;
    case HOST_SIGNAL:
      signal(va_arg(args, int), va_arg(args, sig_handler_t));
      break;
    case HOST_WRITE:
      printf("%s", va_arg(args, char *));
      break;
    case HOST_FLUSH:
      fflush(stdout);
      break;
    default:
      fprintf(stderr, "Unknown action request from PCE: %d", action);
      rval = PCE_FAIL;
  }

  return rval;
}


int
Stub__HostQuery(what, value)
int what;
PceCValue *value;
{ switch(what)
  { case HOST_SYMBOLFILE:
	return PCE_FAIL;
    default:
	fprintf(stderr, "Unknown query from PCE: %d", what);
	return PCE_FAIL;
  }
}


int
Stub__HostSend(PceObject prolog, PceName sel, int argc, PceObject *argv)
{ fprintf(stderr, "hostSend() not implemented.  See class `c'\n");

  return PCE_FAIL;
}


PceObject
Stub__HostGet(PceObject prolog, PceName sel, int argc, PceObject *argv)
{ fprintf(stderr, "hostGet() not implemented.  See class `c'\n");

  return PCE_FAIL;
}


int
Stub__HostCallProc(PceObject handle,
		   PceObject rec, PceObject sel, int argc, PceObject *argv)
{ Cprintf("hostCallProc() not implemented\n");

  return PCE_FAIL;
}


PceObject
Stub__HostCallFunc(PceObject handle,
		   PceObject rec, PceObject sel, int argc, PceObject *argv)
{ Cprintf("hostCallFunc() not implemented\n");

  return PCE_FAIL;
}

