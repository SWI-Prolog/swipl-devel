/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include "md.h"
#include <h/interface.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <signal.h>

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#ifdef O_EXTRA_SYSTEM_TYPES
#include O_EXTRA_SYSTEM_TYPES
#endif

typedef void (*VoidFunc)();

#if HOSTACTION
typedef void (*OnExitFunction) P((int, void *));

#if HAVE_atexit && !HAVE_on_exit
#define HAVE_on_exit 1
typedef struct on_exit_cell * OnExitCell;
static OnExitCell OnExitList;

struct on_exit_cell
{ OnExitFunction	function;	/* function to call */
  void*			context;	/* argument to pass */
  OnExitCell		next;		/* next to call */
};

static void
do_exit_list(int code)
{ OnExitCell c = OnExitList;

  for(; c; c = c->next)
    (*c->function)(code, c->context);
}

void
on_exit(OnExitFunction f, char *s)
{ static int initialised = FALSE;
  OnExitCell c = malloc(sizeof(struct on_exit_cell));

  c->function = f;
  c->context = s;
  c->next = OnExitList;
  OnExitList = c;
  
  if ( !initialised )
  { atexit(do_exit_list);
    initialised = TRUE;
  }
}
#endif

int
hostAction(int action, ...)
{ va_list args;
  int rval = PCE_SUCCEED;

  va_start(args, action);
  
  switch(action)
  { case HOST_ONEXIT:
#if HAVE_ON_EXIT
    { OnExitFunction func = va_arg(args, OnExitFunction);
      char *s = va_arg(args, char *);

      on_exit(func, s);
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
      signal(va_arg(args, int), va_arg(args, VoidFunc));
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

  va_end(args);

  return rval;
}

#endif /*HOSTACTION*/

#if HOSTQUERY

int
hostQuery(what, value)
int what;
PceCValue *value;
{ switch(what)
  { case HOST_SYMBOLFILE:
	return PCE_FAIL;
    case HOST_GETC:
	value->character = getchar();
	return PCE_SUCCEED;
    default:
	fprintf(stderr, "Unknown query from PCE: %d", what);
	return PCE_FAIL;
  }
}

#endif /*HOSTQUERY*/

#if HOSTSEND

int
hostSend(PceObject prolog, PceObject sel, int argc, PceObject argv[])
{ fprintf(stderr, "hostSend() not implemented.  See class `c'\n");

  return PCE_FAIL;
}

#endif /*HOSTSEND*/

#if HOSTGET

PceObject
hostGet(PceObject prolog, PceObject sel, int argc, PceObject argv[])
{ fprintf(stderr, "hostGet() not implemented.  See class `c'\n");

  return PCE_FAIL;
}

#endif /*HOSTGET*/

#if MAIN

extern int pceInitApplication(int argc, char **argv);

int
main(int argc, char* argv[])
{ if ( !pceInitialise(0, argc, argv) )
  { fprintf(stderr, "Sorry, failed to initialise XPCE\n");
    exit(1);
  }
  
  if ( !pceInitApplication(argc, argv) )
  { fprintf(stderr, "Failed to run pceInitApplication()\n");
    exit(1);
  }

  for(;;)
    pceDispatch(0, 1000);
}

#endif /*MAIN*/

#ifdef XMALLOC
		/********************************
		*        XMALLOC/XREALLOC	*
		********************************/

void *
xmalloc(size_t nbytes)
{ void *rval = malloc(nbytes);

  if ( !rval )
  { fprintf(stderr, "[PCE: Not enough memory]\n");
    exit(1);
  }

  return rval;
}


void *
xrealloc(void *ptr, size_t nbytes)
{ void *rval = realloc(ptr, nbytes);

  if ( !rval )
  { fprintf(stderr, "[PCE: Not enough memory]\n");
    exit(1);
  }

  return rval;
}

#endif /*XMALLOC*/
