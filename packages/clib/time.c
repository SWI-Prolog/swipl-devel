/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <error.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <math.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <errno.h>

#ifdef WIN32
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The WIN32 port uses the multimedia timers.   This  module must be linked
with winmm.lib
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <windows.h>
#include <sys/timeb.h>

#ifndef SIGALRM
#define SIGALRM 14
#endif

/* Seems to be there
struct timeval
{ long tv_usec;
  long tv_sec;
};
*/

struct timezone
{ int zone;
};

static int
gettimeofday(struct timeval *tv, struct timezone *tz)
{ struct timeb tb;

  ftime(&tb);
  tv->tv_sec  = tb.time;
  tv->tv_usec = tb.millitm * 1000;

  return 0;
}


#else /*WIN32*/
#include <time.h>
#include <sys/time.h>
#endif /*WIN32*/

#ifdef O_DEBUG
#define DEBUG(g) g
#else
#define DEBUG(g) ((void)0)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines support for timing during execution. Most of this is
highly system dependent, and currently running on Unix systems providing
the setitimer() and  friends  functions.   See  time.pl  for  user-level
documentation.

Design
======

This module keeps a double-linked  list   of  `scheduled events' that is
tagged with and annotated using  the   absolute  time  it should happen.
These  times  are  represented  using   the  struct  timeval,  providing
microsecond resolution.

Whenever an event is added  or   deleted,  the  system calls schedule(),
which takes the current time, checks which  event should be the next one
executed and sets a timer to call on_alarm() at that time.

Problems
========

Various locking and asynchronous issues in the  Prolog kernel need to be
checked and validated. Notably throwing an exception might not always be
guarded appropriately. We should distinguish   various types of critical
regions in the code.  To make a start:

	* Places where it is not save to execute Prolog code
		- When GC is running
		- If the clause-index is being rebuild (maybe ok?)

	* Places where it is not save to long_jmp()
		- State is inconsistent
		- Garbage will not be cleaned
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	on_alarm(int sig);

static module_t	   MODULE_user;
static atom_t	   ATOM_true;
static atom_t	   ATOM_false;
static atom_t	   ATOM_remove;
static atom_t	   ATOM_done;
static atom_t	   ATOM_next;
static atom_t	   ATOM_scheduled;
static functor_t   FUNCTOR_module2;
static functor_t   FUNCTOR_alarm1;
static predicate_t PREDICATE_call1;

#define EV_MAGIC	1920299187	/* Random magic number */

#define EV_DONE		0x0001		/* Handled this one */
#define EV_REMOVE	0x0002		/* Automatically remove */
#define EV_FIRED	0x0004		/* Windows: got this one */

typedef struct event
{ record_t	 goal;			/* Thing to call */
  module_t	 module;		/* Module to call in */
  struct event  *next;			/* linked list for current */
  struct event  *previous;		/* idem */
  unsigned long  flags;			/* misc flags */
  long		 magic;			/* validate magic */
  struct timeval at;			/* Time to deliver */
#ifdef WIN32
  UINT		 mmid;			/* MultiMedia timer id */
#endif
} event, *Event;

typedef void (*handler_t)(int);

static Event first;			/* first of list */
static Event scheduled;			/* The one we scheduled for */

int signal_function_set = FALSE;	/* signal function is set */
static handler_t signal_function;	/* Current signal function */

#ifdef WIN32
static void uninstallEvent(Event ev);
#endif


static Event
allocEvent(struct timeval *at)
{ Event ev = malloc(sizeof(*ev));
  Event e;

  if ( !ev )
  { pl_error(NULL, 0, NULL, ERR_ERRNO, errno);
    return NULL;
  }

  memset(ev, 0, sizeof(*ev));
  ev->at = *at;
  ev->magic = EV_MAGIC;

  DEBUG(Sdprintf("allocEvent(%d.%06d)\n", at->tv_sec, at->tv_usec));

  for(e = first; e; e = e->next)
  { struct timeval d;

    d.tv_sec  = at->tv_sec  - e->at.tv_sec;
    d.tv_usec = at->tv_usec - e->at.tv_usec;
    if ( d.tv_usec < 0 )
    { d.tv_sec--;
      d.tv_usec += 1000000;
    }

    if ( d.tv_sec < 0 )			/* new must be before e */
    { ev->next = e;
      ev->previous = e->previous;
      if ( e->previous )
	e->previous->next = ev;
      e->previous = ev;

      if ( first == e )			/* allocated as first */
	first = ev;

      return ev;
    } else
    { if ( e->next )
	continue;

      ev->previous = e;			/* end of the list */
      e->next = ev;

      return ev;
    }
  }

  first = ev;

  return ev;
}


static void
freeEvent(Event ev)
{ if ( ev->previous )
    ev->previous->next = ev->next;
  else
    first = ev->next;

  if ( ev->next )
    ev->next->previous = ev->previous;

  if ( ev->goal )
    PL_erase(ev->goal);

  ev->magic = 0;

  free(ev);
}


static void
callEvent(Event ev)
{ term_t goal = PL_new_term_ref();

  ev->flags |= EV_DONE;
    
  PL_recorded(ev->goal, goal);
  PL_call_predicate(ev->module,
		    PL_Q_PASS_EXCEPTION,
		    PREDICATE_call1,
		    goal);
}



static void
cleanupHandler()
{ 
#ifndef WIN32
  struct itimerval v;

  DEBUG(Sdprintf("Removed timer\n"));
  memset(&v, 0, sizeof(v));
  setitimer(ITIMER_REAL, &v, NULL);	/* restore? */
#endif

  if ( signal_function_set )
  { signal_function_set = FALSE;
    PL_signal(SIGALRM, signal_function);
  }
}


static void
installHandler()
{ if ( !signal_function_set )
  { signal_function = PL_signal(SIGALRM|PL_SIGSYNC, on_alarm);
    signal_function_set = TRUE;
  }
}


static void
cleanup()
{ Event ev, next;

  for(ev=first; ev; ev = next)
  {
#ifdef WIN32
    uninstallEvent(ev);
#endif
    next = ev->next;
    freeEvent(ev);
  }

  cleanupHandler();
}


#ifdef WIN32

static void
on_alarm(int sig)
{ Event ev, next;

  for(ev=first; ev; ev = next)
  { if ( ev->flags & EV_FIRED )
    { ev->flags &= ~EV_FIRED;

      callEvent(ev);
      ev->mmid = 0;			/* TIME_ONESHOT */

      next = ev->next;
      if ( ev->flags & EV_REMOVE )
	freeEvent(ev);
    } else
      next = ev->next;
  }
}


static void CALLBACK
callTimer(UINT id, UINT msg, DWORD dwuser, DWORD dw1, DWORD dw2)
{ Event ev = (Event)dwuser;

  ev->flags |= EV_FIRED;
  PL_raise(SIGALRM);
}


static void
installEvent(Event ev, double t)
{ MMRESULT rval;

  installHandler();

  rval = timeSetEvent((int)(t*1000),
		      50,			/* resolution (milliseconds) */
		      callTimer,
		      (DWORD)ev,
		      TIME_ONESHOT);

  if ( rval )
  { ev->mmid = rval;
  } else
    PL_warning("Failed to install alarm");
}


static void
uninstallEvent(Event ev)
{ if ( ev->mmid )
  { timeKillEvent(ev->mmid);
    ev->mmid = 0;
  }
}


#else /*WIN32*/

static void
schedule()
{ struct itimerval v;
  Event ev;
  
  for(ev=first; ev; ev = ev->next)
  { struct timeval now;
    struct timeval left;

    if ( ev->flags & EV_DONE )
      continue;

    gettimeofday(&now, NULL);
    left.tv_sec  = ev->at.tv_sec  - now.tv_sec;
    left.tv_usec = ev->at.tv_usec - now.tv_usec;
    if ( left.tv_usec < 0 )
    { left.tv_sec--;
      left.tv_usec += 1000000;
    }

    if ( left.tv_sec < 0 ||
	 (left.tv_sec == 0 && left.tv_usec == 0) )
    { DEBUG(Sdprintf("Passed\n"));

      callEvent(ev);		/* Time has passed.  What about exceptions? */

      continue;		
    }

    scheduled = ev;			/* This is the scheduled one */
    DEBUG(Sdprintf("Scheduled for %d.%06d\n", ev->at.tv_sec, ev->at.tv_usec));

    v.it_value            = left;
    v.it_interval.tv_sec  = 0;
    v.it_interval.tv_usec = 0;

    setitimer(ITIMER_REAL, &v, NULL);	/* Store old? */
    installHandler();

    return;
  }

  cleanupHandler();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
glibc defines backtrace() and friends to  print the calling context. For
debugging this is just great,  as   the  problem  generally appear after
generating an exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG
#ifdef HAVE_EXECINFO_H
#define BACKTRACE 1

#if BACKTRACE
#include <execinfo.h>
#include <string.h>

static void
print_trace (void)
{ void *array[100];
  size_t size;
  char **strings;
  size_t i;
     
  size = backtrace(array, sizeof(array)/sizeof(void *));
  strings = backtrace_symbols(array, size);
     
  Sdprintf("on_alarm() Prolog-context:\n");
  PL_action(PL_ACTION_BACKTRACE, 3);

  Sdprintf("on_alarm() C-context:\n");
  
  for(i = 0; i < size; i++)
  { if ( !strstr(strings[i], "checkData") )
      Sdprintf("\t[%d] %s\n", i, strings[i]);
  }
       
  free(strings);
}
#endif /*BACKTRACE*/
#endif /*HAVE_EXECINFO_H*/
#endif /*O_DEBUG*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This one is  asynchronously  called  from   the  hook  registered  using
PL_signal(). Throwing an exception is normally   safe,  but some foreign
code might not leave the  system   unstable  after resulting long_jmp().
This should all nicely be protected,  but   most  likely  this isn't the
case.

The alternative is to delay the signal to a safe place using PL_raise();
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
on_alarm(int sig)
{ Event ev;

#ifdef BACKTRACE
  print_trace();
#endif

  if ( (ev=scheduled) )
  { scheduled = NULL;

    callEvent(ev);

    if ( ev->flags & EV_REMOVE )
      freeEvent(ev);
  }

  schedule();
}

#endif /*WIN32*/

static int
unify_timer(term_t t, Event ev)
{ if ( !PL_is_variable(t) )
    return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 0, t, "unbound");

  return PL_unify_term(t,
		       PL_FUNCTOR, FUNCTOR_alarm1,
		         PL_POINTER, ev);
}


static int
get_timer(term_t t, Event *ev)
{ if ( PL_is_functor(t, FUNCTOR_alarm1) )
  { term_t a = PL_new_term_ref();
    void *p;

    PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &p) )
    { Event e = p;

      if ( e->magic == EV_MAGIC )
      { *ev = e;
        return TRUE;
      } else
      { return pl_error("get_timer", 1, NULL,
			ERR_DOMAIN, t, "alarm");
      }
    }
  }

  return pl_error("get_timer", 1, NULL,
		  ERR_ARGTYPE, 1, t, "alarm");
}


static int
pl_get_bool_ex(term_t arg, int *val)
{ atom_t a;

  if ( PL_get_atom(arg, &a) )
  { if ( a == ATOM_true )
    { *val = TRUE;
      return TRUE;
    }
    if ( a == ATOM_false )
    { *val = FALSE;
      return FALSE;
    }
  }

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 0, arg, "bool");
}


static foreign_t
alarm4(term_t time, term_t callable, term_t id, term_t options)
{ Event ev;
  double t;
  struct timeval tv;
  module_t m = NULL;
  unsigned long flags = 0L;
    
  if ( options )
  { term_t tail = PL_copy_term_ref(options);
    term_t head = PL_new_term_ref();

    while( PL_get_list(tail, head, tail) )
    { atom_t name;
      int arity;

      if ( PL_get_name_arity(head, &name, &arity) )
      { if ( arity == 1 )
	{ term_t arg = PL_new_term_ref();

	  PL_get_arg(1, head, arg);

	  if ( name == ATOM_remove )
	  { int t;

	    if ( !pl_get_bool_ex(arg, &t) )
	      return FALSE;
	    if ( t )
	      flags |= EV_REMOVE;
	  }	    
	}
      }
    }
    if ( !PL_get_nil(tail) )
      return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 4, options, "list");
  }

  if ( !PL_get_float(time, &t) )
    return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1,
		    time, "number");
		    
  gettimeofday(&tv, NULL);
  tv.tv_usec += (long)((t-floor(t))*1000000);
  tv.tv_sec  += (long)t;
  if ( tv.tv_usec > 1000000 )
  { tv.tv_usec -= 1000000;
    tv.tv_sec++;
  }

  if ( !(ev = allocEvent(&tv)) )
    return FALSE;
  if ( !unify_timer(id, ev) )
  { freeEvent(ev);
    return FALSE;
  }

  ev->flags = flags;
  PL_strip_module(callable, &m, callable);
  ev->module = m;
  ev->goal = PL_record(callable);

#ifdef WIN32
  installEvent(ev, t);
#else
  schedule();
#endif

  return TRUE;
}


static foreign_t
alarm3(term_t time, term_t callable, term_t id)
{ return alarm4(time, callable, id, 0);
}


foreign_t
remove_alarm(term_t alarm)
{ Event ev;

  if ( !get_timer(alarm, &ev) )
    return FALSE;

  if ( scheduled == ev )
  { ev->flags |= EV_DONE;
#ifdef WIN32
    uninstallEvent(ev);
#else
    schedule();
#endif
  }

  freeEvent(ev);

  return TRUE;
}


foreign_t
current_alarm(term_t time, term_t goal, term_t id, term_t status, control_t h)
{ Event ev;
  term_t g;
  fid_t fid;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
      ev = first;
      break;
    case PL_REDO:
      ev = PL_foreign_context_address(h);
      break;
    default:
    case PL_CUTTED:
      return TRUE;
  }

  g = PL_new_term_ref();
  fid = PL_open_foreign_frame();

  for(; ev; PL_rewind_foreign_frame(fid), ev = ev->next)
  { atom_t s;

    if ( ev->flags & EV_DONE )
      s = ATOM_done;
    else if ( ev == scheduled )
      s = ATOM_next;
    else
      s = ATOM_scheduled;
    
    if ( !PL_unify_atom(status, s) )
      continue;

    PL_recorded(ev->goal, g);
    if ( !PL_unify_term(goal,
			PL_FUNCTOR, FUNCTOR_module2,
			  PL_ATOM, PL_module_name(ev->module),
			  PL_TERM, g) )
      continue;

    if ( !PL_unify_float(time, (double)ev->at.tv_sec +
			       (double)ev->at.tv_usec / 1000000.0) )
      continue;

    if ( !unify_timer(id, ev) )
      continue;
      
    PL_close_foreign_frame(fid);

    if ( ev->next )
      PL_retry_address(ev->next);

    return TRUE;
  }

  PL_close_foreign_frame(fid);
  return FALSE;
}


install_t
install()
{ MODULE_user	  = PL_new_module(PL_new_atom("user"));

  FUNCTOR_alarm1  = PL_new_functor(PL_new_atom("$alarm"), 1);
  FUNCTOR_module2 = PL_new_functor(PL_new_atom(":"), 2);

  ATOM_true	  = PL_new_atom("true");
  ATOM_false	  = PL_new_atom("false");
  ATOM_remove	  = PL_new_atom("remove");
  ATOM_done	  = PL_new_atom("done");
  ATOM_next	  = PL_new_atom("next");
  ATOM_scheduled  = PL_new_atom("scheduled");

  PREDICATE_call1 = PL_predicate("call", 1, "user");

  PL_register_foreign("alarm",        4, alarm4,       PL_FA_TRANSPARENT);
  PL_register_foreign("alarm",        3, alarm3,       PL_FA_TRANSPARENT);
  PL_register_foreign("remove_alarm", 1, remove_alarm, 0);
  PL_register_foreign("current_alarm",4, current_alarm,PL_FA_NONDETERMINISTIC);
}


install_t
uninstall()
{ cleanup();
}
