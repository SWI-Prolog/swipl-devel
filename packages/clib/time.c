/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

#define O_DEBUG 1			/* provides time:time_debug(+Level) */
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
#include <string.h>
#include <errno.h>
#include <assert.h>

#ifdef _REENTRANT
#include <pthread.h>
#endif

#ifdef __WINDOWS__
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The __WINDOWS__ port uses the multimedia timers.   This  module must be linked
with winmm.lib
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <windows.h>
#include <sys/timeb.h>
#if (_MSC_VER < 1400) 
typedef DWORD DWORD_PTR;
#endif

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
  tv->tv_sec  = (long)tb.time;
  tv->tv_usec = tb.millitm * 1000;

  return 0;
}


#else /*__WINDOWS__*/

#ifdef _REENTRANT
#define SHARED_TABLE 1
#endif

#include <time.h>
#include <sys/time.h>

#endif /*__WINDOWS__*/

#ifdef O_DEBUG
static int debuglevel = 0;
#define DEBUG(n, g) if ( debuglevel >= n ) g

static foreign_t
pl_time_debug(term_t n)
{ return PL_get_integer(n, &debuglevel);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
glibc defines backtrace() and friends to  print the calling context. For
debugging this is just great,  as   the  problem  generally appear after
generating an exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
     
#ifdef _REENTRANT
  Sdprintf("on_alarm() Prolog-context [thread %d]:\n", PL_thread_self());
#else
  Sdprintf("on_alarm() Prolog-context:\n");
#endif
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
#else /*O_DEBUG*/
#define DEBUG(n, g) ((void)0)
#endif /*O_DEBUG*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines support for timing during execution. Most of this is
highly system dependent.

Design
======

The module contains three implementations:

	* Windows
	The Windows versions is based on multimedia timer objects.
	
	* Unix setitimer (single threaded)
	This implementation uses setitimer() and SIGALRM.  Whenever
	something is changed, re_schedule() is called to set the
	alarm clock for the next wakeup.

	* Unix scheduler thread
	This implementation uses a table shared between all threads and
	a thread that waits for the next signal to be send using
	pthread_cond_timedwait().  The signal SIGALRM is then delivered
	using pthread_kill() to the scheduled thread.

This module keeps a double-linked  list   of  `scheduled events' that is
tagged with and annotated using  the   absolute  time  it should happen.
These  times  are  represented  using   the  struct  timeval,  providing
microsecond   resolution.   If   the     SHARED_TABLE    version   (Unix
multithreaded), there is one table for all  events. In the other designs
each thread has its own table.

The  signal  handler  uses  the  PL_SIGSYNC    option  to  be  scheduled
synchronous with the Prolog activity  and   eb  able to throw exceptions
(the most common alarm activity).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	on_alarm(int sig);

static module_t	   MODULE_user;
static atom_t	   ATOM_remove;
static atom_t	   ATOM_done;
static atom_t	   ATOM_next;
static atom_t	   ATOM_scheduled;
static functor_t   FUNCTOR_module2;
static functor_t   FUNCTOR_alarm1;
static functor_t   FUNCTOR_alarm4;
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
#ifdef SHARED_TABLE
  pthread_t	 thread_id;		/* Thread to call in */
#ifdef O_DEBUG
  int		 pl_thread_id;		/* Prolog thread ID */
#endif
#endif
#ifdef __WINDOWS__
  UINT		 mmid;			/* MultiMedia timer id */
  DWORD		 tid;			/* thread-id of Prolog thread */
#endif
} event, *Event;

typedef void (*handler_t)(int);

typedef struct
{ Event first;				/* first in list */
  Event scheduled;			/* The one we scheduled for */
} schedule;

#ifdef SHARED_TABLE
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cond   = PTHREAD_COND_INITIALIZER;
static int scheduler_running = FALSE;	/* is scheduler running? */
static pthread_t scheduler;		/* thread id of scheduler */

#define LOCK()   pthread_mutex_lock(&mutex);
#define UNLOCK() pthread_mutex_unlock(&mutex);
#else
#define LOCK()   (void)0
#define UNLOCK() (void)0
#endif /*SHARED_TABLE*/

#if defined(_REENTRANT) && !defined(SHARED_TABLE)

static pthread_key_t key;

static schedule *
TheSchedule()
{ schedule *s;

  if ( !(s=pthread_getspecific(key)) )
  { s = PL_malloc(sizeof(schedule));
    memset(s, 0, sizeof(*s));
    pthread_setspecific(key, s);
  }

  return s;
}

static void
free_schedule(void *closure)
{ schedule *s = closure;

  PL_free(s);
}

#else /*defined(_REENTRANT) && !defined(SHARED_TABLE)*/

static schedule the_schedule;		/* the schedule */
#define TheSchedule() (&the_schedule)	/* current schedule */

#endif  /*defined(_REENTRANT) && !defined(SHARED_TABLE)*/

int signal_function_set = FALSE;	/* signal function is set */
static handler_t signal_function;	/* Current signal function */

static void uninstallEvent(Event ev);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate the event, maintaining a time-sorted list of scheduled events.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Event
allocEvent(struct timeval *at)
{ Event ev = malloc(sizeof(*ev));

  if ( !ev )
  { pl_error(NULL, 0, NULL, ERR_ERRNO, errno);
    return NULL;
  }

  memset(ev, 0, sizeof(*ev));
  ev->at = *at;
  ev->magic = EV_MAGIC;

  return ev;
}


static void
insertEvent(Event ev)
{ schedule *sched = TheSchedule();
  Event e;

  DEBUG(1, Sdprintf("insertEvent(%d.%06d)\n", ev->at.tv_sec, ev->at.tv_usec));

  for(e = sched->first; e; e = e->next)
  { struct timeval d;

    d.tv_sec  = ev->at.tv_sec  - e->at.tv_sec;
    d.tv_usec = ev->at.tv_usec - e->at.tv_usec;
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

      if ( sched->first == e )			/* allocated as first */
	sched->first = ev;

      return;
    } else
    { if ( e->next )
	continue;

      ev->previous = e;			/* end of the list */
      e->next = ev;

      return;
    }
  }

  sched->first = ev;
}


static void
freeEvent(Event ev)
{ schedule *sched = TheSchedule();

  if ( sched->scheduled == ev )
    sched->scheduled = NULL;

  if ( ev->previous )
    ev->previous->next = ev->next;
  else
    sched->first = ev->next;

  if ( ev->next )
    ev->next->previous = ev->previous;

  if ( ev->goal )
    PL_erase(ev->goal);

  ev->magic = 0;

  free(ev);
}


#ifndef SHARED_TABLE
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
#endif


static void
cleanupHandler()
{ 
#ifndef __WINDOWS__
  struct itimerval v;

  DEBUG(1, Sdprintf("Removed timer\n"));
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
  schedule *sched = TheSchedule();

  for(ev=sched->first; ev; ev = next)
  { next = ev->next;
    uninstallEvent(ev);
  }

  cleanupHandler();
}


#ifdef __WINDOWS__

static void
on_alarm(int sig)
{ Event ev, next;

  for(ev=TheSchedule()->first; ev; ev = next)
  { assert(ev->magic == EV_MAGIC);

    if ( ev->flags & EV_FIRED )
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
callTimer(UINT id, UINT msg, DWORD_PTR dwuser, DWORD_PTR dw1, DWORD_PTR dw2)
{ Event ev = (Event)dwuser;

  ev->flags |= EV_FIRED;
  PL_w32thread_raise(ev->tid, SIGALRM);
}


static int
installEvent(Event ev, double t)
{ MMRESULT rval;

  insertEvent(ev);

  rval = timeSetEvent((int)(t*1000),
		      50,			/* resolution (milliseconds) */
		      callTimer,
		      (DWORD)ev,
		      TIME_ONESHOT);

  if ( rval )
  { ev->tid = GetCurrentThreadId();
    ev->mmid = rval;

    return TRUE;
  }
    
  return pl_error(NULL, 0, NULL, ERR_RESOURCE, "no_timers");
}


static void
uninstallEvent(Event ev)
{ if ( TheSchedule()->scheduled == ev )
    ev->flags |= EV_DONE;

  if ( ev->mmid )
  { timeKillEvent(ev->mmid);
    ev->mmid = 0;
  }

  freeEvent(ev);
}


#else /*__WINDOWS__*/

#ifdef SHARED_TABLE

static Event
nextEvent(schedule *sched)
{ Event ev;

  for(ev=sched->first; ev; ev = ev->next)
  { if ( ev->flags & (EV_DONE|EV_FIRED) )
      continue;

    return ev;
  }

  return NULL;
}


static void *
alarm_loop(void * closure)
{ schedule *sched = TheSchedule();

  pthread_mutex_lock(&mutex);		/* for condition variable */

  for(;;)
  { Event ev = nextEvent(sched);

    if ( ev )
    { struct timespec timeout;
      int rc;

      timeout.tv_sec  = ev->at.tv_sec;
      timeout.tv_nsec = ev->at.tv_usec*1000;

      DEBUG(1, Sdprintf("Waiting ...\n"));
      rc = pthread_cond_timedwait(&cond, &mutex, &timeout);

      switch( rc )
      { case ETIMEDOUT:
	  DEBUG(1, Sdprintf("Signalling %d (= %d) ...\n",
			    ev->pl_thread_id, ev->thread_id));
	  sched->scheduled = ev;
	  ev->flags |= EV_FIRED;
	  pthread_kill(ev->thread_id, SIGALRM);
	  break;
	case EINTR:
	  continue;
      }
    } else
    { int rc = pthread_cond_wait(&cond, &mutex);

      if ( rc == EINTR )
	continue;
    }
  }

  return NULL;
}


static void
on_alarm(int sig)
{ Event ev;
  schedule *sched = TheSchedule();
  pthread_t self = pthread_self();
  term_t goal = 0;
  module_t module = NULL;

  DEBUG(1, Sdprintf("Signal received in %d (= %d)\n",
		    PL_thread_self(), self));
#ifdef BACKTRACE
  DEBUG(10, print_trace());
#endif

  LOCK();
  for(ev = sched->first; ev; ev=ev->next)
  { assert(ev->magic == EV_MAGIC);

    if ( (ev->flags & EV_FIRED) &&
	 pthread_equal(self, ev->thread_id) )
    { ev->flags &= ~EV_FIRED;

      DEBUG(1, Sdprintf("Calling event\n"));
      ev->flags |= EV_DONE;
      module = ev->module;
      goal = PL_new_term_ref();
      PL_recorded(ev->goal, goal);

      if ( ev->flags & EV_REMOVE )
	freeEvent(ev);
      break;
    }
  }
  UNLOCK();

  if ( goal )
  { PL_call_predicate(module,
		      PL_Q_PASS_EXCEPTION,
		      PREDICATE_call1,
		      goal);
  }
}


static int
installEvent(Event ev, double t)
{ LOCK();

  ev->thread_id = pthread_self();
#ifdef O_DEBUG
  ev->pl_thread_id = PL_thread_self();
#endif

  if ( !scheduler_running )
  { pthread_attr_t attr;
    int rc;

    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_attr_setstacksize(&attr, 1024);

    if ( (rc=pthread_create(&scheduler, &attr, alarm_loop, NULL)) )
      return pl_error("alarm", 4, "Failed to start schedule thread",
		      ERR_ERRNO, rc);
    pthread_attr_destroy(&attr);

    DEBUG(1, Sdprintf("Started scheduler thread\n"));
    scheduler_running = TRUE;
  }

  insertEvent(ev);
  pthread_cond_signal(&cond);
  UNLOCK();

  return TRUE;
}


static void
uninstallEvent(Event ev)
{ LOCK();
  if ( TheSchedule()->scheduled == ev )
    ev->flags |= EV_DONE;
  freeEvent(ev);
  pthread_cond_signal(&cond);
  UNLOCK();
}


#else /*SHARED_TABLE*/

static void
re_schedule()
{ struct itimerval v;
  Event ev;
  schedule *sched = TheSchedule();
  
  for(ev=sched->first; ev; ev = ev->next)
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
    { DEBUG(1, Sdprintf("Passed\n"));

      callEvent(ev);		/* Time has passed.  What about exceptions? */

      continue;		
    }

    sched->scheduled = ev;	/* This is the scheduled one */
    DEBUG(1, Sdprintf("Scheduled for %d.%06d\n", ev->at.tv_sec, ev->at.tv_usec));

    v.it_value            = left;
    v.it_interval.tv_sec  = 0;
    v.it_interval.tv_usec = 0;

    setitimer(ITIMER_REAL, &v, NULL);	/* Store old? */

    return;
  }
}

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
  schedule *sched = TheSchedule();

#ifdef BACKTRACE
  DEBUG(10, print_trace());
#endif

  if ( (ev=sched->scheduled) )
  { assert(ev->magic == EV_MAGIC);
    sched->scheduled = NULL;

    callEvent(ev);

    if ( ev->flags & EV_REMOVE )
      freeEvent(ev);
  }

  re_schedule();
}


static int
installEvent(Event ev, double t)
{ insertEvent(ev);
  re_schedule();

  return TRUE;
}


static void
uninstallEvent(Event ev)
{ if ( TheSchedule()->scheduled == ev )
  { ev->flags |= EV_DONE;
    re_schedule();
  }

  freeEvent(ev);
}

#endif /*SHARED_TABLE*/
#endif /*__WINDOWS__*/


		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

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
{ if ( PL_get_bool(arg, val) )
    return TRUE;

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
	  { int t = FALSE;

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

  if ( !installEvent(ev, t) )
  { freeEvent(ev);
    return FALSE;
  }

  return TRUE;
}


static foreign_t
alarm3(term_t time, term_t callable, term_t id)
{ return alarm4(time, callable, id, 0);
}


foreign_t
remove_alarm(term_t alarm)
{ Event ev = NULL;

  if ( !get_timer(alarm, &ev) )
    return FALSE;

  uninstallEvent(ev);

  return TRUE;
}


foreign_t
current_alarms(term_t time, term_t goal, term_t id, term_t status,
	       term_t matching)
{ Event ev;
  term_t next = PL_new_term_ref();
  term_t g    = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(matching);
  term_t head = PL_new_term_ref();
  term_t av   = PL_new_term_refs(4);
#ifdef SHARED_TABLE
  pthread_t self = pthread_self();
#endif

  LOCK();
  ev = TheSchedule()->first;

  for(; ev; ev = ev->next)
  { atom_t s;
    double at;
    fid_t fid;

#ifdef SHARED_TABLE
    if ( !pthread_equal(self, ev->thread_id) )
      continue;
#endif

    fid = PL_open_foreign_frame();

    if ( ev->flags & EV_DONE )
      s = ATOM_done;
    else if ( ev == TheSchedule()->scheduled )
      s = ATOM_next;
    else
      s = ATOM_scheduled;
    
    if ( !PL_unify_atom(status, s) )
      goto nomatch;

    PL_recorded(ev->goal, g);
    if ( !PL_unify_term(goal,
			PL_FUNCTOR, FUNCTOR_module2,
			  PL_ATOM, PL_module_name(ev->module),
			  PL_TERM, g) )
      goto nomatch;

    at = (double)ev->at.tv_sec + (double)ev->at.tv_usec / 1000000.0;
    if ( !PL_unify_float(time, at) )
      goto nomatch;

    if ( !unify_timer(id, ev) )
      goto nomatch;
      
    PL_discard_foreign_frame(fid);

    PL_put_float(av+0, at);		/* time */
    PL_recorded(ev->goal, av+1);	/* goal */
    PL_put_variable(av+2);		/* id */
    unify_timer(av+2, ev);
    PL_put_atom(av+3, s);		/* status */
    PL_cons_functor_v(next, FUNCTOR_alarm4, av);

    if ( PL_unify_list(tail, head, tail) &&
	 PL_unify(head, next) )
    { continue;
    } else
    { PL_close_foreign_frame(fid);
      UNLOCK();

      return FALSE;
    }

  nomatch:
    PL_discard_foreign_frame(fid);
  }
  UNLOCK();

  return PL_unify_nil(tail);
}


install_t
install()
{ MODULE_user	  = PL_new_module(PL_new_atom("user"));

  FUNCTOR_alarm1  = PL_new_functor(PL_new_atom("$alarm"), 1);
  FUNCTOR_alarm4  = PL_new_functor(PL_new_atom("alarm"), 4);
  FUNCTOR_module2 = PL_new_functor(PL_new_atom(":"), 2);

  ATOM_remove	  = PL_new_atom("remove");
  ATOM_done	  = PL_new_atom("done");
  ATOM_next	  = PL_new_atom("next");
  ATOM_scheduled  = PL_new_atom("scheduled");

  PREDICATE_call1 = PL_predicate("call", 1, "user");

  PL_register_foreign("alarm",          4, alarm4,         PL_FA_TRANSPARENT);
  PL_register_foreign("alarm",          3, alarm3,         PL_FA_TRANSPARENT);
  PL_register_foreign("remove_alarm",   1, remove_alarm,   0);
  PL_register_foreign("remove_alarm_notrace",1, remove_alarm,   PL_FA_NOTRACE);
  PL_register_foreign("current_alarms", 5, current_alarms, 0);
#ifdef O_DEBUG
  PL_register_foreign("time_debug",	1, pl_time_debug,  0);
#endif

#if defined(_REENTRANT) && !defined(SHARED_TABLE)
  pthread_key_create(&key, free_schedule);
#endif
  installHandler();
}


install_t
uninstall()
{ cleanup();

#if defined(_REENTRANT) && !defined(SHARED_TABLE)
  pthread_key_delete(key);
#endif
}
