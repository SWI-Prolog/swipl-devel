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
#include <time.h>
#include <sys/time.h>
#include <signal.h>
#include <math.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <errno.h>

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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	on_alarm(int sig);

static module_t  MODULE_user;
static functor_t FUNCTOR_alarm1;

#define EV_MAGIC	1920299187	/* Random magic number */

#define EV_DONE		0x0001		/* Handled this one */

typedef struct event
{ record_t	 goal;			/* Thing to call */
  module_t	 module;		/* Module to call in */
  struct timeval at;			/* Time to deliver */
  struct event  *next;			/* linked list for current */
  struct event  *previous;		/* idem */
  unsigned long  flags;			/* misc flags */
  long		 magic;			/* validate magic */
} event, *Event;

typedef void (*handler_t)(int);

static Event first;			/* first of list */
static Event scheduled;			/* The one we scheduled for */

int signal_function_set = FALSE;	/* signal function is set */
static handler_t signal_function;	/* Current signal function */

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

  Sdprintf("allocEvent(%d.%06d)\n", at->tv_sec, at->tv_usec);

  for(e = first; e; e = e->next)
  { struct timeval d;

    Sdprintf("\t%d.%06d\n", e->at.tv_sec, e->at.tv_usec);

    d.tv_sec  = at->tv_sec  - e->at.tv_sec;
    d.tv_usec = at->tv_usec - e->at.tv_usec;
    if ( d.tv_usec < 0 )
    { d.tv_sec--;
      d.tv_usec += 1000000;
    }

    if ( d.tv_sec < 0 )
    { if ( e->next )
      { Sdprintf("next\n");
	continue;
      }

      ev->previous = e;
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

  ev->magic = 0;

  free(ev);
}


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
    left.tv_sec  = first->at.tv_sec - now.tv_sec;
    left.tv_usec = first->at.tv_usec - now.tv_usec;
    if ( left.tv_usec < 0 )
    { left.tv_sec--;
      left.tv_usec += 1000000;
    }

    if ( left.tv_sec < 0 ||
	 (left.tv_sec == 0 && left.tv_usec == 0) )
      continue;				/* Time has passed.  Call? */

    scheduled = ev;			/* This is the scheduled one */

    v.it_value            = left;
    v.it_interval.tv_sec  = 0;
    v.it_interval.tv_usec = 0;

    setitimer(ITIMER_REAL, &v, NULL);	/* Store old? */
    if ( !signal_function_set )
    { signal_function = PL_signal(SIGALRM, on_alarm);
      signal_function_set = TRUE;
    }

    return;
  }

  DEBUG(Sdprintf("Removed timer\n"));
  memset(&v, 0, sizeof(v));
  setitimer(ITIMER_REAL, &v, NULL);	/* restore? */
}


static void
on_alarm(int sig)
{ Event ev;

  if ( (ev=scheduled) )
  { predicate_t pred = PL_predicate("call", 1, "user");
    term_t goal = PL_new_term_ref();
    qid_t qid;
    term_t ex;

    scheduled = NULL;

    ev->flags |= EV_DONE;
    
    PL_recorded(ev->goal, goal);
    qid = PL_open_query(ev->module, PL_Q_CATCH_EXCEPTION, pred, goal);
    if ( !PL_next_solution(qid) && (ex = PL_exception(qid)) )
    { PL_cut_query(qid);
      PL_throw(ex);
    }
    PL_close_query(qid);
  }

  schedule();
}


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


static foreign_t
alarm4(term_t time, term_t callable, term_t id, term_t options)
{ Event ev;
  double t;
  struct timeval tv;
  module_t m = NULL;
    
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

  PL_strip_module(callable, &m, callable);
  ev->module = m;
  ev->goal = PL_record(callable);
  schedule();

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
    schedule();
  }

  freeEvent(ev);

  return TRUE;
}


install_t
install()
{ MODULE_user    = PL_new_module(PL_new_atom("user"));
  FUNCTOR_alarm1 = PL_new_functor(PL_new_atom("$alarm"), 1);

  PL_register_foreign("alarm",        4, alarm4,       PL_FA_TRANSPARENT);
  PL_register_foreign("alarm",        3, alarm3,       PL_FA_TRANSPARENT);
  PL_register_foreign("remove_alarm", 1, remove_alarm, 0);
}
