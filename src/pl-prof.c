/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

#include "pl-incl.h"

#ifdef O_PROFILE

#ifdef __WIN32__

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MS-Windows version
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void profile(long ticks, PL_local_data_t *ld);

static LARGE_INTEGER last_profile;
static HANDLE	     mythread;
static PL_local_data_t *my_LD;
static UINT	     timer;
static long	     virtual_events;
static long	     events;

static long
prof_new_ticks(HANDLE thread)
{ FILETIME created, exit, kernel, user;
  LARGE_INTEGER u;
  long ticks;

  if ( !GetThreadTimes(thread,
		       &created,
		       &exit,
		       &kernel,
		       &user) )
    return -1;				/* Error condition */

  u.LowPart  = user.dwLowDateTime;
  u.HighPart = user.dwHighDateTime;

  ticks = (long)((u.QuadPart - last_profile.QuadPart)/10240);
  last_profile = u;

  virtual_events += ticks;
  events++;

  return ticks;
}

static void CALLBACK
callTimer(UINT id, UINT msg, DWORD dwuser, DWORD dw1, DWORD dw2)
{ long newticks;

  SuspendThread(mythread);		/* stop thread to avoid trouble */
  if ( (newticks = prof_new_ticks(mythread)) )
  { if ( newticks < 0 )			/* Windows 95/98/... */
      newticks = 1;
    profile(newticks, my_LD);
  }
  ResumeThread(mythread);
}


static bool
startProfiler(int how)
{ MMRESULT rval;

  DuplicateHandle(GetCurrentProcess(),
		  GetCurrentThread(),
		  GetCurrentProcess(),
		  &mythread,
		  0,
		  FALSE,
		  DUPLICATE_SAME_ACCESS);

  my_LD = LD;

  if ( prof_new_ticks(mythread) < 0 )
  { printMessage(ATOM_informational,
		 ATOM_profile_no_cpu_time);
  }
  virtual_events = 0;
  events = 0;

  rval = timeSetEvent(10,
		      5,		/* resolution (milliseconds) */
		      callTimer,
		      (DWORD)0,
		      TIME_PERIODIC);
  if ( rval )
    timer = rval;
  else
    return PL_error(NULL, 0, NULL, ERR_SYSCALL, "timeSetEvent");

  LD->statistics.profiling = how;

  succeed;
}


void
stopItimer(void)
{ if ( timer )
  { DEBUG(1, Sdprintf("%ld events, %ld virtual\n",
		      events, virtual_events));

    timeKillEvent(timer);
    timer = 0;
  }
  if ( mythread )
  { CloseHandle(mythread);
    mythread = 0;
    my_LD = NULL;
  }
}

#else /*__WIN32__*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
POSIX version
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

static void profile(int sig);
static struct itimerval value, ovalue;	/* itimer controlling structures */

static bool
startProfiler(int how)
{ set_sighandler(SIGPROF, profile);

  value.it_interval.tv_sec  = 0;
  value.it_interval.tv_usec = 1;
  value.it_value.tv_sec  = 0;
  value.it_value.tv_usec = 1;
  
  if (setitimer(ITIMER_PROF, &value, &ovalue) != 0)
    return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, setitimer);
  LD->statistics.profiling = how;

  succeed;
}

void
stopItimer(void)
{ value.it_interval.tv_sec  = 0;
  value.it_interval.tv_usec = 0;
  value.it_value.tv_sec  = 0;
  value.it_value.tv_usec = 0;
  
  if ( LD->statistics.profiling == NO_PROFILING )
    return;
  if (setitimer(ITIMER_PROF, &value, &ovalue) != 0)
  { warning("Failed to stop interval timer: %s", OsError());
    return;
  }
}

#endif /*__WIN32__*/

bool
stopProfiler(void)
{ if ( LD->statistics.profiling == NO_PROFILING )
    succeed;

  stopItimer();
  LD->statistics.profiling = NO_PROFILING;
#ifndef __WIN32__
  set_sighandler(SIGPROF, SIG_IGN);
#endif

  succeed;
}

word
pl_profiler(term_t old, term_t new)
{ int prof = LD->statistics.profiling;
  const atom_t prof_names[] = { ATOM_off, ATOM_cumulative, ATOM_plain };
  atom_t val;

  if ( !PL_unify_atom(old, prof_names[prof]) )
    fail;
  if ( !PL_get_atom_ex(new, &val) )
    fail;
  for(prof=0; prof < 3; prof++)
  { if ( prof_names[prof] == val )
      break;
  }
  if ( prof == 3 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_profile_mode, new);

  if ( prof == LD->statistics.profiling )
    succeed;
  switch(prof)
  { case NO_PROFILING:
      return stopProfiler();
    case CUMULATIVE_PROFILING:
    case PLAIN_PROFILING:
      if (LD->statistics.profiling != NO_PROFILING)
      { stopProfiler();
	pl_reset_profiler();
      }
      return startProfiler(prof);
    default:
      assert(0);
      fail;
  }
}
	
word
pl_profile_count(term_t head, term_t calls, term_t prom)
{ Procedure proc;
  Definition def;
  int pm;

  if ( !get_procedure(head, &proc, 0, GP_FIND) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_procedure, head);

  def = getProcDefinition(proc);
  pm  = (LD->statistics.profile_ticks == 0 ? 0 :
					     ((1000 * def->profile_ticks) /
					      LD->statistics.profile_ticks));
  
  if ( PL_unify_integer(calls, def->profile_calls+def->profile_redos) &&
       PL_unify_integer(prom, pm) )
    succeed;

  fail;
}


word
pl_profile_box(term_t head,
	       term_t calls, term_t redos,
	       term_t exits, term_t fails)
{ Procedure proc;
  Definition def;

  if ( !get_procedure(head, &proc, 0, GP_FIND) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_procedure, head);
  def = getProcDefinition(proc);

  if ( PL_unify_integer(calls, def->profile_calls) &&
       PL_unify_integer(redos, def->profile_redos) &&
       PL_unify_integer(exits, def->profile_calls +
			       def->profile_redos -
			       def->profile_fails) &&
       PL_unify_integer(fails, def->profile_fails) )
    succeed;

  fail;
}


word
pl_reset_profiler(void)
{ if (LD->statistics.profiling != NO_PROFILING)
    stopProfiler();

  for_table(GD->tables.modules, sm,
	    { Module module = sm->value;

	      for_unlocked_table(module->procedures, sp,
				 { Procedure proc = sp->value;
				   Definition def = getProcDefinition(proc);

				   def->profile_calls = 0;
				   def->profile_redos = 0;
				   def->profile_fails = 0;
				   def->profile_ticks = 0;
				   clear(def, PROFILE_TICKED);
				 })
	    })
  LD->statistics.profile_ticks = 0;

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is responsible for collection the profiling statistics  at
run time.  It is called by the UNIX interval timer on each clock tick of
the  machine  (every  20  milli seconds).  If profiling is plain we just
increment the profiling tick of the procedure on top of the stack.   For
cumulative  profiling  we  have  to  scan the entire local stack.  As we
don't want to increment each invokation of recursive  functions  on  the
stack  we  maintain a flag on each function.  This flag is set the first
time the function is found on the stack.  If is is found set the profile
counter will not be incremented.  We do a second pass over the frames to
clear the flags again.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef LD
#define LD LOCAL_LD

static void
#ifdef __WIN32__
profile(long ticks, PL_local_data_t *__PL_ld)
{ 
#else /*__WIN32__*/
profile(int sig)
{ GET_LD

#define ticks 1

#if _AIX
  if ( LD->statistics.profiling == NO_PROFILING )
    return;
#endif

#if !defined(BSD_SIGNALS) && !defined(HAVE_SIGACTION)
  signal(SIGPROF, profile);
#endif

#endif /*__WIN32__*/
  { LocalFrame fr = environment_frame;

    LD->statistics.profile_ticks += ticks;
  
    if ( gc_status.active )
    { PROCEDURE_garbage_collect0->definition->profile_ticks++;
      return;
    }
  
    if (fr == (LocalFrame) NULL)
      return;
  
    if (LD->statistics.profiling == PLAIN_PROFILING)
    { fr->predicate->profile_ticks += ticks;
      return;
    }
  
    for(; fr; fr = parentFrame(fr) )		/* CUMULATIVE_PROFILING */
    { register Definition def = fr->predicate;
      if ( false(def, PROFILE_TICKED) )
      { set(def, PROFILE_TICKED);
	def->profile_ticks += ticks;
      }
    }
    
    for(fr = environment_frame; fr; fr = parentFrame(fr) )
      clear(fr->predicate, PROFILE_TICKED);
  }
}

#else /* O_PROFILE */

void
stopItimer()
{
}

word
pl_profiler(term_t old, term_t new)
{ return notImplemented("profile", 2);
}

word
pl_profile_count(term_t head, term_t calls, term_t prom)
{ return notImplemented("profile_count", 3);
}

word
pl_profile_box(term_t head,
	       term_t calls, term_t redos, term_t exits, term_t fails)
{ return notImplemented("profile_box", 3);
}

word
pl_reset_profiler()
{ return notImplemented("reset_profile", 0);
}

#endif /* O_PROFILE */
