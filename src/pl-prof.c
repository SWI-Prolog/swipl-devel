/*  pl-prof.c,v 1.14 1995/09/08 14:27:28 jan Exp

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: program profiler
*/

#include "pl-incl.h"

#ifdef O_PROFILE

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

forwards void profile(int);

struct itimerval value, ovalue;		/* itmer controlling structures */

#ifdef HAVE_SIGACTION
static struct sigaction osigaction;
#endif

#ifndef SA_NOMASK
#define SA_NOMASK 0
#endif
#ifndef SA_RESTART
#define SA_RESTART 0
#endif

static bool
startProfiler(int how)
{
#ifdef HAVE_SIGACTION
  struct sigaction action;

  memset((char *) &action, 0, sizeof(action));
  action.sa_handler  = profile;
  action.sa_flags    = SA_NOMASK|SA_RESTART;

  sigaction(SIGPROF, &action, &osigaction);
#else
  pl_signal(SIGPROF, profile);
#endif

  value.it_interval.tv_sec  = 0;
  value.it_interval.tv_usec = 1;
  value.it_value.tv_sec  = 0;
  value.it_value.tv_usec = 1;
  
  if (setitimer(ITIMER_PROF, &value, &ovalue) != 0)
    return warning("Failed to start interval timer: %s", OsError());
  statistics.profiling = how;

  succeed;
}

void
stopItimer(void)
{ value.it_interval.tv_sec  = 0;
  value.it_interval.tv_usec = 0;
  value.it_value.tv_sec  = 0;
  value.it_value.tv_usec = 0;
  
  if ( statistics.profiling == NO_PROFILING )
    return;
  if (setitimer(ITIMER_PROF, &value, &ovalue) != 0)
  { warning("Failed to stop interval timer: %s", OsError());
    return;
  }
}

static bool
stopProfiler()
{ if ( statistics.profiling == NO_PROFILING )
    succeed;

  stopItimer();
  statistics.profiling = NO_PROFILING;
#ifdef HAVE_SIGACTION
  sigaction(SIGPROF, &osigaction, NULL);
#else
#ifdef _AIX
  pl_signal(SIGPROF, SIG_IGN);
#else
  pl_signal(SIGPROF, SIG_DFL);
#endif
#endif

  succeed;
}

word
pl_profile(term_t old, term_t new)
{ int prof = statistics.profiling;

  TRY(setInteger(&prof, "profile", old, new));
  if ( prof == statistics.profiling )
    succeed;
  statistics.profiling = prof;
  switch(prof)
  { case NO_PROFILING:
	return stopProfiler();
    case CUMULATIVE_PROFILING:
    case PLAIN_PROFILING:
	if (statistics.profiling != NO_PROFILING)
	{ stopProfiler();
	  pl_reset_profiler();
	}
	return startProfiler(prof);
    default:
	warning("$profile/2: illegal second argument");
	fail;
  }
}
	
word
pl_profile_count(term_t head, term_t calls, term_t prom)
{ Procedure proc;
  Definition def;
  int pm;

  if ( !get_procedure(head, &proc, 0, GP_FIND) )
    return warning("profile_count/3: No such predicate");

  def = proc->definition;
  pm  = (statistics.profile_ticks == 0 ? 0 : (1000 * def->profile_ticks) /
						statistics.profile_ticks);

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
    return warning("profile_box/5: No such predicate");
  def = proc->definition;

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
{ Module module;
  Procedure proc;
  Symbol sm, sp;

  if (statistics.profiling != NO_PROFILING)
    stopProfiler();

  for_table(sm, moduleTable)
  { module = (Module) sm->value;
    for_table(sp, module->procedures)
    { proc = (Procedure) sp->value;

      proc->definition->profile_calls = 0;
      proc->definition->profile_redos = 0;
      proc->definition->profile_fails = 0;
      proc->definition->profile_ticks = 0;
      clear(proc->definition, PROFILE_TICKED);
    }
  }
  statistics.profile_ticks = 0;

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

static void
profile(int sig)
{ register LocalFrame fr = environment_frame;

#if _AIX
  if ( statistics.profiling == NO_PROFILING )
    return;
#endif

#if !defined(BSD_SIGNALS) && !defined(HAVE_SIGACTION)
  signal(SIGPROF, profile);
#endif

  statistics.profile_ticks++;

  if ( gc_status.active )
  { PROCEDURE_garbage_collect0->definition->profile_ticks++;
    return;
  }

  if (fr == (LocalFrame) NULL)
    return;

  if (statistics.profiling == PLAIN_PROFILING)
  { fr->predicate->profile_ticks++;
    return;
  }

  for(; fr; fr = parentFrame(fr) )		/* CUMULATIVE_PROFILING */
  { register Definition def = fr->predicate;
    if ( false(def, PROFILE_TICKED) )
    { set(def, PROFILE_TICKED);
      def->profile_ticks++;
    }
  }
  
  for(fr = environment_frame; fr; fr = parentFrame(fr) )
    clear(fr->predicate, PROFILE_TICKED);
}

#else /* O_PROFILE */

void
stopItimer()
{
}

word
pl_profile(term_t old, term_t new)
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
