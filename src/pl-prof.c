/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: program profiler
*/

#include "pl-incl.h"

#if O_PROFILE

#include <sys/time.h>

#if sun
extern int setitimer(int, struct itimerval *,struct itimerval *);
#endif

forwards void profile(int);

struct itimerval value, ovalue;		/* itmer controlling structures */

static bool
startProfiler(int how)
{ pl_signal(SIGPROF, profile);

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
#if _AIX
  pl_signal(SIGPROF, SIG_IGN);
#else
  pl_signal(SIGPROF, SIG_DFL);
#endif

  succeed;
}

word
pl_profile(Word old, Word new)
{ int prof;

  TRY(unifyAtomic(old, consNum(statistics.profiling)) );
  if (!isInteger(*new))
    fail;
  if ((prof = valNum(*new)) == statistics.profiling)
    succeed;
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
pl_profile_count(Word head, Word calls, Word prom)
{ Procedure proc;
  Definition def;

  if ((proc = findProcedure(head)) == (Procedure) NULL)
    return warning("profile_count/3: No such predicate");
  def = proc->definition;

  TRY(unifyAtomic(calls, consNum(def->profile_calls+def->profile_redos)) );

  return unifyAtomic(prom, consNum((1000 * def->profile_ticks) /
				   statistics.profile_ticks) );
}


word
pl_profile_box(Word head, Word calls, Word redos, Word exits, Word fails)
{ Procedure proc;
  Definition def;

  if ((proc = findProcedure(head)) == (Procedure) NULL)
    return warning("profile_box/5: No such predicate");
  def = proc->definition;

  TRY(unifyAtomic(calls,    consNum(def->profile_calls)));
  TRY(unifyAtomic(redos,    consNum(def->profile_redos)));
  TRY(unifyAtomic(exits,    consNum(def->profile_calls +
				    def->profile_redos -
				    def->profile_fails)));
  return unifyAtomic(fails, consNum(def->profile_fails));
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

#if O_SIG_AUTO_RESET
  signal(SIGPROF, profile);
#endif

  if ( gc_status.active )
  { PROCEDURE_garbage_collect0->definition->profile_ticks++;
    return;
  }

  if (fr == (LocalFrame) NULL)
    return;

  statistics.profile_ticks++;
  if (statistics.profiling == PLAIN_PROFILING)
  { fr->procedure->definition->profile_ticks++;
    return;
  }

  for(; fr; fr = parentFrame(fr) )		/* CUMULATIVE_PROFILING */
  { register Procedure proc = fr->procedure;
    if ( false(proc->definition, PROFILE_TICKED) )
    { set(proc->definition, PROFILE_TICKED);
      proc->definition->profile_ticks++;
    }
  }
  
  for(fr = environment_frame; fr; fr = parentFrame(fr) )
    clear(fr->procedure->definition, PROFILE_TICKED);
}

#else /* O_PROFILE */

void
stopItimer()
{
}

word
pl_profile(old, new)
Word old, new;
{ return notImplemented("profile", 2);
}

word
pl_profile_count(head, calls, prom)
Word head, calls, prom;
{ return notImplemented("profile_count", 3);
}

word
pl_profile_box(head, calls, redos, exits, fails)
Word head, calls, redos, exits, fails;
{ return notImplemented("profile_box", 3);
}

word
pl_reset_profiler()
{ return notImplemented("reset_profile", 0);
}

#endif /* O_PROFILE */
