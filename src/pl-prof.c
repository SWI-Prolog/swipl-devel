/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2026, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-comp.h"
#include "pl-prof.h"
#include "pl-wam.h"
#include "pl-setup.h"
#include "pl-fli.h"
#include "pl-proc.h"
#include "pl-gc.h"
#include "pl-util.h"
#include "pl-pro.h"
#include <math.h>
#include "pl-modul.h"

#undef LD
#define LD LOCAL_LD

#ifdef O_PROFILE

		 /*******************************
		 *	   EXACT WALL CLOCK	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The PROF_WALL_EXACT mode reads a clock each time the current node of the
call tree changes.  prof_clock() is the cheapest monotonic counter we can
find: the invariant TSC on x86, the virtual counter on ARM64 or the OS
monotonic clock.  Its unit is unknown.  We calibrate against the OS
monotonic clock (prof_mono_ns()) over the profiled interval and convert
the counts to nanoseconds when the profiler is stopped.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__)
#ifdef _MSC_VER
#include <intrin.h>
#else
#include <x86intrin.h>
#include <cpuid.h>
#endif
#define PROF_CLOCK_TSC 1
#elif defined(__aarch64__) && !defined(_MSC_VER)
#define PROF_CLOCK_CNTVCT 1
#elif defined(_M_ARM64)
#include <intrin.h>
#define PROF_CLOCK_CNTVCT 1
#endif

static int64_t
prof_mono_ns(void)
{
#ifdef __WINDOWS__
  static LARGE_INTEGER freq;
  LARGE_INTEGER now;

  if ( !freq.QuadPart )
    QueryPerformanceFrequency(&freq);
  QueryPerformanceCounter(&now);

  return (int64_t)((double)now.QuadPart*(1e9/(double)freq.QuadPart));
#elif defined(HAVE_CLOCK_GETTIME)
  struct timespec ts;

  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (int64_t)ts.tv_sec*1000000000 + ts.tv_nsec;
#else
  return llround(WallTime()*1e9);
#endif
}

#ifdef PROF_CLOCK_TSC
static int tsc_ok = -1;			/* -1: unknown */

static bool
invariant_tsc(void)
{ if ( tsc_ok == -1 )
  {
#ifdef _MSC_VER
    int r[4];

    __cpuid(r, 0x80000000);
    if ( (unsigned)r[0] >= 0x80000007 )
    { __cpuid(r, 0x80000007);
      tsc_ok = !!(r[3] & (1<<8));
    } else
      tsc_ok = 0;
#else
    unsigned int a, b, c, d;

    if ( __get_cpuid(0x80000007, &a, &b, &c, &d) )
      tsc_ok = !!(d & (1<<8));
    else
      tsc_ok = 0;
#endif
  }

  return tsc_ok;
}
#endif

static inline uint64_t
prof_clock(void)
{
#ifdef PROF_CLOCK_TSC
  if ( likely(tsc_ok == 1) )
    return __rdtsc();
#elif defined(PROF_CLOCK_CNTVCT)
#ifdef _MSC_VER
  return _ReadStatusReg(ARM64_CNTVCT);
#else
  uint64_t v;
  __asm__ __volatile__ ("mrs %0, cntvct_el0" : "=r" (v));
  return v;
#endif
#endif
  return (uint64_t)prof_mono_ns();
}


#define PROFTYPE_MAGIC 0x639a2fb1

#if USE_LD_MACROS
#define	profile(count)		LDFUNC(profile, count)
#define	thread_prof_ticks(_)	LDFUNC(thread_prof_ticks, _)
#endif

#define LDFUNC_DECLARATIONS

static bool identify_def(term_t t, void *handle);
static bool get_def(term_t t, void **handle);
static void prof_release_def(void *handle);
static void profile(int64_t count);
static int64_t thread_prof_ticks(void);

#undef LDFUNC_DECLARATIONS

static PL_prof_type_t prof_default_type =
{ identify_def,					/* unify a Definition */
  get_def,					/* Find a definition */
  NULL,						/* Acquire */
  prof_release_def,				/* Release a definition */
  .magic = PROFTYPE_MAGIC
};

#define MAX_PROF_TYPES 10
static PL_prof_type_t *types[MAX_PROF_TYPES] = { &prof_default_type };

#define PROFNODE_MAGIC 0x7ae38f24

typedef struct call_node
{ int		    magic;		/* PROFNODE_MAGIC */
  struct call_node *parent;
  void *            handle;		/* handle to procedure-id */
  PL_prof_type_t   *type;
  uint64_t	    calls;		/* Calls from the parent */
  uint64_t	    fails;		/* fails back to choicepoint */
  uint64_t	    exits;		/* exits to the parent */
  uint64_t	    recur;		/* recursive calls */
  uint64_t	    ticks;		/* time-statistics (nsec) */
  uint64_t	    sibling_ticks;	/* ticks in a siblings */
  uint64_t	    raw;		/* PROF_*_EXACT: prof_clock() units */
  struct call_node *win_next;		/* PROF_CPU_EXACT: window chain */
  bool		    in_window;		/* in LD->profile.clock.win_nodes */
  struct call_node *next;		/* next in siblings chain */
  struct call_node *siblings;		/* my offspring */
} call_node;

#if USE_LD_MACROS
#define	collectSiblingsTime(_)	LDFUNC(collectSiblingsTime, _)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static void	freeProfileData(void);
static void	collectSiblingsTime(void);

#undef LDFUNC_DECLARATIONS

#define WITH_LD_IF_PROFILING(_)		WITH_LD(GD->profile.thread) if(LD _)
#define DEFAULT_SAMPLE_PERIOD		5000	/* usecs: 200 samples/sec */
#define WITH_LD_IF_PROFILING_AND(cond)	WITH_LD_IF_PROFILING( && (cond))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
In the exact modes we read the clock when the profiler starts and ends
updating the call tree (accounting).  The time before is charged to the
node we leave, the time spent updating the tree to the accounting time.

PROF_WALL_EXACT accumulates prof_clock() units in node->raw and converts
these to nsec when the profiler is stopped.  PROF_CPU_EXACT divides time
in windows of sample_period.  When a window is closed, we read the CPU
time of the thread and scale the raw counts of the nodes charged in this
window by the CPU time used in the window divided by its length.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Thread CPU time in nsec.  On Windows, GetThreadTimes() only advances
   with the scheduler tick (about 15ms).  We use QueryThreadCycleTime()
   if it behaves; its unit is calibrated by spinning on the CPU.  The
   PROF_CPU_EXACT mode is only available if the clock is precise.
*/

#ifdef __WINDOWS__
static int    cycles_ok = -1;		/* -1: unknown */
static double cycles_per_ns;
static double cycles_rate;		/* measured, for diagnostics */

static void
check_thread_cycles(void)
{ HANDLE me = GetCurrentThread();
  double best = 0.0;

  for(int i=0; i<3; i++)		/* max of 3 to avoid preemption */
  { ULONG64 c0, c1;
    int64_t ns0, ns1;

    if ( !QueryThreadCycleTime(me, &c0) )
      break;
    ns0 = prof_mono_ns();
    do
    { ns1 = prof_mono_ns();
    } while ( ns1 - ns0 < 1000000 );	/* 1 ms */
    QueryThreadCycleTime(me, &c1);
    if ( c1 > c0 )
    { double rate = (double)(c1-c0)/(double)(ns1-ns0);

      if ( rate > best )
	best = rate;
    }
  }

  cycles_rate = best;
  if ( best > 0.1 && best < 100.0 )	/* 100 MHz .. 100 GHz */
  { cycles_per_ns = best;
    cycles_ok = 1;
  } else
  { cycles_ok = 0;
  }
}
#endif

#define prof_thread_cpu_ns(_) LDFUNC(prof_thread_cpu_ns, _)
static int64_t
prof_thread_cpu_ns(DECL_LD)
{
#ifdef __WINDOWS__
  if ( cycles_ok == 1 )
  { ULONG64 c;

    QueryThreadCycleTime(GetCurrentThread(), &c);
    return (int64_t)((double)c/cycles_per_ns);
  }
#endif
  return llround(ThreadCPUTime(CPU_USER)*1e9);
}

/* True if prof_thread_cpu_ns() is precise: it must advance at least
   twice within 3ms, in steps of less than 0.1ms.
*/

#define precise_thread_cpu_clock(why) LDFUNC(precise_thread_cpu_clock, why)
static bool
precise_thread_cpu_clock(DECL_LD const char **why)
{ static int ok = -1;
  static char msg[200];

  if ( ok == -1 )
  { int64_t start, t0, t, step = 0;
    int changes = 0;

#ifdef __WINDOWS__
    check_thread_cycles();
#endif
    start = prof_mono_ns();		/* after check_thread_cycles() */
    t0 = prof_thread_cpu_ns();
    while ( changes < 2 && prof_mono_ns() - start < 3000000 )
    { if ( (t=prof_thread_cpu_ns()) != t0 )
      { step = t - t0;
	t0 = t;
	changes++;
      }
    }
    ok = (changes == 2 && step > 0 && step < 100000);
    Ssprintf(msg, "thread CPU clock: %d changes in 3ms, last step %" PRId64
	     " ns (need 2 changes, step < 100000 ns)",
	     changes, step);
#ifdef __WINDOWS__
    { size_t len = strlen(msg);

      Ssnprintf(msg+len, sizeof(msg)-len,
		"; QueryThreadCycleTime(): %s, %.3f cycles/ns",
		cycles_ok == 1 ? "used" : "rejected", cycles_rate);
    }
#endif
#ifdef PROF_CLOCK_TSC
    { size_t len = strlen(msg);

      Ssnprintf(msg+len, sizeof(msg)-len, "; invariant TSC: %s",
		invariant_tsc() ? "yes" : "no");
    }
#endif
  }

  *why = msg;
  return ok;
}

/* Rough estimate of the prof_clock() frequency, used to determine the
   window length of PROF_CPU_EXACT.
*/

static double
prof_clock_per_ns(void)
{ static double per_ns = 0.0;

  if ( per_ns == 0.0 )
  { int64_t  ns0 = prof_mono_ns(), ns1;
    uint64_t r0  = prof_clock(),   r1;

    do
    { ns1 = prof_mono_ns();
      r1  = prof_clock();
    } while ( ns1 - ns0 < 1000000 );	/* 1 ms */

    per_ns = (double)(r1-r0)/(double)(ns1-ns0);
  }

  return per_ns;
}

/* Close the CPU window at `now`.  If `lnode` is given, the window ends
   with a long interval of `ldelta` that is charged to `lnode`.  The
   thread can only have been off CPU for a significant time in a single,
   long interval.  We therefore consider the short intervals to be on
   CPU and charge the remaining CPU time to the long interval.
*/

#define PROF_LONG_INTERVAL_NS 10000.0	/* 10 usec */

#define prof_close_window(now, lnode, ldelta) \
	LDFUNC(prof_close_window, now, lnode, ldelta)
static void
prof_close_window(DECL_LD uint64_t now, call_node *lnode, uint64_t ldelta)
{ int64_t cpu  = prof_thread_cpu_ns();
  int64_t dcpu = cpu - LD->profile.clock.win_cpu;
  uint64_t raw = now - LD->profile.clock.win_start - ldelta;
  double scale;				/* nsec per prof_clock() unit */
  call_node *n, *next;

  if ( dcpu < 0 )
    dcpu = 0;
  if ( lnode )
  { double per_ns  = prof_clock_per_ns();
    double short_ns = (double)raw/per_ns;

    if ( (double)dcpu >= short_ns )
    { double long_ns = (double)dcpu - short_ns;
      double max_ns  = (double)ldelta/per_ns;

      if ( long_ns > max_ns )
	long_ns = max_ns;
      lnode->ticks += llround(long_ns);
      scale = 1.0/per_ns;
    } else				/* short intervals were preempted */
    { scale = (double)dcpu/(double)raw;
    }
  } else
  { scale = raw ? (double)dcpu/(double)raw : 0.0;
  }

  for(n=LD->profile.clock.win_nodes; n; n=next)
  { next = n->win_next;
    n->ticks += llround((double)n->raw*scale);
    n->raw = 0;
    n->win_next = NULL;
    n->in_window = false;
  }
  LD->profile.accounting_ticks +=
    llround((double)LD->profile.clock.win_accounting*scale);
  LD->profile.ticks += dcpu;

  LD->profile.clock.win_nodes      = NULL;
  LD->profile.clock.win_accounting = 0;
  LD->profile.clock.win_start      = now;
  LD->profile.clock.win_cpu        = cpu;
}

/* Charge the time since the last clock reading to `node` or, if
   `accounting` is true, to the profiler's accounting.
*/

#define prof_charge(node, accounting) LDFUNC(prof_charge, node, accounting)
static void
prof_charge(DECL_LD call_node *node, bool accounting)
{ uint64_t now = prof_clock();
  int64_t delta = (int64_t)(now - LD->profile.clock.last);

  if ( delta < 0 )			/* unsynchronised counters? */
    delta = 0;
  LD->profile.clock.last = now;
  if ( node && node->magic != PROFNODE_MAGIC )
    node = NULL;

  if ( LD->profile.active == PROF_CPU_EXACT )
  { if ( (uint64_t)delta >= LD->profile.clock.long_interval && node &&
	 !accounting )
    { prof_close_window(now, node, delta);
      return;
    }
    if ( accounting )
    { LD->profile.clock.win_accounting += delta;
    } else if ( node )
    { if ( !node->in_window )
      { node->in_window = true;
	node->win_next = LD->profile.clock.win_nodes;
	LD->profile.clock.win_nodes = node;
      }
      node->raw += delta;
    }
    if ( now - LD->profile.clock.win_start >= LD->profile.clock.win_length )
      prof_close_window(now, NULL, 0);
  } else
  { LD->profile.clock.raw += delta;
    if ( accounting )
      LD->profile.clock.accounting += delta;
    else if ( node )
      node->raw += delta;
  }
}

#define prof_accounting_begin(_) LDFUNC(prof_accounting_begin, _)
static inline void
prof_accounting_begin(DECL_LD)
{ if ( unlikely(PROF_IS_EXACT(LD->profile.active)) )
  { LD->profile.samples++;
    prof_charge(LD->profile.current, false);
  }
  LD->profile.accounting = true;
}

#define prof_accounting_end(_) LDFUNC(prof_accounting_end, _)
static inline void
prof_accounting_end(DECL_LD)
{ if ( unlikely(PROF_IS_EXACT(LD->profile.active)) )
    prof_charge(NULL, true);
  LD->profile.accounting = false;
}

/* Convert the prof_clock() counts collected since activation to nsec,
   calibrating prof_clock() against prof_mono_ns() over the interval.
*/

#define prof_convert_clock(_) LDFUNC(prof_convert_clock, _)
static void
prof_convert_clock(DECL_LD)
{ int64_t  ns  = prof_mono_ns() - LD->profile.clock.start_ns;
  uint64_t raw = LD->profile.clock.last - LD->profile.clock.start;
  double scale = raw ? (double)ns/(double)raw : 0.0;
  call_node *n = LD->profile.roots;

  while ( n )				/* depth-first walk of the tree */
  { n->ticks += llround((double)n->raw*scale);
    n->raw = 0;
    if ( n->siblings )
    { n = n->siblings;
    } else
    { while ( n && !n->next )
	n = n->parent;
      if ( n )
	n = n->next;
    }
  }

  LD->profile.ticks += llround((double)LD->profile.clock.raw*scale);
  LD->profile.accounting_ticks +=
    llround((double)LD->profile.clock.accounting*scale);
  LD->profile.clock.raw = 0;
  LD->profile.clock.accounting = 0;
}


bool
activateProfiler(DECL_LD prof_status active)
{ int i;
  PL_local_data_t *profiling;

  const char *why;
  if ( active == PROF_CPU_EXACT && !precise_thread_cpu_clock(&why) )
    return PL_error(NULL, 0, why, ERR_NOT_IMPLEMENTED,
		    "exact_cputime");

  PL_LOCK(L_THREAD);

  if ( active && (profiling=GD->profile.thread) )
  { term_t tid = PL_new_term_ref();
    char msg[100];

    PL_unify_thread_id(tid, LD->thread.info->pl_tid);
    Ssprintf(msg, "Already profiling thread %d",
	     profiling->thread.info->pl_tid);

    PL_UNLOCK(L_THREAD);

    return PL_error(NULL, 0, msg, ERR_PERMISSION,
		    ATOM_profile, ATOM_thread, tid);
  }

  LD->profile.active = active;
  for(i=0; i<MAX_PROF_TYPES; i++)
  { if ( types[i] && types[i]->activate )
      (*types[i]->activate)(active);
  }

  if ( active )
  { LD->profile.mode              = active;
    LD->profile.time_at_start     = PROF_IS_CPU(active)
					? ThreadCPUTime(CPU_USER)
					: WallTime();
    LD->profile.time_at_last_tick = 0;
    if ( PROF_IS_EXACT(active) )
    { unsigned int period = LD->profile.sample_period;

#ifdef PROF_CLOCK_TSC
      invariant_tsc();
#endif
      if ( !period )
	period = DEFAULT_SAMPLE_PERIOD;
      LD->profile.clock.win_length = (uint64_t)(prof_clock_per_ns() *
						(double)period*1000.0);
      LD->profile.clock.long_interval = (uint64_t)(prof_clock_per_ns() *
						   PROF_LONG_INTERVAL_NS);
      LD->profile.clock.start_ns       = prof_mono_ns();
      LD->profile.clock.win_cpu        = prof_thread_cpu_ns();
      LD->profile.clock.start          = prof_clock();
      LD->profile.clock.last           = LD->profile.clock.start;
      LD->profile.clock.win_start      = LD->profile.clock.start;
      LD->profile.clock.raw            = 0;
      LD->profile.clock.accounting     = 0;
      LD->profile.clock.win_accounting = 0;
      LD->profile.clock.win_nodes      = NULL;
    }

    GD->profile.thread = LD;
  } else
  { GD->profile.thread = NULL;
  }

  PL_UNLOCK(L_THREAD);

  updateAlerted(LD);

  LD->profile.sum_ok = false;

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return the time since the last tick in nanoseconds.  We keep the time of
the last tick as nanoseconds since the profiler was activated, such that
the ticks add up exactly to the profiled time and no rounding error
accumulates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int64_t
thread_prof_ticks(DECL_LD)
{ double now = PROF_IS_CPU(LD->profile.active) ? ThreadCPUTime(CPU_USER)
					      : WallTime();
  int64_t t0 = LD->profile.time_at_last_tick;
  int64_t t1 = llround((now - LD->profile.time_at_start)*1e9);

  if ( t1 < t0 )				/* clock went backwards */
    return 0;
  LD->profile.time_at_last_tick = t1;

  DEBUG(MSG_PROF_TICKS,
	Sdprintf("%" PRId64 " ns\n", t1-t0));

  return t1-t0;					/* nanoseconds */
}


#ifdef __WINDOWS__

#include <math.h>

static UINT         timer;			/* our MM timer */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MS-Windows version
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if (_MSC_VER < 1400) && !defined(__MINGW32__)
typedef DWORD DWORD_PTR;
#endif

static void CALLBACK
callTimer(UINT id, UINT msg, DWORD_PTR dwuser, DWORD_PTR dw1, DWORD_PTR dw2)
{ WITH_LD_IF_PROFILING()
  { profile(thread_prof_ticks());
  }
}


static bool
startProfiler(prof_status how)
{ GET_LD
  MMRESULT rval;

  if ( PROF_IS_EXACT(how) )		/* no sampling */
    return activateProfiler(how);
  if ( !LD->profile.sample_period )	/* profiler/2 without $profile/4 */
    LD->profile.sample_period = DEFAULT_SAMPLE_PERIOD;

  if ( !activateProfiler(PROF_CPU) )	/* e.g., already profiling */
    return false;

  rval = timeSetEvent(10,
		      round(LD->profile.sample_period/1000),		/* resolution (milliseconds) */
		      callTimer,
		      (DWORD_PTR)0,
		      TIME_PERIODIC);

  if ( rval )
  { timer = rval;
    return true;
  }

  activateProfiler(PROF_INACTIVE);
  return PL_error(NULL, 0, NULL, ERR_SYSCALL, "timeSetEvent");
}


void
stopItimer(void)
{ if ( timer )
  { timeKillEvent(timer);
    timer = 0;
  }
}

#else /*__WINDOWS__*/

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

static struct itimerval value, ovalue;	/* itimer controlling structures */
static int itimer = -1;			/* ITIMER_* */
static int timer_signal;		/* SIG* */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SIGPROF and SIGALRM are process signals  and may be delivered to any thread,
including threads that are not Prolog  threads.   We  forward  them to the
profiled thread, such that the sample is  taken in its context.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
sig_profile(int sig)
{
#ifdef O_PLMT
  PL_local_data_t *ld = GD->profile.thread;

  if ( ld && ld != GLOBAL_LD )
  { PL_thread_info_t *info = ld->thread.info;

    if ( info && info->has_tid )
      pthread_kill(info->tid, sig);
    return;
  }
#else
  (void)sig;
#endif

  WITH_LD_IF_PROFILING()
  { profile(thread_prof_ticks());
  }
}


static bool
startProfiler(prof_status how)
{ GET_LD
  int sig, timer;

  if ( PROF_IS_EXACT(how) )		/* no sampling */
    return activateProfiler(how);
  if ( !LD->profile.sample_period )	/* profiler/2 without $profile/4 */
    LD->profile.sample_period = DEFAULT_SAMPLE_PERIOD;

  if ( how == PROF_CPU )
  { sig   = SIGPROF;
    timer = ITIMER_PROF;
  } else
  { sig   = SIGALRM;
    timer = ITIMER_REAL;
  }

  if ( !activateProfiler(how) )		/* e.g., already profiling */
    return false;

  set_sighandler(sig, sig_profile);
  timer_signal = sig;

  value.it_interval.tv_sec  = 0;
  value.it_interval.tv_usec = LD->profile.sample_period;  // period in usecs.
  value.it_value.tv_sec  = 0;			/* on systems where 0 means now */
  value.it_value.tv_usec = LD->profile.sample_period;

  if ( setitimer(timer, &value, &ovalue) != 0 )
  { activateProfiler(PROF_INACTIVE);
    set_sighandler(sig, SIG_IGN);
    timer_signal = 0;
    return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, setitimer);
  }

  itimer = timer;

  return true;
}

void
stopItimer(void)
{ if ( itimer != -1 )
  { value.it_interval.tv_sec  = 0;
    value.it_interval.tv_usec = 0;
    value.it_value.tv_sec  = 0;
    value.it_value.tv_usec = 0;

    if ( setitimer(itimer, &value, &ovalue) != 0 )
    { warning("Failed to stop interval timer: %s", OsError());
      return;
    }

    itimer = -1;
  }
}

#endif /*__WINDOWS__*/

bool
stopProfiler(void)
{ WITH_LD_IF_PROFILING_AND(LD->profile.active)
  { double tend = PROF_IS_CPU(LD->profile.active) ? ThreadCPUTime(CPU_USER)
						 : WallTime();

    LD->profile.time += tend - LD->profile.time_at_start;

    if ( PROF_IS_EXACT(LD->profile.active) )
    { prof_charge(LD->profile.current, false);
      if ( LD->profile.active == PROF_CPU_EXACT )
	prof_close_window(LD->profile.clock.last, NULL, 0);
      else
	prof_convert_clock();
    } else
    { stopItimer();
#ifndef __WINDOWS__
      set_sighandler(timer_signal, SIG_IGN);
      timer_signal = 0;
#endif
    }
    activateProfiler(PROF_INACTIVE);
  }

  return true;
}


/** profiler(-Old, +New)

Unify Old with the state of the profiler and set it according to New.
*/

static bool
get_prof_status(term_t t, prof_status *s)
{ GET_LD
  atom_t a;

  if ( PL_get_atom_ex(t, &a) )
  { switch(a)
    { case ATOM_false:
	*s = PROF_INACTIVE;
        return true;
      case ATOM_true:
      case ATOM_cputime:
	*s = PROF_CPU;
        return true;
      case ATOM_walltime:
	*s = PROF_WALL;
        return true;
      case ATOM_exact_walltime:
	*s = PROF_WALL_EXACT;
        return true;
      case ATOM_exact_cputime:
	*s = PROF_CPU_EXACT;
        return true;
      default:
	PL_domain_error("profile_status", t);
        return false;
    }
  }

  return false;
}


static atom_t
prof_status_atom(prof_status s)
{ switch(s)
  { case PROF_CPU:	  return ATOM_cputime;
    case PROF_WALL:	  return ATOM_walltime;
    case PROF_WALL_EXACT: return ATOM_exact_walltime;
    case PROF_CPU_EXACT:  return ATOM_exact_cputime;
    default:		  return ATOM_false;
  }
}

static
PRED_IMPL("profiler", 2, profiler, 0)
{ PRED_LD
  prof_status val;
  int rc;

  if ( !PL_unify_atom(A1, prof_status_atom(LD->profile.active)) )
    return false;
  if ( (rc=PL_compare(A1, A2)) == CMP_EQUAL )
    return true;
  if ( rc == CMP_ERROR )
    return false;
  if ( !get_prof_status(A2, &val) )
    return false;
  if ( val == LD->profile.active )
    succeed;

  if ( val )
    return startProfiler(val);
  else
    return stopProfiler();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Prolog query API:

$prof_sibling_of(?Child, ?Parent)
	Generate hierachy.  If Parent is '-', generate the roots

$prof_node(+Node, -Pred, -Calls, -Redos, -Exits,
	   -Recursive, -Ticks, -SiblingTicks)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define get_node(t, node) LDFUNC(get_node, t, node)
static bool
get_node(DECL_LD term_t t, call_node **node)
{ if ( PL_is_functor(t, FUNCTOR_dprof_node1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    _PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &ptr) )
    { call_node *n = ptr;

      if ( n->magic == PROFNODE_MAGIC )
      { *node = n;
        return true;
      }
    }
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_profile_node, t);
}


#define unify_node(t, node) LDFUNC(unify_node, t, node)
static bool
unify_node(DECL_LD term_t t, call_node *node)
{ return PL_unify_term(t,
		       PL_FUNCTOR, FUNCTOR_dprof_node1,
			 PL_POINTER, node);
}


static
PRED_IMPL("$prof_sibling_of", 2, prof_sibling_of, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  call_node *parent = NULL;
  call_node *sibling = NULL;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { atom_t a;

      if ( !PL_is_variable(A1) )
      { if ( get_node(A1, &sibling) )
	{ if ( sibling->parent )
	    return unify_node(A2, sibling->parent);
	}
	fail;
      } else
      { if ( PL_get_atom(A2, &a) && a == ATOM_minus )
	  sibling = LD->profile.roots;
	else if ( get_node(A2, &parent) )
	  sibling = parent->siblings;
	else
	  return false;
      }

      if ( !sibling )
	fail;

      goto return_sibling;
    }
    case FRG_REDO:
    { sibling = CTX_PTR;

    return_sibling:
      if ( !unify_node(A1, sibling) )
	return false;
      if ( sibling->next )
	ForeignRedoPtr(sibling->next);
      return true;
    }
    case FRG_CUTTED:
    default:
      return true;
  }
}


static bool
identify_def(term_t t, void *handle)
{ return unify_definition(MODULE_user, t, handle, 0, GP_QUALIFY|GP_NAMEARITY);
}


static bool
unify_node_id(term_t t, call_node *n)
{ if ( n->type->magic == PROFTYPE_MAGIC )
  { return (*n->type->unify)(t, n->handle);
  } else
  { GET_LD

    return PL_unify_pointer(t, n->handle);
  }
}


static
PRED_IMPL("$prof_node", 8, prof_node, 0)
{ PRED_LD
  call_node *n = NULL;

  if ( !get_node(A1, &n) )
    return false;

  collectSiblingsTime();

  return ( unify_node_id(A2, n) &&
	   PL_unify_uint64(A3, n->calls) &&
	   PL_unify_uint64(A4, n->exits + n->fails - n->calls) &&
	   PL_unify_uint64(A5, n->exits) &&
	   PL_unify_uint64(A6, n->recur) &&
	   PL_unify_uint64(A7, n->ticks) &&
	   PL_unify_uint64(A8, n->sibling_ticks) );
}


		 /*******************************
		 *	    COLLECT DATA	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
prof_procedure_data(+PredicateIndicator,
		    -TimeSelf, -TimeSiblings, -Parents, -Siblings)
    Where Parents  = list_of(Relative)
      And Siblings = list_of(Relative)
      and Relative = node(Pred, CycleID, Ticks, SiblingTicks,
			  Calls, Redos, Exits)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct prof_ref
{ struct prof_ref *next;		/* next in chain */
  void * handle;			/* Procedure handle */
  PL_prof_type_t   *type;
  int   cycle;
  uint64_t ticks;
  uint64_t sibling_ticks;
  uint64_t calls;			/* calls to/from this predicate */
  uint64_t redos;			/* redos to/from this predicate */
  uint64_t exits;			/* exits to/from this predicate */
} prof_ref;


typedef struct
{ Definition def;
  uint64_t ticks;
  uint64_t sibling_ticks;
  uint64_t calls;
  uint64_t redos;
  uint64_t exits;
  uint64_t recur;
  prof_ref *callers;
  prof_ref *callees;
} node_sum;


static void
free_relatives(prof_ref *r)
{ prof_ref *n;

  for( ; r; r=n)
  { n = r->next;
    freeHeap(r, sizeof(*r));
  }
}


#define DEF_SPONTANEOUS (Definition)0
#define DEF_RECURSIVE   (Definition)1


static void
add_parent_ref(node_sum *sum,
	       call_node *self,
	       void *handle, PL_prof_type_t *type,
	       int cycle)
{ prof_ref *r;

  sum->calls += self->calls;
  sum->redos += self->exits + self->fails - self->calls;
  sum->exits += self->exits;

  for(r=sum->callers; r; r=r->next)
  { if ( r->handle == handle && r->cycle == cycle )
    { r->calls += self->calls;
      r->redos += self->exits + self->fails - self->calls;
      r->exits += self->exits;
      r->ticks += self->ticks;
      r->sibling_ticks += self->sibling_ticks;

      return;
    }
  }

  r = allocHeapOrHalt(sizeof(*r));
  r->calls = self->calls;
  r->redos = self->exits + self->fails - self->calls;
  r->exits = self->exits;
  r->ticks = self->ticks;
  r->sibling_ticks = self->sibling_ticks;
  r->handle = handle;
  r->type = type;
  r->cycle = cycle;
  r->next = sum->callers;
  sum->callers = r;
}


static void
add_recursive_ref(node_sum *sum, uint64_t count, int cycle)
{ prof_ref *r;

  for(r=sum->callers; r; r=r->next)
  { if ( r->handle == DEF_RECURSIVE && r->cycle == cycle )
    { r->calls += count;

      return;
    }
  }

  r = allocHeapOrHalt(sizeof(*r));
  memset(r, 0, sizeof(*r));
  r->calls = count;
  r->handle = DEF_RECURSIVE;
  r->cycle = cycle;
  r->next = sum->callers;
  sum->callers = r;
}


static void
add_sibling_ref(node_sum *sum, call_node *sibling, int cycle)
{ prof_ref *r;

  for(r=sum->callees; r; r=r->next)
  { if ( r->handle == sibling->handle && r->cycle == cycle )
    { r->calls += sibling->calls;
      r->redos += sibling->exits + sibling->fails - sibling->calls;
      r->exits += sibling->exits;
      r->ticks += sibling->ticks;
      r->sibling_ticks += sibling->sibling_ticks;

      return;
    }
  }

  r = allocHeapOrHalt(sizeof(*r));
  r->calls = sibling->calls;
  r->redos = sibling->exits + sibling->fails - sibling->calls;
  r->exits = sibling->exits;
  r->ticks = sibling->ticks;
  r->sibling_ticks = sibling->sibling_ticks;
  r->handle = sibling->handle;
  r->type = sibling->type;
  r->cycle = cycle;
  r->next = sum->callees;
  sum->callees = r;
}




#define sumProfile(n, handle, type, sum, seen) \
	LDFUNC(sumProfile, n, handle, type, sum, seen)

static int
sumProfile(DECL_LD call_node *n, void *handle, PL_prof_type_t *type,
	   node_sum *sum, int seen)
{ call_node *s;
  int count = 0;

  if ( n->handle == handle )
  { if ( PL_handle_signals() < 0 )
      return -1;

    count++;
    sum->ticks += n->ticks;
    if ( !seen )
      sum->sibling_ticks += n->sibling_ticks;
    else			/* nested: n's ticks are in the outer siblings */
      sum->sibling_ticks -= n->ticks;

    if ( n->parent )
      add_parent_ref(sum, n, n->parent->handle, n->parent->type, seen);
    else
      add_parent_ref(sum, n, DEF_SPONTANEOUS, NULL, seen);

    if ( n->recur )
      add_recursive_ref(sum, n->recur, seen);

    for(s=n->siblings; s; s = s->next)
      add_sibling_ref(sum, s, seen);

    seen++;
  }

  for(s=n->siblings; s; s = s->next)
  { int c = sumProfile(s, handle, type, sum, seen);
    if ( c < 0 )
      return c;
    count += c;
  }

  return count;
}


#define unify_relatives(list, r) LDFUNC(unify_relatives, list, r)
static bool
unify_relatives(DECL_LD term_t list, prof_ref *r)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  term_t tmp = PL_new_term_ref();

  for( ; r; r=r->next)
  { bool rc;

    if ( !PL_unify_list(tail, head, tail) )
      return false;

    PL_put_variable(tmp);
    if ( r->handle == DEF_SPONTANEOUS )
      rc=PL_unify_atom_chars(tmp, "<spontaneous>");
    else if ( r->handle == DEF_RECURSIVE )
      rc=PL_unify_atom_chars(tmp, "<recursive>");
    else
      rc=(*r->type->unify)(tmp, r->handle);

    if ( !rc ||
	 !PL_unify_term(head, PL_FUNCTOR, FUNCTOR_node7,
			PL_TERM, tmp,
			PL_INT,  r->cycle,
			PL_INT64, r->ticks,
			PL_INT64, r->sibling_ticks,
			PL_INT64, r->calls,
			PL_INT64, r->redos,
			PL_INT64, r->exits) )
      return false;
  }

  return PL_unify_nil(tail);
}


static bool
get_def(term_t t, void **handle)
{ Procedure proc;

  if ( get_procedure(t, &proc, 0, GP_FIND|GP_NAMEARITY) )
  { *handle = proc->definition;
    return true;
  }

  return false;
}


static bool
get_handle(term_t t, void **handle)
{ for(int i=0; i<MAX_PROF_TYPES; i++)
  { if ( types[i] && types[i]->get &&
	 (*types[i]->get)(t, handle) )
      return true;
  }

  return false;
}

static void
prof_release_def(void *handle)
{ Definition def = handle;

  releaseModule(def->module);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$prof_procedure_data(+PredicateIndicator,
		     -Ticks, -TicksSiblings,
		     -Calls, -Redos, -Exits,
		     -Callers, -Callees)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static
PRED_IMPL("$prof_procedure_data", 8, prof_procedure_data, PL_FA_TRANSPARENT)
{ PRED_LD
  void *handle;
  node_sum sum;
  call_node *n;
  bool rc;
  int count = 0;

  if ( !get_handle(A1, &handle) )
    return false;

  collectSiblingsTime();
  memset(&sum, 0, sizeof(sum));
  for(n=LD->profile.roots; n; n=n->next)
  { int c = sumProfile(n, handle, &prof_default_type, &sum, 0);
    if ( c < 0 )
      return false;
    count += c;
  }

  if ( count == 0 )
    return false;			/* nothing known about this one */

  rc = ( PL_unify_uint64(A2, sum.ticks) &&
	 PL_unify_uint64(A3, sum.sibling_ticks) &&
	 PL_unify_uint64(A4, sum.calls) &&
	 PL_unify_uint64(A5, sum.redos) &&
	 PL_unify_uint64(A6, sum.exits) &&
	 unify_relatives(A7, sum.callers) &&
	 unify_relatives(A8, sum.callees)
       );

  free_relatives(sum.callers);
  free_relatives(sum.callees);

  return rc;
}


/** '$prof_statistics'(-Samples, -Ticks, -AccountingTicks,
		       -Time, -Nodes, -Period, -Ports, -Mode)

@arg Samples is the number of times the statistical profiler was called
     or, for the exact modes, the number of call tree node switches.
@arg Ticks   is the sampled time during profiling in nanoseconds
@arg AccountingTicks is the sampled time spent on accounting (nsec)
@arg Time    is the total CPU time spent profiling
@arg Nodes   is the number of nodes in the call tree
@arg Period  is the number of microseconds between samples
@arg Ports   is one of `true`, `false` or `classic`
@arg Mode    is the profiler/2 mode that collected the data or `false`
*/

static
PRED_IMPL("$prof_statistics", 8, prof_statistics, 0)
{ PRED_LD

  return
    ( PL_unify_uint64(A1, LD->profile.samples) &&
      PL_unify_uint64(A2, LD->profile.ticks) &&
      PL_unify_uint64(A3, LD->profile.accounting_ticks) &&
      PL_unify_float( A4, LD->profile.time) &&
      PL_unify_uint64(A5, LD->profile.nodes) &&
      PL_unify_integer(A6, LD->profile.sample_period) &&
      PL_unify_atom(A7, LD->profile.ports_control == PROFC_FALSE ? ATOM_false :
			LD->profile.ports_control == PROFC_TRUE  ? ATOM_true :
								   ATOM_classic) &&
      PL_unify_atom(A8, prof_status_atom(LD->profile.mode))
    );
}


		 /*******************************
		 *	       RESET		*
		 *******************************/

#if USE_LD_MACROS
#define prof_clear_environments(fr) LDFUNC(prof_clear_environments, fr)
#define prof_clear_choicepoints(ch) LDFUNC(prof_clear_choicepoints, ch)
#define prof_clear_stacks(fr, ch) LDFUNC(prof_clear_stacks, fr, ch)
#endif

static QueryFrame
prof_clear_environments(DECL_LD LocalFrame fr)
{ if ( fr == NULL )
    return NULL;

  for(;;)
  { if ( ison(fr, FR_MARKED) )
      return NULL;
    set(fr, FR_MARKED);
    LD->gc._local_frames++;

    fr->prof_node = NULL;

    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
      return queryOfFrame(fr);
  }
}


static void
prof_clear_choicepoints(DECL_LD Choice ch)
{ for( ; ch; ch = ch->parent )
  { LD->gc._choice_count++;
    prof_clear_environments(ch->frame);
  }
}


static void
prof_clear_stacks(DECL_LD LocalFrame fr, Choice ch)
{ QueryFrame qf;

  while(fr)
  { qf = prof_clear_environments(fr);
    assert(qf->magic == QID_MAGIC);
    prof_clear_choicepoints(ch);
    if ( qf->parent )
    { QueryFrame pqf = qf->parent;

      if ( !(fr = pqf->registers.fr) )
	fr = qf->saved_environment;
      ch = qf->saved_bfr;
    } else
      break;
  }
}


bool
resetProfiler(DECL_LD)
{ if ( LD->profile.active )		/* do not stop another thread */
    stopProfiler();

  assert(LD->gc._local_frames == 0);
  assert(LD->gc._choice_count == 0);

  prof_clear_stacks(environment_frame, LD->choicepoints);
  unmark_stacks(environment_frame, LD->choicepoints, FR_MARKED);

  assert(LD->gc._local_frames == 0);
  assert(LD->gc._choice_count == 0);

  freeProfileData();
  LD->profile.samples          = 0;
  LD->profile.ticks            = 0;
  LD->profile.accounting_ticks = 0;
  LD->profile.time             = 0.0;
  LD->profile.mode             = PROF_INACTIVE;
  LD->profile.accounting       = false;

  succeed;
}


static
PRED_IMPL("reset_profiler", 0, reset_profiler, 0)
{ resetProfiler();

  succeed;
}


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

static
PRED_IMPL("$profile", 4, profile, PL_FA_TRANSPARENT)
{ bool rc;
  prof_status val;

  if ( !get_prof_status(A2, &val) )
    return false;

  atom_t ports_opt;
  if ( !PL_get_atom_ex(A3, &ports_opt) )
    return false;
  switch (ports_opt)
  { case ATOM_false: LD->profile.ports_control = PROFC_FALSE;   break;
    case ATOM_true:  LD->profile.ports_control = PROFC_TRUE;    break;
    default:         LD->profile.ports_control = PROFC_CLASSIC;
  }

  LD->profile.sample_period = DEFAULT_SAMPLE_PERIOD;
  double rate;
  if ( PL_get_float_ex(A4, &rate) )
    LD->profile.sample_period = (unsigned int)(1000000/rate);
  else
    return false;

  resetProfiler();
  if ( !startProfiler(val) )
    return false;
  rc = callProlog(NULL, A1, PL_Q_PASS_EXCEPTION, NULL);
  stopProfiler();

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is responsible for collection  the profiling statistics at
run time. It is called from a  dedicated profiler thread that by default
triggers this function every 5ms.  First, this calls thread_prof_ticks()
to determine the number of  ms  the   relevant  clock  (CPU or wall) has
progressed and then calls this function with `count` set to the relevant
clock increment, i.e., count is a number in the range 0..5.

This function just ticks the leaf node   in  the dynamic call graph. The
function collectSiblingsTime() propagates these   upward when collecting
statistics.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
profile(DECL_LD int64_t count)
{ call_node *node;

  if ( !HAS_LD )
    return;

  LD->profile.samples++;
  LD->profile.ticks += count;

  if ( LD->profile.accounting )			/* we are updating nodes */
  { LD->profile.accounting_ticks += count;
  } else if ( (node=LD->profile.current) && node->magic == PROFNODE_MAGIC )
  { node->ticks += count;
  }
}

		 /*******************************
		 *     HIERARCHY ACCOUNTING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
profCall(Definition handle)

A call was made from the  current  node   to  handle.  This  builds up a
dynamic call tree. The tree is constructed as follows:

  - If there is no current node
    - If the root-set contains a node for `handle`, use it
    - Else create a new root node.
  - If the current node has the same handle, increment `node->recur`
  - If somewhere in the parent chain we find the same parent-child
    transition, we tick this node and return it.  For example, given
    p->q->r->p, a call to q returns the q parent node and increments
    it recursion count.
    JW: This seems wrong
	- It breaks the propagation of self-ticks in the tree.
	- I think the second `p` also creates a second cycle.
  - If the current node has a sibling with `handle`, return it.
  - Else, add a new sibling.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG
static char *
node_name(call_node *n)
{ static char buf[100];

  if ( n->type == &prof_default_type )
    return predicateName(n->handle);

  Ssprintf(buf, "%p", n->handle);
  return buf;
}
#endif

#define prof_call(handle, type) LDFUNC(prof_call, handle, type)
static call_node *
prof_call(DECL_LD void *handle, PL_prof_type_t *type)
{ call_node *node = LD->profile.current;

  prof_accounting_begin();

  if ( !node )				/* root-node of the profile */
  { for(node = LD->profile.roots; node; node=node->next)
    { if ( node->handle == handle )
      { node->calls++;
	LD->profile.current = node;
	DEBUG(MSG_PROF_CALLTREE,
	      Sdprintf("Call: existing root %s\n", node_name(node)));

	prof_accounting_end();
	return node;
      }
    }

    node = allocHeapOrHalt(sizeof(*node));
    memset(node, 0, sizeof(*node));
    LD->profile.nodes++;

    node->magic = PROFNODE_MAGIC;
    node->handle = handle;
    node->type = type;
    node->calls++;
    node->next = LD->profile.roots;
    LD->profile.roots = node;
    LD->profile.current = node;
    prof_accounting_end();
    DEBUG(MSG_PROF_CALLTREE,
	  Sdprintf("Call: new root %s\n", node_name(node)));

    return node;
  }

					/* straight recursion */
  if ( node->handle == handle )
  { node->recur++;
    DEBUG(MSG_PROF_CALLTREE,
	  Sdprintf("Call: direct recursion on %s\n", node_name(node)));
    prof_accounting_end();
    return node;
  } else if ( LD->profile.ports_control != PROFC_TRUE )
  { void *parent = node->handle;

    for(node=node->parent; node; node = node->parent)
    { if ( node->handle == handle &&
	   node->parent &&
	   node->parent->handle == parent )
      { node->recur++;

	LD->profile.current = node;
	DEBUG(MSG_PROF_CALLTREE,
	      Sdprintf("Call: indirect recursion on %s\n", node_name(node)));
	prof_accounting_end();
	return node;
      }
    }
  }

  for(node=LD->profile.current->siblings; node; node=node->next)
  { if ( node->handle == handle )
    { LD->profile.current = node;
      node->calls++;
      DEBUG(MSG_PROF_CALLTREE,
	    Sdprintf("Call: existing child %s\n", node_name(node)));
      prof_accounting_end();
      return node;
    }
  }

  node = allocHeapOrHalt(sizeof(*node));
  memset(node, 0, sizeof(*node));
  LD->profile.nodes++;
  node->magic = PROFNODE_MAGIC;
  node->handle = handle;
  node->type = type;
  node->parent = LD->profile.current;
  node->calls++;
  node->next = LD->profile.current->siblings;
  LD->profile.current->siblings = node;
  LD->profile.current = node;
  DEBUG(MSG_PROF_CALLTREE,
	Sdprintf("Call: new child %s\n", node_name(node)));
  prof_accounting_end();

  return node;
}


call_node *
profCall(DECL_LD Definition def)
{ if ( ison(def, P_NOPROFILE) )
    return LD->profile.current;

  acquireModulePtr(def->module);

  return prof_call(def, &prof_default_type);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Exit, resuming execution in node. Note that   we  ignore the node if the
magic doesn't fit. That can  happen   using  tprofile/1 because the stop
doesn't need to match the  start.   Actually,  we should clear prof_node
references when clearing the data, but  this is rather complicated (must
be synchornised with atom-gc) and  it   still  doesn't cope with foreign
code.  Considering this is development only, we'll leave this for now.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
profResumeParent(DECL_LD struct call_node *node)
{ call_node *n;

  if ( node && node->magic != PROFNODE_MAGIC )
    return;

  prof_accounting_begin();

  if ( LD->profile.ports_control  != PROFC_FALSE )
  { for(n=LD->profile.current; n && n != node; n=n->parent)
    { DEBUG(MSG_PROF_CALLTREE,
	    Sdprintf("Exit: %s\n", node_name(n)));
      n->exits++;
    }
  }
  LD->profile.current = node;

  prof_accounting_end();

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

profExit(call_node from_node)

Exit to the parent of from_node. Note that all nodes from the current
node back to and including from_node are deemed to have exited. This
catches all normal exits (from_node == current) and last calls (from_node
is an ancestor of current).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
profExit(DECL_LD struct call_node *node)
{ if ( !node || node->magic != PROFNODE_MAGIC )
    return;

  profResumeParent(node->parent);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
profFail(call_node cp_node)

Fail to choicepoint node. All nodes in  the branch from the current node
back to a common ancestor with cp_node   are deemed to have failed. Note
that if the current node is the  choicepoint node, nothing fails as this
is internal to the predicate (doesn't pass through Fail port).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
lookback(call_node *from_node, call_node *to_node)
{ struct call_node *node = from_node;

  while( node )
  { if ( node == to_node )
      return true;
    else
      node = node->parent;
  }

  return false;
}

static void
profFailToCP(struct call_node *current, struct call_node *cp_node)
{ while (current && (!lookback(cp_node, current)))
  { DEBUG(MSG_PROF_CALLTREE,
	  Sdprintf("Fail: %s\n", node_name(current)));

    if (current->magic == PROFNODE_MAGIC)
      current->fails++;

    current = current->parent;
  }
}

void
profFail(DECL_LD struct call_node *cp_node)
{ prof_accounting_begin();

  if ( LD->profile.ports_control != PROFC_FALSE )
    profFailToCP(LD->profile.current, cp_node);
  LD->profile.current = cp_node;

  prof_accounting_end();
}

void
profSetHandle(struct call_node *node, void *handle)
{ node->handle = handle;
}


		 /*******************************
		 *	 FOREIGN ACCESS		*
		 *******************************/

bool
PL_register_profile_type(PL_prof_type_t *type)
{ int i;

  for(i=0; i<MAX_PROF_TYPES; i++)
  { if ( types[i] == type )
      return true;
  }
  for(i=0; i<MAX_PROF_TYPES; i++)
  { if ( !types[i] )
    { types[i] = type;
      type->magic = PROFTYPE_MAGIC;
      return true;
    }
  }

  assert(0);
  return false;
}


void *
PL_prof_call(void *handle, PL_prof_type_t *type)
{ GET_LD

  return prof_call(handle, type);
}

void
PL_prof_exit(void *node)
{ GET_LD
  struct call_node *n = node;

  profResumeParent(n->parent);
}


		 /*******************************
		 *	       COLLECT		*
		 *******************************/


static uint64_t
collectSiblingsNode(call_node *n)
{ call_node *s;
  uint64_t count = 0;

  for(s=n->siblings; s; s=s->next)
  { count += collectSiblingsNode(s);
    n->sibling_ticks = count;
  }

  return count+n->ticks;
}


static void
collectSiblingsTime(DECL_LD)
{ if ( !LD->profile.sum_ok )
  { call_node *n;

    for(n=LD->profile.roots; n; n=n->next)
      collectSiblingsNode(n);

    LD->profile.sum_ok = true;
  }
}


#define freeProfileNode(node) LDFUNC(freeProfileNode, node)
static void
freeProfileNode(DECL_LD call_node *node)
{ call_node *n, *next;

  assert(node->magic == PROFNODE_MAGIC);

  for(n=node->siblings; n; n=next)
  { next = n->next;

    if ( n->type && n->type->release )
      (*n->type->release)(n->handle);

    freeProfileNode(n);
  }

  node->magic = 0;
  freeHeap(node, sizeof(*node));
  LD->profile.nodes--;
}


static void
freeProfileData(void)
{ GET_LD
  call_node *n, *next;

  n = LD->profile.roots;
  LD->profile.roots = NULL;
  LD->profile.current = NULL;

  for(; n; n=next)
  { next = n->next;
    freeProfileNode(n);
  }

  assert(LD->profile.nodes == 0);
}

#else /* !O_PROFILE */

bool
PL_register_profile_type(PL_prof_type_t *type)
{ return false;
}

void *
PL_prof_call(void *handle, PL_prof_type_t *type)
{ return NULL;
}

void
PL_prof_exit(void *node)
{
}


#endif /* O_PROFILE */

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(profile)
#if O_PROFILE
  PRED_DEF("$profile", 4, profile, PL_FA_TRANSPARENT)
  PRED_DEF("profiler", 2, profiler, 0)
  PRED_DEF("reset_profiler", 0, reset_profiler, 0)
  PRED_DEF("$prof_node", 8, prof_node, 0)
  PRED_DEF("$prof_sibling_of", 2, prof_sibling_of, PL_FA_NONDETERMINISTIC)
  PRED_DEF("$prof_procedure_data", 8, prof_procedure_data, PL_FA_TRANSPARENT)
  PRED_DEF("$prof_statistics", 8, prof_statistics, 0)
#endif
EndPredDefs
