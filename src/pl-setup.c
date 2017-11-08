/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
                              VU University Amsterdam
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

#define GLOBAL SO_LOCAL			/* allocate global variables here */
#include "pl-incl.h"
#include "os/pl-cstack.h"
#include "pl-dbref.h"
#include "pl-trie.h"
#include "pl-tabling.h"
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))

#undef K
#undef MB
#define K * 1024
#define MB * (1024L * 1024L)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module initialises the system and defines the global variables.  It
also holds the code  for  dynamically  expanding  stacks  based  on  MMU
access.   Finally  it holds the code to handle signals transparently for
foreign language code or packages with which Prolog was linked together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int allocStacks(size_t local, size_t global, size_t trail);
static void initSignals(void);
static void gcPolicy(Stack s, int policy);

int
setupProlog(void)
{ GET_LD
  DEBUG(1, Sdprintf("Starting Heap Initialisation\n"));

#ifdef O_LOGICAL_UPDATE
  next_global_generation();
#endif

  LD->critical = 0;
  LD->magic = LD_MAGIC;
  LD->signal.pending[0] = 0;
  LD->signal.pending[1] = 0;
  LD->statistics.start_time = WallTime();

  startCritical;
  DEBUG(1, Sdprintf("wam_table ...\n"));
  initWamTable();
  DEBUG(1, Sdprintf("character types ...\n"));
  initCharTypes();
  DEBUG(1, Sdprintf("foreign predicates ...\n"));
  initForeign();
#if HAVE_SIGNAL
  DEBUG(1, Sdprintf("Prolog Signal Handling ...\n"));
  initSignals();
#endif
  DEBUG(1, Sdprintf("Stacks ...\n"));
  if ( !initPrologStacks(GD->options.localSize,
			 GD->options.globalSize,
			 GD->options.trailSize) )
    fatalError("Not enough address space to allocate Prolog stacks");
  initPrologLocalData(PASS_LD1);
  LD->tabling.node_pool.limit = GD->options.tableSpace;

  DEBUG(1, Sdprintf("Atoms ...\n"));
  initAtoms();
  DEBUG(1, Sdprintf("Features ...\n"));
  initPrologFlags();
  DEBUG(1, Sdprintf("Functors ...\n"));
  initFunctors();
  DEBUG(1, Sdprintf("Modules ...\n"));
  initModules();
					/* initModules may be called before */
					/* LD is present in the MT version */
  LD->modules.typein = MODULE_user;
  LD->modules.source = MODULE_user;
  DEBUG(1, Sdprintf("Records ...\n"));
  initDBRef();
  initRecords();
  DEBUG(1, Sdprintf("Tries ...\n"));
  initTries();
  DEBUG(1, Sdprintf("Flags ...\n"));
  initFlags();
  DEBUG(1, Sdprintf("Foreign Predicates ...\n"));
  initBuildIns();
  DEBUG(1, Sdprintf("Operators ...\n"));
  initOperators();
  DEBUG(1, Sdprintf("GMP ...\n"));
  initGMP();
  DEBUG(1, Sdprintf("Arithmetic ...\n"));
  initArith();
  DEBUG(1, Sdprintf("Tracer ...\n"));
  initTracer();
  debugstatus.styleCheck = SINGLETON_CHECK;
  DEBUG(1, Sdprintf("IO ...\n"));
  initIO();
  initCharConversion();
#ifdef O_LOCALE
  initLocale();
#endif
  GD->io_initialised = TRUE;
  GD->clauses.cgc_space_factor  = 8;
  GD->clauses.cgc_stack_factor  = 0.03;
  GD->clauses.cgc_clause_factor = 1.0;

  if ( !endCritical )
    return FALSE;

  DEBUG(1, Sdprintf("Heap Initialised\n"));
  return TRUE;
}


void
initPrologLocalData(ARG1_LD)
{
#ifdef O_LIMIT_DEPTH
  depth_limit   = DEPTH_NO_LIMIT;
#endif
#ifdef O_INFERENCE_LIMIT
  LD->inference_limit.limit = INFERENCE_NO_LIMIT;
#endif

  LD->break_level = -1;
  LD->prolog_flag.write_attributes = PL_WRT_ATTVAR_IGNORE;

#ifdef O_PLMT
  simpleMutexInit(&LD->thread.scan_lock);
#endif

  updateAlerted(LD);
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			   SIGNAL HANDLING

SWI-Prolog catches a number of signals:

  - SIGINT is caught to allow the user to interrupt normal execution.
  - SIGUSR2 is caught using an empty handler to break blocking system
    calls and allow handling of Prolog signals from them.
  - SIGTERM, SIGABRT and SIGQUIT are caught to cleanup before killing
    the process again using the same signal.
  - SIGSEGV, SIGILL, SIGBUS, SIGFPE and SIGSYS are caught by
    os/pl-cstack.c to print a backtrace and exit.
  - SIGHUP is caught and causes the process to exit with status 2 after
    cleanup.

If the system is started using --nosignals, only SIGUSR2 is modified.

Note that library(time) uses SIGUSR1.

Code in SWI-Prolog should  call  PL_signal()  rather  than  signal()  to
install  signal  handlers.  SWI-Prolog assumes the handler function is a
void function.  On some systems this gives  some  compiler  warnings  as
they  define  signal handlers to be int functions.  This should be fixed
some day.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if HAVE_SIGNAL
#define HAVE_SIGNALS 1

#define PLSIG_PREPARED 0x00010000	/* signal is prepared */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define the signals and  their  properties.   This  could  be  nicer, but
different systems provide different signals, and   above all, MS systems
provide very few.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct signame
{ int	      sig;
  const char *name;
  int	      flags;
} signames[] =
{
#ifdef SIGHUP
  { SIGHUP,	"hup",    0},
#endif
  { SIGINT,	"int",    0},
#ifdef SIGQUIT
  { SIGQUIT,	"quit",   0},
#endif
  { SIGILL,	"ill",    0},
  { SIGABRT,	"abrt",   0},
  { SIGFPE,	"fpe",    PLSIG_THROW},
#ifdef SIGKILL
  { SIGKILL,	"kill",   0},
#endif
  { SIGSEGV,	"segv",   0},
#ifdef SIGPIPE
  { SIGPIPE,	"pipe",   0},
#endif
#ifdef SIGALRM
  { SIGALRM,	"alrm",   PLSIG_THROW},
#endif
  { SIGTERM,	"term",   0},
#ifdef SIGUSR1
  { SIGUSR1,	"usr1",   0},
#endif
#ifdef SIGUSR2
  { SIGUSR2,	"usr2",   0},
#endif
#ifdef SIGCHLD
  { SIGCHLD,	"chld",   0},
#endif
#ifdef SIGCONT
  { SIGCONT,	"cont",   0},
#endif
#ifdef SIGSTOP
  { SIGSTOP,	"stop",   0},
#endif
#ifdef SIGTSTP
  { SIGTSTP,	"tstp",   0},
#endif
#ifdef SIGTTIN
  { SIGTTIN,	"ttin",   0},
#endif
#ifdef SIGTTOU
  { SIGTTOU,	"ttou",   0},
#endif
#ifdef SIGTRAP
  { SIGTRAP,	"trap",   0},
#endif
#ifdef SIGBUS
  { SIGBUS,	"bus",    0},
#endif
#ifdef SIGSTKFLT
  { SIGSTKFLT,	"stkflt", 0},
#endif
#ifdef SIGURG
  { SIGURG,	"urg",    0},
#endif
#ifdef SIGIO
  { SIGIO,	"io",     0},
#endif
#ifdef SIGPOLL
  { SIGPOLL,	"poll",   0},
#endif
#ifdef SIGXCPU
  { SIGXCPU,	"xcpu",   PLSIG_THROW},
#endif
#ifdef SIGXFSZ
  { SIGXFSZ,	"xfsz",   PLSIG_THROW},
#endif
#ifdef SIGVTALRM
  { SIGVTALRM,	"vtalrm", PLSIG_THROW},
#endif
#ifdef SIGPROF
  { SIGPROF,	"prof",   0},
#endif
#ifdef SIGPWR
  { SIGPWR,	"pwr",    0},
#endif

/* The signals below here are recorded as Prolog interrupts, but
   not supported by OS signals.  They start at offset 32.
*/

  { SIG_EXCEPTION,     "prolog:exception",     0 },
#ifdef SIG_ATOM_GC
  { SIG_ATOM_GC,       "prolog:atom_gc",       0 },
#endif
  { SIG_GC,	       "prolog:gc",	       0 },
#ifdef SIG_THREAD_SIGNAL
  { SIG_THREAD_SIGNAL, "prolog:thread_signal", 0 },
#endif
  { SIG_CLAUSE_GC,     "prolog:clause_gc",     0 },
  { SIG_PLABORT,       "prolog:abort",         0 },

  { -1,		NULL,     0}
};

const char *
signal_name(int sig)
{ struct signame *sn = signames;

  for( ; sn->name; sn++ )
  { if ( sn->sig == sig )
      return sn->name;
  }

  return "unknown";
}


static int
signal_index(const char *name)
{ struct signame *sn = signames;
  char tmp[12];

  if ( strncmp(name, "SIG", 3) == 0 && strlen(name) < 12 )
  { strcpy(tmp, name+3);
    strlwr(tmp);
    name = tmp;
  }

  for( ; sn->name; sn++ )
  { if ( streq(sn->name, name) )
      return sn->sig;
  }

  return -1;
}


int
PL_get_signum_ex(term_t sig, int *n)
{ GET_LD
  char *s;
  int i = -1;

  if ( PL_get_integer(sig, &i) )
  {
  } else if ( PL_get_chars(sig, &s, CVT_ATOM) )
  { i = signal_index(s);
  } else
  { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_signal, sig);
  }

  if ( i > 0 && i < 32 )		/* where to get these? */
  { *n = i;
    return TRUE;
  }

  return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal, sig);
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog main signal handler. Any  signal   arrives  here first, after
which it is dispatched to the real handler.   The task of the handler is
to ensure it is safe to start a query.

There are a few possible problems:

	* The system is writing the body-arguments from the next clause.
	In this case it is working above `lTop'.  So we raise this to the
	maximum offset.

	* The system is performing a garbage collection.  We should block
	signals while in garbage-collection and non-blockable signals should
	raise a fatal error.

	* The system is in a `critical section'.  These are insufficiently
	flagged at the moment.

The sync-argument is TRUE  when   called  from  PL_handle_signals(), and
FALSE otherwise.  It is used to delay signals marked with PLSIG_SYNC.

If we are running in the MT environment, we may get signals from threads
not having a Prolog engine. If there is a registered handler we call it.
This  also  deals  with  Control-C  in  Windows  console  apps,  calling
interruptHandler() in pl-trace.c which in   turn re-routes the interrupt
to the main thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef LD
#define LD LOCAL_LD

void
dispatch_signal(int sig, int sync)
{ GET_LD
  SigHandler sh = &GD->signals.handlers[sig-1];
  fid_t fid;
  term_t lTopSave;
  int saved_current_signal;
  int saved_sync;

#ifdef O_PLMT
  if ( !LD )
  { if ( sh->handler )
      (*sh->handler)(sig);

    return;				/* what else?? */
  }

  DEBUG(MSG_SIGNAL,
	Sdprintf("Got signal %d in thread %d (=%d) %s\n",
		 sig, LD->thread.info->pl_tid,
		 pthread_self(),
		 sync ? " (sync)" : " (async)"));
#else
  DEBUG(MSG_SIGNAL,
	Sdprintf("Got signal %d %s\n",
		 sig, sync ? " (sync)" : " (async)"));
#endif

  if ( true(sh, PLSIG_NOFRAME) && sh->handler )
  { (*sh->handler)(sig);
    return;
  }

  lTopSave = consTermRef(lTop);
  saved_current_signal = LD->signal.current;
  saved_sync = LD->signal.is_sync;

  switch(sig)
  { case SIGFPE:
    case SIGSEGV:
#if defined(SIGBUS) && SIGBUS != SIGSEGV
    case SIGBUS:
#endif
      if ( sig == LD->signal.current )
	sysError("Recursively received fatal signal %d", sig);
  }

  if ( gc_status.active && sig < SIG_PROLOG_OFFSET )
  { fatalError("Received signal %d (%s) while in %ld-th garbage collection",
	       sig, signal_name(sig), gc_status.collections);
  }

  if ( LD->critical || (true(sh, PLSIG_SYNC) && !sync) )
  { PL_raise(sig);			/* wait for better times! */
    return;
  }

  if ( !(fid = PL_open_signal_foreign_frame(sync)) )
  { PL_raise(sig);			/* no space; wait */
    return;
  }

  if ( !sync )
    blockGC(0 PASS_LD);
  LD->signal.current = sig;
  LD->signal.is_sync = sync;

  DEBUG(MSG_SIGNAL,
	Sdprintf("Handling signal %d, pred = %p, handler = %p\n",
		 sig, sh->predicate, sh->handler));

  if ( sh->predicate )
  { term_t sigterm = PL_new_term_ref();
    qid_t qid;
#ifdef O_LIMIT_DEPTH
    uintptr_t olimit = depth_limit;
    depth_limit = DEPTH_NO_LIMIT;
#endif

    PL_put_atom_chars(sigterm, signal_name(sig));
    qid = PL_open_query(NULL,
			PL_Q_PASS_EXCEPTION,
			sh->predicate,
			sigterm);
    if ( PL_next_solution(qid) ) {};		/* cannot ignore return */
    PL_cut_query(qid);
#ifdef O_LIMIT_DEPTH
    depth_limit = olimit;
#endif
  } else if ( true(sh, PLSIG_THROW) )
  { char *predname;
    int  arity;

    if ( environment_frame )
    { predname = stringAtom(environment_frame->predicate->functor->name);
      arity    = environment_frame->predicate->functor->arity;
    } else
    { predname = NULL;
      arity    = 0;
    }

    PL_error(predname, arity, NULL, ERR_SIGNALLED, sig, signal_name(sig));
  } else if ( sh->handler )
  {
#ifdef O_LIMIT_DEPTH
    uintptr_t olimit = depth_limit;
    depth_limit = DEPTH_NO_LIMIT;
#endif
    (*sh->handler)(sig);
#ifdef O_LIMIT_DEPTH
    depth_limit = olimit;
#endif

    DEBUG(MSG_SIGNAL,
	  Sdprintf("Handler %p finished (pending=0x%x,0x%x)\n",
		   sh->handler, LD->signal.pending[0], LD->signal.pending[1]));

    if ( exception_term && !sync )	/* handler: PL_raise_exception() */
    { LD->signal.exception = PL_record(exception_term);
      PL_raise(SIG_EXCEPTION);
      exception_term = 0;
    }
  }

  LD->signal.current = saved_current_signal;
  LD->signal.is_sync = saved_sync;
  if ( sync || exception_term )
    PL_close_foreign_frame(fid);
  else
    PL_discard_foreign_frame(fid);
  lTop = (LocalFrame)valTermRef(lTopSave);

  if ( !sync )
    unblockGC(0 PASS_LD);
}


static void
pl_signal_handler(int sig)
{ dispatch_signal(sig, FALSE);
}

#ifndef SA_RESTART
#define SA_RESTART 0
#endif

handler_t
set_sighandler(int sig, handler_t func)
{
#ifdef HAVE_SIGACTION
  struct sigaction old;
  struct sigaction new;

  memset(&new, 0, sizeof(new));	/* deal with other fields */
  new.sa_handler = func;
/*new.sa_flags   = SA_RESTART;  all blocking functions are restarted */

  if ( sigaction(sig, &new, &old) == 0 )
    return old.sa_handler;
  else
    return SIG_DFL;
#else
#ifdef __WINDOWS__
  switch( sig )				/* Current Windows versions crash */
  { case SIGABRT:			/* when given a non-supported value */
    case SIGFPE:
    case SIGILL:
    case SIGINT:
    case SIGSEGV:
    case SIGTERM:
      break;
    default:
      return SIG_IGN;
  }
#endif
  return signal(sig, func);
#endif
}

#ifdef HAVE_SIGINFO_H
#include <siginfo.h>
#endif


static SigHandler
prepareSignal(int sig)
{ SigHandler sh = &GD->signals.handlers[sig-1];

  if ( false(sh, PLSIG_PREPARED) )
  { set(sh, PLSIG_PREPARED);
    if ( sig < SIG_PROLOG_OFFSET )
      sh->saved_handler = set_sighandler(sig, pl_signal_handler);
  }

  return sh;
}


static void
unprepareSignal(int sig)
{ SigHandler sh = &GD->signals.handlers[sig-1];

  if ( true(sh, PLSIG_PREPARED) )
  { if ( sig < SIG_PROLOG_OFFSET )
      set_sighandler(sig, sh->saved_handler);
    sh->flags         = 0;
    sh->handler       = NULL;
    sh->predicate     = NULL;
    sh->saved_handler = NULL;
  }
}


#ifdef SIGHUP
static void
hupHandler(int sig)
{ (void)sig;

  PL_halt(2);
}
#endif


/* terminate_handler() is called on termination signals like SIGTERM.
   It runs hooks registered using PL_exit_hook() and then kills itself.
   The hooks are called with the exit status `3`.
*/

static void
terminate_handler(int sig)
{ signal(sig, SIG_DFL);

  run_on_halt(&GD->os.exit_hooks, 3);

#if defined(HAVE_KILL) && defined(HAVE_GETPID)
  kill(getpid(), sig);
#else
  exit(3);
#endif
}

static void
initTerminationSignals(void)
{
#ifdef SIGTERM
  PL_signal(SIGTERM, terminate_handler);
#endif
#ifdef SIGABRT
  PL_signal(SIGABRT, terminate_handler);
#endif
#ifdef SIGQUIT
  PL_signal(SIGQUIT, terminate_handler);
#endif
}

static void
sig_exception_handler(int sig)
{ GET_LD
  (void)sig;

  if ( HAS_LD && LD->signal.exception )
  { record_t ex = LD->signal.exception;

    LD->signal.exception = 0;

    PL_put_variable(exception_bin);
    PL_recorded(ex, exception_bin);
    PL_erase(ex);
    exception_term = exception_bin;

    DEBUG(CHK_SECURE, checkData(valTermRef(exception_term)));
  }
}


static void
agc_handler(int sig)
{ GET_LD
  (void)sig;

  if ( GD->statistics.atoms >= GD->atoms.non_garbage + GD->atoms.margin &&
       !gc_status.blocked )
    pl_garbage_collect_atoms();
}


static void
gc_handler(int sig)
{ (void)sig;

  garbageCollect();
}


static void
cgc_handler(int sig)
{ (void)sig;

  pl_garbage_collect_clauses();
}


static void
abort_handler(int sig)
{ (void)sig;

  abortProlog();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The idea behind alert_handler() is to  make blocking system calls return
with EINTR and thus make them interruptable for thread-signals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef SIG_ALERT
static void
alert_handler(int sig)
{ SigHandler sh = &GD->signals.handlers[sig-1];

  if ( sh->saved_handler &&
       sh->saved_handler != SIG_IGN &&
       sh->saved_handler != SIG_DFL )
    (*sh->saved_handler)(sig);
}
#endif


static void
initSignals(void)
{ GET_LD

  /* This is general signal handling that is not strictly needed */
  if ( truePrologFlag(PLFLAG_SIGNALS) )
  { struct signame *sn = signames;
#ifdef SIGPIPE
    set_sighandler(SIGPIPE, SIG_IGN);
#endif
    initTerminationSignals();
    initBackTrace();
    for( ; sn->name; sn++)
    {
#ifdef HAVE_BOEHM_GC
      if ( sn->sig == GC_get_suspend_signal() ||
	   sn->sig == GC_get_thr_restart_signal() )
	sn->flags = 0;
#endif
      if ( sn->flags )
      { SigHandler sh = prepareSignal(sn->sig);
	sh->flags |= sn->flags;
      }
    }

#ifdef SIGHUP
    PL_signal(SIGHUP|PL_SIGSYNC, hupHandler);
#endif
  }

  /* We do need alerting to make thread signals work while the */
  /* system is blocked in a system call. Can be controlled with --sigalert=N */

#ifdef SIG_ALERT
  if ( GD->signals.sig_alert )
    PL_signal(GD->signals.sig_alert|PL_SIGNOFRAME, alert_handler);
#endif

  /* these signals are not related to Unix signals and can thus */
  /* be enabled always */

  PL_signal(SIG_EXCEPTION|PL_SIGSYNC,     sig_exception_handler);
  PL_signal(SIG_GC|PL_SIGSYNC,	          gc_handler);
  PL_signal(SIG_CLAUSE_GC|PL_SIGSYNC,     cgc_handler);
  PL_signal(SIG_PLABORT|PL_SIGSYNC,       abort_handler);
#ifdef SIG_THREAD_SIGNAL
  PL_signal(SIG_THREAD_SIGNAL|PL_SIGSYNC, executeThreadSignals);
#endif
#ifdef SIG_ATOM_GC
  PL_signal(SIG_ATOM_GC|PL_SIGSYNC,       agc_handler);
#endif
}


void
cleanupSignals(void)
{ struct signame *sn = signames;

  for( ; sn->name; sn++)
    unprepareSignal(sn->sig);
}


void
resetSignals(void)
{ GET_LD

  LD->signal.current = 0;
  LD->signal.pending[0] = 0;
  LD->signal.pending[1] = 0;
}

#if defined(O_PLMT) && defined(HAVE_PTHREAD_SIGMASK)
#ifndef HAVE_SIGPROCMASK
#define HAVE_SIGPROCMASK 1
#endif

#define sigprocmask(how, new, old) pthread_sigmask(how, new, old)
#endif

#ifdef HAVE_SIGPROCMASK

void
allSignalMask(sigset_t *set)
{ static sigset_t allmask;
  static int done = FALSE;

  if ( !done )
  { sigset_t tmp;

    sigfillset(&tmp);
    sigdelset(&tmp, SIGSTOP);
    sigdelset(&tmp, SIGCONT);
    sigdelset(&tmp, SIGQUIT);
    sigdelset(&tmp, SIGSEGV);
    sigdelset(&tmp, SIGBUS);
#ifdef O_PROFILE
    sigdelset(&tmp, SIGPROF);
#endif
    allmask = tmp;
    done = TRUE;
  }

  *set = allmask;
}


#if 0
static void
listBlocked()
{ sigset_t current;
  int i;

  sigprocmask(SIG_BLOCK, NULL, &current);

  Sdprintf("Blocked: ");
  for(i=1; i<32; i++)
  { if ( sigismember(&current, i) )
      Sdprintf(" %d", i);
  }
  Sdprintf("\n");
  Sdprintf("UnBlocked: ");
  for(i=1; i<32; i++)
  { if ( !sigismember(&current, i) )
      Sdprintf(" %d", i);
  }
  Sdprintf("\n\n");
}
#endif

void
blockSignals(sigset_t *old)
{ sigset_t set;

  allSignalMask(&set);

  sigprocmask(SIG_BLOCK, &set, old);
  DEBUG(1, Sdprintf("Blocked all signals\n"));
}


void
unblockSignals(sigset_t *old)
{ if ( old )
  { sigprocmask(SIG_SETMASK, old, NULL);
    DEBUG(1, Sdprintf("Restored signal mask\n"));
  } else
  { sigset_t set;

    allSignalMask(&set);

    sigprocmask(SIG_UNBLOCK, &set, NULL);
    DEBUG(1, Sdprintf("UnBlocked all signals\n"));
  }
}


void
unblockSignal(int sig)
{ sigset_t set;

  sigemptyset(&set);
  sigaddset(&set, sig);

  sigprocmask(SIG_UNBLOCK, &set, NULL);
  DEBUG(1, Sdprintf("Unblocked signal %d\n", sig));
}

void
blockSignal(int sig)
{ sigset_t set;

  sigemptyset(&set);
  sigaddset(&set, sig);

  sigprocmask(SIG_BLOCK, &set, NULL);
  DEBUG(1, Sdprintf("signal %d\n", sig));
}

#else /*HAVE_SIGPROCMASK*/

void blockSignals(sigset_t *old) {}
void unblockSignals(sigset_t *old) {}
void unblockSignal(int sig) {}
void blockSignal(int sig) {}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BUG: The interface of PL_signal() is broken   as  it does not return the
current flags associated with the signal and therefore we cannot restore
the signal safely. We should  design  a   struct  based  API  similar to
sigaction().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_sigaction(int sig, pl_sigaction_t *act, pl_sigaction_t *old)
{ SigHandler sh = NULL;

  if ( sig < 0 || sig > MAXSIGNAL )
  { errno = EINVAL;
    return -1;
  }

  if ( sig == 0 )
  { for(sig=SIG_PROLOG_OFFSET; sig<MAXSIGNAL; sig++)
    { sh = &GD->signals.handlers[sig-1];
      if ( sh->flags == 0 )
	break;
    }
    if ( !sh )
    { errno = EBUSY;
      return -2;
    }
  } else
  { sh = &GD->signals.handlers[sig-1];
  }

  if ( old )
  { memset(old, 0, sizeof(*old));
    old->sa_cfunction = sh->handler;
    old->sa_predicate = sh->predicate;
    old->sa_flags     = sh->flags;
  }

  if ( act && act != old )
  { int active = FALSE;

    if ( (act->sa_flags&PLSIG_THROW) || act->sa_predicate )
    { if ( ((act->sa_flags&PLSIG_THROW) && act->sa_predicate) ||
	   act->sa_cfunction )
      { errno = EINVAL;
	return -1;
      }
      active = TRUE;
    } else if ( act->sa_cfunction &&
		(false(sh, PLSIG_PREPARED)||act->sa_cfunction!=sh->saved_handler) )
    { active = TRUE;
    }

    if ( active )
    { sh->handler   = act->sa_cfunction;
      sh->predicate = act->sa_predicate;
      sh->flags     = (sh->flags&~0xffff)|act->sa_flags;
      if ( false(sh, PLSIG_PREPARED) )
	prepareSignal(sig);
    } else
    { unprepareSignal(sig);
      sh->handler   = NULL;
      sh->predicate = NULL;
      sh->flags     = 0;
    }
  }

  return sig;
}


handler_t
PL_signal(int sigandflags, handler_t func)
{
#ifdef HAVE_SIGNALS
  pl_sigaction_t act = {0};
  pl_sigaction_t old;

  act.sa_cfunction = func;
  if ( (sigandflags&PL_SIGSYNC) )
    act.sa_flags |= PLSIG_SYNC;
  if ( (sigandflags&PL_SIGNOFRAME) )
    act.sa_flags |= PLSIG_NOFRAME;

  if ( PL_sigaction((sigandflags & 0xffff), &act, &old) >= 0 )
  { if ( (old.sa_flags&PLSIG_PREPARED) && old.sa_cfunction )
      return old.sa_cfunction;

    return SIG_DFL;
  }

  return NULL;
#else
  return SIG_DFL;
#endif
}


/* return: -1: exception in handler, otherwise number of handled signals
*/

int
PL_handle_signals(void)
{ GET_LD

  if ( !HAS_LD || LD->critical || !is_signalled(PASS_LD1) )
    return 0;
  if ( exception_term )
    return -1;

  return handleSignals(PASS_LD1);
}


int
handleSignals(ARG1_LD)
{ int done = 0;
  int i;

  if ( !HAS_LD || LD->critical )
    return 0;

  for(i=0; i<2; i++)
  { while( LD->signal.pending[i] )
    { int sig = 1+32*i;
      int mask = 1;

      for( ; mask ; mask <<= 1, sig++ )
      { if ( LD->signal.pending[i] & mask )
	{ __sync_and_and_fetch(&LD->signal.pending[i], ~mask);

	  done++;
	  dispatch_signal(sig, TRUE);

	  if ( exception_term )
	    return -1;
	}
      }
    }
  }

  if ( done )
    updateAlerted(LD);

  return done;
}


int
endCritical__LD(ARG1_LD)
{ if ( exception_term )
    return FALSE;

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
on_signal(?SigNum, ?SigName, :OldHandler, :NewHandler)

Assign NewHandler to be called if signal arrives.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_meta_arg(term_t arg, term_t m, term_t t)
{ GET_LD

  if ( PL_is_functor(arg, FUNCTOR_colon2) )
  { _PL_get_arg(1, arg, m);
    _PL_get_arg(2, arg, t);
    return TRUE;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE,
		  ATOM_meta_argument, arg);
}


static int
get_module(term_t t, Module *m)
{ GET_LD
  atom_t a;

  if ( !PL_get_atom_ex(t, &a) )
    return FALSE;
  *m = PL_new_module(a);

  return TRUE;
}


static
PRED_IMPL("$on_signal", 4, on_signal, 0)
{ PRED_LD
  int sign = -1;
  SigHandler sh;
  char *sn;
  atom_t a;
  term_t mold = PL_new_term_ref();
  term_t mnew = PL_new_term_ref();

  term_t sig  = A1;
  term_t name = A2;
  term_t old  = A3;
  term_t new  = A4;

  if ( !get_meta_arg(old, mold, old) ||
       !get_meta_arg(new, mnew, new) )
    return FALSE;

  if ( PL_get_integer(sig, &sign) && sign >= 1 && sign <= MAXSIGNAL )
  { TRY(PL_unify_atom_chars(name, signal_name(sign)));
  } else if ( PL_get_atom_chars(name, &sn) )
  { if ( (sign = signal_index(sn)) != -1 )
    { TRY(PL_unify_integer(sig, sign));
    } else
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal, name);
  } else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_signal, sig);

  sh = &GD->signals.handlers[sign-1];

  if ( false(sh, PLSIG_PREPARED) )		/* not handled */
  { TRY(PL_unify_atom(old, ATOM_default));
  } else if ( true(sh, PLSIG_THROW) )		/* throw exception */
  { TRY(PL_unify_atom(old, ATOM_throw));
  } else if ( sh->predicate )			/* call predicate */
  { Definition def = sh->predicate->definition;

    if ( !PL_unify_atom(mold, def->module->name) ||
	 !PL_unify_atom(old, def->functor->name) )
      return FALSE;
  } else if ( sh->handler )
  { TRY(PL_unify_term(old,
		      PL_FUNCTOR, FUNCTOR_foreign_function1,
		      PL_POINTER, sh->handler));
  }

  if ( PL_compare(old, new) == 0 &&
       PL_compare(mold, mnew) == 0 )
    succeed;					/* no change */

  if ( PL_get_atom(new, &a) )
  { if ( a == ATOM_default )
    { unprepareSignal(sign);
    } else if ( a == ATOM_throw )
    { sh = prepareSignal(sign);
      set(sh, PLSIG_THROW);
      clear(sh, PLSIG_SYNC);
      sh->handler   = NULL;
      sh->predicate = NULL;
    } else
    { Module m;
      predicate_t pred;

      if ( !get_module(mnew, &m) )
	return FALSE;
      pred = lookupProcedure(PL_new_functor(a, 1), m);

      sh = prepareSignal(sign);
      clear(sh, PLSIG_THROW);
      set(sh, PLSIG_SYNC);
      sh->handler = NULL;
      sh->predicate = pred;
    }
  } else if ( PL_is_functor(new, FUNCTOR_foreign_function1) )
  { term_t a = PL_new_term_ref();
    void *f;

    _PL_get_arg(1, new, a);

    if ( PL_get_pointer(a, &f) )
    { sh = prepareSignal(sign);
      clear(sh, PLSIG_THROW|PLSIG_SYNC);
      sh->handler = (handler_t)f;
      sh->predicate = NULL;

      succeed;
    }

    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal_handler, sig);
  } else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_signal_handler, sig);

  succeed;
}

#endif /*HAVE_SIGNAL*/


		 /*******************************
		 *	       STACKS		*
		 *******************************/

static void
enforce_limit(size_t *size, size_t maxarea, const char *name)
{ if ( *size == 0 )
  { *size = maxarea;
  } else if ( *size > (size_t)(MAXTAGGEDPTR+1) )
  { if ( *size != (size_t)-1 )		/* user demanded maximum */
      Sdprintf("WARNING: Maximum stack size for %s stack is %lld MB\n",
	       name, (int64_t)((MAXTAGGEDPTR+1) / (1 MB)));
    *size = MAXTAGGEDPTR+1;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
initPrologStacks() creates the stacks for the calling thread. It is used
both at system startup to create the stack   for the main thread as from
pl-thread.c to create stacks for Prolog threads.

allocStacks() does the  real  work   and  has  several  implementations,
depending on the OS features.

Requested stack sizes are in bytes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
initPrologStacks(size_t local, size_t global, size_t trail)
{ GET_LD
  size_t maxarea;

  maxarea = MAXTAGGEDPTR+1;		/* MAXTAGGEDPTR = 0x..fff.. */
  if ( maxarea > 1024 MB )		/* 64-bit machines */
    maxarea = 1024 MB;

  enforce_limit(&local,	   maxarea,  "local");
  enforce_limit(&global,   maxarea,  "global");
  enforce_limit(&trail,	   maxarea,  "trail");

  if ( !allocStacks(local, global, trail) )
    fail;

  LD->stacks.local.overflow_id    = LOCAL_OVERFLOW;
  LD->stacks.global.overflow_id   = GLOBAL_OVERFLOW;
  LD->stacks.trail.overflow_id    = TRAIL_OVERFLOW;
  LD->stacks.argument.overflow_id = ARGUMENT_OVERFLOW;

  base_addresses[STG_LOCAL]  = (uintptr_t)lBase;
  base_addresses[STG_GLOBAL] = (uintptr_t)gBase;
  base_addresses[STG_TRAIL]  = (uintptr_t)tBase;
  *gBase++ = MARK_MASK;			/* see sweep_global_mark() */
  gMax--;				/*  */
  tMax--;
  emptyStacks();

  DEBUG(1, Sdprintf("base_addresses[STG_LOCAL] = %p\n",
		    base_addresses[STG_LOCAL]));
  DEBUG(1, Sdprintf("base_addresses[STG_GLOBAL] = %p\n",
		    base_addresses[STG_GLOBAL]));
  DEBUG(1, Sdprintf("base_addresses[STG_TRAIL] = %p\n",
		    base_addresses[STG_TRAIL]));

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create nice empty stacks. exception_bin   and  exception_printed are two
term-references that must be low on  the   stack  to  ensure they remain
valid while the stack is unrolled after an exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
emptyStack(Stack s)
{ s->top       = s->base;
  s->gced_size = 0L;
}


void
emptyStacks(void)
{ GET_LD

  environment_frame = NULL;
  fli_context       = NULL;
  LD->query         = NULL;

  emptyStack((Stack)&LD->stacks.local);
  emptyStack((Stack)&LD->stacks.global);
  emptyStack((Stack)&LD->stacks.trail);
  emptyStack((Stack)&LD->stacks.argument);

  LD->mark_bar          = gTop;
  if ( lTop && gTop )
  { int i;

    PL_open_foreign_frame();
    exception_term          = 0;
    exception_bin           = PL_new_term_ref();
    exception_printed       = PL_new_term_ref();
    LD->exception.tmp       = PL_new_term_ref();
    LD->exception.pending   = PL_new_term_ref();
    LD->trim.dummy          = PL_new_term_ref();
#ifdef O_ATTVAR
    LD->attvar.head	    = PL_new_term_ref();
    LD->attvar.tail         = PL_new_term_ref();
    LD->attvar.gc_attvars   = PL_new_term_ref();
    DEBUG(3, Sdprintf("attvar.tail at %p\n", valTermRef(LD->attvar.tail)));
#endif
#ifdef O_GVAR
    destroyGlobalVars();
#endif
    for(i=0; i<TMP_PTR_SIZE; i++)
      LD->tmp.h[i] = PL_new_term_ref();
    LD->tmp.top = 0;
  }
}


		/********************************
		*	STACK ALLOCATION        *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
init_stack() initializes the stack straucture. Params:

  - name is the name of the stack (for diagnostic purposes)
  - size is the allocated size
  - limit is the maximum to which the stack is allowed to grow
  - spare is the amount of spare stack we reserve
  - gc indicates whether gc can collect data on the stack
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
init_stack(Stack s, char *name, size_t size, size_t limit, size_t spare, int gc)
{ s->name	= name;
  s->top	= s->base;
  s->size_limit	= limit;
  s->spare      = spare;
  s->def_spare  = spare;
  s->min_free   = 256*sizeof(word);
  s->max	= addPointer(s->base, size - spare);
  s->gced_size  = 0L;			/* size after last gc */
  s->gc	        = gc;
  gcPolicy(s, GC_FAST_POLICY);
}


static int
allocStacks(size_t local, size_t global, size_t trail)
{ GET_LD
  size_t minglobal   = 8*SIZEOF_VOIDP K;
  size_t minlocal    = 4*SIZEOF_VOIDP K;
  size_t mintrail    = 4*SIZEOF_VOIDP K;
  size_t minargument = 1*SIZEOF_VOIDP K;
  size_t argument    = 1 K K;		/* not really used */

  size_t itrail  = nextStackSizeAbove(mintrail-1);
  size_t iglobal = nextStackSizeAbove(minglobal-1);
  size_t ilocal  = nextStackSizeAbove(minlocal-1);

  local    = max(local,    minlocal);
  global   = max(global,   minglobal);
  trail    = max(trail,    mintrail);

  gBase = NULL;
  tBase = NULL;
  aBase = NULL;

  gBase = (Word)       stack_malloc(iglobal + ilocal);
  tBase = (TrailEntry) stack_malloc(itrail);
  aBase = (Word *)     stack_malloc(minargument);

  if ( !gBase || !tBase || !aBase )
  { if ( gBase )
      *gBase++ = MARK_MASK;		/* compensate for freeStacks */
    freeStacks(PASS_LD1);
    fail;
  }

  lBase = (LocalFrame) addPointer(gBase, iglobal);

  init_stack((Stack)&LD->stacks.global,
	     "global",   iglobal, global, 512*SIZEOF_VOIDP, TRUE);
  init_stack((Stack)&LD->stacks.local,
	     "local",    ilocal,  local,  512*SIZEOF_VOIDP, FALSE);
  init_stack((Stack)&LD->stacks.trail,
	     "trail",    itrail,  trail,  256*SIZEOF_VOIDP, TRUE);
  init_stack((Stack)&LD->stacks.argument,
	     "argument", minargument, argument, 0, FALSE);

  LD->stacks.local.min_free = LOCAL_MARGIN;

  succeed;
}


void
freeStacks(ARG1_LD)
{ if ( gBase )
  { gBase--;
    stack_free(gBase);
    gTop = NULL; gBase = NULL;
    lTop = NULL; lBase = NULL;
  }
  if ( tBase )
  { stack_free(tBase);
    tTop = NULL;
    tBase = NULL;
  }
  if ( aBase )
  { stack_free(aBase);
    aTop = NULL;
    aBase = NULL;
  }
}


void *
stack_malloc(size_t size)
{ void *mem = malloc(size+sizeof(size_t));

  if ( mem )
  { size_t *sp = mem;
    *sp++ = size;
#ifdef SECURE_GC
    memset(sp, 0xFB, size);
#endif
    ATOMIC_ADD(&GD->statistics.stack_space, size);

    return sp;
  }

  return NULL;
}

void *
stack_realloc(void *old, size_t size)
{ size_t *sp = old;
  size_t osize = *--sp;
  void *mem;

#ifdef SECURE_GC
  if ( (mem = stack_malloc(size)) )
  { memcpy(mem, old, (size>osize?osize:size));
    stack_free(old);
    return mem;
  }
#else
  if ( (mem = realloc(sp, size+sizeof(size_t))) )
  { sp = mem;
    *sp++ = size;
    if ( size > osize )
      ATOMIC_ADD(&GD->statistics.stack_space, size-osize);
    else
      ATOMIC_SUB(&GD->statistics.stack_space, osize-size);
    return sp;
  }
#endif

  return NULL;
}

void
stack_free(void *mem)
{ size_t *sp = mem;
  size_t osize = *--sp;

  ATOMIC_SUB(&GD->statistics.stack_space, osize);

#ifdef SECURE_GC
  memset(sp, 0xFB, osize+sizeof(size_t));
#endif
  free(sp);
}


int
trim_stack(Stack s)
{ if ( s->spare < s->def_spare )
  { ssize_t reduce = s->def_spare - s->spare;
    ssize_t room = roomStackP(s);

    if ( room > 0 && room < reduce )
    { DEBUG(MSG_SPARE_STACK,
	    Sdprintf("Only %d spare for %s-stack\n", room, s->name));
      reduce = room;
    }

    s->max = addPointer(s->max, -reduce);
    s->spare += reduce;
  }

  return FALSE;
}


		/********************************
		*     STACK TRIMMING & LIMITS   *
		*********************************/

static void
gcPolicy(Stack s, int policy)
{ GET_LD

  s->gc = ((s == (Stack) &LD->stacks.global ||
	    s == (Stack) &LD->stacks.trail) ? TRUE : FALSE);
  if ( s->gc )
  { s->small  = SMALLSTACK;
    s->factor = 3;
    s->policy = policy;
  } else
  { s->small  = 0;
    s->factor = 0;
    s->policy = 0;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
trimStacks() reclaims all unused space on the stack. Note that the trail
can have references to unused stack. We set the references to point to a
dummy variable, so no harm  will  be   done.  Setting  it  to NULL would
require a test in Undo(), which   is time-critical. trim_stacks normally
isn't. This precaution is explicitly required  for the trimStacks() that
result from a stack-overflow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
trimStacks(int resize ARG_LD)
{ int scantrail;

  LD->trim_stack_requested = FALSE;

  if ( resize )
  { LocalFrame olb = lBase;
    LocalFrame olm = lMax;
    Word ogb = gBase;
    Word ogm = gMax;

    growStacks(GROW_TRIM, GROW_TRIM, GROW_TRIM);

    if ( olb != lBase || olm != lMax || ogb != gBase || ogm != gMax )
      scantrail = TRUE;
    else
      scantrail = FALSE;
  } else
  { trim_stack((Stack) &LD->stacks.local);
    trim_stack((Stack) &LD->stacks.global);
    trim_stack((Stack) &LD->stacks.trail);
    trim_stack((Stack) &LD->stacks.argument);

    scantrail = FALSE;
  }

#ifdef SECURE_GC
  { Word p;				/* clear the stacks */

    for(p=gTop; p<gMax; p++)
      *p = 0xbfbfbfbf;
    for(p=(Word)lTop; p<(Word)lMax; p++)
      *p = 0xbfbfbfbf;
  }
#endif

  if ( scantrail )
  { TrailEntry te;

    for(te = tTop; --te >= tBase; )
    { Word p = te->address;

      if ( isTrailVal(p) )
	continue;

      if ( !onStack(local, p) && !onStack(global, p) )
      { te->address = valTermRef(LD->trim.dummy);
      }
    }
  }

  DEBUG(CHK_SECURE,
	{ scan_global(FALSE);
	  checkStacks(NULL);
	});
}


static
PRED_IMPL("trim_stacks", 0, trim_stacks, 0)
{ PRED_LD

  trimStacks(TRUE PASS_LD);

  succeed;
}


		 /*******************************
		 *	    LOCAL DATA		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
In the end, this should do nice cleanup  of all local data and be called
both by PL_cleanup() and when destroying a  thread. There is still a lot
of work to do.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
freePrologLocalData(PL_local_data_t *ld)
{ int i;

  discardBuffer(&ld->fli._discardable_buffer);

  for(i=0; i<BUFFER_RING_SIZE; i++)
  { discardBuffer(&ld->fli._buffer_ring[i]);
    initBuffer(&ld->fli._buffer_ring[i]);	/* Used by debug-print */
						/* on shutdown */
  }

  freeVarDefs(ld);

#ifdef O_GVAR
  if ( ld->gvar.nb_vars )
    destroyHTable(ld->gvar.nb_vars);
#endif

  if ( ld->bags.default_bag )
  { PL_free(ld->bags.default_bag);
#if defined(O_ATOMGC) && defined(O_PLMT)
    simpleMutexDelete(&ld->bags.mutex);
#endif
  }

#ifdef O_CYCLIC
  clearSegStack(&ld->cycle.lstack);
  clearSegStack(&ld->cycle.vstack);
#endif

  freeArithLocalData(ld);
#ifdef O_PLMT
  if ( ld->prolog_flag.table )
  { PL_LOCK(L_PLFLAG);
    destroyHTable(ld->prolog_flag.table);
    PL_UNLOCK(L_PLFLAG);
  }
#endif

  if ( ld->qlf.getstr_buffer )
    free(ld->qlf.getstr_buffer);

  clearThreadTablingData(ld);
}



		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

static
PRED_IMPL("$set_prolog_stack", 4, set_prolog_stack, 0)
{ PRED_LD
  atom_t a, k;
  Stack stack = NULL;

  term_t name  = A1;
  term_t prop  = A2;
  term_t old   = A3;
  term_t value = A4;

  if ( PL_get_atom(name, &a) )
  { if ( a == ATOM_local )
      stack = (Stack) &LD->stacks.local;
    else if ( a == ATOM_global )
      stack = (Stack) &LD->stacks.global;
    else if ( a == ATOM_trail )
      stack = (Stack) &LD->stacks.trail;
    else if ( a == ATOM_argument )
      stack = (Stack) &LD->stacks.argument;
  }
  if ( !stack )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_stack, name);

  if ( PL_get_atom_ex(prop, &k) )
  { if ( k == ATOM_low )
      return (PL_unify_int64(old, stack->small) &&
	      PL_get_size_ex(value, &stack->small));
    if ( k == ATOM_factor )
      return (PL_unify_integer(old, stack->factor) &&
	      PL_get_integer_ex(value, &stack->factor));
    if ( k == ATOM_limit )
    { size_t newlimit;

      if ( PL_unify_int64(old, stack->size_limit) &&
	   PL_get_size_ex(value, &newlimit) )
      { if ( newlimit < (size_t)sizeStackP(stack)+stack->min_free )
	{ if ( stack->gc )
	  { garbageCollect();
	    trimStacks(TRUE PASS_LD);
	  }

	  if ( newlimit < (size_t)sizeStackP(stack)+stack->min_free )
	    return PL_error(NULL, 0, NULL, ERR_PERMISSION,
			    ATOM_limit, ATOM_stack, name);
	}

	newlimit += stack->spare;
	if ( newlimit > MAXTAGGEDPTR+1 )
	  newlimit = MAXTAGGEDPTR+1;

	stack->size_limit = newlimit;
	return TRUE;
      }

      return FALSE;
    }
    if ( k == ATOM_spare )
    { size_t spare = stack->def_spare/sizeof(word);

      if ( PL_unify_int64(old, spare) &&
	   PL_get_size_ex(value, &spare) )
      { stack->def_spare = spare*sizeof(word);
	trim_stack(stack);
	return TRUE;
      }
      return FALSE;
    }
    if ( k == ATOM_min_free )
    { size_t minfree = stack->min_free/sizeof(word);

      if ( PL_unify_int64(old, minfree) &&
	   PL_get_size_ex(value, &minfree) )
      { stack->min_free = minfree*sizeof(word);
	trim_stack(stack);
	return TRUE;
      }
      return FALSE;
    }

    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_stack_parameter, prop);
  }

  fail;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(setup)
  PRED_DEF("$set_prolog_stack", 4, set_prolog_stack, 0)
  PRED_DEF("$on_signal", 4, on_signal, 0)
  PRED_DEF("trim_stacks", 0, trim_stacks, 0)
EndPredDefs
