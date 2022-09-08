/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2022, University of Amsterdam
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

#define GLOBAL			/* allocate global variables here */
#include "pl-setup.h"
#include "pl-comp.h"
#include "pl-arith.h"
#include "os/pl-cstack.h"
#include "os/pl-ctype.h"
#include "os/pl-prologflag.h"
#include "pl-dbref.h"
#include "pl-trie.h"
#include "pl-tabling.h"
#include "pl-undo.h"
#include "pl-event.h"
#include "pl-fli.h"
#include "pl-funct.h"
#include "pl-modul.h"
#include "pl-rec.h"
#include "pl-flag.h"
#include "pl-ext.h"
#include "pl-op.h"
#include "pl-trace.h"
#include "pl-read.h"
#include "pl-wam.h"
#include "pl-gc.h"
#include "pl-proc.h"
#include "pl-pro.h"
#include "pl-gvar.h"
#include "pl-coverage.h"
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

#if USE_LD_MACROS
#define allocStacks(_)		LDFUNC(allocStacks, _)
#define initSignals(_)		LDFUNC(initSignals, _)
#endif

#define LDFUNC_DECLARATIONS
static int allocStacks(void);
static void initSignals(void);
static void gcPolicy(Stack s, int policy);
#undef LDFUNC_DECLARATIONS

int
setupProlog(void)
{ GET_LD
  DEBUG(1, Sdprintf("Starting Heap Initialisation\n"));

#ifdef O_LOGICAL_UPDATE
  next_generation(NULL);
#endif

  LD->critical = 0;
  LD->magic = LD_MAGIC;
  for (int i = 0; i < SIGMASK_WORDS; i++)
    LD->signal.pending[i] = 0;
  LD->statistics.start_time = WallTime();

  DEBUG(1, Sdprintf("wam_table ...\n"));
  initWamTable();
  DEBUG(1, Sdprintf("character types ...\n"));
  initCharTypes();
  DEBUG(1, Sdprintf("foreign predicates ...\n"));
  initForeign();
  DEBUG(1, Sdprintf("Prolog Signal Handling ...\n"));
  initSignals();
  DEBUG(1, Sdprintf("Stacks ...\n"));
  if ( !initPrologStacks(GD->options.stackLimit) )
    outOfCore();
  GD->combined_stack.name	 = "stack";
  GD->combined_stack.gc		 = TRUE;
  GD->combined_stack.overflow_id = STACK_OVERFLOW;

  initPrologLocalData();

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
  DEBUG(1, Sdprintf("Tabling ...\n"));
  initTabling();
  DEBUG(1, Sdprintf("Flags ...\n"));
  initFlags();
  DEBUG(1, Sdprintf("Foreign Predicates ...\n"));
  initBuildIns();
  DEBUG(1, Sdprintf("Malloc binding ...\n"));
  initMalloc();
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
  setABIVersionPrologFlag();
  GD->io_initialised = TRUE;
  GD->clauses.cgc_space_factor  = 8;
  GD->clauses.cgc_stack_factor  = 0.03;
  GD->clauses.cgc_clause_factor = 1.0;

  DEBUG(1, Sdprintf("Heap Initialised\n"));
  return TRUE;
}


void
initPrologLocalData(DECL_LD)
{
#ifdef O_LIMIT_DEPTH
  LD->depth_info.limit = DEPTH_NO_LIMIT;
#endif
#ifdef O_INFERENCE_LIMIT
  LD->inference_limit.limit = INFERENCE_NO_LIMIT;
#endif

  LD->break_level = -1;
  LD->prolog_flag.write_attributes = PL_WRT_ATTVAR_IGNORE;

#ifdef O_PLMT
  simpleMutexInit(&LD->thread.scan_lock);
  LD->transaction.gen_base = GEN_INFINITE;
#endif

#if STDC_CV_ALERT
  cnd_init(&LD->signal.alert_cv);
  mtx_init(&LD->signal.alert_mtx, mtx_plain);
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
  - SIGSEGV, SIGILL, SIGBUS and SIGSYS are caught by
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

#define PLSIG_USERFLAGS 0x0000ffff	/* range of API-visible flags */
#define PLSIG_STATEFLAGS 0xffff0000	/* range of internal flags */

#define PLSIG_PREPARED 0x00010000	/* signal is prepared */
#define PLSIG_IGNORED  0x00020000	/* signal is ignored */

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
#ifdef HAVE_OS_SIGNALS
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
  { SIGPIPE,	"pipe",   PLSIG_IGNORE},
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
#endif /*HAVE_OS_SIGNALS*/

/* The signals below here are recorded as Prolog interrupts, but
   not supported by OS signals.  They start at offset 32.
*/

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

static int
is_fatal_signal(int sig)
{ switch(sig)
  {
#ifdef SIGFPE
    case SIGFPE:
#endif
#ifdef SIGSEGV
    case SIGSEGV:
#if defined(SIGBUS) && SIGBUS != SIGSEGV
    case SIGBUS:
#endif
#endif
#ifdef SIGILL
    case SIGILL:
#endif
#ifdef SIGSYS
    case SIGSYS:
#endif
      return TRUE;
  }

  return FALSE;
}


void
dispatch_signal(int sig, int sync)
{ GET_LD
  SigHandler sh = &GD->signals.handlers[SIGNAL_INDEX(sig)];
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
	{ const pl_wchar_t *name = L"";
	  int tid = LD->thread.info->pl_tid;
	  atom_t alias;

	  if ( PL_get_thread_alias(tid, &alias) )
	    name = PL_atom_wchars(alias, NULL);
	  Sdprintf("Got signal %d in thread %d (%Ws) %s\n",
		   sig, tid, name,
		   sync ? " (sync)" : " (async)");
	});
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

  if ( is_fatal_signal(sig) && sig == LD->signal.current )
    sysError("Recursively received fatal signal %d", sig);

  if ( gc_status.active && !IS_VSIG(sig) )
  { fatalError("Received signal %d (%s) while in %ld-th garbage collection",
	       sig, signal_name(sig), LD->gc.stats.totals.collections);
  }

  if ( (LD->critical || (true(sh, PLSIG_SYNC) && !sync))
#if O_SIGNALS
       && sig != SIGINT
#endif
       && !is_fatal_signal(sig)	)
  { PL_raise(sig);			/* wait for better times! */
    return;
  }

  if ( !(fid = PL_open_signal_foreign_frame(sync)) )
  { if ( is_fatal_signal(sig) )
      sigCrashHandler(sig);		/* should not return */
    PL_raise(sig);			/* no space; wait */
    return;
  }

  if ( !sync )
    blockGC(0);
  LD->signal.current = sig;
  LD->signal.is_sync = sync;

  DEBUG(MSG_SIGNAL,
	Sdprintf("Handling signal %d, pred = %p, handler = %p\n",
		 sig, sh->predicate, sh->handler));

  if ( sh->predicate )
  { term_t sigterm = PL_new_term_ref();
    qid_t qid;
#ifdef O_LIMIT_DEPTH
    size_t olimit = LD->depth_info.limit;
    LD->depth_info.limit = DEPTH_NO_LIMIT;
#endif

    PL_put_atom_chars(sigterm, signal_name(sig));
    qid = PL_open_query(NULL,
			PL_Q_PASS_EXCEPTION,
			sh->predicate,
			sigterm);
    if ( PL_next_solution(qid) ) {};		/* cannot ignore return */
    PL_cut_query(qid);
#ifdef O_LIMIT_DEPTH
    LD->depth_info.limit = olimit;
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
  { int ex_pending = (exception_term && !sync);
#ifdef O_LIMIT_DEPTH
    uintptr_t olimit = LD->depth_info.limit;
    LD->depth_info.limit = DEPTH_NO_LIMIT;
#endif
    (*sh->handler)(sig);
#ifdef O_LIMIT_DEPTH
    LD->depth_info.limit = olimit;
#endif

    DEBUG(MSG_SIGNAL,
	  Sdprintf("Handler %p finished (pending=0x%"PRIxFAST32",0x%"PRIxFAST32")\n",
		   sh->handler, LD->signal.pending[0], SIGMASK_WORDS > 1 ? LD->signal.pending[1] : 0));

    if ( !ex_pending && exception_term && !sync )	/* handler: PL_raise_exception() */
      fatalError("Async exception handler for signal %s (%d) raised "
		 "an exception", signal_name(sig), sig);
  }

  LD->signal.current = saved_current_signal;
  LD->signal.is_sync = saved_sync;
  if ( sync )
    PL_close_foreign_frame(fid);
  else
    PL_discard_foreign_frame(fid);
  lTop = (LocalFrame)valTermRef(lTopSave);

  if ( !sync )
    unblockGC(0);

					/* we cannot return.  First try */
					/* longjmp.  If that fails, crash */
  if ( is_fatal_signal(sig) )
  { if ( exception_term )
    { PL_rethrow();
      sigCrashHandler(sig);
    }
    exit(4);
  }
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
#elif defined(HAVE_SIGNAL)
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
#endif /*__WINDOWS__*/
  return signal(sig, func);
#else
  return NULL;
#endif
}

static SigHandler
prepareSignal(int sig, int plsig_flags)
{ SigHandler sh = &GD->signals.handlers[SIGNAL_INDEX(sig)];
  int current_state = sh->flags & PLSIG_STATEFLAGS;
  int desired_state = (plsig_flags & PLSIG_IGNORE) ? PLSIG_IGNORED : PLSIG_PREPARED;

  plsig_flags &= ~(PLSIG_STATEFLAGS | PLSIG_IGNORE);

  if ( current_state != desired_state )
  { clearFlags(sh);
    set(sh, desired_state | plsig_flags);
    if ( !IS_VSIG(sig) )
    { handler_t old_handler = set_sighandler(sig, desired_state == PLSIG_IGNORED ? SIG_IGN : pl_signal_handler);
      if ( current_state == 0 )
        sh->saved_handler = old_handler;
    }
  } else
  { sh->flags = (sh->flags & ~PLSIG_USERFLAGS) | plsig_flags;
  }

  return sh;
}


static void
unprepareSignal(int sig)
{ SigHandler sh = &GD->signals.handlers[SIGNAL_INDEX(sig)];

  if ( true(sh, PLSIG_STATEFLAGS) )
  { if ( !IS_VSIG(sig) )
      set_sighandler(sig, sh->saved_handler);
    sh->flags         = 0;
    sh->handler       = NULL;
    sh->predicate     = NULL;
    sh->saved_handler = NULL;
  }
}


#if O_SIGNALS

#ifdef SIGHUP
static void
hupHandler(int sig)
{ (void)sig;

  PL_halt(128+sig);
}
#endif


/* terminate_handler() is called on termination signals like SIGTERM.
   It runs hooks registered using PL_exit_hook() and then kills itself.
   The hooks are called with the exit status `3`.
*/

static void
terminate_handler(int sig)
{ signal(sig, SIG_DFL);

  run_on_halt(&GD->os.exit_hooks, 128+sig);

#if defined(HAVE_KILL) && defined(HAVE_GETPID)
  kill(getpid(), sig);
#else
  switch( sig )
  {
#ifdef SIGTERM
    case SIGTERM:
      exit(128+SIGTERM);
#endif
#ifdef SIGQUIT
    case SIGQUIT:
      exit(128+SIGQUIT);
#endif
#ifdef SIGABRT
    case SIGABRT:
      abort();
#endif
    default:
      assert(0); /* not reached */
  }
#endif
}

void
terminate_on_signal(int signo)
{ PL_signal(signo, terminate_handler);
}

static void
initTerminationSignals(void)
{
#ifdef SIGTERM
  terminate_on_signal(SIGTERM);
#endif
#ifdef SIGABRT
  terminate_on_signal(SIGABRT);
#endif
#ifdef SIGQUIT
  terminate_on_signal(SIGQUIT);
#endif
}
#endif /*O_SIGNALS*/

#ifdef O_C_STACK_GUARDED
static void
alt_segv_handler(int sig)
{ GET_LD
  (void)sig;

  DEBUG(MSG_SIGNAL,
	Sdprintf("Got C-stack overflow; critical = %d\n",
		 LD->signal.sig_critical));

  if ( LD->signal.sig_critical )
  { longjmp(LD->signal.context, TRUE);
    /*NORETURN*/
  }

  sigCrashHandler(sig);
}
#endif

int
initGuardCStack(void)
{
#ifdef O_C_STACK_GUARDED
  GET_LD
  stack_t ss = {0};

  if ( (LD->signal.alt_stack = malloc(SIGSTKSZ)) )
  { ss.ss_sp = LD->signal.alt_stack;
    ss.ss_size = SIGSTKSZ;
    ss.ss_flags = 0;

    if ( sigaltstack(&ss, NULL) == 0)
    { DEBUG(MSG_SIGNAL, Sdprintf("Setup altstack (%zd bytes)\n", SIGSTKSZ));

      if ( LD == &PL_local_data )	/* main thread, only need this once */
      { struct sigaction sa = {0};

	sa.sa_flags = SA_ONSTACK;
	sa.sa_handler = alt_segv_handler;
	sigemptyset(&sa.sa_mask);

	if ( sigaction(SIGSEGV, &sa, NULL) == 0 )
	{ DEBUG(MSG_SIGNAL, Sdprintf("Setup SEGV on altstack\n"));
	  return TRUE;
	}
      }
    }
  }
#endif

  return FALSE;
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

  garbageCollect(0);
}

static void
gc_tune_handler(int sig)
{ (void)sig;

  call_tune_gc_hook();
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
{ SigHandler sh = &GD->signals.handlers[SIGNAL_INDEX(sig)];

  DEBUG(MSG_THREAD_SIGNAL, Sdprintf("[%d]: received alert\n", PL_thread_self()));

  if ( sh->saved_handler &&
       sh->saved_handler != SIG_IGN &&
       sh->saved_handler != SIG_DFL )
    (*sh->saved_handler)(sig);
}
#endif


static void
initSignals(DECL_LD)
{
#if O_SIGNALS
  /* This is general signal handling that is not strictly needed */
  if ( truePrologFlag(PLFLAG_SIGNALS) )
  { struct signame *sn = signames;
#ifdef HAVE_OS_SIGNALS
    initTerminationSignals();
    initGuardCStack();
#endif /*HAVE_OS_SIGNALS*/
    initBackTrace();
    for( ; sn->name; sn++)
    {
#ifdef HAVE_BOEHM_GC
      if ( sn->sig == GC_get_suspend_signal() ||
	   sn->sig == GC_get_thr_restart_signal() )
	sn->flags = 0;
#endif
      if ( sn->flags )
        prepareSignal(sn->sig, sn->flags);
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
#endif /*O_SIGNALS*/

  /* these signals are not related to Unix signals and can thus */
  /* be enabled always */

  PL_signal(SIG_GC|PL_SIGSYNC,	          gc_handler);
  PL_signal(SIG_TUNE_GC|PL_SIGSYNC,	  gc_tune_handler);
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
  for (int i = 0; i < SIGMASK_WORDS; i++)
    LD->signal.pending[i] = 0;
}

#if defined(O_PLMT) && defined(HAVE_PTHREAD_SIGMASK)
#ifndef HAVE_SIGPROCMASK
#define HAVE_SIGPROCMASK 1
#endif

#define sigprocmask(how, new, old) pthread_sigmask(how, new, old)
#endif

#if O_SIGNALS && defined(HAVE_SIGPROCMASK)

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

#else /*O_SIGNALS && defined(HAVE_SIGPROCMASK)*/

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

  if ( sig && !IS_VALID_SIGNAL(sig) )
  { errno = EINVAL;
    return -1;
  }

  if ( sig == 0 )
  { for(sig=SIG_USER_OFFSET; sig<=MAXSIGNAL; sig++)
    { sh = &GD->signals.handlers[SIGNAL_INDEX(sig)];
      if ( sh->flags == 0 )
	break;
    }
    if ( !sh )
    { errno = EBUSY;
      return -2;
    }
  } else
  { sh = &GD->signals.handlers[SIGNAL_INDEX(sig)];
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
      sh->flags     = (sh->flags&~PLSIG_USERFLAGS)|act->sa_flags;
      prepareSignal(sig, act->sa_flags);
    } else
    { unprepareSignal(sig);
      sh->handler   = NULL;
      sh->predicate = NULL;
      sh->flags     = 0;
    }
  }

  return sig;
}

#ifndef SIG_DFL
#define SIG_DFL (handler_t)-1
#endif

handler_t
PL_signal(int sigandflags, handler_t func)
{ pl_sigaction_t act = {0};
  pl_sigaction_t old;

  act.sa_cfunction = func;
  if ( (sigandflags&PL_SIGSYNC) )
    act.sa_flags |= PLSIG_SYNC;
  if ( (sigandflags&PL_SIGNOFRAME) )
    act.sa_flags |= PLSIG_NOFRAME;

  if ( PL_sigaction((sigandflags & PLSIG_USERFLAGS), &act, &old) >= 0 )
  { if ( (old.sa_flags&PLSIG_PREPARED) && old.sa_cfunction )
      return old.sa_cfunction;

    return SIG_DFL;
  }

  return NULL;
}


/* return: -1: exception in handler, otherwise number of handled signals
*/

int
PL_handle_signals(void)
{ GET_LD

  if ( !is_signalled() )
    return 0;
  else
    return handleSignals();
}

#ifndef __unix__
#define handleSigInt(_) LDFUNC(handleSigInt, _)
static int
handleSigInt(DECL_LD)
{ if ( LD->signal.forced == SIGINT && WSIGMASK_ISSET(LD->signal.pending, SIGINT) )
  { WSIGMASK_CLEAR(LD->signal.pending, SIGINT);

    LD->signal.forced = 0;
    dispatch_signal(SIGINT, TRUE);

    if ( exception_term )
      return -1;
    updateAlerted(LD);

    return 1;
  }

  return 0;
}
#endif

int
handleSignals(DECL_LD)
{ int done = 0;
  int i;

  if ( !is_signalled() )
    return 0;
  if ( !HAS_LD )
    return 0;
  if ( exception_term )
    return -1;
#ifndef __unix__				/* on Unix we ask to signal twice */
  if ( (done=handleSigInt()) )
    return done;
#endif
  if ( LD->critical )
  { DEBUG(MSG_THREAD_SIGNAL,
	  Sdprintf("[%d]: ignoring signal (critical = %d)\n",
		   PL_thread_self(), LD->critical));
    return 0;
  }

  for(i=0; i<SIGMASK_WORDS; i++)
  { while( LD->signal.pending[i] )
    { int sig = MINSIGNAL+SIGMASK_WIDTH*i;
      sigmask_t mask = 1;

      for( ; mask ; mask <<= 1, sig++ )
      { if ( LD->signal.pending[i] & mask )
	{ ATOMIC_AND(&LD->signal.pending[i], ~mask);

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


#ifdef SIG_ALERT
static
PRED_IMPL("prolog_alert_signal", 2, prolog_alert_signal, 0)
{ PRED_LD
  const char *sname = signal_name(GD->signals.sig_alert);
  int rc;

  if ( strcmp(sname, "unknown") == 0 )
    rc = PL_unify_integer(A1, GD->signals.sig_alert);
  else
    rc = PL_unify_atom_chars(A1, sname);

  if ( rc )
  { if ( PL_compare(A1,A2) == CMP_EQUAL )
    { return TRUE;
    } else
    { int new;

      if ( (PL_get_integer(A2, &new) && new == 0) ||
	   PL_get_signum_ex(A2, &new) )
      { if ( GD->signals.sig_alert )
	{ unprepareSignal(GD->signals.sig_alert);
	  GD->signals.sig_alert = 0;
	}
	if ( new )
	{ GD->signals.sig_alert = new;
	  PL_signal(GD->signals.sig_alert|PL_SIGNOFRAME, alert_handler);
	}

	return TRUE;
      }
    }
  }

  return FALSE;
}
#endif


void
startCritical(DECL_LD)
{ LD->critical++;
}


int
endCritical(DECL_LD)
{ if ( --LD->critical == 0 && LD->alerted && exception_term )
    return FALSE;

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
on_signal(?SigNum, ?SigName, :OldHandler, :NewHandler)

Assign NewHandler to be called if signal arrives.

We always support this even when compiled without OS-level signal support,
because of internal virtual signal handling.
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

  if ( PL_get_integer(sig, &sign) && IS_VALID_SIGNAL(sign) )
  { TRY(PL_unify_atom_chars(name, signal_name(sign)));
  } else if ( PL_get_atom_chars(name, &sn) )
  { if ( (sign = signal_index(sn)) != -1 )
    { TRY(PL_unify_integer(sig, sign));
    } else
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal, name);
  } else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_signal, sig);

  sh = &GD->signals.handlers[SIGNAL_INDEX(sign)];

  if ( false(sh, PLSIG_STATEFLAGS) )		/* not handled */
  { TRY(PL_unify_atom(old, ATOM_default));
  } else if ( true(sh, PLSIG_IGNORED) )		/* signal ignored */
  { TRY(PL_unify_atom(old, ATOM_ignore));
  } else if ( true(sh, PLSIG_THROW) )		/* throw exception */
  { TRY(PL_unify_atom(old, ATOM_throw));
  } else if ( sh->predicate )			/* call predicate */
  { Definition def = sh->predicate->definition;

    if ( PL_unify_atom(mold, def->module->name) )
    { if ( !PL_unify_atom(old, def->functor->name) )
	return FALSE;
    } else
    { if ( !PL_unify_term(old, PL_FUNCTOR, FUNCTOR_colon2,
			         PL_ATOM, def->module->name,
			         PL_ATOM, def->functor->name) )
	return FALSE;
    }
  } else if ( sh->handler )
  { if ( sh->handler == PL_interrupt )
    { TRY(PL_unify_atom(old, ATOM_debug));
    } else
    { TRY(PL_unify_term(old,
			PL_FUNCTOR, FUNCTOR_foreign_function1,
			PL_POINTER, sh->handler));
    }
  }

  if ( PL_compare(old, new) == 0 &&
       PL_compare(mold, mnew) == 0 )
    succeed;					/* no change */

  if ( PL_get_atom(new, &a) )
  { if ( a == ATOM_default )
    { unprepareSignal(sign);
    } else if ( a == ATOM_ignore )
    { prepareSignal(sign, PLSIG_IGNORE);	/* request to ignore this signal */
    } else if ( a == ATOM_throw )
    { sh = prepareSignal(sign, PLSIG_THROW|PLSIG_SYNC);
      sh->handler   = NULL;
      sh->predicate = NULL;
    } else if ( a == ATOM_debug )
    { sh = prepareSignal(sign, 0);

      sh->handler = (handler_t)PL_interrupt;
      sh->predicate = NULL;

    } else
    { Module m;
      predicate_t pred;

      if ( !get_module(mnew, &m) )
	return FALSE;
      pred = lookupProcedure(PL_new_functor(a, 1), m);

      sh = prepareSignal(sign, PLSIG_SYNC);
      sh->handler = NULL;
      sh->predicate = pred;
    }
  } else if ( PL_is_functor(new, FUNCTOR_foreign_function1) )
  { term_t a = PL_new_term_ref();
    void *f;

    _PL_get_arg(1, new, a);

    if ( PL_get_pointer(a, &f) )
    { sh = prepareSignal(sign, 0);
      sh->handler = (handler_t)f;
      sh->predicate = NULL;

      succeed;
    }

    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal_handler, new);
  } else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_signal_handler, new);

  succeed;
}


		 /*******************************
		 *	       STACKS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
initPrologStacks() creates the stacks for the calling thread. It is used
both at system startup to create the stack   for the main thread as from
pl-thread.c to create stacks for Prolog threads.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
initPrologStacks(size_t limit)
{ GET_LD

  LD->stacks.limit = limit;
  if ( !allocStacks() )
    return FALSE;

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

  return TRUE;
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
    LD->undo.undo_list      = init_undo_list(); /* must be first.  See __do_undo() */
    LD->tabling.delay_list  = init_delay_list();
    LD->tabling.idg_current = PL_new_term_ref();
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
  - spare is the amount of spare stack we reserve
  - gc indicates whether gc can collect data on the stack
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
init_stack(Stack s, char *name, size_t size, size_t spare, int gc)
{ s->name	= name;
  s->top	= s->base;
  s->spare      = spare;
  s->def_spare  = spare;
  s->min_free   = 256*sizeof(word);
  s->max	= addPointer(s->base, size - spare);
  s->gced_size  = 0L;			/* size after last gc */
  s->gc	        = gc;
  gcPolicy(s, GC_FAST_POLICY);
}


static int
allocStacks(DECL_LD)
{ size_t minglobal = 8*SIZEOF_VOIDP K;
  size_t minlocal  = 4*SIZEOF_VOIDP K;
  size_t mintrail  = 4*SIZEOF_VOIDP K;
  size_t minarg    = 1*SIZEOF_VOIDP K;

  size_t itrail  = nextStackSizeAbove(mintrail-1);
  size_t iglobal = nextStackSizeAbove(minglobal-1);
  size_t ilocal  = nextStackSizeAbove(minlocal-1);

  itrail  = stack_nalloc(itrail);
  minarg  = stack_nalloc(minarg);
  iglobal = stack_nalloc(iglobal+ilocal)-ilocal;

  gBase = NULL;
  tBase = NULL;
  aBase = NULL;

  gBase = (Word)       stack_malloc(iglobal + ilocal);
  tBase = (TrailEntry) stack_malloc(itrail);
  aBase = (Word *)     stack_malloc(minarg);

  if ( !gBase || !tBase || !aBase )
  { if ( gBase )
      *gBase++ = MARK_MASK;		/* compensate for freeStacks */
    freeStacks();
    return FALSE;
  }

  lBase   = (LocalFrame) addPointer(gBase, iglobal);

  init_stack((Stack)&LD->stacks.global,
	     "global",   iglobal, 512*SIZEOF_VOIDP, TRUE);
  init_stack((Stack)&LD->stacks.local,
	     "local",    ilocal,  512*SIZEOF_VOIDP + LOCAL_MARGIN, FALSE);
  init_stack((Stack)&LD->stacks.trail,
	     "trail",    itrail,  256*SIZEOF_VOIDP, TRUE);
  init_stack((Stack)&LD->stacks.argument,
	     "argument", minarg,  0,                FALSE);

  LD->stacks.local.min_free = LOCAL_MARGIN;

  return TRUE;
}


void
freeStacks(DECL_LD)
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


int
trim_stack(Stack s)
{ if ( s->spare < s->def_spare )
  { ssize_t reduce = s->def_spare - s->spare;
    ssize_t room = roomStackP(s);

    DEBUG(MSG_SPARE_STACK, Sdprintf("Reset spare for %s (%zd->%zd)\n",
				    s->name, s->spare, s->def_spare));
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
trimStacks() reclaims all unused space on the stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
trimStacks(DECL_LD int resize)
{ LD->trim_stack_requested = FALSE;

  if ( resize )
  { growStacks(GROW_TRIM, GROW_TRIM, GROW_TRIM);
  } else
  { trim_stack((Stack) &LD->stacks.local);
    trim_stack((Stack) &LD->stacks.global);
    trim_stack((Stack) &LD->stacks.trail);
  }

#ifdef SECURE_GC
  { Word p;				/* clear the stacks */

    for(p=gTop; p<gMax; p++)
      *p = 0xbfbfbfbf;
    for(p=(Word)lTop; p<(Word)lMax; p++)
      *p = 0xbfbfbfbf;
  }
#endif

  DEBUG(CHK_SECURE,
	{ scan_global(FALSE);
	  checkStacks(NULL);
	});
}


static
PRED_IMPL("trim_stacks", 0, trim_stacks, 0)
{ PRED_LD

  trimStacks(TRUE);

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
{ discardBuffer(&ld->fli._discardable_buffer);
  discardStringStack(&ld->fli.string_buffers);
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
  free_predicate_references(ld);
  destroy_event_list(&ld->event.hook.onthreadexit);
  free_thread_wait(ld);
#endif

#ifdef O_LOCALE
  if ( ld->locale.current )
    releaseLocale(ld->locale.current);
#endif

  if ( ld->qlf.getstr_buffer )
    free(ld->qlf.getstr_buffer);

  clearThreadTablingData(ld);
  if ( ld->tabling.node_pool )
    free_alloc_pool(ld->tabling.node_pool);

#ifdef O_C_STACK_GUARDED
  if ( ld->signal.alt_stack )
    free(ld->signal.alt_stack);
#endif

#ifdef O_COVERAGE
  free_coverage_data(ld);
#endif

  free_undo_data(ld);

  if ( ld->btrace_store )
  { btrace_destroy(ld->btrace_store);
    ld->btrace_store = NULL;
  }

  cleanAbortHooks(ld);
  unreferenceStandardStreams(ld);
}

/* The following definitions aren't necessary for compiling, and in fact
 * you could comment this whole section out without breaking the code.
 * However, they don't take up much space in the binary and they assist
 * in C-level debugging, so I'm leaving them in regardless of O_DEBUG. */

const intptr_t __PL_ld = -1;
const intptr_t PL__ctx = -1;

inline PL_local_data_t*
(__FIND_LD)(PL_local_data_t *pl_ld, control_t pl_ctx, PL_local_data_t *fallback)
{ if ((intptr_t)pl_ld != -1)
  { return pl_ld; }
  if ((intptr_t)pl_ctx != -1)
  { return pl_ctx->engine; }
  return fallback;
}

#ifndef no_local_ld
PL_local_data_t*
no_local_ld(void)
{ return NULL; }
#endif


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

int
set_stack_limit(size_t limit)
{ GET_LD

  if ( limit < LD->stacks.limit &&
       limit < sizeStack(local) +
               sizeStack(global) +
               sizeStack(trail) )
  { garbageCollect(GC_USER);
    trimStacks(TRUE);

    if ( limit < sizeStack(local) +
	         sizeStack(global) +
	         sizeStack(trail) )
    { term_t ex;


      return ( (ex=PL_new_term_ref()) &&
	       PL_put_int64(ex, limit) &&
	       PL_error(NULL, 0, NULL, ERR_PERMISSION,
			ATOM_limit, ATOM_stacks, ex)
	     );
    }
  }

  LD->stacks.limit = limit;

  return TRUE;
}


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

      if ( !printMessage(ATOM_warning,
			 PL_FUNCTOR_CHARS, "deprecated", 1,
			   PL_FUNCTOR_CHARS, "set_prolog_stack", 2,
			     PL_TERM, A1,
			     PL_ATOM, ATOM_limit) )
	return FALSE;

      return ( PL_unify_int64(old, LD->stacks.limit) &&
	       PL_get_size_ex(value, &newlimit) &&
	       set_stack_limit(newlimit)
	     );
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
  PRED_DEF("$set_prolog_stack",	  4, set_prolog_stack,	  0)
  PRED_DEF("trim_stacks",	  0, trim_stacks,	  0)
  PRED_DEF("$on_signal",	  4, on_signal,		  0)
#ifdef SIG_ALERT
  PRED_DEF("prolog_alert_signal", 2, prolog_alert_signal, 0)
#endif
EndPredDefs
