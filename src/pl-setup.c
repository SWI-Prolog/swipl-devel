/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*#define O_DEBUG 1*/

#define GLOBAL SO_LOCAL			/* allocate global variables here */
#include "pl-incl.h"
#include "os/pl-cstack.h"
#include "pl-dbref.h"
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

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

  LD->critical = 0;
  LD->signal.pending = 0;

  startCritical;
  DEBUG(1, Sdprintf("wam_table ...\n"));
  initWamTable();
  DEBUG(1, Sdprintf("character types ...\n"));
  initCharTypes();
  DEBUG(1, Sdprintf("foreign predicates ...\n"));
  initForeign();
#if HAVE_SIGNAL
  DEBUG(1, Sdprintf("Prolog Signal Handling ...\n"));
  if ( truePrologFlag(PLFLAG_SIGNALS) )
  { initSignals();
    initBackTrace();
  }
#endif
  DEBUG(1, Sdprintf("Stacks ...\n"));
  if ( !initPrologStacks(GD->options.localSize,
			 GD->options.globalSize,
			 GD->options.trailSize) )
    fatalError("Not enough address space to allocate Prolog stacks");
  initPrologLocalData(PASS_LD1);

  DEBUG(1, Sdprintf("Atoms ...\n"));
  initAtoms();
  DEBUG(1, Sdprintf("Features ...\n"));
  initPrologFlags();
  DEBUG(1, Sdprintf("Functors ...\n"));
  initFunctors();
  DEBUG(1, Sdprintf("Modules ...\n"));
  initTables();
  initModules();
					/* initModules may be called before */
					/* LD is present in the MT version */
  LD->modules.typein = MODULE_user;
  LD->modules.source = MODULE_user;
  DEBUG(1, Sdprintf("Records ...\n"));
  initDBRef();
  initRecords();
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
  initFiles();
  initIO();
  initCharConversion();
  GD->io_initialised = TRUE;

  if ( !endCritical )
    return FALSE;

  DEBUG(1, Sdprintf("Heap Initialised\n"));
  return TRUE;
}


void
initPrologLocalData(ARG1_LD)
{
#ifdef O_LIMIT_DEPTH
  depth_limit   = (uintptr_t)DEPTH_NO_LIMIT;
#endif

  LD->break_level = -1;
  LD->prolog_flag.write_attributes = PL_WRT_ATTVAR_IGNORE;

  updateAlerted(LD);
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			   SIGNAL HANDLING

SWI-Prolog catches a number of signals.   Interrupt  is catched to allow
the user to interrupt  normal   execution.  Segmentation  violations are
trapped on machines using the MMU to implement stack overflow checks and
stack expansion. These signal handlers needs  to be preserved over saved
states and the system  should  allow   foreign  language  code to handle
signals without interfering  with  Prologs   signal  handlers.  For this
reason a layer is wired around the OS signal handling.

Code in SWI-Prolog should  call  PL_signal()  rather  than  signal()  to
install  signal  handlers.  SWI-Prolog assumes the handler function is a
void function.  On some systems this gives  some  compiler  warnigns  as
they  define  signal handlers to be int functions.  This should be fixed
some day.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if HAVE_SIGNAL
#define HAVE_SIGNALS 1

#define PLSIG_PREPARED 0x01		/* signal is prepared */
#define PLSIG_THROW    0x02		/* throw signal(num, name) */
#define PLSIG_SYNC     0x04		/* call synchronously */
#define PLSIG_NOFRAME  0x08		/* Do not create a Prolog frame */

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
  { SIG_EXCEPTION,     "prolog:exception",     0 },
#ifdef SIG_ATOM_GC
  { SIG_ATOM_GC,   "prolog:atom_gc",       0 },
#endif
  { SIG_GC,	       "prolog:gc",	       0 },
#ifdef SIG_THREAD_SIGNAL
  { SIG_THREAD_SIGNAL, "prolog:thread_signal", 0 },
#endif

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

static void
dispatch_signal(int sig, int sync)
{ GET_LD
  SigHandler sh = &GD->sig_handlers[sig];
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

  DEBUG(1, Sdprintf("Got signal %d in thread %d (=%d) %s\n",
		    sig, LD->thread.info->pl_tid,
		    pthread_self(),
		    sync ? " (sync)" : " (async)"));
#else
  DEBUG(1, Sdprintf("Got signal %d %s\n",
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

  DEBUG(1, Sdprintf("Handling signal %d, pred = %p, handler = %p\n",
		    sig, sh->predicate, sh->handler));

  if ( sh->predicate )
  { term_t sigterm = PL_new_term_ref();
    term_t except;
    qid_t qid;

    PL_put_atom_chars(sigterm, signal_name(sig));
    qid = PL_open_query(NULL,
			PL_Q_CATCH_EXCEPTION,
			sh->predicate,
			sigterm);
    if ( !PL_next_solution(qid) && (except = PL_exception(qid)) )
    { PL_cut_query(qid);
      if ( !sync )
	unblockGC(0 PASS_LD);
      PL_throw(except);
      return;				/* make sure! */
    } else
    { if ( sync )
	PL_cut_query(qid);
      else
	PL_close_query(qid);
    }
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
    if ( !sync )
      unblockGC(0 PASS_LD);

    PL_throw(exception_term);		/* throw longjmp's */
    return;				/* make sure! */
  } else if ( sh->handler )
  { (*sh->handler)(sig);

    if ( exception_term && !sync )	/* handler: PL_raise_exception() */
    { LD->signal.exception = PL_record(exception_term);
      PL_raise(SIG_EXCEPTION);
      exception_term = 0;
    }
  }

  LD->signal.current = saved_current_signal;
  LD->signal.is_sync = saved_sync;
  if ( sync )
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
{ SigHandler sh = &GD->sig_handlers[sig];

  if ( false(sh, PLSIG_PREPARED) )
  { set(sh, PLSIG_PREPARED);
    if ( sig < SIG_PROLOG_OFFSET )
      sh->saved_handler = set_sighandler(sig, pl_signal_handler);
  }

  return sh;
}


static void
unprepareSignal(int sig)
{ SigHandler sh = &GD->sig_handlers[sig];

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


static void
sig_exception_handler(int sig)
{ GET_LD
  (void)sig;

  if ( LD && LD->signal.exception )
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
free_clauses_handler(int sig)
{ GET_LD
  ClauseRef cref;
  (void)sig;

  if ( (cref=LD->freed_clauses) )
  { LD->freed_clauses = NULL;
    freeClauseList(cref);
  }
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
{ (void)sig;
}
#endif


static void
initSignals(void)
{ struct signame *sn = signames;

#ifdef SIGPIPE
  set_sighandler(SIGPIPE, SIG_IGN);
#endif

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

  PL_signal(SIG_EXCEPTION|PL_SIGSYNC, sig_exception_handler);
  PL_signal(SIG_GC|PL_SIGSYNC, gc_handler);
  PL_signal(SIG_FREECLAUSES|PL_SIGSYNC, free_clauses_handler);
  PL_signal(SIG_PLABORT|PL_SIGSYNC, abort_handler);

#ifdef SIG_ALERT
  PL_signal(SIG_ALERT|PL_SIGNOFRAME, alert_handler);
#endif
#ifdef SIG_THREAD_SIGNAL
  PL_signal(SIG_THREAD_SIGNAL|PL_SIGSYNC, executeThreadSignals);
#endif
#ifdef SIG_ATOM_GC
  PL_signal(SIG_ATOM_GC|PL_SIGSYNC, agc_handler);
#endif
#ifdef SIGHUP
  PL_signal(SIGHUP|PL_SIGSYNC, hupHandler);
#endif
}


void
cleanupSignals(void)
{ struct signame *sn = signames;

  for( ; sn->name; sn++)
    unprepareSignal(sn->sig);
}


void
resetSignals()
{ GET_LD

  LD->signal.current = 0;
  LD->signal.pending = 0L;
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



handler_t
PL_signal(int sigandflags, handler_t func)
{ if ( HAVE_SIGNALS )
  { handler_t old;
    SigHandler sh;
    int sig = (sigandflags & 0xffff);

    if ( sig >= MAXSIGNAL )
    { warning("PL_signal(): illegal signal number: %d", sig);
      return SIG_DFL;
    }

    sh = &GD->sig_handlers[sig];
    if ( true(sh, PLSIG_PREPARED) )
    { old = sh->handler;
      if ( func == sh->saved_handler )
	unprepareSignal(sig);
      else
	sh->handler = func;
    } else
    { sh = prepareSignal(sig);
      old = sh->saved_handler;
      sh->handler = func;
    }
    if ( func != SIG_DFL )
      clear(sh, PLSIG_THROW);		/* we have a user handler now */

    if ( (sigandflags & PL_SIGSYNC) )
      set(sh, PLSIG_SYNC);
    else
      clear(sh, PLSIG_SYNC);
    if ( (sigandflags & PL_SIGNOFRAME) )
      set(sh, PLSIG_NOFRAME);
    else
      clear(sh, PLSIG_NOFRAME);

    return old;
  } else
    return SIG_DFL;
}


/* return: -1: exception in handler, otherwise number of handled signals
*/

int
PL_handle_signals()
{ GET_LD

  if ( !LD || LD->critical || !LD->signal.pending )
    return 0;

  return handleSignals(PASS_LD1);
}


int
handleSignals(ARG1_LD)
{ int done = 0;

  if ( !LD || LD->critical )
    return 0;

  while( LD->signal.pending )
  { int64_t mask = 1;
    int sig = 1;

    for( ; mask ; mask <<= 1, sig++ )
    { if ( LD->signal.pending & mask )
      { simpleMutexLock(&LD->signal.sig_lock);
	LD->signal.pending &= ~mask;	/* reset the signal */
	simpleMutexUnlock(&LD->signal.sig_lock);

	done++;
	dispatch_signal(sig, TRUE);

	if ( exception_term )
	  goto out;
      }
    }
  }

out:
  if ( exception_term )
    return -1;
  if ( done )
    updateAlerted(PASS_LD1);

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

  sh = &GD->sig_handlers[sign];

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
    *size = maxarea;
  else if ( *size > (size_t)(MAXTAGGEDPTR+1) )
  { if ( *size != (size_t)-1 )		/* user demanded maximum */
      Sdprintf("WARNING: Maximum stack size for %s stack is %d MB\n",
	       name, (MAXTAGGEDPTR+1) / (1 MB));
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
    exception_bin         = PL_new_term_ref();
    exception_printed     = PL_new_term_ref();
    LD->exception.tmp     = PL_new_term_ref();
    LD->exception.pending = PL_new_term_ref();
    LD->trim.dummy        = PL_new_term_ref();
#ifdef O_ATTVAR
    LD->attvar.head	= PL_new_term_ref();
    LD->attvar.tail       = PL_new_term_ref();
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
Malloc/realloc/free based stack allocation
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
init_stack(Stack s, char *name, size_t size, size_t limit, size_t spare)
{ GET_LD

  s->name	= name;
  s->top	= s->base;
  s->size_limit	= limit;
  s->spare      = spare;
  s->def_spare  = spare;
  s->min_free   = 256*sizeof(word);
  s->max	= addPointer(s->base, size - spare);
  s->gced_size  = 0L;			/* size after last gc */
  s->gc	        = ((s == (Stack) &LD->stacks.global ||
		    s == (Stack) &LD->stacks.trail) ? TRUE : FALSE);
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

  gBase = (Word)       stack_malloc(iglobal + ilocal);
  tBase = (TrailEntry) stack_malloc(itrail);
  aBase = (Word *)     stack_malloc(minargument);
  if ( !gBase || !tBase || !aBase )
  { freeStacks(PASS_LD1);
    fail;
  }

  lBase = (LocalFrame) addPointer(gBase, iglobal);

  init_stack((Stack)&LD->stacks.global,
	     "global",   iglobal, global, 512*SIZEOF_VOIDP);
  init_stack((Stack)&LD->stacks.local,
	     "local",    ilocal,  local,  512*SIZEOF_VOIDP);
  init_stack((Stack)&LD->stacks.trail,
	     "trail",    itrail,  trail,  256*SIZEOF_VOIDP);
  init_stack((Stack)&LD->stacks.argument,
	     "argument", argument, argument, 0);

  LD->stacks.local.min_free = LOCAL_MARGIN;

  succeed;
}


void
freeStacks(ARG1_LD)
{ if ( gBase ) { gBase--;
		 stack_free(gBase); gBase = NULL; lBase = NULL; }
  if ( tBase ) { stack_free(tBase); tBase = NULL; }
  if ( aBase ) { stack_free(aBase); aBase = NULL; }
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

    PL_LOCK(L_MISC);
    GD->statistics.stack_space += size;
    PL_UNLOCK(L_MISC);
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
    PL_LOCK(L_MISC);
    GD->statistics.stack_space -= osize;
    GD->statistics.stack_space += size;
    PL_UNLOCK(L_MISC);
    return sp;
  }
#endif

  return NULL;
}

void
stack_free(void *mem)
{ size_t *sp = mem;
  size_t osize = *--sp;

  PL_LOCK(L_MISC);
  GD->statistics.stack_space -= osize;
  PL_UNLOCK(L_MISC);

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

    if ( room < reduce )
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called at the end of handling an exception. We cannot do GC, however, we
can request it, after it will be executed   at the start of the recovery
handler. If no GC is needed, we call trimStacks() to re-enable the spare
stack-space if applicable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
resumeAfterException(void)
{ GET_LD

  if ( !considerGarbageCollect((Stack)NULL) )
    trimStacks(FALSE, PASS_LD1);

  LD->exception.processing = FALSE;
  LD->outofstack = NULL;
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
    PL_free(ld->bags.default_bag);

#ifdef O_CYCLIC
  clearSegStack(&ld->cycle.lstack);
  clearSegStack(&ld->cycle.vstack);
#endif

  freeArithLocalData(ld);
#ifdef O_PLMT
  simpleMutexDelete(&ld->signal.sig_lock);
  if ( ld->prolog_flag.table )
  { PL_LOCK(L_PLFLAG);
    destroyHTable(ld->prolog_flag.table);
    PL_UNLOCK(L_PLFLAG);
  }
#endif

  if ( ld->qlf.getstr_buffer )
    free(ld->qlf.getstr_buffer);
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
