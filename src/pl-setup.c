/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

/*#define O_DEBUG 1*/

#define GLOBAL SO_LOCAL			/* allocate global variables here */
#include "pl-incl.h"
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

static   int allocStacks(intptr_t local, intptr_t global, intptr_t trail, intptr_t argument);
forwards void initSignals(void);

#undef I
#define I TAGEX_INDIRECT

const unsigned int tagtypeex[] = 
{
	    /* var     attvar  int    float   atom   string    term     ref */
/* static */	0,	0,	0,	0,	0,	0,	0,	0,
/* heap */	0,	0,      I,	I,	0,	I,	0,	0,
/* global */	0,	0,      I,	I,	0,	I,	0,	0,
/* local */	0,	0,	0,	0,	0,	0,	0,	0
};

#undef I

void
setupProlog(void)
{ DEBUG(1, Sdprintf("Starting Heap Initialisation\n"));

  LD->critical = 0;
  LD->aborted = FALSE;
  LD->pending_signals = 0;

  startCritical;
  initCharTypes();
  initForeign();
#if HAVE_SIGNAL
  DEBUG(1, Sdprintf("Prolog Signal Handling ...\n"));
  if ( trueFeature(SIGNALS_FEATURE) )
    initSignals();
#endif
  DEBUG(1, Sdprintf("Stacks ...\n"));
  if ( !initPrologStacks(GD->options.localSize, 
			 GD->options.globalSize, 
			 GD->options.trailSize, 
			 GD->options.argumentSize) )
    fatalError("Not enough address space to allocate Prolog stacks");
  initPrologLocalData();

  DEBUG(1, Sdprintf("Atoms ...\n"));
  initAtoms();
  DEBUG(1, Sdprintf("Features ...\n"));
  initFeatures();
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
  DEBUG(1, Sdprintf("wam_table ...\n"));
  initWamTable();
  DEBUG(1, Sdprintf("IO ...\n"));
  initIO();
  initCharConversion();
  DEBUG(1, Sdprintf("Term ...\n"));
  resetTerm();
  GD->io_initialised = TRUE;

  endCritical;

  DEBUG(1, Sdprintf("Heap Initialised\n"));
}


void
initPrologLocalData(void)
{
#ifdef O_LIMIT_DEPTH
  depth_limit   = (uintptr_t)DEPTH_NO_LIMIT;
  depth_reached = 0;
#endif

  environment_frame = (LocalFrame) NULL;
  LD->statistics.inferences = 0;
  LD->float_format = "%g";
  LD->feature.write_attributes = PL_WRT_ATTVAR_IGNORE;
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define the signals and  their  properties.   This  could  be  nicer, but
different systems provide different signals, and   above all, MS systems
provide very few.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct signame
{ int 	      sig;
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
  { SIGILL,	"ill",    PLSIG_THROW},
  { SIGABRT,	"abrt",   0},
  { SIGFPE,	"fpe",    PLSIG_THROW},
#ifdef SIGKILL
  { SIGKILL,	"kill",   0},
#endif
  { SIGSEGV,	"segv",   PLSIG_THROW},
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
  { SIGBUS,	"bus",    PLSIG_THROW},
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
  { -1,		NULL,     0}
};

static const char *
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

  for( ; sn->name; sn++ )
  { if ( streq(sn->name, name) )
      return sn->sig;
  }

  return -1;
}


int
_PL_get_signum(term_t sig, int *n)
{ char *s;
  int i = -1;

  if ( !PL_get_integer(sig, &i) )
  { if ( PL_get_atom_chars(sig, &s) )
      i = signal_index(s);
  }
  if ( i > 0 && i < 32 )		/* where to get these? */
  { *n = i;
    return TRUE;
  }

  return FALSE;
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
  uintptr_t lTopSave;
  int saved_current_signal;

#ifdef O_PLMT
  if ( !LD )
  { if ( sh->handler )
      (*sh->handler)(sig);

    return;				/* what else?? */
  }
#endif

  DEBUG(1, Sdprintf("Got signal %d in thread %d (=%d) %s\n",
		    sig, LD->thread.info->pl_tid,
		    pthread_self(),
		    sync ? " (sync)" : " (async)"));
	
  lTopSave = (char*)lTop - (char*)lBase;
  saved_current_signal = LD->current_signal;

  switch(sig)
  { case SIGFPE:
    case SIGSEGV:
#if defined(SIGBUS) && SIGBUS != SIGSEGV
    case SIGBUS:
#endif
      if ( sig == LD->current_signal )
	sysError("Recursively received fatal signal %d", sig);
  }

  if ( gc_status.active )
  { fatalError("Received signal %d (%s) while in %ld-th garbage collection",
	       sig, signal_name(sig), gc_status.collections);
  }

  if ( LD->critical || (true(sh, PLSIG_SYNC) && !sync) )
  { PL_raise(sig);			/* wait for better times! */
    return;
  }

  blockGC(PASS_LD1);
  LD->current_signal = sig;
  fid = PL_open_signal_foreign_frame();

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
      unblockGC(PASS_LD1);
      PL_throw(except);
      return;				/* make sure! */
    } else
      PL_close_query(qid);
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
    unblockGC(PASS_LD1);

    PL_throw(exception_term);		/* throw longjmp's */
    return;				/* make sure! */
  } else if ( sh->handler )
  { (*sh->handler)(sig);

    if ( exception_term && !sync )	/* handler: PL_raise_exception() */
    { LD->pending_exception = PL_record(exception_term);
      PL_raise(SIG_EXCEPTION);
      exception_term = 0;
    }
  }

  LD->current_signal = saved_current_signal;
  PL_discard_foreign_frame(fid);
  lTop = addPointer(lBase, lTopSave);

  unblockGC(PASS_LD1);
}

#undef LD
#define LD GLOBAL_LD

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
  return signal(sig, func);
#endif
} 

#ifdef HAVE_SIGINFO_H
#include <siginfo.h>
#endif


#if defined(O_SEGV_HANDLING) && defined(O_DYNAMIC_STACKS)
static handler_t
set_stack_guard_handler(int sig, void *func)
{ 
#ifdef HAVE_SIGACTION
  struct sigaction old;
  struct sigaction new;

  memset(&new, 0, sizeof(new));	/* deal with other fields */
#ifdef USE_SIGINFO
  new.sa_sigaction = func;
  new.sa_flags     = SA_RESTART|SA_SIGINFO;
#else
  new.sa_handler   = func;
  new.sa_flags     = SA_RESTART;
#endif

  if ( !trueFeature(SIGNALS_FEATURE) &&
       !GD->options.silent )
  {			/* We need double \\ to get a single at Sdprintf() */
    Sdprintf("\\% Prolog still handles SIG_SEGV\n"
	     "\\% To avoid this, reconfigure using --disable-segv-handling\n");
  }

  if ( sigaction(sig, &new, &old) == 0 )
    return old.sa_handler;
  else
    return SIG_DFL;
#else
  return signal(sig, func);
#endif
}
#endif /*O_SEGV_HANDLING*/


static SigHandler
prepareSignal(int sig)
{ SigHandler sh = &GD->sig_handlers[sig];

  if ( false(sh, PLSIG_PREPARED) )
  { set(sh, PLSIG_PREPARED);
    sh->saved_handler = set_sighandler(sig, pl_signal_handler);
  }

  return sh;
}


static void
unprepareSignal(int sig)
{ SigHandler sh = &GD->sig_handlers[sig];

  if ( true(sh, PLSIG_PREPARED) )
  { set_sighandler(sig, sh->saved_handler);
    sh->flags         = 0;
    sh->handler       = NULL;
    sh->predicate     = NULL;
    sh->saved_handler = NULL;
  }
}


static void
hupHandler(int sig)
{ PL_halt(2);
}


static void
initSignals(void)
{ struct signame *sn = signames;
  
#ifdef SIGPIPE
  set_sighandler(SIGPIPE, SIG_IGN);
#endif

  for( ; sn->name; sn++)
  { if ( sn->flags )
    { SigHandler sh = prepareSignal(sn->sig);
      sh->flags |= sn->flags;
    }
  }

#ifdef SIGHUP
  PL_signal(SIGHUP, hupHandler);
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
{ LD->current_signal = 0;
  LD->pending_signals = 0L;
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
{ sigfillset(set);
#ifdef O_PROFILE
  sigdelset(set, SIGPROF);
#endif
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

    if ( sig > MAXSIGNAL )
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

    return old;
  } else
    return SIG_DFL;
}


int
PL_handle_signals()
{ int done = 0;
  PL_local_data_t *ld = LD;

  if ( !ld || ld->critical )
    return 0;

  while(ld->pending_signals)
  { uintptr_t mask = 1;
    int sig = 1;

    for( ; mask ; mask <<= 1, sig++ )
    { if ( ld->pending_signals & mask )
      { ld->pending_signals &= ~mask;	/* reset the signal */

	done++;

#ifdef O_PLMT
        if ( sig == SIG_THREAD_SIGNAL )
	  executeThreadSignals(sig);
	else
#endif
#ifdef O_ATOMGC
	if ( sig == SIG_ATOM_GC )
	{ if ( GD->statistics.atoms >= GD->atoms.non_garbage + GD->atoms.margin &&
	       !gc_status.blocked )
	    pl_garbage_collect_atoms();
	} else
#endif
        if ( sig == SIG_EXCEPTION && ld->pending_exception )
	{ record_t ex = ld->pending_exception;
	  
	  ld->pending_exception = 0;

	  PL_put_variable(exception_bin);
	  PL_recorded(ex, exception_bin);
	  PL_erase(ex);
	  exception_term = exception_bin;

	  SECURE(checkData(valTermRef(exception_term)));
	} else
	  dispatch_signal(sig, TRUE);

	if ( exception_term )
	  return -1;
      }
    }
  }

  if ( exception_term )
    return -1;
  return done;
}


void
endCritical__LD(ARG1_LD)
{ if ( LD->aborted )
    pl_abort(ABORT_NORMAL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
signal(+Signal, :Old, :Handler)
	Assign Handler to be called if signal arrises.  Example:

		signal(usr1, Old, handle_user_1/1).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

foreign_t
pl_on_signal(term_t sig, term_t name, term_t old, term_t new)
{ int sign = -1;
  SigHandler sh;
  char *sn;
  atom_t a;
  Module m = NULL;

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

    if ( def->module == MODULE_user )
    { TRY(PL_unify_atom(old, def->functor->name));
    } else
    { TRY(PL_unify_term(old,
			PL_FUNCTOR, FUNCTOR_colon2,
			   PL_ATOM, def->module->name, 
			   PL_ATOM, def->functor->name));
    }
  } else if ( sh->handler )
  { TRY(PL_unify_term(old,
		      PL_FUNCTOR, FUNCTOR_foreign_function1,
		      PL_POINTER, sh->handler));
  }    

  if ( PL_compare(old, new) == 0 )
    succeed;					/* no change */

  PL_strip_module(new, &m, new);

  if ( PL_get_atom(new, &a) )
  { if ( a == ATOM_default )
    { unprepareSignal(sign);
    } else if ( a == ATOM_throw )
    { sh = prepareSignal(sign);
      set(sh, PLSIG_THROW);
      sh->handler   = NULL;
      sh->predicate = NULL;
    } else
    { predicate_t pred = lookupProcedure(PL_new_functor(a, 1), m);
      
      sh = prepareSignal(sign);
      clear(sh, PLSIG_THROW);
      sh->handler = NULL;
      sh->predicate = pred;
    }
  } else if ( PL_is_functor(new, FUNCTOR_foreign_function1) )
  { term_t a = PL_new_term_ref();
    void *f;

    PL_get_arg(1, new, a);

    if ( PL_get_pointer(a, &f) )
    { sh = prepareSignal(sign);
      clear(sh, PLSIG_THROW);
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
enforce_limit(intptr_t *size, intptr_t maxarea, const char *name)
{ if ( *size == 0 )
    *size = maxarea;
  else if ( *size > (intptr_t)(MAXTAGGEDPTR+1) )
  { if ( *size != LONG_MAX )		/* user demanded maximum */
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
initPrologStacks(intptr_t local, intptr_t global, intptr_t trail, intptr_t argument)
{ uintptr_t maxarea;

  maxarea = MAXTAGGEDPTR+1;		/* MAXTAGGEDPTR = 0x..fff.. */
  if ( maxarea > 1024 MB )		/* 64-bit machines */
    maxarea = 1024 MB;

  enforce_limit(&local,	   maxarea,  "local");
  enforce_limit(&global,   maxarea,  "global");
  enforce_limit(&trail,	   maxarea,  "trail");
  enforce_limit(&argument, global/2, "argument");

  if ( !allocStacks(local, global, trail, argument) )
    fail;

  base_addresses[STG_LOCAL]  = (uintptr_t)lBase;
  base_addresses[STG_GLOBAL] = (uintptr_t)gBase;
  base_addresses[STG_TRAIL]  = (uintptr_t)tBase;
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
emptyStacks()
{ environment_frame = NULL;
  fli_context       = NULL;

  emptyStack((Stack)&LD->stacks.local);
  emptyStack((Stack)&LD->stacks.global);
  emptyStack((Stack)&LD->stacks.trail);
  emptyStack((Stack)&LD->stacks.argument);

  PL_open_foreign_frame();
  exception_bin         = PL_new_term_ref();
  exception_printed     = PL_new_term_ref();
  LD->exception.tmp     = PL_new_term_ref();
  LD->exception.pending = PL_new_term_ref();
#ifdef O_ATTVAR
  LD->attvar.head	= PL_new_term_ref();
  LD->attvar.tail       = PL_new_term_ref();
#endif
#ifdef O_GVAR
  destroyGlobalVars();
#endif

  LD->mark_bar          = (Word)(~(uintptr_t)0);
}

#if O_DYNAMIC_STACKS

static void init_stack(Stack s, char *name,
		       void* base, intptr_t limit, intptr_t minsize);
static void gcPolicy(Stack s, int policy);

#ifdef O_SEGV_HANDLING
#ifdef USE_SIGINFO
RETSIGTYPE _PL_segv_handler(int sig, siginfo_t *info, void *);
#else
RETSIGTYPE _PL_segv_handler(int sig);
#endif
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
STACK_SEPARATION defines the  space  between   the  stacks.  The maximum
discontinuity while writing the local stack  is determined by the number
of variables in the clause.  An example worst case is:

foo :-
	(   failing_goal,
	    bar(term(A, B, C, ....))
	;   hello(AnotherVar)
	).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define STACK_SEPARATION ROUND(MAXVARIABLES*sizeof(word), size_alignment)
#define STACK_SIGNAL	 (2 * size_alignment)
#define STACK_RESERVE	 (1 * size_alignment)
#define STACK_MINIMUM    (32 * 1024)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			STACK MEMORY MANAGEMENT

In these days some operating systems allows the  user  to  map  physical
memory  anywhere  in  the  virtual  address  space.  For multiple stacks
machines such as Prolog, this is ideal.  The  stacks  can  be  allocated
very  far  appart  with  large  gaps  between  them.   Stack overflow is
detected by hardware and results (in  Unix)  in  a  segmentation  fault.
This fault is trapped and the stack is automatically expanded by mapping
more  memory.

In theory the stacks can be deallocated dynamically as  well,  returning
the  resources to the system.  Currently this can be done explicitely by
calling  trim_stacks/0  and  the  garbage  collector.    It   might   be
interesting  to  do  this  automatically  at  certain points to minimise
memory requirements.  How?

Currently this mechanism uses mmap()  and   munmap()  available  in most
todays Unix systems or VirtualAlloc() and friends in Win32.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <errno.h>
#ifndef __WINDOWS__
extern int errno;
#endif /*__WINDOWS__*/

static int size_alignment;	/* Stack sizes must be aligned to this */

static intptr_t
align_size(intptr_t x)
{ return x % size_alignment ? (x / size_alignment + 1) * size_alignment : x;
}

#ifdef MMAP_STACK
#include <sys/mman.h>
#include <fcntl.h>

static int mapfd = -1;			/* File descriptor used for mapping */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return a file descriptor to a file, open  for  reading  and  holding  at
least  one  page of 0's. On some systems /dev/zero is available for this
trick.  If not, a file of one page is created under the name /tmp/pl-map
if it does not already exists and this file is opened for  reading.   It
can  be  shared  by  many  SWI-Prolog  processes  and (therefore) is not
removed on exit.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif

#ifdef SGIMMAP
#define MAPCOMMON  (MAP_PRIVATE|MAP_AUTOGROW|MAP_AUTORESRV)
#define MAPPROTECT (PROT_READ|PROT_WRITE)
#else
#define MAPCOMMON (MAP_PRIVATE|MAP_NORESERVE)
#define MAPPROTECT (PROT_NONE)
#endif

#ifdef HAVE_MAP_ANON
#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
#define MAP_ANON MAP_ANONYMOUS
#endif

#define MAP_FLAGS (MAP_ANON|MAPCOMMON)
#define get_map_fd() (-1)

#else /*HAVE_MAP_ANON*/

#define MAP_FLAGS (MAPCOMMON)

static int
get_map_fd()
{ int fd;
  static char *map = "/tmp/pl-map";

  if ( (fd = open("/dev/zero", O_RDONLY)) >= 0 )
    return fd;

  if ( (fd = open(map, O_RDONLY)) < 0 )
  { if ( errno == ENOENT )
    { char buf[1024];
      char *s;
      int n;
      int oldmask = umask(0);

      if ( (fd = open(map, O_RDWR|O_CREAT, 0666)) < 0 )
      { fatalError("Can't create map file %s: %s", map, OsError());
        return -1;
      }
      umask(oldmask);
      for(n=1024, s = buf; n > 0; n--)
        *s++ = EOS;
      for(n=size_alignment/1024; n > 0; n--)
      { if ( write(fd, buf, 1024) != 1024 )
          fatalError("Failed to create map file %s: %s\n", map, OsError());
      }

      return fd;
    }
    fatalError("Can't open map file %s: %s", map, OsError());
    return -1;
  }

  return fd;
}
#endif /*HAVE_MAP_ANON*/


static void
mapOrOutOf(Stack s)
{ uintptr_t incr;
  intptr_t  newroom;

  if ( s->top > s->max )
    incr = ROUND(((uintptr_t)s->top - (uintptr_t)s->max), size_alignment);
  else
    incr = size_alignment;

  newroom = (uintptr_t)s->limit - ((uintptr_t)s->max + incr);
  if ( newroom < 0 )
    outOfStack(s, STACK_OVERFLOW_FATAL);

#ifndef SGIMMAP
  if ( mprotect(s->max, incr, PROT_READ|PROT_WRITE) < 0 )
    fatalError("mprotect() failed at 0x%x for %d bytes: %s\n",
	       s->max, incr, OsError());
#endif /*SGIMMAP*/

  DEBUG(1, Sdprintf("Expanded %s stack with %d bytes from %p\n",
		    s->name, incr, s->max));

  s->max = addPointer(s->max, incr);

  if ( newroom < STACK_SIGNAL )
  { if ( newroom < STACK_RESERVE )
    {
#ifdef O_SEGV_HANDLING
      LD->current_signal = SIGSEGV;
#endif
      outOfStack(s, STACK_OVERFLOW_THROW);
    } else
      outOfStack(s, STACK_OVERFLOW_SIGNAL);
  }

  considerGarbageCollect(s);
}


#ifndef O_SEGV_HANDLING
void
ensureRoomStack(Stack s, size_t bytes)
{ while((char *)s->max <= &((char *)s->top)[bytes])
    mapOrOutOf(s);
}
#endif


static void
unmap(Stack s)
{ caddress top  = (s->top > s->min ? s->top : s->min);
  caddress addr = (caddress) align_size((intptr_t) top + size_alignment);

  if ( addr < s->max )
  { intptr_t len = (char *)s->max - (char *)addr;

#ifdef MAP_FIXED
    munmap(addr, len);
    if ( mmap(addr, len, MAPPROTECT, MAP_FIXED|MAP_FLAGS, mapfd, 0L) != addr )
      fatalError("Failed to remap 0x%x bytes at %p: %s",
		 len, addr, OsError());
#else
    if ( mprotect(addr, len, PROT_NONE) != 0 )
      fatalError("Failed to mprotect(%p, %d, PROT_NONE): %s",
		 addr, len, OsError());
#endif

    s->max = addr;
  }
}

/* mmap() version */

static int
allocStacks(intptr_t local, intptr_t global, intptr_t trail, intptr_t argument)
{ caddress lbase, gbase, tbase, abase;
  intptr_t glsize;
  intptr_t minglobal   = 4*SIZEOF_VOIDP K;
  intptr_t minlocal    = 2*SIZEOF_VOIDP K;
  intptr_t mintrail    = 2*SIZEOF_VOIDP K;
  intptr_t minargument = 1*SIZEOF_VOIDP K;
  
  size_alignment = getpagesize();
  while(size_alignment < 4*SIZEOF_VOIDP K)
    size_alignment *= 2;

#ifndef HAVE_MAP_ANON
  PL_LOCK(L_MISC);
  if ( mapfd < 0 )
    mapfd = get_map_fd();
  PL_UNLOCK(L_MISC);
#endif

  local    = max(local,    minlocal + STACK_SIGNAL);
  global   = max(global,   minglobal + STACK_SIGNAL);
  trail    = max(trail,    mintrail + STACK_SIGNAL);
  argument = max(argument, minargument + STACK_SIGNAL);

  local    = (intptr_t) align_size(local);	/* Round up to page boundary */
  global   = (intptr_t) align_size(global);
  trail    = (intptr_t) align_size(trail);
  argument = (intptr_t) align_size(argument);
  glsize   = global+local;

  tbase = mmap(NULL, trail,    MAPPROTECT, MAP_FLAGS, mapfd, 0L);
  abase = mmap(NULL, argument, MAPPROTECT, MAP_FLAGS, mapfd, 0L);
  gbase = mmap(NULL, glsize,   MAPPROTECT, MAP_FLAGS, mapfd, 0L);
  lbase = addPointer(gbase, global);

  if ( tbase == MAP_FAILED || abase == MAP_FAILED || gbase == MAP_FAILED )
  { if ( tbase != MAP_FAILED )
      munmap(tbase, trail);
    if ( abase != MAP_FAILED )
      munmap(abase, argument);
    if ( gbase != MAP_FAILED )
      munmap(gbase, glsize);

    fail;
  }

#define INIT_STACK(name, print, base, limit, minsize) \
  DEBUG(1, Sdprintf("%s stack at 0x%x; size = %ld\n", print, base, limit)); \
  init_stack((Stack) &LD->stacks.name, print, base, limit, minsize);
#define K * 1024

  INIT_STACK(global,   "global",   gbase, global,   minglobal);
  INIT_STACK(local,    "local",    lbase, local,    minlocal);
  INIT_STACK(trail,    "trail",    tbase, trail,    mintrail);
  INIT_STACK(argument, "argument", abase, argument, minargument);

#ifdef O_SEGV_HANDLING
  set_stack_guard_handler(SIGSEGV, _PL_segv_handler);
#endif
  
  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Free stacks for the current Prolog thread
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
freeStacks(ARG1_LD)
{ intptr_t tlen, alen, gllen;

#undef LD
#define LD LOCAL_LD
  tlen  = limitStack(trail);
  alen  = limitStack(argument);
  gllen = limitStack(global) + limitStack(local);

  munmap(tBase, tlen);
  munmap(aBase, alen);
  munmap(gBase, gllen);
#undef LD
#define LD GLOBAL_LD
}


#endif /* MMAP_STACK */


#ifdef HAVE_VIRTUALALLOC

#undef FD_ZERO
#undef FD_ISSET
#undef FD_SET
#include <windows.h>
#undef small

static void
mapOrOutOf(Stack s)
{ uintptr_t incr;
  intptr_t  newroom;

  if ( s->top > s->max )
    incr = ROUND(((uintptr_t)s->top - (uintptr_t)s->max), size_alignment);
  else
    incr = size_alignment;

  newroom = (uintptr_t)s->limit - ((uintptr_t)s->max + incr);
  if ( newroom < 0 )
    outOfStack(s, STACK_OVERFLOW_FATAL);

  if ( VirtualAlloc(s->max, incr,
		    MEM_COMMIT, PAGE_READWRITE ) != s->max )
    fatalError("VirtualAlloc() failed at 0x%x for %d bytes: %d\n",
	       s->max, incr, GetLastError());

  DEBUG(1, Sdprintf("mapped %d bytes from 0x%x to 0x%x\n",
		    incr, (unsigned) s->max,
		    (uintptr_t) s->max + size_alignment));

  s->max = addPointer(s->max, incr);

  if ( newroom <= STACK_SIGNAL )
  { if ( newroom <= STACK_RESERVE )
    {
#ifdef O_SEGV_HANDLING
      LD->current_signal = SIGSEGV;
#endif
      outOfStack(s, STACK_OVERFLOW_THROW);
    } else
      outOfStack(s, STACK_OVERFLOW_SIGNAL);
  }

  considerGarbageCollect(s);
}


#ifndef O_SEGV_HANDLING
void
ensureRoomStack(Stack s, size_t bytes)
{ while((char *)s->max <= &((char *)s->top)[bytes])
    mapOrOutOf(s);
}
#endif

static void
unmap(Stack s)
{ caddress top  = (s->top > s->min ? s->top : s->min);
  caddress addr = (caddress) align_size((intptr_t) top + size_alignment);

  if ( addr < s->max )
  { if ( !VirtualFree(addr, (uintptr_t)s->max - (uintptr_t)addr, MEM_DECOMMIT) )
      fatalError("Failed to unmap memory: %d", GetLastError());
    s->max = addr;
  }
}


/* Windows VirtualAlloc() version */

static int
allocStacks(intptr_t local, intptr_t global, intptr_t trail, intptr_t argument)
{ caddress lbase, gbase, tbase, abase;
  intptr_t glsize;
  SYSTEM_INFO info;
  intptr_t minglobal   = 4*SIZEOF_VOIDP K;
  intptr_t minlocal    = 2*SIZEOF_VOIDP K;
  intptr_t mintrail    = 2*SIZEOF_VOIDP K;
  intptr_t minargument = 1*SIZEOF_VOIDP K;

  GetSystemInfo(&info);
  size_alignment = info.dwPageSize;

  local    = max(local,    minlocal + STACK_SIGNAL);
  global   = max(global,   minglobal + STACK_SIGNAL);
  trail    = max(trail,    mintrail + STACK_SIGNAL);
  argument = max(argument, minargument + STACK_SIGNAL);

  local    = (intptr_t) align_size(local);	/* Round up to page boundary */
  global   = (intptr_t) align_size(global);
  trail    = (intptr_t) align_size(trail);
  argument = (intptr_t) align_size(argument);
  glsize   = global+local;

  tbase = VirtualAlloc(NULL, trail,    MEM_RESERVE, PAGE_READWRITE);
  abase = VirtualAlloc(NULL, argument, MEM_RESERVE, PAGE_READWRITE);
  gbase = VirtualAlloc(NULL, glsize,   MEM_RESERVE, PAGE_READWRITE);
  lbase = addPointer(gbase, global);

  if ( !tbase || !abase || !gbase )
  { if ( !tbase )
      VirtualFree(tbase, 0, MEM_RELEASE);
    if ( !abase )
      VirtualFree(abase, 0, MEM_RELEASE);
    if ( !gbase )
      VirtualFree(gbase, 0, MEM_RELEASE);

    fail;
  }

#define INIT_STACK(name, print, base, limit, minsize) \
  DEBUG(1, Sdprintf("%s stack at 0x%x; size = %ld\n", print, base, limit)); \
  init_stack((Stack) &LD->stacks.name, print, base, limit, minsize);
#define K * 1024

  INIT_STACK(global,   "global",   gbase, global,   minglobal);
  INIT_STACK(local,    "local",    lbase, local,    minlocal);
  INIT_STACK(trail,    "trail",    tbase, trail,    mintrail);
  INIT_STACK(argument, "argument", abase, argument, minargument);

#ifdef O_SEGV_HANDLING
  set_stack_guard_handler(SIGSEGV, _PL_segv_handler);
#endif

  succeed;
}


static void
freeStack(Stack s)
{ VirtualFree(s->base, (uintptr_t)s->max-(uintptr_t)s->base, MEM_DECOMMIT);
}


void
freeStacks(PL_local_data_t *ld)
{ DEBUG(1, Sdprintf("[%d]: freeStacks()\n", PL_thread_self()));

  freeStack((Stack)&ld->stacks.global);	/* Region must be entirely committed */
  freeStack((Stack)&ld->stacks.local);	/* or decommitted */
  freeStack((Stack)&ld->stacks.trail);
  freeStack((Stack)&ld->stacks.argument);

  if ( !VirtualFree(ld->stacks.global.base, 0, MEM_RELEASE) ||
       !VirtualFree(ld->stacks.trail.base, 0, MEM_RELEASE) ||
       !VirtualFree(ld->stacks.argument.base, 0, MEM_RELEASE) )
    Sdprintf("Failed to release stacks\n");
}

#endif /*HAVE_VIRTUALALLOC*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This the the signal handler for segmentation  faults if we are using MMU
controlled stacks. It can operate in two modes. If the signal handler is
not passed an address we will increase all stack areas that are close to
exhaution. If an address is passed, we  will locate the proper stack and
extend that. Currently we check for   the  POSIX.1b SA_SIGINFO handling,
supported at least by recent Linux and Solaris.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef USE_SIGINFO
static bool
expandStack(Stack s, void *addr)
{ if ( addr < s->max || addr >= addPointer(s->limit, STACK_SEPARATION) )
    fail;				/* outside this area */

  if ( addr <= s->max + STACK_SEPARATION*2 )
  { mapOrOutOf(s);

    succeed;
  }

  fail;
}
#endif /*USE_SIGINFO*/

#ifdef O_SEGV_HANDLING
#warning "SEGV guarded stacks are no longer supported"
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code is left here for future use,   but  it no longer works. If you
want to make it working, please  remember   that  the  stacks may now be
allocated without a free page at the end, so make sure to leave the last
page blank in the signal handler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

RETSIGTYPE
#ifdef USE_SIGINFO
_PL_segv_handler(int sig, siginfo_t *info, void *extra)
#else
_PL_segv_handler(int sig)
#endif
{ Stack stacka = (Stack) &LD->stacks;
  int mapped = 0;
  int i;

#ifdef USE_SIGINFO
  void *addr;

  if ( info && info->si_addr )
  { addr = info->si_addr;

    DEBUG(0, Sdprintf("Page fault at %p\n", addr));
    for(i=0; i<N_STACKS; i++)
    { if ( expandStack(&stacka[i], addr) )
	return;
    }

    pl_signal_handler(sig);
    return;
  }
#endif

  DEBUG(1, Sdprintf("Page fault.  Free room (g+l+t) = %ld+%ld+%ld\n",
		    roomStack(global), roomStack(local), roomStack(trail)));

  for(i=0; i<N_STACKS; i++)
  { intptr_t r = (uintptr_t)stacka[i].max - (uintptr_t)stacka[i].top;

    if ( r < size_alignment )
    { DEBUG(1, Sdprintf("Mapped %s stack (free was %d)\n", stacka[i].name, r));
      mapOrOutOf(&stacka[i]);
      mapped++;
    }
  }

  if ( mapped )
    return;

  pl_signal_handler(sig);
}

#endif /*O_SEGV_HANDLING*/

static void
init_stack(Stack s, char *name, void *base, intptr_t limit, intptr_t minsize)
{ s->name       = name;
  s->base       = s->max = s->top = base;
  s->limit	= addPointer(base, limit);
  s->min        = addPointer(base, minsize);
  s->gced_size  = 0L;			/* size after last gc */
  gcPolicy(s, GC_FAST_POLICY);

  DEBUG(1, Sdprintf("%-8s stack from 0x%08x to 0x%08x\n",
		    s->name, (uintptr_t)s->base, (uintptr_t)s->limit));

  while(s->max < s->min)
    mapOrOutOf(s);
}

void
resetStacks()
{ emptyStacks();

  trimStacks(PASS_LD1);
}


		/********************************
		*     STACK TRIMMING & LIMITS   *
		*********************************/

static void
gcPolicy(Stack s, int policy)
{ s->gc = ((s == (Stack) &LD->stacks.global ||
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


#else /* O_DYNAMIC_STACKS */


		/********************************
		*    SIMPLE STACK ALLOCATION    *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On systems that do not allow us to get access to the MMU (or that do not
have an MMU)  the  stacks  have  fixed  size  and  overflow  checks  are
implemented  in  software.   The stacks are allocated using malloc(). If
you malloc() does not allow you to get more than 64K bytes in one go you
better start looking for another Prolog system (IBM-PC  is  an  example:
why does IBM bring computers on the marked that are 10 years out-of-date
at the moment of announcement?).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_stack_parameter(term_t name, term_t key, term_t old, term_t new)
{ atom_t a, k;
  Stack stack = NULL;
  intptr_t *value = NULL;

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

  if ( PL_get_atom(key, &k) )
  { if ( k == ATOM_min_free )
      value = &stack->minfree;
  }
  if ( !value )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_stack_parameter, key);

  if ( !PL_unify_int64(old, *value) ||
       !PL_get_intptr_ex(new, value) )
    fail;

  succeed;
}


static void
init_stack(Stack s, char *name, intptr_t size, intptr_t limit, intptr_t minfree)
{ s->name 	= name;
  s->top	= s->base;
  s->limit	= addPointer(s->base, limit);
  s->max	= addPointer(s->base, size);
  s->minfree	= minfree;
  s->gced_size  = 0L;			/* size after last gc */
  s->gc	        = ((s == (Stack) &LD->stacks.global ||
		    s == (Stack) &LD->stacks.trail) ? TRUE : FALSE);
  s->small      = (s->gc ? SMALLSTACK : 0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Malloc'ed version of the stacks. Our initial  stack size is a bit bigger
than the minimum to avoid a stack-shift right at the start, We must find
a better way to specify the minimum.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
allocStacks(intptr_t local, intptr_t global, intptr_t trail, intptr_t argument)
{ intptr_t minglobal   = 25*SIZEOF_VOIDP K;
  intptr_t minlocal    = 4*SIZEOF_VOIDP K;
  intptr_t mintrail    = 4*SIZEOF_VOIDP K;
  intptr_t minargument = 1*SIZEOF_VOIDP K;

#if O_SHIFT_STACKS
  intptr_t itrail  = nextStackSizeAbove(mintrail);
  intptr_t iglobal = nextStackSizeAbove(minglobal);
  intptr_t ilocal  = nextStackSizeAbove(minlocal);
#else
  intptr_t itrail  = trail;
  intptr_t iglobal = global;
  intptr_t ilocal  = local;
#endif

  local    = max(local,    minlocal);
  global   = max(global,   minglobal);
  trail    = max(trail,    mintrail);
  argument = max(argument, minargument);

  gBase = (Word) malloc(iglobal + ilocal);
  tBase = (TrailEntry) malloc(itrail);
  aBase = (Word *)     malloc(argument);
  if ( !gBase || !tBase || !aBase )
  { freeStacks(LD);
    fail;
  }

  lBase = (LocalFrame) addPointer(gBase, iglobal);

  init_stack((Stack)&LD->stacks.global,
	     "global",   iglobal, global,  minglobal);
  init_stack((Stack)&LD->stacks.local,
	     "local",    ilocal,  local,   minlocal);
  init_stack((Stack)&LD->stacks.trail,
	     "trail",    itrail,  trail,   mintrail);
  init_stack((Stack)&LD->stacks.argument,
	     "argument", argument, argument, minargument);

  succeed;
}


void
freeStacks(ARG1_LD)
{
#undef LD
#define LD LOCAL_LD
  if ( gBase ) { free(gBase); gBase = NULL; lBase = NULL; }
  if ( tBase ) { free(tBase); tBase = NULL; }
  if ( aBase ) { free(aBase); aBase = NULL; }
#undef LD
#define LD GLOBAL_LD
}


void
resetStacks()
{ emptyStacks();
}

#endif /* O_DYNAMIC_STACKS */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
trimStacks() reclaims all unused space on the stack. Note that the trail
can have references to unused stack. We set the references to point to a
dummy variable, so no harm  will  be   done.  Setting  it  to NULL would
require a test in Undo(), which   is time-critical. trim_stacks normally
isn't. This precaution is explicitly requires  for the trimStacks() that
result from a stack-overflow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
trimStacks(ARG1_LD)
{
#undef LD
#define LD LOCAL_LD
  TrailEntry te;
  Word dummy = NULL;

#ifdef O_SHIFT_STACKS
  if ( !growStacks(NULL, NULL, NULL, -1, -1, -1) )
    return;
#else
#ifdef O_DYNAMIC_STACKS
  unmap((Stack) &LD->stacks.local);
  unmap((Stack) &LD->stacks.global);
  unmap((Stack) &LD->stacks.trail);
  unmap((Stack) &LD->stacks.argument);
#endif /*O_DYNAMIC_STACKS*/
#endif /*O_SHIFT_STACKS*/

  LD->stacks.global.gced_size = usedStack(global);
  LD->stacks.trail.gced_size  = usedStack(trail);

  for(te = tTop; --te >= tBase; )
  { Word p = te->address;
    
    if ( isTrailVal(p) )
      continue;

    if ( !onStack(local, p) && !onStack(global, p) )
    { if ( !dummy )
      { dummy = allocGlobal(1);
	setVar(*dummy);
      }

      te->address = dummy;
    }
  }
#undef LD
#define LD GLOBAL_LD
}


word
pl_trim_stacks()
{ trimStacks(PASS_LD1);

#ifdef O_DYNAMIC_STACKS
  gcPolicy((Stack) &LD->stacks.global, GC_FAST_POLICY);
  gcPolicy((Stack) &LD->stacks.trail,  GC_FAST_POLICY);
#endif

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
freeLocalData(PL_local_data_t *ld)
{ int i;

  discardBuffer(&ld->fli._discardable_buffer);

  for(i=0; i<BUFFER_RING_SIZE; i++)
    discardBuffer(&ld->fli._buffer_ring[i]);

  if ( ld->comp.vardefs )
  { free(ld->comp.vardefs);

    memset(&ld->comp, 0, sizeof(ld->comp));
  }

#ifdef O_GVAR
  if ( ld->gvar.nb_vars )
    destroyHTable(ld->gvar.nb_vars);
#endif

#ifdef O_CYCLIC
  clearSegStack(&ld->cycle.stack);
#endif

  if ( ld->arith.stack.base )
    PL_free(ld->arith.stack.base);
}
