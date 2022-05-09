/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2021, University of Amsterdam,
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

/* #define O_DEBUG 1 */

#define _GNU_SOURCE 1			/* get recursive mutex stuff to */
					/* compile clean with glibc.  Can */
					/* this do any harm? */
#ifdef __WINDOWS__
#include <winsock2.h>
#include <windows.h>
#include <errno.h>			/* must be before pl-incl.h */
#endif

#if __MINGW32__
#define __try
#define __except(_) if (0)
#define __finally
#endif

#include "pl-incl.h"
#include "pl-thread.h"
#include "pl-tabling.h"
#include "pl-undo.h"
#include "os/pl-cstack.h"
#include "pl-prof.h"
#include "pl-event.h"
#include "pl-comp.h"
#include "pl-setup.h"
#include "pl-fli.h"
#include "pl-wam.h"
#include "pl-pro.h"
#include "pl-trace.h"
#include "pl-rec.h"
#include "pl-index.h"
#include "pl-proc.h"
#include "pl-modul.h"
#include "pl-util.h"
#include "pl-prims.h"
#include "pl-supervisor.h"
#include <stdio.h>
#include <math.h>

#if __WINDOWS__				/* this is a stub.  Should be detected */
#undef HAVE_PTHREAD_SETNAME_NP		/* in configure.ac */
#endif

#ifdef O_PLMT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			   MULTITHREADING SUPPORT

APPROACH
========

    * Prolog threads are C-threads
      Prolog multi-threading is based upon a C thread library.  At first,
      we will concentrate on the Posix pthread standard, due to its wide
      availability especially on Unix systems.

      This approach has some clear advantages: clean mixing of thread-based
      foreign code and portability.

    * Data
      All global data that cannot be removed is split into three large
      structures:

	+ PL_code_data
	  This structure contains `code' data: data that is set up at
	  initialisation time and never changed afterwards.
	  PL_initialise() initialises this and no further precautions
	  are needed.

	+ PL_global_data
	  This structure contains all global data required for the
	  Prolog `heap'.  This data is shared between threads and
	  access should be properly synchronised.

	+ PL_local_data
	  This structure contains the thread-local data.  If a new
	  Prolog engine is initialised in a thread, a new copy of this
	  structure is allocated and initialised.

	  For compatibility reasons, we cannot pass this pointer around
	  as an argument between all functions in the system.  We will
	  locate it through the thread-id using a function.  Any function
	  requiring frequent access can fetch this pointer once at
	  start-up.  Cooperating functions can pass this pointer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <errno.h>
#if defined(__linux__)
#include <syscall.h>
#ifdef HAVE_GETTID_MACRO
_syscall0(pid_t,gettid)
#endif
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#ifdef HAVE_SYS_SYSCALL_H
#include <sys/syscall.h>
#endif

#ifdef HAVE_SYS_CPUSET_H
#include <sys/param.h>         /* pulls sys/cdefs.h and sys/types.h for sys/cpuset.h */
#include <sys/cpuset.h>        /* CPU_ZERO(), CPU_SET, cpuset_t */
#endif
#ifdef HAVE_PTHREAD_NP_H
#include <pthread_np.h>        /* pthread_*_np */
#endif
#ifdef HAVE_CPUSET_T
typedef cpuset_t cpu_set_t;
#endif

#ifdef HAVE_SEMA_INIT			/* Solaris */
#include <synch.h>

typedef sema_t sem_t;
#define sem_trywait(s)	sema_trywait(s)
#define sem_destroy(s)	sema_destroy(s)
#define sem_post(s)	sema_post(s)
#define sem_init(s, type, cnt) sema_init(s, cnt, type, NULL)

#else /*HAVE_SEMA_INIT*/
#include <semaphore.h>

#ifndef USYNC_THREAD
#define USYNC_THREAD 0
#endif

#endif /*HAVE_SEMA_INIT*/

#ifdef USE_SEM_OPEN			/* see below */
static sem_t *sem_canceled_ptr;
#else
static sem_t sem_canceled;		/* used on halt */
#define sem_canceled_ptr (&sem_canceled)
#endif

#ifndef __WINDOWS__
#include <signal.h>

#ifdef USE_SEM_OPEN

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Apple Darwin (6.6) only contains the sem_init()   function as a stub. It
only provides named semaphores  through   sem_open().  These defines and
my_sem_open() try to hide the details of   this as much as possible from
the rest of the code. Note  that   we  unlink  the semaphore right after
creating it, using the common Unix trick to keep access to it as long as
we do not close it. We assume  the   OS  will close the semaphore as the
application terminates. All this is highly   undesirable, but it will do
for now. The USE_SEM_OPEN define  is  set   by  configure  based  on the
substring "darwin" in the architecture identifier.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define sem_init(ptr, flags, val) my_sem_open(&ptr, val)
#define sem_destroy(ptr)	  ((void)0)

static int
my_sem_open(sem_t **ptr, unsigned int val)
{ if ( !*ptr )
  { sem_t *sem = sem_open("pl", O_CREAT|O_EXCL, 0600, val);

    DEBUG(MSG_THREAD, Sdprintf("sem = %p\n", sem));

    if ( sem == NULL )
    { perror("sem_open");
      exit(1);
    }

    *ptr = sem;

    sem_unlink("pl");
  }

  return 0;
}

#endif /*USE_SEM_OPEN*/

#ifndef SA_RESTART
#define SA_RESTART 0
#endif
#endif /*!__WINDOWS__*/

#ifdef __WINDOWS__
/* Deal with different versions of the windows thread library
*/

static HANDLE
get_windows_thread(PL_thread_info_t *info)
{
#ifdef HAVE_PTHREAD_GETW32THREADHANDLE_NP
  HANDLE wt = NULL;
  __try
  { wt = pthread_getw32threadhandle_np(info->tid);
  } __except(EXCEPTION_EXECUTE_HANDLER)
  return wt;
#else
  return OpenThread(THREAD_ALL_ACCESS, FALSE, info->w32id);
#endif
}

static void
close_windows_thread(HANDLE wt)
{
#ifndef HAVE_PTHREAD_GETW32THREADHANDLE_NP
  CloseHandle(wt);
#endif
}
#endif /*__WINDOWS__*/


		 /*******************************
		 *	    GLOBAL DATA		*
		 *******************************/

static Table threadTable;		/* name --> reference symbol */
static int threads_ready = FALSE;	/* Prolog threads available */
static Table queueTable;		/* name --> queue */
static simpleMutex queueTable_mutex;	/* GC synchronization */
static int will_exec;			/* process will exec soon */

#ifdef HAVE___THREAD
__thread PL_local_data_t *GLOBAL_LD;
#else
TLD_KEY PL_ldata;			/* key for thread PL_local_data */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The global mutexes. Most are using  within   a  module and their name is
simply the module-name. The idea is that   a module holds a coherent bit
of data that needs a mutex for all operations.

Some remarks:

    L_MISC
	General-purpose mutex.  Should only be used for simple, very
	local tasks and may not be used to lock anything significant.

    __WINDOWS__
	We use native windows CRITICAL_SECTIONS for mutexes here to
	get the best performance, notably on single-processor hardware.
	This is selected in pl-mutex.h based on the macro
	USE_CRITICAL_SECTIONS

	Unfortunately critical sections have no static initialiser,
	so we need something called before anything else happens.  This
	can only be DllMain().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef USE_CRITICAL_SECTIONS
#undef PTHREAD_MUTEX_INITIALIZER
#define PTHREAD_MUTEX_INITIALIZER {0}
#endif

#define COUNT_MUTEX_INITIALIZER(name) \
 { PTHREAD_MUTEX_INITIALIZER, \
   name, \
   0L \
 }

/* NOTE: These must be kept in sequence such that they align with
   the #defines for L_* in pl-thread.h
*/

counting_mutex _PL_mutexes[] =
{ COUNT_MUTEX_INITIALIZER("L_MISC"),		/* 0 */
  COUNT_MUTEX_INITIALIZER("L_ALLOC"),
  COUNT_MUTEX_INITIALIZER("L_REHASH_ATOMS"),
  COUNT_MUTEX_INITIALIZER("L_FLAG"),
  COUNT_MUTEX_INITIALIZER("L_FUNCTOR"),
  COUNT_MUTEX_INITIALIZER("L_RECORD"),
  COUNT_MUTEX_INITIALIZER("L_THREAD"),
  COUNT_MUTEX_INITIALIZER("L_MUTEX"),
  COUNT_MUTEX_INITIALIZER("L_PREDICATE"),
  COUNT_MUTEX_INITIALIZER("L_MODULE"),
  COUNT_MUTEX_INITIALIZER("L_SRCFILE"),		/* 10 */
  COUNT_MUTEX_INITIALIZER("L_TABLE"),
  COUNT_MUTEX_INITIALIZER("L_BREAK"),
  COUNT_MUTEX_INITIALIZER("L_FILE"),
  COUNT_MUTEX_INITIALIZER("L_SEETELL"),
  COUNT_MUTEX_INITIALIZER("L_PLFLAG"),
  COUNT_MUTEX_INITIALIZER("L_OP"),
  COUNT_MUTEX_INITIALIZER("L_INIT"),
  COUNT_MUTEX_INITIALIZER("L_TERM"),
  COUNT_MUTEX_INITIALIZER("L_FOREIGN"),
  COUNT_MUTEX_INITIALIZER("L_OS"),		/* 20 */
  COUNT_MUTEX_INITIALIZER("L_LOCALE"),
  COUNT_MUTEX_INITIALIZER("L_SORTR"),
  COUNT_MUTEX_INITIALIZER("L_UMUTEX"),
  COUNT_MUTEX_INITIALIZER("L_INIT_ATOMS"),
  COUNT_MUTEX_INITIALIZER("L_CGCGEN"),
  COUNT_MUTEX_INITIALIZER("L_EVHOOK"),
  COUNT_MUTEX_INITIALIZER("L_OSDIR"),
  COUNT_MUTEX_INITIALIZER("L_ALERT"),
  COUNT_MUTEX_INITIALIZER("L_GENERATION")
#ifdef __WINDOWS__
, COUNT_MUTEX_INITIALIZER("L_DDE")
, COUNT_MUTEX_INITIALIZER("L_CSTACK")
#endif
};


static void
link_mutexes(void)
{ counting_mutex *m;
  int n = sizeof(_PL_mutexes)/sizeof(*m);
  int i;

  GD->thread.mutexes = _PL_mutexes;
  for(i=0, m=_PL_mutexes; i<n-1; i++, m++)
    m->next = m+1;
}


#ifdef USE_CRITICAL_SECTIONS

static void
W32initMutexes(void)
{ static int done = FALSE;
  counting_mutex *m;
  int n = sizeof(_PL_mutexes)/sizeof(*m);
  int i;

  if ( done )
    return;
  done = TRUE;

  for(i=0, m=_PL_mutexes; i<n; i++, m++)
    simpleMutexInit(&m->mutex);
}

static void
deleteMutexes()
{ counting_mutex *m;
  int n = sizeof(_PL_mutexes)/sizeof(*m);
  int i;

  for(i=0, m=_PL_mutexes; i<n; i++, m++)
    simpleMutexDelete(&m->mutex);
}


#ifdef O_SHARED_KERNEL

BOOL WINAPI
DllMain(HINSTANCE hinstDll, DWORD fdwReason, LPVOID lpvReserved)
{ BOOL result = TRUE;

  switch(fdwReason)
  { case DLL_PROCESS_ATTACH:
      GD->thread.instance = hinstDll;
      W32initMutexes();
#ifndef HAVE___THREAD
      TLD_alloc(&PL_ldata);
#endif
      break;
    case DLL_PROCESS_DETACH:
      deleteMutexes();
#ifndef HAVE___THREAD
      TLD_free(&PL_ldata);
#endif
      break;
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
      break;
  }

  return result;
}

#endif /*O_SHARED_KERNEL*/

#endif /*USE_CRITICAL_SECTIONS*/

static
PRED_IMPL("mutex_statistics", 0, mutex_statistics, 0)
{ PRED_LD
  counting_mutex *cm;
  IOSTREAM *s = Scurout;

#ifdef O_CONTENTION_STATISTICS
  Sfprintf(s, "Name                                                       locked collisions\n"
	      "----------------------------------------------------------------------------\n");
#else
  Sfprintf(s, "Name                               locked\n"
	      "-----------------------------------------\n");
#endif
  PL_LOCK(L_MUTEX);
  for(cm = GD->thread.mutexes; cm; cm = cm->next)
  { int lc;

    if ( cm->count == 0 )
      continue;

    Sfprintf(s, "%-56Us %8d", cm->name, cm->count); /* %Us: UTF-8 string */
#ifdef O_CONTENTION_STATISTICS
    Sfprintf(s, " %8d", cm->collisions);
#endif
    lc = (cm == &_PL_mutexes[L_MUTEX] ? 1 : 0);

    if ( cm->lock_count > lc )
      Sfprintf(s, " LOCKS: %d\n", cm->lock_count - lc);
    else
      Sfprintf(s, "\n");
  }
  PL_UNLOCK(L_MUTEX);

  succeed;
}


#ifdef PTW32_STATIC_LIB
static void
win_thread_initialize(void)
{ static int done = FALSE;

  if ( done )
    return;
  done = TRUE;
  ptw32_processInitialize();
}
#endif

		 /*******************************
		 *	  LOCAL PROTOTYPES	*
		 *******************************/

#if USE_LD_MACROS
#define	get_message_queue_unlocked(t, queue)	LDFUNC(get_message_queue_unlocked, t, queue)
#define	get_message_queue(t, queue)		LDFUNC(get_message_queue, t, queue)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static PL_thread_info_t *alloc_thread(void);
static void	destroy_message_queue(message_queue *queue);
static void	destroy_thread_message_queue(message_queue *queue);
static void	init_message_queue(message_queue *queue, size_t max_size);
static size_t	sizeof_message_queue(message_queue *queue);
static size_t	sizeof_local_definitions(PL_local_data_t *ld);
static void	freeThreadSignals(PL_local_data_t *ld);
static thread_handle *create_thread_handle(PL_thread_info_t *info);
static void	free_thread_info(PL_thread_info_t *info);
static void	set_system_thread_id(PL_thread_info_t *info);
static thread_handle *symbol_thread_handle(atom_t a);
static void	destroy_interactor(thread_handle *th, int gc);
static PL_engine_t PL_current_engine(void);
static void	detach_engine(PL_engine_t e);

static int	unify_queue(term_t t, message_queue *q);
static int	get_message_queue_unlocked(term_t t, message_queue **queue);
static int	get_message_queue(term_t t, message_queue **queue);
static void	release_message_queue(message_queue *queue);
static void	initMessageQueues(void);
static int	get_thread(term_t t, PL_thread_info_t **info, int warn);
static int	is_alive(int status);
static void	init_predicate_references(PL_local_data_t *ld);
static int	ldata_in_use(PL_local_data_t *ld);
#ifdef O_C_BACKTRACE
static void	print_trace(int depth);
#else
#define		print_trace(depth) (void)0
#endif

static void timespec_diff(struct timespec *diff,
			  const struct timespec *a, const struct timespec *b);
static int  timespec_sign(const struct timespec *t);

#undef LDFUNC_DECLARATIONS

		 /*******************************
		 *	     LOCAL DATA		*
		 *******************************/

#undef LD
#define LD LOCAL_LD


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static char *
ThError(int e)
{ return strerror(e);
}


		 /*******************************
		 *     RUNTIME ENABLE/DISABLE	*
		 *******************************/

int
enableThreads(int enable)
{ if ( enable )
  { GD->thread.enabled = TRUE;		/* print system message? */
  } else
  { PL_LOCK(L_THREAD);
    if ( GD->statistics.threads_created -
	 GD->statistics.threads_finished == 1 ) /* I am alone :-( */
    { GD->thread.enabled = FALSE;
    } else
    { GET_LD
      term_t key = PL_new_term_ref();

      PL_put_atom(key, ATOM_threads);

      PL_UNLOCK(L_THREAD);
      return PL_error(NULL, 0, "Active threads",
		      ERR_PERMISSION,
		      ATOM_modify, ATOM_flag, key);
    }
    PL_UNLOCK(L_THREAD);
  }

  succeed;
}


		 /*******************************
		 *	 THREAD ALLOCATION	*
		 *******************************/

static int
initialise_thread(PL_thread_info_t *info)
{ assert(info->thread_data);

  TLD_set_LD(info->thread_data);

  if ( !info->stack_limit ) info->stack_limit = GD->options.stackLimit;
  if ( !info->table_space ) info->table_space = GD->options.tableSpace;

  if ( !initPrologStacks(info->stack_limit) )
  { info->status = PL_THREAD_NOMEM;
    TLD_set_LD(NULL);
    return FALSE;
  }

  WITH_LD(info->thread_data) initPrologLocalData();
  info->thread_data->magic = LD_MAGIC;

  initGuardCStack();

  return TRUE;
}


static void
free_local_data(PL_local_data_t *ld)
{ simpleMutexDelete(&ld->thread.scan_lock);
#if STDC_CV_ALERT
  cnd_destroy(&ld->signal.alert_cv);
  mtx_destroy(&ld->signal.alert_mtx);
#endif
  freeHeap(ld, sizeof(*ld));
}

static PL_local_data_t *ld_free_list = NULL;

static void
clean_ld_free_list(void)
{ if ( ld_free_list )
  { PL_local_data_t *ld, **prev = &ld_free_list;
    for(ld = ld_free_list; ld; ld=*prev)
    { if ( !ldata_in_use(ld) )
      { *prev = ld->next_free;
	free_local_data(ld);
      } else
      { prev = &ld->next_free;
      }
    }
  }
}

static void
maybe_free_local_data(PL_local_data_t *ld)
{ if ( !ldata_in_use(ld) )
  { free_local_data(ld);
  } else
  { PL_LOCK(L_THREAD);
    clean_ld_free_list();
    ld->next_free = ld_free_list;
    ld_free_list = ld;
    PL_UNLOCK(L_THREAD);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
free_prolog_thread()
    Called from a cleanup-handler to release all resources associated
    with a thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
freePrologThread(PL_local_data_t *ld, int after_fork)
{ PL_thread_info_t *info;
  int acknowledge;
  double time;
  PL_local_data_t *old_ld;

  { GET_LD
    old_ld = LD;
  }

  if ( !threads_ready )
    return;				/* Post-mortem */

  TLD_set_LD(ld);
  { GET_LD

    info = ld->thread.info;
    DEBUG(MSG_THREAD, Sdprintf("Freeing prolog thread %d (status = %d)\n",
			       info->pl_tid, info->status));

    if ( !after_fork )
    { int rc1, rc2;

      PL_LOCK(L_THREAD);
      if ( info->status == PL_THREAD_RUNNING )
	info->status = PL_THREAD_EXITED;	/* foreign pthread_exit() */
      acknowledge = (ld->exit_requested == EXIT_REQ_PROCESS);
      PL_UNLOCK(L_THREAD);

      if ( ld->stacks.argument.base )		/* are stacks initialized? */
      { WITH_LD(ld) startCritical();
	info->in_exit_hooks = TRUE;
	if ( LD == ld )
	  rc1 = callEventHook(PLEV_THIS_THREAD_EXIT);
	else
	  rc1 = TRUE;
	rc2 = callEventHook(PLEV_THREAD_EXIT, info);
	if ( (!rc1 || !rc2) && exception_term )
	{ Sdprintf("Event hook \"thread_finished\" left an exception\n");
	  PL_write_term(Serror, exception_term, 1200,
			PL_WRT_QUOTED|PL_WRT_NEWLINE);
	  PL_clear_exception();
	}
	info->in_exit_hooks = FALSE;
	WITH_LD(ld) endCritical();	/* TBD: exception? */
      }
    } else
    { acknowledge = FALSE;
      info->detached = TRUE;		/* cleanup */
    }

  #ifdef O_PROFILE
    if ( ld->profile.active )
      WITH_LD(ld) activateProfiler(FALSE);
  #endif

    cleanupLocalDefinitions(ld);

    DEBUG(MSG_THREAD, Sdprintf("Destroying data\n"));
    ld->magic = 0;
    if ( ld->stacks.global.base )		/* otherwise not initialised */
    { simpleMutexLock(&ld->thread.scan_lock);
      WITH_LD(ld) freeStacks();
      simpleMutexUnlock(&ld->thread.scan_lock);
    }
    freePrologLocalData(ld);

    /*PL_unregister_atom(ld->prompt.current);*/

    freeThreadSignals(ld);
    time = info->is_engine ? 0.0 : ThreadCPUTime(PASS_AS_LD(ld) CPU_USER);

    if ( !after_fork )
    { PL_LOCK(L_THREAD);
      GD->statistics.threads_finished++;
      assert(GD->statistics.threads_created - GD->statistics.threads_finished >= 1);
      GD->statistics.thread_cputime += time;
    }
    destroy_thread_message_queue(&ld->thread.messages);
    info->thread_data = NULL;		/* avoid a loop */
    info->has_tid = FALSE;		/* needed? */
    if ( !after_fork )
      PL_UNLOCK(L_THREAD);

    if ( acknowledge )			/* == canceled */
    { DEBUG(MSG_CLEANUP_THREAD,
	    Sdprintf("Acknowledge dead of %d\n", info->pl_tid));
#if !defined(_GNU_SOURCE) || defined(__GLIBC__)
/* MUSL C library crashes when detaching from the cleanup handler */
/* This is used for program termination only, so we should be ok */
      pthread_detach(info->tid);
#endif
    }

    if ( info->detached || acknowledge )
      free_thread_info(info);
    ld->thread.info = NULL;		/* help force a crash if ld used */
    maybe_free_local_data(ld);

    if ( acknowledge )
      sem_post(sem_canceled_ptr);
  }

  TLD_set_LD(old_ld);
}


static void
free_prolog_thread(void *data)
{ PL_local_data_t *ld = data;

  freePrologThread(ld, FALSE);
}


#ifdef O_QUEUE_STATS
static void msg_statistics(void);
#endif

#ifdef O_ATFORK					/* Not yet default */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
reinit_threads_after_fork() is called after a  fork   in  the child. Its
task is to  restore  the  consistency   of  the  thread  information. It
performs the following tasks:

    * Reinitialize all mutexes we know of
      Just calling simpleMutexInit() seems a bit dubious, but as we have
      no clue about their locking status, what are the alternatives?
    * Make the thread the main (=1) thread if this is not already the
      case.  This is needed if another thread has initiated the fork.
    * Reclaim stacks and other resources associated with other threads
      that existed before the fork.

There are several issues with the current implementation:

    * If fork/1 is called while the calling thread holds a mutex (as in
      with_mutex/2), the consistency of this mutex will be lost.
    * I doubt we have all mutexes.  Certainly not of the packages.
      We should check at least I/O, user-level mutexes and other mutexes
      that are created locally.
    * If another thread than the main thread forks, we still need to
      properly reclaim the main-thread resources.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
reinit_threads_after_fork(void)
{ GET_LD
  counting_mutex *m;
  PL_thread_info_t *info;
  int i;

  if ( will_exec ||
       (GD->statistics.threads_created - GD->statistics.threads_finished) == 1)
    return;					/* no point */

  info = LD->thread.info;

  for(m = GD->thread.mutexes; m; m = m->next)
  { simpleMutexInit(&m->mutex);			/* Dubious */
    m->count = 0;
    m->lock_count = 0;
#ifdef O_CONTENTION_STATISTICS
    m->collisions = 0;
#endif
  }

  if ( info->pl_tid != 1 )
  { DEBUG(MSG_THREAD, Sdprintf("Forked thread %d\n", info->pl_tid));
    *GD->thread.threads[1] = *info;
    info->status = PL_THREAD_UNUSED;
    info = GD->thread.threads[1];
    LD->thread.info = info;
    info->pl_tid = 1;
  }
  set_system_thread_id(info);

  for(i=2; i<=GD->thread.highest_id; i++)
  { if ( (info=GD->thread.threads[i]) )
    { if ( info->status != PL_THREAD_UNUSED )
      { freePrologThread(info->thread_data, TRUE);
	info->status = PL_THREAD_UNUSED;
      }
    }
  }
  GD->thread.highest_id = 1;

  GD->statistics.thread_cputime = 0.0;
  GD->statistics.threads_created = 1;
  GD->statistics.threads_finished = 0;

  assert(PL_thread_self() == 1);
}

#else

#undef pthread_atfork				/* may be a macro */
#define pthread_atfork(prep, parent, child) (void)0

#endif /*O_ATFORK*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_cleanup_fork() must be called between  fork()   and  exec() to remove
traces of Prolog that are not  supposed   to  leak into the new process.
Note that we must be careful  here.   Notably,  the  code cannot lock or
unlock any mutex as the behaviour of mutexes is undefined over fork().

Earlier versions used the file-table to  close file descriptors that are
in use by Prolog. This can't work as   the  table is guarded by a mutex.
Now we use the FD_CLOEXEC flag in Snew();
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
PL_cleanup_fork(void)
{ will_exec = TRUE;
  stopItimer();
}


void
initPrologThreads(void)
{ PL_thread_info_t *info;
  static int init_ldata_key = FALSE;

  initAlloc();

#if defined(USE_CRITICAL_SECTIONS) && !defined(O_SHARED_KERNEL)
  W32initMutexes();		/* see also DllMain() */
#endif

#ifdef PTW32_STATIC_LIB
  win_thread_initialize();
#endif

  PL_LOCK(L_THREAD);
  if ( threads_ready )
  { PL_UNLOCK(L_THREAD);
    return;
  }

#ifdef O_QUEUE_STATS
  atexit(msg_statistics);
#endif

  if ( !init_ldata_key )
  { init_ldata_key = TRUE;
#if !(defined(USE_CRITICAL_SECTIONS) && defined(O_SHARED_KERNEL))
#ifndef HAVE___THREAD
    TLD_alloc(&PL_ldata);		/* see also alloc_thread() */
#endif
#endif
  }
  TLD_set_LD(&PL_local_data);

  PL_local_data.magic = LD_MAGIC;
  { GD->thread.thread_max = 4;		/* see resizeThreadMax() */
    GD->thread.highest_allocated = 1;
    GD->thread.threads = PL_malloc(GD->thread.thread_max *
				   sizeof(*GD->thread.threads));
    memset(GD->thread.threads, 0,
	   GD->thread.thread_max * sizeof(*GD->thread.threads));
    info = GD->thread.threads[1] = allocHeapOrHalt(sizeof(*info));
    memset(info, 0, sizeof(*info));
    info->pl_tid = 1;
    info->debug = TRUE;
    GD->thread.highest_id = 1;
    info->thread_data = &PL_local_data;
    info->status = PL_THREAD_RUNNING;
    PL_local_data.thread.info = info;
    PL_local_data.thread.magic = PL_THREAD_MAGIC;
    set_system_thread_id(info);
    init_message_queue(&PL_local_data.thread.messages, 0);
    init_predicate_references(&PL_local_data);

    GD->statistics.thread_cputime = 0.0;
    GD->statistics.threads_created = 1;
    pthread_mutex_init(&GD->thread.index.mutex, NULL);
    pthread_cond_init(&GD->thread.index.cond, NULL);
    initMutexes();
    link_mutexes();
    threads_ready = TRUE;
  }

  pthread_atfork(NULL, NULL, reinit_threads_after_fork);
  initMessageQueues();

  PL_UNLOCK(L_THREAD);
}


void
cleanupThreads(void)
{ int i;
  /*TLD_free(PL_ldata);*/		/* this causes crashes */

  if ( queueTable )
  { destroyHTable(queueTable);		/* removes shared queues */
    queueTable = NULL;
    simpleMutexDelete(&queueTable_mutex);
  }
  if ( GD->thread.mutexTable )
  { destroyHTable(GD->thread.mutexTable);
    GD->thread.mutexTable = NULL;
  }
  if ( threadTable )
  { destroyHTable(threadTable);
    threadTable = NULL;
  }
  for(i=1; i<GD->thread.thread_max; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];

    if ( info )
      freeHeap(info, sizeof(*info));
  }
  free_lingering(&GD->thread.lingering, GEN_MAX);
  PL_free(GD->thread.threads);
  GD->thread.threads = NULL;
  threads_ready = FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cleanup the system. This is  rather   complicated  in the multi-threaded
environment. We do want to give threads  the opportunity to cleanup, but
asynchronously signalling a thread to ask for its termination may not be
honoured.  The approach is as follows:

  - Find all running threads, not being myself.  Then
    - If the thread already stopped, join it
    - If it is running and has a cancel function, call this.
    - Else send abort/0 using the thread signal API.
  - For both last cases, count the number of threads we signalled.
  - Now wait for them to die for at most a second (turn this into a
    flag?) by:
    - Setting up a semaphore because semaphores are signal-safe
    - Have the thread exit handler call sem_post() when complete
    - Call sem_wait() as many times as the number of cancelled threads.

Unfortunately, this may not work, so  we   need  a rescue. We have three
options:

  - Use sem_timedwait().  This is the preferred solution.  Unfortunately
    it is not available everywhere (e.g., MacOS).
  - Use sem_wait() and guard it using setitimer().  That seems to work
    on MacOS.
  - The initial implementation: a polling loop using sem_trywait().
    Unfortunately that typically causes a 0.1 sec delay in terminating.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(HAVE_SEM_TIMEDWAIT) && defined(HAVE_SETITIMER)
#define USE_TIMER_WAIT 1

#include <sys/time.h>
static int exit_wait_timeout = FALSE;

static void
dummy_handler(int sig)
{ (void)sig;

  exit_wait_timeout = TRUE;
}
#endif

int
exitPrologThreads(void)
{ int rc;
  int i;
  int me = PL_thread_self();
  int canceled = 0;

  DEBUG(MSG_THREAD, Sdprintf("exitPrologThreads(): me = %d\n", me));

  sem_init(sem_canceled_ptr, USYNC_THREAD, 0);

  for(i=1; i<= GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];

    if ( info && info->thread_data && i != me )
    { switch(info->status)
      { case PL_THREAD_FAILED:
	case PL_THREAD_EXITED:
	case PL_THREAD_EXCEPTION:
	{ void *r;
	  int rc;

	  if ( !COMPARE_AND_SWAP_INT(&info->joining_by, 0, me) &&
	       (rc=pthread_join(info->tid, &r)) )
	    Sdprintf("Failed to join thread %d: %s\n", i, ThError(rc));

	  break;
	}
	case PL_THREAD_RUNNING:
	{ if ( !info->is_engine )
	  { info->thread_data->exit_requested = EXIT_REQ_PROCESS;

	    if ( info->cancel )
	    { switch( (*info->cancel)(i) )
	      { case PL_THREAD_CANCEL_FAILED:
		  break;
	        case PL_THREAD_CANCEL_MUST_JOIN:
		  canceled++;
		  /*FALLTHROUGH*/
	        case PL_THREAD_CANCEL_JOINED:
		  continue;
	      }
	    }

	    if ( PL_thread_raise(i, SIG_PLABORT) )
	      canceled++;
	  }

	  break;
	}
	default:
	  break;
      }
    }
  }

  if ( canceled > 0 )		    /* see (*) above */
  { DEBUG(MSG_CLEANUP_THREAD, Sdprintf("Waiting for %d threads ", canceled));

#ifdef USE_TIMER_WAIT
    struct itimerval timeout = {0};
    struct sigaction act = {0};

    timeout.it_value.tv_sec = 1;
    act.sa_handler = dummy_handler;

    if ( sigaction(SIGALRM, &act, NULL) != 0 ||
	 setitimer(ITIMER_REAL, &timeout, NULL) != 0 )
      Sdprintf("WARNING: Failed to install timeout for shutdown\n");

    while(canceled > 0)
    { if ( sem_wait(sem_canceled_ptr) == 0 )
      { canceled--;
	DEBUG(MSG_CLEANUP_THREAD, Sdprintf("Left %d", canceled));
      } else
      { if ( errno != EINTR || exit_wait_timeout )
	  break;
      }
    }

#elif defined(HAVE_SEM_TIMEDWAIT)
    struct timespec deadline;

    get_current_timespec(&deadline);
    deadline.tv_nsec += 1000000000; /* 1 sec */
    carry_timespec_nanos(&deadline);

    while(canceled > 0)
    { if ( sem_timedwait(sem_canceled_ptr, &deadline) == 0 )
      { canceled--;
	DEBUG(MSG_CLEANUP_THREAD, Sdprintf("Left %d", canceled));
      } else
      { if ( errno != EINTR )
	  break;
      }
    }

#else
    int maxwait = 10;

    for(maxwait = 10; maxwait > 0 && canceled > 0; maxwait--)
    { while ( sem_trywait(sem_canceled_ptr) == 0 )
      { DEBUG(MSG_CLEANUP_THREAD, Sdprintf("."));
	canceled--;
      }
      if ( canceled > 0 )
      { DEBUG(MSG_CLEANUP_THREAD, Sdprintf("W"));
	Pause(0.1);
      }
    }

#endif
    DEBUG(MSG_CLEANUP_THREAD, Sdprintf("\nLeft: %d threads\n", canceled));
  }

  if ( canceled )
  { GET_LD
    fid_t fid;

    if ( (fid = PL_open_foreign_frame()) )
    { term_t head    = PL_new_term_ref();
      term_t running = PL_new_term_ref();
      term_t tail    = PL_copy_term_ref(running);

      rc = TRUE;
      for(i = 1; i <= GD->thread.highest_id; i++)
      { PL_thread_info_t *info = GD->thread.threads[i];

	if ( info && info->thread_data && i != me )
	{ if ( info->status == PL_THREAD_RUNNING )
	  { if ( !PL_unify_list(tail, head, tail) ||
		 !unify_thread_id(head, info) )
	    { rc = FALSE;
	      break;
	    }
	  }
	}
      }

      if ( rc )
      { rc = ( PL_unify_nil(tail) &&
	       printMessage(ATOM_informational,
			    PL_FUNCTOR_CHARS, "threads_not_died", 1,
			      PL_TERM, running)
	     );
      }
    } else
    { rc = FALSE;
    }

    if ( !rc )
      Sdprintf("%d threads wouldn't die\n", canceled);
    rc = FALSE;
  } else
  { DEBUG(MSG_THREAD, Sdprintf("done\n"));
#ifndef WIN64			/* FIXME: Hangs if nothing is printed */
    sem_destroy(sem_canceled_ptr);
#endif
    rc = TRUE;
  }

  threads_ready = FALSE;
  return rc;
}


		 /*******************************
		 *	    ALIAS NAME		*
		 *******************************/

int
aliasThread(int tid, atom_t type, atom_t name)
{ GET_LD
  PL_thread_info_t *info;
  thread_handle *th;
  int rc = TRUE;

  PL_LOCK(L_THREAD);
  if ( !threadTable )
    threadTable = newHTable(16);

  if ( (threadTable && lookupHTable(threadTable, (void *)name)) ||
       (queueTable  && lookupHTable(queueTable,  (void *)name)) )
  { term_t obj = PL_new_term_ref();

    PL_UNLOCK(L_THREAD);
    PL_put_atom(obj, name);
    return PL_error(NULL, 0, "Alias name already taken",
		    ERR_PERMISSION, ATOM_create, type, obj);
  }

  info = GD->thread.threads[tid];
  if ( (th = create_thread_handle(info)) )
  { th->alias = name;
    PL_register_atom(name);
    PL_register_atom(info->symbol);
    addNewHTable(threadTable, (void *)name, (void *)info->symbol);
  } else
  { rc = PL_no_memory();
  }
  PL_UNLOCK(L_THREAD);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_get_thread_alias() gets the alias name of   a  thread. It is intended
for crash-analysis. Normal applications should use PL_unify_thread_id().

The implementation is wrong for two reasons. It should register the atom
and this implementation should lock L_THREAD  because otherwise the atom
may be gone even before it is   locked. However, locking is not unlikely
to deadlock during crash analysis ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_get_thread_alias(int tid, atom_t *alias)
{ PL_thread_info_t *info;
  thread_handle *th;

  if ( tid == 0 )
    tid = PL_thread_self();
  if ( tid < 1 || tid > GD->thread.highest_id )
    return FALSE;

  info = GD->thread.threads[tid];
  if ( info->symbol &&
       (th=symbol_thread_handle(info->symbol)) &&
       th->alias )
  { *alias = th->alias;

    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *	     GC ENGINES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gc_thread() is (indirectly) called from atom-GC if the engine is not yet
fully reclaimed, we have several situations:

  - The engine is still running.  Detach it, such that it will be
    reclaimed silently when done.
  - The engine has completed.  Join it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static thread_handle *gced_threads = NULL;
static int       thread_gc_running = FALSE;

static void
discard_thread(thread_handle *h)
{ int alive = FALSE;
  PL_thread_info_t *info;

  if ( true(h, TH_IS_INTERACTOR) )
  { destroy_interactor(h, TRUE);		/* cannot be running */
    return;
  }

  PL_LOCK(L_THREAD);
  if ( (info=h->info) &&
       (alive=is_alive(info->status)) &&
       !info->detached )
  { if ( info->has_tid )
    { if ( pthread_detach(info->tid) == 0 )
	info->detached = TRUE;
    }
  }
  PL_UNLOCK(L_THREAD);

  if ( !alive )
  { double delay = 0.0001;

    if ( !info->detached &&
	 COMPARE_AND_SWAP_INT(&info->joining_by, 0, -1) )
    { void *r;

      while( pthread_join(info->tid, &r) == EINTR )
	;
    }

    while ( info->thread_data )
    { Pause(delay);
      if ( delay < 0.01 )
	delay *= 2;
    }
    free_thread_info(info);
  }
}


static void *
thread_gc_loop(void *closure)
{
#if O_SIGNALS && defined(HAVE_SIGPROCMASK)
  sigset_t set;
  allSignalMask(&set);
  pthread_sigmask(SIG_BLOCK, &set, NULL);
#endif

  for(;;)
  { thread_handle *h;

    do
    { h = gced_threads;
    } while ( h && !COMPARE_AND_SWAP_PTR(&gced_threads, h, h->next_free) );

    if ( h )
    { if ( GD->cleaning == CLN_NORMAL )
	discard_thread(h);
      PL_free(h);
    } else
    { break;
    }
  }

  thread_gc_running = FALSE;
  return NULL;
}


static void
start_thread_gc_thread(void)
{ if ( !thread_gc_running )
  { pthread_attr_t attr;
    int rc;
    pthread_t thr;

    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    thread_gc_running = TRUE;
    rc = pthread_create(&thr, &attr, thread_gc_loop, NULL);
    pthread_attr_destroy(&attr);
    if ( rc != 0 )
    { Sdprintf("Failed to start thread gc thread\n");
      thread_gc_running = FALSE;
    }
  }
}


static void
gc_thread(thread_handle *ref)
{ if ( GD->cleaning == CLN_NORMAL )	/* otherwise we are terminating */
  { thread_handle *h;

    do
    { h = gced_threads;
      ref->next_free = h;
    } while( !COMPARE_AND_SWAP_PTR(&gced_threads, h, ref) );

    start_thread_gc_thread();
  } else
    PL_free(ref);
}


		 /*******************************
		 *	   THREAD SYMBOL	*
		 *******************************/

static int
write_thread_handle(IOSTREAM *s, atom_t eref, int flags)
{ thread_handle **refp = PL_blob_data(eref, NULL, NULL);
  thread_handle *ref = *refp;
  (void)flags;

  Sfprintf(s, "<%s>(%d,%p)",
	   true(ref, TH_IS_INTERACTOR) ? "engine" : "thread",
	   ref->engine_id, ref);
  return TRUE;
}


static int
release_thread_handle(atom_t aref)
{ thread_handle **refp = PL_blob_data(aref, NULL, NULL);
  thread_handle *ref   = *refp;
  PL_thread_info_t *info;

  if ( (info=ref->info) )
  { /* assert(info->detached == FALSE || info->is_engine); TBD: Sort out */
    info->symbol = 0;				/* TBD: Dubious */
    gc_thread(ref);
  } else
    PL_free(ref);

  return TRUE;
}


static int
save_thread(atom_t aref, IOSTREAM *fd)
{ thread_handle **refp = PL_blob_data(aref, NULL, NULL);
  thread_handle *ref   = *refp;
  (void)fd;

  return PL_warning("Cannot save reference to <%s>(%d,%p)",
		    true(ref, TH_IS_INTERACTOR) ? "engine" : "thread",
		    ref->engine_id, ref);
}


static atom_t
load_thread(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-thread-handle>");
}


static PL_blob_t thread_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "thread",
  release_thread_handle,
  NULL,
  write_thread_handle,
  NULL,
  save_thread,
  load_thread
};


		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resizing max-threads. Note that we do *not* deallocate the old structure
to ensure we can  access  GD->thread.threads[i]   at  any  time  without
locking.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
resizeThreadMax(void)
{ int newmax = GD->thread.thread_max*2;
  PL_thread_info_t **newinfo, **oldinfo;
  size_t dsize = GD->thread.thread_max * sizeof(*GD->thread.threads);

  oldinfo = GD->thread.threads;
  newinfo = PL_malloc(newmax * sizeof(*GD->thread.threads));
  memset(addPointer(newinfo,dsize), 0, dsize);
  memcpy(newinfo, oldinfo, dsize);
  GD->thread.threads = newinfo;
  GD->thread.thread_max = newmax;
  linger(&GD->thread.lingering, PL_free, oldinfo);

  return TRUE;
}


/* MT: thread-safe */

static PL_thread_info_t *
alloc_thread(void)
{ PL_thread_info_t *info;
  PL_local_data_t *ld;

  ld = allocHeapOrHalt(sizeof(PL_local_data_t));
  memset(ld, 0, sizeof(PL_local_data_t));

  do
  { info = GD->thread.free;
  } while ( info && !COMPARE_AND_SWAP_PTR(&GD->thread.free, info, info->next_free) );

  if ( info )
  { int i = info->pl_tid;
    assert(info->status == PL_THREAD_UNUSED);
    memset(info, 0, sizeof(*info));
    info->pl_tid = i;
    PL_LOCK(L_THREAD);
  } else
  { int i;

    info = allocHeapOrHalt(sizeof(*info));
    memset(info, 0, sizeof(*info));

    PL_LOCK(L_THREAD);
    i = info->pl_tid = ++GD->thread.highest_allocated;
    if ( i == GD->thread.thread_max )
      resizeThreadMax();
    if ( i > GD->thread.peak_id )
      GD->thread.peak_id = i;

    assert(GD->thread.threads[i] == NULL);
    GD->thread.threads[i] = info;
  }

  ld->thread.info = info;
  ld->thread.magic = PL_THREAD_MAGIC;
  info->thread_data = ld;
  info->status = PL_THREAD_RESERVED;
  info->debug = TRUE;

  if ( info->pl_tid > GD->thread.highest_id )
    GD->thread.highest_id = info->pl_tid;
  PL_UNLOCK(L_THREAD);

  ATOMIC_INC(&GD->statistics.threads_created);

  return info;
}


int
PL_thread_self(void)
{ GET_LD
  PL_local_data_t *ld = LD;
  PL_thread_info_t *info;

  if ( ld && (info=ld->thread.info) )
    return info->pl_tid;

  return -1;				/* thread has no Prolog thread */
}


int
PL_unify_thread_id(term_t t, int i)
{ if ( i < 1 ||
       i > GD->thread.highest_id ||
       GD->thread.threads[i]->status == PL_THREAD_UNUSED ||
       GD->thread.threads[i]->status == PL_THREAD_RESERVED )
    return -1;				/* error */

  return unify_thread_id(t, GD->thread.threads[i]);
}


int
PL_get_thread_id_ex(term_t t, int *idp)
{ PL_thread_info_t *info;

  if ( !get_thread(t, &info, TRUE) )
    return FALSE;

  *idp = info->pl_tid;

  return TRUE;
}



#ifdef __WINDOWS__

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_w32thread_raise(DWORD id, int sig)
    Sets the signalled mask for a specific Win32 thread. This is a
    partial work-around for the lack of proper asynchronous signal
    handling in the Win32 platform.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_w32thread_raise(DWORD id, int sig)
{ int i;

  if ( !IS_VALID_SIGNAL(sig) )
    return FALSE;			/* illegal signal */

  PL_LOCK(L_THREAD);
  for(i = 1; i <= GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];

    if ( info && info->w32id == id && info->thread_data )
    { PL_local_data_t *ld = info->thread_data;

      PL_UNLOCK(L_THREAD);
      if ( pendingSignal(ld, sig) )
	ld->signal.forced = sig;
      else
	raiseSignal(ld, sig);
      PostThreadMessage(id, WM_SIGNALLED, 0, 0L);
      DEBUG(MSG_THREAD, Sdprintf("Signalled %d to thread %d\n", sig, i));
      return TRUE;
    }
  }
  PL_UNLOCK(L_THREAD);

  return FALSE;				/* can't find thread */
}

#endif /*__WINDOWS__*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Make a thread stop processing blocking operations such that it can check
whether it is signalled. Returns  TRUE   when  successful,  FALSE if the
thread no longer exists and -1 if alerting is disabled.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
alertThread(PL_thread_info_t *info)
{ PL_local_data_t *ld = info->thread_data;

  if ( ld->thread.alert.type )
  { int done = FALSE;

    PL_LOCK(L_ALERT);
    switch(ld->thread.alert.type)
    { case ALERT_QUEUE_RD:
        cv_broadcast(&ld->thread.alert.obj.queue->cond_var);
	done = TRUE;
	break;
      case ALERT_QUEUE_WR:
        cv_broadcast(&ld->thread.alert.obj.queue->drain_var);
	done = TRUE;
	break;
#if STDC_CV_ALERT
      case ALERT_LOCK_CV:
        /* thread_wait_signal locks L_ALERT inside alert_mtx, so we
	 * must do the same to avoid deadlocks. A thread race here
	 * will only cause us to alert a non-waiting thread, which
	 * isn't a problem. */
	PL_UNLOCK(L_ALERT);
	mtx_lock(&ld->signal.alert_mtx);
	PL_LOCK(L_ALERT);
	done = cnd_broadcast(&ld->signal.alert_cv) == thrd_success;
	mtx_unlock(&ld->signal.alert_mtx);
	break;
#endif
    }
    PL_UNLOCK(L_ALERT);
    if ( done )
      return TRUE;
  }

#if STDC_CV_ALERT
  /* Try sending a notification on the thread's alert cv, if it exists */
  cnd_broadcast(&ld->signal.alert_cv);
#endif

#ifdef __WINDOWS__
  if ( info->w32id )
  { PostThreadMessage(info->w32id, WM_SIGNALLED, 0, 0L);
    return TRUE;			/* NOTE: PostThreadMessage() can */
					/* fail if thread is being created */
  }
#elif defined(SIG_ALERT)
  WITH_LD(ld)
  { if ( info->has_tid && truePrologFlag(PLFLAG_SIGNALS) && GD->signals.sig_alert )
    { DEBUG(MSG_THREAD_SIGNAL, Sdprintf("Sending signal %d to %d\n",
					GD->signals.sig_alert, info->pl_tid));
      return pthread_kill(info->tid, GD->signals.sig_alert) == 0;
    }
  }
#endif
  return -1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_thread_raise() raises a  synchronous  signal   in  (usually)  another
thread.

(*) HACK. We should not lock L_THREAD in this function but we do need to
prevent ld from dropping while we process   it.  Possibly we should move
the signal mask to the info structure? This patch is badly needed as the
system now crashes on any  alrm   from  library(time). We need something
better though.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_thread_raise(int tid, int sig)
{ if ( tid >= 1 && tid <= GD->thread.highest_id )
  { PL_thread_info_t *info = GD->thread.threads[tid];

    if ( info &&
	 info->status != PL_THREAD_UNUSED &&
	 info->status != PL_THREAD_RESERVED )
    { GET_LD
      PL_local_data_t *ld;
      int rc;

      if ( LD )					/* See (*) */
	ld = acquire_ldata(info);
      else
	ld = info->thread_data;

      rc = ( ld &&
	     ld->magic == LD_MAGIC &&
	     raiseSignal(ld, sig) &&
	     (alertThread(info) != FALSE) );

      if ( LD )
	release_ldata(ld);

      return rc;
    } else
    { return FALSE;
    }
  }

  return FALSE;
}


#define thread_wait_signal(_) LDFUNC(thread_wait_signal, _)
static int
thread_wait_signal(DECL_LD)
{ int i;

  while( !is_signalled() )
  {
#ifdef __WINDOWS__
    MSG msg;
    if ( !GetMessage(&msg, (HWND)-1, WM_SIGNALLED, WM_SIGNALLED) )
      return -1;
    continue;
#elif defined(SIG_ALERT)
    if ( truePrologFlag(PLFLAG_SIGNALS) )
    { sigset_t set;
      int sig;

      sigemptyset(&set);
      sigaddset(&set, GD->signals.sig_alert);
      sigwait(&set, &sig);
      continue;
    }
#endif
#if STDC_CV_ALERT
    LD->thread.alert.type = ALERT_LOCK_CV;
    mtx_lock(&LD->signal.alert_mtx);
    if (!is_signalled())
      cnd_wait(&LD->signal.alert_cv, &LD->signal.alert_mtx);
    PL_LOCK(L_ALERT);
    LD->thread.alert.type = 0;
    PL_UNLOCK(L_ALERT);
    mtx_unlock(&LD->signal.alert_mtx);
#else
    /* Shouldn't happen? but let's avoid the spin cycle anyway */
    Pause(0.001);
#endif
  }

  for(i=0; i<SIGMASK_WORDS; i++)
  { while( LD->signal.pending[i] )
    { int sig = MINSIGNAL+SIGMASK_WIDTH*i;
      sigmask_t mask = 1;

      for( ; mask ; mask <<= 1, sig++ )
      { if ( LD->signal.pending[i] & mask )
	{ __atomic_and_fetch(&LD->signal.pending[i], ~mask, __ATOMIC_SEQ_CST);

	  if ( sig == SIG_THREAD_SIGNAL )
	  { dispatch_signal(sig, TRUE);
	    if ( exception_term )
	      return -1;
	  } else
	  { return sig;
	  }
	}
      }
    }
  }

  return -1;					/* cannot happen */
}

static
PRED_IMPL("$thread_sigwait", 1, thread_sigwait, 0)
{ PRED_LD
  int sig;

  if ( (sig = thread_wait_signal()) >= 0 )
    return PL_unify_atom_chars(A1, signal_name(sig));

  return FALSE;
}



const char *
threadName(int id)
{ PL_thread_info_t *info;
  thread_handle *th;
  char tmp[16];

  if ( id == 0 )
    id = PL_thread_self();
  if ( id < 0 )
    return "[Not a prolog thread]";

  info = GD->thread.threads[id];
  if ( info->symbol &&
       (th=symbol_thread_handle(info->symbol)) &&
       th->alias )
    return PL_atom_chars(th->alias);

  sprintf(tmp, "%d", id);
  return buffer_string(tmp, BUF_STACK);
}


intptr_t
system_thread_id(PL_thread_info_t *info)
{ if ( !info )
  { GET_LD
    if ( LD )
      info = LD->thread.info;
    else
      return -1;
  }
#ifdef PID_IDENTIFIES_THREAD
  return info->pid;
#else
#ifdef __WINDOWS__
  return info->w32id;
#else
  return (intptr_t)info->tid;
#endif
#endif
}

static void
set_system_thread_id(PL_thread_info_t *info)
{
  info->tid = pthread_self();
  info->has_tid = TRUE;
#if defined(HAVE_GETTID_SYSCALL)
  info->pid = syscall(__NR_gettid);
#elif defined(HAVE_GETTID_MACRO)
  info->pid = gettid();
#elif defined(__CYGWIN__)
  info->pid = getpid();
#elif defined(__WINDOWS__)
  info->w32id = GetCurrentThreadId();
#endif
}


static int
set_os_thread_name_from_charp(const char *s)
{
#ifdef HAVE_PTHREAD_SETNAME_NP
  char name[16];

  strncpy(name, s, 15);
  name[15] = EOS;

#ifdef HAVE_PTHREAD_SETNAME_NP_WITH_TID
  if ( pthread_setname_np(pthread_self(), name) == 0 )
    return TRUE;
#elif HAVE_PTHREAD_SETNAME_NP_WITH_TID_AND_ARG
  if ( pthread_setname_np(pthread_self(), "%s", (void *)name) == 0 )
    return TRUE;
#else
  if ( pthread_setname_np(name) == 0 )
    return TRUE;
#endif
#endif
  return FALSE;
}


static int
set_os_thread_name(atom_t alias)
{
#ifdef HAVE_PTHREAD_SETNAME_NP
  GET_LD
  term_t t = PL_new_term_ref();
  PL_put_atom(t, alias);
  char *s;

  if ( PL_get_chars(t, &s, CVT_ATOM|REP_MB|BUF_DISCARDABLE) )
    return set_os_thread_name_from_charp(s);
#endif
  return FALSE;
}


static const opt_spec make_thread_options[] =
{ { ATOM_alias,		 OPT_ATOM },
  { ATOM_debug,		 OPT_BOOL },
  { ATOM_detached,	 OPT_BOOL },
  { ATOM_stack_limit,	 OPT_SIZE },
  { ATOM_c_stack,	 OPT_SIZE },
  { ATOM_at_exit,	 OPT_TERM },
  { ATOM_inherit_from,	 OPT_TERM },
  { ATOM_affinity,	 OPT_TERM },
  { ATOM_queue_max_size, OPT_SIZE },
  { NULL_ATOM,		 0 }
};


static void
set_thread_completion(PL_thread_info_t *info, int rc, term_t ex)
{ PL_LOCK(L_THREAD);
  if ( rc )
  { info->status = PL_THREAD_SUCCEEDED;
  } else
  { if ( ex )
    { if ( info->detached )
	info->return_value = 0;
      else
	info->return_value = PL_record(ex);
      info->status = PL_THREAD_EXCEPTION;
    } else
    { info->status = PL_THREAD_FAILED;
    }
  }
  PL_UNLOCK(L_THREAD);
}


static void *
start_thread(void *closure)
{ PL_thread_info_t *info = closure;
  thread_handle *th;
  term_t ex, goal;
  int rval;

  assert(info->goal);
#if O_SIGNALS
  blockSignal(SIGINT);			/* only the main thread processes */
					/* Control-C */
#endif
  set_system_thread_id(info);		/* early to get exit code ok */

  if ( !initialise_thread(info) )
    return (void *)FALSE;

  { GET_LD

    pthread_cleanup_push(free_prolog_thread, info->thread_data);

    PL_LOCK(L_THREAD);
    info->status = PL_THREAD_RUNNING;
    PL_UNLOCK(L_THREAD);

    if ( info->symbol &&
	 (th=symbol_thread_handle(info->symbol)) &&
	 th->alias )
      set_os_thread_name(th->alias);

    goal = PL_new_term_ref();
    PL_put_atom(goal, ATOM_dthread_init);

    rval = callProlog(MODULE_system, goal, PL_Q_CATCH_EXCEPTION, &ex);
    if ( rval )
      rval = callEventHook(PLEV_THREAD_START, info);

    if ( rval )
    { if ( !PL_recorded(info->goal, goal) )
      { rval = raiseStackOverflow(GLOBAL_OVERFLOW);
	ex = exception_term;
      } else
      { rval  = callProlog(info->module, goal, PL_Q_CATCH_EXCEPTION, &ex);
      }
    }

    if ( !rval && info->detached )
    { if ( ex )
      { int print = TRUE;

	if ( LD->exit_requested )
	{ if ( classify_exception(ex) == EXCEPT_ABORT )
	    print = FALSE;
	}

	if ( print )
	{ if ( !printMessage(ATOM_warning,
			     PL_FUNCTOR_CHARS, "abnormal_thread_completion", 2,
			       PL_TERM, goal,
			       PL_FUNCTOR, FUNCTOR_exception1,
			       PL_TERM, ex) )
	    PL_clear_exception();	/* The thread is dead anyway */
	}
      } else
      { if ( !printMessage(ATOM_warning,
			   PL_FUNCTOR_CHARS, "abnormal_thread_completion", 2,
			     PL_TERM, goal,
			     PL_ATOM, ATOM_fail) )
	  PL_clear_exception();		/* The thread is dead anyway */
      }
    }

    set_thread_completion(info, rval, ex);
    pthread_cleanup_pop(1);
  }

  return (void *)TRUE;
}


static void
copy_local_data(PL_local_data_t *ldnew, PL_local_data_t *ldold,
		size_t max_queue_size)
{ GET_LD

  if ( !LD )
    TLD_set_LD(ldnew);

  PL_register_atom(ldold->prompt.current);
  ldnew->prompt			  = ldold->prompt;
  if ( ldold->prompt.first )
  { ldnew->prompt.first		  = ldold->prompt.first;
    PL_register_atom(ldnew->prompt.first);
  }
  ldnew->modules		  = ldold->modules;
  ldnew->IO			  = ldold->IO;
  ldnew->IO.input_stack		  = NULL;
  ldnew->IO.output_stack	  = NULL;
  ldnew->encoding		  = ldold->encoding;
#ifdef O_LOCALE
  ldnew->locale.current		  = acquireLocale(ldold->locale.current);
#endif
  ldnew->_debugstatus		  = ldold->_debugstatus;
  ldnew->_debugstatus.retryFrame  = 0;
  ldnew->_debugstatus.suspendTrace= 0;
  if ( ldold->_debugstatus.skiplevel != SKIP_VERY_DEEP )
  { ldnew->_debugstatus.debugging = DBG_OFF;
    ldnew->_debugstatus.tracing = FALSE;
    ldnew->_debugstatus.skiplevel = SKIP_VERY_DEEP;
  }

  alloc_pool *pool = ldold->tabling.node_pool;
  if ( pool )
    ldnew->tabling.node_pool = new_alloc_pool(pool->name, pool->limit);
  ldnew->fli.string_buffers.tripwire
				  = ldold->fli.string_buffers.tripwire;
  ldnew->statistics.start_time    = WallTime();
  ldnew->prolog_flag.mask	  = ldold->prolog_flag.mask;
  ldnew->prolog_flag.occurs_check = ldold->prolog_flag.occurs_check;
  ldnew->prolog_flag.access_level = ldold->prolog_flag.access_level;
#ifdef O_GMP
  ldnew->arith.rat                = ldold->arith.rat;
#endif
  ldnew->arith.f                  = ldold->arith.f;
  if ( ldold->prolog_flag.table )
  { PL_LOCK(L_PLFLAG);
    ldnew->prolog_flag.table	  = copyHTable(ldold->prolog_flag.table);
    PL_UNLOCK(L_PLFLAG);
  }
  ldnew->tabling.restraint        = ldold->tabling.restraint;
  ldnew->tabling.in_assert_propagation = FALSE;
  if ( !ldnew->thread.info->debug )
  { ldnew->_debugstatus.tracing   = FALSE;
    ldnew->_debugstatus.debugging = DBG_OFF;
    setPrologRunMode_LD(ldnew, RUN_MODE_NORMAL);
  }
  ldnew->thread.waiting_for = NULL;
  init_message_queue(&ldnew->thread.messages, max_queue_size);
  init_predicate_references(ldnew);
  referenceStandardStreams(ldnew);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The pthread stacksize must be  a  multiple   of  the  page  size on some
systems. We do the rounding here. If we do not know the page size we use
8192, which should typically be a multiple of the page size.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
round_pages(size_t n)
{ size_t psize;

#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
  if ( (psize = sysconf(_SC_PAGESIZE)) == (size_t)-1 )
    psize = 8192;
#else
  psize = 8192;
#endif

  return ROUND(n, psize);
}


#if defined(HAVE_PTHREAD_ATTR_SETAFFINITY_NP) || defined(HAVE_SCHED_SETAFFINITY)

static int
get_cpuset(term_t affinity, cpu_set_t *set)
{ GET_LD
  term_t head, tail;
  int n=0;
  int cpu_count = CpuCount();

  if ( !(tail = PL_copy_term_ref(affinity)) ||
       !(head = PL_new_term_ref()) )
    return FALSE;

  CPU_ZERO(set);
  while(PL_get_list_ex(tail, head, tail))
  { int i;

    if ( !PL_get_integer_ex(head, &i) )
      return FALSE;
    if ( i < 0 )
      return PL_domain_error("not_less_than_zero", head);
    if ( i >= cpu_count )
      return PL_existence_error("cpu", head);

    CPU_SET(i, set);

    if ( n++ == 100 && !PL_is_acyclic(tail) )
      return PL_type_error("list", tail);
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( n == 0 )
    return PL_domain_error("cpu_affinity", affinity);

  return TRUE;
}

#endif /*defined(HAVE_PTHREAD_ATTR_SETAFFINITY_NP) || defined(HAVE_SCHED_SETAFFINITY)*/

static int
set_affinity(term_t affinity, pthread_attr_t *attr)
{
#ifdef HAVE_PTHREAD_ATTR_SETAFFINITY_NP
  cpu_set_t cpuset;

  if ( !get_cpuset(affinity, &cpuset) )
    return EINVAL;

  return pthread_attr_setaffinity_np(attr, sizeof(cpuset), &cpuset);
#endif

  return 0;
}


word
pl_thread_create(term_t goal, term_t id, term_t options)
{ GET_LD
  PL_thread_info_t *info;
  thread_handle *th;
  PL_local_data_t *ldnew, *ldold = LD;
  atom_t alias = NULL_ATOM, idname;
  pthread_attr_t attr;
  size_t stack = 0;
  size_t c_stack = 0;
  term_t inherit_from = 0;
  term_t at_exit = 0;
  term_t affinity = 0;
  size_t queue_max_size = 0;
  int rc = 0;
  const char *func;
  int debug = -1;
  int detached = FALSE;

  if ( !PL_is_callable(goal) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, goal);

  if ( !GD->thread.enabled || GD->cleaning != CLN_NORMAL )
    return PL_error(NULL, 0, "threading disabled",
		      ERR_PERMISSION,
		      ATOM_create, ATOM_thread, goal);

  if ( !(info = alloc_thread()) )
    return PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_threads);

  ldnew = info->thread_data;

  if ( !scan_options(options, 0, /*OPT_ALL,*/
		     ATOM_thread_option, make_thread_options,
		     &alias,
		     &debug,
		     &detached,
		     &stack,		/* stack */
		     &c_stack,		/* c_stack */
		     &at_exit,
		     &inherit_from,
		     &affinity,
		     &queue_max_size) )
  { free_thread_info(info);
    fail;
  }
  info->detached = detached;
  if ( at_exit && !PL_is_callable(at_exit) )
  { free_thread_info(info);
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, at_exit);
  }
  if ( inherit_from )
  { PL_thread_info_t *oinfo;

    if ( get_thread(inherit_from, &oinfo, TRUE) )
    { ldold = oinfo->thread_data;
    } else
    { free_thread_info(info);
      return FALSE;
    }
  }
  if ( debug >= 0 )
    info->debug = debug;
  else
    info->debug = ldold->thread.info->debug;

  if ( !PL_is_variable(id) &&
       !(PL_get_atom(id, &idname) && idname == alias) )
  { free_thread_info(info);
    return PL_error("thread_create", 3, NULL, ERR_UNINSTANTIATION, 2, id);
  }

  if ( stack )
    info->stack_limit = stack;
  else
    info->stack_limit = ldold->stacks.limit;

  th = create_thread_handle(info);
  if ( alias )
  { if ( !aliasThread(info->pl_tid, ATOM_thread, alias) )
    { free_thread_info(info);
      fail;
    }
  }
  if ( !unify_thread_id(id, info) )
  { free_thread_info(info);

    if ( !PL_exception(0) )
      return PL_uninstantiation_error(id);

    fail;
  }
  if ( !info->detached )
    PL_unregister_atom(th->symbol);

  info->goal = PL_record(goal);
  info->module = PL_context();
  copy_local_data(ldnew, ldold, queue_max_size);
  if ( at_exit )
    register_event_hook(&ldnew->event.hook.onthreadexit, 0, FALSE, at_exit, 0);

  pthread_attr_init(&attr);
  if ( info->detached )
  { func = "pthread_attr_setdetachstate";
    rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  }
  if ( rc == 0 && affinity )
    rc = set_affinity(affinity, &attr);
  if ( rc == 0 )
  {
#ifdef USE_COPY_STACK_SIZE
    struct rlimit rlim;
    if ( !stack && getrlimit(RLIMIT_STACK, &rlim) == 0 )
    { if ( rlim.rlim_cur != RLIM_INFINITY )
	stack = rlim.rlim_cur;
					/* What is an infinite stack!? */
    }
#endif
    if ( c_stack )
    { stack = round_pages(c_stack);
      func = "pthread_attr_setstacksize";
      rc = pthread_attr_setstacksize(&attr, c_stack);
      info->c_stack_size = c_stack;
    } else
    { pthread_attr_getstacksize(&attr, &info->c_stack_size);
    }
  }
  if ( rc == 0 )
  { PL_LOCK(L_THREAD);
    info->status = PL_THREAD_CREATED;
    assert(info->goal);
    func = "pthread_create";
    rc = pthread_create(&info->tid, &attr, start_thread, info);
    PL_UNLOCK(L_THREAD);
  }
  pthread_attr_destroy(&attr);

  if ( rc != 0 )
  { free_thread_info(info);
    if ( !PL_exception(0) )
      PL_error(NULL, 0, ThError(rc),
	       ERR_SYSCALL, func);
    return FALSE;
  }

  return TRUE;
}


static thread_handle *
symbol_thread_handle(atom_t a)
{ void *data;
  size_t len;
  PL_blob_t *type;

  if ( a && (data=PL_blob_data(a, &len, &type)) && type == &thread_blob )
  { thread_handle **erd = data;

    return *erd;
  }

  return NULL;
}


static int
get_thread(term_t t, PL_thread_info_t **info, int warn)
{ GET_LD
  int i = -1;
  atom_t a;

  if ( PL_get_atom(t, &a) )
  { thread_handle *er;
    atom_t symbol = a;

  from_symbol:
    if ( (er=symbol_thread_handle(symbol)) )
    { if ( er->info && !THREAD_STATUS_INVALID(er->info->status) )
      { i = er->engine_id;
      } else
      { no_thread:
	if ( warn )
	  PL_existence_error("thread", t);
	return FALSE;
      }
    } else if ( isTextAtom(symbol) )	/* alias name? */
    { word w;

      if ( (w = (word)lookupHTable(threadTable, (void *)symbol)) )
      { symbol = w;
	goto from_symbol;
      } else
	goto no_thread;
    } else
    { if ( warn )
	PL_type_error("thread", t);
      return FALSE;
    }
  } else if ( !PL_get_integer(t, &i) )
  { if ( warn )
      PL_type_error("thread", t);
    return FALSE;
  }

  if ( i < 1 ||
       i > GD->thread.highest_id ||
       THREAD_STATUS_INVALID(GD->thread.threads[i]->status) )
  { goto no_thread;
  }

  *info = GD->thread.threads[i];

  return TRUE;
}


static thread_handle *
create_thread_handle(PL_thread_info_t *info)
{ atom_t symbol;

  if ( (symbol=info->symbol) )
  { return symbol_thread_handle(symbol);
  } else
  { thread_handle *ref;

    if ( info->is_engine )
      ref = PL_malloc(sizeof(*ref)+sizeof(simpleMutex));
    else
      ref = PL_malloc(sizeof(*ref));

    if ( ref )
    { int new;

      memset(ref, 0, sizeof(*ref));
      if ( info->is_engine )
      { ref->interactor.mutex = (simpleMutex*)(ref+1);
	simpleMutexInit(ref->interactor.mutex);
      }

      ref->info      = info;
      ref->engine_id = info->pl_tid;
      ref->symbol    = lookupBlob((char*)&ref, sizeof(ref), &thread_blob, &new);
      assert(new);
      info->symbol   = ref->symbol;
    }

    return ref;
  }
}


int
unify_thread_id(term_t id, PL_thread_info_t *info)
{ GET_LD
  thread_handle *th;

  if ( (th = create_thread_handle(info)) )
  { atom_t name = th->alias ? th->alias : th->symbol;

    return PL_unify_atom(id, name);
  } else					/* during destruction? */
    return PL_unify_integer(id, info->pl_tid);
}


/* If lock = TRUE, this is used from thread_property/2 and we must
   be careful that the thread may vanish during the process if it
   is a detached thread.  Note that we only avoid crashes.  The fact
   that the value may not be true at the moment it is requested is
   simply a limitation of status pulling.
*/

#define unify_engine_status(status, info) LDFUNC(unify_engine_status, status, info)
static int
unify_engine_status(DECL_LD term_t status, PL_thread_info_t *info)
{ return PL_unify_atom(status, ATOM_suspended);
}


static int
unify_thread_status(term_t status, PL_thread_info_t *info,
		    thread_status stat, int lock)
{ GET_LD

  switch(stat)
  { case PL_THREAD_CREATED:
    case PL_THREAD_RUNNING:
    { int rc = FALSE;
      if ( info->is_engine )
      { if ( lock ) PL_LOCK(L_THREAD);
	if ( !info->has_tid )
	  rc = unify_engine_status(status, info);
	if ( lock ) PL_UNLOCK(L_THREAD);
      }
      return rc || PL_unify_atom(status, ATOM_running);
    }
    case PL_THREAD_EXITED:
    { term_t tmp = PL_new_term_ref();
      int rc = TRUE;

      if ( info->return_value )
      { if ( lock ) PL_LOCK(L_THREAD);
	if ( info->return_value )
	  rc = PL_recorded(info->return_value, tmp);
	if ( lock ) PL_UNLOCK(L_THREAD);
      }

      if ( !rc )
	return raiseStackOverflow(GLOBAL_OVERFLOW);
      else
	return PL_unify_term(status,
			     PL_FUNCTOR, FUNCTOR_exited1,
			       PL_TERM, tmp);
    }
    case PL_THREAD_SUCCEEDED:
      return PL_unify_atom(status, ATOM_true);
    case PL_THREAD_FAILED:
      return PL_unify_atom(status, ATOM_false);
    case PL_THREAD_EXCEPTION:
    { term_t tmp = PL_new_term_ref();
      int rc = TRUE;

      if ( lock ) PL_LOCK(L_THREAD);
      if ( info->return_value )
	rc = PL_recorded(info->return_value, tmp);
      if ( lock ) PL_UNLOCK(L_THREAD);
      if ( !rc )
	return raiseStackOverflow(GLOBAL_OVERFLOW);
      else
	return PL_unify_term(status,
			     PL_FUNCTOR, FUNCTOR_exception1,
			       PL_TERM, tmp);
    }
    case PL_THREAD_NOMEM:
    { return PL_unify_term(status,
			   PL_FUNCTOR, FUNCTOR_exception1,
			     PL_FUNCTOR, FUNCTOR_error2,
			       PL_FUNCTOR, FUNCTOR_resource_error1,
			         PL_ATOM, ATOM_memory,
			       PL_VARIABLE);
    }
    default:
      DEBUG(MSG_THREAD, Sdprintf("info->status = %d\n", info->status));
      fail;				/* can happen in current_thread/2 */
  }
}


word
pl_thread_self(term_t self)
{ GET_LD

  return unify_thread_id(self, LD->thread.info);
}

static void
unalias_thread(thread_handle *th)
{ atom_t name;

  if ( (name=th->alias) )
  { atom_t symbol;

    if ( (symbol=(word)deleteHTable(threadTable, (void *)th->alias)) )
    { th->alias = NULL_ATOM;
      PL_unregister_atom(name);
      PL_unregister_atom(symbol);
    }
  }
}


static void
free_thread_info(PL_thread_info_t *info)
{ record_t rec_rv, rec_g;
  PL_thread_info_t *freelist;

  assert(info->status != PL_THREAD_UNUSED);
  info->status = PL_THREAD_UNUSED;

  if ( info->thread_data )
  { info->detached = FALSE;	/* avoid recursion and we are dead anyway */
    free_prolog_thread(info->thread_data);
  }

  PL_LOCK(L_THREAD);
  if ( info->symbol )
  { thread_handle *th;

    if ( (th=symbol_thread_handle(info->symbol)) )
    { th->info = NULL;
      if ( th->alias && !info->is_engine )
	unalias_thread(th);
    }

    if ( info->detached && !info->is_engine )
      PL_unregister_atom(info->symbol);
  }

  if ( (rec_rv=info->return_value) )	/* sync with unify_thread_status() */
    info->return_value = NULL;
  if ( (rec_g=info->goal) )
    info->goal = NULL;

  if ( info->pl_tid == GD->thread.highest_id )
  { int i;

    for(i=info->pl_tid-1; i>1; i--)
    { PL_thread_info_t *ih = GD->thread.threads[i];
      if ( ih && ih->status != PL_THREAD_UNUSED )
	break;
    }

    GD->thread.highest_id = i;
  }
  PL_UNLOCK(L_THREAD);

  do
  { freelist = GD->thread.free;
    info->next_free = freelist;
  } while( !COMPARE_AND_SWAP_PTR(&GD->thread.free, freelist, info) );

  if ( rec_rv ) PL_erase(rec_rv);
  if ( rec_g )  PL_erase(rec_g);
}


static int
pthread_join_interruptible(pthread_t thread, void **retval)
{
#ifdef HAVE_PTHREAD_TIMEDJOIN_NP
  for(;;)
  { struct timespec deadline;
    int rc;

    get_current_timespec(&deadline);
    deadline.tv_nsec += 250000000;
    carry_timespec_nanos(&deadline);

    if ( (rc=pthread_timedjoin_np(thread, retval, &deadline)) == ETIMEDOUT )
    { if ( PL_handle_signals() < 0 )
	return EINTR;
    } else
      return rc;
  }
#else
  return pthread_join(thread, retval);
#endif
}



static
PRED_IMPL("thread_join", 2, thread_join, 0)
{ PRED_LD
  PL_thread_info_t *info;
  void *r;
  word rval;
  int rc;
  thread_status status;

  term_t thread = A1;
  term_t retcode = A2;

  PL_LOCK(L_THREAD);
  if ( !get_thread(thread, &info, TRUE) )
  { PL_UNLOCK(L_THREAD);
    return FALSE;
  }

  if ( info == LD->thread.info ||
       info->detached ||
       !COMPARE_AND_SWAP_INT(&info->joining_by, 0, PL_thread_self()) )
  { return PL_error("thread_join", 2,
		    info->joining_by ? "Already being joined" :
		    info->detached   ? "Cannot join detached thread"
				     : "Cannot join self",
		    ERR_PERMISSION, ATOM_join, ATOM_thread, thread);
  }
  PL_UNLOCK(L_THREAD);

  rc = pthread_join_interruptible(info->tid, &r);

  if ( rc )
  { info->joining_by = 0;

    switch(rc)
    { case EINTR:
	return FALSE;
      case ESRCH:
	Sdprintf("Join %s: ESRCH from %d\n",
		 threadName(info->pl_tid), info->tid);
	return PL_error("thread_join", 2, NULL,
			ERR_EXISTENCE, ATOM_thread, thread);
      default:
	return PL_error("thread_join", 2, ThError(rc),
			ERR_SYSCALL, "pthread_join");
    }
  }

  status = info->status;
  if ( !THREAD_STATUS_INVALID(status) &&
       COMPARE_AND_SWAP_INT((int*)&info->status, (int)status, (int)PL_THREAD_JOINED) )
  { rval = unify_thread_status(retcode, info, status, FALSE);

    info->joining_by = 0;
    free_thread_info(info);
  } else
  { info->joining_by = 0;		/* Cannot happen anymore (I think) */
    rval = PL_error(NULL, 0, "already joined",
		    ERR_EXISTENCE, ATOM_thread, thread);
  }

  return rval;
}


word
pl_thread_exit(term_t retcode)
{ GET_LD
  PL_thread_info_t *info = LD->thread.info;

  PL_LOCK(L_THREAD);
  info->status = PL_THREAD_EXITED;
  info->return_value = PL_record(retcode);
  PL_UNLOCK(L_THREAD);

  DEBUG(MSG_THREAD, Sdprintf("thread_exit(%d)\n", info->pl_tid));

  for(QueryFrame qf=LD->query; qf; qf = qf->parent)
    freeHeap(qf->qid, sizeof(*qf->qid));

  pthread_exit(NULL);
  assert(0);
  fail;
}


static
PRED_IMPL("thread_detach", 1, thread_detach, 0)
{ PL_thread_info_t *info;
  PL_thread_info_t *release = NULL;

  PL_LOCK(L_THREAD);
  if ( !get_thread(A1, &info, TRUE) )
  { PL_UNLOCK(L_THREAD);
    fail;
  }

  if ( !info->detached )
  { int rc;

    if ( info->joining_by )
    { PL_UNLOCK(L_THREAD);
      return PL_error(NULL, 0, "Thread is being joined",
		      ERR_PERMISSION,
		      ATOM_detach, ATOM_thread, A1);
    }

    if ( (rc=pthread_detach(info->tid)) )
    { assert(rc == ESRCH);

      release = info;
    } else
    { PL_register_atom(info->symbol);
      info->detached = TRUE;
    }
  }

  PL_UNLOCK(L_THREAD);

  if ( release )
    free_thread_info(release);

  succeed;
}


static
PRED_IMPL("thread_alias", 1, thread_alias, 0)
{ PRED_LD
  PL_thread_info_t *info = LD->thread.info;
  thread_handle *th;
  atom_t alias;

  if ( (th = create_thread_handle(info)) &&
       th->alias )
  { term_t ex = PL_new_term_ref();

    return ( unify_thread_id(ex, info) &&
	     PL_permission_error("re-alias", "thread", ex) );
  }

  return ( PL_get_atom_ex(A1, &alias) &&
	   aliasThread(PL_thread_self(), ATOM_thread, alias) );
}


static size_t
sizeof_thread(PL_thread_info_t *info)
{ size_t size = sizeof(*info);
  struct PL_local_data *ld = info->thread_data;

  if ( info->status != PL_THREAD_RUNNING )
    return 0;

  if ( ld )
  { size += sizeof(*ld);
    size += sizeStackP(&ld->stacks.global)   + ld->stacks.global.spare;
    size += sizeStackP(&ld->stacks.local)    + ld->stacks.local.spare;
    size += sizeStackP(&ld->stacks.trail)    + ld->stacks.trail.spare;
    size += sizeStackP(&ld->stacks.argument);

    size += sizeof_message_queue(&ld->thread.messages);
    size += sizeof_local_definitions(ld);

    if ( ld->tabling.node_pool )
      size += ld->tabling.node_pool->size;
  }

  return size;
}


		 /*******************************
		 *	  THREAD PROPERTY	*
		 *******************************/

static atom_t
symbol_alias(atom_t symbol)
{ thread_handle *th;

  if ( (th=symbol_thread_handle(symbol)) )
    return th->alias;

  return NULL_ATOM;
}

#define thread_id_propery(info, prop) LDFUNC(thread_id_propery, info, prop)
static int
thread_id_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ return PL_unify_integer(prop, info->pl_tid);
}

#define thread_alias_propery(info, prop) LDFUNC(thread_alias_propery, info, prop)
static int
thread_alias_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ atom_t symbol, alias;

  if ( (symbol=info->symbol) &&
       (alias=symbol_alias(symbol)) )
    return PL_unify_atom(prop, alias);

  fail;
}

#define thread_status_propery(info, prop) LDFUNC(thread_status_propery, info, prop)
static int
thread_status_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ IGNORE_LD

  return unify_thread_status(prop, info, info->status, TRUE);
}

#define thread_detached_propery(info, prop) LDFUNC(thread_detached_propery, info, prop)
static int
thread_detached_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ IGNORE_LD

  return PL_unify_bool_ex(prop, info->detached);
}

#define thread_debug_propery(info, prop) LDFUNC(thread_debug_propery, info, prop)
static int
thread_debug_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ IGNORE_LD

  return PL_unify_bool_ex(prop, info->debug);
}

#define thread_engine_propery(info, prop) LDFUNC(thread_engine_propery, info, prop)
static int
thread_engine_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ IGNORE_LD

  return PL_unify_bool_ex(prop, info->is_engine);
}

#define thread_thread_propery(info, prop) LDFUNC(thread_thread_propery, info, prop)
static int
thread_thread_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ if ( info->is_engine )
  { thread_handle *th = symbol_thread_handle(info->symbol);

    if ( th->interactor.thread )
    { atom_t alias = symbol_alias(th->interactor.thread);

      return PL_unify_atom(prop, alias ? alias : th->interactor.thread);
    }
  }

  return FALSE;
}

#define thread_tid_propery(info, prop) LDFUNC(thread_tid_propery, info, prop)
static int
thread_tid_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ IGNORE_LD

  if ( info->has_tid )
  { intptr_t tid = system_thread_id(info);

    if ( tid != -1 )
      return PL_unify_integer(prop, system_thread_id(info));
  }

  return FALSE;
}

#define thread_size_propery(info, prop) LDFUNC(thread_size_propery, info, prop)
static int
thread_size_propery(DECL_LD PL_thread_info_t *info, term_t prop)
{ size_t size;

  PL_LOCK(L_THREAD);
  size = sizeof_thread(info);
  PL_UNLOCK(L_THREAD);

  if ( size )
    return PL_unify_int64(prop, size);

  return FALSE;
}

static const tprop tprop_list [] =
{ { FUNCTOR_id1,	       LDFUNC_REF(thread_id_propery) },
  { FUNCTOR_alias1,	       LDFUNC_REF(thread_alias_propery) },
  { FUNCTOR_status1,	       LDFUNC_REF(thread_status_propery) },
  { FUNCTOR_detached1,	       LDFUNC_REF(thread_detached_propery) },
  { FUNCTOR_debug1,	       LDFUNC_REF(thread_debug_propery) },
  { FUNCTOR_engine1,	       LDFUNC_REF(thread_engine_propery) },
  { FUNCTOR_thread1,	       LDFUNC_REF(thread_thread_propery) },
  { FUNCTOR_system_thread_id1, LDFUNC_REF(thread_tid_propery) },
  { FUNCTOR_size1,	       LDFUNC_REF(thread_size_propery) },
  { 0,			       NULL }
};


typedef struct
{ int		tid;
  const tprop  *p;
  int		enum_threads;
  int		enum_properties;
} tprop_enum;


static int
advance_state(tprop_enum *state)
{ if ( state->enum_properties )
  { state->p++;
    if ( state->p->functor )
      succeed;

    state->p = tprop_list;
  }
  if ( state->enum_threads )
  { do
    { state->tid++;
      if ( state->tid > GD->thread.highest_id )
	fail;
    } while ( GD->thread.threads[state->tid]->status == PL_THREAD_UNUSED ||
              GD->thread.threads[state->tid]->status == PL_THREAD_RESERVED );

    succeed;
  }

  fail;
}


static
PRED_IMPL("thread_property", 2, thread_property, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  term_t thread = A1;
  term_t property = A2;
  tprop_enum statebuf;
  tprop_enum *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { PL_thread_info_t *info;

      memset(&statebuf, 0, sizeof(statebuf));
      state = &statebuf;

      if ( PL_is_variable(thread) )
      { switch( get_prop_def(property, ATOM_thread_property,
			     tprop_list, &statebuf.p) )
	{ case 1:
	    state->tid = 1;
	    state->enum_threads = TRUE;
	    goto enumerate;
	  case 0:
	    state->p = tprop_list;
	    state->tid = 1;
	    state->enum_threads = TRUE;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    fail;
	}
      } else if ( get_thread(thread, &info, TRUE) )
      { state->tid = info->pl_tid;

	switch( get_prop_def(property, ATOM_thread_property,
			     tprop_list, &statebuf.p) )
	{ case 1:
	    goto enumerate;
	  case 0:
	    state->p = tprop_list;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    fail;
	}
      } else
      { fail;
      }
    }
    case FRG_REDO:
      state = CTX_PTR;
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      freeForeignState(state, sizeof(*state));
      succeed;
    default:
      assert(0);
      fail;
  }

enumerate:
  { term_t arg = PL_new_term_ref();

    if ( !state->enum_properties )
      _PL_get_arg(1, property, arg);

    for(;;)
    { PL_thread_info_t *info = GD->thread.threads[state->tid];

      if ( info && LDFUNCP(*state->p->function)(info, arg) )
      { if ( state->enum_properties )
	{ if ( !PL_unify_term(property,
			      PL_FUNCTOR, state->p->functor,
			        PL_TERM, arg) )
	    goto error;
	}
	if ( state->enum_threads )
	{ if ( !unify_thread_id(thread, info) )
	    goto error;
	}

	if ( advance_state(state) )
	{ if ( state == &statebuf )
	  { tprop_enum *copy = allocForeignState(sizeof(*copy));

	    *copy = *state;
	    state = copy;
	  }

	  ForeignRedoPtr(state);
	}

	if ( state != &statebuf )
	  freeForeignState(state, sizeof(*state));
	succeed;
      }

      if ( !advance_state(state) )
      { error:
	if ( state != &statebuf )
	  freeForeignState(state, sizeof(*state));
	fail;
      }
    }
  }
}


static
PRED_IMPL("is_thread", 1, is_thread, 0)
{ PL_thread_info_t *info;

  return get_thread(A1, &info, FALSE);
}


static
PRED_IMPL("thread_setconcurrency", 2, thread_setconcurrency, 0)
{ PRED_LD

#ifdef HAVE_PTHREAD_SETCONCURRENCY
  int val = pthread_getconcurrency();
  int rc;

  if ( PL_unify_integer(A1, val) )
  { if ( PL_compare(A1, A2) != 0  )
    { if ( PL_get_integer_ex(A2, &val) )
      { if ( (rc=pthread_setconcurrency(val)) != 0 )
	  return PL_error(NULL, 0, ThError(rc),
			  ERR_SYSCALL, "pthread_setconcurrency");
      }
    }
  }

  succeed;
#else
  return PL_unify_integer(A1, 0);
#endif
}

#if defined(HAVE_SCHED_SETAFFINITY) && defined(PID_IDENTIFIES_THREAD)
#define HAVE_PRED_THREAD_AFFINITY 1
static
PRED_IMPL("thread_affinity", 3, thread_affinity, 0)
{ PRED_LD
  PL_thread_info_t *info;
  int rc;

  PL_LOCK(L_THREAD);
  if ( (rc=get_thread(A1, &info, TRUE)) )
  { cpu_set_t cpuset;

    if ( (rc=sched_getaffinity(info->pid, sizeof(cpuset), &cpuset)) == 0 )
    { int count = CPU_COUNT(&cpuset);
      int i, n;
      term_t tail = PL_copy_term_ref(A2);
      term_t head = PL_new_term_ref();

      for(i=0, n=0; n<count; i++)
      { if ( CPU_ISSET(i, &cpuset) )
	{ n++;
	  if ( !PL_unify_list_ex(tail, head, tail) ||
	       !PL_unify_integer(head, i) )
	    goto error;			/* rc == 0 (FALSE) */
	}
      }
      if ( !PL_unify_nil_ex(tail) )
	goto error;			/* rc == 0 (FALSE) */
    } else
    { rc = PL_error(NULL, 0, ThError(rc),
		    ERR_SYSCALL, "sched_getaffinity");
      goto error;
    }

    if ( PL_compare(A2, A3) != 0 )
    { if ( (rc=get_cpuset(A3, &cpuset)) )
      { if ( (rc=sched_setaffinity(info->pid, sizeof(cpuset), &cpuset)) == 0 )
	{ rc = TRUE;
	} else
	{ rc = PL_error(NULL, 0, ThError(rc),
			ERR_SYSCALL, "sched_setaffinity");
	}
      }
    } else
    { rc = TRUE;
    }
  }
error:
  PL_UNLOCK(L_THREAD);

  return rc;
}
#endif /*HAVE_SCHED_SETAFFINITY*/


		 /*******************************
		 *	     CLEANUP		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Request a function to run when the Prolog thread is about to detach, but
still capable of running Prolog queries.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_thread_at_exit(void (*function)(void *), void *closure, int global)
{ GET_LD
  event_list **list = global ? &GD->event.hook.onthreadexit
			     : &LD->event.hook.onthreadexit;
  int (*func)() = (void *)function;

  return register_event_function(list, 0, FALSE, func, closure, 0);
}

		 /*******************************
		 *	   THREAD SIGNALS	*
		 *******************************/

typedef struct _thread_sig
{ struct _thread_sig *next;		/* Next in queue */
  Module   module;			/* Module for running goal */
  record_t goal;			/* Goal to run */
  int	   blocked;			/* Signal is blocked */
} thread_sig;


static int
is_alive(int status)
{ switch(status)
  { case PL_THREAD_CREATED:
    case PL_THREAD_RUNNING:
      succeed;
    default:
      fail;
  }
}


static
PRED_IMPL("thread_signal", 2, thread_signal, META|PL_FA_ISO)
{ PRED_LD
  Module m = NULL;
  thread_sig *sg = NULL;
  PL_thread_info_t *info;
  PL_local_data_t *ld;
  int rc;

  term_t thread = A1;
  term_t goal   = A2;

  if ( !PL_strip_module(goal, &m, goal) )
    return FALSE;

  sg = allocHeapOrHalt(sizeof(*sg));
  sg->next    = NULL;
  sg->module  = m;
  sg->goal    = PL_record(goal);
  sg->blocked = FALSE;

  PL_LOCK(L_THREAD);
  if ( !(rc=get_thread(thread, &info, TRUE)) )
    goto out;
  if ( !(rc=is_alive(info->status)) )
  { error:
    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_thread, thread);
    goto out;
  }

  ld = info->thread_data;
  if ( !ld->thread.sig_head )
  { ld->thread.sig_head = ld->thread.sig_tail = sg;
  } else
  { ld->thread.sig_tail->next = sg;
    ld->thread.sig_tail = sg;
  }
  sg = NULL;
  raiseSignal(ld, SIG_THREAD_SIGNAL);
  if ( info->has_tid && !alertThread(info) )
    goto error;

out:
  if ( sg )
  { PL_erase(sg->goal);
    freeHeap(sg, sizeof(*sg));
  }
  PL_UNLOCK(L_THREAD);

  return rc;
}


void
updatePendingThreadSignals(DECL_LD)
{ thread_sig *sg;

  for(sg=LD->thread.sig_head; sg; sg=sg->next)
  { if ( !sg->blocked )
    { raiseSignal(LD, SIG_THREAD_SIGNAL);
      break;
    }
  }
}


struct siglist
{ term_t list;
  term_t tail;
  term_t head;
  term_t goal;
  Module m;
};


#define append_signal(sg, sl) \
	LDFUNC(append_signal, sg, sl)

static int
append_signal(DECL_LD thread_sig *sg, struct siglist *sl)
{ if ( !sl->tail )
  { if ( !(sl->tail = PL_copy_term_ref(sl->list)) ||
	 !(sl->head=PL_new_term_ref()) ||
	 !(sl->goal=PL_new_term_ref()) ||
	 !PL_strip_module(sl->tail, &sl->m, sl->tail) )
      return FALSE;
  }

  if ( sg->module == sl->m )
    return ( PL_unify_list(sl->tail, sl->head, sl->tail) &&
	     PL_recorded(sg->goal, sl->goal) &&
	     PL_unify(sl->goal, sl->head) );
  else
    return ( PL_put_atom(sl->head, sg->module->name) &&
	     PL_recorded(sg->goal, sl->goal) &&
	     PL_cons_functor(sl->goal, FUNCTOR_colon2, sl->head, sl->goal) &&
	     PL_unify_list(sl->tail, sl->head, sl->tail) &&
	     PL_unify(sl->goal, sl->head) );
}


#define close_signals(sl) \
	LDFUNC(close_signals, sl)

static int
close_signals(DECL_LD struct siglist *sl)
{ return PL_unify_nil(sl->tail ? sl->tail : sl->list);
}


static
PRED_IMPL("sig_pending", 1, sig_pending, META)
{ PRED_LD

  if ( LD->thread.sig_head )
  { struct siglist sl = {0};
    thread_sig *sg;
    int rc;

    sl.list = A1;

    PL_LOCK(L_THREAD);
    for( sg=LD->thread.sig_head; sg; sg = sg->next )
    { if ( !(rc=append_signal(sg, &sl)) )
	break;
    }
    PL_UNLOCK(L_THREAD);

    return rc && close_signals(&sl);
  } else
    return PL_unify_nil(A1);
}

static
PRED_IMPL("sig_remove", 2, sig_remove, META)
{ PRED_LD
  Module m = NULL;
  term_t pattern = PL_new_term_ref();
  term_t ex = PL_new_term_ref();
  term_t tmp = 0;
  int rc = TRUE;
  thread_sig *sg, *prev = NULL, *next;
  struct siglist sl = {0};

  if ( !PL_strip_module(A1, &m, pattern) )
    return FALSE;
  sl.list = A2;

  PL_LOCK(L_THREAD);
  for( sg=LD->thread.sig_head; sg; prev=sg, sg = next )
  { next = sg->next;

    if ( sg->module == m )
    { if ( !tmp && !(tmp=PL_new_term_ref()) )
      { rc = FALSE;
	break;
      }
      if ( PL_recorded(sg->goal, tmp) &&
	   can_unify(valTermRef(pattern), valTermRef(tmp), ex) )
      { thread_sig *rm = sg;

	if ( !(rc=append_signal(sg, &sl)) )
	  break;

	sg = prev;			/* do not update prev */
	if ( prev )
	{ prev->next = next;
	} else
	{ LD->thread.sig_head = next;
	  if ( !LD->thread.sig_head )
	    LD->thread.sig_tail = NULL;
	}

	PL_erase(rm->goal);
	freeHeap(rm, sizeof(*rm));
      } else if ( PL_exception(0) )
      { rc = FALSE;
	break;
      } else if ( !PL_is_variable(ex) )
      { rc = PL_raise_exception(ex);
	break;
      }
    }
  }
  PL_UNLOCK(L_THREAD);

  return rc && close_signals(&sl);
}

#define unblock_signals(_) \
	LDFUNC(unblock_signals, _)

static void
unblock_signals(DECL_LD)
{ thread_sig *sg;
  int unblocked = 0;

  if ( (sg=LD->thread.sig_head) )
  { for(; sg; sg = sg->next)
    { if ( sg->blocked )
      { sg->blocked = FALSE;
	unblocked++;
      }
    }
  }

  DEBUG(MSG_THREAD_SIGNAL,
	Sdprintf("Unblocked %d signals\n", unblocked));

  if  ( unblocked )
    raiseSignal(LD, SIG_THREAD_SIGNAL);
}

#define signal_is_blocked(sg) \
	LDFUNC(signal_is_blocked, sg)

static int
signal_is_blocked(DECL_LD thread_sig *sg)
{ predicate_t pred;
  fid_t fid;
  int rc = FALSE;

  pred = _PL_predicate("signal_is_blocked", 1, "$syspreds",
		       &GD->procedures.signal_is_blocked1);

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av = PL_new_term_refs(1);
    term_t m  = PL_new_term_ref();

    startCritical();
    rc = ( PL_put_atom(m, sg->module->name) &&
	   PL_recorded(sg->goal, av+0) &&
	   PL_cons_functor(av+0, FUNCTOR_colon2, m, av+0) &&
	   PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av)
	 );
    rc = endCritical() && rc;

    if ( rc )
    { DEBUG(MSG_THREAD_SIGNAL,
	    { Sdprintf("Blocked signal: ");
	      PL_write_term(Serror, av+0, 1200,
			    PL_WRT_QUOTED|PL_WRT_NEWLINE);
	    });
      sg->blocked = TRUE;
    }

    PL_discard_foreign_frame(fid);
  }

  return rc;
}

static
PRED_IMPL("$sig_unblock", 0, sig_unblock, 0)
{ PRED_LD

  unblock_signals();

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Execute pending signals on this thread. If the thread is no longer alive
all signals are ignored. This routine   returns after having handled all
signals or after an exception was raised while copying the signal to the
stack or executing it. No signals are processed while we are executing a
signal handler. Of course, new signals may arrive.

Note that during the  execution  new  signals   may  be  added  by other
threads. Other manipulation of the signal queue can only be done by this
thread, but may happen as part of the signal execution.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
executeThreadSignals(int sig)
{ GET_LD
  fid_t fid;
  (void)sig;
  term_t goal;
  thread_sig *sg, *prev = NULL, *next;

  if ( !is_alive(LD->thread.info->status) ||
       !(sg=LD->thread.sig_head) ||
       !(fid=PL_open_foreign_frame()) ||
       !(goal=PL_new_term_ref()) )
    return;

  for( ; sg; prev=sg, sg=next )
  { Module gm;
    term_t ex;
    int rval = FALSE;

    prev=sg;
    next=sg->next;

    if ( sg->blocked ||
	 signal_is_blocked(sg) )
      continue;

    PL_LOCK(L_THREAD);
    if ( sg == LD->thread.sig_head )
    { if ( !(LD->thread.sig_head = sg->next) )
	LD->thread.sig_tail = NULL;
    } else
    { prev->next = sg->next;
      if ( sg == LD->thread.sig_tail )
	LD->thread.sig_tail = prev;
    }
    PL_UNLOCK(L_THREAD);

    gm = sg->module;
    rval = PL_recorded(sg->goal, goal);
    PL_erase(sg->goal);
    freeHeap(sg, sizeof(*sg));
    sg = prev;

    if ( !rval )
      return;				/* No signal or no space to handle */

    DEBUG(MSG_THREAD,
	  Sdprintf("[%d] Executing thread signal\n", PL_thread_self()));

    {
#ifdef O_LIMIT_DEPTH
      size_t olimit = LD->depth_info.limit;
      LD->depth_info.limit = DEPTH_NO_LIMIT;
#endif
      startCritical();
      rval = callProlog(gm, goal, PL_Q_CATCH_EXCEPTION, &ex);
      rval = endCritical() && rval;
#ifdef O_LIMIT_DEPTH
      LD->depth_info.limit = olimit;
#endif
    }

    if ( !rval && ex )
    { PL_raise_exception(ex);
      PL_close_foreign_frame(fid);

      DEBUG(MSG_THREAD,
	    { Sdprintf("[%d]: Thread signal raised exception. Backtrace:\n",
		       PL_thread_self());
	      PL_backtrace(5, 0);
	      Sdprintf("[%d]: end Prolog backtrace\n", PL_thread_self());
	    });

      return;
    }
  }

  PL_close_foreign_frame(fid);
}


static void
freeThreadSignals(PL_local_data_t *ld)
{ thread_sig *sg;
  thread_sig *next;

  for( sg = ld->thread.sig_head; sg; sg = next )
  { next = sg->next;

    PL_erase(sg->goal);
    freeHeap(sg, sizeof(*sg));
  }
}


		 /*******************************
		 *	    INTERACTORS		*
		 *******************************/

#define get_interactor(t, thp, warn) LDFUNC(get_interactor, t, thp, warn)
static int
get_interactor(DECL_LD term_t t, thread_handle **thp, int warn)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  { thread_handle *th;
    atom_t symbol = a;

  from_symbol:
    if ( (th=symbol_thread_handle(symbol)) &&
	 true(th, TH_IS_INTERACTOR) )
    { if ( th->info || true(th, (TH_INTERACTOR_NOMORE|TH_INTERACTOR_DONE)) )
      { *thp = th;
	return TRUE;
      }
      if ( warn )
	PL_existence_error("engine", t);
    } else if ( isTextAtom(symbol) )
    { word w;

      if ( (w = (word)lookupHTable(threadTable, (void *)symbol)) )
      { symbol = w;
	goto from_symbol;
      }
      if ( warn )
	PL_existence_error("engine", t);
    } else
    { if ( warn )
	PL_type_error("engine", t);
    }
  } else
  { if ( warn )
      PL_type_error("engine", t);
  }

  return FALSE;
}


/** '$engine_create'(-Handle, +GoalAndTemplate, +Options)
*/

static const opt_spec make_engine_options[] =
{ { ATOM_stack_limit,	OPT_SIZE|OPT_INF },
  { ATOM_alias,		OPT_ATOM },
  { ATOM_inherit_from,	OPT_TERM },
  { NULL_ATOM,		0 }
};


static
PRED_IMPL("$engine_create", 3, engine_create, 0)
{ PRED_LD
  PL_engine_t new;
  PL_thread_attr_t attrs;
  size_t stack	      =	0;
  atom_t alias	      =	NULL_ATOM;
  term_t inherit_from =	0;

  memset(&attrs, 0, sizeof(attrs));
  if ( !scan_options(A3, 0,
		     ATOM_engine_option, make_engine_options,
		     &stack,
		     &alias,
		     &inherit_from) )
    return FALSE;

  if ( stack )
    attrs.stack_limit = stack;
  else
    attrs.stack_limit = LD->stacks.limit;

  if ( (new = PL_create_engine(&attrs)) )
  { PL_engine_t me;
    predicate_t pred;
    record_t r;
    thread_handle *th;
    term_t t;
    int rc;

    new->thread.info->is_engine = TRUE;
    th = create_thread_handle(new->thread.info);
    set(th, TH_IS_INTERACTOR);
    ATOMIC_INC(&GD->statistics.engines_created);

    if ( alias )
    { if ( !aliasThread(new->thread.info->pl_tid, ATOM_engine, alias) )
      { destroy_interactor(th, FALSE);
	return FALSE;
      }
    }

    if ( !(rc = unify_thread_id(A1, new->thread.info)) )
    { destroy_interactor(th, FALSE);
      if ( !PL_exception(0) )
	return PL_uninstantiation_error(A1);

      return FALSE;
    }
    PL_unregister_atom(th->symbol);

    pred = _PL_predicate("call", 1, "system", &GD->procedures.call1);

    r = PL_record(A2);
    rc = PL_set_engine(new, &me);
    assert(rc == PL_ENGINE_SET);

    WITH_LD(new)
    { if  ( (t = PL_new_term_ref()) &&
	    (th->interactor.argv = PL_new_term_refs(2)) &&
	    PL_recorded(r, t) &&
	    PL_get_arg(1, t, th->interactor.argv+0) &&
	    PL_get_arg(2, t, th->interactor.argv+1) )
      { th->interactor.query = PL_open_query(NULL,
					     PL_Q_CATCH_EXCEPTION|
					     PL_Q_ALLOW_YIELD|
					     PL_Q_EXT_STATUS,
					     pred, th->interactor.argv+1);
	PL_set_engine(me, NULL);
      } else
      { assert(0);			/* TBD: copy exception */
      }
    }

    PL_erase(r);

    return TRUE;
  }

  return PL_no_memory();
}


static void
destroy_interactor(thread_handle *th, int gc)
{ if ( th->interactor.query )
  { PL_engine_t me;

    PL_set_engine(th->info->thread_data, &me);
    PL_close_query(th->interactor.query);
    PL_set_engine(me, NULL);
    th->interactor.query = 0;
  }
  if ( th->info )
  { PL_destroy_engine(th->info->thread_data);
    ATOMIC_INC(&GD->statistics.engines_finished);
    assert(th->info == NULL || gc);
  }
  if ( th->interactor.package )
  { PL_erase(th->interactor.package);
    th->interactor.package = 0;
  }
  clear(th, (TH_INTERACTOR_NOMORE|TH_INTERACTOR_DONE));
  simpleMutexDelete(th->interactor.mutex);
  th->interactor.mutex = NULL;
  unalias_thread(th);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called when we computed the last result.   This means we can destroy the
Prolog engine, but the reference (and  possible alias) must remain valid
to deal with the engine_destroy/1  or   final  engine_next/1 if the last
answer was deterministic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
done_interactor(thread_handle *th)
{
#ifndef NDEBUG
  GET_LD
  PL_engine_t e = th->info->thread_data;
  assert(e->thread.info->open_count == 1);
  assert(e == LD);
#endif

  set(th, TH_INTERACTOR_DONE);
  PL_thread_destroy_engine();
  ATOMIC_INC(&GD->statistics.engines_finished);
  assert(th->info == NULL);
}


static
PRED_IMPL("engine_destroy", 1, engine_destroy, 0)
{ PRED_LD
  thread_handle *th;

  if ( get_interactor(A1, &th, TRUE) )
  { destroy_interactor(th, FALSE);

    return TRUE;
  }

  return FALSE;
}


static void
copy_debug_mode(PL_local_data_t *to, PL_local_data_t *from)
{ PL_local_data_t *current = PL_current_engine();

  if ( to->_debugstatus.debugging != from->_debugstatus.debugging )
  { TLD_set_LD(to);
    debugmode(from->_debugstatus.debugging, NULL);
    TLD_set_LD(current);
  }
  if ( to->_debugstatus.tracing != from->_debugstatus.tracing )
  { TLD_set_LD(to);
    tracemode(from->_debugstatus.tracing, NULL);
    TLD_set_LD(current);
  }
}


static PL_local_data_t *
activate_interactor(thread_handle *th)
{ PL_local_data_t *ld = th->info->thread_data;

  TLD_set_LD(ld);
  ld->thread.info->tid = pthread_self();
  ld->thread.info->has_tid = TRUE;
  set_system_thread_id(ld->thread.info);

  return ld;
}


static PL_local_data_t *
suspend_interactor(PL_engine_t me, thread_handle *th)
{ if ( th->info )
    detach_engine(th->info->thread_data);
  TLD_set_LD(me);

  return me;
}


#define YIELD_ENGINE_YIELD  256		/* keep in sync with engine_yield/1 */

#define interactor_post_answer_nolock(th, ref, package, term) LDFUNC(interactor_post_answer_nolock, th, ref, package, term)
static int
interactor_post_answer_nolock(DECL_LD thread_handle *th,
			      term_t ref, term_t package, term_t term)
{ PL_engine_t me = LD;

  if ( package && th->interactor.package )
    return PL_permission_error("post_to", "engine", ref);

  if ( !th->interactor.query )
  { if ( true(th, TH_INTERACTOR_NOMORE) )
    { clear(th, TH_INTERACTOR_NOMORE);
      set(th, TH_INTERACTOR_DONE);
      return FALSE;
    }
    return PL_existence_error("engine", ref);
  }

  if ( package )
    th->interactor.package = PL_record(package);

  WITH_LD ( activate_interactor(th) )
  { term_t t;
    int rc;
    record_t r;

    if (!LOCAL_LD)
      break; /* WITH_LD is a for() statement and can be broken out of */

    copy_debug_mode(LD, me);
    rc = PL_next_solution(th->interactor.query);

    switch( rc )
    { case PL_S_TRUE:
      { r = PL_record(th->interactor.argv+0);
	break;
      }
      case PL_S_LAST:
      { r = PL_record(th->interactor.argv+0);
	PL_close_query(th->interactor.query);
	th->interactor.query = 0;
	done_interactor(th);
	set(th, TH_INTERACTOR_NOMORE);
	break;
      }
      case PL_S_FALSE:
      { PL_close_query(th->interactor.query);
	th->interactor.query = 0;
	done_interactor(th);
	suspend_interactor(me, th);

	return FALSE;
      }
      case PL_S_EXCEPTION:
      { record_t r = PL_record(PL_exception(th->interactor.query));
	term_t ex;
	int rc;

	PL_close_query(th->interactor.query);
	th->interactor.query = 0;
	done_interactor(th);
	WITH_LD(suspend_interactor(me, th))
	{ rc = ( (ex = PL_new_term_ref()) &&
		 PL_recorded(r, ex) &&
		 PL_raise_exception(ex) );

	  PL_erase(r);
	}
	return rc;
      }
      case YIELD_ENGINE_YIELD:			/* engine_yield/1 */
      { r = PL_record(PL_yielded(th->interactor.query));
	break;
      }
      default:
      { term_t ex = PL_new_term_ref();

	return ( PL_put_integer(ex, rc) &&
		 PL_domain_error("engine_yield_code", ex) );
      }
    }

    WITH_LD(suspend_interactor(me, th))
    { rc = ( (t=PL_new_term_ref()) &&
	     PL_recorded(r, t) &&
	     PL_unify(term, t) );
      PL_erase(r);
    }

    return rc;
  }

  assert(0);
  return FALSE;
}


static atom_t
thread_symbol(const PL_local_data_t *ld)
{ if ( ld->thread.info->is_engine )
  { const thread_handle *th = symbol_thread_handle(ld->thread.info->symbol);
    return th->interactor.thread;
  } else
  { return ld->thread.info->symbol;
  }
}


#define interactor_post_answer(ref, package, term) LDFUNC(interactor_post_answer, ref, package, term)
static int
interactor_post_answer(DECL_LD term_t ref, term_t package, term_t term)
{ thread_handle *th;

  if ( get_interactor(ref, &th, TRUE) )
  { int rc;

    simpleMutexLock(th->interactor.mutex);
    th->interactor.thread = thread_symbol(LD);
    rc = interactor_post_answer_nolock(th, ref, package, term);
    th->interactor.thread = NULL_ATOM;
    simpleMutexUnlock(th->interactor.mutex);

    return rc;
  }

  return FALSE;
}


/** engine_next(+Engine, -Next)
*/

static
PRED_IMPL("engine_next", 2, engine_next, 0)
{ PRED_LD

  return interactor_post_answer(A1, 0, A2);
}

/** engine_post(+Engine, +Term)
*/

static
PRED_IMPL("engine_post", 2, engine_post, 0)
{ PRED_LD
  thread_handle *th;

  if ( get_interactor(A1, &th, TRUE) )
  { int rc;

    simpleMutexLock(th->interactor.mutex);
    if ( !th->interactor.package )
    { if ( (th->interactor.package = PL_record(A2)) )
	rc = TRUE;
      else
	rc = FALSE;
    } else
    { rc = PL_permission_error("post_to", "engine", A1);
    }
    simpleMutexUnlock(th->interactor.mutex);

    return rc;
  }

  return FALSE;
}

/** engine_post(+Engine, +Term, -Next)
*/

static
PRED_IMPL("engine_post", 3, engine_post, 0)
{ PRED_LD

  return interactor_post_answer(A1, A2, A3);
}


static
PRED_IMPL("engine_fetch", 1, engine_fetch, 0)
{ PRED_LD
  PL_thread_info_t *info = LD->thread.info;
  term_t exv;

  if ( info->is_engine )
  { thread_handle *th = symbol_thread_handle(info->symbol);

    if ( th->interactor.package )
    { term_t tmp;
      int rc;

      rc = ( (tmp = PL_new_term_ref()) &&
	     PL_recorded(th->interactor.package, tmp) &&
	     PL_unify(A1, tmp) );
      PL_erase(th->interactor.package);
      th->interactor.package = 0;

      return rc;
    }
  }

  return ( (exv = PL_new_term_refs(2)) &&
	    PL_unify_atom_chars(exv+0, "delivery") &&
	    unify_thread_id(exv+1, info) &&
	    PL_error("engine_fetch", 1, "Use engine_post/2,3", ERR_EXISTENCE3,
		     ATOM_term, exv+0, exv+1) );
}


static
PRED_IMPL("is_engine", 1, is_engine, 0)
{ PRED_LD
  thread_handle *th;

  return get_interactor(A1, &th, FALSE);
}


		 /*******************************
		 *	  MESSAGE QUEUES	*
		 *******************************/

typedef enum
{ QUEUE_WAIT_READ,			/* wait for message */
  QUEUE_WAIT_DRAIN			/* wait for queue to drain */
} queue_wait_type;

#define MSG_WAIT_INTR		(-1)
#define MSG_WAIT_TIMEOUT	(-2)
#define MSG_WAIT_DESTROYED	(-3)

#define dispatch_cond_wait(queue, wait, deadline) LDFUNC(dispatch_cond_wait, queue, wait, deadline)
static int dispatch_cond_wait(DECL_LD message_queue *queue,
			      queue_wait_type wait,
			      struct timespec *deadline);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code deals with telling other threads something.  The interface:

	thread_get_message(-Message)
	thread_send_message(+Id, +Message)

Queues can be waited for by   multiple  threads using different (partly)
instantiated patterns for Message. For this   reason all waiting threads
should be restarted using pthread_cond_broadcast().   However,  if there
are a large number of workers only   waiting for `any' message this will
cause all of them to wakeup for only   one  to grab the message. This is
highly undesirable and therefore the queue  keeps two counts: the number
of waiting threads and the number waiting  with a variable. Only if they
are not equal and there are multiple waiters we must be using broadcast.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct thread_message
{ struct thread_message *next;		/* next in queue */
  record_t            message;		/* message in queue */
  word		      key;		/* Indexing key */
  uint64_t	      sequence_id;	/* Numbered sequence */
} thread_message;


#define create_thread_message(msg) LDFUNC(create_thread_message, msg)
static thread_message *
create_thread_message(DECL_LD term_t msg)
{ thread_message *msgp;
  record_t rec;

  if ( !(rec=compileTermToHeap(msg, R_NOLOCK)) )
    return NULL;

  if ( (msgp = allocHeap(sizeof(*msgp))) )
  { msgp->next    = NULL;
    msgp->message = rec;
    msgp->key     = getIndexOfTerm(msg);
  } else
  { freeRecord(rec);
  }

  return msgp;
}


static void
free_thread_message(thread_message *msg)
{ if ( msg->message )
    freeRecord(msg->message);

  freeHeap(msg, sizeof(*msg));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
queue_message() adds a message to a message queue.  The caller must hold
the queue-mutex.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define queue_message(queue, msgp, deadline) LDFUNC(queue_message, queue, msgp, deadline)
static int
queue_message(DECL_LD message_queue *queue, thread_message *msgp,
	      struct timespec *deadline)
{ if ( queue->max_size > 0 && queue->size >= queue->max_size )
  { queue->wait_for_drain++;

    while ( queue->size >= queue->max_size )
    { switch ( dispatch_cond_wait(queue, QUEUE_WAIT_DRAIN, deadline) )
      { case CV_INTR:
	{ if ( !LD )			/* needed for clean exit */
	  { Sdprintf("Forced exit from queue_message()\n");
	    exit(1);
	  }

	  if ( is_signalled() )			/* thread-signal */
	  { queue->wait_for_drain--;
	    return MSG_WAIT_INTR;
	  }
	  break;
	}
	case CV_TIMEDOUT:
	  queue->wait_for_drain--;
	  return MSG_WAIT_TIMEOUT;
	case CV_READY:
	case CV_MAYBE:
	  break;
	default:
	  assert(0); // should never happen
      }
      if ( queue->destroyed )
      { queue->wait_for_drain--;
	return MSG_WAIT_DESTROYED;
      }
    }

    queue->wait_for_drain--;
  }

  msgp->sequence_id = ++queue->sequence_next;
  if ( !queue->head )
  { queue->head = queue->tail = msgp;
  } else
  { queue->tail->next = msgp;
    queue->tail = msgp;
  }
  queue->size++;

  if ( queue->waiting )
  { if ( queue->waiting > queue->waiting_var && queue->waiting > 1 )
    { DEBUG(MSG_QUEUE,
	    Sdprintf("%d: %d of %d non-var waiters on %p; broadcasting\n",
		     PL_thread_self(),
		     queue->waiting - queue->waiting_var,
		     queue->waiting,
		     queue));
      cv_broadcast(&queue->cond_var);
    } else
    { DEBUG(MSG_QUEUE, Sdprintf("%d: %d waiters on %p; signalling\n",
				PL_thread_self(), queue->waiting, queue));
      cv_signal(&queue->cond_var);
    }
  } else
  { DEBUG(MSG_QUEUE, Sdprintf("%d: no waiters on %p\n",
			      PL_thread_self(), queue));
  }

  return TRUE;
}


		 /*******************************
		 *     READING FROM A QUEUE	*
		 *******************************/

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#ifdef HAVE_FTIME
#include <sys/timeb.h>
#endif

static void
timespec_set_dbl(struct timespec *spec, double stamp)
{ double ip, fp;

  fp = modf(stamp, &ip);
  spec->tv_sec = (time_t)ip;
  spec->tv_nsec = (long)(fp*1000000000.0);
}

static void
timespec_diff(struct timespec *diff,
	      const struct timespec *a, const struct timespec *b)
{ diff->tv_sec  = a->tv_sec - b->tv_sec;
  diff->tv_nsec = a->tv_nsec - b->tv_nsec;
  if ( diff->tv_nsec < 0 )
  { --diff->tv_sec;
    diff->tv_nsec += 1000000000;
  }
}


static void
timespec_add(struct timespec *spec, const struct timespec *add)
{ spec->tv_sec  += add->tv_sec;
  spec->tv_nsec += add->tv_nsec;
  carry_timespec_nanos(spec);
}


static int
timespec_sign(const struct timespec *t)
{ return ( t->tv_sec > 0 ?  1 :
	   t->tv_sec < 0 ? -1 :
	   t->tv_nsec > 0 ? 1 :
                            0 );

}


static int
timespec_cmp(const struct timespec *a, const struct timespec *b)
{ struct timespec diff;

  timespec_diff(&diff, a, b);

  return timespec_sign(&diff);
}


void
get_current_timespec(struct timespec *time)
{
#ifdef HAVE_CLOCK_GETTIME
  clock_gettime(CLOCK_REALTIME, time);
#else
#ifdef HAVE_GETTIMEOFDAY
  struct timeval now;

  gettimeofday(&now, NULL);
  time->tv_sec  = now.tv_sec;
  time->tv_nsec = now.tv_usec*1000;
#else						/* HAVE_FTIME */
  struct timeb now;

  ftime(&now);
  time->tv_sec  = now.time;
  time->tv_nsec = now.millitm*1000000;
#endif
#endif
}


void
carry_timespec_nanos(struct timespec *time)
{ DEBUG(0, assert(time->tv_nsec >= 0));

  while ( time->tv_nsec >= 1000000000 )
  { time->tv_nsec -= 1000000000;
    time->tv_sec += 1;
  }
}

#ifdef __WINDOWS__

static DWORD
timespec_msecs(const struct timespec *spec)
{ return spec->tv_sec*1000 + (spec->tv_nsec + 1000000 - 1) / 1000000;
}

int
cv_timedwait(message_queue *queue,
	     CONDITION_VARIABLE *cond, CRITICAL_SECTION *mutex,
	     struct timespec *deadline, const struct timespec *retry_every)
{ GET_LD
  struct timespec tmp_timeout;
  DWORD api_timeout;
  int last = FALSE;
  int rc;
  struct timespec retry;

  if ( !retry_every )
  { retry_every   = &retry;
    retry.tv_sec  = 0;
    retry.tv_nsec = 250000000;
  }
  api_timeout = timespec_msecs(retry_every);

  for(;;)
  { get_current_timespec(&tmp_timeout);
    timespec_add(&tmp_timeout, retry_every);

    if ( deadline && timespec_cmp(&tmp_timeout, deadline) >= 0 )
    { struct timespec d;

      get_current_timespec(&tmp_timeout);
      timespec_diff(&d, deadline, &tmp_timeout);
      if ( timespec_sign(&d) > 0 )
	api_timeout = timespec_msecs(&d);
      else
	return CV_TIMEDOUT;

      last = TRUE;
    }

    rc = SleepConditionVariableCS(cond, mutex, api_timeout);

    if ( is_signalled() )
      return CV_INTR;
    if ( !rc )
      return last ? CV_TIMEDOUT : CV_MAYBE;
    if ( rc )
      return CV_READY;
  }
}

#else /*__WINDOWS__*/

/* return: 0: ok, EINTR: interrupted, ETIMEDOUT: timeout
*/

int
cv_timedwait(message_queue *queue,
	     pthread_cond_t *cond, pthread_mutex_t *mutex,
	     struct timespec *deadline, const struct timespec *retry_every)
{ GET_LD
  struct timespec tmp_timeout;
  struct timespec *api_timeout = &tmp_timeout;
  int rc;
  struct timespec retry;

  if ( !retry_every )
  { retry_every = &retry;
    retry.tv_sec = 0;
    retry.tv_nsec = 250000000;
  }

  for(;;)
  { get_current_timespec(&tmp_timeout);
    timespec_add(&tmp_timeout, retry_every);

    if ( deadline && timespec_cmp(&tmp_timeout, deadline) >= 0 )
      api_timeout = deadline;

    rc = pthread_cond_timedwait(cond, mutex, api_timeout);
    DEBUG(MSG_QUEUE_WAIT,
	  Sdprintf("%d: wait on %p returned %d; size = %ld (%s)\n",
		   PL_thread_self(), queue, rc,
		   queue ? (long)queue->size : 0,
		   api_timeout == deadline ? "final" : "0.25sec"));

    switch( rc )
    { case ETIMEDOUT:
	if ( is_signalled() )
	  return CV_INTR;
	if ( api_timeout == deadline )
	  return CV_TIMEDOUT;
	return CV_MAYBE;
      case EINTR:			/* can not happen in POSIX, but can in */
      case 0:				/* legacy systems */
	if ( is_signalled() )
	  return CV_INTR;
        return CV_READY;
      default:
	assert(0);
	return CV_READY;
    }
  }
}

#endif /*__WINDOWS__*/

static int
dispatch_cond_wait(DECL_LD message_queue *queue, queue_wait_type wait,
		   struct timespec *deadline)
{ int rc;

  LD->thread.alert.obj.queue = queue;
  LD->thread.alert.type	     = wait == QUEUE_WAIT_READ ? ALERT_QUEUE_RD
						       : ALERT_QUEUE_WR;

  rc = cv_timedwait(queue,
		    (wait == QUEUE_WAIT_READ ? &queue->cond_var
					     : &queue->drain_var),
		    &queue->mutex,
		    deadline, NULL);

  PL_LOCK(L_ALERT);
  LD->thread.alert.type = 0;
  LD->thread.alert.obj.queue = NULL;
  PL_UNLOCK(L_ALERT);

  return rc;
}

#ifdef O_QUEUE_STATS
static uint64_t getmsg  = 0;
static uint64_t unified = 0;
static uint64_t skipped = 0;

static void
msg_statistics(void)
{ Sdprintf("get_message: %lld, unified: %lld, skipped: %lld\n",
	   getmsg, unified, skipped);
}
#define QSTAT(n) (n++)
#else
#define QSTAT(n) ((void)0)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_message() reads the next message from the  message queue. It must be
called with queue->mutex locked.  It returns one of

	* TRUE
	* FALSE
	* MSG_WAIT_INTR
	  Got EINTR while waiting.
	* MSG_WAIT_TIMEOUT
	  Got timeout while waiting
	* MSG_WAIT_DESTROYED
	  Queue was destroyed while waiting

(*) We need  to lock  because AGC  marks our atoms  while the  thread is
running.  The thread may pick   a  message containing  an atom  from the
queue,  which now has not  been   marked  and is  no longer part  of the
queue, so it isn't marked in the queue either.

Originally, I thought we  only  need  this   for  queues  that  are  not
associated to threads. Those associated  with   a  thread  mark both the
stacks and the queue in one pass,  marking the atoms in either. However,
we also need to lock to avoid  get_message() destroying the record while
markAtomsMessageQueue() scans it. This fixes the reopened Bug#142.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define get_message(queue, msg, deadline) LDFUNC(get_message, queue, msg, deadline)
static int
get_message(DECL_LD message_queue *queue, term_t msg, struct timespec *deadline)
{ int isvar = PL_is_variable(msg) ? 1 : 0;
  word key = (isvar ? 0L : getIndexOfTerm(msg));
  fid_t fid = PL_open_foreign_frame();
  uint64_t seen = 0;

  QSTAT(getmsg);

  DEBUG(MSG_QUEUE,
	{ Sdprintf("%d: get_message(%p) key 0x%lx for ",
		   PL_thread_self(), queue, key);
	  PL_write_term(Serror, msg, 1200, PL_WRT_QUOTED|PL_WRT_NEWLINE);
	});

  for(;;)
  { int rc;
    thread_message *msgp = queue->head;
    thread_message *prev = NULL;

    if ( queue->destroyed )
      return MSG_WAIT_DESTROYED;

    DEBUG(MSG_QUEUE,
	  Sdprintf("%d: queue size=%ld\n",
		   PL_thread_self(), (long)queue->size));

    for( ; msgp; prev = msgp, msgp = msgp->next )
    { term_t tmp;

      if ( msgp->sequence_id < seen )
      { QSTAT(skipped);
	DEBUG(MSG_QUEUE, Sdprintf("Already seen %ld (<%ld)\n",
				  (long)msgp->sequence_id, (long)seen));
	continue;
      }
      seen = msgp->sequence_id;

      if ( key && msgp->key && key != msgp->key )
      { DEBUG(MSG_QUEUE, Sdprintf("Message key mismatch\n"));
	continue;			/* fast search */
      }

      QSTAT(unified);
      tmp = PL_new_term_ref();
      if ( !PL_recorded(msgp->message, tmp) )
      { PL_discard_foreign_frame(fid);
        return raiseStackOverflow(GLOBAL_OVERFLOW);
      }
      DEBUG(MSG_QUEUE,
	    { Sdprintf("%d: found term ", PL_thread_self());
	      PL_write_term(Serror, tmp, 1200, PL_WRT_QUOTED|PL_WRT_NEWLINE);
	    });

      rc = PL_unify(msg, tmp);

      if ( rc )
      { term_t ex = PL_new_term_ref();

	if ( !(rc=foreignWakeup(ex)) )
	{ if ( !isVar(*valTermRef(ex)) )
	    PL_raise_exception(ex);
	}
      }

      if ( rc )
      { DEBUG(MSG_QUEUE, Sdprintf("%d: match\n", PL_thread_self()));

	if (GD->atoms.gc_active)
	  markAtomsRecord(msgp->message);

        simpleMutexLock(&queue->gc_mutex);	/* see (*) */
	if ( prev )
	{ if ( !(prev->next = msgp->next) )
	    queue->tail = prev;
	} else
	{ if ( !(queue->head = msgp->next) )
	    queue->tail = NULL;
	}
        simpleMutexUnlock(&queue->gc_mutex);

	free_thread_message(msgp);
	queue->size--;
	if ( queue->wait_for_drain )
	{ DEBUG(MSG_QUEUE, Sdprintf("Queue drained. wakeup writers\n"));
	  cv_signal(&queue->drain_var);
	}

	PL_close_foreign_frame(fid);
	return TRUE;
      } else if ( exception_term )
      { PL_close_foreign_frame(fid);
	return FALSE;
      }

      PL_rewind_foreign_frame(fid);
    }

    queue->waiting++;
    queue->waiting_var += isvar;
    DEBUG(MSG_QUEUE_WAIT, Sdprintf("%d: waiting on queue\n", PL_thread_self()));
    rc = dispatch_cond_wait(queue, QUEUE_WAIT_READ, deadline);
    switch ( rc )
    { case CV_INTR:
      { DEBUG(MSG_QUEUE_WAIT, Sdprintf("%d: CV_INTR\n", PL_thread_self()));

	if ( !LD )			/* needed for clean exit */
	{ Sdprintf("Forced exit from get_message()\n");
	  exit(1);
	}

	if ( is_signalled() )		/* thread-signal */
	{ queue->waiting--;
	  queue->waiting_var -= isvar;
	  PL_discard_foreign_frame(fid);
	  return MSG_WAIT_INTR;
	}
	break;
      }
      case CV_TIMEDOUT:
      { DEBUG(MSG_QUEUE_WAIT, Sdprintf("%d: CV_TIMEDOUT\n", PL_thread_self()));

	queue->waiting--;
	queue->waiting_var -= isvar;
	PL_discard_foreign_frame(fid);
	return MSG_WAIT_TIMEOUT;
      }
      case CV_READY:
      case CV_MAYBE:
	DEBUG(MSG_QUEUE_WAIT,
	      Sdprintf("%d: wakeup (%d) on queue\n",
		       PL_thread_self(), rc));
	break;
      default:
	assert(0);
    }
    queue->waiting--;
    queue->waiting_var -= isvar;
  }
}


#define peek_message(queue, msg) LDFUNC(peek_message, queue, msg)
static int
peek_message(DECL_LD message_queue *queue, term_t msg)
{ thread_message *msgp;
  term_t tmp = PL_new_term_ref();
  word key = getIndexOfTerm(msg);
  fid_t fid = PL_open_foreign_frame();

  msgp = queue->head;

  for( msgp = queue->head; msgp; msgp = msgp->next )
  { if ( key && msgp->key && key != msgp->key )
      continue;

    if ( !PL_recorded(msgp->message, tmp) )
      return raiseStackOverflow(GLOBAL_OVERFLOW);

    if ( PL_unify(msg, tmp) )
      return TRUE;
    else if ( exception_term )
      return FALSE;

    PL_rewind_foreign_frame(fid);
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deletes the contents of the message-queue as well as the queue itself.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
destroy_message_queue(message_queue *queue)
{ thread_message *msgp;
  thread_message *next;

  if ( !queue->initialized )
    return;				/* deallocation is centralised */
  queue->initialized = FALSE;

  assert(!queue->waiting && !queue->wait_for_drain);

  for( msgp = queue->head; msgp; msgp = next )
  { next = msgp->next;

    free_thread_message(msgp);
  }

  simpleMutexDelete(&queue->gc_mutex);
  cv_destroy(&queue->cond_var);
  if ( queue->max_size > 0 )
    cv_destroy(&queue->drain_var);
  if ( !queue->anonymous )
    simpleMutexDelete(&queue->mutex);
}


/* destroy the input queue of a thread.  We have to take care of the case
   where our input queue is been waited for by another thread.  This is
   similar to message_queue_destroy/1.
 */

static void
destroy_thread_message_queue(message_queue *q)
{ int done = FALSE;

  if (!q->initialized )
    return;

  while(!done)
  { simpleMutexLock(&q->mutex);
    q->destroyed = TRUE;
    if ( q->waiting || q->wait_for_drain )
    { if ( q->waiting )
	cv_broadcast(&q->cond_var);
      if ( q->wait_for_drain )
	cv_broadcast(&q->drain_var);
    } else
      done = TRUE;
    simpleMutexUnlock(&q->mutex);
  }

  destroy_message_queue(q);
}


static void
init_message_queue(message_queue *queue, size_t max_size)
{ memset(queue, 0, sizeof(*queue));
  simpleMutexInit(&queue->mutex);
  simpleMutexInit(&queue->gc_mutex);
  cv_init(&queue->cond_var, NULL);
  queue->max_size = max_size;
  if ( queue->max_size != 0 )
    cv_init(&queue->drain_var, NULL);
  queue->initialized = TRUE;
}


static size_t
sizeof_message_queue(message_queue *queue)
{ size_t size = 0;
  thread_message *msgp;

  simpleMutexLock(&queue->gc_mutex);
  for( msgp = queue->head; msgp; msgp = msgp->next )
  { size += sizeof(*msgp);
    size += msgp->message->size;
  }
  simpleMutexUnlock(&queue->gc_mutex);

  return size;
}

					/* Prolog predicates */

static const opt_spec timeout_options[] =
{ { ATOM_timeout,	OPT_DOUBLE },
  { ATOM_deadline,	OPT_DOUBLE },
  { NULL_ATOM,		0 }
};

#ifndef DBL_MAX
#define DBL_MAX         1.7976931348623158e+308
#endif

/* This function is shared between thread_get_message/3 and
   thread_send_message/3.

   It extracts a deadline from the deadline/1 and timeout/1 options.
   In both cases, the deadline is passed through to dispatch_cond_wait().
   Semantics are relatively simple:

	1. If neither option is given, the deadline is NULL, which
	   corresponds to an indefinite wait, or a deadline in the
	   infinite future.
	2. A timeout is _exactly_ like a deadline of Now + Timeout,
	   where Now is evaluated near the beginning of this function.
	3. If both deadline and a timeout options are given, the
	   earlier deadline is effective.
	4. If the effective deadline is before Now, then return
	   FALSE (leading to failure).
*/

#define process_deadline_options(options, ts, pts) LDFUNC(process_deadline_options, options, ts, pts)

static int
process_deadline_options(DECL_LD term_t options,
			 struct timespec *ts, struct timespec **pts)
{ struct timespec now;
  struct timespec deadline;
  struct timespec timeout;
  struct timespec *dlop=NULL;
  double tmo = DBL_MAX;
  double dlo = DBL_MAX;

  if ( !scan_options(options, 0,
		     ATOM_timeout_option, timeout_options,
		     &tmo, &dlo) )
    return FALSE;

  get_current_timespec(&now);

  if ( dlo != DBL_MAX )
  { timespec_set_dbl(&deadline, dlo);
    dlop = &deadline;

    if ( timespec_cmp(&deadline,&now) < 0 ) // if deadline in the past...
      return FALSE;                         // ... then FAIL
  }

  // timeout option is processed exactly as if the deadline
  // was set to now + timeout.
  if ( tmo != DBL_MAX )
  { if ( tmo > 0.0 )
    { double ip, fp=modf(tmo,&ip);

      timeout.tv_sec  = now.tv_sec + (time_t)ip;
      timeout.tv_nsec = now.tv_nsec + (long)(fp*1000000000.0);
      carry_timespec_nanos(&timeout);
      if ( dlop==NULL || timespec_cmp(&timeout,&deadline) < 0 )
	dlop = &timeout;
    } else if ( tmo == 0.0 )
    { dlop = &now;				/* scan once */
    } else               // if timeout is negative ...
      return FALSE;      // ... then FAIL
  }
  if (dlop)
  { *ts  = *dlop;
    *pts = ts;
  } else
  { *pts=NULL;
  }

  return TRUE;
}


#define wait_queue_message(qterm, q, msg, deadline) LDFUNC(wait_queue_message, qterm, q, msg, deadline)
static int
wait_queue_message(DECL_LD term_t qterm, message_queue *q, thread_message *msg,
		   struct timespec *deadline)
{ int rc;

  for(;;)
  { rc = queue_message(q, msg, deadline);

    switch(rc)
    { case MSG_WAIT_INTR:
      { if ( PL_handle_signals() >= 0 )
	  continue;
	rc = FALSE;
	break;
      }
      case MSG_WAIT_DESTROYED:
      { if ( qterm )
	  PL_existence_error("message_queue", qterm);
	rc = FALSE;
	break;
      }
      case MSG_WAIT_TIMEOUT:
	rc = FALSE;
        break;
      case TRUE:
	break;
      default:
	assert(0);
    }

    break;
  }

  return rc;
}

#define thread_send_message(queue, msgterm, deadline) LDFUNC(thread_send_message, queue, msgterm, deadline)
static int
thread_send_message(DECL_LD term_t queue, term_t msgterm,
			struct timespec *deadline)
{ message_queue *q;
  thread_message *msg;
  int rc;

  if ( !(msg = create_thread_message(msgterm)) )
    return PL_no_memory();

  if ( !get_message_queue(queue, &q) )
  { free_thread_message(msg);
    return FALSE;
  }

  rc = wait_queue_message(queue, q, msg, deadline);
  release_message_queue(q);

  if ( rc == FALSE )
    free_thread_message(msg);

  return rc;
}

static
PRED_IMPL("thread_send_message", 2, thread_send_message, PL_FA_ISO)
{ PRED_LD

  return thread_send_message(A1, A2, NULL);
}

static
PRED_IMPL("thread_send_message", 3, thread_send_message, 0)
{ PRED_LD
  struct timespec deadline;
  struct timespec *dlop=NULL;

  return process_deadline_options(A3,&deadline,&dlop)
    &&   thread_send_message(A1, A2, dlop);
}



static
PRED_IMPL("thread_get_message", 1, thread_get_message, PL_FA_ISO)
{ PRED_LD
  int rc;

  for(;;)
  { simpleMutexLock(&LD->thread.messages.mutex);
    rc = get_message(&LD->thread.messages, A1, NULL);
    simpleMutexUnlock(&LD->thread.messages.mutex);

    if ( rc == MSG_WAIT_INTR )
    { if ( PL_handle_signals() >= 0 )
	continue;
      rc = FALSE;
    }

    break;
  }

  return rc;
}


static
PRED_IMPL("thread_peek_message", 1, thread_peek_message_1, PL_FA_ISO)
{ PRED_LD
  int rc;

  simpleMutexLock(&LD->thread.messages.mutex);
  rc = peek_message(&LD->thread.messages, A1);
  simpleMutexUnlock(&LD->thread.messages.mutex);

  return rc;
}


		 /*******************************
		 *     USER MESSAGE QUEUES	*
		 *******************************/

typedef struct mqref
{ message_queue *queue;
} mqref;

static int
write_message_queue_ref(IOSTREAM *s, atom_t aref, int flags)
{ mqref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<message_queue>(%p)", ref->queue);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC a message queue from the atom  garbage collector. This should be fine
because atoms in messages do  not  have   locked  atoms,  so  we are not
calling atom functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_message_queue_ref(atom_t aref)
{ mqref *ref = PL_blob_data(aref, NULL, NULL);
  message_queue *q;

  DEBUG(MSG_QUEUE_GC,
	Sdprintf("GC message_queue %p\n", ref->queue));

  if ( (q=ref->queue) )
  { destroy_message_queue(q);			/* can be called twice */
    if ( !q->destroyed )
      deleteHTable(queueTable, (void *)q->id);
    simpleMutexDelete(&q->mutex);
    PL_free(q);
  }

  return TRUE;
}


static int
save_message_queue(atom_t aref, IOSTREAM *fd)
{ mqref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <message_queue>(%p)", ref->queue);
}


static atom_t
load_message_queue(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-message-queue-ref>");
}


static PL_blob_t message_queue_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "message_queue",
  release_message_queue_ref,
  NULL,
  write_message_queue_ref,
  NULL,
  save_message_queue,
  load_message_queue
};


static void
initMessageQueues(void)
{ message_queue_blob.atom_name = ATOM_message_queue;
  PL_register_blob_type(&message_queue_blob);
}

static int
unify_queue(term_t t, message_queue *q)
{ GET_LD

  return PL_unify_atom(t, q->id);
}


static void
free_queue_symbol(void *name, void *value)
{ message_queue *q = value;

  destroy_message_queue(q);			/* must this be synced? */
  PL_free(q);
}


static message_queue *
unlocked_message_queue_create(term_t queue, long max_size)
{ GET_LD
  atom_t name = NULL_ATOM;
  message_queue *q;
  word id;

  if ( !queueTable )
  { simpleMutexInit(&queueTable_mutex);
    queueTable = newHTable(16);
    queueTable->free_symbol = free_queue_symbol;
  }

  if ( PL_get_atom(queue, &name) )
  { if ( lookupHTable(queueTable, (void *)name) ||
	 lookupHTable(threadTable, (void *)name) )
    { PL_error("message_queue_create", 1, NULL, ERR_PERMISSION,
	       ATOM_create, ATOM_message_queue, queue);
      return NULL;
    }
    id = name;
  } else if ( PL_is_variable(queue) )
  { id = 0;
  } else
  { PL_error("message_queue_create", 1, NULL,
	     ERR_TYPE, ATOM_message_queue, queue);
    return NULL;
  }

  q = PL_malloc(sizeof(*q));
  init_message_queue(q, max_size);
  q->type = QTYPE_QUEUE;
  if ( !id )
  { mqref ref;
    int new;

    ref.queue = q;
    q->id = lookupBlob((void*)&ref, sizeof(ref), &message_queue_blob, &new);
    q->anonymous = TRUE;
  } else
  { q->id = id;
  }
  addNewHTable(queueTable, (void *)q->id, q);

  if ( unify_queue(queue, q) )
  { if ( q->anonymous )
      PL_unregister_atom(q->id);		/* reclaim on GC */
    else
      PL_register_atom(q->id);			/* protect against reclaim */
    return q;
  }

  return NULL;
}


/* MT: Caller must hold the L_THREAD mutex

   Note that this version does not deal with anonymous queues.  High
   level code must use get_message_queue();
*/

static int
get_message_queue_unlocked(DECL_LD term_t t, message_queue **queue)
{ atom_t name;
  word id = 0;
  int tid = 0;

  if ( PL_get_atom(t, &name) )
  { thread_handle *er;

    if ( (er=symbol_thread_handle(name)) )
    { if ( er->info )
	tid = er->engine_id;
      else
	return PL_existence_error("thread", t);
    } else
      id = name;
  } else if ( !PL_get_integer(t, &tid) )
  { return PL_type_error("message_queue", t);
  }

  if ( tid > 0 )
  { have_tid:
    if ( tid >= 1 && tid <= GD->thread.highest_id )
    { PL_thread_info_t *info = GD->thread.threads[tid];

      if ( info->status == PL_THREAD_UNUSED ||
	   info->status == PL_THREAD_RESERVED ||
	   !info->thread_data )
	return PL_existence_error("thread", t);

      *queue = &info->thread_data->thread.messages;
      return TRUE;
    }

    return PL_type_error("thread", t);
  }

  if ( !id )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_message_queue, t);

  if ( queueTable )
  { message_queue *q;
    if ( (q = lookupHTable(queueTable, (void *)id)) )
    { *queue = q;
      return TRUE;
    }
  }
  if ( threadTable )
  { word w;

    if ( (w = (word)lookupHTable(threadTable, (void *)id)) )
    { thread_handle *th;

      if ( (th=symbol_thread_handle(w)) )
      { tid = th->engine_id;
	goto have_tid;
      }
    }
  }

  return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_message_queue, t);
}


/* Get a message queue and lock it
*/

static int
get_message_queue(DECL_LD term_t t, message_queue **queue)
{ int rc;
  message_queue *q;
  PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &message_queue_blob )
  { mqref *ref = data;

    q = ref->queue;
    simpleMutexLock(&q->mutex);
    if ( !q->destroyed )
    { *queue = q;
      return TRUE;
    }
    simpleMutexUnlock(&q->mutex);
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_message_queue, t);
  }

  PL_LOCK(L_THREAD);
  rc = get_message_queue_unlocked(t, queue);
  if ( rc )
  { message_queue *q = *queue;

    simpleMutexLock(&q->mutex);
    if ( q->destroyed )
    { rc = PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_message_queue, t);
      simpleMutexUnlock(&q->mutex);
    }
  }
  PL_UNLOCK(L_THREAD);

  return rc;
}


/* Release a message queue, deleting it if it is no longer needed
*/

static void
release_message_queue(message_queue *queue)
{ int del = ( queue->destroyed &&
	      !(queue->waiting || queue->wait_for_drain) &&
	      queue->type != QTYPE_THREAD );

  simpleMutexUnlock(&queue->mutex);

  if ( del )
  { destroy_message_queue(queue);
    if ( !queue->anonymous )
      PL_free(queue);
  }
}


static
PRED_IMPL("message_queue_create", 1, message_queue_create, 0)
{ int rval;

  PL_LOCK(L_THREAD);
  rval = (unlocked_message_queue_create(A1, 0) ? TRUE : FALSE);
  PL_UNLOCK(L_THREAD);

  return rval;
}


static const opt_spec message_queue_options[] =
{ { ATOM_alias,		OPT_ATOM },
  { ATOM_max_size,	OPT_SIZE },
  { NULL_ATOM,		0 }
};


static
PRED_IMPL("message_queue_create", 2, message_queue_create2, 0)
{ PRED_LD
  atom_t alias = 0;
  size_t max_size = 0;			/* to be processed */
  message_queue *q;

  if ( !scan_options(A2, 0,
		     ATOM_queue_option, message_queue_options,
		     &alias,
		     &max_size) )
    fail;

  if ( alias )
  { if ( !PL_unify_atom(A1, alias) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_variable, A1);
  }

  PL_LOCK(L_THREAD);
  q = unlocked_message_queue_create(A1, max_size);
  PL_UNLOCK(L_THREAD);

  return q ? TRUE : FALSE;
}


static
PRED_IMPL("message_queue_destroy", 1, message_queue_destroy, 0)
{ PRED_LD
  message_queue *q;

  if ( !get_message_queue(A1, &q) )
    return FALSE;

  if ( q->type == QTYPE_THREAD )
  { release_message_queue(q);

    return PL_error(NULL, 0, "is a thread-queue", ERR_PERMISSION,
		    ATOM_destroy, ATOM_message_queue, A1);
  }

  simpleMutexLock(&queueTable_mutex);	/* see markAtomsMessageQueues() */
  deleteHTable(queueTable, (void*)q->id);
  simpleMutexUnlock(&queueTable_mutex);

  if ( !q->anonymous )
    PL_unregister_atom(q->id);

  simpleMutexLock(&q->gc_mutex);	/* see markAtomsMessageQueues() */
  q->destroyed = TRUE;
  simpleMutexUnlock(&q->gc_mutex);

  if ( q->waiting )
    cv_broadcast(&q->cond_var);
  if ( q->wait_for_drain )
    cv_broadcast(&q->drain_var);

  release_message_queue(q);

  return TRUE;
}


		 /*******************************
		 *    MESSAGE QUEUE PROPERTY	*
		 *******************************/

#define message_queue_alias_property(q, prop) LDFUNC(message_queue_alias_property, q, prop)
static int		/* message_queue_property(Queue, alias(Name)) */
message_queue_alias_property(DECL_LD message_queue *q, term_t prop)
{ if ( !q->anonymous )
    return PL_unify_atom(prop, q->id);

  fail;
}


#define message_queue_size_property(q, prop) LDFUNC(message_queue_size_property, q, prop)
static int		/* message_queue_property(Queue, size(Size)) */
message_queue_size_property(DECL_LD message_queue *q, term_t prop)
{ return PL_unify_integer(prop, q->size);
}


#define message_queue_max_size_property(q, prop) LDFUNC(message_queue_max_size_property, q, prop)
static int		/* message_queue_property(Queue, max_size(Size)) */
message_queue_max_size_property(DECL_LD message_queue *q, term_t prop)
{ size_t ms;

  if ( (ms=q->max_size) > 0 )
    return PL_unify_integer(prop, ms);

  fail;
}

#define message_queue_waiting_property(q, prop) LDFUNC(message_queue_waiting_property, q, prop)
static int		/* message_queue_property(Queue, waiting(Count)) */
message_queue_waiting_property(DECL_LD message_queue *q, term_t prop)
{ int waiting;

  if ( (waiting=q->waiting) > 0 )
    return PL_unify_integer(prop, waiting);

  fail;
}

static const tprop qprop_list [] =
{ { FUNCTOR_alias1,	    LDFUNC_REF(message_queue_alias_property) },
  { FUNCTOR_size1,	    LDFUNC_REF(message_queue_size_property) },
  { FUNCTOR_max_size1,	    LDFUNC_REF(message_queue_max_size_property) },
  { FUNCTOR_waiting1,	    LDFUNC_REF(message_queue_waiting_property) },
  { 0,			    NULL }
};


typedef struct
{ TableEnum e;				/* Enumerator on queue-table */
  message_queue *q;			/* current queue */
  const tprop *p;			/* Pointer in properties */
  int enum_properties;			/* Enumerate the properties */
} qprop_enum;


static int
advance_qstate(qprop_enum *state)
{ if ( state->enum_properties )
  { state->p++;
    if ( state->p->functor )
      succeed;

    state->p = qprop_list;
  }
  if ( state->e )
  { message_queue *q;

    if ( advanceTableEnum(state->e, NULL, (void**)&q) )
    { state->q = q;

      succeed;
    }
  }

  fail;
}


static void
free_qstate(qprop_enum *state)
{ if ( state->e )
    freeTableEnum(state->e);

  freeForeignState(state, sizeof(*state));
}


static
PRED_IMPL("message_queue_property", 2, message_property, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  term_t queue = A1;
  term_t property = A2;
  qprop_enum statebuf;
  qprop_enum *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { memset(&statebuf, 0, sizeof(statebuf));
      state = &statebuf;

      if ( PL_is_variable(queue) )
      { if ( !queueTable )
	  return FALSE;

	switch( get_prop_def(property, ATOM_message_queue_property,
			     qprop_list, &state->p) )
	{ case 1:
	    state->e = newTableEnum(queueTable);
	    goto enumerate;
	  case 0:
	    state->e = newTableEnum(queueTable);
	    state->p = qprop_list;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    fail;
	}
      } else if ( get_message_queue(queue, &state->q) )
      { release_message_queue(state->q); /* FIXME: we need some form of locking! */

	switch( get_prop_def(property, ATOM_message_queue_property,
			     qprop_list, &state->p) )
	{ case 1:
	    goto enumerate;
	  case 0:
	    state->p = qprop_list;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    fail;
	}
      } else
      { fail;
      }
    }
    case FRG_REDO:
      state = CTX_PTR;
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      free_qstate(state);
      succeed;
    default:
      assert(0);
      fail;
  }

enumerate:
  if ( !state->q )			/* first time, enumerating queues */
  { message_queue *q;

    assert(state->e);
    if ( advanceTableEnum(state->e, NULL, (void**)&q) )
    { state->q = q;
    } else
    { freeTableEnum(state->e);
      assert(state == &statebuf);
      fail;
    }
  }


  { term_t a1 = PL_new_term_ref();

    if ( !state->enum_properties )
      _PL_get_arg(1, property, a1);

    for(;;)
    { if ( LDFUNCP(*state->p->function)(state->q, a1) )
      { if ( state->enum_properties )
	{ if ( !PL_unify_term(property,
			      PL_FUNCTOR, state->p->functor,
			        PL_TERM, a1) )
	    goto error;
	}
	if ( state->e )
	{ if ( !unify_queue(queue, state->q) )
	    goto error;
	}

	if ( advance_qstate(state) )
	{ if ( state == &statebuf )
	  { qprop_enum *copy = allocForeignState(sizeof(*copy));

	    *copy = *state;
	    state = copy;
	  }

	  ForeignRedoPtr(state);
	}

	if ( state != &statebuf )
	  free_qstate(state);
        else if ( state->e )
          freeTableEnum(state->e);
	succeed;
      }

      if ( !advance_qstate(state) )
      { error:
	if ( state != &statebuf )
	  free_qstate(state);
        else if ( state->e )
          freeTableEnum(state->e);
	fail;
      }
    }
  }
}

static
PRED_IMPL("message_queue_set", 2, message_queue_set, 0)
{ PRED_LD
  message_queue *q;
  atom_t name;
  size_t arity;
  int rc;

  if ( !get_message_queue(A1, &q) )
    return FALSE;

  if ( PL_get_name_arity(A2, &name, &arity) && arity == 1 )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, A2, a);
    if ( name == ATOM_max_size )
    { size_t mx;

      if ( (rc=PL_get_size_ex(a, &mx)) )
      { size_t omax = q->max_size;

	q->max_size = mx;

	if ( mx > omax && q->wait_for_drain )
	{ DEBUG(MSG_QUEUE, Sdprintf("Queue drained. wakeup writers\n"));
	  cv_signal(&q->drain_var);
	}

	rc = TRUE;
      }
    } else
    { rc = PL_domain_error("message_queue_property", A2);
    }
  } else
    rc = PL_type_error("compound", A2);

  release_message_queue(q);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
thread_get_message(+Queue, -Message)
thread_get_message(-Message)
    Get a message from a message queue. If the queue is not provided get
    a message from the queue implicitly associated to the thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define thread_get_message(queue, msg, deadline) LDFUNC(thread_get_message, queue, msg, deadline)
static int
thread_get_message(DECL_LD term_t queue, term_t msg, struct timespec *deadline)
{ int rc;

  for(;;)
  { message_queue *q;

    if ( !get_message_queue(queue, &q) )
      return FALSE;

    rc = get_message(q, msg, deadline);
    release_message_queue(q);

    switch(rc)
    { case MSG_WAIT_INTR:
	if ( PL_handle_signals() >= 0 )
	  continue;
	rc = FALSE;
	break;
      case MSG_WAIT_DESTROYED:
	rc = PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_message_queue, queue);
        break;
      case MSG_WAIT_TIMEOUT:
	rc = FALSE;
        break;
      default:
	;
    }

    break;
  }

  return rc;
}


static
PRED_IMPL("thread_get_message", 2, thread_get_message, 0)
{ PRED_LD

  return thread_get_message(A1, A2, NULL);
}


static
PRED_IMPL("thread_get_message", 3, thread_get_message, 0)
{ PRED_LD
  struct timespec deadline;
  struct timespec *dlop=NULL;

  return process_deadline_options(A3,&deadline,&dlop)
    &&   thread_get_message(A1, A2, dlop);
}


static
PRED_IMPL("thread_peek_message", 2, thread_peek_message_2, 0)
{ PRED_LD
  message_queue *q;
  int rc;

  if ( !get_message_queue(A1, &q) )
    fail;

  rc = peek_message(q, A2);
  release_message_queue(q);
  return rc;
}

		 /*******************************
		 *	       WAIT		*
		 *******************************/

static thread_wait_area*
new_wait_area(void)
{ thread_wait_area *wa = malloc(sizeof(*wa));

  if ( wa )
  { memset(wa, 0, sizeof(*wa));
    simpleMutexInit(&wa->mutex);
    cv_init(&wa->cond, NULL);
  }

  return wa;
}

void
free_wait_area(thread_wait_area *wa)
{ simpleMutexDelete(&wa->mutex);
  cv_destroy(&wa->cond);
  free(wa);
}

void
free_thread_wait(PL_local_data_t *ld)
{ thread_wait_for *twf;

  if ( (twf=ld->thread.waiting_for) )
  { ld->thread.waiting_for = NULL;
    discardBuffer(&twf->channels);
    free(twf);
  }
}

static int
ensure_wait_area_module(Module module)
{ if ( !module->wait )
  { thread_wait_area *wa = new_wait_area();

    if ( wa )
    { if ( !COMPARE_AND_SWAP_PTR(&module->wait, NULL, wa) )
	free_wait_area(wa);
    } else
    { return PL_no_memory();
    }
  }

  return TRUE;
}

#define ensure_waiting_for(_) LDFUNC(ensure_waiting_for, _)
static int
ensure_waiting_for(DECL_LD)
{ if ( !LD->thread.waiting_for )
  { if ( !(LD->thread.waiting_for = malloc(sizeof(*LD->thread.waiting_for))) )
      return PL_no_memory();
    memset(LD->thread.waiting_for, 0, sizeof(*LD->thread.waiting_for));
    initBuffer(&LD->thread.waiting_for->channels);
  }

  return TRUE;
}


static thread_dcell *
register_waiting(Module m, PL_local_data_t *ld)
{ thread_dcell *c;

  if ( !(c=ld->thread.waiting_for->registered) )
  { c = malloc(sizeof(*c));

    if ( c )
    { thread_dcell *t;

      c->ld = ld;
      if ( (t=m->wait->w_tail) )
      { c->prev = t;
	c->next = NULL;
	t->next = c;
	m->wait->w_tail = c;
      } else
      { m->wait->w_tail =
	m->wait->w_head = c;
	c->next = c->prev = NULL;
      }
      ld->thread.waiting_for->registered = c;
    }
  }

  return c;
}

static void
unregister_waiting(Module m, PL_local_data_t *ld)
{ thread_dcell *c = ld->thread.waiting_for->registered;

  if ( c )
  { ld->thread.waiting_for->registered = NULL;

    if ( c->next )
      c->next->prev = c->prev;
    else
      m->wait->w_tail = c->prev;

    if ( c->prev )
      c->prev->next = c->next;
    else
      m->wait->w_head = c->next;

    free(c);
  }
}

#define add_wch(wch) LDFUNC(add_wch, wch)
static int
add_wch(DECL_LD thread_wait_channel *wch)
{ wch->signalled = FALSE;
  addBuffer(&LD->thread.waiting_for->channels, *wch, thread_wait_channel);

  return TRUE;
}

#define add_wait_for_module(m) LDFUNC(add_wait_for_module, m)
static int
add_wait_for_module(DECL_LD Module m)
{ thread_wait_channel wch = {0};

  wch.type = TWF_MODULE;
  wch.obj.module = m;
  wch.generation = m->last_modified;
  set(m, M_WAITED_FOR);

  return add_wch(&wch);
}

#define thread_wait_preds(m, preds) LDFUNC(thread_wait_preds, m, preds)
static int
thread_wait_preds(DECL_LD Module m, term_t preds)
{ term_t tail = PL_copy_term_ref(preds);
  term_t head = PL_new_term_ref();

  while(PL_get_list_ex(tail, head, tail))
  { Procedure proc;
    thread_wait_channel wch = {.flags = TWF_ASSERT|TWF_RETRACT};
    functor_t fdef;
    Module m2 = m;

    if ( PL_is_functor(head, FUNCTOR_minus1) )
    { wch.flags &= ~TWF_ASSERT;
      _PL_get_arg(1, head, head);
    } else if ( PL_is_functor(head, FUNCTOR_plus1) )
    { wch.flags &= ~TWF_RETRACT;
      _PL_get_arg(1, head, head);
    }

    if ( get_functor(head, &fdef, &m2, 0, GF_NAMEARITY) )
    { if ( m != m2 )
	return PL_permission_error("thread_wait", "external_procedure", head);

      if ( (proc=lookupBodyProcedure(fdef, m)) )
      { wch.type = TWF_PREDICATE;
	wch.obj.predicate = getProcDefinition(proc);
	wch.generation = wch.obj.predicate->last_modified;
	if ( true(wch.obj.predicate, P_THREAD_LOCAL) )
	  return PL_permission_error("thread_wait", "thread_local", head);
	set(wch.obj.predicate, P_WAITED_FOR);
	add_wch(&wch);
      } else
	return FALSE;
    } else
      return FALSE;
  }

  return TRUE;
}


#define unify_modified(t) LDFUNC(unify_modified, t)
static int
unify_modified(DECL_LD term_t t)
{ thread_wait_for *twf;

  if ( (twf=LD->thread.waiting_for) )
  { thread_wait_channel *ch = baseBuffer(&twf->channels, thread_wait_channel);
    size_t i, count = entriesBuffer(&twf->channels, thread_wait_channel);
    term_t head = PL_new_term_ref();
    term_t tail = PL_copy_term_ref(t);
    Module m = contextModule(environment_frame);

    for(i=0; i<count; i++, ch++)
    { if ( ch->type == TWF_PREDICATE &&
	   ch->generation != ch->obj.predicate->last_modified )
      { if ( !PL_unify_list_ex(tail, head, tail) ||
	     !unify_definition(m, head, ch->obj.predicate, 0,
			       GP_HIDESYSTEM|GP_NAMEARITY) )
	  return FALSE;
      }
    }
    return PL_unify_nil(tail);
  }

  return PL_unify_nil(t);
}


/** thread_wait_on_goal(:Goal, +Options) is semidet.
 */

static const opt_spec thread_wait_options[] =
{ { ATOM_db,		OPT_BOOL   },
  { ATOM_wait_preds,	OPT_TERM   },
  { ATOM_modified,      OPT_TERM   },
  { ATOM_retry_every,	OPT_DOUBLE },
  { ATOM_module,        OPT_ATOM },
  { NULL_ATOM,		0 }
};

#define wait_filter_satisfied(_) LDFUNC(wait_filter_satisfied, _)
static int wait_filter_satisfied(DECL_LD);

static
PRED_IMPL("thread_wait", 2, thread_wait, 0)
{ PRED_LD
  int rc = FALSE;
  struct timespec deadline;
  struct timespec *dlop=NULL;
  int db = -1;				/* wait on db changes */
  Module module = NULL;
  double retry_every = 1.0;
  term_t wait_preds = 0, modified = 0;
  struct timespec retry;
  int ign_filter = TRUE;
  term_t options = PL_new_term_ref();
  atom_t mname = NULL_ATOM;

  if ( !PL_strip_module(A2, &module, options) ||
       !process_deadline_options(options, &deadline, &dlop) ||
       !scan_options(options, 0, ATOM_thread_wait_options, thread_wait_options,
		     &db, &wait_preds, &modified, &retry_every, &mname) )
    return FALSE;

  if ( mname )
    module = lookupModule(mname);

  if ( modified )
  { if ( !PL_is_variable(modified) )
      return PL_uninstantiation_error(modified);
    Mark(fli_context->mark);
  }

  if ( LD->transaction.generation )
    return PL_error("thread_wait", 2, "in transaction",
		    ERR_PERMISSION,
		    ATOM_thread, ATOM_wait, A1);

  if ( !ensure_waiting_for() ||
       !ensure_wait_area_module(module) )
    return FALSE;

  if ( LD->thread.waiting_for->registered )
    return PL_error("thread_wait", 2, "recursive call",
		    ERR_PERMISSION,
		    ATOM_thread, ATOM_wait, A1);

  if ( wait_preds && !thread_wait_preds(module, wait_preds) )
    goto out;
  if ( db == TRUE || isEmptyBuffer(&LD->thread.waiting_for->channels) )
    add_wait_for_module(module);

  timespec_set_dbl(&retry, retry_every);

  LD->thread.waiting_for->signalled = FALSE;
  simpleMutexLock(&module->wait->mutex);
  for(;;)
  { if ( (ign_filter || wait_filter_satisfied()) &&
	 ( (rc = ((!modified || unify_modified(modified)) &&
		  callProlog(NULL, A1, PL_Q_PASS_EXCEPTION, NULL))) ||
	   PL_exception(0) ) )
      break;

    if ( modified )
      Undo(fli_context->mark);

    register_waiting(module, LD);
    ign_filter = FALSE;
    DEBUG(MSG_THREAD_WAIT, Sdprintf("Wait on module %s ...\n",
				    PL_atom_chars(module->name)));
    int wrc = cv_timedwait(NULL, &module->wait->cond,
			   &module->wait->mutex, dlop, &retry);
    DEBUG(MSG_THREAD_WAIT, Sdprintf("Wakeup %d\n", wrc));
    switch(wrc)
    { case CV_INTR:
	if ( PL_handle_signals() >= 0 )
	  continue;
        rc = FALSE;
        goto out_for_loop;
      case CV_TIMEDOUT:
	rc = FALSE;
        goto out_for_loop;
      case CV_MAYBE:
	ign_filter = TRUE;
      case CV_READY:
	continue;
      default:
	assert(0);
    }
  }
out_for_loop:
  simpleMutexUnlock(&module->wait->mutex);

out:
  unregister_waiting(module, LD);
  emptyBuffer(&LD->thread.waiting_for->channels, 1024);
  return rc;
}


static int
wait_filter_satisfied(DECL_LD)
{ thread_wait_for *twf = LD->thread.waiting_for;
  int signalled = twf->signalled;

  twf->signalled = FALSE;

  return signalled;
}


static int
signal_waiting_thread(PL_local_data_t *ld, thread_wait_channel *wch)
{ thread_wait_for *twf;
  int done = 0;

  DEBUG(MSG_THREAD_WAIT, Sdprintf("Checking wakeup for %d\n",
				  ld->thread.info->pl_tid));

  if ( (twf=ld->thread.waiting_for) &&
       !twf->signalled &&
       !ld->transaction.generation )
  { if ( wch->type == TWF_PREDICATE || wch->type == TWF_MODULE )
    { thread_wait_channel *ch = baseBuffer(&twf->channels, thread_wait_channel);
      size_t i, count = entriesBuffer(&twf->channels, thread_wait_channel);
      Definition def;
      Module m;

      if ( wch->type == TWF_PREDICATE )
      { def = wch->obj.predicate;
	m = def->module;
      } else
      { def = NULL;
	m = wch->obj.module;
      }

      for(i=0; i<count; i++, ch++)
      { switch( ch->type )
	{ case TWF_DB:
	    DEBUG(MSG_THREAD_WAIT, Sdprintf("  db modified\n"));
	    ch->signalled = TRUE;
	    done++;
	    break;
	  case TWF_MODULE:
	    if ( ch->obj.module == m )
	    { DEBUG(MSG_THREAD_WAIT, Sdprintf("  module modified\n"));
	      ch->signalled = TRUE;
	      done++;
	    }
	    break;
	  case TWF_PREDICATE:
	    if ( ch->obj.predicate == def && (ch->flags&wch->flags) )
	    { DEBUG(MSG_THREAD_WAIT, Sdprintf("  predicate %s modified\n",
					     predicateName(def)));
	      ch->signalled = TRUE;
	      done++;
	    }
	    break;
	  default:
	    assert(0);
	}
      }
    } else
    { assert(0);			/* not yet used */
    }

    if ( done )
      twf->signalled = TRUE;
  }

  return done;
}


typedef struct module_cell
{ Module module;
  struct module_cell *prev;
} module_cell;


#define updating(m) LDFUNC(updating, m)
static int
updating(DECL_LD Module m)
{ if ( LD->thread.waiting_for )
  { module_cell *mc;

    for(mc=LD->thread.waiting_for->updating; mc; mc = mc->prev)
    { if ( mc->module == m )
	return TRUE;
    }
  }

  return FALSE;
}


int
signal_waiting_threads(Module m, thread_wait_channel *wch)
{ GET_LD
  thread_dcell *c;
  int done = 0;
  int lock = !LD->thread.waiting_for || !LD->thread.waiting_for->registered;

  if ( updating(m) )
    return 0;
					/* TBD: check waiting on same module? */
  if ( lock )
    simpleMutexLock(&m->wait->mutex);
  for(c=m->wait->w_head; c; c = c->next)
    done += signal_waiting_thread(c->ld, wch);
  if ( done )
    cv_broadcast(&m->wait->cond);
  if ( lock )
    simpleMutexUnlock(&m->wait->mutex);

  return done;
}


static const opt_spec thread_update_options[] =
{ { ATOM_notify,  OPT_ATOM },
  { ATOM_module,  OPT_ATOM },
  { NULL_ATOM,	  0 }
};

static
PRED_IMPL("thread_update", 2, thread_update, PL_FA_TRANSPARENT)
{ PRED_LD
  int lock = !LD->thread.waiting_for || !LD->thread.waiting_for->registered;
  int rc;
  atom_t notify = ATOM_broadcast;
  atom_t mname = NULL_ATOM;
  Module module = NULL;
  term_t options = PL_new_term_ref();
  module_cell mc;

  if ( !PL_strip_module(A2, &module, options) ||
       !scan_options(options, 0, ATOM_thread_update_options,
		     thread_update_options,
		     &notify, &mname) )
    return FALSE;
  if ( notify != ATOM_broadcast && notify != ATOM_signal )
  { term_t ex = PL_new_term_ref();
    PL_put_atom(ex, notify);
    return PL_domain_error("notify_option", ex);
  }
  if ( mname )
    module = lookupModule(mname);

  if ( !ensure_waiting_for() ||
       !ensure_wait_area_module(module) )
    return FALSE;

  wakeupThreadsModule(module, 0);

  if ( lock )
    simpleMutexLock(&module->wait->mutex);

  mc.module = module;
  mc.prev = LD->thread.waiting_for->updating;
  LD->thread.waiting_for->updating = &mc;
  rc=callProlog(NULL, A1, PL_Q_PASS_EXCEPTION, NULL);
  LD->thread.waiting_for->updating = mc.prev;

  if ( !rc )
    goto out;

  DEBUG(MSG_THREAD_WAIT,
	Sdprintf("%s to module %s\n",
		 notify == ATOM_broadcast ? "broadcast" : "signal",
		 PL_atom_chars(module->name)));

  if ( notify == ATOM_broadcast )
    cv_broadcast(&module->wait->cond);
  else
    cv_signal(&module->wait->cond);

out:
  if ( lock )
    simpleMutexUnlock(&module->wait->mutex);

  return rc;
}

		 /*******************************
		 *	 MUTEX PRIMITIVES	*
		 *******************************/

#ifdef NEED_RECURSIVE_MUTEX_INIT

#ifdef RECURSIVE_MUTEXES
static int
recursive_attr(pthread_mutexattr_t **ap)
{ static int done;
  static pthread_mutexattr_t attr;
  int rc;

  if ( done )
  { *ap = &attr;
    return 0;
  }

  PL_LOCK(L_THREAD);
  if ( done )
  { PL_UNLOCK(L_THREAD);

    *ap = &attr;
    return 0;
  }
  if ( (rc=pthread_mutexattr_init(&attr)) )
    goto error;
#ifdef HAVE_PTHREAD_MUTEXATTR_SETTYPE
  if ( (rc=pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE)) )
    goto error;
#else
#ifdef HAVE_PTHREAD_MUTEXATTR_SETKIND_NP
  if ( (rc=pthread_mutexattr_setkind_np(&attr, PTHREAD_MUTEX_RECURSIVE_NP)) )
    goto error;
#endif
#endif

  done = TRUE;
  PL_UNLOCK(L_THREAD);
  *ap = &attr;

  return 0;

error:
  PL_UNLOCK(L_THREAD);
  return rc;
}
#endif

int
recursiveMutexInit(recursiveMutex *m)
{
  pthread_mutexattr_t *attr = NULL;

#ifdef RECURSIVE_MUTEXES
  int rc;

  if ( (rc=recursive_attr(&attr)) )
    return rc;

#else /*RECURSIVE_MUTEXES*/

  m->owner = 0;
  m->count = 0;

#endif /* RECURSIVE_MUTEXES */

  return pthread_mutex_init(&(m->lock), attr);
}

#endif /*NEED_RECURSIVE_MUTEX_INIT*/

#ifdef NEED_RECURSIVE_MUTEX_DELETE

int
recursiveMutexDelete(recursiveMutex *m)
{ if ( m->owner != 0 )
    return EBUSY;

  return pthread_mutex_destroy(&(m->lock));
}

#endif /*NEED_RECURSIVE_MUTEX_DELETE*/

#ifndef RECURSIVE_MUTEXES
int
recursiveMutexLock(recursiveMutex *m)
{ int result = 0;
  pthread_t self = pthread_self();

  if ( pthread_equal(self, m->owner) )
    m->count++;
  else
  { result = pthread_mutex_lock(&(m->lock));
    m->owner = self;
    m->count = 1;
  }

  return result;
}


int
recursiveMutexTryLock(recursiveMutex *m)
{ int result = 0;
  pthread_t self = pthread_self();

  if ( pthread_equal(self, m->owner) )
    m->count++;
  else
  { result = pthread_mutex_trylock(&(m->lock));
    if ( result == 0 )
    { m->owner = self;
      m->count = 1;
    }
  }

  return result;
}


int
recursiveMutexUnlock(recursiveMutex *m)
{ int result = 0;
  pthread_t self = pthread_self();

  if ( pthread_equal(self,m->owner) )
  { if ( --m->count < 1 )
    { m->owner = 0;
      result = pthread_mutex_unlock(&(m->lock));
    }
  } else if ( !pthread_equal(m->owner, 0) )
  { Sdprintf("unlocking unowned mutex %p,not done!!\n", m);
    Sdprintf("\tlocking thread was %u , unlocking is %u\n",m->owner,self);

    result = -1;
  }

  return result;
}

#endif /*RECURSIVE_MUTEXES*/

void
initSimpleMutex(counting_mutex *m, const char *name)
{ simpleMutexInit(&m->mutex);
  m->count = 0;
  m->lock_count = 0;
#ifdef O_CONTENTION_STATISTICS
  m->collisions = 0;
#endif
  m->name = name ? store_string(name) : (char*)NULL;
  m->prev = NULL;

  PL_LOCK(L_MUTEX);
  m->next = GD->thread.mutexes;
  GD->thread.mutexes = m;
  if ( m->next )
    m->next->prev = m;
  PL_UNLOCK(L_MUTEX);
}


counting_mutex *
allocSimpleMutex(const char *name)
{ counting_mutex *m = allocHeapOrHalt(sizeof(*m));
  initSimpleMutex(m, name);

  return m;
}


void
deleteSimpleMutex(counting_mutex *m)
{ PL_LOCK(L_MUTEX);
  if ( m->next )
    m->next->prev = m->prev;
  if ( m->prev )
    m->prev->next = m->next;
  else
    GD->thread.mutexes = m->next;
  PL_UNLOCK(L_MUTEX);

  simpleMutexDelete(&m->mutex);
  remove_string((char *)m->name);
}


void
freeSimpleMutex(counting_mutex *m)
{ deleteSimpleMutex(m);
  freeHeap(m, sizeof(*m));
}


		 /*******************************
		 *	FOREIGN INTERFACE	*
		 *******************************/

int
PL_thread_attach_engine(PL_thread_attr_t *attr)
{ GET_LD
  PL_thread_info_t *info;
  PL_local_data_t *ldnew;
  PL_local_data_t *ldmain;

  if ( LD )
  { if ( LD->thread.info->open_count+1 == 0 )
    { errno = EINVAL;
      return -1;
    }
    LD->thread.info->open_count++;
    return LD->thread.info->pl_tid;
  }

  if ( !GD->thread.enabled || GD->cleaning != CLN_NORMAL )
  {
#ifdef EPERM				/* FIXME: Better reporting */
    errno = EPERM;
#endif
    return -1;
  }

  if ( !(info = alloc_thread()) )
    return -1;				/* out of threads */

  ldmain = GD->thread.threads[1]->thread_data;
  ldnew = info->thread_data;

  if ( attr )
  { if ( attr->stack_limit )
      info->stack_limit = attr->stack_limit;

    info->cancel = attr->cancel;
  }

  info->goal       = NULL;
  info->module     = MODULE_user;
  info->detached   = attr == NULL ||
                     (attr->flags & PL_THREAD_NOT_DETACHED) == 0;
  info->open_count = 1;

  copy_local_data(ldnew, ldmain, attr ? attr->max_queue_size : 0);

  if ( !initialise_thread(info) )
  { free_thread_info(info);
    errno = ENOMEM;
    return -1;
  }
  set_system_thread_id(info);
  PL_LOCK(L_THREAD);
  info->status = PL_THREAD_RUNNING;
  PL_UNLOCK(L_THREAD);

  if ( attr )
  { if ( attr->alias )
    { if ( !aliasThread(info->pl_tid, ATOM_thread, PL_new_atom(attr->alias)) )
      { free_thread_info(info);
	errno = EPERM;
	TLD_set_LD(NULL);
	return -1;
      }
    }
    if ( true(attr, PL_THREAD_NO_DEBUG) )
    { ldnew->_debugstatus.tracing   = FALSE;
      ldnew->_debugstatus.debugging = DBG_OFF;
      setPrologRunMode_LD(ldnew, RUN_MODE_NORMAL);
      info->debug = FALSE;
    }
  }

  updateAlerted(ldnew);
  PL_call_predicate(MODULE_system, PL_Q_NORMAL, PROCEDURE_dthread_init0, 0);

  return info->pl_tid;
}


int
PL_thread_destroy_engine(void)
{ GET_LD

  if ( LD )
  { if ( --LD->thread.info->open_count == 0 )
    { free_prolog_thread(LD);
      TLD_set_LD(NULL);
    }

    return TRUE;
  }

  return FALSE;				/* we had no thread */
}


int
attachConsole(void)
{ GET_LD
  fid_t fid = PL_open_foreign_frame();
  int rval;
  predicate_t pred = PL_predicate("attach_console", 0, "user");

  rval = PL_call_predicate(NULL, PL_Q_NODEBUG, pred, 0);

  PL_discard_foreign_frame(fid);

  return rval;
}


		 /*******************************
		 *	      ENGINES		*
		 *******************************/

static PL_engine_t
PL_current_engine(void)
{ GET_LD

  return LD;
}


static void
detach_engine(PL_engine_t e)
{ PL_thread_info_t *info = e->thread.info;

  info->has_tid = FALSE;
#ifdef __linux__
  info->pid = -1;
#endif
#ifdef __WINDOWS__
  info->w32id = 0;
#endif
  memset(&info->tid, 0, sizeof(info->tid));
}


int
PL_set_engine(PL_engine_t new, PL_engine_t *old)
{ PL_engine_t current = PL_current_engine();

  if ( new != current && new != PL_ENGINE_CURRENT )
  { PL_LOCK(L_THREAD);

    if ( new )
    { if ( new == PL_ENGINE_MAIN )
	new = &PL_local_data;

      if ( new->magic != LD_MAGIC )
      { PL_UNLOCK(L_THREAD);
	return PL_ENGINE_INVAL;
      }
      if ( new->thread.info->has_tid )
      { PL_UNLOCK(L_THREAD);
	return PL_ENGINE_INUSE;
      }
    }

    if ( current )
      detach_engine(current);

    if ( new )
    { TLD_set_LD(new);
      new->thread.info->tid = pthread_self();

      set_system_thread_id(new->thread.info);
    } else
    { TLD_set_LD(NULL);
    }

    PL_UNLOCK(L_THREAD);
  }

  if ( old )
  { *old = current;
  }

  return PL_ENGINE_SET;
}


PL_engine_t
PL_create_engine(PL_thread_attr_t *attributes)
{ PL_engine_t e, current;

  PL_set_engine(NULL, &current);
  if ( PL_thread_attach_engine(attributes) >= 0 )
  { e = PL_current_engine();
  } else
    e = NULL;

  PL_set_engine(current, NULL);

  return e;
}


int
PL_destroy_engine(PL_engine_t e)
{ int rc;

  if ( e == PL_current_engine() )
  { rc = PL_thread_destroy_engine();
  } else
  { PL_engine_t current;

    if ( PL_set_engine(e, &current) == PL_ENGINE_SET )
    { rc = PL_thread_destroy_engine();
      PL_set_engine(current, NULL);

    } else
      rc = FALSE;
  }

  return rc;
}


		 /*******************************
		 *	      GC THREAD		*
		 *******************************/

static int GC_id = 0;
static int GC_starting = 0;

static rc_cancel cancelGCThread(int tid);

static void *
GCmain(void *closure)
{ PL_thread_attr_t attrs = {0};
#if O_SIGNALS && defined(HAVE_SIGPROCMASK)
  sigset_t set;
  allSignalMask(&set);
#ifdef SIG_ALERT
  if ( GD->signals.sig_alert )
    sigdelset(&set, GD->signals.sig_alert);
#endif
  pthread_sigmask(SIG_BLOCK, &set, NULL);
#endif

  attrs.alias = "gc";
  attrs.flags = PL_THREAD_NO_DEBUG|PL_THREAD_NOT_DETACHED;
  set_os_thread_name_from_charp("gc");

  if ( PL_thread_attach_engine(&attrs) > 0 )
  { GET_LD
    PL_thread_info_t *info = LD->thread.info;
    predicate_t pred;
    int rc;

    pred = _PL_predicate("$gc", 0, "system", &GD->procedures.dgc0);

    GC_id = PL_thread_self();
    info->cancel = cancelGCThread;
    rc = PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, 0);
    GC_id = 0;

    set_thread_completion(info, rc, exception_term);
    PL_thread_destroy_engine();
  }

  GC_starting = FALSE;

  return NULL;
}


static int
GCthread(void)
{ if ( GC_id <= 0 )
  { if ( COMPARE_AND_SWAP_INT(&GC_starting, FALSE, TRUE) )
    { pthread_attr_t attr;
      pthread_t thr;
      int rc;

      if ( !GD->thread.gc.initialized )
      { pthread_mutex_init(&GD->thread.gc.mutex, NULL);
	pthread_cond_init(&GD->thread.gc.cond, NULL);
	GD->thread.gc.initialized = TRUE;
      } else
      { GD->thread.gc.requests = 0;
      }

      pthread_attr_init(&attr);
      rc = pthread_create(&thr, &attr, GCmain, NULL);
      pthread_attr_destroy(&attr);
      if ( rc != 0 )
	GC_starting = FALSE;
    }
  }

  return GC_id;
}


static int
gc_sig_request(int sig)
{ switch(sig)
  { case SIG_ATOM_GC:
      return GCREQUEST_AGC;
    case SIG_CLAUSE_GC:
      return GCREQUEST_CGC;
    case SIG_PLABORT:
      return GCREQUEST_ABORT;
    default:
      assert(0);
      return 0;
  }
}


static int
signalGCThreadCond(int tid, int sig)
{ (void)tid;

  pthread_mutex_lock(&GD->thread.gc.mutex);
  GD->thread.gc.requests |= gc_sig_request(sig);
  pthread_cond_signal(&GD->thread.gc.cond);
  pthread_mutex_unlock(&GD->thread.gc.mutex);

  return TRUE;
}


int
signalGCThread(int sig)
{ GET_LD
  int tid;

  if ( truePrologFlag(PLFLAG_GCTHREAD) &&
       !GD->bootsession &&
       (tid = GCthread()) > 0 &&
       signalGCThreadCond(tid, sig) )
    return TRUE;

  return raiseSignal(LD, sig);
}


static int
gc_running(void)
{ int tid;
  PL_thread_info_t *info;

  if ( (tid=GC_id) > 0 && (info = GD->thread.threads[tid]) &&
       info->status == PL_THREAD_RUNNING )
    return tid;

  return 0;
}


int
isSignalledGCThread(DECL_LD int sig)
{ if ( gc_running() )
  { return (GD->thread.gc.requests & gc_sig_request(sig)) != 0;
  } else
  { return PL_pending(sig);
  }
}


static
PRED_IMPL("$gc_wait", 1, gc_wait, 0)
{ PRED_LD

  for(;;)
  { unsigned int req;
    atom_t action;

    pthread_mutex_lock(&GD->thread.gc.mutex);
    req = GD->thread.gc.requests;
    if ( !req )
      pthread_cond_wait(&GD->thread.gc.cond, &GD->thread.gc.mutex);
    pthread_mutex_unlock(&GD->thread.gc.mutex);

    if ( (req&GCREQUEST_ABORT) )
      action = ATOM_abort;
    else if ( (req&GCREQUEST_AGC) )
      action = ATOM_garbage_collect_atoms;
    else if ( (req&GCREQUEST_CGC) )
      action = ATOM_garbage_collect_clauses;
    else
      continue;

    return PL_unify_atom(A1, action);
  }
}


static
PRED_IMPL("$gc_clear", 1, gc_clear, 0)
{ PRED_LD
  atom_t action;

  if ( PL_get_atom_ex(A1, &action) )
  { unsigned int mask;

    if ( action == ATOM_garbage_collect_atoms )
      mask = GCREQUEST_AGC;
    else if ( action == ATOM_garbage_collect_clauses )
      mask = GCREQUEST_CGC;
    else
      return PL_domain_error("action", A1);

    pthread_mutex_lock(&GD->thread.gc.mutex);
    GD->thread.gc.requests &= ~mask;
    pthread_mutex_unlock(&GD->thread.gc.mutex);

    return TRUE;
  }

  return FALSE;
}


static rc_cancel
cancelGCThread(int tid)
{ signalGCThreadCond(tid, SIG_PLABORT);
  return PL_THREAD_CANCEL_MUST_JOIN;
}

static
PRED_IMPL("$gc_stop", 0, gc_stop, 0)
{ int tid;

  if ( (tid=gc_running()) )
    return cancelGCThread(tid) != PL_THREAD_CANCEL_FAILED;

  return FALSE;
}


		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
thread_statistics(+Thread, +Key, -Value)
    Same as statistics(+Key, -Value) but operates on another thread.

statistics(heapused, X) walks along  all   threads  and  therefore locks
L_THREAD. As it returns the same result   on  all threads we'll redirect
this to run on our own thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("thread_statistics", 3, thread_statistics, 0)
{ PRED_LD
  PL_thread_info_t *info;
  PL_local_data_t *ld;
  int rval;
  atom_t k;

  PL_LOCK(L_THREAD);
  if ( !get_thread(A1, &info, TRUE) )
  { PL_UNLOCK(L_THREAD);
    fail;
  }

  if ( !(ld=info->thread_data) )
  { PL_UNLOCK(L_THREAD);
    return PL_error(NULL, 0, NULL,
		    ERR_PERMISSION,
		    ATOM_statistics, ATOM_thread, A1);
  }

  if ( !PL_get_atom(A2, &k) )
    k = 0;

  if ( k == ATOM_heapused )
    ld = LD;
  else if ( k == ATOM_cputime || k == ATOM_runtime )
    ld->statistics.user_cputime = ThreadCPUTime(PASS_AS_LD(ld) CPU_USER);
  else if ( k == ATOM_system_time )
    ld->statistics.system_cputime = ThreadCPUTime(PASS_AS_LD(ld) CPU_SYSTEM);
  else if ( k == ATOM_warnings || k == ATOM_errors )
  { PL_UNLOCK(L_THREAD);
    return PL_unify_integer(A3, k == ATOM_warnings ? ld->statistics.warnings
						   : ld->statistics.errors);
  }

  if ( LD == ld )		/* self: unlock first to avoid deadlock */
  { PL_UNLOCK(L_THREAD);
    return pl_statistics_ld(A2, A3, ld);
  }

  rval = pl_statistics_ld(A2, A3, ld);
  PL_UNLOCK(L_THREAD);

  return rval;
}


#ifdef __WINDOWS__

/* How to make the memory visible?
*/

					/* see also pl-nt.c */
#define nano * 0.000000001
#define ntick 100.0

double
ThreadCPUTime(DECL_LD int which)
{ PL_thread_info_t *info = LD->thread.info;
  double t;
  FILETIME created, exited, kerneltime, usertime;
  HANDLE win_thread;

  if ( !info->has_tid )
    return 0.0;

  if ( !(win_thread = get_windows_thread(info)) )
    return 0.0;

  if ( GetThreadTimes(win_thread,
		      &created, &exited, &kerneltime, &usertime) )
  { FILETIME *p;

    if ( which == CPU_SYSTEM )
      p = &kerneltime;
    else
      p = &usertime;

    t = (double)p->dwHighDateTime * (4294967296.0 * ntick nano);
    t += (double)p->dwLowDateTime  * (ntick nano);
  } else
    t = 0.0;

  close_windows_thread(win_thread);

  return t;
}

#else /*__WINDOWS__*/

#define timespec_to_double(ts) \
	((double)(ts).tv_sec + (double)(ts).tv_nsec/(double)1000000000.0)

#ifdef PTHREAD_CPUCLOCKS
#define NO_THREAD_SYSTEM_TIME 1

double
ThreadCPUTime(DECL_LD int which)
{ PL_thread_info_t *info = LD->thread.info;

  if ( which == CPU_SYSTEM )
    return 0.0;

  if ( info->has_tid )
  { clockid_t clock_id;
    struct timespec ts;
    int rc;

    if ( (rc=pthread_getcpuclockid(info->tid, &clock_id)) == 0 )
    { if (clock_gettime(clock_id, &ts) == 0)
	return timespec_to_double(ts);
    } else
    { DEBUG(MSG_THREAD,
	    Sdprintf("Could not get thread time: %s\n", strerror(rc)));
    }
  }

  return 0.0;
}

#else /*PTHREAD_CPUCLOCKS*/

#ifdef HAVE_MACH_THREAD_ACT_H	/* MacOS and other Mach based systems */

#include <mach/thread_act.h>

double
ThreadCPUTime(DECL_LD int which)
{ PL_thread_info_t *info = LD->thread.info;

  if ( info->has_tid )
  { kern_return_t error;
    struct thread_basic_info th_info;
    mach_msg_type_number_t th_info_count = THREAD_BASIC_INFO_COUNT;
    thread_t thread = pthread_mach_thread_np(info->tid);

    if ((error = thread_info(thread, THREAD_BASIC_INFO,
			     (thread_info_t)&th_info, &th_info_count))
	!= KERN_SUCCESS)
      return 0.0;

    if ( which == CPU_SYSTEM )
      return ( th_info.system_time.seconds +
	       th_info.system_time.microseconds / 1e6 );
    else
      return ( th_info.user_time.seconds +
	       th_info.user_time.microseconds / 1e6 );
  }

  return 0.0;
}

#else /*HAVE_MACH_THREAD_ACT_H*/

#ifdef LINUX_PROCFS

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unfortunately POSIX threads does not define a   way  to get the CPU time
per thread. Some systems do have mechanisms  for that. POSIX does define
clock_gettime(CLOCK_THREAD_CPUTIME_ID), but it certainly doesn't work in
Linux 2.6.8.

Autoconf detects the presense of  /proc  and   we  read  the values from
there. This is rather slow. To make things not  too bad we use a pool of
open handles to entries in the /proc  table. All this junk should really
be moved into a file trying to implement  thread CPU time for as many as
possible platforms. If you happen to know  such a library, please let me
know.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <fcntl.h>

#define CACHED_PROCPS_ENTRIES 5

typedef struct
{ int tid;				/* process id */
  int fd;				/* file descriptor */
  int offset;				/* start of numbers */
  int usecoount;
} procps_entry;

static procps_entry procps_entries[CACHED_PROCPS_ENTRIES]; /* cached entries */

static void
close_procps_entry(procps_entry *e)
{ if ( e->tid )
  { close(e->fd);
    memset(e, 0, sizeof(*e));
  }
}


static procps_entry*
reclaim_procps_entry()
{ procps_entry *e, *low;
  int i; int lowc;

  low=e=procps_entries;
  lowc=low->usecoount;

  for(e++, i=1; i<CACHED_PROCPS_ENTRIES; e++, i++)
  { if ( e->usecoount < lowc )
    { lowc = e->usecoount;
      low = e;
    }
  }

  for(e=procps_entries, i=0; i<CACHED_PROCPS_ENTRIES; e++, i++)
    e->usecoount = 0;

  close_procps_entry(low);

  return low;
}


static procps_entry *
open_procps_entry(procps_entry *e, int tid)
{ char fname[256];
  int fd;

  sprintf(fname, "/proc/self/task/%d/stat", tid);
  if ( (fd=open(fname, O_RDONLY)) >= 0 )
  { char buffer[1000];
    int pos;

    pos = read(fd, buffer, sizeof(buffer)-1);
    if ( pos > 0 )
    { char *bp;

      buffer[pos] = EOS;
      if ( (bp=strrchr(buffer, ')')) )
      { e->tid = tid;
	e->fd = fd;
	e->offset = (bp-buffer)+4;
	e->usecoount = 1;

	return e;
      }
    }
  }

  return NULL;
}


static procps_entry*
get_procps_entry(int tid)
{ int i;
  procps_entry *e;

  for(e=procps_entries, i=0; i<CACHED_PROCPS_ENTRIES; e++, i++)
  { if ( e->tid == tid )
    { e->usecoount++;

      return e;
    }
  }

  for(e=procps_entries, i=0; i<CACHED_PROCPS_ENTRIES; e++, i++)
  { if ( e->tid == 0 )
      return open_procps_entry(e, tid);
  }

  e = reclaim_procps_entry();
  return open_procps_entry(e, tid);
}


double
ThreadCPUTime(DECL_LD int which)
{ PL_thread_info_t *info = LD->thread.info;
  procps_entry *e;

  if ( (e=get_procps_entry(info->pid)) )
  { char buffer[1000];
    char *s;
    int64_t ticks;
    int i, n, nth = 10;			/* user time */

    if ( which == CPU_SYSTEM )
      nth++;

    lseek(e->fd, e->offset, SEEK_SET);
    n = read(e->fd, buffer, sizeof(buffer)-1);
    if ( n >= 0 )
      buffer[n] = EOS;
					/* most likely does not need reuse */
    if ( info->status != PL_THREAD_RUNNING )
      close_procps_entry(e);

    for(s=buffer, i=0; i<nth; i++, s++)
    { while(*s != ' ')
	s++;
    }

    ticks = atoll(s);
    return (double)ticks/100.0;
  }

  return 0.0;
}

#else /*LINUX_PROCFS*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Systems where we can get the  thread   CPU  time from the calling thread
only. Upto Linux 2.4, linux threads where   more  like processes, and we
can  get  these  using  times().  Some    POSIX  systems  have  a  clock
CLOCK_THREAD_CPUTIME_ID. Odly enough, on Debian etche at least, fetching
this with clock_gettime() produces bogus,  but   getting  it through the
syscall interface works. This might  be   because  the  kernel is fairly
up-to-date, but the C library is very much out of data (glibc 2.3)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef SIG_SYNCTIME
#define SIG_SYNCTIME SIGUSR1
#endif

#ifdef USE_SEM_OPEN
static sem_t *sem_synctime_ptr;
#else
static sem_t sem_synctime;			/* used for atom-gc */
#define sem_synctime_ptr (&sem_synctime)
#endif

#ifdef LINUX_CPUCLOCKS
#define NO_THREAD_SYSTEM_TIME 1

static void
SyncUserCPU(int sig)
{ GET_LD

  if ( LD )
  { struct timespec ts;

    if ( syscall(__NR_clock_gettime, CLOCK_THREAD_CPUTIME_ID, &ts) == 0 )
    { LD->statistics.user_cputime = timespec_to_double(ts);
    } else
    { perror("clock_gettime");
    }
  }

  if ( sig )
    sem_post(sem_synctime_ptr);
}


static void
SyncSystemCPU(int sig)
{ assert(0);
}

#else /*LINUX_CPUCLOCKS*/
#define THREAD_CPU_BY_PID 1

static void
SyncUserCPU(int sig)
{ GET_LD

  if ( LD )
    LD->statistics.user_cputime = CpuTime(CPU_USER);
  if ( sig )
    sem_post(sem_synctime_ptr);
}


static void
SyncSystemCPU(int sig)
{ GET_LD

  if ( LD )
    LD->statistics.system_cputime = CpuTime(CPU_SYSTEM);
  if ( sig )
    sem_post(sem_synctime_ptr);
}

#endif  /*LINUX_CPUCLOCKS*/

double
ThreadCPUTime(DECL_LD int which)
{ GET_LD
  PL_thread_info_t *info = LD->thread.info;

#ifdef NO_THREAD_SYSTEM_TIME
  if ( which == CPU_SYSTEM )
    return 0.0;
#endif

  if ( info->thread_data == LD )
  { if ( which == CPU_USER )
      SyncUserCPU(0);
    else
      SyncSystemCPU(0);
  } else
  { struct sigaction old;
    struct sigaction new;
    sigset_t sigmask;
    sigset_t set;
    int ok;

    blockSignals(&set);
    sem_init(sem_synctime_ptr, USYNC_THREAD, 0);
    allSignalMask(&sigmask);
    memset(&new, 0, sizeof(new));
    new.sa_handler = (which == CPU_USER ? SyncUserCPU : SyncSystemCPU);
    new.sa_flags   = SA_RESTART;
    new.sa_mask    = sigmask;
    sigaction(SIG_SYNCTIME, &new, &old);

    if ( info->has_tid )
      ok = (pthread_kill(info->tid, SIG_SYNCTIME) == 0);
    else
      ok = FALSE;

    if ( ok )
    { while( sem_wait(sem_synctime_ptr) == -1 && errno == EINTR )
	;
    }
    sem_destroy(sem_synctime_ptr);
    sigaction(SIG_SYNCTIME, &old, NULL);
    unblockSignals(&set);
    if ( !ok )
      return 0.0;
  }

  if ( which == CPU_USER )
    return info->thread_data->statistics.user_cputime;
  else
    return info->thread_data->statistics.system_cputime;
}

#endif /*LINUX_PROCFS*/
#endif /*HAVE_MACH_THREAD_ACT_H*/
#endif /*PTHREAD_CPUCLOCKS*/
#endif /*__WINDOWS__*/


		 /*******************************
		 *     ITERATE OVER THREADS	*
		 *******************************/

void
forThreadLocalDataUnsuspended(void (*func)(PL_local_data_t *, void* ctx),
			      void *ctx)
{ GET_LD
  int me = PL_thread_self();
  int i;

  for( i=1; i<=GD->thread.highest_id; i++ )
  { if ( i != me )
    { PL_thread_info_t *info = GD->thread.threads[i];

      if ( info && info->thread_data &&
	   ( info->status == PL_THREAD_RUNNING || info->in_exit_hooks ) )
      { PL_local_data_t *ld;

	if ( (ld = acquire_ldata(info)) )
	{ simpleMutexLock(&ld->thread.scan_lock);
	  (*func)(ld, ctx);
	  simpleMutexUnlock(&ld->thread.scan_lock);
	}
      }
    }
  }
}


		 /*******************************
		 *	 ATOM MARK SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We do not register atoms  in   message  queues as the PL_register_atom()
calls seriously harms concurrency  due  to   contention  on  L_ATOM. So,
instead we must sweep atoms in records  in the message queues. Note that
atom-gc runs with the L_THREAD mutex  locked.   As  both he thread mutex
queue tables are guarded by this  mutex, the loops in markAtomsThreads()
are safe.

The marking must be synchronised with removing   a  message from a queue
(get_message())  and  destroy_message_queue().  Notably  the  latter  is
tricky as this deletes `queue->gc_mutex` and frees the queue, so we must
ensure we do not get into   destroy_message_queue()  when AGC is marking
the queue's messages.  There are three types of queues:

  - Thread queues.  AGC is synced with thread creation and destruction
    and thus safe.
  - Anonymous message queues. These are reclaimed from AGC and thus
    safe.
  - Named queues.  These are destroyed using message_queue_destroy/1.
    For this case we
    - Use `queueTable_mutex` to sync deletion from `queueTable` with
      enumeration in markAtomsMessageQueues() which ensures we safely
      get the queue with queue->gc_mutex locked.
    - We hold queue->gc_mutex during the scan, which delays
      message_queue_destroy/1 to set `queue->destroyed`.
    - If `queue->destroyed` is set, AGC cannot get the queue anymore
      and we can safely discard it.

This is implemented by Keri Harris and Jan Wielemaker. We are not really
happy with the complicated  locking.  Alternatively   we  could  use the
linger interface where the lingering queues   are reclaimed by AGC. That
might be cleaner, but is a  more   involved  patch  that doesn't perform
significantly better.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
markAtomsMessageQueue(message_queue *queue)
{ thread_message *msg;

  for(msg=queue->head; msg; msg=msg->next)
  { markAtomsRecord(msg->message);
  }
}


void
markAtomsThreadMessageQueue(PL_local_data_t *ld)
{ message_queue *q = &ld->thread.messages;

  simpleMutexLock(&q->gc_mutex);
  markAtomsMessageQueue(q);
  simpleMutexUnlock(&q->gc_mutex);
}


void
markAtomsMessageQueues(void)
{ if ( queueTable )
  { message_queue *q;
    TableEnum e = newTableEnum(queueTable);

    while ( TRUE )
    { simpleMutexLock(&queueTable_mutex);
      if ( !advanceTableEnum(e, NULL, (void**)&q) )
      { simpleMutexUnlock(&queueTable_mutex);
        break;
      }
      simpleMutexLock(&q->gc_mutex);
      simpleMutexUnlock(&queueTable_mutex);
      markAtomsMessageQueue(q);
      simpleMutexUnlock(&q->gc_mutex);
    }
    freeTableEnum(e);
  }
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
localiseDefinition(Definition def)
    Create a thread-local definition for the predicate `def'.

    This function is called from getProcDefinition() if the procedure is
    not yet `localised'.  Calling this function must be guarded by the
    L_PREDICATE mutex.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef offsetof
#define offsetof(structure, field) ((int) &(((structure *)NULL)->field))
#endif

static void
registerLocalDefinition(Definition def)
{ GET_LD
  DefinitionChain cell = allocHeapOrHalt(sizeof(*cell));

  cell->definition = def;
  cell->next = LD->thread.local_definitions;
  LD->thread.local_definitions = cell;
}


static void
unregisterLocalDefinition(Definition def, PL_local_data_t *ld)
{ DefinitionChain cell;

  for(cell = ld->thread.local_definitions; cell; cell = cell->next)
  { if ( cell->definition == def )
    { cell->definition = NULL;
      return;
    }
  }
}


LocalDefinitions
new_ldef_vector(void)
{ LocalDefinitions f = allocHeapOrHalt(sizeof(*f));

  memset(f, 0, sizeof(*f));
  f->blocks[0] = f->preallocated - 1;
  f->blocks[1] = f->preallocated - 1;
  f->blocks[2] = f->preallocated - 1;

  return f;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called  from  destroyDefinition()   for    a   thread-local   predicate.
destroyDefinition() is called  for  destroying   temporary  modules.  If
thread-local predicates are defined in the   temporary module these must
be destroyed and the registration with  module   must  be removed or the
thread cleanup will access the destroyed predicate.

Now, the thread having a localization for  this predicate is most likely
the calling thread, but in theory other threads can be involved and thus
we need to scan the entire localization array.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
destroyLocalDefinitions(Definition def)
{ GET_LD
  LocalDefinitions ldefs = def->impl.local.local;
  int b;

  for(b=0; b<MAX_BLOCKS; b++)
  { Definition *d0 = ldefs->blocks[b];

    if ( d0 )
    { size_t bs = (size_t)1<<b;
      size_t tid = bs;
      size_t end = tid+bs;

      for(tid=bs; tid<end; tid++)
      { if ( d0[tid] )
	{ PL_thread_info_t *info = GD->thread.threads[tid];
	  PL_local_data_t *ld;

	  if ( LD )					/* See (*) */
	    ld = acquire_ldata(info);
	  else
	    ld = info->thread_data;

	  DEBUG(MSG_THREAD_LOCAL,
		if ( ld != LD )
		  Sdprintf("Destroying thread local predicate %s "
			   "with active local definitions\n",
			   predicateName(def)));

	  unregisterLocalDefinition(def, ld);
	  destroyLocalDefinition(def, tid);

	  if ( LD )
	    release_ldata(ld);
	}
      }
    }
  }

  free_ldef_vector(ldefs);
  def->impl.local.local = NULL;
}


void
free_ldef_vector(LocalDefinitions ldefs)
{ int i;

  for(i=3; i<MAX_BLOCKS; i++)
  { size_t bs = (size_t)1<<i;
    Definition *d0 = ldefs->blocks[i];

    if ( d0 )
      freeHeap(d0+bs, bs*sizeof(Definition));
  }

  freeHeap(ldefs, sizeof(*ldefs));
}


Definition
localiseDefinition(Definition def)
{ Definition local = allocHeapOrHalt(sizeof(*local));
  size_t bytes = sizeof(arg_info)*def->functor->arity;

  *local = *def;
  local->impl.any.args = allocHeapOrHalt(bytes);
  memcpy(local->impl.any.args, def->impl.any.args, bytes);
  clear(local, P_THREAD_LOCAL|P_DIRTYREG);	/* remains P_DYNAMIC */
  local->impl.clauses.first_clause = NULL;
  local->impl.clauses.clause_indexes = NULL;
  ATOMIC_INC(&GD->statistics.predicates);
  ATOMIC_ADD(&local->module->code_size, sizeof(*local));
  DEBUG(MSG_PRED_COUNT, Sdprintf("Localise def[%d] %s at %p\n",
				 PL_thread_self(), predicateName(def), local));

  setDefaultSupervisor(local);
  registerLocalDefinition(def);

  return local;
}


void
cleanupLocalDefinitions(PL_local_data_t *ld)
{ DefinitionChain ch = ld->thread.local_definitions;
  DefinitionChain next;
  unsigned int id = ld->thread.info->pl_tid;

  for( ; ch; ch = next)
  { Definition def = ch->definition;
    next = ch->next;

    if ( def )
    { DEBUG(MSG_CLEANUP,
	    Sdprintf("Clean local def in thread %d for %s\n",
		     id,
		     predicateName(def)));

      assert(true(def, P_THREAD_LOCAL));
      destroyLocalDefinition(def, id);
    }
    freeHeap(ch, sizeof(*ch));
  }
}


static size_t
sizeof_local_definitions(PL_local_data_t *ld)
{ DefinitionChain ch = ld->thread.local_definitions;
  size_t size = 0;

  for( ; ch; ch = ch->next)
  { Definition def = ch->definition;
    Definition local;

    assert(true(def, P_THREAD_LOCAL));
    if ( (local = getProcDefinitionForThread(def, ld->thread.info->pl_tid)) )
      size += sizeof_predicate(local);
  }

  return size;
}



/** '$thread_local_clause_count'(:Head, +Thread, -NumberOfClauses) is semidet.

True when NumberOfClauses is the number  of clauses for the thread-local
predicate Head in Thread.  Fails silently if

  - Head does not refer to an existing predicate
  - The predicate exists, but is not thread local (should
    this be an error?)
  - The thread does not exist
  - The definition was never localised for Thread. One
    could argue to return 0 for this case.

@author	Keri Harris
*/

static
PRED_IMPL("$thread_local_clause_count", 3, thread_local_clause_count, 0)
{ PRED_LD
  Procedure proc;
  Definition def;
  PL_thread_info_t *info;
  int number_of_clauses = 0;

  term_t pred = A1;
  term_t thread = A2;
  term_t count  = A3;

  if ( !get_procedure(pred, &proc, 0, GP_RESOLVE) )
    fail;

  def = proc->definition;
  if ( false(def, P_THREAD_LOCAL) )
    fail;

  if ( !get_thread(thread, &info, FALSE) )
    fail;

  PL_LOCK(L_THREAD);
  if ( (def = getProcDefinitionForThread(proc->definition, info->pl_tid)) )
    number_of_clauses = def->impl.clauses.number_of_clauses;
  PL_UNLOCK(L_THREAD);

  return PL_unify_integer(count, number_of_clauses);
}


#else /*O_PLMT*/

int
signalGCThread(int sig)
{ GET_LD

  return raiseSignal(LD, sig);
}

int
isSignalledGCThread(DECL_LD int sig)
{ return PL_pending(sig);
}


int
PL_thread_self()
{ return -2;
}


int
PL_unify_thread_id(term_t t, int i)
{ if ( i == -2 )
    return PL_unify_atom(t, ATOM_main);
  return -1;
}


int
PL_thread_at_exit(void (*function)(void *), void *closure, int global)
{ return FALSE;
}

int
PL_thread_attach_engine(PL_thread_attr_t *attr)
{ return -2;
}

int
PL_thread_destroy_engine()
{ return FALSE;
}

#ifdef __WINDOWS__
int
PL_w32thread_raise(DWORD id, int sig)
{ return PL_raise(sig);
}
#endif

foreign_t
pl_thread_self(term_t id)
{ return PL_unify_atom(id, ATOM_main);
}

PL_engine_t
PL_current_engine(void)
{ return LD;
}

int
PL_set_engine(PL_engine_t new, PL_engine_t *old)
{ if ( new != LD && new != PL_ENGINE_MAIN )
    return PL_ENGINE_INVAL;

  if ( old )
  { *old = LD;
  }

  return PL_ENGINE_SET;
}

PL_engine_t
PL_create_engine(PL_thread_attr_t *attributes)
{ return NULL;
}

int
PL_destroy_engine(PL_engine_t e)
{ fail;
}

void
PL_cleanup_fork(void)
{
}

double
ThreadCPUTime(DECL_LD int which) {
  return CpuTime(which);
}

void
initPrologThreads()
{					/* TBD: only once? */
#ifdef O_MULTIPLE_ENGINES
  PL_current_engine_ptr = &PL_local_data;
#endif
}

int
PL_get_thread_alias(int tid, atom_t *alias)
{ *alias = ATOM_main;
  return TRUE;
}

#endif  /*O_PLMT*/

int
get_prop_def(term_t t, atom_t expected, const tprop *list, const tprop **def)
{ GET_LD
  functor_t f;

  if ( PL_get_functor(t, &f) )
  { const tprop *p = list;

    for( ; p->functor; p++ )
    { if ( f == p->functor )
      { *def = p;
        return TRUE;
      }
    }

    PL_error(NULL, 0, NULL, ERR_DOMAIN, expected, t);
    return -1;
  }

  if ( PL_is_variable(t) )
    return 0;

  PL_error(NULL, 0, NULL, ERR_TYPE, expected, t);
  return -1;
}


		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the summed size of the local stack.   This is a measure for the CGC
marking cost.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
cgc_thread_stats(DECL_LD cgc_stats *stats)
{
#ifdef O_PLMT
  int i;

  for(i=1; i<=GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    PL_local_data_t *ld = acquire_ldata(info);

    if ( ld )
    { char *ltop  = (char*)ld->stacks.local.top;
      char *lbase = (char*)ld->stacks.local.base;

      if ( ltop > lbase )
      { stats->local_size += ltop-lbase;
	stats->threads++;
	stats->erased_skipped += ld->clauses.erased_skipped;
      }
      release_ldata(ld);
    }
    if ( GD->clauses.cgc_active )
      return FALSE;
  }
#else
  stats->local_size = usedStack(local);
  stats->threads = 1;
  stats->erased_skipped = LD->clauses.erased_skipped;
#endif

  return TRUE;
}


		 /*******************************
		 *    HASH-TABLE KVS IN USE     *
		 *******************************/

int
pl_kvs_in_use(KVS kvs)
{
#ifdef O_PLMT
  int i;

  for(i=1; i<=GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->access.kvs == kvs )
    { return TRUE;
    }
  }
#endif

  return FALSE;
}


		 /*******************************
		 *      ATOM-TABLE IN USE       *
		 *******************************/

int
pl_atom_table_in_use(AtomTable atom_table)
{
#ifdef O_PLMT
  int i;

  for(i=1; i<=GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->access.atom_table == atom_table )
    { return TRUE;
    }
  }
#endif

  return FALSE;
}


int
pl_atom_bucket_in_use(Atom *bucket)
{
#ifdef O_PLMT
  int i;

  for(i=1; i<=GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->access.atom_bucket == bucket )
    { return TRUE;
    }
  }
#endif

  return FALSE;
}


#ifdef O_PLMT
static int
ldata_in_use(PL_local_data_t *ld)
{ int i;

  for(i=1; i<=GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->access.ldata == ld )
    { return TRUE;
    }
  }

  return FALSE;
}
#endif


Atom**
pl_atom_buckets_in_use(void)
{
#ifdef O_PLMT
  int i, index=0;
  size_t sz = 32;

  Atom **buckets = allocHeapOrHalt(sz * sizeof(Atom*));
  memset(buckets, 0, sz * sizeof(Atom*));

  for(i=1; i<=GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->access.atom_bucket )
    { if ( index >= sz-1 )
      { int j = 0;
        size_t oldsz = sz;
        sz *= 2;
        Atom **newbuckets = allocHeapOrHalt(sz * sizeof(Atom*));
	memset(newbuckets, 0, sz * sizeof(Atom*));
	for ( ; j < oldsz; j++ )
	{ newbuckets[j] = buckets[j];
	}
	PL_free(buckets);
        buckets = newbuckets;
      }
      buckets[index] = info->access.atom_bucket;
      if ( buckets[index] )	/* atom_bucket may have been released */
        index++;
    }
  }

  return buckets;
#endif

  return NULL;
}


Definition*
predicates_in_use(void)
{
#ifdef O_PLMT
  if ( GD->cleaning != CLN_DATA )
  { int i, index=0;
    size_t sz = 32;

    Definition *buckets = allocHeapOrHalt(sz * sizeof(Definition));
    memset(buckets, 0, sz * sizeof(Definition*));

    for(i=1; i<=GD->thread.highest_id; i++)
    { PL_thread_info_t *info = GD->thread.threads[i];
      if ( info && info->access.predicate )
      { if ( index >= sz-1 )
	{ int j = 0;
	  size_t oldsz = sz;
	  sz *= 2;
	  Definition *newbuckets = allocHeapOrHalt(sz * sizeof(Definition));
	  memset(newbuckets, 0, sz * sizeof(Definition));
	  for ( ; j < oldsz; j++ )
	  { newbuckets[j] = buckets[j];
	  }
	  PL_free(buckets);
	  buckets = newbuckets;
	}
	buckets[index] = info->access.predicate;
	if ( buckets[index] )	/* atom_bucket may have been released */
	  index++;
      }
    }

    return buckets;
  }
#endif

  return NULL;
}


		 /*******************************
		 *     FUNCTOR-TABLE IN USE     *
		 *******************************/

int
pl_functor_table_in_use(FunctorTable functor_table)
{
#ifdef O_PLMT
  int me = PL_thread_self();
  int i;

  for(i=1; i<=GD->thread.highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( i != me && info && info->access.functor_table == functor_table )
    { return TRUE;
    }
  }
#endif

  return FALSE;
}

		 /*******************************
		 * CLAUSE/3 PREDICATE REFERENCES*
		 *******************************/

#ifdef O_PLMT

static void
init_predicate_references(PL_local_data_t *ld)
{ definition_refs *refs = &ld->predicate_references;

  memset(refs, 0, sizeof(*refs));
  refs->blocks[0] = refs->preallocated - 1;
  refs->blocks[1] = refs->preallocated - 1;
  refs->blocks[2] = refs->preallocated - 1;
}

void
free_predicate_references(PL_local_data_t *ld)
{ definition_refs *refs = &ld->predicate_references;
  int i;

  for(i=3; i<MAX_BLOCKS; i++)
  { size_t bs = (size_t)1<<i;
    definition_ref *d0 = refs->blocks[i];

    if ( d0 )
    { freeHeap(d0+bs, bs*sizeof(definition_ref));
      refs->blocks[i] = NULL;
    }
  }
}
#endif /*O_PLMT*/

#define cgcActivatePredicate(def, gen) LDFUNC(cgcActivatePredicate, def, gen)
static void
cgcActivatePredicate(DECL_LD Definition def, gen_t gen)
{ DirtyDefInfo ddi;

  if ( (ddi=lookupHTable(GD->procedures.dirty, def)) )
    ddi_add_access_gen(ddi, gen);
}


definition_ref *
pushPredicateAccessObj(DECL_LD Definition def)
{ definition_refs *refs = &LD->predicate_references;
  definition_ref *dref;
  size_t top = refs->top+1;
  size_t idx = MSB(top);

  DEBUG(MSG_CGC_PRED_REF,
	Sdprintf("pushPredicateAccess(%s)\n", predicateName(def)));

  if ( top >= ((size_t)1<<MAX_BLOCKS) - (LD->exception.processing ? 0 : 1000) )
    return PL_representation_error("predicate references"),NULL;

  if ( !refs->blocks[idx] )
  { size_t bs = (size_t)1<<idx;
    definition_ref *newblock;

    if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(definition_ref))) )
      return PL_no_memory(),NULL;

    memset(newblock, 0, bs*sizeof(definition_ref));
    if ( !COMPARE_AND_SWAP_PTR(&refs->blocks[idx], NULL, newblock-bs) )
      PL_free(newblock);
  }

  enterDefinition(def);			/* probably not needed in the end */
  dref = &refs->blocks[idx][top];
  dref->predicate  = def;
  dref->generation = current_generation(def);
  refs->top = top;
  do
  { dref->generation = current_generation(def);
  } while ( dref->generation != current_generation(def) );

  return dref;
}


static definition_ref *
def_ref(const definition_refs *refs, size_t top)
{ size_t idx = MSB(top);

  return &refs->blocks[idx][top];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that out-of-order access is possible.   Matt  Lilley created a case
where the term_expansion/2 rule for end_of_file loads source files using
forall(clause(chain_modules(M)),     use_module(M)).     This      locks
chain_modules/1. The created system:'$load_context_module/2'   fact gets
asserted to the load  context  and  is   popped  when  loading  the file
completes, while the reference to  chain_modules/1   is  popped when the
enumeration completes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
popPredicateAccess(DECL_LD Definition def)
{ definition_refs *refs = &LD->predicate_references;
  definition_ref *dref;

  DEBUG(MSG_CGC_PRED_REF,
	Sdprintf("popPredicateAccess(%s)\n", predicateName(def)));

  dref = def_ref(refs, refs->top);
  if ( dref->predicate == def )
  { dref->predicate  = NULL;
    dref->generation = 0;
  } else
  { size_t top;

    DEBUG(MSG_CGC_PRED_REF, Sdprintf("  Out or order!\n"));
    for(top = refs->top; top > 0; top-- )
    { dref = def_ref(refs, top);
      if ( dref->predicate == def )
      { for(; top < refs->top; top++)
	{ definition_ref *dr2 = def_ref(refs, top+1);

	  *dref = *dr2;
	  dref = dr2;
	}
	goto out;
      }
    }
    assert(0);
  }
out:
  leaveDefinition(def);			/* probably not needed in the end */

  refs->top--;
}

size_t
popNPredicateAccess(DECL_LD size_t n)
{ definition_refs *refs = &LD->predicate_references;

  DEBUG(MSG_CGC_PRED_REF,
	Sdprintf("popNPredicateAccess(%d)\n", (int)n));

  while(n-- > 0)
  { definition_ref *dref;
    size_t top = refs->top;
    size_t idx = MSB(top);

    dref = &refs->blocks[idx][top];
    DEBUG(MSG_CGC_PRED_REF,
	  Sdprintf("  -- %s\n", predicateName(dref->predicate)));
    leaveDefinition(dref->predicate);
    dref->predicate  = NULL;
    dref->generation = 0;

    refs->top--;
  }

  return refs->top;
}


static inline int
is_pointer_like(void *ptr)
{
#if SIZEOF_VOIDP == 4
  intptr_t mask = 0x3;
#elif SIZEOF_VOIDP == 8
  intptr_t mask = 0x7;
#else
#error "Unknown pointer size"
#endif
  return ptr && ((intptr_t)ptr&mask) == 0;
}

void
markAccessedPredicates(PL_local_data_t *ld)
{ GET_LD
  definition_refs *refs = &ld->predicate_references;
  size_t i;

  for(i=1; i<=refs->top; i++)
  { int idx = MSB(i);
    volatile definition_ref *drefp = &refs->blocks[idx][i];
    definition_ref dref = *drefp;	/* struct copy */

    if ( is_pointer_like(dref.predicate) )
      cgcActivatePredicate(dref.predicate, dref.generation);
  }
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define NDET PL_FA_NONDETERMINISTIC
#define META PL_FA_TRANSPARENT

BeginPredDefs(thread)
#ifdef O_PLMT
  PRED_DEF("thread_alias",           1, thread_alias,	       0)
  PRED_DEF("thread_detach",	     1,	thread_detach,	       PL_FA_ISO)
  PRED_DEF("thread_join",	     2,	thread_join,	       0)
  PRED_DEF("thread_statistics",	     3,	thread_statistics,     0)
  PRED_DEF("thread_property",	     2,	thread_property,       NDET|PL_FA_ISO)
  PRED_DEF("is_thread",		     1,	is_thread,	       0)
  PRED_DEF("$thread_sigwait",	     1, thread_sigwait,	       0)
#ifdef HAVE_PRED_THREAD_AFFINITY
  PRED_DEF("thread_affinity",        3, thread_affinity,       0)
#endif

  PRED_DEF("message_queue_create",   1,	message_queue_create,  0)
  PRED_DEF("message_queue_create",   2,	message_queue_create2, PL_FA_ISO)
  PRED_DEF("message_queue_property", 2,	message_property,      NDET|PL_FA_ISO)
  PRED_DEF("message_queue_set",      2, message_queue_set,     0)

  PRED_DEF("thread_send_message",    2,	thread_send_message,   PL_FA_ISO)
  PRED_DEF("thread_send_message",    3,	thread_send_message,   0)
  PRED_DEF("thread_get_message",     1,	thread_get_message,    PL_FA_ISO)
  PRED_DEF("thread_get_message",     2,	thread_get_message,    PL_FA_ISO)
  PRED_DEF("thread_get_message",     3,	thread_get_message,    PL_FA_ISO)
  PRED_DEF("thread_peek_message",    1,	thread_peek_message_1, PL_FA_ISO)
  PRED_DEF("thread_peek_message",    2,	thread_peek_message_2, PL_FA_ISO)
  PRED_DEF("message_queue_destroy",  1,	message_queue_destroy, PL_FA_ISO)

  PRED_DEF("thread_signal",	     2, thread_signal,         META|PL_FA_ISO)
  PRED_DEF("sig_pending",	     1, sig_pending,           META)
  PRED_DEF("sig_remove",             2, sig_remove,	       META)
  PRED_DEF("$sig_unblock",	     0, sig_unblock,	       0)

  PRED_DEF("thread_wait",	     2, thread_wait,           META)
  PRED_DEF("thread_update",	     2, thread_update,         META)
  PRED_DEF("thread_setconcurrency",  2,	thread_setconcurrency, 0)

  PRED_DEF("$engine_create",	     3,	engine_create,	       0)
  PRED_DEF("engine_destroy",	     1,	engine_destroy,	       0)
  PRED_DEF("engine_next",	     2,	engine_next,	       0)
  PRED_DEF("engine_post",	     2,	engine_post,	       0)
  PRED_DEF("engine_post",	     3,	engine_post,	       0)
  PRED_DEF("engine_fetch",	     1, engine_fetch,	       0)
  PRED_DEF("is_engine",		     1,	is_engine,	       0)

  PRED_DEF("mutex_statistics",	     0,	mutex_statistics,      0)

  PRED_DEF("$thread_local_clause_count", 3, thread_local_clause_count, 0)
  PRED_DEF("$gc_wait",               1, gc_wait,               0)
  PRED_DEF("$gc_clear",              1, gc_clear,              0)
  PRED_DEF("$gc_stop",               0, gc_stop,               0)
#endif
EndPredDefs
