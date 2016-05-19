/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2015, University of Amsterdam,
			      VU University Amsterdam

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

/* #define O_DEBUG 1 */

#define _GNU_SOURCE 1			/* get recursive mutex stuff to */
					/* compile clean with glibc.  Can */
					/* this do any harm? */

#if defined(__MINGW32__)
#define __try
#define __except(_) if (0)
#define __finally
#endif

#ifdef __MINGW32__
#include <winsock2.h>
#include <windows.h>
#endif

#include "pl-incl.h"
#include "pl-tabling.h"
#include "os/pl-cstack.h"
#include "pl-prof.h"
#include <stdio.h>
#include <math.h>
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
#include <linux/unistd.h>
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

static sem_t *sem_mark_ptr;

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

static Table threadTable;		/* name --> integer-id */
static int thread_highest_id;		/* Highest handed thread-id */
static int threads_ready = FALSE;	/* Prolog threads available */
static Table queueTable;		/* name --> queue */
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
{ COUNT_MUTEX_INITIALIZER("L_MISC"),
  COUNT_MUTEX_INITIALIZER("L_ALLOC"),
  COUNT_MUTEX_INITIALIZER("L_ATOM"),
  COUNT_MUTEX_INITIALIZER("L_FLAG"),
  COUNT_MUTEX_INITIALIZER("L_FUNCTOR"),
  COUNT_MUTEX_INITIALIZER("L_RECORD"),
  COUNT_MUTEX_INITIALIZER("L_THREAD"),
  COUNT_MUTEX_INITIALIZER("L_MUTEX"),
  COUNT_MUTEX_INITIALIZER("L_PREDICATE"),
  COUNT_MUTEX_INITIALIZER("L_MODULE"),
  COUNT_MUTEX_INITIALIZER("L_SRCFILE"),
  COUNT_MUTEX_INITIALIZER("L_TABLE"),
  COUNT_MUTEX_INITIALIZER("L_BREAK"),
  COUNT_MUTEX_INITIALIZER("L_FILE"),
  COUNT_MUTEX_INITIALIZER("L_SEETELL"),
  COUNT_MUTEX_INITIALIZER("L_PLFLAG"),
  COUNT_MUTEX_INITIALIZER("L_OP"),
  COUNT_MUTEX_INITIALIZER("L_INIT"),
  COUNT_MUTEX_INITIALIZER("L_TERM"),
  COUNT_MUTEX_INITIALIZER("L_GC"),
  COUNT_MUTEX_INITIALIZER("L_AGC"),
  COUNT_MUTEX_INITIALIZER("L_FOREIGN"),
  COUNT_MUTEX_INITIALIZER("L_OS"),
  COUNT_MUTEX_INITIALIZER("L_LOCALE")
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
initMutexes(void)
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
      initMutexes();
      TLD_alloc(&PL_ldata);
      break;
    case DLL_PROCESS_DETACH:
      deleteMutexes();
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
{ counting_mutex *cm;

#ifdef O_CONTENTION_STATISTICS
  Sdprintf("Name                               locked collisions\n"
	   "----------------------------------------------------\n");
#else
  Sdprintf("Name                               locked\n"
	   "-----------------------------------------\n");
#endif
  PL_LOCK(L_MUTEX);
  for(cm = GD->thread.mutexes; cm; cm = cm->next)
  { int lc;

    if ( cm->count == 0 )
      continue;

    Sdprintf("%-32Us %8d", cm->name, cm->count); /* %Us: UTF-8 string */
#ifdef O_CONTENTION_STATISTICS
    Sdprintf(" %8d", cm->collisions);
#endif
    lc = (cm == &_PL_mutexes[L_MUTEX] ? 1 : 0);

    if ( cm->lock_count > lc )
      Sdprintf(" LOCKS: %d\n", cm->count - lc);
    else
      Sdprintf("\n");
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

static PL_thread_info_t *alloc_thread(void);
static void	unalloc_mutex(pl_mutex *m);
static void	destroy_message_queue(message_queue *queue);
static void	destroy_thread_message_queue(message_queue *queue);
static void	init_message_queue(message_queue *queue, long max_size);
static void	freeThreadSignals(PL_local_data_t *ld);
static void	unaliasThread(atom_t name);
static void	run_thread_exit_hooks(PL_local_data_t *ld);
static void	free_thread_info(PL_thread_info_t *info);
static void	set_system_thread_id(PL_thread_info_t *info);
static int	unify_queue(term_t t, message_queue *q);
static int	get_message_queue_unlocked__LD(term_t t, message_queue **queue ARG_LD);
static int	get_message_queue__LD(term_t t, message_queue **queue ARG_LD);
static void	release_message_queue(message_queue *queue);
static void	initMessageQueues(void);
static pl_mutex *mutexCreate(atom_t name);
static void	initMutexRef(void);
static int	thread_at_exit(term_t goal, PL_local_data_t *ld);
static int	get_thread(term_t t, PL_thread_info_t **info, int warn);
static int	is_alive(int status);
static void	init_predicate_references(PL_local_data_t *ld);
static void	free_predicate_references(PL_local_data_t *ld);
#ifdef O_C_BACKTRACE
static void	print_trace(int depth);
#else
#define		print_trace(depth) (void)0
#endif

static void get_current_timespec(struct timespec *time);
static void carry_timespec_nanos(struct timespec *time);

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
		 *	LOCK ON L_THREAD	*
		 *******************************/

#define LOCK()   PL_LOCK(L_THREAD)
#define UNLOCK() PL_UNLOCK(L_THREAD)


		 /*******************************
		 *     RUNTIME ENABLE/DISABLE	*
		 *******************************/

int
enableThreads(int enable)
{ if ( enable )
  { GD->thread.enabled = TRUE;		/* print system message? */
  } else
  { LOCK();
    if ( GD->statistics.threads_created -
	 GD->statistics.threads_finished == 1 ) /* I am alone :-( */
    { GD->thread.enabled = FALSE;
    } else
    { GET_LD
      term_t key = PL_new_term_ref();

      PL_put_atom(key, ATOM_threads);

      UNLOCK();
      return PL_error(NULL, 0, "Active threads",
		      ERR_PERMISSION,
		      ATOM_modify, ATOM_flag, key);
    }
    UNLOCK();
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

  if ( !info->local_size    ) info->local_size    = GD->options.localSize;
  if ( !info->global_size   ) info->global_size   = GD->options.globalSize;
  if ( !info->trail_size    ) info->trail_size    = GD->options.trailSize;

  if ( !initPrologStacks(info->local_size,
			 info->global_size,
			 info->trail_size) )
  { info->status = PL_THREAD_NOMEM;
    return FALSE;
  }

  initPrologLocalData(info->thread_data);
  info->thread_data->magic = LD_MAGIC;

  return TRUE;
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

  if ( !threads_ready )
    return;				/* Post-mortem */

  info = ld->thread.info;
  DEBUG(MSG_THREAD, Sdprintf("Freeing prolog thread %d (status = %d)\n",
			     info->pl_tid, info->status));

  if ( !after_fork )
  { LOCK();
    if ( info->status == PL_THREAD_RUNNING )
      info->status = PL_THREAD_EXITED;	/* foreign pthread_exit() */
    acknowledge = info->thread_data->exit_requested;
    UNLOCK();

    info->in_exit_hooks = TRUE;
    callEventHook(PL_EV_THREADFINISHED, info);
    run_thread_exit_hooks(ld);
    info->in_exit_hooks = FALSE;
  } else
  { acknowledge = FALSE;
    info->detached = TRUE;		/* cleanup */
  }

#ifdef O_PROFILE
  if ( ld->profile.active )
    activateProfiler(FALSE, ld);
#endif

  cleanupLocalDefinitions(ld);

  DEBUG(MSG_THREAD, Sdprintf("Destroying data\n"));
  ld->magic = 0;
  if ( ld->stacks.global.base )		/* otherwise assume they are not */
    freeStacks(ld);			/* initialised */
  freePrologLocalData(ld);
  clearThreadTablingData(ld);

  /*PL_unregister_atom(ld->prompt.current);*/

  freeThreadSignals(ld);
  time = ThreadCPUTime(ld, CPU_USER);

  if ( !after_fork )
  { LOCK();
    GD->statistics.threads_finished++;
    assert(GD->statistics.threads_created - GD->statistics.threads_finished >= 1);
    GD->statistics.thread_cputime += time;
  }
  destroy_thread_message_queue(&ld->thread.messages);
  free_predicate_references(ld);
  if ( ld->btrace_store )
  { btrace_destroy(ld->btrace_store);
    ld->btrace_store = NULL;
  }
#ifdef O_LOCALE
  if ( ld->locale.current )
    releaseLocale(ld->locale.current);
#endif
  info->thread_data = NULL;		/* avoid a loop */
  info->has_tid = FALSE;		/* needed? */
  if ( !after_fork )
    UNLOCK();

  if ( info->detached || acknowledge )
    free_thread_info(info);

  ld->thread.info = NULL;		/* help force a crash if ld used */
  freeHeap(ld, sizeof(*ld));

  if ( acknowledge )			/* == canceled */
  { pthread_detach(pthread_self());
    sem_post(sem_canceled_ptr);
  }
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
    info->name = ATOM_main;
  }
  set_system_thread_id(info);

  for(i=2; i<=thread_highest_id; i++)
  { if ( (info=GD->thread.threads[i]) )
    { if ( info->status != PL_THREAD_UNUSED )
      { freePrologThread(info->thread_data, TRUE);
	info->status = PL_THREAD_UNUSED;
      }
    }
  }
  thread_highest_id = 1;

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


static void
unalloc_mutex_symbol(void *name, void *value)
{ unalloc_mutex(value);
}


void
initPrologThreads(void)
{ PL_thread_info_t *info;
  static int init_ldata_key = FALSE;

  initAlloc();

#if defined(USE_CRITICAL_SECTIONS) && !defined(O_SHARED_KERNEL)
  initMutexes();		/* see also DllMain() */
#endif

#ifdef PTW32_STATIC_LIB
  win_thread_initialize();
#endif

  LOCK();
  if ( threads_ready )
  { UNLOCK();
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
    GD->thread.threads = allocHeapOrHalt(GD->thread.thread_max *
				   sizeof(*GD->thread.threads));
    memset(GD->thread.threads, 0,
	   GD->thread.thread_max * sizeof(*GD->thread.threads));
    info = GD->thread.threads[1] = allocHeapOrHalt(sizeof(*info));
    memset(info, 0, sizeof(*info));
    info->pl_tid = 1;
    info->debug = TRUE;
    thread_highest_id = 1;
    info->thread_data = &PL_local_data;
    info->status = PL_THREAD_RUNNING;
    PL_local_data.thread.info = info;
    PL_local_data.thread.magic = PL_THREAD_MAGIC;
    set_system_thread_id(info);
    init_message_queue(&PL_local_data.thread.messages, -1);
    init_predicate_references(&PL_local_data);

    GD->statistics.thread_cputime = 0.0;
    GD->statistics.threads_created = 1;
    GD->thread.mutexTable = newHTable(16);
    GD->thread.mutexTable->free_symbol = unalloc_mutex_symbol;
    initMutexRef();
    link_mutexes();
    threads_ready = TRUE;
  }

  pthread_atfork(NULL, NULL, reinit_threads_after_fork);
  initMessageQueues();

  UNLOCK();
}


void
cleanupThreads(void)
{ int i;
  /*TLD_free(PL_ldata);*/		/* this causes crashes */

  if ( queueTable )
  { destroyHTable(queueTable);		/* removes shared queues */
    queueTable = NULL;
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
  freeHeap(GD->thread.threads,
	   GD->thread.thread_max * sizeof(*GD->thread.threads));
  GD->thread.threads = NULL;
  threads_ready = FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A first step towards clean destruction of the system.  Ideally, we would
like the following to happen:

    * Close-down all threads except for the main one
	+ Have all thread_at_exit/1 hooks called
    * Run the at_halt/1 hooks in the main thread
    * Exit from the main thread.

There are a lot of problems however.

    * Somehow Halt() should always be called from the main thread
      to have the process working properly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
exitPrologThreads(void)
{ int rc;
  int i;
  int me = PL_thread_self();
  int canceled = 0;

  DEBUG(MSG_THREAD, Sdprintf("exitPrologThreads(): me = %d\n", me));

  sem_init(sem_canceled_ptr, USYNC_THREAD, 0);

  for(i=1; i<= thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];

    if ( info && info->thread_data && i != me )
    { switch(info->status)
      { case PL_THREAD_FAILED:
	case PL_THREAD_EXITED:
	case PL_THREAD_EXCEPTION:
	{ void *r;
	  int rc;

	  if ( (rc=pthread_join(info->tid, &r)) )
	    Sdprintf("Failed to join thread %d: %s\n", i, ThError(rc));

	  break;
	}
	case PL_THREAD_RUNNING:
	{ info->thread_data->exit_requested = TRUE;

	  if ( info->cancel )
	  { if ( (*info->cancel)(i) == TRUE )
	      break;			/* done so */
	  }

	  if ( PL_thread_raise(i, SIG_PLABORT) )
	    canceled++;

	  break;
	}
	default:
	  break;
      }
    }
  }

  DEBUG(MSG_THREAD, Sdprintf("Waiting for %d threads ...", canceled));
  for(i=canceled; i-- > 0;)
  { int maxwait = 10;

    while(maxwait--)
    { if ( sem_trywait(sem_canceled_ptr) == 0 )
      { DEBUG(MSG_THREAD, Sdprintf(" (ok)"));
	canceled--;
	break;
      }
      Pause(0.1);
    }
  }

  if ( canceled )
  { GET_LD
    fid_t fid;

    if ( (fid = PL_open_foreign_frame()) )
    { term_t head    = PL_new_term_ref();
      term_t running = PL_new_term_ref();
      term_t tail    = PL_copy_term_ref(running);

      rc = TRUE;
      for(i = 1; i <= thread_highest_id; i++)
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

bool
aliasThread(int tid, atom_t name)
{ GET_LD
  LOCK();
  if ( !threadTable )
    threadTable = newHTable(16);

  if ( (threadTable && lookupHTable(threadTable, (void *)name)) ||
       (queueTable  && lookupHTable(queueTable,  (void *)name)) )
  { term_t obj = PL_new_term_ref();

    UNLOCK();
    PL_put_atom(obj, name);
    return PL_error("thread_create", 1, "Alias name already taken",
		    ERR_PERMISSION, ATOM_create, ATOM_thread, obj);
  }

  addNewHTable(threadTable, (void *)name, (void *)(intptr_t)tid);
  PL_register_atom(name);
  GD->thread.threads[tid]->name = name;

  UNLOCK();

  succeed;
}

static void
unaliasThread(atom_t name)
{ GET_LD
  if ( threadTable )
  { LOCK();
    if ( lookupHTable(threadTable, (void *)name) )
    { deleteHTable(threadTable, (void *)name);
      PL_unregister_atom(name);
    }
    UNLOCK();
  }
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
  atom_t name;

  if ( tid == 0 )
    tid = PL_thread_self();
  if ( tid < 1 || tid > thread_highest_id )
    return FALSE;

  info = GD->thread.threads[tid];
  if ( (name=info->name) )
  { *alias = name;

    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resizing max-threads. Note that we do *not* deallocate the old structure
to ensure we can  access  GD->thread.threads[i]   at  any  time  without
locking.

TBD: remember the old ones, such that PL_cleanup() can remove them.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
resizeThreadMax(void)
{ int newmax = GD->thread.thread_max*2;
  PL_thread_info_t **newinfo, **oldinfo;
  size_t dsize = GD->thread.thread_max * sizeof(*GD->thread.threads);

  oldinfo = GD->thread.threads;
  newinfo = allocHeapOrHalt(newmax * sizeof(*GD->thread.threads));
  memset(addPointer(newinfo,dsize), 0, dsize);
  memcpy(newinfo, oldinfo, dsize);
  GD->thread.threads = newinfo;
  GD->thread.thread_max = newmax;
  GC_LINGER(oldinfo);

  return TRUE;
}


/* MT: Caller must lock */

static PL_thread_info_t *
alloc_thread(void)				/* called with L_THREAD locked */
{ int i	= 1;

retry:
  for(; i<GD->thread.thread_max; i++)
  { PL_thread_info_t *info;

    if ( !(info=GD->thread.threads[i]) )
    { info = allocHeapOrHalt(sizeof(*info));
      memset(info, 0, sizeof(*info));
      GD->thread.threads[i] = info;
    }

    if ( info->status == PL_THREAD_UNUSED )
    { PL_local_data_t *ld = allocHeapOrHalt(sizeof(PL_local_data_t));

      memset(ld, 0, sizeof(PL_local_data_t));

      info->pl_tid = i;
      ld->thread.info = info;
      ld->thread.magic = PL_THREAD_MAGIC;
      info->thread_data = ld;
      info->status = PL_THREAD_RESERVED;
      info->debug = TRUE;

      if ( i > thread_highest_id )
	thread_highest_id = i;

      GD->statistics.threads_created++;

      return info;
    }
  }

  if ( resizeThreadMax() )
    goto retry;

  return NULL;				/* out of threads */
}


int
PL_thread_self(void)
{ GET_LD
  PL_local_data_t *ld = LD;

  if ( ld )
    return ld->thread.info->pl_tid;

  return -1;				/* thread has no Prolog thread */
}


int
PL_unify_thread_id(term_t t, int i)
{ if ( i < 1 ||
       i > thread_highest_id ||
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

  if ( sig < 0 || sig > MAXSIGNAL )
    return FALSE;			/* illegal signal */

  LOCK();
  for(i = 1; i <= thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];

    if ( info && info->w32id == id && info->thread_data )
    { raiseSignal(info->thread_data, sig);
      if ( info->w32id )
	PostThreadMessage(info->w32id, WM_SIGNALLED, 0, 0L);
      UNLOCK();
      DEBUG(MSG_THREAD, Sdprintf("Signalled %d to thread %d\n", sig, i));
      return TRUE;
    }
  }
  UNLOCK();

  return FALSE;				/* can't find thread */
}

#endif /*__WINDOWS__*/

static int
alertThread(PL_thread_info_t *info)
{
#ifdef __WINDOWS__
  if ( info->w32id )
  { PostThreadMessage(info->w32id, WM_SIGNALLED, 0, 0L);
    return TRUE;			/* NOTE: PostThreadMessage() can */
					/* fail if thread is being created */
  }
#else
  if ( info->has_tid )
    return pthread_kill(info->tid, SIG_ALERT) == 0;
#endif
  return FALSE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_thread_raise() is used  for  re-routing   interrupts  in  the Windows
version, where the signal handler is running  from a different thread as
Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_thread_raise(int tid, int sig)
{ PL_thread_info_t *info;

  LOCK();
  if ( tid < 1 || tid > thread_highest_id )
  { error:
    UNLOCK();
    return FALSE;
  }
  info = GD->thread.threads[tid];
  if ( info->status == PL_THREAD_UNUSED ||
       info->status == PL_THREAD_RESERVED )
    goto error;

  if ( !raiseSignal(info->thread_data, sig) ||
       !alertThread(info) )
    goto error;

  UNLOCK();

  return TRUE;
}


const char *
threadName(int id)
{ PL_thread_info_t *info;
  char tmp[16];

  if ( id == 0 )
    id = PL_thread_self();
  if ( id < 0 )
    return "[Not a prolog thread]";

  info = GD->thread.threads[id];

  if ( info->name )
    return PL_atom_chars(info->name);

  sprintf(tmp, "%d", id);
  return buffer_string(tmp, BUF_RING);
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
#ifdef __linux__
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
#ifdef HAVE_GETTID_SYSCALL
  info->pid = syscall(__NR_gettid);
#else
#ifdef HAVE_GETTID_MACRO
  info->pid = gettid();
#else
#ifdef __WINDOWS__
  info->w32id = GetCurrentThreadId();
#endif
#endif
#endif
}


static const opt_spec make_thread_options[] =
{ { ATOM_local,		OPT_SIZE|OPT_INF },
  { ATOM_global,	OPT_SIZE|OPT_INF },
  { ATOM_trail,	        OPT_SIZE|OPT_INF },
  { ATOM_alias,		OPT_ATOM },
  { ATOM_debug,		OPT_BOOL },
  { ATOM_detached,	OPT_BOOL },
  { ATOM_stack,		OPT_SIZE },
  { ATOM_c_stack,	OPT_SIZE },
  { ATOM_at_exit,	OPT_TERM },
  { ATOM_inherit_from,	OPT_TERM },
  { NULL_ATOM,		0 }
};


static int
mk_kbytes(size_t *sz, atom_t name ARG_LD)
{ if ( *sz != (size_t)-1 )
  { size_t s = *sz * 1024;

    if ( s/1024 != *sz )
    { term_t t = PL_new_term_ref();

      return ( PL_put_int64(t, *sz) &&	/* TBD: size_t is unsigned! */
	       PL_error(NULL, 0, NULL, ERR_DOMAIN, name, t) );
    }

    *sz = s;
  }

  return TRUE;
}


static void *
start_thread(void *closure)
{ PL_thread_info_t *info = closure;
  term_t ex, goal;
  int rval;

  assert(info->goal);
  blockSignal(SIGINT);			/* only the main thread processes */
					/* Control-C */
  set_system_thread_id(info);		/* early to get exit code ok */

  if ( !initialise_thread(info) )
    return (void *)FALSE;

  { GET_LD

    pthread_cleanup_push(free_prolog_thread, info->thread_data);

    LOCK();
    info->status = PL_THREAD_RUNNING;
    UNLOCK();

    goal = PL_new_term_ref();
    PL_put_atom(goal, ATOM_dthread_init);

    rval = callProlog(MODULE_system, goal, PL_Q_CATCH_EXCEPTION, &ex);

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
	{ atom_t a;

	  if ( PL_get_atom(ex, &a) && a == ATOM_aborted )
	    print = FALSE;
	}

	if ( print )
	  printMessage(ATOM_warning,
		       PL_FUNCTOR_CHARS, "abnormal_thread_completion", 2,
			 PL_TERM, goal,
			 PL_FUNCTOR, FUNCTOR_exception1,
			   PL_TERM, ex);
      } else
      { printMessage(ATOM_warning,
		     PL_FUNCTOR_CHARS, "abnormal_thread_completion", 2,
		       PL_TERM, goal,
		       PL_ATOM, ATOM_fail);
      }
    }

    LOCK();
    if ( rval )
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
    UNLOCK();

    pthread_cleanup_pop(1);
  }

  return (void *)TRUE;
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


word
pl_thread_create(term_t goal, term_t id, term_t options)
{ GET_LD
  PL_thread_info_t *info;
  PL_local_data_t *ldnew, *ldold = LD;
  atom_t alias = NULL_ATOM, idname;
  pthread_attr_t attr;
  size_t stack = 0;
  term_t inherit_from = 0;
  term_t at_exit = 0;
  int rc = 0;
  const char *func;

  if ( !PL_is_callable(goal) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, goal);

  LOCK();
  if ( !GD->thread.enabled || GD->cleaning != CLN_NORMAL )
  { UNLOCK();
    return PL_error(NULL, 0, "threading disabled",
		      ERR_PERMISSION,
		      ATOM_create, ATOM_thread, goal);
  }

  info = alloc_thread();

  UNLOCK();
  if ( !info )
    return PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_threads);

  ldnew = info->thread_data;

  if ( !scan_options(options, 0, /*OPT_ALL,*/
		     ATOM_thread_option, make_thread_options,
		     &info->local_size,
		     &info->global_size,
		     &info->trail_size,
		     &alias,
		     &info->debug,
		     &info->detached,
		     &stack,		/* stack */
		     &stack,		/* c_stack */
		     &at_exit,
		     &inherit_from) )
  { free_thread_info(info);
    fail;
  }
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
  if ( !PL_is_variable(id) &&
       !(PL_get_atom(id, &idname) && idname == alias) )
  { free_thread_info(info);
    return PL_error("thread_create", 3, NULL, ERR_UNINSTANTIATION, 2, id);
  }

  if ( !mk_kbytes(&info->local_size,  ATOM_local   PASS_LD) ||
       !mk_kbytes(&info->global_size, ATOM_global  PASS_LD) ||
       !mk_kbytes(&info->trail_size,  ATOM_trail   PASS_LD) ||
       !mk_kbytes(&stack,             ATOM_c_stack PASS_LD) )
  { free_thread_info(info);
    return FALSE;
  }

  if ( alias )
  { if ( !aliasThread(info->pl_tid, alias) )
    { free_thread_info(info);
      fail;
    }
  }
  if ( !unify_thread_id(id, info) )
  { free_thread_info(info);

    if ( !PL_is_variable(id) )
      return PL_error(NULL, 0, "thread-id", ERR_UNINSTANTIATION, 0, id);

    fail;
  }

  info->goal = PL_record(goal);
  info->module = PL_context();

					/* copy settings */

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

  ldnew->tabling.node_pool.size   = 0;
  ldnew->statistics.start_time    = WallTime();
  ldnew->prolog_flag.mask	  = ldold->prolog_flag.mask;
  ldnew->prolog_flag.occurs_check = ldold->prolog_flag.occurs_check;
  ldnew->prolog_flag.access_level = ldold->prolog_flag.access_level;
  if ( ldold->prolog_flag.table )
  { PL_LOCK(L_PLFLAG);
    ldnew->prolog_flag.table	  = copyHTable(ldold->prolog_flag.table);
    PL_UNLOCK(L_PLFLAG);
  }
  if ( !info->debug )
  { ldnew->_debugstatus.tracing   = FALSE;
    ldnew->_debugstatus.debugging = DBG_OFF;
    set(&ldnew->prolog_flag.mask, PLFLAG_LASTCALL);
  }
  init_message_queue(&ldnew->thread.messages, -1);
  init_predicate_references(ldnew);
  if ( at_exit )
    thread_at_exit(at_exit, ldnew);

  pthread_attr_init(&attr);
  if ( info->detached )
  { func = "pthread_attr_setdetachstate";
    rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  }
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
    if ( stack )
    { stack = round_pages(stack);
      func = "pthread_attr_setstacksize";
      rc = pthread_attr_setstacksize(&attr, stack);
      info->stack_size = stack;
    } else
    { pthread_attr_getstacksize(&attr, &info->stack_size);
    }
  }
  if ( rc == 0 )
  { LOCK();
    info->status = PL_THREAD_CREATED;
    assert(info->goal);
    func = "pthread_create";
    rc = pthread_create(&info->tid, &attr, start_thread, info);
    UNLOCK();
  }
  pthread_attr_destroy(&attr);

  if ( rc != 0 )
  { free_thread_info(info);
    return PL_error(NULL, 0, ThError(rc),
		    ERR_SYSCALL, func);
  }

  succeed;
}


static int
get_thread(term_t t, PL_thread_info_t **info, int warn)
{ GET_LD
  int i = -1;

  if ( !PL_get_integer(t, &i) )
  { atom_t name;

    if ( !PL_get_atom(t, &name) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_thread, t);
    if ( threadTable )
    { int tid;

      if ( (tid = (int)(intptr_t)lookupHTable(threadTable, (void *)name)) )
	i = tid;
    }
  }

  if ( i < 1 ||
       i > thread_highest_id ||
       GD->thread.threads[i]->status == PL_THREAD_UNUSED ||
       GD->thread.threads[i]->status == PL_THREAD_RESERVED )
  { if ( warn )
      return PL_error(NULL, 0, "no info record",
		      ERR_EXISTENCE, ATOM_thread, t);
    else
      return FALSE;
  }

  *info = GD->thread.threads[i];

  return TRUE;
}


static int
get_thread_sync(term_t t, PL_thread_info_t **info, int warn)
{ int rc;

  LOCK();
  rc = get_thread(t, info, warn);
  UNLOCK();

  return rc;
}


int
unify_thread_id(term_t id, PL_thread_info_t *info)
{ GET_LD
  int rc;

  if ( info->name )
    rc = PL_unify_atom(id, info->name);
  else
    rc = PL_unify_integer(id, info->pl_tid);

  return rc;
}


/* If lock = TRUE, this is used from thread_property/2 and we must
   be careful that the thread may vanish during the process if it
   is a detached thread.  Note that we only avoid crashes.  The fact
   that the value may not be true at the moment it is requested is
   simply a limitation of status pulling.
*/

static int
unify_thread_status(term_t status, PL_thread_info_t *info, int lock)
{ GET_LD

  switch(info->status)
  { case PL_THREAD_CREATED:
    case PL_THREAD_RUNNING:
      return PL_unify_atom(status, ATOM_running);
    case PL_THREAD_EXITED:
    { term_t tmp = PL_new_term_ref();
      int rc = TRUE;

      if ( lock ) LOCK();
      if ( info->return_value )
	rc = PL_recorded(info->return_value, tmp);
      if ( lock ) UNLOCK();

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

      if ( lock ) LOCK();
      if ( info->return_value )
	rc = PL_recorded(info->return_value, tmp);
      if ( lock ) UNLOCK();
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
free_thread_info(PL_thread_info_t *info)
{ record_t rec_rv, rec_g;

  if ( info->thread_data )
    free_prolog_thread(info->thread_data);
  if ( info->name )
    unaliasThread(info->name);

  LOCK();
  if ( (rec_rv=info->return_value) )	/* sync with unify_thread_status() */
    info->return_value = NULL;
  if ( (rec_g=info->goal) )
    info->goal = NULL;

  if ( info->pl_tid == thread_highest_id )
  { int i;

    for(i=info->pl_tid-1; i>1; i--)
    { PL_thread_info_t *ih = GD->thread.threads[i];
      if ( ih->status != PL_THREAD_UNUSED )
	break;
    }

    thread_highest_id = i;
  }

  memset(info, 0, sizeof(*info));	/* sets status to PL_THREAD_UNUSED */
  UNLOCK();

  if ( rec_rv ) PL_erase(rec_rv);
  if ( rec_g )  PL_erase(rec_g);
}


static
PRED_IMPL("thread_join", 2, thread_join, 0)
{ PRED_LD
  PL_thread_info_t *info;
  void *r;
  word rval;
  int rc;

  term_t thread = A1;
  term_t retcode = A2;

  if ( !get_thread_sync(thread, &info, TRUE) )
    fail;

  if ( info == LD->thread.info || info->detached )
  { return PL_error("thread_join", 2,
		    info->detached ? "Cannot join detached thread"
				   : "Cannot join self",
		    ERR_PERMISSION, ATOM_join, ATOM_thread, thread);
  }

  while( (rc=pthread_join(info->tid, &r)) == EINTR )
  { if ( PL_handle_signals() < 0 )
      fail;
  }
  switch(rc)
  { case 0:
      break;
    case ESRCH:
      Sdprintf("ESRCH from %d\n", info->tid);
      return PL_error("thread_join", 2, NULL,
		      ERR_EXISTENCE, ATOM_thread, thread);
    default:
      return PL_error("thread_join", 2, ThError(rc),
		      ERR_SYSCALL, "pthread_join");
  }

  rval = unify_thread_status(retcode, info, FALSE);

  free_thread_info(info);

  return rval;
}


word
pl_thread_exit(term_t retcode)
{ GET_LD
  PL_thread_info_t *info = LD->thread.info;

  LOCK();
  info->status = PL_THREAD_EXITED;
  info->return_value = PL_record(retcode);
  UNLOCK();

  DEBUG(MSG_THREAD, Sdprintf("thread_exit(%d)\n", info->pl_tid));

  pthread_exit(NULL);
  assert(0);
  fail;
}


static
PRED_IMPL("thread_detach", 1, thread_detach, 0)
{ PL_thread_info_t *info;
  PL_thread_info_t *release = NULL;

  LOCK();
  if ( !get_thread(A1, &info, TRUE) )
  { UNLOCK();
    fail;
  }

  if ( !info->detached )
  { int rc;

    if ( (rc=pthread_detach(info->tid)) )
    { assert(rc == ESRCH);

      release = info;
    } else
    { info->detached = TRUE;
    }
  }

  UNLOCK();

  if ( release )
    free_thread_info(release);

  succeed;
}


		 /*******************************
		 *	  THREAD PROPERTY	*
		 *******************************/

static int
thread_alias_propery(PL_thread_info_t *info, term_t prop ARG_LD)
{ atom_t a;

  if ( (a=info->name) )
    return PL_unify_atom(prop, a);

  fail;
}

static int
thread_status_propery(PL_thread_info_t *info, term_t prop ARG_LD)
{ IGNORE_LD

  return unify_thread_status(prop, info, TRUE);
}

static int
thread_detached_propery(PL_thread_info_t *info, term_t prop ARG_LD)
{ IGNORE_LD

  return PL_unify_bool_ex(prop, info->detached);
}

static int
thread_debug_propery(PL_thread_info_t *info, term_t prop ARG_LD)
{ IGNORE_LD

  return PL_unify_bool_ex(prop, info->debug);
}

static int
thread_tid_propery(PL_thread_info_t *info, term_t prop ARG_LD)
{ IGNORE_LD
  intptr_t tid = system_thread_id(info);

  if ( tid != -1 )
    return PL_unify_integer(prop, system_thread_id(info));
  else
    return FALSE;
}

typedef struct
{ functor_t functor;			/* functor of property */
  int (*function)();			/* function to generate */
} tprop;


static const tprop tprop_list [] =
{ { FUNCTOR_alias1,	       thread_alias_propery },
  { FUNCTOR_status1,	       thread_status_propery },
  { FUNCTOR_detached1,	       thread_detached_propery },
  { FUNCTOR_debug1,	       thread_debug_propery },
  { FUNCTOR_system_thread_id1, thread_tid_propery },
  { 0,			       NULL }
};


typedef struct
{ int		tid;
  const tprop  *p;
  int		enum_threads;
  int		enum_properties;
} tprop_enum;


static int
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
      if ( state->tid > thread_highest_id )
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
      } else if ( get_thread_sync(thread, &info, TRUE) )
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

      if ( info && (*state->p->function)(info, arg PASS_LD) )
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


		 /*******************************
		 *	     CLEANUP		*
		 *******************************/

typedef enum { EXIT_PROLOG, EXIT_C } exit_type;

typedef struct _at_exit_goal
{ struct _at_exit_goal *next;		/* Next in queue */
  exit_type type;			/* Prolog or C */
  union
  { struct
    { Module   module;			/* Module for running goal */
      record_t goal;			/* Goal to run */
    } prolog;
    struct
    { void (*function)(void *);		/* called function */
      void *closure;			/* client data */
    } c;
  } goal;
} at_exit_goal;


static int
thread_at_exit(term_t goal, PL_local_data_t *ld)
{ GET_LD
  Module m = NULL;
  at_exit_goal *eg;

  if ( !PL_strip_module(goal, &m, goal) )
    return FALSE;

  eg = allocHeapOrHalt(sizeof(*eg));
  eg->next = NULL;
  eg->type = EXIT_PROLOG;
  eg->goal.prolog.module = m;
  eg->goal.prolog.goal   = PL_record(goal);

  eg->next = ld->thread.exit_goals;
  ld->thread.exit_goals = eg;

  succeed;
}


foreign_t
pl_thread_at_exit(term_t goal)
{ GET_LD

  return thread_at_exit(goal, LD);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Request a function to run when the Prolog thread is about to detach, but
still capable of running Prolog queries.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_thread_at_exit(void (*function)(void *), void *closure, int global)
{ GET_LD

  at_exit_goal *eg = allocHeapOrHalt(sizeof(*eg));

  eg->next = NULL;
  eg->type = EXIT_C;
  eg->goal.c.function = function;
  eg->goal.c.closure  = closure;

  if ( global )
  { LOCK();
    eg->next = GD->thread.exit_goals;
    GD->thread.exit_goals = eg;
    UNLOCK();
  } else
  { eg->next = LD->thread.exit_goals;
    LD->thread.exit_goals = eg;
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Newly pushed hooks are executed  after   all  currently registered hooks
have finished.

Q: What to do with exceptions?
Q: Should we limit the passes?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
run_exit_hooks(at_exit_goal *eg, int free)
{ GET_LD
  at_exit_goal *next;
  term_t goal;
  fid_t fid;

  if ( !(goal = PL_new_term_ref()) ||
       !(fid = PL_open_foreign_frame()) )
    return FALSE;

  for( ; eg; eg = next)
  { next = eg->next;

    switch(eg->type)
    { case EXIT_PROLOG:
      { int rc = PL_recorded(eg->goal.prolog.goal, goal);
        if ( free )
	  PL_erase(eg->goal.prolog.goal);
	if ( rc )
	{ DEBUG(MSG_THREAD,
		{ Sdprintf("Calling exit goal: ");
		  PL_write_term(Serror, goal, 1200, PL_WRT_QUOTED);
		  Sdprintf("\n");
		});

	  callProlog(eg->goal.prolog.module, goal, PL_Q_NODEBUG, NULL);
	}
	PL_rewind_foreign_frame(fid);
	break;
      }
      case EXIT_C:
	(*eg->goal.c.function)(eg->goal.c.closure);
        break;
      default:
	assert(0);
    }

    if ( free )
      freeHeap(eg, sizeof(*eg));
  }

  PL_discard_foreign_frame(fid);
  PL_reset_term_refs(goal);

  return TRUE;
}



static void
run_thread_exit_hooks(PL_local_data_t *ld)
{ GET_LD

  if ( LD == ld )	/* if FALSE, we are called from another thread (create) */
  { at_exit_goal *eg;
    fid_t fid = PL_open_foreign_frame();

    while( (eg = ld->thread.exit_goals) )
    { ld->thread.exit_goals = NULL;	/* empty these */

      run_exit_hooks(eg, TRUE);
    }

    run_exit_hooks(GD->thread.exit_goals, FALSE);
    PL_close_foreign_frame(fid);
  }
}


		 /*******************************
		 *	   THREAD SIGNALS	*
		 *******************************/

typedef struct _thread_sig
{ struct _thread_sig *next;		/* Next in queue */
  Module   module;			/* Module for running goal */
  record_t goal;			/* Goal to run */
} thread_sig;


static int
is_alive(int status)
{ switch(status)
  { case PL_THREAD_CREATED:
    case PL_THREAD_RUNNING:
    case PL_THREAD_SUSPENDED:
    case PL_THREAD_RESUMING:
      succeed;
    default:
      fail;
  }
}


foreign_t
pl_thread_signal(term_t thread, term_t goal)
{ GET_LD
  Module m = NULL;
  thread_sig *sg;
  PL_thread_info_t *info;
  PL_local_data_t *ld;

  if ( !PL_strip_module(goal, &m, goal) )
    return FALSE;

  LOCK();
  if ( !get_thread(thread, &info, TRUE) )
  { UNLOCK();
    fail;
  }
  if ( !is_alive(info->status) )
  { error:
    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_thread, thread);
    UNLOCK();
    fail;
  }

  sg = allocHeapOrHalt(sizeof(*sg));
  sg->next = NULL;
  sg->module = m;
  sg->goal = PL_record(goal);

  ld = info->thread_data;
  if ( !ld->thread.sig_head )
    ld->thread.sig_head = ld->thread.sig_tail = sg;
  else
  { ld->thread.sig_tail->next = sg;
    ld->thread.sig_tail = sg;
  }
  raiseSignal(ld, SIG_THREAD_SIGNAL);
  if ( info->has_tid && !alertThread(info) )
    goto error;

  UNLOCK();

  succeed;
}


void
executeThreadSignals(int sig)
{ GET_LD
  thread_sig *sg, *next;
  fid_t fid;
  (void)sig;

  if ( !is_alive(LD->thread.info->status) )
    return;

  LOCK();
  sg = LD->thread.sig_head;
  LD->thread.sig_head = LD->thread.sig_tail = NULL;
  UNLOCK();

  fid = PL_open_foreign_frame();

  for( ; sg; sg = next)
  { term_t goal = PL_new_term_ref();
    Module gm;
    term_t ex;
    int rval;

    next = sg->next;
    rval = PL_recorded(sg->goal, goal);
    PL_erase(sg->goal);
    gm = sg->module;
    freeHeap(sg, sizeof(*sg));

    DEBUG(MSG_THREAD,
	  Sdprintf("[%d] Executing thread signal\n", PL_thread_self()));
    if ( rval )
    { rval = callProlog(gm, goal, PL_Q_CATCH_EXCEPTION, &ex);
    } else
    { rval = raiseStackOverflow(GLOBAL_OVERFLOW);
      ex = exception_term;
    }

    if ( !rval && ex )
    { PL_raise_exception(ex);
      PL_close_foreign_frame(fid);

      DEBUG(MSG_THREAD,
	    { print_trace(8);
	      Sdprintf("[%d]: Prolog backtrace:\n", PL_thread_self());
	      PL_backtrace(5, 0);
	      Sdprintf("[%d]: end Prolog backtrace:\n", PL_thread_self());
	    });

      for(sg = next; sg; sg=next)
      { next = sg->next;
	PL_erase(sg->goal);
	freeHeap(sg, sizeof(*sg));
      }

      return;
    }

    PL_rewind_foreign_frame(fid);
  }

  PL_discard_foreign_frame(fid);
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
		 *	  MESSAGE QUEUES	*
		 *******************************/

typedef enum
{ QUEUE_WAIT_READ,			/* wait for message */
  QUEUE_WAIT_DRAIN			/* wait for queue to drain */
} queue_wait_type;

#define MSG_WAIT_INTR		(-1)
#define MSG_WAIT_TIMEOUT	(-2)
#define MSG_WAIT_DESTROYED	(-3)

static int dispatch_cond_wait(message_queue *queue,
			      queue_wait_type wait,
			      struct timespec *deadline);

#ifdef __WINDOWS__
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Earlier implementations used pthread-win32 condition   variables.  As we
need to dispatch messages while waiting for a condition variable we need
to use pthread_cond_timedwait() which is   really  complicated and SLOW.
Below is an  alternative  emulation   of  pthread_cond_wait()  that does
dispatch messages. It is not fair  nor   correct,  but  neither of these
problems bothers us considering the promises we make about Win32 message
queues. This implentation is about 250 times faster, providing about the
same performance as on native  pthread   implementations  such as Linux.
This work was sponsored by SSS, http://www.sss.co.nz

This implementation is based on   the following, summarizing discussions
on comp.lang.thread.

Strategies for Implementing POSIX Condition Variables on Win32
Douglas C. Schmidt and Irfan Pyarali
Department of Computer Science
Washington University, St. Louis, Missouri
http://www.cs.wustl.edu/~schmidt/win32-cv-1.html

It uses the second alternative, avoiding   the extra critical section as
we assume the condition  variable  is   always  associated  to  the same
critical section (associated to the same SWI-Prolog message queue).

The resulting implementation suffers from the following problems:

  * Unfairness
    If two threads are waiting and two messages arrive on the queue it
    is possible for one thread to consume both of them. We never
    anticipated on `fair' behaviour in this sense in SWI-Prolog, so
    we should not be bothered.  Nevertheless existing application may
    have assumed fairness.

  * Incorrectness
    If two threads are waiting, a broadcast happens and a third thread
    kicks in it is possible one of the threads does not get a wakeup.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static int
win32_cond_init(win32_cond_t *cv)
{ cv->events[SIGNAL]    = CreateEvent(NULL, FALSE, FALSE, NULL);
  cv->events[BROADCAST] = CreateEvent(NULL, TRUE,  FALSE, NULL);
  cv->waiters = 0;

  return 0;
}


static int
win32_cond_destroy(win32_cond_t *cv)
{ CloseHandle(cv->events[SIGNAL]);
  CloseHandle(cv->events[BROADCAST]);

  return 0;
}


static int
win32_cond_wait(win32_cond_t *cv,
		CRITICAL_SECTION *external_mutex,
	        struct timespec *deadline)
{ int rc, last;
  DWORD dwMilliseconds;

  if ( deadline )
  { struct timespec now;

    get_current_timespec(&now);
    dwMilliseconds = 1000*(DWORD)(deadline->tv_sec - now.tv_sec)
                     + (deadline->tv_nsec - now.tv_nsec)/1000000;

     if ( dwMilliseconds <= 0 )
      return ETIMEDOUT;
  } else
  { dwMilliseconds = INFINITE;
  }

  cv->waiters++;

  LeaveCriticalSection(external_mutex);
  rc = MsgWaitForMultipleObjects(2,
				 cv->events,
				 FALSE,	/* wait for either event */
				 dwMilliseconds,
				 QS_ALLINPUT);
  DEBUG(MSG_THREAD, Sdprintf("dwMilliseconds=%ld, rc=%d\n", dwMilliseconds, rc));
  if ( rc == WAIT_OBJECT_0+2 )
  { GET_LD
    MSG msg;

    while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
    { TranslateMessage(&msg);
      DispatchMessage(&msg);
    }

    if ( is_signalled(LD) )
    { EnterCriticalSection(external_mutex);
      return EINTR;
    }
  } else if ( rc == WAIT_TIMEOUT )
  { EnterCriticalSection(external_mutex);
    return ETIMEDOUT;
  }

  EnterCriticalSection(external_mutex);

  cv->waiters--;
  last = (rc == WAIT_OBJECT_0 + BROADCAST && cv->waiters == 0);
  if ( last )
    ResetEvent (cv->events[BROADCAST]);

  return 0;
}


static int
win32_cond_signal(win32_cond_t *cv)	/* must be holding associated mutex */
{ if ( cv->waiters > 0 )
    SetEvent(cv->events[SIGNAL]);

  return 0;
}


static int
win32_cond_broadcast(win32_cond_t *cv)	/* must be holding associated mutex */
{ if ( cv->waiters > 0 )
    SetEvent(cv->events[BROADCAST]);

  return 0;
}

#define cv_broadcast	win32_cond_broadcast
#define cv_signal	win32_cond_signal
#define cv_init(cv,p)	win32_cond_init(cv)
#define cv_destroy	win32_cond_destroy
#else /*__WINDOWS__*/
#define cv_broadcast	pthread_cond_broadcast
#define cv_signal	pthread_cond_signal
#define cv_init(cv,p)	pthread_cond_init(cv, p)
#define cv_destroy	pthread_cond_destroy
#endif /*__WINDOWS__*/

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


static thread_message *
create_thread_message(term_t msg ARG_LD)
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

static int
queue_message(message_queue *queue, thread_message *msgp, struct timespec *deadline ARG_LD)
{ if ( queue->max_size > 0 && queue->size >= queue->max_size )
  { queue->wait_for_drain++;

    while ( queue->size >= queue->max_size )
    { switch ( dispatch_cond_wait(queue, QUEUE_WAIT_DRAIN, deadline) )
      { case EINTR:
      { if ( !LD )			/* needed for clean exit */
	{ Sdprintf("Forced exit from queue_message()\n");
	  exit(1);
	}

	if ( is_signalled(LD) )			/* thread-signal */
	{ queue->wait_for_drain--;
	  return MSG_WAIT_INTR;
	}
	break;
      }
      case ETIMEDOUT:
	queue->wait_for_drain--;
        return MSG_WAIT_TIMEOUT;
      case 0:
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
    { DEBUG(MSG_THREAD,
	    Sdprintf("%d of %d non-var waiters; broadcasting\n",
		     queue->waiting - queue->waiting_var,
		     queue->waiting));
      cv_broadcast(&queue->cond_var);
    } else
    { DEBUG(MSG_THREAD, Sdprintf("%d var waiters; signalling\n", queue->waiting));
      cv_signal(&queue->cond_var);
    }
  } else
  { DEBUG(MSG_THREAD, Sdprintf("No waiters\n"));
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

static int
timespec_cmp(struct timespec *a, struct timespec *b)
{ struct timespec diff;

  diff.tv_sec  = a->tv_sec - b->tv_sec;
  diff.tv_nsec = a->tv_nsec - b->tv_nsec;
  if ( diff.tv_nsec < 0 )
  { --diff.tv_sec;
    diff.tv_nsec += 1000000000;
  }

  return ( diff.tv_sec > 0 ?  1 :
	   diff.tv_sec < 0 ? -1 :
	   diff.tv_nsec > 0 ? 1 :
                              0 );
}


static void
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


static void
carry_timespec_nanos(struct timespec *time)
{ while ( time->tv_nsec >= 1000000000 )
  { time->tv_nsec -= 1000000000;
    time->tv_sec += 1;
  }
}

#ifdef __WINDOWS__

static int
dispatch_cond_wait(message_queue *queue, queue_wait_type wait, struct timespec *deadline)
{ return win32_cond_wait((wait == QUEUE_WAIT_READ ? &queue->cond_var
						  : &queue->drain_var),
			 &queue->mutex,
			 deadline);
}

#else /*__WINDOWS__*/

/* return: 0: ok, EINTR: interrupted, ETIMEDOUT: timeout
*/

static int
dispatch_cond_wait(message_queue *queue, queue_wait_type wait, struct timespec *deadline)
{ GET_LD
  int rc;

  for(;;)
  { struct timespec tmp_timeout;
    struct timespec *api_timeout = &tmp_timeout;

    get_current_timespec(&tmp_timeout);
    tmp_timeout.tv_nsec += 250000000;
    carry_timespec_nanos(&tmp_timeout);

    if ( deadline && timespec_cmp(&tmp_timeout, deadline) >= 0 )
      api_timeout = deadline;

    rc = pthread_cond_timedwait((wait == QUEUE_WAIT_READ ? &queue->cond_var
							 : &queue->drain_var),
				&queue->mutex, api_timeout);

#ifdef O_DEBUG
    if ( LD && LD->thread.info )	/* can be absent during shutdown */
    { switch( LD->thread.info->ldata_status )
      { case LDATA_IDLE:
	case LDATA_ANSWERED:
	case LDATA_SIGNALLED:
	  break;
	default:
	  Sdprintf("%d: ldata_status = %d\n",
		   PL_thread_self(), LD->thread.info->ldata_status);
      }
    } else
    { return EINTR;
    }
#endif

    switch( rc )
    { case ETIMEDOUT:
	if ( is_signalled(LD) )
	  return EINTR;
        if ( api_timeout == deadline )
	  return ETIMEDOUT;

	return 0;
      default:
	return rc;
    }
  }
}

#endif /*__WINDOWS__*/

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

static int
get_message(message_queue *queue, term_t msg, struct timespec *deadline ARG_LD)
{ int isvar = PL_is_variable(msg) ? 1 : 0;
  word key = (isvar ? 0L : getIndexOfTerm(msg));
  fid_t fid = PL_open_foreign_frame();
  uint64_t seen = 0;

  QSTAT(getmsg);

  for(;;)
  { thread_message *msgp = queue->head;
    thread_message *prev = NULL;

    if ( queue->destroyed )
      return MSG_WAIT_DESTROYED;

    DEBUG(MSG_QUEUE,
	  if ( queue->size > 0 )
	    Sdprintf("%d: scanning queue (size=%ld)\n",
		     PL_thread_self(), queue->size));

    for( ; msgp; prev = msgp, msgp = msgp->next )
    { int rc;
      term_t tmp;

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
      rc = PL_unify(msg, tmp);
      DEBUG(MSG_QUEUE, { pl_writeln(tmp);
			 pl_writeln(msg);
		       });

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
    switch ( dispatch_cond_wait(queue, QUEUE_WAIT_READ, deadline) )
    { case EINTR:
      { DEBUG(MSG_QUEUE_WAIT, Sdprintf("%d: EINTR\n", PL_thread_self()));

	if ( !LD )			/* needed for clean exit */
	{ Sdprintf("Forced exit from get_message()\n");
	  exit(1);
	}

	if ( is_signalled(LD) )		/* thread-signal */
	{ queue->waiting--;
	  queue->waiting_var -= isvar;
	  PL_discard_foreign_frame(fid);
	  return MSG_WAIT_INTR;
	}
	break;
      }
      case ETIMEDOUT:
      { DEBUG(MSG_QUEUE_WAIT, Sdprintf("%d: ETIMEDOUT\n", PL_thread_self()));

	queue->waiting--;
	queue->waiting_var -= isvar;
	PL_discard_foreign_frame(fid);
	return MSG_WAIT_TIMEOUT;
      }
      case 0:
	DEBUG(MSG_QUEUE_WAIT,
	      Sdprintf("%d: wakeup on queue\n", PL_thread_self()));
	break;
      default:
	assert(0);
    }
    queue->waiting--;
    queue->waiting_var -= isvar;
  }
}


static int
peek_message(message_queue *queue, term_t msg ARG_LD)
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

  if ( GD->cleaning || !queue->initialized )
    return;				/* deallocation is centralised */
  queue->initialized = FALSE;

  assert(!queue->waiting && !queue->wait_for_drain);

  for( msgp = queue->head; msgp; msgp = next )
  { next = msgp->next;

    freeRecord(msgp->message);
    freeHeap(msgp, sizeof(*msgp));
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
init_message_queue(message_queue *queue, long max_size)
{ memset(queue, 0, sizeof(*queue));
  simpleMutexInit(&queue->mutex);
  simpleMutexInit(&queue->gc_mutex);
  cv_init(&queue->cond_var, NULL);
  queue->max_size = max_size;
  if ( queue->max_size > 0 )
    cv_init(&queue->drain_var, NULL);
  queue->initialized = TRUE;
}

					/* Prolog predicates */

static const opt_spec thread_get_message_options[] =
{ { ATOM_timeout,	OPT_DOUBLE },
  { ATOM_deadline,	OPT_DOUBLE },
  { NULL_ATOM,		0 }
};

#ifndef DBL_MAX
#define DBL_MAX         1.7976931348623158e+308
#endif

/* This function is shared between thread_get_message/3 and thread_send_message/3.
	It extracts a deadline from the deadline/1 and timeout/1 options.
   In both cases, the deadline is passed through to dispatch_cond_wait().
	Semantics are relatively simple:
	1. If neither option is given, the deadline is NULL, which corresponds to
		an indefinite wait, or a deadline in the infinite future.
	2. A timeout is _exactly_ like a deadline of Now + Timeout, where Now is
		evaluated near the beginning of this function.
	3. If both deadline and a timeout options are given, the earlier deadline is effective.
	4. If the effective deadline is before Now, then return FALSE (leading to failure).
*/
static int process_deadline_options(term_t options, struct timespec *ts, struct timespec **pts)
{
  struct timespec now;
  struct timespec deadline;
  struct timespec timeout;
  struct timespec *dlop=NULL;
  double tmo = DBL_MAX;
  double dlo = DBL_MAX;

  if ( !scan_options(options, 0,
		     ATOM_thread_get_message_option, thread_get_message_options,
		     &tmo, &dlo) )
    return FALSE;

  get_current_timespec(&now);

  if ( dlo != DBL_MAX )
  { double ip, fp;

    fp = modf(dlo, &ip);
    deadline.tv_sec = (time_t)ip;
    deadline.tv_nsec = (long)(fp*1000000000.0);
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
  { *ts=*dlop; *pts=ts; }
  else
  { *pts=NULL; }
  return TRUE;
}


static int
thread_send_message__LD(term_t queue, term_t msgterm, struct timespec *deadline ARG_LD)
{ message_queue *q;
  thread_message *msg;
  int rc;

  if ( !(msg = create_thread_message(msgterm PASS_LD)) )
    return PL_no_memory();

  for(;;)
  { if ( !get_message_queue__LD(queue, &q PASS_LD) )
    { free_thread_message(msg);
      return FALSE;
    }

    rc = queue_message(q, msg, deadline PASS_LD);
    release_message_queue(q);

    switch(rc)
    { case MSG_WAIT_INTR:
      { if ( PL_handle_signals() >= 0 )
	  continue;
	free_thread_message(msg);
	rc = FALSE;

	break;
      }
      case MSG_WAIT_DESTROYED:
      { free_thread_message(msg);
	rc = PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_message_queue, queue);
	break;
      }
      case MSG_WAIT_TIMEOUT:
	 rc = FALSE;
	 break;
    }

    break;
  }

  return rc;
}

static
PRED_IMPL("thread_send_message", 2, thread_send_message, PL_FA_ISO)
{ PRED_LD

  return thread_send_message__LD(A1, A2, NULL PASS_LD);
}

static
PRED_IMPL("thread_send_message", 3, thread_send_message, 0)
{ PRED_LD
  struct timespec deadline;
  struct timespec *dlop=NULL;

  return process_deadline_options(A3,&deadline,&dlop)
    &&   thread_send_message__LD(A1, A2, dlop PASS_LD);
}



static
PRED_IMPL("thread_get_message", 1, thread_get_message, PL_FA_ISO)
{ PRED_LD
  int rc;

  for(;;)
  { simpleMutexLock(&LD->thread.messages.mutex);
    rc = get_message(&LD->thread.messages, A1, NULL PASS_LD);
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
  rc = peek_message(&LD->thread.messages, A1 PASS_LD);
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
  { queueTable = newHTable(16);
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
   level code must use get_message_queue__LD();
*/

static int
get_message_queue_unlocked__LD(term_t t, message_queue **queue ARG_LD)
{ atom_t name;
  word id = 0;
  int tid;

  if ( PL_get_atom(t, &name) )
  { id = name;
  } else if ( PL_get_integer(t, &tid) )
  { thread_queue:
    if ( tid < 1 || tid > thread_highest_id ||
	 GD->thread.threads[tid]->status == PL_THREAD_UNUSED ||
	 GD->thread.threads[tid]->status == PL_THREAD_RESERVED ||
	 !GD->thread.threads[tid]->thread_data )
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_thread, t);

    *queue = &GD->thread.threads[tid]->thread_data->thread.messages;
    return TRUE;
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
  { if ( (tid = (int)(intptr_t)lookupHTable(threadTable, (void *)id)) )
    { goto thread_queue;
    }
  }

  return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_message_queue, t);
}


/* Get a message queue and lock it
*/

static int
get_message_queue__LD(term_t t, message_queue **queue ARG_LD)
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

  LOCK();
  rc = get_message_queue_unlocked__LD(t, queue PASS_LD);
  if ( rc )
  { message_queue *q = *queue;

    simpleMutexLock(&q->mutex);
    if ( q->destroyed )
    { rc = PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_message_queue, t);
      simpleMutexUnlock(&q->mutex);
    }
  }
  UNLOCK();

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

  LOCK();
  rval = (unlocked_message_queue_create(A1, -1) ? TRUE : FALSE);
  UNLOCK();

  return rval;
}


static const opt_spec message_queue_options[] =
{ { ATOM_alias,		OPT_ATOM },
  { ATOM_max_size,	OPT_NATLONG },
  { NULL_ATOM,		0 }
};


static
PRED_IMPL("message_queue_create", 2, message_queue_create2, 0)
{ PRED_LD
  atom_t alias = 0;
  long max_size = -1;			/* to be processed */
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

  LOCK();
  q = unlocked_message_queue_create(A1, max_size);
  UNLOCK();

  return q ? TRUE : FALSE;
}


static
PRED_IMPL("message_queue_destroy", 1, message_queue_destroy, 0)
{ PRED_LD
  message_queue *q;

  if ( !get_message_queue__LD(A1, &q PASS_LD) )
    return FALSE;

  if ( q->type == QTYPE_THREAD )
  { release_message_queue(q);

    return PL_error(NULL, 0, "is a thread-queue", ERR_PERMISSION,
		    ATOM_destroy, ATOM_message_queue, A1);
  }

  deleteHTable(queueTable, (void*)q->id);
  if ( !q->anonymous )
    PL_unregister_atom(q->id);

  q->destroyed = TRUE;

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

static int			/* message_queue_property(Queue, alias(Name)) */
message_queue_alias_property(message_queue *q, term_t prop ARG_LD)
{ if ( !q->anonymous )
    return PL_unify_atom(prop, q->id);

  fail;
}


static int			/* message_queue_property(Queue, size(Size)) */
message_queue_size_property(message_queue *q, term_t prop ARG_LD)
{ return PL_unify_integer(prop, q->size);
}


static int			/* message_queue_property(Queue, max_size(Size)) */
message_queue_max_size_property(message_queue *q, term_t prop ARG_LD)
{ if ( q->max_size > 0 )
    return PL_unify_integer(prop, q->max_size);

  fail;
}


static const tprop qprop_list [] =
{ { FUNCTOR_alias1,	    message_queue_alias_property },
  { FUNCTOR_size1,	    message_queue_size_property },
  { FUNCTOR_max_size1,	    message_queue_max_size_property },
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
      } else if ( get_message_queue__LD(queue, &state->q PASS_LD) )
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
    { if ( (*state->p->function)(state->q, a1 PASS_LD) )
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
thread_get_message(+Queue, -Message)
thread_get_message(-Message)
    Get a message from a message queue. If the queue is not provided get
    a message from the queue implicitly associated to the thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
thread_get_message__LD(term_t queue, term_t msg, struct timespec *deadline ARG_LD)
{ int rc;

  for(;;)
  { message_queue *q;

    if ( !get_message_queue__LD(queue, &q PASS_LD) )
      return FALSE;

    rc = get_message(q, msg, deadline PASS_LD);
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

  return thread_get_message__LD(A1, A2, NULL PASS_LD);
}


static
PRED_IMPL("thread_get_message", 3, thread_get_message, 0)
{ PRED_LD
  struct timespec deadline;
  struct timespec *dlop=NULL;

  return process_deadline_options(A3,&deadline,&dlop)
    &&   thread_get_message__LD(A1, A2, dlop PASS_LD);
}


static
PRED_IMPL("thread_peek_message", 2, thread_peek_message_2, 0)
{ PRED_LD
  message_queue *q;
  int rc;

  if ( !get_message_queue__LD(A1, &q PASS_LD) )
    fail;

  rc = peek_message(q, A2 PASS_LD);
  release_message_queue(q);
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

  LOCK();
  if ( done )
  { UNLOCK();

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
  UNLOCK();
  *ap = &attr;

  return 0;

error:
  UNLOCK();
  return rc;
}
#endif

int
recursiveMutexInit(recursiveMutex *m)
{
#ifdef RECURSIVE_MUTEXES
  pthread_mutexattr_t *attr = NULL;
  int rc;

  if ( (rc=recursive_attr(&attr)) )
    return rc;

  return pthread_mutex_init(m, attr);

#else /*RECURSIVE_MUTEXES*/

  m->owner = 0;
  m->count = 0;
  return pthread_mutex_init(&(m->lock), NULL);

#endif /* RECURSIVE_MUTEXES */
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


counting_mutex *
allocSimpleMutex(const char *name)
{ counting_mutex *m = allocHeapOrHalt(sizeof(*m));

  simpleMutexInit(&m->mutex);
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

  return m;
}


void
freeSimpleMutex(counting_mutex *m)
{ PL_LOCK(L_MUTEX);
  if ( m->next )
    m->next->prev = m->prev;
  if ( m->prev )
    m->prev->next = m->next;
  else
    GD->thread.mutexes = NULL;
  PL_UNLOCK(L_MUTEX);

  simpleMutexDelete(&m->mutex);
  remove_string((char *)m->name);
  freeHeap(m, sizeof(*m));
}


		 /*******************************
		 *	    USER MUTEXES	*
		 *******************************/

typedef struct mutexref
{ pl_mutex	*mutex;
} mutexref;

static int try_really_destroy_mutex(pl_mutex *m);

static int
write_mutexref(IOSTREAM *s, atom_t aref, int flags)
{ mutexref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<mutex>(%p)", ref->mutex);
  return TRUE;
}


static int
release_mutexref(atom_t aref)
{ mutexref *ref = PL_blob_data(aref, NULL, NULL);
  pl_mutex *m;

  DEBUG(MSG_MUTEX_GC,
	Sdprintf("GC mutex %p\n", ref->mutex));

  if ( (m=ref->mutex) )
  { if ( !m->destroyed )
      deleteHTable(GD->thread.mutexTable, (void *)m->id);

    if ( m->owner )
    { Sdprintf("WARNING: <mutex>(%p) garbage collected "
	       "while owned by thread %d\n",
	       m, m->owner);

      if ( m->owner == PL_thread_self() )
	pthread_mutex_unlock(&m->mutex);
      else
	return TRUE;
    }

    if ( m->initialized )
      pthread_mutex_destroy(&m->mutex);
    unalloc_mutex(m);
  }

  return TRUE;
}


static int
save_mutexref(atom_t aref, IOSTREAM *fd)
{ mutexref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <mutex>(%p)", ref->mutex);
}


static atom_t
load_mutexref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-mutex-ref>");
}


static PL_blob_t mutex_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "mutex",
  release_mutexref,
  NULL,
  write_mutexref,
  NULL,
  save_mutexref,
  load_mutexref
};


static void
initMutexRef(void)
{ mutex_blob.atom_name = ATOM_mutex;	/* avoid early initAtoms() */
  PL_register_blob_type(&mutex_blob);
}


static void
unalloc_mutex(pl_mutex *m)
{ freeHeap(m, sizeof(*m));
}


static void
destroy_mutex(pl_mutex *m)
{ if ( m->initialized )
  { m->initialized = FALSE;
    pthread_mutex_destroy(&m->mutex);
  }
  if ( !m->anonymous )
    unalloc_mutex(m);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
User-level mutexes. On Windows we can't   use  critical sections here as
TryEnterCriticalSection() is only defined on NT 4, not on Windows 95 and
friends.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unify_mutex(term_t t, pl_mutex *m)
{ GET_LD

  return PL_unify_atom(t, m->id);
}


static int
unify_mutex_owner(term_t t, int owner)
{ if ( owner )
    return unify_thread_id(t, GD->thread.threads[owner]);
  else
    return PL_unify_nil(t);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) We must lock the atom, but   threads are initialised before the atom
infrastructure :-( Note that this is hacky, but safe: m->id is an ATOM_*
built-in atom and  needs  not  to  be   locked.  Putting  this  test  in
PL_register_atom() would be cleaner, but that  routine is much more time
critical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static pl_mutex *
mutexCreate(atom_t name)
{ pl_mutex *m;

  if ( (m=allocHeap(sizeof(*m))) )
  { memset(m, 0, sizeof(*m));
    pthread_mutex_init(&m->mutex, NULL);
    m->initialized = TRUE;

    if ( name == NULL_ATOM )
    { mutexref ref;
      int new;

      ref.mutex = m;
      m->id = lookupBlob((void*)&ref, sizeof(ref), &mutex_blob, &new);
      m->anonymous = TRUE;
    } else
    { m->id = name;
    }

    addNewHTable(GD->thread.mutexTable, (void *)m->id, m);
    if ( m->anonymous )
      PL_unregister_atom(m->id);		/* reclaim on GC */
    else if ( GD->atoms.builtin )		/* (*) */
      PL_register_atom(m->id);
  } else
    PL_no_memory();

  return m;
}


static pl_mutex *
unlocked_pl_mutex_create(term_t mutex)
{ GET_LD
  atom_t name = NULL_ATOM;
  pl_mutex *m;
  word id;

  if ( PL_get_atom(mutex, &name) )
  { if ( lookupHTable(GD->thread.mutexTable, (void *)name) )
    { PL_error("mutex_create", 1, NULL, ERR_PERMISSION,
	       ATOM_create, ATOM_mutex, mutex);
      return NULL;
    }
    id = name;
  } else if ( PL_is_variable(mutex) )
  { id = NULL_ATOM;
  } else
  { PL_error("mutex_create", 1, NULL, ERR_TYPE, ATOM_mutex, mutex);
    return NULL;
  }

  if ( (m=mutexCreate(id)) )
  { if ( !unify_mutex(mutex, m) )
    { destroy_mutex(m);
      m = NULL;
    }
  }

  return m;
}


static
PRED_IMPL("mutex_create", 1, mutex_create1, 0)
{ int rval;

  LOCK();
  rval = (unlocked_pl_mutex_create(A1) ? TRUE : FALSE);
  UNLOCK();

  return rval;
}


static const opt_spec mutex_options[] =
{ { ATOM_alias,		OPT_ATOM },
  { NULL_ATOM,		0 }
};


static
PRED_IMPL("mutex_create", 2, mutex_create2, 0)
{ PRED_LD
  int rval;
  atom_t alias = 0;

  if ( !scan_options(A2, 0,
		     ATOM_mutex_option, mutex_options,
		     &alias) )
    fail;

  if ( alias )
  { if ( !PL_unify_atom(A1, alias) )
      return PL_error("mutex_create", 2, NULL, ERR_UNINSTANTIATION, 1, A1);
  }

  LOCK();
  rval = (unlocked_pl_mutex_create(A1) ? TRUE : FALSE);
  UNLOCK();

  return rval;
}


static int
get_mutex(term_t t, pl_mutex **mutex, int create)
{ GET_LD
  atom_t name;
  word id = 0;
  pl_mutex *m = NULL;

  if ( PL_get_atom(t, &name) )
  { PL_blob_t *type;
    mutexref *ref = PL_blob_data(name, NULL, &type);

    if ( type == &mutex_blob )
    { m = ref->mutex;
      goto out;
    } else if ( isTextAtom(name) )
    { id = name;
    }
  }

  if ( !id )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_mutex, t);

  LOCK();
  if ( GD->thread.mutexTable &&
       (m = lookupHTable(GD->thread.mutexTable, (void *)id)) )
  { ;
  } else if ( create )
  { m = unlocked_pl_mutex_create(t);
  } else
  { PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_mutex, t);
  }
  UNLOCK();

out:
  if ( m )
  { if ( !m->destroyed )
    { *mutex = m;
      return TRUE;
    }
    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_mutex, t);
  }

  return FALSE;
}



int
PL_mutex_lock(struct pl_mutex *m)
{ int self = PL_thread_self();

  if ( self == m->owner )
  { m->count++;
  } else
  { pthread_mutex_lock(&m->mutex);
    m->count = 1;
    m->owner = self;
  }

  return TRUE;
}


static
PRED_IMPL("mutex_lock", 1, mutex_lock, 0)
{ pl_mutex *m;

  if ( !get_mutex(A1, &m, TRUE) )
    return FALSE;

  return  PL_mutex_lock(m);
}


static int
PL_mutex_trylock(struct pl_mutex *m)
{ int self = PL_thread_self();
  int rc;

  if ( self == m->owner )
  { m->count++;
  } else if ( (rc = pthread_mutex_trylock(&m->mutex)) == 0 )
  { m->count = 1;
    m->owner = self;
  } else
  { assert(rc == EBUSY);
    return FALSE;
  }

  return TRUE;
}


static
PRED_IMPL("mutex_trylock", 1, mutex_trylock, 0)
{ pl_mutex *m;

  if ( !get_mutex(A1, &m, TRUE) )
    return FALSE;

  return  PL_mutex_trylock(m);
}


int
PL_mutex_unlock(struct pl_mutex *m)
{ int self = PL_thread_self();

  if ( self == m->owner )
  { if ( --m->count == 0 )
    { m->owner = 0;

      pthread_mutex_unlock(&m->mutex);
    }

    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The error message of this  predicate  is   not  thread-safe.  I.e. it is
possible the message is wrong. This can   only be fixed by modifying the
API of PL_mutex_unlock(), which is asking a  bit too much for this small
error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("mutex_unlock", 1, mutex_unlock, 0)
{ pl_mutex *m;

  if ( !get_mutex(A1, &m, FALSE) )
    return FALSE;

  if ( PL_mutex_unlock(m) )
  { if ( m->auto_destroy )
    { LOCK();
      try_really_destroy_mutex(m);
      UNLOCK();
    }

    return TRUE;
  } else
  { char *msg = m->owner ? "not owner" : "not locked";

    return PL_error("mutex_unlock", 1, msg, ERR_PERMISSION,
		    ATOM_unlock, ATOM_mutex, A1);
  }
}


static
PRED_IMPL("mutex_unlock_all", 0, mutex_unlock_all, 0)
{ TableEnum e;
  pl_mutex *m;
  int tid = PL_thread_self();

  e = newTableEnum(GD->thread.mutexTable);
  while( advanceTableEnum(e, NULL, (void**)&m) )
  { if ( m->owner == tid )
    { m->count = 0;
      m->owner = 0;
      pthread_mutex_unlock(&m->mutex);
    }
  }
  freeTableEnum(e);
  return TRUE;
}


static int
try_really_destroy_mutex(pl_mutex *m)
{ if ( PL_mutex_trylock(m) )
  { if ( m->count == 1 )
    { m->destroyed = TRUE;
      deleteHTable(GD->thread.mutexTable, (void *)m->id);
      if ( !m->anonymous )
	PL_unregister_atom(m->id);
      m->count = 0;
      m->owner = 0;
      pthread_mutex_unlock(&m->mutex);
      destroy_mutex(m);
      return TRUE;
    } else
      PL_mutex_unlock(m);
  }

  return FALSE;
}


static
PRED_IMPL("mutex_destroy", 1, mutex_destroy, 0)
{ pl_mutex *m;

  if ( !get_mutex(A1, &m, FALSE) )
    return FALSE;

  LOCK();
  if ( !try_really_destroy_mutex(m) )
    m->auto_destroy = TRUE;
  UNLOCK();

  return TRUE;
}


		 /*******************************
		 *	  MUTEX_PROPERTY	*
		 *******************************/

static int		/* mutex_property(Mutex, alias(Name)) */
mutex_alias_property(pl_mutex *m, term_t prop ARG_LD)
{ if ( !m->anonymous )
    return PL_unify_atom(prop, m->id);

  fail;
}


static int		/* mutex_property(Mutex, status(locked(By, Count))) */
mutex_status_property(pl_mutex *m, term_t prop ARG_LD)
{ if ( m->owner )
  { int owner = m->owner;
    int count = m->count;
    term_t owner_term = PL_new_term_ref();

    return (PL_unify_term(prop, PL_FUNCTOR, FUNCTOR_locked2,
			    PL_TERM, owner_term,
			    PL_INT, count) &&
	    unify_mutex_owner(owner_term, owner));
  } else
  { return PL_unify_atom(prop, ATOM_unlocked);
  }

  fail;
}


static const tprop mprop_list [] =
{ { FUNCTOR_alias1,	    mutex_alias_property },
  { FUNCTOR_status1,	    mutex_status_property },
  { 0,			    NULL }
};


typedef struct
{ TableEnum e;				/* Enumerator on mutex-table */
  pl_mutex *m;				/* current mutex */
  const tprop *p;			/* Pointer in properties */
  int enum_properties;			/* Enumerate the properties */
} mprop_enum;


static int
advance_mstate(mprop_enum *state)
{ if ( state->enum_properties )
  { state->p++;
    if ( state->p->functor )
      succeed;

    state->p = mprop_list;
  }
  if ( state->e )
  { pl_mutex *m;

    if ( advanceTableEnum(state->e, NULL, (void**)&m) )
    { state->m = m;

      succeed;
    }
  }

  fail;
}


static void
free_mstate(mprop_enum *state)
{ if ( state->e )
    freeTableEnum(state->e);

  freeForeignState(state, sizeof(*state));
}


static
PRED_IMPL("mutex_property", 2, mutex_property, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  term_t mutex = A1;
  term_t property = A2;
  mprop_enum statebuf;
  mprop_enum *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { memset(&statebuf, 0, sizeof(statebuf));
      state = &statebuf;

      if ( PL_is_variable(mutex) )
      { switch( get_prop_def(property, ATOM_mutex_property,
			     mprop_list, &state->p) )
	{ case 1:
	    state->e = newTableEnum(GD->thread.mutexTable);
	    goto enumerate;
	  case 0:
	    state->e = newTableEnum(GD->thread.mutexTable);
	    state->p = mprop_list;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    fail;
	}
      } else if ( get_mutex(mutex, &state->m, FALSE) )
      { switch( get_prop_def(property, ATOM_mutex_property,
			     mprop_list, &state->p) )
	{ case 1:
	    goto enumerate;
	  case 0:
	    state->p = mprop_list;
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
      free_mstate(state);
      succeed;
    default:
      assert(0);
      fail;
  }

enumerate:
  if ( !state->m )			/* first time, enumerating mutexes */
  { pl_mutex *m;

    assert(state->e);
    if ( advanceTableEnum(state->e, NULL, (void**)&m) )
    { state->m = m;
    } else
    { freeTableEnum(state->e);
      assert(state != &statebuf);
      fail;
    }
  }


  { term_t arg = PL_new_term_ref();

    if ( !state->enum_properties )
      _PL_get_arg(1, property, arg);

    for(;;)
    { if ( (*state->p->function)(state->m, arg PASS_LD) )
      { if ( state->enum_properties )
	{ if ( !PL_unify_term(property,
			      PL_FUNCTOR, state->p->functor,
			        PL_TERM, arg) )
	    goto error;
	}
	if ( state->e )
	{ if ( !unify_mutex(mutex, state->m) )
	    goto error;
	}

	if ( advance_mstate(state) )
	{ if ( state == &statebuf )
	  { mprop_enum *copy = allocForeignState(sizeof(*copy));

	    *copy = *state;
	    state = copy;
	  }

	  ForeignRedoPtr(state);
	}

	if ( state != &statebuf )
	  free_mstate(state);
	succeed;
      }

      if ( !advance_mstate(state) )
      { error:
	if ( state != &statebuf )
	  free_mstate(state);
	fail;
      }
    }
  }
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
    LD->thread.info->open_count++;

  LOCK();
  if ( !GD->thread.enabled || GD->cleaning != CLN_NORMAL )
  { UNLOCK();
#ifdef EPERM				/* FIXME: Better reporting */
    errno = EPERM;
#endif
    return -1;
  }

  info = alloc_thread();
  UNLOCK();
  if ( !info )
    return -1;				/* out of threads */

  ldmain = GD->thread.threads[1]->thread_data;
  ldnew = info->thread_data;

  if ( attr )
  { if ( attr->local_size )
      info->local_size = attr->local_size * 1024;
    if ( attr->global_size )
      info->global_size = attr->global_size * 1024;
    if ( attr->trail_size )
      info->trail_size = attr->trail_size * 1024;

    info->cancel = attr->cancel;
  }

  info->goal       = NULL;
  info->module     = MODULE_user;
  info->detached   = TRUE;		/* C-side should join me */
  info->status     = PL_THREAD_RUNNING;
  info->open_count = 1;

  init_message_queue(&ldnew->thread.messages, -1);
  init_predicate_references(ldnew);

  ldnew->prompt			 = ldmain->prompt;
  ldnew->modules		 = ldmain->modules;
  ldnew->IO			 = ldmain->IO;
  ldnew->IO.input_stack		 = NULL;
  ldnew->IO.output_stack	 = NULL;
  ldnew->encoding		 = ldmain->encoding;
#ifdef O_LOCALE
  ldnew->locale.current		 = acquireLocale(ldmain->locale.current);
#endif
  ldnew->_debugstatus		 = ldmain->_debugstatus;
  ldnew->_debugstatus.retryFrame = 0;
  ldnew->prolog_flag.mask	 = ldmain->prolog_flag.mask;
  if ( ldmain->prolog_flag.table )
  { TLD_set_LD(info->thread_data);

    PL_LOCK(L_PLFLAG);
    ldnew->prolog_flag.table	 = copyHTable(ldmain->prolog_flag.table);
    PL_UNLOCK(L_PLFLAG);
  }

  if ( !initialise_thread(info) )
  { free_thread_info(info);
    errno = ENOMEM;
    return -1;
  }
  set_system_thread_id(info);

  if ( attr )
  { if ( attr->alias )
    { if ( !aliasThread(info->pl_tid, PL_new_atom(attr->alias)) )
      { free_thread_info(info);
	errno = EPERM;
	return -1;
      }
    }
    if ( true(attr, PL_THREAD_NO_DEBUG) )
    { ldnew->_debugstatus.tracing   = FALSE;
      ldnew->_debugstatus.debugging = DBG_OFF;
      set(&ldnew->prolog_flag.mask, PLFLAG_LASTCALL);
      info->debug = FALSE;
    }
  }

  info->status = PL_THREAD_CREATED;
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

#ifdef __linux__
  info->pid = -1;
#endif
#ifdef __WINDOWS__
  info->w32id = 0;
#endif
  memset(&info->tid, 0, sizeof(info->tid));
  info->has_tid = FALSE;

  TLD_set_LD(NULL);
}


int
PL_set_engine(PL_engine_t new, PL_engine_t *old)
{ PL_engine_t current = PL_current_engine();

  if ( new != current && new != PL_ENGINE_CURRENT )
  { LOCK();

    if ( new )
    { if ( new == PL_ENGINE_MAIN )
	new = &PL_local_data;

      if ( new->magic != LD_MAGIC )
      { UNLOCK();
	return PL_ENGINE_INVAL;
      }
      if ( new->thread.info->has_tid )
      { UNLOCK();
	return PL_ENGINE_INUSE;
      }
    }

    if ( current )
      detach_engine(current);

    if ( new )
    { TLD_set_LD(new);
      new->thread.info->tid = pthread_self();

      set_system_thread_id(new->thread.info);
    }

    UNLOCK();
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

  LOCK();
  if ( !get_thread(A1, &info, TRUE) )
  { UNLOCK();
    fail;
  }

  if ( !(ld=info->thread_data) )
  { UNLOCK();
    return PL_error(NULL, 0, NULL,
		    ERR_PERMISSION,
		    ATOM_statistics, ATOM_thread, A1);
  }

  if ( !PL_get_atom(A2, &k) )
    k = 0;

  if ( k == ATOM_heapused )
    ld = LD;
  else if ( k == ATOM_cputime || k == ATOM_runtime )
    ld->statistics.user_cputime = ThreadCPUTime(ld, CPU_USER);
  else if ( k == ATOM_system_time )
    ld->statistics.system_cputime = ThreadCPUTime(ld, CPU_SYSTEM);

  if ( LD == ld )		/* self: unlock first to avoid deadlock */
  { UNLOCK();
    return pl_statistics_ld(A2, A3, ld PASS_LD);
  }

  rval = pl_statistics_ld(A2, A3, ld PASS_LD);
  UNLOCK();

  return rval;
}


#ifdef __WINDOWS__

/* How to make the memory visible?
*/

					/* see also pl-nt.c */
#define nano * 0.000000001
#define ntick 100.0

double
ThreadCPUTime(PL_local_data_t *ld, int which)
{ PL_thread_info_t *info = ld->thread.info;
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
ThreadCPUTime(PL_local_data_t *ld, int which)
{ PL_thread_info_t *info = ld->thread.info;

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
ThreadCPUTime(PL_local_data_t *ld, int which)
{ PL_thread_info_t *info = ld->thread.info;

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
ThreadCPUTime(PL_local_data_t *ld, int which)
{ PL_thread_info_t *info = ld->thread.info;
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
    sem_post(sem_mark_ptr);
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
    sem_post(sem_mark_ptr);
}


static void
SyncSystemCPU(int sig)
{ GET_LD

  if ( LD )
    LD->statistics.system_cputime = CpuTime(CPU_SYSTEM);
  if ( sig )
    sem_post(sem_mark_ptr);
}

#endif  /*LINUX_CPUCLOCKS*/

double
ThreadCPUTime(PL_local_data_t *ld, int which)
{ GET_LD
  PL_thread_info_t *info = ld->thread.info;

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
    sem_init(sem_mark_ptr, USYNC_THREAD, 0);
    allSignalMask(&sigmask);
    memset(&new, 0, sizeof(new));
    new.sa_handler = (which == CPU_USER ? SyncUserCPU : SyncSystemCPU);
    new.sa_flags   = SA_RESTART;
    new.sa_mask    = sigmask;
    sigaction(SIG_FORALL, &new, &old);

    if ( info->has_tid )
      ok = (pthread_kill(info->tid, SIG_FORALL) == 0);
    else
      ok = FALSE;

    if ( ok )
    { while( sem_wait(sem_mark_ptr) == -1 && errno == EINTR )
	;
    }
    sem_destroy(&sem_mark);
    sigaction(SIG_FORALL, &old, NULL);
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
forThreadLocalDataUnsuspended(void (*func)(PL_local_data_t *), unsigned flags)
{ int me = PL_thread_self();
  PL_thread_info_t **th;

  for( th = &GD->thread.threads[1];
       th <= &GD->thread.threads[thread_highest_id];
       th++ )
  { PL_thread_info_t *info = *th;

    if ( info->thread_data && info->pl_tid != me &&
	 ( info->status == PL_THREAD_RUNNING || info->in_exit_hooks ) )
    { PL_local_data_t *ld = info->thread_data;
      (*func)(ld);
    }
  }

  DEBUG(MSG_THREAD, Sdprintf(" All done!\n"));
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

We must lock the individual queues before   processing. This is safe, as
these mutexes are never helt long  and   the  other  threads are not yet
silenced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
markAtomsMessageQueue(message_queue *queue)
{ thread_message *msg;

  simpleMutexLock(&queue->gc_mutex);
  for(msg=queue->head; msg; msg=msg->next)
  { markAtomsRecord(msg->message);
  }
  simpleMutexUnlock(&queue->gc_mutex);
}


void
markAtomsThreadMessageQueue(PL_local_data_t *ld)
{ markAtomsMessageQueue(&ld->thread.messages);
}


void
markAtomsMessageQueues(void)
{ if ( queueTable )
  { message_queue *q;
    TableEnum e = newTableEnum(queueTable);

    while( advanceTableEnum(e, NULL, (void**)&q) )
    { markAtomsMessageQueue(q);
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


LocalDefinitions
new_ldef_vector(void)
{ LocalDefinitions f = allocHeapOrHalt(sizeof(*f));

  memset(f, 0, sizeof(*f));
  f->blocks[0] = f->preallocated - 1;
  f->blocks[1] = f->preallocated - 1;
  f->blocks[2] = f->preallocated - 1;

  return f;
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

  *local = *def;
  clear(local, P_THREAD_LOCAL);		/* remains P_DYNAMIC */
  local->impl.clauses.first_clause = NULL;
  local->impl.clauses.clause_indexes = NULL;
  ATOMIC_INC(&GD->statistics.predicates);
  ATOMIC_ADD(&local->module->code_size, sizeof(*local));
  DEBUG(MSG_PROC_COUNT, Sdprintf("Localise %s\n", predicateName(def)));

  createSupervisor(local);
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

    DEBUG(MSG_CLEANUP,
	  Sdprintf("Clean local def in thread %d for %s\n",
		   id,
		   predicateName(def)));

    assert(true(def, P_THREAD_LOCAL));
    destroyLocalDefinition(def, id);
    freeHeap(ch, sizeof(*ch));
  }
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

  if ( !get_thread_sync(thread, &info, FALSE) )
    fail;

  LOCK();
  if ( (def = getProcDefinitionForThread(proc->definition, info->pl_tid)) )
    number_of_clauses = def->impl.clauses.number_of_clauses;
  UNLOCK();

  return PL_unify_integer(count, number_of_clauses);
}



		 /*******************************
		 *	DEBUGGING SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is called from  GNU  <assert.h>,   so  we  can print which
thread caused the problem. If the thread is   not the main one, we could
try to recover!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(HAVE_CTIME_R) && !defined(ctime_r)
#define ctime_r(timep, buf) strcpy(buf, ctime(timep))
#endif

void
__assert_fail(const char *assertion,
	      const char *file,
	      unsigned int line,
	      const char *function)
{ int tid = PL_thread_self();
  atom_t alias;
  const pl_wchar_t *name = L"";
  time_t now = time(NULL);
  char tbuf[48];

  ctime_r(&now, tbuf);
  tbuf[24] = '\0';

  if ( PL_get_thread_alias(tid, &alias) )
    name = PL_atom_wchars(alias, NULL);

  Sdprintf("[Thread %d (%Ws) at %s] %s:%d: %s: Assertion failed: %s\n",
	   PL_thread_self(), name, tbuf,
	   file, line, function, assertion);
  save_backtrace("assert_fail");
  print_backtrace_named("assert_fail");
  abort();
}

#else /*O_PLMT*/

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
ThreadCPUTime(PL_local_data_t *ld, int which) {
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

		 /*******************************
		 *	    WITH-MUTEX		*
		 *******************************/

foreign_t
pl_with_mutex(term_t mutex, term_t goal)
{ term_t ex = 0;
  int rval;

#ifdef O_PLMT
  pl_mutex *m;

  if ( !get_mutex(mutex, &m, TRUE) )
    return FALSE;
  PL_mutex_lock(m);
  rval = callProlog(NULL, goal, PL_Q_CATCH_EXCEPTION, &ex);
  PL_mutex_unlock(m);
#else
  rval = callProlog(NULL, goal, PL_Q_CATCH_EXCEPTION, &ex);
#endif

  if ( !rval && ex )
  { DEBUG(CHK_SECURE,
	  { GET_LD
	    checkData(valTermRef(ex));
	  });
    PL_raise_exception(ex);
  }

  return rval;
}


		 /*******************************
		 *    HASH-TABLE KVS IN USE     *
		 *******************************/

int
pl_kvs_in_use(KVS kvs)
{
#ifdef O_PLMT
  int i;

  for(i=1; i<=thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->kvs == kvs )
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

  for(i=1; i<=thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->atom_table == atom_table )
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

  for(i=1; i<=thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->atom_bucket == bucket )
    { return TRUE;
    }
  }
#endif

  return FALSE;
}


Atom**
pl_atom_buckets_in_use()
{
#ifdef O_PLMT
  int i, index=0;
  size_t sz = 32;

  Atom **buckets = allocHeapOrHalt(sz * sizeof(Atom*));
  memset(buckets, 0, sz * sizeof(Atom*));

  for(i=1; i<=thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->atom_bucket )
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
      buckets[index] = info->atom_bucket;
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
  int i, index=0;
  size_t sz = 32;

  Definition *buckets = allocHeapOrHalt(sz * sizeof(Definition));
  memset(buckets, 0, sz * sizeof(Definition*));

  for(i=1; i<=thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( info && info->predicate )
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
      buckets[index] = info->predicate;
      if ( buckets[index] )	/* atom_bucket may have been released */
        index++;
    }
  }

  return buckets;
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

  for(i=1; i<=thread_highest_id; i++)
  { PL_thread_info_t *info = GD->thread.threads[i];
    if ( i != me && info && info->functor_table == functor_table )
    { return TRUE;
    }
  }
#endif

  return FALSE;
}

		 /*******************************
		 * CLAUSE/3 PREDICATE REFERENCES*
		 *******************************/

static void
init_predicate_references(PL_local_data_t *ld)
{ definition_refs *refs = &ld->predicate_references;

  memset(refs, 0, sizeof(*refs));
  refs->blocks[0] = refs->preallocated - 1;
  refs->blocks[1] = refs->preallocated - 1;
  refs->blocks[2] = refs->preallocated - 1;

#ifdef O_PLMT
  simpleMutexInit(&ld->clauses.local_shift_mutex);
#endif
}

static void
free_predicate_references(PL_local_data_t *ld)
{ definition_refs *refs = &ld->predicate_references;
  int i;

  for(i=3; i<MAX_BLOCKS; i++)
  { size_t bs = (size_t)1<<i;
    definition_ref *d0 = refs->blocks[i];

    if ( d0 )
      freeHeap(d0+bs, bs*sizeof(definition_ref));
  }

#ifdef O_PLMT
  simpleMutexDelete(&ld->clauses.local_shift_mutex);
#endif
}

int
pushPredicateAccess__LD(Definition def, gen_t gen ARG_LD)
{ definition_refs *refs = &LD->predicate_references;
  definition_ref *dref;
  size_t top = refs->top+1;
  size_t idx = MSB(top);

  DEBUG(MSG_CGC_PRED_REF,
	Sdprintf("pushPredicateAccess(%s)\n", predicateName(def)));

  if ( !refs->blocks[idx] )
  { size_t bs = (size_t)1<<idx;
    definition_ref *newblock;

    if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(definition_ref))) )
      outOfCore();

    memset(newblock, 0, bs*sizeof(definition_ref));
    refs->blocks[idx] = newblock-bs;
  }

  enterDefinition(def);			/* probably not needed in the end */
  dref = &refs->blocks[idx][top];
  dref->predicate  = def;
  dref->generation = gen;

  refs->top = top;

  return TRUE;
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
popPredicateAccess__LD(Definition def ARG_LD)
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
popNPredicateAccess__LD(size_t n ARG_LD)
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
    DirtyDefInfo ddi;
    definition_ref dref = refs->blocks[idx][i];

    if ( is_pointer_like(dref.predicate) &&
	 (ddi=lookupHTable(GD->procedures.dirty, dref.predicate)) )
    { if ( dref.generation < ddi->oldest_generation )
	ddi->oldest_generation = dref.generation;
    }
  }
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(thread)
#ifdef O_PLMT
  PRED_DEF("thread_detach", 1, thread_detach, PL_FA_ISO)
  PRED_DEF("thread_join", 2, thread_join, 0)
  PRED_DEF("thread_statistics", 3, thread_statistics, 0)
  PRED_DEF("thread_property", 2, thread_property, PL_FA_NONDETERMINISTIC|PL_FA_ISO)
  PRED_DEF("message_queue_create", 1, message_queue_create, 0)
  PRED_DEF("message_queue_create", 2, message_queue_create2, PL_FA_ISO)
  PRED_DEF("message_queue_property", 2, message_property, PL_FA_NONDETERMINISTIC|PL_FA_ISO)
  PRED_DEF("thread_send_message", 2, thread_send_message, PL_FA_ISO)
  PRED_DEF("thread_send_message", 3, thread_send_message, 0)
  PRED_DEF("thread_get_message", 1, thread_get_message, PL_FA_ISO)
  PRED_DEF("thread_get_message", 2, thread_get_message, PL_FA_ISO)
  PRED_DEF("thread_get_message", 3, thread_get_message, PL_FA_ISO)
  PRED_DEF("thread_peek_message", 1, thread_peek_message_1, PL_FA_ISO)
  PRED_DEF("thread_peek_message", 2, thread_peek_message_2, PL_FA_ISO)
  PRED_DEF("message_queue_destroy", 1, message_queue_destroy, PL_FA_ISO)
  PRED_DEF("thread_setconcurrency", 2, thread_setconcurrency, 0)

  PRED_DEF("mutex_statistics", 0, mutex_statistics, 0)
  PRED_DEF("mutex_create",     1, mutex_create1,    0)
  PRED_DEF("mutex_create",     2, mutex_create2,    PL_FA_ISO)
  PRED_DEF("mutex_destroy",    1, mutex_destroy,    PL_FA_ISO)
  PRED_DEF("mutex_lock",       1, mutex_lock,	    PL_FA_ISO)
  PRED_DEF("mutex_trylock",    1, mutex_trylock,    PL_FA_ISO)
  PRED_DEF("mutex_unlock",     1, mutex_unlock,	    PL_FA_ISO)
  PRED_DEF("mutex_unlock_all", 0, mutex_unlock_all, 0)
  PRED_DEF("mutex_property", 2, mutex_property, PL_FA_NONDETERMINISTIC|PL_FA_ISO)

  PRED_DEF("$thread_local_clause_count", 3, thread_local_clause_count, 0)
#endif
EndPredDefs
