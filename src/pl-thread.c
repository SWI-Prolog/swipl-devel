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

#define O_DEBUG 1

#define _GNU_SOURCE 1			/* get recursive mutex stuff to */
					/* compile clean with glibc.  Can */
					/* this to any harm? */
#include "pl-incl.h"
#include <stdio.h>
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

static sem_t sem_canceled;		/* used on halt */

#ifndef WIN32
#include <signal.h>

static sem_t sem_mark;			/* used for atom-gc */
#ifndef SA_RESTART
#define SA_RESTART 0
#endif

#define SIG_FORALL SIGHUP
#define SIG_RESUME SIG_FORALL
#endif





		 /*******************************
		 *	    GLOBAL DATA		*
		 *******************************/

static Table threadTable;		/* name --> integer-id */
static PL_thread_info_t threads[MAX_THREADS];
static int threads_ready = FALSE;	/* Prolog threads available */
static Table queueTable;		/* name --> queue */
static int queue_id;			/* next generated id */


TLD_KEY PL_ldata;			/* key for thread PL_local_data */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The global mutexes. Most are using  within   a  module and their name is
simply the module-name. The idea is that   a module holds a coherent bit
of data that needs a mutex for all operations.

Some remarks:

    L_MISC
	General-purpose mutex.  Should only be used for simple, very
	local tasks and may not be used to lock anything significant.

    WIN32
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

simpleMutex _PL_mutexes[] =
{ PTHREAD_MUTEX_INITIALIZER,		/* L_MISC */
  PTHREAD_MUTEX_INITIALIZER,		/* L_ALLOC */
  PTHREAD_MUTEX_INITIALIZER,		/* L_ATOM */
  PTHREAD_MUTEX_INITIALIZER,		/* L_FLAG */
  PTHREAD_MUTEX_INITIALIZER,		/* L_FUNCTOR */
  PTHREAD_MUTEX_INITIALIZER,		/* L_RECORD */
  PTHREAD_MUTEX_INITIALIZER,		/* L_THREAD */
  PTHREAD_MUTEX_INITIALIZER,		/* L_PREDICATE */
  PTHREAD_MUTEX_INITIALIZER,		/* L_MODULE */
  PTHREAD_MUTEX_INITIALIZER,		/* L_TABLE */
  PTHREAD_MUTEX_INITIALIZER,		/* L_BREAK */
  PTHREAD_MUTEX_INITIALIZER,		/* L_FILE */
  PTHREAD_MUTEX_INITIALIZER,		/* L_FEATURE */
  PTHREAD_MUTEX_INITIALIZER,		/* L_OP */
  PTHREAD_MUTEX_INITIALIZER,		/* L_INIT */
  PTHREAD_MUTEX_INITIALIZER,		/* L_TERM */
  PTHREAD_MUTEX_INITIALIZER		/* L_GC */
};

#ifdef USE_CRITICAL_SECTIONS

static void
initMutexes()
{ simpleMutex *m;
  int n = sizeof(_PL_mutexes)/sizeof(simpleMutex);
  int i;

  for(i=0, m=_PL_mutexes; i<n; i++, m++)
    simpleMutexInit(m);
}


BOOL WINAPI
DllMain(HINSTANCE hinstDll, DWORD fdwReason, LPVOID lpvReserved)
{ BOOL result = TRUE;

  switch(fdwReason)
  { case DLL_PROCESS_ATTACH:
      GD->thread.instance = hinstDll;
      initMutexes();
      break;
    case DLL_PROCESS_DETACH:
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
      break;
  }

  return result;
}

#endif /*USE_CRITICAL_SECTIONS*/


		 /*******************************
		 *	  LOCAL PROTOTYPES	*
		 *******************************/

static PL_thread_info_t *alloc_thread(void);
static void	destroy_message_queue(message_queue *queue);
static void	init_message_queue(message_queue *queue);
static void	freeThreadSignals(PL_local_data_t *ld);
static void	unaliasThread(atom_t name);
static void	run_thread_exit_hooks();
static void	free_thread_info(PL_thread_info_t *info);
static void	set_system_thread_id(PL_thread_info_t *info);
static int	get_message_queue(term_t t, message_queue **queue,
				  int create);
static void	cleanupLocalDefinitions(PL_local_data_t *ld);
static int	unify_thread(term_t id, PL_thread_info_t *info);
static pl_mutex *mutexCreate(atom_t name);

#ifdef WIN32
static void	attachThreadWindow(PL_local_data_t *ld);
#endif

		 /*******************************
		 *	LOW-LEVEL UTILIIES	*
		 *******************************/

#ifdef HAVE_ASM_ATOMIC_H
#include <asm/atomic.h>
#endif

void
PL_atomic_inc(int *addr)
{
#ifdef HAVE_ATOMIC_INC		/* only if sizeof(int) == sizeof(atomic_t) */
  atomic_inc((atomic_t *)addr);
#else
#ifdef WIN32
  assert(sizeof(int) == sizeof(long));
  InterlockedIncrement((long *)addr);
#else
  PL_LOCK(L_MISC);
  (*addr)++;
  PL_UNLOCK(L_MISC);
#endif
#endif
}


void
PL_atomic_dec(int *addr)
{
#ifdef HAVE_ATOMIC_INC
  atomic_dec((atomic_t *)addr);
#else
#ifdef WIN32
  InterlockedDecrement((long *)addr);
#else
  PL_LOCK(L_MISC);
  (*addr)--;
  PL_UNLOCK(L_MISC);
#endif
#endif
}

#undef LOCK				/* clash with Linux asm/atomic.h */


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
    { term_t key = PL_new_term_ref();

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

  TLD_set(PL_ldata, info->thread_data);

  if ( !info->local_size    ) info->local_size    = GD->options.localSize;
  if ( !info->global_size   ) info->global_size   = GD->options.globalSize;
  if ( !info->trail_size    ) info->trail_size    = GD->options.trailSize;
  if ( !info->argument_size ) info->argument_size = GD->options.argumentSize;

  if ( !initPrologStacks(info->local_size,
			 info->global_size,
			 info->trail_size,
			 info->argument_size) )
    fail;

  initPrologLocalData();
#ifdef WIN32				/* For signals.  Do this always? */
  attachThreadWindow(info->thread_data);
#endif

  LOCK();
  GD->statistics.threads_created++;
  UNLOCK();

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
free_prolog_thread()
    Called from a cleanup-handler to release all resources associated
    with a thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
free_prolog_thread(void *data)
{ PL_local_data_t *ld = data;
  PL_thread_info_t *info;
  int acknowlege;

  if ( !threads_ready )
    return;				/* Post-mortem */

  info = ld->thread.info;
  if ( info->status == PL_THREAD_RUNNING )
    info->status = PL_THREAD_EXITED;	/* foreign pthread_exit() */
  acknowlege = (info->status == PL_THREAD_CANCELED);
  DEBUG(1, Sdprintf("Freeing prolog thread %d\n", info-threads));

#ifdef WIN32
  if ( ld->thread.hwnd )
  { HWND hwnd = ld->thread.hwnd;

    ld->thread.hwnd = NULL;
    DestroyWindow(hwnd);
  }
#endif

  run_thread_exit_hooks();
  
  DEBUG(2, Sdprintf("Destroying data\n"));
  freeStacks(ld);
  freeLocalData(ld);

  if ( ld->feature.table )
    destroyHTable(ld->feature.table);
  /*PL_unregister_atom(ld->prompt.current);*/

  freeThreadSignals(ld);
  cleanupLocalDefinitions(ld);

  LOCK();
  destroy_message_queue(&ld->thread.messages);
  GD->statistics.threads_finished++;
  GD->statistics.thread_cputime += CpuTime(CPU_USER);
  info->thread_data = NULL;
  UNLOCK();

  mergeAllocPool(&GD->alloc_pool, &ld->alloc_pool);
  freeHeap(ld, sizeof(*ld));

  if ( info->detached )
    free_thread_info(info);

  if ( acknowlege )
    sem_post(&sem_canceled);
}


void
initPrologThreads()
{ PL_thread_info_t *info;

  LOCK();
  if ( threads_ready )
  { UNLOCK();
    return;
  }

  TLD_alloc(&PL_ldata);			/* see also alloc_thread() */
  TLD_set(PL_ldata, &PL_local_data);
  info = &threads[1];
  info->tid = pthread_self();
  info->pl_tid = 1;
  info->thread_data = &PL_local_data;
  info->status = PL_THREAD_RUNNING;
  PL_local_data.thread.info = info;
  PL_local_data.thread.magic = PL_THREAD_MAGIC;
#ifdef WIN32
  info->w32id = GetCurrentThreadId();
  attachThreadWindow(info->thread_data);
#endif
  set_system_thread_id(info);

  GD->statistics.thread_cputime = 0.0;
  GD->statistics.threads_created = 1;
  GD->thread.mutexTable = newHTable(16);
  GD->thread.MUTEX_load = mutexCreate(ATOM_dload);
  threads_ready = TRUE;
  UNLOCK();
}


void
cleanupThreads()
{ TLD_free(PL_ldata);
  threadTable = NULL;
  queueTable = NULL;
  memset(&threads, 0, sizeof(threads));
  threads_ready = FALSE;
  queue_id = 0;
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

void
exitPrologThreads()
{ PL_thread_info_t *t;
  int i;
  int me = PL_thread_self();
  int canceled = 0;

  DEBUG(1, Sdprintf("exitPrologThreads(): me = %d\n", me));

  sem_init(&sem_canceled, USYNC_THREAD, 0);

  for(t=&threads[1], i=1; i<MAX_THREADS; i++, t++)
  { if ( t->thread_data && i != me )
    { switch(t->status)
      { case PL_THREAD_FAILED:
	case PL_THREAD_EXITED:
	case PL_THREAD_EXCEPTION:
	{ void *r;
	  int rc;

	  if ( (rc=pthread_join(t->tid, &r)) )
	    Sdprintf("Failed to join thread %d: %s\n", i, ThError(rc));

	  break;
	}
	case PL_THREAD_RUNNING:
	{ if ( t->cancel )
	  { if ( (*t->cancel)(i) == TRUE )
	      break;			/* done so */
	  }

#ifdef WIN32
  	  t->thread_data->exit_requested = TRUE;
	  t->thread_data->pending_signals |= (1L << (SIGINT-1));
	  PostThreadMessage(t->w32id, WM_QUIT, 0, 0);
	  DEBUG(1, Sdprintf("Cancelled %d\n", i));
	  canceled++;
#else
	{ int rc;

	  if ( (rc=pthread_cancel(t->tid)) == 0 )
	  { t->status = PL_THREAD_CANCELED;
	    canceled++;
	  } else
	  { Sdprintf("Failed to cancel thread %d: %s\n", i, ThError(rc));
	  }
	}
#endif
	  break;
	}
      }
    }
  }

  DEBUG(1, Sdprintf("Waiting for %d threads ...", canceled));
  for(i=canceled; i-- > 0;)
  { int maxwait = 10;

    while(maxwait--)
    { if ( sem_trywait(&sem_canceled) == 0 )
      { DEBUG(1, Sdprintf(" (ok)"));
	canceled--;
	break;
      }
      Pause(0.1);
    }
  }
  if ( canceled )
  { Sdprintf("%d threads wouldn't die\n", canceled);
  } else
  { DEBUG(1, Sdprintf("done\n"));
  }

  if ( canceled == 0 )			/* safe */
    sem_destroy(&sem_canceled);

  threads_ready = FALSE;
}


		 /*******************************
		 *	    ALIAS NAME		*
		 *******************************/

bool
aliasThread(int tid, atom_t name)
{ LOCK();
  if ( !threadTable )
    threadTable = newHTable(16);

  if ( (threadTable && lookupHTable(threadTable, (void *)name)) ||
       (queueTable  && lookupHTable(queueTable,  (void *)name)) )
  { term_t obj = PL_new_term_ref();

    UNLOCK();
    PL_put_atom(obj, name);
    return PL_error("thread_create", 1, "Alias name already taken",
		    ERR_PERMISSION, ATOM_thread, ATOM_create, obj);
  }

  addHTable(threadTable, (void *)name, (void *)tid);
  PL_register_atom(name);
  threads[tid].name = name;

  UNLOCK();

  succeed;
}

static void
unaliasThread(atom_t name)
{ if ( threadTable )
  { Symbol s;

    LOCK();
    if ( (s = lookupHTable(threadTable, (void *)name)) )
    { PL_unregister_atom(name);
      deleteSymbolHTable(threadTable, s);
    }
    UNLOCK();
  }
}

		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

static PL_thread_info_t *
alloc_thread()
{ int i;

  for(i=1; i<MAX_THREADS; i++)
  { if ( threads[i].status == PL_THREAD_UNUSED )
    { PL_local_data_t *ld = allocHeap(sizeof(PL_local_data_t));
      memset(ld, 0, sizeof(PL_local_data_t));

      threads[i].pl_tid = i;
      threads[i].thread_data = ld;
      threads[i].status = PL_THREAD_CREATED;
      ld->thread.info = &threads[i];
      ld->thread.magic = PL_THREAD_MAGIC;

      return &threads[i];
    }
  }

  return NULL;				/* out of threads */
}


int
PL_thread_self()
{ PL_local_data_t *ld = LD;

  if ( ld )
    return ld->thread.info->pl_tid;

  return -1;				/* thread has no Prolog thread */
}


#ifdef WIN32

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_w32thread_raise(DWORD id, int sig)
    Sets the signalled mask for a specific Win32 thread. This is a
    partial work-around for the lack of proper asynchronous signal
    handling in the Win32 platform.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_w32thread_raise(DWORD id, int sig)
{ PL_thread_info_t *info;
  int i;

  if ( sig < 0 || sig > MAXSIGNAL )
    return FALSE;			/* illegal signal */

  LOCK();
  for(i = 0, info = threads; i < MAX_THREADS; i++, info++)
  { if ( info->w32id == id && info->thread_data )
    { info->thread_data->pending_signals |= (1L << (sig-1));
      UNLOCK();
      return TRUE;
    }
  }
  UNLOCK();

  return FALSE;				/* can't find thread */
}

#endif /*WIN32*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_thread_raise() is used  for  re-routing   interrupts  in  the Windows
version, where the signal handler is running  from a different thread as
Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_thread_raise(int tid, int sig)
{ LOCK();
  if ( tid < 0 || tid >= MAX_THREADS ||
       threads[tid].status == PL_THREAD_UNUSED )
  { UNLOCK();
    return FALSE;
  }

  threads[tid].thread_data->pending_signals |= (1L << (sig-1));
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

  info = &threads[id];

  if ( info->name )
    return PL_atom_chars(info->name);

  sprintf(tmp, "%d", id);
  return buffer_string(tmp, BUF_RING);
}


static void
set_system_thread_id(PL_thread_info_t *info)
{ long id;

#ifdef __linux__
  id = info->pid = getpid();
#else
#ifdef WIN32
  id = info->w32id = GetCurrentThreadId();
#else
  id = (long)pthread_self();
#endif
#endif

  if ( GD->statistics.threads_created > 1 )
  { fid_t fid = PL_open_foreign_frame();
    term_t name = PL_new_term_ref();
    term_t value = PL_new_term_ref();

    PL_put_atom_chars(name, "system_thread_id");
    PL_put_integer(value, id);
    pl_set_feature(name, value);
    PL_discard_foreign_frame(fid);
  } else
  { PL_set_feature("system_thread_id", PL_INTEGER, id);
  }
}


static const opt_spec make_thread_options[] = 
{ { ATOM_local,		OPT_LONG },
  { ATOM_global,	OPT_LONG },
  { ATOM_trail,	        OPT_LONG },
  { ATOM_argument,	OPT_LONG },
  { ATOM_alias,		OPT_ATOM },
  { ATOM_detached,	OPT_BOOL },
  { ATOM_stack,		OPT_LONG },
  { NULL_ATOM,		0 }
};


static void *
start_thread(void *closure)
{ PL_thread_info_t *info = closure;
  term_t ex, goal;
  int rval;

  blockSignal(SIGINT);			/* only the main thread processes */
					/* Control-C */
  LOCK();
  info->status = PL_THREAD_RUNNING;
  UNLOCK();

  if ( !initialise_thread(info) )
  { info->status = PL_THREAD_NOMEM;
    return (void *)TRUE;
  }
    
  set_system_thread_id(info);

  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
  pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);

  pthread_cleanup_push(free_prolog_thread, info->thread_data);

  goal = PL_new_term_ref();
  PL_recorded(info->goal, goal);
  rval  = callProlog(info->module, goal, PL_Q_CATCH_EXCEPTION, &ex);

  LOCK();
  if ( rval )
  { info->status = PL_THREAD_SUCCEEDED;
  } else
  { if ( ex )
    { info->status = PL_THREAD_EXCEPTION;
      info->return_value = PL_record(ex);
    } else
    { info->status = PL_THREAD_FAILED;
    }
  }    
  UNLOCK();

  pthread_cleanup_pop(1);

  return (void *)TRUE;
}


word
pl_thread_create(term_t goal, term_t id, term_t options)
{ PL_thread_info_t *info;
  PL_local_data_t *ldnew;
  atom_t alias = NULL_ATOM;
  pthread_attr_t attr;
  long stack = 0;
  int rc;

  if ( !(PL_is_compound(goal) || PL_is_atom(goal)) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, goal);

  LOCK();

  if ( !GD->thread.enabled )
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

  if ( !scan_options(options, 0,
		     ATOM_thread_option, make_thread_options,
		     &info->local_size,
		     &info->global_size,
		     &info->trail_size,
		     &info->argument_size,
		     &alias,
		     &info->detached,
		     &stack) )
  { free_thread_info(info);
    fail;
  }

  info->local_size    *= 1024;
  info->global_size   *= 1024;
  info->trail_size    *= 1024;
  info->argument_size *= 1024;
  stack		      *= 1024;

  info->goal = PL_record(goal);
  info->module = PL_context();

  if ( alias )
  { if ( !aliasThread(info->pl_tid, alias) )
    { free_thread_info(info);
      fail;
    }
  }
					/* copy settings */

  PL_register_atom(LD->prompt.current);
  ldnew->prompt			 = LD->prompt;
  ldnew->modules		 = LD->modules;
  ldnew->IO			 = LD->IO;
  ldnew->_fileerrors		 = LD->_fileerrors;
  ldnew->float_format		 = LD->float_format;
  ldnew->_debugstatus		 = LD->_debugstatus;
  ldnew->_debugstatus.retryFrame = NULL;
  ldnew->feature.mask		 = LD->feature.mask;
  if ( LD->feature.table )
  { PL_LOCK(L_FEATURE);
    ldnew->feature.table	 = copyHTable(LD->feature.table);
    PL_UNLOCK(L_FEATURE);
  }
  init_message_queue(&info->thread_data->thread.messages);

  pthread_attr_init(&attr);
  if ( info->detached )
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  if ( stack )
    pthread_attr_setstacksize(&attr, stack);
  rc = pthread_create(&info->tid, &attr, start_thread, info);
  pthread_attr_destroy(&attr);
  if ( rc != 0 )
  { free_thread_info(info);
    return PL_error(NULL, 0, ThError(rc),
		    ERR_SYSCALL, "pthread_create");
  }

  return unify_thread(id, info);
}


static int
get_thread(term_t t, PL_thread_info_t **info, int warn)
{ int i = -1;

  if ( !PL_get_integer(t, &i) )
  { atom_t name;

    if ( !PL_get_atom(t, &name) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_thread, t);
    if ( threadTable )
    { Symbol s;

      if ( (s = lookupHTable(threadTable, (void *)name)) )
	i = (int)s->value;
    }
  }

  if ( i < 0 || i >= MAX_THREADS || threads[i].status == PL_THREAD_UNUSED )
  { if ( warn )
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_thread, t);
    else
      return FALSE;
  }
  
  *info = &threads[i];

  return TRUE;
}


static int
unify_thread(term_t id, PL_thread_info_t *info)
{ if ( info->name )
    return PL_unify_atom(id, info->name);

  return PL_unify_integer(id, info->pl_tid);
}


static int
unify_thread_status(term_t status, PL_thread_info_t *info)
{ switch(info->status)
  { case PL_THREAD_CREATED:
    case PL_THREAD_RUNNING:
      return PL_unify_atom(status, ATOM_running);
    case PL_THREAD_EXITED:
    { term_t tmp = PL_new_term_ref();

      if ( info->return_value )
	PL_recorded(info->return_value, tmp);

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

      PL_recorded(info->return_value, tmp);
      return PL_unify_term(status,
			   PL_FUNCTOR, FUNCTOR_exception1,
			     PL_TERM, tmp);
    }
    case PL_THREAD_NOMEM:
    { return PL_unify_term(status,
			   PL_FUNCTOR, FUNCTOR_exception1,
			     PL_FUNCTOR, FUNCTOR_error2,
			       PL_FUNCTOR, FUNCTOR_resource_error1,
			         PL_CHARS, "virtual_memory",
			       PL_VARIABLE);
    }
    case PL_THREAD_CANCELED:
      return PL_unify_atom(status, ATOM_canceled);
    default:
      fatalError("info->status = %d", info->status);
      assert(0);
      fail;
  }
}


word
pl_thread_self(term_t self)
{ return unify_thread(self, LD->thread.info);
}


static void
free_thread_info(PL_thread_info_t *info)
{ if ( info->thread_data )
    free_prolog_thread(info->thread_data);
  if ( info->return_value )
    PL_erase(info->return_value);
  if ( info->goal )
    PL_erase(info->goal);

  if ( info->name )
    unaliasThread(info->name);

  memset(info, 0, sizeof(*info));	/* sets status to PL_THREAD_UNUSED */
}


word
pl_thread_join(term_t thread, term_t retcode)
{ PL_thread_info_t *info;
  void *r;
  word rval;
  int rc;

  if ( !get_thread(thread, &info, TRUE) )
    fail;
  if ( info == LD->thread.info || info->detached )
  { return PL_error("thread_join", 2,
		    info->detached ? "Cannot join detached thread"
				   : "Cannot join self",
		    ERR_PERMISSION, ATOM_join, ATOM_thread, thread);
  }

  while( (rc=pthread_join(info->tid, &r)) == EINTR )
    ;
  if ( rc != 0 )
    return PL_error("thread_join", 2, ThError(rc),
		    ERR_SYSCALL, "pthread_join");
  
  rval = unify_thread_status(retcode, info);
   
  free_thread_info(info);

  return rval;
}


word
pl_thread_exit(term_t retcode)
{ PL_thread_info_t *info = LD->thread.info;

  LOCK();
  if ( LD->exit_requested )
    info->status = PL_THREAD_CANCELED;
  else
    info->status = PL_THREAD_EXITED;
  info->return_value = PL_record(retcode);
  UNLOCK();

  DEBUG(1, Sdprintf("thread_exit(%d)\n", info-threads));

  pthread_exit(NULL);
  assert(0);
  fail;
}


word
pl_thread_kill(term_t t, term_t sig)
{
#ifdef HAVE_PTHREAD_KILL
  PL_thread_info_t *info;
  int s, rc;

  
  if ( !get_thread(t, &info, TRUE) )
    fail;
  if ( !_PL_get_signum(sig, &s) )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal, sig);

  if ( (rc=pthread_kill(info->tid, s)) )
  { assert(rc == ESRCH);

    return PL_error("thread_kill", 2, NULL, ERR_EXISTENCE, ATOM_thread, t);
  }

  succeed;
#else
  return notImplemented("thread_kill", 2);
#endif
}


static
PRED_IMPL("thread_detach", 1, thread_detach, 0)
{ PL_thread_info_t *info;

  LOCK();
  if ( !get_thread(A1, &info, TRUE) )
  { UNLOCK();
    fail;
  }

  if ( !info->detached )
  { int rc;

    if ( (rc=pthread_detach(info->tid)) )
    { assert(rc == ESRCH);

      free_thread_info(info);
    } else
      info->detached = TRUE;
  }

  UNLOCK();
  succeed;
}


word
pl_current_thread(term_t id, term_t status, control_t h)
{ int current;

  switch(ForeignControl(h))
  { case FRG_FIRST_CALL:
    { PL_thread_info_t *info;

      if ( PL_is_variable(id) )
      { current = 1;
	goto redo;
      }
      if ( !get_thread(id, &info, FALSE) )
        fail;

      return unify_thread_status(status, info);
    }
    case FRG_REDO:
      current = ForeignContextInt(h);
    redo:
      for( ; current < MAX_THREADS; current++ )
      { mark m;

	if ( !threads[current].tid )
	   continue;

	Mark(m);
	if ( unify_thread(id, &threads[current]) &&
	     unify_thread_status(status, &threads[current]) )
	  ForeignRedoInt(current+1);
	Undo(m);
      }
      fail;
    case FRG_CUTTED:
    default:
      succeed;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sum the amount of heap allocated through all threads allocation-pools.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

long
threadLocalHeapUsed(void)
{ int i;
  PL_thread_info_t *info;
  long heap = 0;

  LOCK();
  for(i=0, info=threads; i<MAX_THREADS; i++, info++)
  { PL_local_data_t *ld;

    if ( (ld = info->thread_data) )
    { heap += ld->alloc_pool.allocated;
    }
  }
  UNLOCK();

  return heap;
}


static
PRED_IMPL("thread_setconcurrency", 2, thread_setconcurrency, 0)
{
#ifdef HAVE_PTHREAD_GETCONCURRENCY
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


foreign_t
pl_thread_at_exit(term_t goal)
{ Module m = NULL;
  at_exit_goal *eg = allocHeap(sizeof(*eg));

  PL_strip_module(goal, &m, goal);
  eg->next = NULL;
  eg->type = EXIT_PROLOG;
  eg->goal.prolog.module = m;
  eg->goal.prolog.goal   = PL_record(goal);

  eg->next = LD->thread.exit_goals;
  LD->thread.exit_goals = eg;

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Request a function to run when the Prolog thread is about to detach, but
still capable of running Prolog queries.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_thread_at_exit(void (*function)(void *), void *closure, int global)
{ at_exit_goal *eg = allocHeap(sizeof(*eg));

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

static void
run_exit_hooks(at_exit_goal *eg, int free)
{ at_exit_goal *next;
  term_t goal = PL_new_term_ref();
  fid_t fid = PL_open_foreign_frame();

  for( ; eg; eg = next)
  { next = eg->next;
  
    switch(eg->type)
    { case EXIT_PROLOG:
	PL_recorded(eg->goal.prolog.goal, goal);
        if ( free )
	  PL_erase(eg->goal.prolog.goal);
	callProlog(eg->goal.prolog.module, goal, PL_Q_NODEBUG, NULL);
	PL_rewind_foreign_frame(fid);
	break;
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
  resetTermRefs(goal);
}



static void
run_thread_exit_hooks()
{ at_exit_goal *eg;

  while( (eg = LD->thread.exit_goals) )
  { LD->thread.exit_goals = NULL;	/* empty these */

    run_exit_hooks(eg, TRUE);
  }

  run_exit_hooks(GD->thread.exit_goals, FALSE);
}


		 /*******************************
		 *	   THREAD SIGNALS	*
		 *******************************/

#ifdef WIN32
#define WM_SIGNALLED (WM_USER+1)
#define WM_MENU	     (WM_USER+2)

static WINAPI
thread_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ switch(message)
  { case WM_SIGNALLED:
      PL_handle_signals();
      return 0;
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}


static char *
ThreadWindowClass()
{ static char winclassname[40];
  static WNDCLASS wndClass;

  if ( !winclassname[0] )
  { sprintf(winclassname,
	    "SWI-Prolog-thread-window-%d",
	    GD->thread.instance);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID)thread_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= GD->thread.instance;
    wndClass.hIcon		= NULL;
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= winclassname;

    RegisterClass(&wndClass);
  }

  return winclassname;
}

static void
attachThreadWindow(PL_local_data_t *ld)
{ HWND hwnd;

  hwnd = CreateWindow(ThreadWindowClass(),
		      "SWI-Prolog thread window",
		      0,
		      0, 0, 32, 32,
		      NULL, NULL, GD->thread.instance, NULL);

  ld->thread.hwnd = hwnd;
}

#endif /*WIN32*/

typedef struct _thread_sig
{ struct _thread_sig *next;		/* Next in queue */
  Module   module;			/* Module for running goal */
  record_t goal;			/* Goal to run */
} thread_sig;


foreign_t
pl_thread_signal(term_t thread, term_t goal)
{ Module m = NULL;
  thread_sig *sg;
  PL_thread_info_t *info;
  PL_local_data_t *ld;

  if ( !get_thread(thread, &info, TRUE) )
    fail;

  PL_strip_module(goal, &m, goal);
  sg = allocHeap(sizeof(*sg));
  sg->next = NULL;
  sg->module = m;
  sg->goal = PL_record(goal);

  LOCK();
  ld = info->thread_data;
  if ( !ld->thread.sig_head )
    ld->thread.sig_head = ld->thread.sig_tail = sg;
  else
  { ld->thread.sig_tail->next = sg;
    ld->thread.sig_tail = sg;
  }
  ld->pending_signals |= (1L << (SIG_THREAD_SIGNAL-1));

#ifdef WIN32
  if ( ld->thread.hwnd )
    PostMessage(ld->thread.hwnd, WM_SIGNALLED, 0, 0L);
#endif

  UNLOCK();

  succeed;
}


void
executeThreadSignals(int sig)
{ thread_sig *sg, *next;
  fid_t fid = PL_open_foreign_frame();
  term_t goal = PL_new_term_ref();

  LOCK();
  sg = LD->thread.sig_head;
  LD->thread.sig_head = LD->thread.sig_tail = NULL;
  UNLOCK();

  for( ; sg; sg = next)
  { term_t ex;
    int rval;
  
    next = sg->next;
    PL_recorded(sg->goal, goal);
    PL_erase(sg->goal);
    rval = callProlog(sg->module, goal, PL_Q_CATCH_EXCEPTION, &ex);
    freeHeap(sg, sizeof(*sg));

    if ( !rval && ex )
    { PL_close_foreign_frame(fid);
      PL_raise_exception(ex);

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

typedef struct _thread_msg
{ struct _thread_msg *next;		/* next in queue */
  record_t            message;		/* message in queue */
  word		      key;		/* Indexing key */
} thread_message;


static void
queue_message(message_queue *queue, term_t msg)
{ thread_message *msgp;

  msgp = allocHeap(sizeof(*msgp));
  msgp->next    = NULL;
  msgp->message = PL_record(msg);
  msgp->key     = getIndexOfTerm(msg);
  
  pthread_mutex_lock(&queue->mutex);

  if ( !queue->head )
  { queue->head = queue->tail = msgp;
  } else
  { queue->tail->next = msgp;
    queue->tail = msgp;
  }

  if ( queue->waiting )
  { if ( queue->waiting > queue->waiting_var && queue->waiting > 1 )
    { DEBUG(1, Sdprintf("%d of %d non-var waiters; broadcasting\n",
			queue->waiting - queue->waiting_var,
		        queue->waiting));
      pthread_cond_broadcast(&queue->cond_var);
    } else
    { DEBUG(1, Sdprintf("%d var waiters; signalling\n", queue->waiting));
      pthread_cond_signal(&queue->cond_var);
    }
  } else
  { DEBUG(1, Sdprintf("No waiters\n"));
  }

  pthread_mutex_unlock(&queue->mutex);
}


typedef struct
{ message_queue *queue;
  int            isvar;
} get_msg_cleanup_context;


static void
cleanup_get_message(void *context)
{ get_msg_cleanup_context *ctx = context;

  ctx->queue->waiting--;
  ctx->queue->waiting_var -= ctx->isvar;
  pthread_mutex_unlock(&ctx->queue->mutex);
}


static int
get_message(message_queue *queue, term_t msg)
{ get_msg_cleanup_context ctx;
  term_t tmp = PL_new_term_ref();
  int isvar = PL_is_variable(msg) ? 1 : 0;
  word key = (isvar ? 0L : getIndexOfTerm(msg));
  mark m;

  Mark(m);

  ctx.queue = queue;
  ctx.isvar = isvar;
  pthread_cleanup_push(cleanup_get_message, (void *)&ctx);
  pthread_mutex_lock(&queue->mutex);

  for(;;)
  { thread_message *msgp = queue->head;
    thread_message *prev = NULL;

    DEBUG(1, Sdprintf("%d: scanning queue\n", PL_thread_self()));
    for( ; msgp; prev = msgp, msgp = msgp->next )
    { if ( key && msgp->key && key != msgp->key )
	continue;			/* fast search */

      PL_recorded(msgp->message, tmp);

      if ( PL_unify(msg, tmp) )
      { if ( prev )
	{ if ( !(prev->next = msgp->next) )
	    queue->tail = prev;
	} else
	{ if ( !(queue->head = msgp->next) )
	    queue->tail = NULL;
	}
	PL_erase(msgp->message);
	freeHeap(msgp, sizeof(*msgp));
	goto out;
      }

      Undo(m);			/* reclaim term */
    }
				/* linux docs say it may return EINTR */
				/* does it re-lock in that case? */
    queue->waiting++;
    queue->waiting_var += isvar;
    DEBUG(1, Sdprintf("%d: waiting on queue\n", PL_thread_self()));
    while( pthread_cond_wait(&queue->cond_var, &queue->mutex) == EINTR )
      ;
    DEBUG(1, Sdprintf("%d: wakeup on queue\n", PL_thread_self()));
    queue->waiting--;
    queue->waiting_var -= isvar;
  }
out:

  pthread_mutex_unlock(&queue->mutex);
  pthread_cleanup_pop(0);
  succeed;
}


static int
peek_message(message_queue *queue, term_t msg)
{ thread_message *msgp;
  term_t tmp = PL_new_term_ref();
  word key = getIndexOfTerm(msg);
  mark m;

  Mark(m);

  pthread_mutex_lock(&queue->mutex);
  msgp = queue->head;

  for( msgp = queue->head; msgp; msgp = msgp->next )
  { if ( key && msgp->key && key != msgp->key )
      continue;
    PL_recorded(msgp->message, tmp);

    if ( PL_unify(msg, tmp) )
    { pthread_mutex_unlock(&queue->mutex);
      succeed;
    }

    Undo(m);
  }
     
  pthread_mutex_unlock(&queue->mutex);
  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deletes the contents of the message-queue as well as the queue itself.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
destroy_message_queue(message_queue *queue)
{ thread_message *msgp;
  thread_message *next;

  for( msgp = queue->head; msgp; msgp = next )
  { next = msgp->next;

    PL_erase(msgp->message);
    freeHeap(msgp, sizeof(*msgp));
  }
  
  pthread_mutex_destroy(&queue->mutex);
  pthread_cond_destroy(&queue->cond_var);
}


static void
init_message_queue(message_queue *queue)
{ memset(queue, 0, sizeof(*queue));
  pthread_mutex_init(&queue->mutex, NULL);
  pthread_cond_init(&queue->cond_var, NULL);
}


					/* Prolog predicates */

word
pl_thread_send_message(term_t queue, term_t msg)
{ message_queue *q;

  LOCK();
  if ( !get_message_queue(queue, &q, TRUE) )
  { UNLOCK();
    fail;
  }

  queue_message(q, msg);
  UNLOCK();

  succeed;
}


foreign_t
pl_thread_get_message(term_t msg)
{ return get_message(&LD->thread.messages, msg);
}


word
pl_thread_peek_message(term_t msg)
{ return peek_message(&LD->thread.messages, msg);
}


		 /*******************************
		 *     USER MESSAGE QUEUES	*
		 *******************************/

static int
unify_queue(term_t t, message_queue *q)
{ if ( isAtom(q->id) )
    return PL_unify_atom(t, q->id);
  else
    return PL_unify_term(t,
			 PL_FUNCTOR, FUNCTOR_dmessage_queue1,
			 PL_INTEGER, valInt(q->id));
}


static message_queue *
unlocked_message_queue_create(term_t queue)
{ Symbol s;
  atom_t name = NULL_ATOM;
  message_queue *q;
  word id;

  if ( !queueTable )
    queueTable = newHTable(16);
  
  if ( PL_get_atom(queue, &name) )
  { if ( (s = lookupHTable(queueTable, (void *)name)) ||
	 (s = lookupHTable(threadTable, (void *)name)) )
    { PL_error("message_queue_create", 1, NULL, ERR_PERMISSION,
	       ATOM_message_queue, ATOM_create, queue);
      return NULL;
    }
    id = name;
  } else if ( PL_is_variable(queue) )
  { id = consInt(queue_id++);
  } else
  { PL_error("message_queue_create", 1, NULL,
	     ERR_TYPE, ATOM_message_queue, queue);
    return NULL;
  }

  q = alignedAllocHeap(sizeof(*q));
  init_message_queue(q);
  q->id    = id;
  addHTable(queueTable, (void *)id, q);

  if ( unify_queue(queue, q) )
    return q;

  return NULL;
}


/* MT: Caller must hold the L_THREAD mutex
*/

static int
get_message_queue(term_t t, message_queue **queue, int create)
{ atom_t name;
  word id = 0;
  int tid;

  if ( PL_get_atom(t, &name) )
  { id = name;
  } else if ( PL_is_functor(t, FUNCTOR_dmessage_queue1) )
  { term_t a = PL_new_term_ref();
    long i;

    PL_get_arg(1, t, a);
    if ( PL_get_long(a, &i) )
      id = consInt(i);
  } else if ( PL_get_integer(t, &tid) )
  { thread_queue:
    if ( tid < 0 || tid >= MAX_THREADS ||
	 threads[tid].status == PL_THREAD_UNUSED )
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_thread, t);

    *queue = &threads[tid].thread_data->thread.messages;
    return TRUE;
  }
  if ( !id )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_message_queue, t);

  if ( queueTable )
  { Symbol s = lookupHTable(queueTable, (void *)id);

    if ( s )
    { *queue = s->value;
      return TRUE;
    }
  }
  if ( threadTable )
  { Symbol s = lookupHTable(threadTable, (void *)id);

    if ( s )
    { tid = (int)s->value;
      goto thread_queue;
    }
  }

  if ( create && isAtom(id) )
  { message_queue *new;

    if ( (new = unlocked_message_queue_create(t)) )
    { *queue = new;
      return TRUE;
    }
  }

  return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_message_queue, t);
}


static
PRED_IMPL("message_queue_create", 1, message_queue_create, 0)
{ int rval;

  LOCK();
  rval = (unlocked_message_queue_create(A1) ? TRUE : FALSE);
  UNLOCK();

  return rval;
}


static
PRED_IMPL("message_queue_destroy", 1, message_queue_destroy, 0)
{ message_queue *q;
  Symbol s;

  LOCK();
  if ( !get_message_queue(A1, &q, FALSE) )
  { UNLOCK();
    fail;
  }

  destroy_message_queue(q);
  s = lookupHTable(queueTable, (void *)q->id);
  deleteSymbolHTable(queueTable, s);
  freeHeap(q, sizeof(*q));
  UNLOCK();

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
thread_get_message(+Queue, -Message)
thread_get_message(-Message)
    Get a message from a message queue. If the queue is not provided get
    a message from the queue implicitly associated to the thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("thread_get_message", 2, thread_get_message, 0)
{ message_queue *q;

  if ( !get_message_queue(A1, &q, TRUE) )
    fail;
  
  return get_message(q, A2);
}


static
PRED_IMPL("thread_peek_message", 2, thread_peek_message, 0)
{ message_queue *q;

  if ( !get_message_queue(A1, &q, TRUE) )
    fail;
  
  return peek_message(q, A2);
}


		 /*******************************
		 *	 MUTEX PRIMITIVES	*
		 *******************************/

#ifdef NEED_RECURSIVE_MUTEX_INIT

int
recursiveMutexInit(recursiveMutex *m)
{ 
#ifdef RECURSIVE_MUTEXES
  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
#ifdef HAVE_PTHREAD_MUTEXATTR_SETTYPE
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
#else
#ifdef HAVE_PTHREAD_MUTEXATTR_SETKIND_NP
  pthread_mutexattr_setkind_np(&attr, PTHREAD_MUTEX_RECURSIVE_NP);
#endif
#endif
  pthread_mutex_init(m, &attr);
#else /*RECURSIVE_MUTEXES*/
  m->owner = 0;
  m->count = 0;
  pthread_mutex_init(&(m->lock),NULL);
#endif /* RECURSIVE_MUTEXES */

  return 0;
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

		 /*******************************
		 *	    USER MUTEXES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
User-level mutexes. On Windows we can't   use  critical sections here as
TryEnterCriticalSection() is only defined on NT 4, not on Windows 95 and
friends.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unify_mutex(term_t t, pl_mutex *m)
{ if ( isAtom(m->id) )
    return PL_unify_atom(t, m->id);
  else
    return PL_unify_term(t,
			 PL_FUNCTOR, FUNCTOR_dmutex1,
			 PL_INTEGER, valInt(m->id));
}


static int
unify_mutex_owner(term_t t, int owner)
{ if ( owner )
    return unify_thread(t, &threads[owner]);
  else
    return PL_unify_nil(t);
}


static pl_mutex *
mutexCreate(atom_t name)
{ pl_mutex *m;

  m = allocHeap(sizeof(*m));
  pthread_mutex_init(&m->mutex, NULL);
  m->count = 0;
  m->owner = 0;
  m->id    = name;
  addHTable(GD->thread.mutexTable, (void *)name, m);

  return m;
}


static pl_mutex *
unlocked_pl_mutex_create(term_t mutex)
{ Symbol s;
  atom_t name = NULL_ATOM;
  pl_mutex *m;
  word id;

  if ( PL_get_atom(mutex, &name) )
  { if ( (s = lookupHTable(GD->thread.mutexTable, (void *)name)) )
    { PL_error("mutex_create", 1, NULL, ERR_PERMISSION,
	       ATOM_mutex, ATOM_create, mutex);
      return NULL;
    }
    id = name;
  } else if ( PL_is_variable(mutex) )
  { id = consInt(GD->thread.mutex_next_id++);
  } else
  { PL_error("mutex_create", 1, NULL, ERR_TYPE, ATOM_mutex, mutex);
    return NULL;
  }

  m = mutexCreate(id);

  if ( unify_mutex(mutex, m) )
    return m;

  return NULL;
}


foreign_t
pl_mutex_create(term_t mutex)
{ int rval;

  LOCK();
  rval = (unlocked_pl_mutex_create(mutex) ? TRUE : FALSE);
  UNLOCK();

  return rval;
}


static int
get_mutex(term_t t, pl_mutex **mutex, int create)
{ atom_t name;
  word id = 0;

  if ( PL_get_atom(t, &name) )
  { id = name;
  } else if ( PL_is_functor(t, FUNCTOR_dmutex1) )
  { term_t a = PL_new_term_ref();
    long i;

    PL_get_arg(1, t, a);
    if ( PL_get_long(a, &i) )
      id = consInt(i);
  }
  if ( !id )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_mutex, t);

  LOCK();
  if ( GD->thread.mutexTable )
  { Symbol s = lookupHTable(GD->thread.mutexTable, (void *)id);

    if ( s )
    { *mutex = s->value;
      UNLOCK();
      return TRUE;
    }
  }

  if ( create && isAtom(id) )
  { pl_mutex *new;

    if ( (new = unlocked_pl_mutex_create(t)) )
    { *mutex = new;
      UNLOCK();
      return TRUE;
    }
  }
  UNLOCK();

  return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_mutex, t);
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

  succeed;
}


foreign_t
pl_mutex_lock(term_t mutex)
{ pl_mutex *m;

  if ( !get_mutex(mutex, &m, TRUE) )
    fail;

  return  PL_mutex_lock(m);
}


foreign_t
pl_mutex_trylock(term_t mutex)
{ pl_mutex *m;
  int self = PL_thread_self();
  int rval;

  if ( !get_mutex(mutex, &m, TRUE) )
    fail;

  if ( self == m->owner )
  { m->count++;
  } else if ( MUTEX_OK(rval = pthread_mutex_trylock(&m->mutex)) )
  { m->count = 1;
    m->owner = self;
  } else if ( rval == MUTEX_BUSY )
  { fail;
  } else
  { assert(0);
  }

  succeed;
}


int
PL_mutex_unlock(struct pl_mutex *m)
{ int self = PL_thread_self();

  if ( self == m->owner )
  { if ( --m->count == 0 )
    { m->owner = 0;

      pthread_mutex_unlock(&m->mutex);
    }

    succeed;
  }

  fail;
}


foreign_t
pl_mutex_unlock(term_t mutex)
{ pl_mutex *m;

  if ( !get_mutex(mutex, &m, FALSE) )
    fail;

  if ( !PL_mutex_unlock(m) )
    return PL_error("mutex_unlock", 1, MSG_ERRNO, ERR_PERMISSION,
		    ATOM_mutex, ATOM_unlock, mutex);

  succeed;
}


foreign_t
pl_mutex_unlock_all()
{ TableEnum e;
  Symbol s;
  int tid = PL_thread_self();

  e = newTableEnum(GD->thread.mutexTable);
  while( (s = advanceTableEnum(e)) )
  { pl_mutex *m = s->value;
    
    if ( m->owner == tid )
    { m->count = 0;
      m->owner = 0;
      pthread_mutex_unlock(&m->mutex);
    }
  }
  freeTableEnum(e);
  succeed;
}


foreign_t
pl_mutex_destroy(term_t mutex)
{ pl_mutex *m;
  Symbol s;

  LOCK();
  if ( !get_mutex(mutex, &m, FALSE) )
    fail;

  if ( m->owner )
  { char msg[100];
    
    UNLOCK();
    Ssprintf(msg, "Owned by thread %d", m->owner); /* TBD: named threads */
    return PL_error("mutex_destroy", 1, msg,
		    ERR_PERMISSION, ATOM_mutex, ATOM_destroy, mutex);
  }

  pthread_mutex_destroy(&m->mutex);
  s = lookupHTable(GD->thread.mutexTable, (void *)m->id);
  deleteSymbolHTable(GD->thread.mutexTable, s);
  freeHeap(m, sizeof(*m));
  UNLOCK();

  succeed;
}


foreign_t
pl_current_mutex(term_t mutex, term_t owner, term_t count, control_t h)
{ TableEnum e;
  Symbol s;
  mark mrk;

  switch(ForeignControl(h))
  { case FRG_FIRST_CALL:
    { if ( PL_is_variable(mutex) )
      { e = newTableEnum(GD->thread.mutexTable);
      } else
      { pl_mutex *m;

        if ( get_mutex(mutex, &m, FALSE) &&
	     unify_mutex_owner(owner, m->owner) &&
	     PL_unify_integer(count, m->count) )
	  succeed;

	fail;
      }
      break;
    }
    case FRG_REDO:
      e = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
      e = ForeignContextPtr(h);
      freeTableEnum(e);
    default:
      succeed;
  }

  Mark(mrk);
  while ( (s = advanceTableEnum(e)) )
  { pl_mutex *m = s->value;

    if ( unify_mutex(mutex, m) &&
	 unify_mutex_owner(owner, m->owner) &&
	 PL_unify_integer(count, m->count) )
    { ForeignRedoPtr(e);
    }
    Undo(mrk);
  }

  freeTableEnum(e);
  fail;
}


		 /*******************************
		 *	FOREIGN INTERFACE	*
		 *******************************/

int
PL_thread_attach_engine(PL_thread_attr_t *attr)
{ PL_thread_info_t *info;
  PL_local_data_t *ldnew;
  PL_local_data_t *ldmain;

  if ( LD )
    LD->thread.info->open_count++;

  LOCK();
  if ( !GD->thread.enabled )
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

  ldmain = threads[1].thread_data;
  ldnew = info->thread_data;

  if ( attr )
  { if ( attr->local_size )
      info->local_size = attr->local_size * 1024;
    if ( attr->global_size )
      info->global_size = attr->global_size * 1024;
    if ( attr->trail_size )
      info->trail_size = attr->trail_size * 1024;
    if ( attr->argument_size )
      info->argument_size = attr->argument_size * 1024;

    info->cancel = attr->cancel;
  }
  
  info->goal       = NULL;
  info->module     = MODULE_user;
  info->detached   = TRUE;		/* C-side should join me */
  info->status     = PL_THREAD_RUNNING;
  info->open_count = 1;
  init_message_queue(&info->thread_data->thread.messages);

  ldnew->prompt			 = ldmain->prompt;
  ldnew->modules		 = ldmain->modules;
  ldnew->IO			 = ldmain->IO;
  ldnew->_fileerrors		 = ldmain->_fileerrors;
  ldnew->float_format		 = ldmain->float_format;
  ldnew->_debugstatus		 = ldmain->_debugstatus;
  ldnew->_debugstatus.retryFrame = NULL;
  ldnew->feature.mask		 = ldmain->feature.mask;
  if ( ldmain->feature.table )
  { PL_LOCK(L_FEATURE);
    ldnew->feature.table	 = copyHTable(ldmain->feature.table);
    PL_UNLOCK(L_FEATURE);
  }

  if ( !initialise_thread(info) )
  { free_thread_info(info);
    errno = ENOMEM;
    return -1;
  }
  info->tid = pthread_self();		/* we are complete now */
  set_system_thread_id(info);
  if ( attr && attr->alias )
  { if ( !aliasThread(info->pl_tid, PL_new_atom(attr->alias)) )
    { free_thread_info(info);
      errno = EPERM;
      return -1;
    }
  }

  return info->pl_tid;
}


int
PL_thread_destroy_engine()
{ PL_local_data_t *ld = LD;

  if ( ld )
  { if ( --ld->thread.info->open_count == 0 )
    { free_prolog_thread(ld);
      TLD_set(PL_ldata, NULL);
    }

    return TRUE;
  }

  return FALSE;				/* we had no thread */
}


int
attachConsole()
{ fid_t fid = PL_open_foreign_frame();
  int rval;
  predicate_t pred = PL_predicate("attach_console", 0, "user");

  rval = PL_call_predicate(NULL, PL_Q_NODEBUG, pred, 0);

  PL_discard_foreign_frame(fid);

  return rval;
}



		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

static void	sync_statistics(PL_thread_info_t *info, atom_t key);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
thread_statistics(+Thread, +Key, -Value)
    Same as statistics(+Key, -Value) but operates on another thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("thread_statistics", 3, thread_statistics, 0)
{ PL_thread_info_t *info;
  int rval;

  LOCK();
  if ( !get_thread(A1, &info, TRUE) )
  { UNLOCK();
    fail;
  }

  if ( !info->thread_data )
  { UNLOCK();
    return PL_error(NULL, 0, NULL,
		    ERR_PERMISSION,
		    ATOM_statistics, ATOM_thread, A1);
  }

  if ( LD != info->thread_data )
  { atom_t k;

    if ( !PL_get_atom(A2, &k) )
      k = 0;

    sync_statistics(info, k);
  }

  rval = pl_statistics_ld(A2, A3, info->thread_data);
  UNLOCK();

  return rval;
}


#ifdef WIN32

/* How to make the memory visible?
*/

					/* see also pl-nt.c */
#define nano * 0.0000001
#define ntick 1.0			/* manual says 100.0 ??? */

static void
sync_statistics(PL_thread_info_t *info, atom_t key)
{ if ( key == ATOM_cputime || key == ATOM_runtime || key == ATOM_system_time )
  { double t;
    FILETIME created, exited, kerneltime, usertime;
    HANDLE win_thread = pthread_getw32threadhandle_np(info->tid);

    if ( GetThreadTimes(win_thread,
			&created, &exited, &kerneltime, &usertime) )
    { FILETIME *p;

      if ( key == ATOM_system_time )
	p = &kerneltime;
      else
	p = &usertime;

      t = (double)p->dwHighDateTime * (4294967296.0 * ntick nano);
      t += (double)p->dwLowDateTime  * (ntick nano);

      if ( key == ATOM_system_time )
	info->thread_data->statistics.system_cputime = t;
      else
	info->thread_data->statistics.user_cputime = t;
    }
  }
}

#else /*WIN32*/

static void
SyncUserCPU(int sig)
{ LD->statistics.user_cputime = CpuTime(CPU_USER);
  sem_post(&sem_mark);
}


static void
SyncSystemCPU(int sig)
{ LD->statistics.system_cputime = CpuTime(CPU_SYSTEM);
  sem_post(&sem_mark);
}


static void
sync_statistics(PL_thread_info_t *info, atom_t key)
{ if ( key == ATOM_cputime || key == ATOM_runtime || key == ATOM_system_time )
  { struct sigaction old;
    struct sigaction new;

    sem_init(&sem_mark, USYNC_THREAD, 0);
    memset(&new, 0, sizeof(new));
    if ( key == ATOM_cputime || key == ATOM_runtime )
      new.sa_handler = SyncUserCPU;
    else /*if ( key == ATOM_system_time )*/
      new.sa_handler = SyncSystemCPU;

    new.sa_flags   = SA_RESTART;
    sigaction(SIG_FORALL, &new, &old);
    if ( pthread_kill(info->tid, SIG_FORALL) == 0 )
    { sem_wait(&sem_mark);
    }
    sem_destroy(&sem_mark);
    sigaction(SIG_FORALL, &old, NULL);
  }
}

#endif /*WIN32*/


		 /*******************************
		 *	      ATOM-GC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  is  hairy  and  hard-to-port   code.    The   job   of  the  entry
threadMarkAtomsOtherThreads() is to mark the   atoms referenced from all
other threads. This function is  called from pl_garbage_collect_atoms(),
which already has locked the L_ATOM and L_THREAD mutexes.

We set up a semaphore and  signal   all  the  other threads. Each thread
receiving a the SIG_MARKATOMS signal calls markAtomsOnStacks() and posts
the semaphore. The latter performs its  job with certain heuristics, but
must ensure it doesn't  forget  any  atoms   (a  few  too  many  is ok).
Basically this signal handler can run whenever  necessary, as long as as
the thread is not in a GC,  which   makes  it impossible to traverse the
stacks.

Special attention is required  for   stack-creation  and destruction. We
should not be missing threads that  are   about  to be created or signal
them when they have just died. We   do this by locking the status-change
with the L_THREAD mutex, which is held by the atom-garbage collector, so
each starting thread will hold  until   collection  is complete and each
terminating one will live a bit longer until atom-GC is complete.

After a thread is done marking its atom  is just continues. This is safe
as it may stop referencing atoms but   this  doesn't matter. It can only
refer to new atoms by creating them, in which case the thread will block
or by executing an instruction that refers to the atom. In this case the
atom is locked by the instruction anyway.

[WIN32]  The  windows  case  is  entirely    different  as  we  have  no
asynchronous signals. Fortunately we  can   suspend  and resume threads.
This makes the code a lot easier as   you can see below. Problem is that
only one processor is doing the  job,   where  atom-gc  is a distributed
activity in the POSIX based code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32

void					/* For comments, see above */
forThreadLocalData(void (*func)(PL_local_data_t *), unsigned flags)
{ int i;
  int me = PL_thread_self();

  for(i=0; i<MAX_THREADS; i++)
  { if ( threads[i].thread_data && i != me &&
	 threads[i].status == PL_THREAD_RUNNING )
    { HANDLE win_thread = pthread_getw32threadhandle_np(threads[i].tid);

      if ( SuspendThread(win_thread) != -1L )
      { (*func)(threads[i].thread_data);
        if ( (flags & PL_THREAD_SUSPEND_AFTER_WORK) )
	  threads[i].status = PL_THREAD_SUSPENDED;
	else
	  ResumeThread(win_thread);
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resume all suspended threads.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
resumeThreads(void)
{ int i;
  int me = PL_thread_self();

  for(i=0; i<MAX_THREADS; i++)
  { if ( threads[i].thread_data && i != me &&
	 threads[i].status == PL_THREAD_SUSPENDED )
    { HANDLE win_thread = pthread_getw32threadhandle_np(threads[i].tid);

      ResumeThread(win_thread);
      threads[i].status = PL_THREAD_RUNNING;
    }
  }
}

#else /*WIN32*/

static void (*ldata_function)(PL_local_data_t *data);

static void
wait_resume(PL_thread_info_t *t)
{ sigset_t signal_set;

  sigfillset(&signal_set);
  sigdelset(&signal_set, SIG_RESUME);
  do
  { sigsuspend(&signal_set);
  } while(t->status != PL_THREAD_RESUMING);
  t->status = PL_THREAD_RUNNING;

  DEBUG(1, Sdprintf("Resuming %d\n", t-threads));
}


static void
resume_handler(int sig)
{ sem_post(&sem_mark);
}


void
resumeThreads(void)
{ struct sigaction old;
  struct sigaction new;
  int i;
  PL_thread_info_t *t;
  int signalled = 0;

  memset(&new, 0, sizeof(new));
  new.sa_handler = resume_handler;
  new.sa_flags   = SA_RESTART;
  sigaction(SIG_RESUME, &new, &old);

  sem_init(&sem_mark, USYNC_THREAD, 0);

  for(t = threads, i=0; i<MAX_THREADS; i++, t++)
  { if ( t->status == PL_THREAD_SUSPENDED )
    { int rc;

      t->status = PL_THREAD_RESUMING;

      DEBUG(1, Sdprintf("Sending SIG_RESUME to %d\n", i));
      if ( (rc=pthread_kill(t->tid, SIG_RESUME)) == 0 )
	signalled++;
      else
	Sdprintf("resumeThreads(): Failed to signal %d: %s\n", i, ThError(rc));
    }
  }

  while(signalled)
  { sem_wait(&sem_mark);
    signalled--;
  }
  sem_destroy(&sem_mark);

  sigaction(SIG_RESUME, &old, NULL);
}


static void
doThreadLocalData(int sig)
{ PL_local_data_t *ld = LD;	/* assumes pthread_getspecific() works */
				/* in a signal handler */

  (*ldata_function)(ld);

  if ( ld->thread.forall_flags & PL_THREAD_SUSPEND_AFTER_WORK )
  { PL_thread_info_t *info = ld->thread.info;
    info->status = PL_THREAD_SUSPENDED;

    DEBUG(1, Sdprintf("\n\tDone work on %d; suspending ...",
		      PL_thread_self()));
    
    sem_post(&sem_mark);
    wait_resume(info);
  } else
  { DEBUG(1, Sdprintf("\n\tDone work on %d", PL_thread_self()));
    sem_post(&sem_mark);
  }
}


void
forThreadLocalData(void (*func)(PL_local_data_t *), unsigned flags)
{ int i;
  struct sigaction old;
  struct sigaction new;
  int me = PL_thread_self();
  int signalled = 0;

  DEBUG(1, Sdprintf("Calling forThreadLocalData()\n"));

  assert(ldata_function == NULL);
  ldata_function = func;

  sem_init(&sem_mark, USYNC_THREAD, 0);

  memset(&new, 0, sizeof(new));
  new.sa_handler = doThreadLocalData;
  new.sa_flags   = SA_RESTART;
  sigaction(SIG_FORALL, &new, &old);

  for(i=1; i<MAX_THREADS; i++)
  { if ( threads[i].thread_data && i != me &&
	 threads[i].status == PL_THREAD_RUNNING )
    { int rc;

      DEBUG(1, Sdprintf("Signalling %d\n", i));
      threads[i].thread_data->thread.forall_flags = flags;
      if ( (rc=pthread_kill(threads[i].tid, SIG_FORALL)) == 0 )
      { signalled++;
      } else if ( rc != ESRCH )
	Sdprintf("forThreadLocalData(): Failed to signal: %s\n", ThError(rc));
    }
  }

  DEBUG(1, Sdprintf("Signalled %d threads.  Waiting ... ", signalled));

  while(signalled)
  { sem_wait(&sem_mark);
    DEBUG(1, Sdprintf(" (ok)"));
    signalled--;
  }

  sem_destroy(&sem_mark);

  DEBUG(1, Sdprintf(" All done!\n"));

  sigaction(SIG_FORALL, &old, NULL);

  assert(ldata_function == func);
  ldata_function = NULL;
}

#endif /*WIN32*/

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
{ DefinitionChain cell = allocHeap(sizeof(*cell));

  cell->definition = def;
  cell->next = LD->thread.local_definitions;
  LD->thread.local_definitions = cell;
}


Definition
localiseDefinition(Definition def)
{ Definition local = allocHeap(sizeof(*local));
  int id = LD->thread.info->pl_tid;

  *local = *def;
  clear(local, P_THREAD_LOCAL);
  local->definition.clauses = NULL;
  local->hash_info = NULL;
  
  if ( !def->definition.local ||
       id >= def->definition.local->size )
  { int newsize = def->definition.local ? def->definition.local->size : 1;
    LocalDefinitions new;
    int bytes;
    int i=0;

    do
    { newsize *= 2;
    } while ( newsize <= id );
     
    bytes = offsetof(struct local_definitions, thread[newsize]);
    new = allocHeap(bytes);
    new->size = newsize;
    if ( def->definition.local )
    { for(; i<def->definition.local->size; i++)
	new->thread[i] = def->definition.local->thread[i];
    }
    for(; i<newsize; i++)
      new->thread[i] = NULL;
    if ( def->definition.local )
      freeHeap(def->definition.local,
	       offsetof(struct local_definitions,
			thread[def->definition.local->size]));
    def->definition.local = new;
  }

  def->definition.local->thread[id] = local;
  registerLocalDefinition(def);

  return local;
}


static void
cleanupLocalDefinitions(PL_local_data_t *ld)
{ DefinitionChain ch = ld->thread.local_definitions;
  DefinitionChain next;
  int id = ld->thread.info->pl_tid;

  for( ; ch; ch = next)
  { Definition local, def = ch->definition;
    next = ch->next;
    
    assert(true(def, P_THREAD_LOCAL));
    PL_LOCK(L_PREDICATE);
    local = def->definition.local->thread[id];
    def->definition.local->thread[id] = NULL;
    PL_UNLOCK(L_PREDICATE);

    destroyDefinition(local);

    freeHeap(ch, sizeof(*ch));
  }
}


		 /*******************************
		 *	DEBUGGING SUPPORT	*
		 *******************************/

PL_local_data_t *
_LD()
{ PL_local_data_t *ld = ((PL_local_data_t *)TLD_get(PL_ldata));
  return ld;
}

PL_local_data_t *
_LDN(int n)
{ return threads[n].thread_data;
}

#undef lBase
LocalFrame
lBase()
{ return (LD->stacks.local.base);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is called from  GNU  <assert.h>,   so  we  can print which
thread caused the problem. If the thread is   not the main one, we could
try to recover!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
__assert_fail(const char *assertion,
	      const char *file,
	      unsigned int line,
	      const char *function)
{ Sdprintf("[Thread %d] %s:%d: %s: Assertion failed: %s\n",
	   PL_thread_self(),
	   file, line, function, assertion);
  abort();
}

#else /*O_PLMT*/

#define pl_mutex_lock(mutex)
#define pl_mutex_unlock(mutex)

int
PL_thread_self()
{ return -2;
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

#ifdef WIN32
int
PL_w32thread_raise(DWORD id, int sig)
{ return PL_raise(sig);
}
#endif

foreign_t
pl_thread_self(term_t id)
{ return PL_unify_atom(id, ATOM_main);
}

#endif  /*O_PLMT*/

		 /*******************************
		 *	    WITH-MUTEX		*
		 *******************************/

foreign_t
pl_with_mutex(term_t mutex, term_t goal)
{ term_t ex = 0;
  int rval;

  pl_mutex_lock(mutex);
  rval = callProlog(NULL, goal, PL_Q_CATCH_EXCEPTION, &ex);
  pl_mutex_unlock(mutex);

  if ( !rval && ex )
    PL_raise_exception(ex);

  return rval;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(thread)
#ifdef O_PLMT
  PRED_DEF("thread_detach", 1, thread_detach, 0)
  PRED_DEF("thread_statistics", 3, thread_statistics, 0)
  PRED_DEF("message_queue_create", 1, message_queue_create, 0)
  PRED_DEF("thread_get_message", 2, thread_get_message, 0)
  PRED_DEF("thread_peek_message", 2, thread_peek_message, 0)
  PRED_DEF("message_queue_destroy", 1, message_queue_destroy, 0)
  PRED_DEF("thread_setconcurrency", 2, thread_setconcurrency, 0)
#endif
EndPredDefs
