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
#ifdef HAVE_SEM_INIT
#include <semaphore.h>

static sem_t sem_mark;			/* used for atom-gc */
static sem_t sem_canceled;		/* used on halt */
#else
#ifdef HAVE_SEMA_INIT
#include <synch.h>
static sema_t sem_mark;			/* used for atom-gc */
static sema_t sem_canceled;		/* used on halt */
#endif /*HAVE_SEMA_INIT*/
#endif /*HAVE_SEM_INIT*/


		 /*******************************
		 *	    GLOBAL DATA		*
		 *******************************/

static Table threadAliases;		/* name --> integer-id */
static PL_thread_info_t threads[MAX_THREADS];

pthread_key_t PL_ldata;			/* key for thread PL_local_data */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The global mutexes. Most are using  within   a  module and their name is
simply the module-name. The idea is that   a module holds a coherent bit
of data that needs a mutex for all operations.

Some remarks:

    L_MISC
	General-purpose mutex.  Should only be used for simple, very
	local tasks and may not be used to lock anything significant.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pthread_mutex_t _PL_mutexes[] =
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
  PTHREAD_MUTEX_INITIALIZER,		/* L_INIT_ALLOC */
  PTHREAD_MUTEX_INITIALIZER,		/* L_FILE */
  PTHREAD_MUTEX_INITIALIZER,		/* L_FEATURE */
  PTHREAD_MUTEX_INITIALIZER,		/* L_OP */
  PTHREAD_MUTEX_INITIALIZER,		/* L_INIT */
  PTHREAD_MUTEX_INITIALIZER		/* L_TERM */
};

#define LOCK()   PL_LOCK(L_THREAD)
#define UNLOCK() PL_UNLOCK(L_THREAD)

		 /*******************************
		 *	  LOCAL PROTOTYPES	*
		 *******************************/

static PL_thread_info_t *alloc_thread(void);
static void	freeThreadMessages(PL_local_data_t *ld);
static void	freeThreadSignals(PL_local_data_t *ld);
static void	unaliasThread(atom_t name);
static void	run_thread_exit_hooks();
static void	free_thread_info(PL_thread_info_t *info);


		 /*******************************
		 *	 THREAD ALLOCATION	*
		 *******************************/

PL_local_data_t *
allocPrologLocalData()
{ PL_local_data_t *ld = allocHeap(sizeof(PL_local_data_t));
  memset(ld, 0, sizeof(PL_local_data_t));
  pthread_setspecific(PL_ldata, ld);

  return ld;
}


int
PL_initialise_thread(PL_thread_info_t *info)
{ assert(info->thread_data);

  pthread_setspecific(PL_ldata, info->thread_data);

  if ( !info->local_size    ) info->local_size    = GD->options.localSize;
  if ( !info->global_size   ) info->global_size   = GD->options.globalSize;
  if ( !info->trail_size    ) info->trail_size    = GD->options.trailSize;
  if ( !info->argument_size ) info->argument_size = GD->options.argumentSize;

  initPrologStacks(info->local_size,
		   info->global_size,
		   info->trail_size,
		   info->argument_size);

  initPrologLocalData();

  pthread_mutex_init(&info->thread_data->thread.queue_mutex, NULL);
  pthread_cond_init(&info->thread_data->thread.cond_var, NULL);

  LOCK();
  GD->statistics.threads_created++;
  UNLOCK();

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
free_prolog_thread()
    Called to free thread-specific data.  Please note that LD (obtained
    through pthread_getspecific() is no longer accessible! 

    We try to call the exit hooks from here by temporary reinstalling
    the thread local-data.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
free_prolog_thread(void *data)
{ PL_local_data_t *ld = data;
  PL_thread_info_t *info = ld->thread.info;

  DEBUG(1, Sdprintf("Freeing prolog thread %u\n", pthread_self()));

  pthread_setspecific(PL_ldata, data);	/* put it back */
  run_thread_exit_hooks();
  
  freeStacks(ld);
  freeLocalData(ld);

  if ( ld->feature.table )
    destroyHTable(ld->feature.table);
  /*PL_unregister_atom(ld->prompt.current);*/

  freeThreadMessages(ld);
  freeThreadSignals(ld);

  pthread_setspecific(PL_ldata, NULL);	/* to NULL (avoid recursion) */

  LOCK();
  GD->statistics.threads_finished++;
  GD->statistics.thread_cputime += CpuTime(CPU_USER);
  UNLOCK();

  if ( info->status == PL_THREAD_CANCELED )
#ifdef HAVE_SEM_INIT
    sem_post(&sem_canceled);
#else
#  ifdef HAVE_SEMA_INIT
    sema_post(&sem_canceled);
#  endif
#endif

  if ( info->detached )
    free_thread_info(info);
}


void
initPrologThreads()
{ PL_thread_info_t *info;

  pthread_key_create(&PL_ldata, free_prolog_thread);
  info = alloc_thread();
  info->tid = pthread_self();
  pthread_setspecific(PL_ldata, info->thread_data);
  GD->statistics.thread_cputime = 0.0;
  GD->statistics.threads_created = 1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A first step towards clean destruction of the system.  Ideally, we would
like the following to happen:

    * Close-down all threads except for the main one
	+ Have all thread_at_exit/1 hooks called
    * Run the at_halt/1 hooks in the main thread
    * Exit from the main thread.

There are a lot of problems however.

    * Cancellation is not safe, as mutexes are not guarded by
      pthread_cleanup_push()
    * Somehow Halt() should always be called from the main thread
      to have the process working properly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
exitPrologThreads()
{ int i;
  int me = PL_thread_self();
  int canceled = 0;

#ifdef HAVE_SEM_INIT
  sem_init(&sem_canceled, 0, 0);
#else
#  ifdef HAVE_SEMA_INIT
  sema_init(&sem_canceled, 0, USYNC_THREAD,NULL);
#  endif
#endif
  for(i=1; i<MAX_THREADS; i++)
  { if ( threads[i].thread_data && i != me )
    { switch(threads[i].status)
      { case PL_THREAD_FAILED:
	case PL_THREAD_EXITED:
	case PL_THREAD_EXCEPTION:
	{ void *r;
	  if ( pthread_join(threads[i].tid, &r) )
	    Sdprintf("Failed to join thread %d: %s\n", i, OsError());

	  break;
	}
	case PL_THREAD_RUNNING:
	  if ( pthread_cancel(threads[i].tid) == 0 )
	  { threads[i].status = PL_THREAD_CANCELED;
	    canceled++;
	  } else
	  { Sdprintf("Failed to cancel thread %d: %s\n", i, OsError());
	  }
	  break;
      }
    }
  }

  while(canceled)
  { 
    int maxwait = 10;

    while(maxwait--) {
#ifdef HAVE_SEM_INIT
    if ( sem_trywait(&sem_canceled) == 0 )
#else
#ifdef HAVE_SEMA_INIT
    if (sema_trywait(&sem_canceled) == 0 )
#endif
#endif
	break;
      Pause(0.1);
    }

    canceled--;
  }
#ifdef HAVE_SEM_INIT
  sem_destroy(&sem_canceled);
#else
#ifdef HAVE_SEMA_INIT
  sema_destroy(&sem_canceled);
#endif
#endif
}


		 /*******************************
		 *	    ALIAS NAME		*
		 *******************************/

bool
aliasThread(int tid, atom_t name)
{ int rval;

  LOCK();
  if ( !threadAliases )
    threadAliases = newHTable(16);

  if ( (rval = addHTable(threadAliases, (void *)name, (void *)tid)) )
  { PL_register_atom(name);
    threads[tid].thread_data->thread.name = name;
  }
  UNLOCK();

  return rval;
}

static void
unaliasThread(atom_t name)
{ if ( threadAliases )
  { Symbol s;

    LOCK();
    if ( (s = lookupHTable(threadAliases, (void *)name)) )
    { PL_unregister_atom(name);
      deleteSymbolHTable(threadAliases, s);
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

  LOCK();
  for(i=1; i<MAX_THREADS; i++)
  { if ( threads[i].thread_data == 0 )
    { PL_local_data_t *ld = allocHeap(sizeof(PL_local_data_t));
      memset(ld, 0, sizeof(PL_local_data_t));

      threads[i].pl_tid = i;
      threads[i].thread_data = ld;
            threads[i].status = PL_THREAD_CREATED;
      ld->thread.info = &threads[i];
      ld->thread.magic = PL_THREAD_MAGIC;

      UNLOCK();
      return &threads[i];
    }
  }
  UNLOCK();

  return NULL;				/* out of threads */
}


int
PL_thread_self()
{ PL_local_data_t *ld = LD;

  if ( ld )
    return ld->thread.info->pl_tid;

  return -1;				/* thread has no Prolog thread */
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

  if ( info->thread_data->thread.name )
    return PL_atom_chars(info->thread_data->thread.name);

  sprintf(tmp, "%d", id);
  return buffer_string(tmp, BUF_RING);
}


static const opt_spec make_thread_options[] = 
{ { ATOM_local,		OPT_LONG },
  { ATOM_global,	OPT_LONG },
  { ATOM_trail,	        OPT_LONG },
  { ATOM_argument,	OPT_LONG },
  { ATOM_alias,		OPT_ATOM },
  { ATOM_detached,	OPT_BOOL },
  { NULL_ATOM,		0 }
};


static void *
start_thread(void *closure)
{ PL_thread_info_t *info = closure;
  term_t ex, goal;
  int rval;

  blockSignal(SIGINT);			/* only the main thread processes */
					/* Control-C */

#ifdef __linux__
  info->pid = getpid();
#endif

  LOCK();
  info->status = PL_THREAD_RUNNING;
  UNLOCK();

  PL_initialise_thread(info);
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

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
  run_thread_exit_hooks();

  return (void *)TRUE;
}


word
pl_thread_create(term_t goal, term_t id, term_t options)
{ PL_thread_info_t *info = alloc_thread();
  PL_local_data_t *ldnew;
  atom_t alias = NULL_ATOM;
  pthread_attr_t attr;

  if ( !info )
    return PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_threads);
  if ( !pl_callable(goal) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, goal);

  ldnew = info->thread_data;

  if ( !scan_options(options, 0,
		     ATOM_thread_option, make_thread_options,
		     &info->local_size,
		     &info->global_size,
		     &info->trail_size,
		     &info->argument_size,
		     &alias,
		     &info->detached) )
    fail;

  info->local_size    *= 1024;
  info->global_size   *= 1024;
  info->trail_size    *= 1024;
  info->argument_size *= 1024;

  info->goal = PL_record(goal);
  info->module = PL_context();

  if ( alias )
    aliasThread(info->pl_tid, alias);

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

  pthread_attr_init(&attr);
  if ( info->detached )
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  if ( pthread_create(&info->tid, &attr, start_thread, info) != 0 )
    return PL_warning("Could not create thread: %s", OsError());
  pthread_attr_destroy(&attr);
					/* TBD: exception, unallocate! */

  return PL_unify_integer(id, info->pl_tid);
}


static int
get_thread(term_t t, PL_thread_info_t **info, int warn)
{ int i = -1;

  if ( !PL_get_integer(t, &i) )
  { atom_t name;

    if ( !PL_get_atom(t, &name) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_thread, t);
    if ( threadAliases )
    { Symbol s;

      if ( (s = lookupHTable(threadAliases, (void *)name)) )
	i = (int)s->value;
    }
  }

  if ( i < 0 || i >= MAX_THREADS || !threads[i].thread_data )
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
{ if ( info->thread_data->thread.name )
    return PL_unify_atom(id, info->thread_data->thread.name);

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
    case PL_THREAD_CANCELED:
      return PL_unify_atom(status, ATOM_canceled);
    default:
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
{ PL_local_data_t *data = info->thread_data;

  if ( info->return_value )
    PL_erase(info->return_value);
  if ( info->goal )
    PL_erase(info->goal);

  if ( info->thread_data->thread.name )
    unaliasThread(info->thread_data->thread.name);

  memset(info, 0, sizeof(*info));
  freeHeap(data, sizeof(*data));
}


word
pl_thread_join(term_t thread, term_t retcode)
{ PL_thread_info_t *info;
  void *r;
  word rval;

  if ( !get_thread(thread, &info, TRUE) )
    fail;
  if ( info == LD->thread.info )	/* joining myself */
    return PL_error("thread_join", 2, "Cannot join self",
		    ERR_PERMISSION, PL_new_atom("join"), ATOM_thread, thread);

  if ( pthread_join(info->tid, &r) )
    return PL_error("thread_join", 2, MSG_ERRNO, ERR_SYSCALL, "pthread_join");
  
  rval = unify_thread_status(retcode, info);
   
  free_thread_info(info);

  return rval;
}


word
pl_thread_exit(term_t retcode)
{ PL_thread_info_t *info = LD->thread.info;

    LOCK();
  info->status = PL_THREAD_EXITED;
  info->return_value = PL_record(retcode);
    UNLOCK();

  pthread_exit(NULL);
  fail;					/* should not happen */
}


word
pl_thread_kill(term_t t, term_t sig)
{ PL_thread_info_t *info;
  int s;

  if ( !get_thread(t, &info, TRUE) )
    fail;
  if ( !_PL_get_signum(sig, &s) )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_signal, sig);

  if ( pthread_kill(info->tid, s) )
  { assert(errno == ESRCH);

    return PL_error("thread_kill", 2, NULL, ERR_EXISTENCE, ATOM_thread, t);
  }

  succeed;
}



word
pl_current_thread(term_t id, term_t status, word h)
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

		 /*******************************
		 *	     CLEANUP		*
		 *******************************/

typedef struct _at_exit_goal
{ struct _at_exit_goal *next;		/* Next in queue */
  Module   module;			/* Module for running goal */
  record_t goal;			/* Goal to run */
} at_exit_goal;


foreign_t
pl_thread_at_exit(term_t goal)
{ Module m = NULL;
  at_exit_goal *eg = allocHeap(sizeof(*eg));

  PL_strip_module(goal, &m, goal);
  eg->next = NULL;
  eg->module = m;
  eg->goal = PL_record(goal);

  eg->next = LD->thread.exit_goals;
  LD->thread.exit_goals = eg;

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Newly pushed hooks are executed  after   all  currently registered hooks
have finished. 

Q: What to do with exceptions?
Q: Should we limit the passes?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
run_thread_exit_hooks()
{ term_t goal = PL_new_term_ref();
  fid_t fid = PL_open_foreign_frame();
  at_exit_goal *eg;

  while( (eg = LD->thread.exit_goals) )
  { at_exit_goal *next;

    LD->thread.exit_goals = NULL;	/* empty these */

    for( ; eg; eg = next)
    { next = eg->next;
  
      PL_recorded(eg->goal, goal);
      PL_erase(eg->goal);
      callProlog(eg->module, goal, PL_Q_NODEBUG, NULL);
      PL_rewind_foreign_frame(fid);
      freeHeap(eg, sizeof(*eg));
    }
  }

  PL_discard_foreign_frame(fid);
  PL_reset_term_refs(goal);
}


		 /*******************************
		 *	   THREAD SIGNALS	*
		 *******************************/

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

Messages are send asynchronously.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct _thread_msg
{ struct _thread_msg *next;		/* next in queue */
  record_t            message;		/* message in queue */
} thread_message;


word
pl_thread_send_message(term_t thread, term_t msg)
{ PL_thread_info_t *info;
  PL_local_data_t *ld;
  thread_message *msgp;

  if ( !get_thread(thread, &info, TRUE) )
    fail;

  msgp = allocHeap(sizeof(*msgp));
  msgp->next    = NULL;
  msgp->message = PL_record(msg);

  LOCK();
  ld = info->thread_data;
  if ( !ld->thread.msg_head )
    ld->thread.msg_head = ld->thread.msg_tail = msgp;
  else
  { ld->thread.msg_tail->next = msgp;
    ld->thread.msg_tail = msgp;
  }
  pthread_cond_signal(&ld->thread.cond_var);
  UNLOCK();

  succeed;
}


word
pl_thread_get_message(term_t msg)
{ PL_local_data_t *ld = LD;
  thread_message *msgp;
  thread_message *prev = NULL;
  term_t tmp = PL_new_term_ref();
  mark m;

  Mark(m);

  LOCK();
  msgp = ld->thread.msg_head;

  for(;;)
  { for( ; msgp; prev = msgp, msgp = msgp->next )
    { PL_recorded(msgp->message, tmp);

      if ( PL_unify(msg, tmp) )
      { if ( prev )
	{ if ( !(prev->next = msgp->next) )
	    ld->thread.msg_tail = prev;
	} else
	{ if ( !(ld->thread.msg_head = msgp->next) )
	    ld->thread.msg_tail = NULL;
	}
	PL_erase(msgp->message);
	freeHeap(msgp, sizeof(*msgp));
	UNLOCK();
	succeed;
      }
      Undo(m);				/* reclaim term */
    }
    pthread_mutex_lock(&ld->thread.queue_mutex);
    UNLOCK();
    pthread_cond_wait(&ld->thread.cond_var, &ld->thread.queue_mutex);
    LOCK();
    pthread_mutex_unlock(&ld->thread.queue_mutex);

    msgp = (prev ? prev->next : ld->thread.msg_head);
  }
}


word
pl_thread_peek_message(term_t msg)
{ PL_local_data_t *ld = LD;
  thread_message *msgp;
  term_t tmp = PL_new_term_ref();
  mark m;

  Mark(m);

  LOCK();
  msgp = ld->thread.msg_head;

  for( msgp = ld->thread.msg_head; msgp; msgp = msgp->next )
  { PL_recorded(msgp->message, tmp);

    if ( PL_unify(msg, tmp) )
    { UNLOCK();
      succeed;
    }

    Undo(m);
  }
     
  UNLOCK();
  fail;
}


static void
freeThreadMessages(PL_local_data_t *ld)
{ thread_message *msgp;
  thread_message *next;

  for( msgp = ld->thread.msg_head; msgp; msgp = next )
  { next = msgp->next;

    PL_erase(msgp->message);
    freeHeap(msgp, sizeof(*msgp));
  }
}

		 /*******************************
		 *	    USER MUTEXES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
User-level mutexes (critical sections in MS parlance).  
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Table mutexTable;
static int mutex_id;

typedef struct _pl_mutex
{ pthread_mutex_t mutex;		/* the system mutex */
  int count;				/* lock count */
  int owner;				/* integer id of owner */
  word id;				/* id of the mutex */
} pl_mutex;


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


recursive_mutex_t *
newRecursiveMutex()
{ recursive_mutex_t *m = allocHeap(sizeof(*m));

#ifdef RECURSIVE_MUTEXES
  /*pthread_mutex_t *m = allocHeap(sizeof(*m));*/
  pthread_mutexattr_t attr;

  if ( !m )
    return NULL;

  pthread_mutexattr_init(&attr);
#ifdef HAVE_PTHREAD_MUTEXATTR_SETKIND_NP
  pthread_mutexattr_setkind_np(&attr, PTHREAD_MUTEX_RECURSIVE_NP);
#else
#ifdef HAVE_PTHREAD_MUTEXATTR_SETTYPE
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
#endif
#endif
  pthread_mutex_init(m, &attr);
#else
  m->owner = 0;
  m->count = 0;
  pthread_mutex_init(&(m->lock),NULL);
#endif /* RECURSIVE_MUTEXES */

  return m;
}


int
freeRecursiveMutex(recursive_mutex_t *m)
{
#ifdef RECURSIVE_MUTEXES
  if ( pthread_mutex_destroy(m) != 0 )
    fail;
#else
  if (m->owner != 0)
    fail;
  else if ( pthread_mutex_destroy(&(m->lock)) != 0)
    fail;
#endif
  freeHeap(m, sizeof(*m));
  succeed;
}


#ifndef RECURSIVE_MUTEXES
int
recursive_mutex_lock(recursive_mutex_t *m)
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
recursive_mutex_trylock(recursive_mutex_t *m)
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
recursive_mutex_unlock(recursive_mutex_t *m)
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

static pl_mutex *
unlocked_pl_mutex_create(term_t mutex)
{ Symbol s;
  atom_t name = NULL_ATOM;
  pl_mutex *m;
  word id;

  if ( !mutexTable )
    mutexTable = newHTable(16);
  
  if ( PL_get_atom(mutex, &name) )
  { if ( (s = lookupHTable(mutexTable, (void *)name)) )
    { PL_error("mutex_create", 1, NULL, ERR_PERMISSION,
	       ATOM_mutex, ATOM_create, mutex);
      return NULL;
    }
    id = name;
  } else if ( PL_is_variable(mutex) )
  { id = consInt(mutex_id++);
  } else
  { PL_error("mutex_create", 1, NULL, ERR_TYPE, ATOM_mutex, mutex);
    return NULL;
  }

  m = allocHeap(sizeof(*m));
  pthread_mutex_init(&m->mutex, NULL);
  m->count = 0;
  m->owner = 0;
  m->id    = id;
  addHTable(mutexTable, (void *)id, m);

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
  if ( mutexTable )
  { Symbol s = lookupHTable(mutexTable, (void *)id);

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



foreign_t
pl_mutex_lock(term_t mutex)
{ pl_mutex *m;
  int self = PL_thread_self();

  if ( !get_mutex(mutex, &m, TRUE) )
    fail;

  if ( self == m->owner )
  { m->count++;
  } else if ( pthread_mutex_lock(&m->mutex) == 0 )
  { m->count = 1;
    m->owner = self;
  } else
  { assert(0);
  }

  succeed;
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
  } else if ( (rval = pthread_mutex_trylock(&m->mutex)) == 0 )
  { m->count = 1;
    m->owner = self;
  } else if ( rval == EBUSY )
  { fail;
  } else
  { assert(0);
  }

  succeed;
}


foreign_t
pl_mutex_unlock(term_t mutex)
{ pl_mutex *m;
  int self = PL_thread_self();

  if ( !get_mutex(mutex, &m, FALSE) )
    fail;

  if ( self == m->owner )
  { if ( --m->count == 0 )
    { m->owner = 0;

      if ( pthread_mutex_unlock(&m->mutex) != 0 )
	assert(0);
    }

    succeed;
  }

  return PL_error("mutex_unlock", 1, MSG_ERRNO, ERR_PERMISSION,
		  ATOM_mutex, ATOM_unlock, mutex);
}


foreign_t
pl_mutex_unlock_all()
{ TableEnum e;
  Symbol s;
  int tid = PL_thread_self();

  if ( !mutexTable )
    succeed;
  
  e = newTableEnum(mutexTable);
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

  if ( !get_mutex(mutex, &m, FALSE) )
    fail;

  if ( !pthread_mutex_destroy(&m->mutex) )
    return PL_error("mutex_destroy", 1, NULL,
		    ERR_PERMISSION, ATOM_mutex, ATOM_destroy, mutex);

  LOCK();
  s = lookupHTable(mutexTable, (void *)m->id);
  deleteSymbolHTable(mutexTable, s);
  freeHeap(m, sizeof(*m));
  UNLOCK();

  succeed;
}


foreign_t
pl_current_mutex(term_t mutex, term_t owner, term_t count, word h)
{ TableEnum e;
  Symbol s;
  mark mrk;

  switch(ForeignControl(h))
  { case FRG_FIRST_CALL:
    { if ( PL_is_variable(mutex) )
      { if ( !mutexTable )
	  fail;
	e = newTableEnum(mutexTable);
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

  if ( !(info = alloc_thread()) )
    return -1;				/* out of threads */

  ldmain = threads[1].thread_data;
  ldnew = info->thread_data;

  if ( attr )
  { if ( attr->local_size )	info->local_size    = attr->local_size;
    if ( attr->global_size )	info->global_size   = attr->global_size;
    if ( attr->trail_size )	info->trail_size    = attr->trail_size;
    if ( attr->argument_size )	info->argument_size = attr->argument_size;
    if ( attr->alias )
      aliasThread(info->pl_tid, PL_new_atom(attr->alias));
  }
  
  info->goal       = NULL;
  info->module     = MODULE_user;
  info->detached   = TRUE;		/* C-side should join me */
  info->status     = PL_THREAD_RUNNING;
  info->open_count = 1;

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

  PL_initialise_thread(info); 
  info->tid = pthread_self();		/* we are complete now */

  return info->pl_tid;
}


int
PL_thread_destroy_engine()
{ PL_local_data_t *ld = LD;

  if ( ld )
  { if ( --ld->thread.info->open_count == 0 )
    { free_prolog_thread(ld);
      pthread_setspecific(PL_ldata, NULL);
    }

    return TRUE;
  }

  return FALSE;				/* we had no thread */
}

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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#include <signal.h>

#ifndef SA_RESTART
#define SA_RESTART 0
#endif

static void
threadMarkAtoms(int sig)
{
#ifdef __linux__
  pid_t me = getpid();
#else
  pthread_t me = pthread_self();
#endif
  int i;

  for(i=0; i<MAX_THREADS; i++)
  {
#ifdef __linux__
    if ( threads[i].pid == me )
#else
    if ( threads[i].tid == me )
#endif
    { markAtomsOnStacks(threads[i].thread_data);
      break;
    }
  }
#ifdef HAVE_SEM_INIT
  sem_post(&sem_mark);
#else
#ifdef HAVE_SEMA_INIT
  sema_post(&sem_mark);
#endif
#endif
}


#define SIG_MARKATOMS SIGHUP

void
threadMarkAtomsOtherThreads()
{ int i;
  struct sigaction old;
  struct sigaction new;
  int me = PL_thread_self();
  int signalled = 0;

#ifdef HAVE_SEM_INIT
  sem_init(&sem_mark, 0, 0);
#else
#ifdef HAVE_SEMA_INIT
  sema_init(&sem_mark,0,USYNC_THREAD,NULL);
#endif
#endif

  memset(&new, 0, sizeof(new));
  new.sa_handler = threadMarkAtoms;
  new.sa_flags   = SA_RESTART;
  sigaction(SIG_MARKATOMS, &new, &old);

  for(i=1; i<MAX_THREADS; i++)
  { if ( threads[i].thread_data && i != me &&
	 threads[i].status == PL_THREAD_RUNNING )
    { DEBUG(1, Sdprintf("Signalling %d\n", i));
      if ( pthread_kill(threads[i].tid, SIG_MARKATOMS) == 0 )
      { signalled++;
      } else if ( errno != ESRCH )
	Sdprintf("Failed to signal: %s\n", OsError());
    }
  }

  DEBUG(1, Sdprintf("Signalled %d threads.  Waiting ... ", signalled));

  while(signalled)
  { 
#ifdef HAVE_SEM_INIT
    sem_wait(&sem_mark);
#else
#ifdef HAVE_SEMA_INIT
    sema_wait(&sem_mark);
#endif
#endif
    signalled--;
  }
#ifdef HAVE_SEM_INIT
  sem_destroy(&sem_mark);
#else
#ifdef HAVE_SEMA_INIT
  sema_destroy(&sem_mark);
#endif
#endif

  DEBUG(1, Sdprintf("done!\n"));

  sigaction(SIG_MARKATOMS, &old, NULL);
}

		 /*******************************
		 *	DEBUGGING SUPPORT	*
		 *******************************/

PL_local_data_t *
_LD()
{ PL_local_data_t *ld = ((PL_local_data_t *)pthread_getspecific(PL_ldata));
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
PL_thread_attach_engine(PL_thread_attr_t *attr)
{ return -2;
}

int
PL_thread_destroy_engine()
{ return FALSE;
}

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

