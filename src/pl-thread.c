/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amseterdam. All rights reserved.
*/

#include "pl-incl.h"
#ifdef O_PLMT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			   MULTITHREADING SUPPORT

APPROACH
========

    * Prolog threads are C-threads
      Prolog multi-threading is based upon a C thread library.  At first,
      we will concentrate on teh Posix pthread standard, due to its wide
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
	  area needed.

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
	  start-up.  Coperating functions can pass this pointer.

STEPS
=====

    * Realise PL_initialise_thread() to set up a Prolog engine in the
      current thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PL_thread_info_t *alloc_thread(void);

pthread_key_t PL_ldata;			/* key for thread PL_local_data */
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
  PTHREAD_MUTEX_INITIALIZER		/* L_BREAK */
};

#define LOCK()   PL_LOCK(L_THREAD)
#define UNLOCK() PL_UNLOCK(L_THREAD)

PL_local_data_t *
allocPrologLocalData()
{ PL_local_data_t *ld = allocHeap(sizeof(PL_local_data_t));
  memset(ld, 0, sizeof(PL_local_data_t));
  pthread_setspecific(PL_ldata, ld);

  return ld;
}


int
PL_initialise_thread(PL_thread_info_t *info)
{ if ( !info )
    info = alloc_thread();

  if ( !info->thread_data )
  { info->thread_data = allocHeap(sizeof(PL_local_data_t));
    memset(info->thread_data, 0, sizeof(PL_local_data_t));
    info->thread_data->thread.info = info;
  }

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

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
free_prolog_thread()
    Called to free thread-specific data.  Please note that LD (obtained
    through pthread_getspecific() is no longer accessible! 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
free_prolog_thread(void *data)
{ PL_local_data_t *ld = data;
  int i;

  freeStacks(ld);

  discardBuffer(&ld->fli._discardable_buffer);
  for(i=0; i<BUFFER_RING_SIZE; i++)
    discardBuffer(&ld->fli._buffer_ring[i]);
}


void
initPrologThreads()
{ PL_thread_info_t *info;

  pthread_key_create(&PL_ldata, free_prolog_thread);
  info = alloc_thread();
  info->tid = pthread_self();
  pthread_setspecific(PL_ldata, info->thread_data);
}


		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

static PL_thread_info_t threads[MAX_THREADS];

static PL_thread_info_t *
alloc_thread()
{ int i;

  LOCK();
  for(i=1; i<MAX_THREADS; i++)
  { if ( threads[i].tid == 0 )
    { PL_local_data_t *ld = allocHeap(sizeof(PL_local_data_t));
      memset(ld, 0, sizeof(PL_local_data_t));

      memset(&threads[i], 0, sizeof(PL_thread_info_t));
      threads[i].pl_tid = i;
      threads[i].thread_data = ld;
      threads[i].status = PL_THREAD_RUNNING;
      ld->thread.info = &threads[i];
      ld->thread.magic = PL_THREAD_MAGIC;

      UNLOCK();
      return &threads[i];
    }
  }
  UNLOCK();

  return NULL;				/* out of threads */
}


static int
PL_thread_self()
{ PL_local_data_t *ld = LD;

  if ( ld )
    return ld->thread.info->pl_tid;

  return -1;				/* thread has no Prolog thread */
}


static const opt_spec make_thread_options[] = 
{ { ATOM_local,		OPT_INT },
  { ATOM_global,	OPT_INT },
  { ATOM_trail,	        OPT_INT },
  { ATOM_argument,	OPT_INT },
  { NULL_ATOM,		0 }
};


static void *
start_thread(void *closure)
{ PL_thread_info_t *info = closure;
  term_t ex, goal;
  int rval;

  PL_initialise_thread(info);
  goal = PL_new_term_ref();
  
  PL_recorded(info->goal, goal);
  rval  = callProlog(info->module, goal, PL_Q_CATCH_EXCEPTION, &ex);
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

  return (void *)TRUE;
}


word
pl_thread_create(term_t goal, term_t id, term_t options)
{ PL_thread_info_t *info = alloc_thread();

  if ( !scan_options(options, 0,
		     ATOM_thread_option, make_thread_options,
		     &info->local_size,
		     &info->global_size,
		     &info->trail_size,
		     &info->argument_size) )
    fail;

  info->local_size    *= 1024;
  info->global_size   *= 1024;
  info->trail_size    *= 1024;
  info->argument_size *= 1024;

  info->goal = PL_record(goal);
  info->module = PL_context();

  info->thread_data->prompt  = LD->prompt;
  info->thread_data->modules = LD->modules;
  info->thread_data->IO      = LD->IO;

  if ( pthread_create(&info->tid, NULL, start_thread, info) != 0 )
    return PL_warning("Could not create thread: %s", OsError());

  return PL_unify_integer(id, info->pl_tid);
}


static int
get_thread(term_t t, PL_thread_info_t **info)
{ int i;

  if ( !PL_get_integer(t, &i) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_thread, t);
  if ( i < 0 || i >= MAX_THREADS || !threads[i].tid )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_thread, t);
  
  *info = &threads[i];

  return TRUE;
}


static int
unify_thread(term_t id, PL_thread_info_t *info)
{ return PL_unify_integer(id, info->pl_tid);
}


static int
unify_thread_status(term_t status, PL_thread_info_t *info)
{ switch(info->status)
  { case PL_THREAD_RUNNING:
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
    default:
      assert(0);
      fail;
  }
}


word
pl_thread_self(term_t self)
{ DEBUG(1, Sdprintf("thread_self(): LD at %p\n", LD));

  return PL_unify_integer(self, PL_thread_self());
}


word
pl_thread_join(term_t thread, term_t retcode)
{ PL_thread_info_t *info;
  void *r;
  word rval;

  if ( !get_thread(thread, &info) )
    fail;
  if ( pthread_join(info->tid, &r) )
    return PL_error("thread_join", 2, MSG_ERRNO, ERR_SYSCALL, ATOM_nil);
  
  rval = unify_thread_status(retcode, info);
   
  if ( info->return_value )
    PL_erase(info->return_value);
  if ( info->goal )
    PL_erase(info->goal);

  freeHeap(info->thread_data, sizeof(*info->thread_data));

  info->tid = 0;

  return rval;
}


word
pl_thread_exit(term_t retcode)
{ PL_thread_info_t *info = LD->thread.info;

  info->status = PL_THREAD_EXITED;
  info->return_value = PL_record(retcode);
  pthread_exit(NULL);
  fail;					/* should not happen */
}


word
pl_thread_kill(term_t t, term_t sig)
{ PL_thread_info_t *info;
  int s;

  if ( !get_thread(t, &info) )
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
      if ( !get_thread(id, &info) )
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
		 *	  DEBUGGING AIDS	*
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

#endif /*O_PLMT*/


