/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amseterdam. All rights reserved.
*/

#ifndef PL_THREAD_H_DEFINED
#define PL_THREAD_H_DEFINED

#ifdef O_PLMT
#include <pthread.h>

#define MAX_THREADS 100			/* for now */

typedef struct _PL_thread_info_t
{ int		    pl_tid;		/* Prolog thread id */
  ulong		    local_size;		/* Stack sizes */
  ulong		    global_size;
  ulong		    trail_size;
  ulong		    argument_size;
  bool		    detached;		/* detached thread */
  PL_local_data_t  *thread_data;	/* The thread-local data  */
  pthread_t	    tid;		/* Thread identifier */
  int		    status;		/* PL_THREAD_* */
  module_t	    module;		/* Module for starting goal */
  record_t	    goal;		/* Goal to start thread */
  record_t	    return_value;	/* Value (term) returned */
} PL_thread_info_t;

#define PL_THREAD_MAGIC 0x2737234f

#define PL_THREAD_RUNNING	1
#define PL_THREAD_EXITED	2
#define PL_THREAD_SUCCEEDED	3
#define PL_THREAD_FAILED	4
#define PL_THREAD_EXCEPTION	5

#define SIG_THREAD_SIGNAL SIGUSR1

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
				Thread-local data

All  thread-local  data  is  combined  in    one  structure  defined  in
pl-global.h. If Prolog is compiled for single-threading this is a simple
global variable and the macro LD is defined   to  pick up the address of
this variable. In multithreaded context,  POSIX pthread_getspecific() is
used to get separate versions for each  thread. Functions uisng LD often
may wish to write:

<header>
{ GET_LD
#undef LD
#define LD LOCAL_LD
  ...

#undef LD
#define LD GLOBAL_LD
}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern pthread_key_t PL_ldata;		/* key to local data */
extern pthread_mutex_t _PL_mutexes[];	/* Prolog mutexes */

#define L_MISC		0
#define L_ALLOC		1
#define L_ATOM		2
#define L_FLAG	        3
#define L_FUNCTOR	4
#define L_RECORD	5
#define L_THREAD	6
#define L_PREDICATE	7
#define L_MODULE	8
#define L_TABLE		9
#define L_BREAK	       10
#define L_INIT_ALLOC   11
#define L_FILE	       12
#define L_FEATURE      13
#define L_OP	       14

#ifdef O_DEBUG_MT
#define PL_LOCK(id) \
	do { Sdprintf("%s:%d: LOCK(%s)\n", __BASE_FILE__, __LINE__, #id); \
             pthread_mutex_lock(&_PL_mutexes[id]); \
	   } while(0)
#define PL_UNLOCK(id) \
	do { Sdprintf("%s:%d: UNLOCK(%s)\n", __BASE_FILE__, __LINE__, #id); \
	     pthread_mutex_unlock(&_PL_mutexes[id]); \
	   } while(0)
#else
#define PL_LOCK(id)   pthread_mutex_lock(&_PL_mutexes[id])
#define PL_UNLOCK(id) pthread_mutex_unlock(&_PL_mutexes[id])
#endif

#if 0
#define GET_LD    PL_local_data_t *__PL_ld = GLOBAL_LD;
#define LOCAL_LD  __PL_ld
#define GLOBAL_LD ((PL_local_data_t *)pthread_getspecific(PL_ldata))
#define LD	  GLOBAL_LD
#else
#define GET_LD	  PL_local_data_t *__PL_ld = GLOBAL_LD;
#define LOCAL_LD  __PL_ld
#define GLOBAL_LD _LD()
#define LD	  GLOBAL_LD
extern PL_local_data_t *_LD(void) __attribute((const));
#endif

extern PL_local_data_t *allocPrologLocalData(void);
extern void		initPrologThreads(void);
extern bool		aliasThread(int tid, atom_t name);
extern word		pl_thread_create(term_t goal, term_t id,
					 term_t options);
extern word		pl_thread_self(term_t self);
extern word		pl_thread_join(term_t thread, term_t retcode);
extern word		pl_thread_exit(term_t retcode);
extern word		pl_current_thread(term_t id, term_t status, word h);
extern word		pl_thread_kill(term_t thread, term_t sig);
extern word		pl_thread_send_message(term_t thread, term_t msg);
extern word		pl_thread_get_message(term_t msg);
extern word		pl_thread_peek_message(term_t msg);
extern foreign_t	pl_thread_signal(term_t thread, term_t goal);

extern foreign_t	pl_thread_at_exit(term_t goal);
extern int		PL_thread_self(void);

extern foreign_t	pl_mutex_create(term_t mutex);
extern foreign_t	pl_mutex_destroy(term_t mutex);
extern foreign_t	pl_mutex_lock(term_t mutex);
extern foreign_t	pl_mutex_trylock(term_t mutex);
extern foreign_t	pl_mutex_unlock(term_t mutex);
extern foreign_t	pl_mutex_unlock_all(void);
extern foreign_t	pl_current_mutex(term_t mutex,
					 term_t owner,
					 term_t count,
					 word h);

const char *		threadName(int id);
void			executeThreadSignals(int sig);
foreign_t		pl_attach_xterm(term_t in, term_t out);
#else /*O_PLMT*/

		 /*******************************
		 *	 NON-THREAD STUFF	*
		 *******************************/

#define GET_LD
#define LOCAL_LD  (&PL_local_data)
#define GLOBAL_LD (&PL_local_data)
#define LD	  GLOBAL_LD

#define PL_LOCK(id)
#define PL_UNLOCK(id)

#endif /*O_PLMT*/

#endif /*PL_THREAD_H_DEFINED*/

