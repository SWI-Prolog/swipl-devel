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
extern word		pl_thread_create(term_t goal, term_t id,
					 term_t options);
extern word		pl_thread_self(term_t self);
extern word		pl_thread_join(term_t thread, term_t retcode);
extern word		pl_thread_exit(term_t retcode);
extern word		pl_current_thread(term_t id, term_t status, word h);

#else /*O_PLMT*/

		 /*******************************
		 *	 NON-THREAD STUFF	*
		 *******************************/

#define GET_LD
#define LOCAL_LD  (&PL_local_data)
#define GLOBAL_LD (&PL_local_data)
#define LD	  GLOBAL_LD

#endif /*O_PLMT*/

#endif /*PL_THREAD_H_DEFINED*/

