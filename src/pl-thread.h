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

#ifndef PL_THREAD_H_DEFINED
#define PL_THREAD_H_DEFINED

#ifdef O_PLMT
#include <pthread.h>

#ifdef HAVE_SEMA_INIT
#include <synch.h>
#endif

#define MAX_THREADS 100			/* for now */

typedef struct _PL_thread_info_t
{ int		    pl_tid;		/* Prolog thread id */
  unsigned long	    local_size;		/* Stack sizes */
  unsigned long	    global_size;
  unsigned long	    trail_size;
  unsigned long	    argument_size;
  int		    open_count;		/* for PL_thread_detach_engine() */
  bool		    detached;		/* detached thread */
  int		    status;		/* PL_THREAD_* */
  pthread_t	    tid;		/* Thread identifier */
#ifdef __linux__
  pid_t		    pid;		/* for identifying */
#endif
#ifdef WIN32
  unsigned long	    w32id;		/* Win32 thread HANDLE */
#endif
  PL_local_data_t  *thread_data;	/* The thread-local data  */
  module_t	    module;		/* Module for starting goal */
  record_t	    goal;		/* Goal to start thread */
  record_t	    return_value;	/* Value (term) returned */
  atom_t	    name;		/* Name of the thread */
} PL_thread_info_t;


#define PL_THREAD_MAGIC 0x2737234f

#define PL_THREAD_RUNNING	1
#define PL_THREAD_EXITED	2
#define PL_THREAD_SUCCEEDED	3
#define PL_THREAD_FAILED	4
#define PL_THREAD_EXCEPTION	5
#define PL_THREAD_CANCELED	6
#define	PL_THREAD_CREATED	7
#define	PL_THREAD_SUSPENDED	8
#define PL_THREAD_RESUMING	9

extern simpleMutex _PL_mutexes[];	/* Prolog mutexes */

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
#define L_FILE	       11
#define L_FEATURE      12
#define L_OP	       13
#define L_INIT	       14
#define L_TERM	       15

#ifdef O_DEBUG_MT
#define PL_LOCK(id) \
	do { Sdprintf("[%s] %s:%d: LOCK(%s)\n", \
		      threadName(0), \
		      __BASE_FILE__, __LINE__, #id); \
             simpleMutexLock(&_PL_mutexes[id]); \
	   } while(0)
#define PL_UNLOCK(id) \
	do { Sdprintf("[%s] %s:%d: UNLOCK(%s)\n", \
		      threadName(0), \
		      __BASE_FILE__, __LINE__, #id); \
	     simpleMutexUnlock(&_PL_mutexes[id]); \
	   } while(0)
#else
#define PL_LOCK(id)   simpleMutexLock(&_PL_mutexes[id])
#define PL_UNLOCK(id) simpleMutexUnlock(&_PL_mutexes[id])
#endif

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

		 /*******************************
		 *   NATIVE THREAD-LOCAL DATA	*
		 *******************************/

#ifdef WIN32
typedef DWORD	TLD_KEY;

#define TLD_alloc(p)	(*(p) = TlsAlloc())
#define TLD_get(p)	TlsGetValue((p))
#define TLD_set(p, v)	TlsSetValue((p), (v))

#else
typedef pthread_key_t TLD_KEY;

#define TLD_alloc(p)	pthread_key_create(p, NULL)
#define TLD_get(p)	pthread_getspecific(p)
#define TLD_set(p, v)	pthread_setspecific((p), (v))
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If available, use GCC's __attribute((const)) to tell the compiler it may
choose to store the result of LD is a local variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern TLD_KEY PL_ldata;		/* key to local data */

#if defined(__GNUC__) && 0
#define GLOBAL_LD _LD()
extern PL_local_data_t *_LD(void) __attribute((const));
#else
#define GLOBAL_LD ((PL_local_data_t *)TLD_get(PL_ldata))
#endif
#define GET_LD    PL_local_data_t *__PL_ld = GLOBAL_LD;
#define PRED_LD   PL_local_data_t *__PL_ld = PL__ctx->engine;

#define ARG1_LD   PL_local_data_t *__PL_ld
#define ARG_LD    , ARG1_LD
#define PASS_LD1  LD
#define PASS_LD   , LD
#define LOCAL_LD  __PL_ld
#define LD	  GLOBAL_LD


		 /*******************************
		 *	    FUNCTIONS		*
		 *******************************/

extern void		initPrologThreads(void);
extern void		exitPrologThreads(void);
extern bool		aliasThread(int tid, atom_t name);
extern word		pl_thread_create(term_t goal, term_t id,
					 term_t options);
extern word		pl_thread_join(term_t thread, term_t retcode);
extern word		pl_thread_exit(term_t retcode);
extern word		pl_current_thread(term_t id, term_t status, control_t h);
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
					 control_t h);

const char *		threadName(int id);
void			executeThreadSignals(int sig);
foreign_t		pl_attach_xterm(term_t in, term_t out);
long			threadLocalHeapUsed(void);


		 /*******************************
		 *	 GLOBAL GC SUPPORT	*
		 *******************************/

void			forThreadLocalData(void (*func)(PL_local_data_t *),
					   unsigned flags);
void			resumeThreads(void);

#define PL_THREAD_SUSPEND_AFTER_WORK	0x1 /* forThreadLocalData() */

		 /*******************************
		 *	   ATOMIC COUNTERS	*
		 *******************************/

void			PL_atomic_inc(int *addr);
void			PL_atomic_dec(int *addr);

#else /*O_PLMT, end of threading-stuff */

		 /*******************************
		 *	 NON-THREAD STUFF	*
		 *******************************/

#define GET_LD
#define PRED_LD
#define LOCAL_LD  (&PL_local_data)
#define GLOBAL_LD (&PL_local_data)
#define LD	  GLOBAL_LD

#define PL_LOCK(id)
#define PL_UNLOCK(id)

#endif /*O_PLMT*/

		 /*******************************
		 *	LD-USING FUNCTIONS	*
		 *******************************/

#define allocGlobal(n)		allocGlobal__LD(n PASS_LD)
#define allocHeap(n)		allocHeap__LD(n PASS_LD)
#define freeHeap(p, n)		freeHeap__LD(p, n PASS_LD)
#define freeRecord(r)		freeRecord__LD(r PASS_LD)
#define makeNum(n)		makeNum__LD(n PASS_LD)
#define getInputStream(t, s)	getInputStream__LD(t, s PASS_LD)
#define valReal(w)		valReal__LD(w PASS_LD)
#define compileTermToHeap(t, f)	compileTermToHeap__LD(t, f PASS_LD)

#define _PL_get_arg(n, t, a)	_PL_get_arg__LD(n, t, a PASS_LD)
#define _PL_put_number(t, n) 	_PL_put_number__LD(t, n PASS_LD)
#define PL_new_term_ref()	PL_new_term_ref__LD(PASS_LD1)
#define PL_new_term_refs(n)	PL_new_term_refs__LD(n PASS_LD)
#define PL_unify(t1, t2)	PL_unify__LD(t1, t2 PASS_LD)
#define PL_unify_integer(t, i)	PL_unify_integer__LD(t, i PASS_LD)
#define PL_get_atom(t, a)	PL_get_atom__LD(t, a PASS_LD)
#define PL_put_atom(t, a)	PL_put_atom__LD(t, a PASS_LD)
#define PL_is_functor(t, f)	PL_is_functor__LD(t, f PASS_LD)
#define PL_put_integer(t, i) 	PL_put_integer__LD(t, i PASS_LD)
#define PL_strip_module(q, m, t) PL_strip_module__LD(q, m, t PASS_LD)
#define PL_get_integer(t, i)	PL_get_integer__LD(t, i PASS_LD)
#define PL_get_long(t, i)	PL_get_long__LD(t, i PASS_LD)
#define PL_get_pointer(t, ptr)	PL_get_pointer__LD(t, ptr PASS_LD)
#define PL_put_term(t1, t2)	PL_put_term__LD(t1, t2 PASS_LD)
#define PL_get_functor(t, f)	PL_get_functor__LD(t, f PASS_LD)
#define PL_unify_atom(t, a)	PL_unify_atom__LD(t, a PASS_LD)
#define PL_unify_pointer(t, p)	PL_unify_pointer__LD(t, p PASS_LD)
#define PL_is_variable(t)	PL_is_variable__LD(t PASS_LD)
#define PL_is_atomic(t)		PL_is_atomic__LD(t PASS_LD)
#define PL_get_list(l, h, t)	PL_get_list__LD(l, h, t PASS_LD)
#define PL_is_atom(t)		PL_is_atom__LD(t PASS_LD)
#define PL_unify_list(l, h, t)	PL_unify_list__LD(l, h, t PASS_LD)
#define PL_cons_list(l, h, t)	PL_cons_list__LD(l, h, t PASS_LD)

#endif /*PL_THREAD_H_DEFINED*/
