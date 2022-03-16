/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2022, University of Amsterdam
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

#include "pl-incl.h"

#ifndef PL_THREAD_H_DEFINED
#define PL_THREAD_H_DEFINED
#include "pl-mutex.h"
#include "os/pl-buffer.h"

#ifdef O_PLMT
#include <pthread.h>

#ifdef HAVE_SEMA_INIT
#include <synch.h>
#endif

#ifndef __WINDOWS__
#define SIG_ALERT  SIGUSR2
#if HAVE_STDC_THREADS
/* Use a C11 condition variable to do cross-thread alerting, if signal
 * support is compiled out (!O_SIGNALS) or disabled (--sigalert=0)
 */
#define STDC_CV_ALERT 1
#endif
#endif

#if defined(__linux__) || defined(__CYGWIN__)
#define PID_IDENTIFIES_THREAD 1
#endif

#define GCREQUEST_AGC   0x01		/* GD->thread.gc.requests */
#define GCREQUEST_CGC   0x02
#define GCREQUEST_ABORT 0x04

#define EXIT_REQ_PROCESS 1
#define EXIT_REQ_THREAD  2

typedef enum
{ LDATA_IDLE = 0,
  LDATA_SIGNALLED,
  LDATA_ANSWERING,
  LDATA_ANSWERED
} ldata_status_t;

#define THREAD_STATUS_INVALID(s) ((int)(s) < (int)PL_THREAD_RUNNING)

typedef enum
{ PL_THREAD_UNUSED = 0,			/* no thread on this slot */
  PL_THREAD_RESERVED,			/* allocated but not yet created */
  PL_THREAD_JOINED,			/* just joined */
					/* normal states */
  PL_THREAD_RUNNING,			/* a normally running one */
  PL_THREAD_EXITED,			/* died with thread_exit/1 */
  PL_THREAD_SUCCEEDED,			/* finished with Yes */
  PL_THREAD_FAILED,			/* finished with No */
  PL_THREAD_EXCEPTION,			/* finished with exception */
  PL_THREAD_NOMEM,			/* couldn't start due no-memory */
  PL_THREAD_CREATED,			/* just created */
} thread_status;

typedef struct _PL_thread_info_t
{ int		    pl_tid;		/* Prolog thread id */
  size_t	    stack_limit;	/* Stack sizes */
  size_t	    table_space;	/* Max size for local tables */
  size_t	    c_stack_size;	/* system (C-) stack */
  rc_cancel	    (*cancel)(int id);	/* cancel function */
  unsigned short    open_count;		/* for PL_thread_detach_engine() */
  unsigned	    detached      : 1;	/* detached thread */
  unsigned	    debug         : 1;	/* thread can be debugged */
  unsigned	    in_exit_hooks : 1;	/* TRUE: running exit hooks */
  unsigned	    has_tid       : 1;	/* TRUE: tid = valid */
  unsigned	    is_engine	  : 1;	/* TRUE: created as engine */
  int		    joining_by;		/* TID of joining thread */
  thread_status	    status;		/* PL_THREAD_* */
  pthread_t	    tid;		/* Thread identifier */
#ifdef PID_IDENTIFIES_THREAD
  pid_t		    pid;		/* for identifying */
#endif
#ifdef __WINDOWS__
  DWORD		    w32id;		/* Win32 thread HANDLE */
#endif
  struct PL_local_data  *thread_data;	/* The thread-local data  */
  module_t	    module;		/* Module for starting goal */
  record_t	    goal;		/* Goal to start thread */
  record_t	    return_value;	/* Value (term) returned */
  atom_t	    symbol;		/* thread_handle symbol */
  struct _PL_thread_info_t *next_free;	/* Next in free list */

					/* lock-free access to data */
  struct
  { KVS		    kvs;		/* current hash-table map accessed */
    AtomTable	    atom_table;		/* current atom-table accessed */
    Atom *	    atom_bucket;	/* current atom bucket-list accessed */
    FunctorTable    functor_table;	/* current atom-table accessed */
    Definition	    predicate;		/* current predicate walked */
    struct PL_local_data *ldata;	/* current ldata accessed */
  } access;
} PL_thread_info_t;


#define TH_IS_INTERACTOR	0x0001	/* Thread is an interactor (engine) */
#define TH_INTERACTOR_NOMORE	0x0002	/* No more answers */
#define TH_INTERACTOR_DONE	0x0004	/* Answered last */

typedef struct thread_handle
{ PL_thread_info_t     *info;		/* represented engine */
  atom_t		symbol;		/* associated symbol */
  atom_t		alias;		/* alias name of the thread */
  int			engine_id;	/* numeric engine id */
  int			flags;		/* symbol flags */
  struct
  { qid_t query;			/* Query handle */
    term_t argv;			/* Arguments */
    record_t package;			/* Exchanged term */
    simpleMutex *mutex;			/* Sync access */
    atom_t thread;			/* Associated thread */
  } interactor;
  struct thread_handle *next_free;	/* Free for GC */
} thread_handle;


#define QTYPE_THREAD	0
#define QTYPE_QUEUE	1

typedef struct message_queue
{ simpleMutex	       mutex;		/* Message queue mutex */
#ifdef __WINDOWS__
  CONDITION_VARIABLE   cond_var;
  CONDITION_VARIABLE   drain_var;
#else
  pthread_cond_t       cond_var;	/* condvar for reading */
  pthread_cond_t       drain_var;	/* condvar for writing */
#endif
  struct thread_message   *head;	/* Head of message queue */
  struct thread_message   *tail;	/* Tail of message queue */
  uint64_t	       sequence_next;	/* next for sequence id */
  word		       id;		/* Id of the queue */
  size_t	       size;		/* # terms in queue */
  size_t	       max_size;	/* Max # terms in queue */
  int		       waiting;		/* # waiting threads */
  int		       waiting_var;	/* # waiting with unbound */
  int		       wait_for_drain;	/* # threads waiting for write */
  unsigned	anonymous : 1;		/* <message_queue>(0x...) */
  unsigned	initialized : 1;	/* Queue is initialised */
  unsigned	destroyed : 1;		/* Thread is being destroyed */
  unsigned	type : 2;		/* QTYPE_* */
#ifdef O_ATOMGC
  simpleMutex          gc_mutex;	/* Atom GC scanning sychronization */
#endif
} message_queue;

typedef struct pl_mutex
{ pthread_mutex_t mutex;		/* the system mutex */
  int count;				/* lock count */
  int owner;				/* integer id of owner */
  atom_t id;				/* id of the mutex */
  unsigned anonymous    : 1;		/* <mutex>(0x...) */
  unsigned initialized  : 1;		/* Mutex is initialized */
  unsigned destroyed    : 1;		/* Mutex is destroyed */
  unsigned auto_destroy	: 1;		/* asked to destroy */
} pl_mutex;

#define ALERT_QUEUE_RD	1
#define ALERT_QUEUE_WR	2
#if STDC_CV_ALERT
# define ALERT_LOCK_CV	3
#endif

typedef struct alert_channel
{ int	type;				/* Type of channel */
  union
  { message_queue *queue;
  } obj;
} alert_channel;

#define PL_THREAD_MAGIC 0x2737234f

extern counting_mutex _PL_mutexes[];	/* Prolog mutexes */

#define L_MISC		0
#define L_ALLOC		1
#define L_REHASH_ATOMS	2
#define L_FLAG	        3
#define L_FUNCTOR	4
#define L_RECORD	5
#define L_THREAD	6
#define L_MUTEX		7
#define L_PREDICATE	8
#define L_MODULE	9
#define L_SRCFILE      10
#define L_TABLE	       11
#define L_BREAK	       12
#define L_FILE	       13
#define L_SEETELL      14
#define L_PLFLAG       15
#define L_OP	       16
#define L_INIT	       17
#define L_TERM	       18
#define L_FOREIGN      19
#define L_OS	       20
#define L_LOCALE       21
#define L_SORTR        22
#define L_UMUTEX       23
#define L_INIT_ATOMS   24
#define L_CGCGEN       25
#define L_EVHOOK       26
#define L_OSDIR	       27
#define L_ALERT	       28
#define L_GENERATION   29
#ifdef __WINDOWS__
#define L_DDE	       30
#define L_CSTACK       31
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The IF_MT(id, g) macro  is  used  to   bypass  mutexes  if  threading  is
disabled. We cannot do this for the L_THREAD mutex however as we need to
control when threads can be created.

We  assume  id  ==  L_THREAD  is  optimized  away  if  id  is  known  at
compile-time
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define IF_MT(id, g) if ( id == L_THREAD || GD->thread.enabled ) g

static inline void
countingMutexLock(counting_mutex *cm)
{
#if O_CONTENTION_STATISTICS
  if ( !simpleMutexTryLock(&cm->mutex) )
  { cm->collisions++;
    simpleMutexLock(&cm->mutex);
  }
#else
  simpleMutexLock(&cm->mutex);
#endif

  cm->count++;
  cm->lock_count++;
}

static inline void
countingMutexUnlock(counting_mutex *cm)
{ assert(cm->lock_count > 0);
  cm->lock_count--;
  simpleMutexUnlock(&cm->mutex);
}

#ifdef O_DEBUG_MT
#define PL_LOCK(id) \
	do { Sdprintf("[%s] %s:%d: LOCK(%s)\n", \
		      threadName(0), \
		      __BASE_FILE__, __LINE__, #id); \
             IF_MT(id, countingMutexLock(&_PL_mutexes[id])); \
	   } while(0)
#define PL_UNLOCK(id) \
	do { Sdprintf("[%s] %s:%d: UNLOCK(%s)\n", \
		      threadName(0), \
		      __BASE_FILE__, __LINE__, #id); \
	     IF_MT(id, countingMutexUnlock(&_PL_mutexes[id])); \
	   } while(0)
#else
#define PL_LOCK(id)   IF_MT(id, countingMutexLock(&_PL_mutexes[id]))
#define PL_UNLOCK(id) IF_MT(id, countingMutexUnlock(&_PL_mutexes[id]))
#endif

#define LOCKDEF(def)   PL_LOCK(L_PREDICATE)
#define UNLOCKDEF(def) PL_UNLOCK(L_PREDICATE)

#define LOCKMODULE(module)	countingMutexLock((module)->mutex)
#define UNLOCKMODULE(module)	countingMutexUnlock((module)->mutex)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
				Thread-local data

All  thread-local  data  is  combined  in    one  structure  defined  in
pl-global.h. If Prolog is compiled for single-threading this is a simple
global variable and the macro LD is defined   to  pick up the address of
this variable. In multithreaded context,  POSIX pthread_getspecific() is
used to get separate versions for each thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *   NATIVE THREAD-LOCAL DATA	*
		 *******************************/

#ifdef HAVE___THREAD
extern __thread PL_local_data_t *GLOBAL_LD;
#define TLD_set_LD(v) (GLOBAL_LD = (v))
#else

#ifdef __WINDOWS__
typedef DWORD	TLD_KEY;

#define TLD_alloc(p)	(*(p) = TlsAlloc())
#define TLD_get(p)	TlsGetValue((p))
#define TLD_set(p, v)	TlsSetValue((p), (v))
#define TLD_free(p)	TlsFree(p);

#else
typedef pthread_key_t TLD_KEY;

#define TLD_alloc(p)	pthread_key_create(p, NULL)
#define TLD_get(p)	pthread_getspecific(p)
#define TLD_set(p, v)	pthread_setspecific((p), (v))
#define TLD_free(p)	pthread_key_delete(p)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If available, use GCC's __attribute((const)) to tell the compiler it may
choose to store the result of LD is a local variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern TLD_KEY PL_ldata;		/* key to local data */

#define GLOBAL_LD ((PL_local_data_t *)TLD_get(PL_ldata))
#define TLD_set_LD(v) TLD_set(PL_ldata, v)
#endif /*HAVE___THREAD*/


		 /*******************************
		 *	    WAIT SUPPORT	*
		 *******************************/

typedef struct thread_dcell
{ PL_local_data_t     *ld;		/* The thread */
  struct thread_dcell *next;		/* next in chain */
  struct thread_dcell *prev;		/* previous in chain */
} thread_dcell;

typedef enum twf_type
{ TWF_DB,
  TWF_MODULE,
  TWF_PREDICATE
} twf_type;

typedef struct thread_wait_channel
{ twf_type	type;			/* TWF_* */
  int		signalled;		/* Is signalled */
  int		flags;			/* TWF_ASSERT/RETRACT */
  gen_t		generation;		/* Overall generation */
  union
  { void       *any;
    Module      module;			/* Module */
    Definition  predicate;		/* Predicate */
  } obj;
} thread_wait_channel;

typedef struct thread_wait_for		/* thread data for wait/update */
{ buffer	channels;		/* channels we listen on */
  int		signalled;		/* are we signalled? */
  thread_dcell *registered;		/* cell in thread_wait_area */
  struct module_cell *updating;		/* thread_update/2 modules */
} thread_wait_for;

typedef struct thread_wait_area		/* module data for wait/update */
{ simpleMutex	mutex;
#ifdef __WINDOWS__
  CONDITION_VARIABLE cond;
#else
  pthread_cond_t     cond;
#endif
  thread_dcell *w_head;			/* waiting thread head */
  thread_dcell *w_tail;			/* waiting thread tail */
} thread_wait_area;



#define wakeupThreads(def, wflags) \
	do \
	{ if ( def->module->wait && def->module->wait->w_head ) \
	  { thread_wait_channel wch = { .type = TWF_PREDICATE, \
				        .obj.any = def, \
					.flags = wflags \
				      }; \
	    signal_waiting_threads(def->module, &wch); \
	  } \
	} while(0)

#define wakeupThreadsModule(module, wflags) \
	do \
	{ if ( module->wait && module->wait->w_head ) \
	  { thread_wait_channel wch = { .type = TWF_MODULE, \
				        .obj.any = module, \
					.flags = wflags \
				      }; \
	    signal_waiting_threads(module, &wch); \
	  } \
	} while(0)

		 /*******************************
		 *	       WINDOWS		*
		 *******************************/

#define WM_SIGNALLED (WM_USER+4201)	/* how to select a good number!? */


		 /*******************************
		 *	    FUNCTIONS		*
		 *******************************/

int		exitPrologThreads(void);
bool		aliasThread(int tid, atom_t type, atom_t name);
word		pl_thread_create(term_t goal, term_t id,
				 term_t options);
word		pl_thread_exit(term_t retcode);

foreign_t	pl_thread_at_exit(term_t goal);
int		PL_thread_self(void);
#ifdef O_PLMT
int		unify_thread_id(term_t id, PL_thread_info_t *info);
#endif
int		enableThreads(int enable);


const char *	threadName(int id);
void		executeThreadSignals(int sig);
foreign_t	pl_attach_xterm(term_t in, term_t out);
int		attachConsole(void);
Definition	localiseDefinition(Definition def);
LocalDefinitions new_ldef_vector(void);
void		free_ldef_vector(LocalDefinitions ldefs);
void		cleanupLocalDefinitions(PL_local_data_t *ld);
void		destroyLocalDefinitions(Definition def);
int		PL_mutex_lock(struct pl_mutex *m);
int		PL_mutex_unlock(struct pl_mutex *m);
int		PL_thread_raise(int tid, int sig);
void		cleanupThreads(void);
intptr_t	system_thread_id(PL_thread_info_t *info);
void		get_current_timespec(struct timespec *time);
void	        carry_timespec_nanos(struct timespec *time);
int		signal_waiting_threads(Module m, thread_wait_channel *wch);
void		free_wait_area(thread_wait_area *wa);
void		free_thread_wait(PL_local_data_t *ld);
void		free_predicate_references(PL_local_data_t *ld);

		 /*******************************
		 *	 GLOBAL GC SUPPORT	*
		 *******************************/

void	forThreadLocalDataUnsuspended(
		    void (*func)(struct PL_local_data *, void *ctx),
		    void *ctx);
void	resumeThreads(void);
void	markAtomsMessageQueues(void);
void	markAtomsThreadMessageQueue(PL_local_data_t *ld);

#define acquire_ldata(info)	LDFUNC(acquire_ldata, info)
#define release_ldata(ld)	(LD->thread.info->access.ldata = NULL)
/* defined in pl-inline.h */
static inline PL_local_data_t *acquire_ldata(DECL_LD PL_thread_info_t *info);

		 /*******************************
		 *     CONDITION VARIABLES	*
		 *******************************/

#define CV_READY	0		/* was signalled */
#define CV_MAYBE	1		/* might be signalled */
#define CV_TIMEDOUT	2		/* timed out */
#define CV_INTR		3		/* interrupted */

#ifdef __WINDOWS__

int	cv_timedwait(message_queue *queue,
		     CONDITION_VARIABLE *cv,
		     CRITICAL_SECTION *external_mutex,
		     struct timespec *deadline,
		     const struct timespec *retry_every);

#define cv_broadcast(cv)	WakeAllConditionVariable(cv)
#define cv_signal(cv)		WakeConditionVariable(cv)
#define cv_init(cv,p)		InitializeConditionVariable(cv)
#define cv_destroy(cv)		(void)cv

#else

int	cv_timedwait(message_queue *queue,
		     pthread_cond_t *cv,
		     pthread_mutex_t *external_mutex,
		     struct timespec *deadline,
		     const struct timespec *retry_every);

#define cv_broadcast(cv)	pthread_cond_broadcast(cv)
#define cv_signal(cv)		pthread_cond_signal(cv)
#define cv_init(cv,p)		pthread_cond_init(cv,p)
#define cv_destroy(cv)		pthread_cond_destroy(cv)

#endif /* __WINDOWS__ */

#define cv_wait(cv, m)		cv_timedwait(NULL, cv, m, NULL, NULL)


#else /*O_PLMT, end of threading-stuff */

		 /*******************************
		 *	 NON-THREAD STUFF	*
		 *******************************/

#ifdef O_MULTIPLE_ENGINES

#define GLOBAL_LD PL_current_engine_ptr

#else /*O_MULTIPLE_ENGINES*/

#define GLOBAL_LD (&PL_local_data)

#endif /*O_MULTIPLE_ENGINES*/

#define PL_LOCK(id)
#define PL_UNLOCK(id)

#define LOCKDEF(def)
#define UNLOCKDEF(def)
#define UNLOCKDYNDEF(def)
#define LOCKMODULE(module)
#define UNLOCKMODULE(module)

#define acquire_ldata(ld)	(ld)
#define release_ldata(ld)	(void)0

#endif /*O_PLMT*/

		 /*******************************
		 *	       COMMON		*
		 *******************************/

foreign_t	pl_thread_self(term_t self);

#define TWF_ASSERT	0x0001		/* Predicate actions */
#define TWF_RETRACT	0x0002

typedef struct
{ functor_t functor;			/* functor of property */
  /* FIXME: we should not be storing function pointers with
   * incomplete prototypes! WASM in particular cannot call a
   * function unless it has a full, correct prototype
   */
  int (*function)();			/* function to generate */
} tprop;

#if USE_LD_MACROS
#define	pushPredicateAccessObj(def)	LDFUNC(pushPredicateAccessObj, def)
#define	popPredicateAccess(def)		LDFUNC(popPredicateAccess, def)
#define	popNPredicateAccess(n)		LDFUNC(popNPredicateAccess, n)
#define	cgc_thread_stats(stats)		LDFUNC(cgc_thread_stats, stats)
#define	isSignalledGCThread(sig)	LDFUNC(isSignalledGCThread, sig)
#define	ThreadCPUTime(which)		LDFUNC(ThreadCPUTime, which)
#define updatePendingThreadSignals(_)	LDFUNC(updatePendingThreadSignals, _)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int		get_prop_def(term_t t, atom_t expected,
			     const tprop *list, const tprop **def);
void		initPrologThreads(void);
int		pl_atom_table_in_use(AtomTable atom_table);
int		pl_atom_bucket_in_use(Atom *atom_bucket);
Atom**		pl_atom_buckets_in_use(void);
Definition*	predicates_in_use(void);
int		pl_functor_table_in_use(FunctorTable functor_table);
int		pl_kvs_in_use(KVS kvs);
definition_ref* pushPredicateAccessObj(Definition def);
void		popPredicateAccess(Definition def);
size_t		popNPredicateAccess(size_t n);
void		markAccessedPredicates(PL_local_data_t *ld);
int		cgc_thread_stats(cgc_stats *stats);
int		signalGCThread(int sig);
int		isSignalledGCThread(int sig);
double	        ThreadCPUTime(int which);
void		updatePendingThreadSignals(void);

#undef LDFUNC_DECLARATIONS

#endif /*PL_THREAD_H_DEFINED*/
