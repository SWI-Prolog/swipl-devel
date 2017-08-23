/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2017, University of Amsterdam
                              VU University Amsterdam
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

#ifndef PL_THREAD_H_DEFINED
#define PL_THREAD_H_DEFINED

#ifdef O_PLMT
#include <pthread.h>

#ifdef HAVE_SEMA_INIT
#include <synch.h>
#endif

#ifndef __WINDOWS__
#define SIG_ALERT  SIGUSR2
#endif

typedef enum
{ LDATA_IDLE = 0,
  LDATA_SIGNALLED,
  LDATA_ANSWERING,
  LDATA_ANSWERED
} ldata_status_t;

#define THREAD_STATUS_INVALID(s) ((s) == PL_THREAD_UNUSED || \
				  (s) == PL_THREAD_RESERVED)

typedef enum
{ PL_THREAD_UNUSED = 0,			/* no thread on this slot */
  PL_THREAD_RUNNING,			/* a normally running one */
  PL_THREAD_EXITED,			/* died with thread_exit/1 */
  PL_THREAD_SUCCEEDED,			/* finished with Yes */
  PL_THREAD_FAILED,			/* finished with No */
  PL_THREAD_EXCEPTION,			/* finished with exception */
  PL_THREAD_NOMEM,			/* couldn't start due no-memory */
  PL_THREAD_RESERVED,			/* allocated but not yet created */
  PL_THREAD_CREATED,			/* just created */
} thread_status;

#ifdef __WINDOWS__
enum
{ SIGNAL     = 0,
  BROADCAST  = 1,
  MAX_EVENTS = 2
} win32_event_t;

typedef struct
{ HANDLE events[MAX_EVENTS];		/* events to be signalled */
  int    waiters;			/* # waiters */
} win32_cond_t;
#endif

typedef struct _PL_thread_info_t
{ int		    pl_tid;		/* Prolog thread id */
  size_t	    local_size;		/* Stack sizes */
  size_t	    global_size;
  size_t	    trail_size;
  size_t	    stack_size;		/* system (C-) stack */
  int		    (*cancel)(int id);	/* cancel function */
  unsigned short    open_count;		/* for PL_thread_detach_engine() */
  unsigned	    detached      : 1;	/* detached thread */
  unsigned	    debug         : 1;	/* thread can be debugged */
  unsigned	    in_exit_hooks : 1;	/* TRUE: running exit hooks */
  unsigned	    has_tid       : 1;	/* TRUE: tid = valid */
  unsigned	    is_engine	  : 1;	/* TRUE: created as engine */
  thread_status	    status;		/* PL_THREAD_* */
  pthread_t	    tid;		/* Thread identifier */
#ifdef __linux__
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
  win32_cond_t	       cond_var;
  win32_cond_t	       drain_var;
#else
  pthread_cond_t       cond_var;	/* condvar for reading */
  pthread_cond_t       drain_var;	/* condvar for writing */
#endif
  struct thread_message   *head;	/* Head of message queue */
  struct thread_message   *tail;	/* Tail of message queue */
  uint64_t	       sequence_next;	/* next for sequence id */
  word		       id;		/* Id of the queue */
  long		       size;		/* # terms in queue */
  long		       max_size;	/* Max # terms in queue */
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
#ifdef __WINDOWS__
#define L_DDE	       25
#define L_CSTACK       26
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

#define LOCKSRCFILE(sf)		countingMutexLock((sf)->mutex)
#define UNLOCKSRCFILE(sf)	countingMutexUnlock((sf)->mutex)

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
		 *	       WINDOWS		*
		 *******************************/

#define WM_SIGNALLED (WM_USER+4201)	/* how to select a good number!? */


		 /*******************************
		 *	    FUNCTIONS		*
		 *******************************/

COMMON(int)		exitPrologThreads(void);
COMMON(bool)		aliasThread(int tid, atom_t type, atom_t name);
COMMON(word)		pl_thread_create(term_t goal, term_t id,
					 term_t options);
COMMON(word)		pl_thread_exit(term_t retcode);
COMMON(foreign_t)	pl_thread_signal(term_t thread, term_t goal);

COMMON(foreign_t)	pl_thread_at_exit(term_t goal);
extern int		PL_thread_self(void);

COMMON(const char *)	threadName(int id);
COMMON(void)		executeThreadSignals(int sig);
COMMON(foreign_t)	pl_attach_xterm(term_t in, term_t out);
COMMON(int)		attachConsole(void);
COMMON(Definition)	localiseDefinition(Definition def);
COMMON(LocalDefinitions) new_ldef_vector(void);
COMMON(void)		free_ldef_vector(LocalDefinitions ldefs);
COMMON(void)		cleanupLocalDefinitions(PL_local_data_t *ld);
int			PL_mutex_lock(struct pl_mutex *m);
int			PL_mutex_unlock(struct pl_mutex *m);
int			PL_thread_raise(int tid, int sig);
COMMON(void)		cleanupThreads(void);
COMMON(intptr_t)	system_thread_id(PL_thread_info_t *info);
COMMON(double)	        ThreadCPUTime(PL_local_data_t *ld, int which);
COMMON(void)		get_current_timespec(struct timespec *time);
COMMON(void)	        carry_timespec_nanos(struct timespec *time);

		 /*******************************
		 *	 GLOBAL GC SUPPORT	*
		 *******************************/

COMMON(void)	forThreadLocalDataUnsuspended(
		    void (*func)(struct PL_local_data *),
		    unsigned flags);
COMMON(void)	resumeThreads(void);
COMMON(void)	markAtomsMessageQueues(void);
COMMON(void)	markAtomsThreadMessageQueue(PL_local_data_t *ld);

#define acquire_ldata(info)	acquire_ldata__LD(info PASS_LD)
#define release_ldata(ld)	(LD->thread.info->access.ldata = NULL)

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
#define LOCKSRCFILE(sf)
#define UNLOCKSRCFILE(sf)

#define acquire_ldata(ld)	(ld)
#define release_ldata(ld)	(void)0

COMMON(double)	        ThreadCPUTime(PL_local_data_t *ld, int which);

#endif /*O_PLMT*/

		 /*******************************
		 *	       COMMON		*
		 *******************************/

typedef struct
{ functor_t functor;			/* functor of property */
  int (*function)();			/* function to generate */
} tprop;

COMMON(int)	get_prop_def(term_t t, atom_t expected,
			     const tprop *list, const tprop **def);

COMMON(void)	initPrologThreads(void);
COMMON(int)	pl_atom_table_in_use(AtomTable atom_table);
COMMON(int)	pl_atom_bucket_in_use(Atom *atom_bucket);
COMMON(Atom**)	pl_atom_buckets_in_use(void);
COMMON(Definition*)	predicates_in_use(void);
COMMON(int)	pl_functor_table_in_use(FunctorTable functor_table);
COMMON(int)	pl_kvs_in_use(KVS kvs);
COMMON(int)	pushPredicateAccess__LD(Definition def, gen_t gen ARG_LD);
COMMON(void)	popPredicateAccess__LD(Definition def ARG_LD);
COMMON(size_t)	popNPredicateAccess__LD(size_t n ARG_LD);
COMMON(void)	markAccessedPredicates(PL_local_data_t *ld);
COMMON(int)     cgc_thread_stats(cgc_stats *stats ARG_LD);
COMMON(int)	signalGCThread(int sig);
COMMON(int)	isSignalledGCThread(int sig ARG_LD);

#endif /*PL_THREAD_H_DEFINED*/
