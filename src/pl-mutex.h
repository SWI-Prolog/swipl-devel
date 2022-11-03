/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2020, University of Amsterdam
			      CWI, Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  file  defines  just  the   mutexes    needed   by  the  SWI-Prolog
multi-threading support. It is in a separate file because it needs to be
included before SWI-Stream.h, which in turn  needs to be included early.
The remainder of the thread support must be at the end to exploit access
to the other Prolog data-types.

To allow for multiple thread-implementations, we  do not use plain POSIX
mutex-primitives in the remainder of  the   code.  Instead,  mutexes are
controlled using the following macros:

	type simpleMutex	Non-recursive mutex
	type recursiveMutex	Recursive mutex

	simpleMutexInit(p)	Initialise a simple mutex
	simpleMutexDelete(p)	Delete a simple mutex
	simpleMutexLock(p)	Lock a simple mutex
	simpleMutexTryLock(p)	Try Lock a simple mutex
	simpleMutexUnlock(p)	unlock a simple mutex

	recursiveMutexInit(p)	Initialise a recursive mutex
	recursiveMutexDelete(p)	Delete a recursive mutex
	recursiveMutexLock(p)	Lock a recursive mutex
	recursiveMutexTryLock(p) Try Lock a recursive mutex
	recursiveMutexUnlock(p)	unlock a recursive mutex
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-incl.h"

#ifndef PL_MUTEX_H_DEFINED
#define PL_MUTEX_H_DEFINED

void		initMutexes(void);
foreign_t	pl_with_mutex(term_t mutex, term_t goal);

#ifdef O_PLMT

int		get_mutex(term_t t, pl_mutex **mutex, int create);

#if defined(__WINDOWS__)
#define USE_CRITICAL_SECTIONS 1
#endif

#include <pthread.h>

#ifdef USE_CRITICAL_SECTIONS
#ifndef WINDOWS_LEAN_AND_MEAN
#define WINDOWS_LEAN_AND_MEAN
#endif
#include <winsock2.h>

#include <windows.h>
#define RECURSIVE_MUTEXES 1

#define simpleMutex CRITICAL_SECTION

#define simpleMutexInit(p)	InitializeCriticalSection(p)
#define simpleMutexDelete(p)	DeleteCriticalSection(p)
#define simpleMutexLock(p)	EnterCriticalSection(p)
#if _WIN32_WINNT >= 0x0400
#define simpleMutexTryLock(p)	TryEnterCriticalSection(p)
#endif
#define simpleMutexUnlock(p)	LeaveCriticalSection(p)

#else /* USE_CRITICAL_SECTIONS */

typedef pthread_mutex_t simpleMutex;

#define simpleMutexInit(p)	pthread_mutex_init(p, NULL)
#define simpleMutexDelete(p)	pthread_mutex_destroy(p)
#define simpleMutexLock(p)	pthread_mutex_lock(p)
#define simpleMutexTryLock(p)	(pthread_mutex_trylock(p) == 0)
#define simpleMutexUnlock(p)	pthread_mutex_unlock(p)

#endif /*USE_CRITICAL_SECTIONS*/

/* This struct name also appears in SWI-Stream.h */
typedef struct recursiveMutex {
  pthread_mutex_t lock;
#ifndef RECURSIVE_MUTEXES
	pthread_t owner;
     unsigned int count;
#endif
} recursiveMutex;

#ifdef RECURSIVE_MUTEXES

#define NEED_RECURSIVE_MUTEX_INIT 1
extern int recursiveMutexInit(recursiveMutex *m);
#define recursiveMutexDelete(p)	 pthread_mutex_destroy(&((p)->lock))
#define recursiveMutexLock(p)	 pthread_mutex_lock(&((p)->lock))
#define recursiveMutexTryLock(p) pthread_mutex_trylock(&((p)->lock))
#define recursiveMutexUnlock(p)	 pthread_mutex_unlock(&((p)->lock))

#else /*RECURSIVE_MUTEXES*/

#define NEED_RECURSIVE_MUTEX_INIT 1
#define NEED_RECURSIVE_MUTEX_DELETE 1
extern int recursiveMutexInit(recursiveMutex *m);
extern int recursiveMutexDelete(recursiveMutex *m);
extern int recursiveMutexLock(recursiveMutex *m);
extern int recursiveMutexTryLock(recursiveMutex *m);
extern int recursiveMutexUnlock(recursiveMutex *m);

#endif /*RECURSIVE_MUTEXES*/

#ifdef simpleMutexTryLock
#define O_CONTENTION_STATISTICS 1
#ifndef USE_CRITICAL_SECTIONS
#include <errno.h>
#endif
#endif

typedef struct counting_mutex
{ simpleMutex mutex;			/* mutex itself */
  const char  *name;			/* name of the mutex */
  uint64_t     count;			/* # times locked */
  unsigned int lock_count;		/* # times unlocked */
#ifdef O_CONTENTION_STATISTICS
  unsigned int collisions;		/* # contentions */
#endif
  struct counting_mutex *next;		/* next of allocated chain */
  struct counting_mutex *prev;		/* prvious in allocated chain */
} counting_mutex;

extern counting_mutex  *allocSimpleMutex(const char *name);
extern void		initSimpleMutex(counting_mutex *m, const char *name);
extern void		deleteSimpleMutex(counting_mutex *m);
extern void		freeSimpleMutex(counting_mutex *m);

#else /*O_PLMT*/

#define simpleMutexLock(p)	(void)0
#define simpleMutexUnlock(p)	(void)0

#endif /*O_PLMT*/

#endif /*PL_MUTEX_H_DEFINED*/
