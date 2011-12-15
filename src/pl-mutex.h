/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

#ifndef PL_MUTEX_H_DEFINED
#define PL_MUTEX_H_DEFINED

#ifdef O_PLMT

#if defined(__WINDOWS__)
#define USE_CRITICAL_SECTIONS 1
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0400		/* get TryEnterCriticalSection() */
#endif
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

#ifdef RECURSIVE_MUTEXES
typedef pthread_mutex_t recursiveMutex;

#define NEED_RECURSIVE_MUTEX_INIT 1
extern int recursiveMutexInit(recursiveMutex *m);
#define recursiveMutexDelete(p)	 pthread_mutex_destroy(p)
#define recursiveMutexLock(p)	 pthread_mutex_lock(p)
#define recursiveMutexTryLock(p) pthread_mutex_trylock(p)
#define recursiveMutexUnlock(p)	 pthread_mutex_unlock(p)

#else /*RECURSIVE_MUTEXES*/

typedef struct {
  pthread_mutex_t lock;
	pthread_t owner;
	unsigned int count;
} recursiveMutex;

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
  const char *name;			/* name of the mutex */
  long count;				/* # times locked */
  long unlocked;			/* # times unlocked */
#ifdef O_CONTENTION_STATISTICS
  long collisions;			/* # contentions */
#endif
  struct counting_mutex *next;		/* next of allocated chain */
} counting_mutex;

extern counting_mutex  *allocSimpleMutex(const char *name);
extern void		freeSimpleMutex(counting_mutex *m);

#else /*O_PLMT*/

#define simpleMutexLock(p)	(void)0
#define simpleMutexUnlock(p)	(void)0

#endif /*O_PLMT*/

#endif /*PL_MUTEX_H_DEFINED*/
