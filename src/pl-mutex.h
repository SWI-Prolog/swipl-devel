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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  file  defines  just  the   mutexes    needed   by  the  SWI-Prolog
multi-threading support. It is in a separate file because it needs to be
included before pl-stream.h, which in turn   needs to be included early.
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

#if defined(WIN32)
#define USE_CRITICAL_SECTIONS 1
#endif

#include <pthread.h>

#ifdef USE_CRITICAL_SECTIONS
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#define RECURSIVE_MUTEXES 1

#define simpleMutex CRITICAL_SECTION

#define simpleMutexInit(p)	InitializeCriticalSection(p)
#define simpleMutexDelete(p)	DeleteCriticalSection(p)
#define simpleMutexLock(p)	EnterCriticalSection(p)
#define simpleMutexUnlock(p)	LeaveCriticalSection(p)

#define MUTEX_BUSY 0			/* return from unsuccessful */
					/* recursiveMutexTryLock() */
#define MUTEX_OK(g) (g)

#else /* USE_CRITICAL_SECTIONS */

typedef pthread_mutex_t simpleMutex;

#define simpleMutexInit(p)	pthread_mutex_init(p, NULL)
#define simpleMutexDelete(p)	pthread_mutex_destroy(p)
#define simpleMutexLock(p)	pthread_mutex_lock(p)
#define simpleMutexUnlock(p)	pthread_mutex_unlock(p)

#define MUTEX_OK(g) ((g) == 0)
#define MUTEX_BUSY  EBUSY

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

#ifndef USE_CRITICAL_SECTIONS
#define O_CONTENTION_STATISTICS 1
#include <errno.h>
#endif

typedef struct counting_mutex
{ simpleMutex mutex;			/* mutex itself */
  const char *name;			/* name of the mutex */
  unsigned long count;			/* # times locked */
  unsigned long unlocked;		/* # times unlocked */
#ifdef O_CONTENTION_STATISTICS
  unsigned long contention;		/* # contentions */
#endif
  struct counting_mutex *next;		/* next of allocated chain */
} counting_mutex;

extern counting_mutex  *allocSimpleMutex(const char *name);
extern void		freeSimpleMutex(counting_mutex *m);

#endif /*O_PLMT*/
#endif /*PL_MUTEX_H_DEFINED*/
