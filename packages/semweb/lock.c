/*  $Id$

    Part of the SWI-Prolog Semweb package

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "lock.h"
#include "debug.h"
#include <assert.h>
#include <string.h>

static int
permission_error(const char *op, const char *type, const char *obj,
		 const char *msg)
{ term_t ex = PL_new_term_ref();
  term_t ctx = PL_new_term_ref();

  if ( msg )
    PL_unify_term(ctx, PL_FUNCTOR_CHARS, "context", 2,
		         PL_VARIABLE,
		         PL_CHARS, msg);

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "permission_error", 3,
		        PL_CHARS, op,
		        PL_CHARS, type,
		        PL_CHARS, obj,
		      PL_TERM, ctx);

  return PL_raise_exception(ex);
}


#ifdef _REENTRANT


		 /*******************************
		 *	 WINDOWS VERSION	*
		 *******************************/

#ifdef WIN32

#include <windows.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  is  a  slightly  simplified  version  of  the  condition  variable
emulation used in SWI-Prolog. We have   deleted the broadcast facilities
of the CVs as this is not used in this code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int 
win32_cond_init(win32_cond_t *cv)
{ cv->events[SIGNAL]    = CreateEvent(NULL, FALSE, FALSE, NULL);
  cv->waiters = 0;

  return 0;
}


static int
win32_cond_destroy(win32_cond_t *cv)
{ CloseHandle(cv->events[SIGNAL]);

  return 0;
}

#define WAIT_INTR (-1)

static int 
win32_cond_wait(win32_cond_t *cv,
		CRITICAL_SECTION *external_mutex)
{ int rc;

  cv->waiters++;

  LeaveCriticalSection(external_mutex);
  rc = MsgWaitForMultipleObjects(1,
				 cv->events,
				 FALSE,	/* wait for either event */
				 INFINITE,
				 QS_ALLINPUT);
  if ( rc == WAIT_OBJECT_0+1 )
  { MSG msg;

    while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
    { TranslateMessage(&msg);
      DispatchMessage(&msg);
    }

    if ( PL_handle_signals() < 0 )
    { EnterCriticalSection(external_mutex);
      return WAIT_INTR;
    }
  }

  EnterCriticalSection(external_mutex);

  cv->waiters--;

  return 0;
}


static int 
win32_cond_signal(win32_cond_t *cv)	/* must be holding associated mutex */
{ if ( cv->waiters > 0 )
    SetEvent(cv->events[SIGNAL]);

  return 0;
}


int
rdlock(rwlock *lock)
{ int self = PL_thread_self();

  if ( lock->writer == self )
  { lock->lock_level++;			/* read nested in write */

    return TRUE;
  }

  EnterCriticalSection(&lock->mutex);

  if ( lock->allow_readers == TRUE )
  { ok:

    lock->readers++;
    lock->read_by_thread[self]++;
    LeaveCriticalSection(&lock->mutex);

    return TRUE;
  }
  
  lock->waiting_readers++;

  for(;;)
  { int rc = win32_cond_wait(&lock->rdcondvar, &lock->mutex);

    if ( rc == WAIT_INTR )
    { LeaveCriticalSection(&lock->mutex);
      return FALSE;
    } else if ( rc == 0 )
    { if ( lock->allow_readers == TRUE )
      { lock->waiting_readers--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }
}


int
wrlock(rwlock *lock, int allow_readers)
{ int self = PL_thread_self();

  if ( lock->writer == self )		/* recursive write lock, used for */
  { lock->lock_level++;			/* nested transactions */

    return TRUE;
  }

  EnterCriticalSection(&lock->mutex);
  
  if ( lock->writer == -1 && lock->readers == 0 )
  { ok:

    lock->writer = self;
    lock->lock_level = 1;
    lock->allow_readers = allow_readers;
    LeaveCriticalSection(&lock->mutex);
    DEBUG(3, Sdprintf("WRLOCK(%d): OK\n", self));

    return TRUE;
  }

  if ( lock->read_by_thread[self] > 0 )
  { LeaveCriticalSection(&lock->mutex);
    return permission_error("write", "rdf_db", "default",
			    "Operation would deadlock");
  }

  lock->waiting_writers++;
  DEBUG(3, Sdprintf("WRLOCK(%d): waiting ...\n", self));

  for(;;)
  { int rc = win32_cond_wait(&lock->wrcondvar, &lock->mutex);

    if ( rc == WAIT_INTR )
    { LeaveCriticalSection(&lock->mutex);
      return FALSE;
    } else if ( rc == 0 )
    { if ( lock->writer == -1 && lock->readers == 0 )
      { lock->waiting_writers--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }     
}


int
lockout_readers(rwlock *lock)
{ EnterCriticalSection(&lock->mutex);
  
  if ( lock->readers == 0 )
  { ok:

    lock->allow_readers = FALSE;
    LeaveCriticalSection(&lock->mutex);

    return TRUE;
  }

  lock->waiting_upgrade++;

  for(;;)
  { int rc = win32_cond_wait(&lock->upcondvar, &lock->mutex);

    if ( rc == WAIT_INTR )
    { LeaveCriticalSection(&lock->mutex);
      return FALSE;
    } else if ( rc == 0 )
    { if ( lock->readers == 0 )
      { lock->waiting_upgrade--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }
}


void
reallow_readers(rwlock *lock)
{ EnterCriticalSection(&lock->mutex);
  lock->allow_readers = TRUE;
  LeaveCriticalSection(&lock->mutex);
}


int
unlock(rwlock *lock, int rd)
{ int self = PL_thread_self();
  int signal;

  if ( lock->writer == self && lock->lock_level > 1 )
  { lock->lock_level--;
    return TRUE;
  }

  EnterCriticalSection(&lock->mutex);
  if ( rd )				/* must be a read lock */
  { lock->readers--;
    lock->read_by_thread[self]--;
    signal = (lock->readers == 0);
  } else
  { lock->writer = -1;
    lock->allow_readers = TRUE;
    signal = TRUE;
  }

  if ( signal )
  { enum { NONE, READ, WRITE, UPGRADE } waiting;

    waiting = (lock->waiting_upgrade ? UPGRADE :
	       lock->waiting_writers ? WRITE :
	       lock->waiting_readers ? READ : NONE);
  
    switch(waiting)
    { case UPGRADE:
	win32_cond_signal(&lock->upcondvar);
	break;
      case WRITE:
	win32_cond_signal(&lock->wrcondvar);
	break;
      case READ:
	win32_cond_signal(&lock->rdcondvar);
	break;
      default:
	;
    }
  }

  LeaveCriticalSection(&lock->mutex);	/* In our WIN32 emulation we */
					/* must hold the associated mutex */
  return TRUE;
}


int
lock_misc(rwlock *lock)
{ EnterCriticalSection(&lock->misc_mutex);

  return TRUE;
}


int
unlock_misc(rwlock *lock)
{ LeaveCriticalSection(&lock->misc_mutex);
  
  return TRUE;
}


static int
init_lock(rwlock *lock)
{ int bytes;

  InitializeCriticalSection(&lock->mutex);
  InitializeCriticalSection(&lock->misc_mutex);

  if ( !win32_cond_init(&lock->wrcondvar) == 0 ||
       !win32_cond_init(&lock->rdcondvar) == 0 ||
       !win32_cond_init(&lock->upcondvar) == 0 )
  {					/* TBD: System error */
    return FALSE;
  }

  lock->writer          = -1;
  lock->allow_readers   = TRUE;
  lock->readers         = 0;
  lock->waiting_readers = 0;
  lock->waiting_writers = 0;
  lock->waiting_upgrade = 0;
  lock->lock_level      = 0;

  bytes = sizeof(int)*PL_query(PL_QUERY_MAX_THREADS);

  lock->read_by_thread = rdf_malloc(db, bytes);
  memset(lock->read_by_thread, 0, bytes);

  return TRUE;
}

#else /*WIN32*/

		 /*******************************
		 *	   POSIX VERSION	*
		 *******************************/

#include <pthread.h>
#include <errno.h>

int
rdlock(rwlock *lock)
{ int self = PL_thread_self();

  if ( lock->writer == self )
  { lock->lock_level++;			/* read nested in write */

    return TRUE;
  }

  pthread_mutex_lock(&lock->mutex);

  if ( lock->allow_readers == TRUE )
  { ok:

    lock->readers++;
    lock->read_by_thread[self]++;
    pthread_mutex_unlock(&lock->mutex);

    return TRUE;
  }
  
  lock->waiting_readers++;

  for(;;)
  { int rc = pthread_cond_wait(&lock->rdcondvar, &lock->mutex);

    if ( rc == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
      continue;
    } else if ( rc == 0 )
    { if ( lock->allow_readers == TRUE )
      { lock->waiting_readers--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
WRLOCK() and LOCKOUT_READERS() can be  used   in  two ways. Conventional
write locks are established using   WRLOCK(db,  FALSE) ... WRUNLOCK(db).
For transactions, we allow concurrent  readers   until  we  are ready to
commit, in which case we use  WRLOCK(db, TRUE) ... LOCKOUT_READERS() ...
WRUNLOCK(db)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
wrlock(rwlock *lock, int allow_readers)
{ int self = PL_thread_self();

  if ( lock->writer == self )		/* recursive write lock, used for */
  { lock->lock_level++;			/* nested transactions */

    return TRUE;
  }

  pthread_mutex_lock(&lock->mutex);
  
  if ( lock->writer == -1 && lock->readers == 0 )
  { ok:

    lock->writer = self;
    lock->lock_level = 1;
    lock->allow_readers = allow_readers;
    pthread_mutex_unlock(&lock->mutex);
    DEBUG(3, Sdprintf("WRLOCK(%d): OK\n", self));

    return TRUE;
  }

  if ( lock->read_by_thread[self] > 0 )
  { DEBUG(1, Sdprintf("SELF(%d) has %d readers\n",
		      self, lock->read_by_thread[self]));
    pthread_mutex_unlock(&lock->mutex);
    return permission_error("write", "rdf_db", "default",
			    "Operation would deadlock");
  }

  lock->waiting_writers++;
  DEBUG(3, Sdprintf("WRLOCK(%d): waiting ...\n", self));

  for(;;)
  { int rc = pthread_cond_wait(&lock->wrcondvar, &lock->mutex);

    if ( rc == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
      continue;
    } else if ( rc == 0 )
    { if ( lock->writer == -1 && lock->readers == 0 )
      { lock->waiting_writers--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }     
}


int
lockout_readers(rwlock *lock)
{ pthread_mutex_lock(&lock->mutex);
  
  if ( lock->readers == 0 )
  { ok:

    lock->allow_readers = FALSE;
    pthread_mutex_unlock(&lock->mutex);

    return TRUE;
  }

  lock->waiting_upgrade++;

  for(;;)
  { int rc = pthread_cond_wait(&lock->upcondvar, &lock->mutex);

    if ( rc == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
      continue;
    } else if ( rc == 0 )
    { if ( lock->readers == 0 )
      { lock->waiting_upgrade--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }
}


void
reallow_readers(rwlock *lock)
{ pthread_mutex_lock(&lock->mutex);
  lock->allow_readers = TRUE;
  pthread_mutex_unlock(&lock->mutex);
}


int
unlock(rwlock *lock, int rd)		/* TRUE: read lock */
{ int self = PL_thread_self();
  int signal;

  if ( lock->writer == self && lock->lock_level > 1 )
  { lock->lock_level--;
    return TRUE;
  }

  pthread_mutex_lock(&lock->mutex);
  if ( rd )				/* read lock */
  { lock->readers--;
    lock->read_by_thread[self]--;
    signal = (lock->readers == 0);
  } else
  { lock->writer = -1;
    lock->allow_readers = TRUE;
    signal = TRUE;
  }

  if ( signal )
  { enum { NONE, READ, WRITE, UPGRADE } waiting;

    waiting = (lock->waiting_upgrade ? UPGRADE :
	       lock->waiting_writers ? WRITE :
	       lock->waiting_readers ? READ : NONE);
    pthread_mutex_unlock(&lock->mutex);
  
    switch(waiting)
    { case UPGRADE:
	pthread_cond_signal(&lock->upcondvar);
	break;
      case WRITE:
	pthread_cond_signal(&lock->wrcondvar);
	break;
      case READ:
	pthread_cond_signal(&lock->rdcondvar);
	break;
      default:
	;
    }
  } else
  { pthread_mutex_unlock(&lock->mutex);
  }
  
  return TRUE;
}


int
lock_misc(rwlock *lock)
{ return pthread_mutex_lock(&lock->misc_mutex) == 0;
}


int
unlock_misc(rwlock *lock)
{ return pthread_mutex_unlock(&lock->misc_mutex) == 0;
}


int
init_lock(rwlock *lock)
{ int bytes;
  int maxthreads;

  if ( !pthread_mutex_init(&lock->mutex, NULL) == 0 ||
       !pthread_mutex_init(&lock->misc_mutex, NULL) == 0 ||
       !pthread_cond_init(&lock->wrcondvar, NULL) == 0 ||
       !pthread_cond_init(&lock->rdcondvar, NULL) == 0 ||
       !pthread_cond_init(&lock->upcondvar, NULL) == 0 )
  {					/* TBD: System error */
    return FALSE;
  }

  lock->writer          = -1;
  lock->readers	      = 0;
  lock->allow_readers   = TRUE;
  lock->waiting_readers = 0;
  lock->waiting_writers = 0;
  lock->waiting_upgrade = 0;
  lock->lock_level      = 0;

  maxthreads = PL_query(PL_QUERY_MAX_THREADS);
  bytes = sizeof(int)*maxthreads;
  DEBUG(1, Sdprintf("MAX_THREADS = %d\n", maxthreads));
  if ( !(lock->read_by_thread = malloc(bytes)) )
    return FALSE;
  memset(lock->read_by_thread, 0, bytes);

  return TRUE;
}

#endif /*WIN32*/


#else /*_REENTRANT*/

int
rdlock(rwlock *lock)
{ lock->readers++;

  return TRUE;
}

int
wrlock(rwlock *lock, int allow_readers)
{ if ( lock->readers )
    return permission_error("write", "rdf_db", "default",
			    "Operation would deadlock");

  lock->writer = 0;

  return TRUE;
}

int
unlock(rwlock *lock, int rd)
{ if ( rd )
  { lock->readers--;
  } else
  { lock->writer = -1;
  }

  return TRUE;
}


int
lock_misc(rwlock *lock)
{ return TRUE;
}


int
unlock_misc(rwlock *lock)
{ return TRUE;
}


int
init_lock(rwlock *lock)
{ lock->writer = -1;
  lock->readers = 0;

  return TRUE;
}

#endif /*_REENTRANT*/

