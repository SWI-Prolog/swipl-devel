/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  file  defines  just  the   mutexes    needed   by  the  SWI-Prolog
multi-threading support. It is in a separate file because it needs to be
included before pl-stream.h, which in turn   needs to be included early.
The remainder of the thread support must be at the end to exploit access
to the other Prolog data-types.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef PL_MUTEX_H_DEFINED
#define PL_MUTEX_H_DEFINED

#ifdef O_PLMT

#include <pthread.h>

#ifdef RECURSIVE_MUTEXES

typedef pthread_mutex_t recursive_mutex_t;
#define recursive_mutex_lock(mutex) (pthread_mutex_lock(mutex))
#define recursive_mutex_trylock(mutex) (pthread_mutex_trylock(mutex))
#define recursive_mutex_unlock(mutex) (pthread_mutex_unlock(mutex))

#else /*RECURSIVE_MUTEXES*/

typedef struct my_mutex_t {
  pthread_mutex_t lock;
	pthread_t owner;
	unsigned int count;
} recursive_mutex_t;

extern int recursive_mutex_lock(recursive_mutex_t *m);
extern int recursive_mutex_trylock(recursive_mutex_t *m);
extern int recursive_mutex_unlock(recursive_mutex_t *m);
#endif /*RECURSIVE_MUTEXES*/

#endif

#endif /*PL_MUTEX_H_DEFINED*/
