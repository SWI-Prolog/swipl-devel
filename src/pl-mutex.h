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
