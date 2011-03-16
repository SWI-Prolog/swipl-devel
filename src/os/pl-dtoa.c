/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam

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

#include "pl-incl.h"
#include "pl-dtoa.h"

#define IEEE_8087 1

#define MALLOC PL_malloc
#define FREE PL_free

#ifdef _REENTRANT
#define Long int			/* 32-bits */
#define MULTIPLE_THREADS

/* TBD: Use the pl-thread.[ch] locks for better speed on Windows
*/

static pthread_mutex_t mutex_0 = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mutex_1 = PTHREAD_MUTEX_INITIALIZER;

static inline void
ACQUIRE_DTOA_LOCK(int n)
{ if ( n == 0 )
    pthread_mutex_lock(&mutex_0);
  else
    pthread_mutex_lock(&mutex_1);
}

static inline void
FREE_DTOA_LOCK(int n)
{ if ( n == 0 )
    pthread_mutex_unlock(&mutex_0);
  else
    pthread_mutex_unlock(&mutex_1);
}

#endif /*MULTIPLE_THREADS*/

#include "dtoa.c"
