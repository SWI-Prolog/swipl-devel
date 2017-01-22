/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2015, University of Amsterdam
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
#include "pl-dtoa.h"

#ifdef WORDS_BIGENDIAN
#define IEEE_MC68k 1
#else
#define IEEE_8087 1
#endif

#define MALLOC PL_malloc
#define FREE PL_free

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Long must be a 32-bit int.  For now we use int.  Ideally we would use
int32_t, but MS does not yet support stdint.h.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define Long int			/* 32-bits */

#if defined(_REENTRANT) && defined(O_PLMT)
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
