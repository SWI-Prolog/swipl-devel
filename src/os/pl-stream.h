/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, University of Amsterdam
                         VU University Amsterdam
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

#ifndef PL_STREAM_H_INCLUDED
#define PL_STREAM_H_INCLUDED

#include "SWI-Stream.h"

#ifdef O_PLMT
#define ATOMIC_ADD(ptr, v)		__sync_add_and_fetch(ptr, v)
#define ATOMIC_SUB(ptr, v)		__sync_sub_and_fetch(ptr, v)
#define ATOMIC_INC(ptr)			ATOMIC_ADD(ptr, 1) /* ++(*ptr) */
#define ATOMIC_DEC(ptr)			ATOMIC_SUB(ptr, 1) /* --(*ptr) */
#else
#define ATOMIC_ADD(ptr, v)		(*ptr += v)
#define ATOMIC_SUB(ptr, v)		(*ptr -= v)
#define ATOMIC_INC(ptr)			(++(*ptr))
#define ATOMIC_DEC(ptr)			(--(*ptr))
#endif


#ifdef O_DEBUG_STREAM_REFERENCES

#define Sreference(s) Sreference_(s, __FILE__, __LINE__)
#define Sunreference(s) Sunreference_(s, __FILE__, __LINE__)

void Sreference_(IOSTREAM *s, const char *f, int line);
int  Sunreference_(IOSTREAM *s, const char *f, int line);

#ifdef PL_STREAM_IMPL
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
extern int PL_thread_self(void);

static FILE *debugfd = NULL;

static FILE *
dbgfd(void)
{ if ( !debugfd )
  { static int tried = 0;

    if ( !tried )
    { char *f;

      tried = 1;
      if ( (f = getenv("SWI_DEBUG_IOREF")) )
	debugfd = fopen(f, "w");
    }
  }

  return debugfd;
}

void
dbgflush(void)
{ if ( debugfd )
    fflush(debugfd);
}

void
Sreference_(IOSTREAM *s, const char *f, int line)
{ FILE *fd;

  ATOMIC_INC(&(s)->references);
  assert(s->references);

  if ( (fd = dbgfd()) )
    fprintf(fd, "%p [%3d] %s:%d ++=%d\n",
	    s, PL_thread_self(), f, line, s->references);
}

int
Sunreference_(IOSTREAM *s, const char *f, int line)
{ FILE *fd;
  int rc;

  assert(s->references);
  rc = ATOMIC_DEC(&(s)->references);

  if ( (fd = dbgfd()) )
  { fprintf(fd, "%p [%3d] %s:%d --=%d\n",
	    s, PL_thread_self(), f, line, rc);
  }

  return rc;
}

static void
S__created(IOSTREAM *s)
{ FILE *fd;

  if ( (fd = dbgfd()) )
    fprintf(fd, "%p [%3d] CREATED\n", s, PL_thread_self());
}

static void
S__destroyed(IOSTREAM *s)
{ FILE *fd;

  if ( (fd = dbgfd()) )
    fprintf(fd, "%p [%3d] DESTROYED\n", s, PL_thread_self());
}

#endif /*PL_STREAM_IMPL*/

#else /*O_DEBUG_STREAM_REFERENCES*/

#define Sreference(s)     ATOMIC_INC(&(s)->references)
#define Sunreference(s)   ATOMIC_DEC(&(s)->references)
#define S__created(s)	  (void)0
#define S__destroyed(s)	  (void)0

#endif /*O_DEBUG_STREAM_REFERENCES*/

#endif /*PL_STREAM_H_INCLUDED*/
