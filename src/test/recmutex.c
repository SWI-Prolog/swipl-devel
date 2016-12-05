/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2011, University of Amsterdam
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

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __MINGW32__
#include <windows.h>
#define sleep(x) Sleep(x*1000)
#endif

pthread_mutex_t *my_mutex;

int locked = 1;

void *
test_recursive(void *arg)
{ int status = pthread_mutex_lock(my_mutex);

  if ( status != 0 )
  { fprintf(stderr,"error while locking mutex (1):%s\n",
	    strerror(status));
    exit(1);
  }

  status = pthread_mutex_lock(my_mutex);
  if ( status != 0 )
  { fprintf(stderr,"error while locking mutex (2) \n");
    exit(1);
  }

  locked = 0;
  status = pthread_mutex_unlock(my_mutex);
  if ( status != 0 )
  { fprintf(stderr,"error while unlocking mutex (2) \n");
    exit(1);
  }

  status = pthread_mutex_unlock(my_mutex);
  if ( status != 0 )
  { fprintf(stderr,"error while unlocking mutex (1) \n");
    exit(1);
  }

  pthread_exit(NULL);
  return NULL;
}


int
main(int argc, char **argv)
{ int status;
  pthread_t thread_id;
  pthread_mutexattr_t attr;

  my_mutex = malloc(sizeof(pthread_mutex_t));
  pthread_mutexattr_init(&attr);
#ifdef HAVE_PTHREAD_MUTEXATTR_SETKIND_NP
  pthread_mutexattr_setkind_np(&attr, PTHREAD_MUTEX_RECURSIVE_NP);
#else
#ifdef HAVE_PTHREAD_MUTEXATTR_SETTYPE
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
#endif
#endif
  pthread_mutex_init(my_mutex,&attr);

  status = pthread_create(&thread_id,NULL,test_recursive,NULL);
  sleep(1);
  exit(locked);
}
