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

#define __assert_fail __sys_assert_fail

#include <assert.h>
#include "pl-incl.h"
#include "os/pl-cstack.h"
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#undef __assert_fail
void __assert_fail(const char *assertion,
		   const char *file,
		   unsigned int line,
		   const char *function);


		 /*******************************
		 *	DEBUGGING SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is called from  GNU  <assert.h>,   so  we  can print which
thread  caused  the  problem.  We  redefine    this  to  get  additional
information. This is done  in  a  separate   file,  so  we  can redefine
__assert_fail to __sys_assert_fail and  avoid  a   type  error  on small
variations the type of __assert_fail, such as `_Noreturn` as it is found
in musl libc (https://www.musl-libc.org/) used by Alpine Linux.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(HAVE_CTIME_R) && !defined(ctime_r)
#define ctime_r(timep, buf) strcpy(buf, ctime(timep))
#endif

void
__assert_fail(const char *assertion,
	      const char *file,
	      unsigned int line,
	      const char *function)
{ time_t now = time(NULL);
  char tbuf[48];

  ctime_r(&now, tbuf);
  tbuf[24] = '\0';

#ifdef O_PLMT
  { const pl_wchar_t *name = L"";
    int tid = PL_thread_self();
    atom_t alias;

    if ( PL_get_thread_alias(tid, &alias) )
      name = PL_atom_wchars(alias, NULL);

    Sdprintf("[Thread %d (%Ws) at %s] %s:%d: %s: Assertion failed: %s\n",
	     PL_thread_self(), name, tbuf,
	     file, line, function, assertion);
  }
#else
  Sdprintf("[At %s] %s:%d: %s: Assertion failed: %s\n",
	   tbuf, file, line, function, assertion);
#endif

  save_backtrace("assert_fail");
  print_backtrace_named("assert_fail");
  abort();
}


