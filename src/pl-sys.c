/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2023, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include "pl-fli.h"
#ifdef HAVE_SCHED_YIELD
#include <sched.h>
#endif

#ifndef MAXVARNAME
#define MAXVARNAME 1024
#endif

/** shell(+Command:text, -Status:integer) is det.

Run an external command and wait for its completion.
*/

static
PRED_IMPL("shell", 2, shell, 0)
{ GET_LD
  char *cmd;

  if ( PL_get_chars(A1, &cmd, CVT_ALL|REP_FN|CVT_EXCEPTION) )
  { int rval = System(cmd);

    return PL_unify_integer(A2, rval);
  }

  fail;
}


static
PRED_IMPL("getenv", 2, getenv, 0)
{ char *n;
  term_t var = A1;
  term_t value = A2;

  if ( PL_get_chars(var, &n, CVT_ALL|REP_FN|CVT_EXCEPTION) )
  { char buf[1024];
    size_t size;

    if ( (size=getenv3(n, buf, sizeof(buf))) != (size_t)-1 )
    { if ( size < sizeof(buf) )
      { return PL_unify_chars(value, PL_ATOM|REP_FN, size, buf);
      } else
      { char *buf = PL_malloc(size+1);
        int rc;

        size = getenv3(n, buf, size+1);
        if ( size > 0 )
          rc = PL_unify_chars(value, PL_ATOM|REP_FN, size, buf);
        else
          rc = false;

        PL_free(buf);
        return rc;
      }
    }

    fail;
  }

  fail;
}


static
PRED_IMPL("setenv", 2, setenv, 0)
{ char *n, *v;

  if ( PL_get_chars(A1, &n, CVT_ALL|REP_FN|BUF_STACK|CVT_EXCEPTION) &&
       PL_get_chars(A2, &v, CVT_ALL|REP_FN|CVT_EXCEPTION) )
    return Setenv(n, v);

  fail;
}


static
PRED_IMPL("unsetenv", 1, unsetenv, 0)
{ char *n;

  if ( PL_get_chars(A1, &n, CVT_ALL|REP_FN|CVT_EXCEPTION) )
    return Unsetenv(n);

  fail;
}


static
PRED_IMPL("get_time", 1, get_time, 0)
{ return PL_unify_float(A1, WallTime());
}


static
PRED_IMPL("sleep", 1, sleep, 0)
{ double t;

  if ( PL_get_float_ex(A1, &t) )
  { if ( t < 0.0 )
      return true;
#ifdef HAVE_SCHED_YIELD
    if ( t == 0.0 )
    { sched_yield();
      return true;
    }
#endif
    return Pause(t);
  }

  return false;
}

#ifdef __WINDOWS__
#include <process.h>
#endif

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(system)
  PRED_DEF("shell",    2, shell,    0)
  PRED_DEF("setenv",   2, setenv,   0)
  PRED_DEF("unsetenv", 1, unsetenv, 0)
  PRED_DEF("getenv",   2, getenv,   0)
  PRED_DEF("get_time", 1, get_time, 0)
  PRED_DEF("sleep",    1, sleep,    0)
EndPredDefs
