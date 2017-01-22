/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2009, University of Amsterdam
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


word
pl_getenv(term_t var, term_t value)
{ char *n;

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
          rc = FALSE;

        PL_free(buf);
        return rc;
      }
    }

    fail;
  }

  fail;
}


word
pl_setenv(term_t var, term_t value)
{ char *n, *v;

  if ( PL_get_chars(var, &n, CVT_ALL|REP_FN|BUF_RING|CVT_EXCEPTION) &&
       PL_get_chars(value, &v, CVT_ALL|REP_FN|CVT_EXCEPTION) )
    return Setenv(n, v);

  fail;
}


word
pl_unsetenv(term_t var)
{ char *n;

  if ( PL_get_chars(var, &n, CVT_ALL|REP_FN|CVT_EXCEPTION) )
    return Unsetenv(n);

  fail;
}


word
pl_get_time(term_t t)
{ return PL_unify_float(t, WallTime());
}


word
pl_sleep(term_t time)
{ double t;

  if ( PL_get_float_ex(time, &t) )
    return Pause(t);

  fail;
}

#ifdef __WINDOWS__
#include <process.h>
#endif

word
pl_get_pid(term_t pid)
{ GET_LD
  return PL_unify_integer(pid, getpid());
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(system)
  PRED_DEF("shell", 2, shell, 0)
EndPredDefs
