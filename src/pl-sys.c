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

#include "pl-incl.h"

#ifndef MAXVARNAME
#define MAXVARNAME 1024
#endif

word
pl_shell(term_t command, term_t status)
{ char *cmd;

  if ( PL_get_chars_ex(command, &cmd, CVT_ALL|REP_FN) )
  { int rval = System(cmd);

    return PL_unify_integer(status, rval);
  }
    
  fail;
}


#define MAXVARLEN 2048

word
pl_getenv(term_t var, term_t value)
{ char *n;

  if ( PL_get_chars_ex(var, &n, CVT_ALL|REP_FN) )
  { char buf[MAXVARLEN];
    char *s;

    if ( (s=getenv3(n, buf, sizeof(buf))) )
    { Sdprintf("Got %d chars\n", strlen(buf));
      return PL_unify_chars(value, PL_ATOM|REP_FN, -1, s);
    }

    fail;
  }

  fail;
}  


word
pl_setenv(term_t var, term_t value)
{ char *n, *v;

  if ( PL_get_chars_ex(var, &n, CVT_ALL|REP_FN|BUF_RING) &&
       PL_get_chars_ex(value, &v, CVT_ALL|REP_FN) )
  { Setenv(n, v);
    succeed;
  }

  fail;
}


word
pl_unsetenv(term_t var)
{ char *n;

  if ( PL_get_chars_ex(var, &n, CVT_ALL|REP_FN) )
  { Unsetenv(n);

    succeed;
  }

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

#ifdef __WIN32__
#include <process.h>
#endif

word
pl_get_pid(term_t pid)
{ return PL_unify_integer(pid, getpid());
}
