/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl
*/

#include "pl-incl.h"

#ifndef MAXVARNAME
#define MAXVARNAME 1024
#endif

word
pl_shell(term_t command, term_t status)
{ char *cmd;

  if ( PL_get_chars(command, &cmd, CVT_ALL) )
  { int rval = System(cmd);

    return PL_unify_integer(status, rval);
  }
  
  return warning("shell/1: instantiation fault");
}


word
pl_getenv(term_t var, term_t value)
{ char *n;

  if ( PL_get_chars(var, &n, CVT_ALL) )
  { char *v;

    if ( (v = getenv(n)) )
      return PL_unify_atom_chars(value, v);
    fail;
  }

  return warning("getenv/2: instantiation fault");
}  


word
pl_setenv(term_t var, term_t value)
{ char *n, *v;

  if ( PL_get_chars(var, &n, CVT_ALL|BUF_RING) &&
       PL_get_chars(value, &v, CVT_ALL) )
  { Setenv(n, v);
    succeed;
  }

  return warning("setenv/2: instantiation fault");
}


word
pl_unsetenv(term_t var)
{ char *n;

  if ( PL_get_chars(var, &n, CVT_ALL) )
  { Unsetenv(n);

    succeed;
  }

  return warning("unsetenv/1: instantiation fault");
}


word
pl_argv(term_t argv)
{ int n;
  term_t head = PL_new_term_ref();
  term_t list = PL_copy_term_ref(argv);

  for(n=0; n<mainArgc; n++)
  { if ( !PL_unify_list(list, head, list) ||
	 !PL_unify_atom_chars(head, mainArgv[n]) )
      fail;
  }

  return PL_unify_nil(list);
}


word
pl_convert_time(term_t time, term_t year, term_t month,
		term_t day, term_t hour, term_t minute,
		term_t second, term_t usec)
{ double tf;

  if ( PL_get_float(time, &tf) )
  { long t    = (long) tf;
    long us   = (long)((tf - (double) t) * 1000.0);
    struct tm *tm = LocalTime(&t);

    if ( PL_unify_integer(year,   tm->tm_year + 1900) &&
	 PL_unify_integer(month,  tm->tm_mon + 1) &&
	 PL_unify_integer(day,    tm->tm_mday) &&
	 PL_unify_integer(hour,   tm->tm_hour) &&
	 PL_unify_integer(minute, tm->tm_min) &&
	 PL_unify_integer(second, tm->tm_sec) &&
	 PL_unify_integer(usec,   us) )
      succeed;
  } else
    warning("convert_time/8: instantiation fault");

  fail;
}


word
pl_get_time(term_t t)
{ double stime;
#ifndef HAVE_GETTIMEOFDAY
  stime = (double)time((time_t *)NULL);
#else
  struct timeval tp;

  gettimeofday(&tp, NULL);
  stime = (double)tp.tv_sec + (double)tp.tv_usec/1000000.0;
#endif

  return PL_unify_float(t, stime);
}


word
pl_sleep(term_t time)
{ double t;

  if ( PL_get_float(time, &t) )
    Pause(t);
  
  succeed;
}
