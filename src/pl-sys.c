/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl
*/

#include "pl-incl.h"

word
pl_shell(Word command, Word status)
{ char *cmd = primitiveToString(*command, FALSE);

  if ( cmd == (char *) NULL )
    return warning("shell/1: instantiation fault");

  return unifyAtomic(status, consNum(System(cmd)) );
}

word
pl_getenv(Word var, Word value)
{ char *n, *v;

  if ( (n = primitiveToString(*var, FALSE)) == (char *) NULL )
    return warning("getenv/2: instantiation fault");

  if ((v = getenv(n)) == (char *) NULL)
    fail;

  return unifyAtomic(value, lookupAtom(v));
}  

word
pl_setenv(Word var, Word value)
{ char *n, *v;

  initAllocLocal();
  n = primitiveToString(*var, TRUE);
  v = primitiveToString(*value, TRUE);
  stopAllocLocal();

  if ( n == (char *)NULL || v == (char *) NULL )
    return warning("setenv/2: instantiation fault");

  Setenv(n, v);

  succeed;
}

word
pl_unsetenv(Word var)
{ char *n;

  if ( (n = primitiveToString(*var, FALSE)) == (char *) NULL )
    return warning("unsetenv/1: instantiation fault");

  Unsetenv(n);

  succeed;
}

word
pl_argv(Word list)
{ int n;
  word w;

  for(n=0; n<mainArgc; n++)
  { w = (word) lookupAtom(mainArgv[n]);
    APPENDLIST(list, &w);
  }
  CLOSELIST(list);

  succeed;
}


word
pl_grep(Word file, Word search, Word line, word h)
{ char *fn;
  IOSTREAM *fd;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      { if ( (fn = primitiveToString(*file, FALSE)) == (char *) NULL )
	  return warning("$grep/3: instantiation fault");
	if ( (fn = ExpandOneFile(fn)) == (char *)NULL )
	  fail;
	if ( (fd = Sopen_file(fn, "r")) == (IOSTREAM *) NULL )
	  return warning("$grep/3: cannot open %s: %s", fn, OsError());
      }
      goto redo;
    case FRG_REDO:
      { char buf[1024];
	char *s;

	fd = (IOSTREAM *) ForeignContextAddress(h);
      redo:
	if ( (s = primitiveToString(*search, FALSE)) == (char *) NULL )
	  return warning("$grep/3: instantiation fault");
	while( Sfgets(buf, 1023, fd) != (char *) NULL )
	{ if ( (*s == '^' && strprefix(buf, &s[1])) ||
	       strsub(buf, s) )
	  { for( s = buf; *s; s++ )	/* get rid of final newline */
	    { if ( *s == '\n' )
	      { *s = EOS;
	        break;
	      }
	    }	      

	    if ( unifyAtomic(line, globalString(buf)) == FALSE )
	      continue;

	    ForeignRedo(fd);
	  }
	}         
	Sclose(fd);

	fail;
      }
    case FRG_CUTTED:
    default:;
	Sclose((IOSTREAM *)ForeignContextAddress(h));
	succeed;
  }
}

word
pl_convert_time(Word time, Word year, Word month, Word day, Word hour, Word minute, Word second, Word usec)
{ if ( isReal(*time) )
  { double tf = valReal(*time);
    long t    = (long) tf;
    long us   = (long)((tf - (double) t) * 1000.0);
    struct tm *tm = LocalTime(&t);

    TRY(unifyAtomic(year, 	consNum(tm->tm_year + 1900) ));
    TRY(unifyAtomic(month, 	consNum(tm->tm_mon + 1) ));
    TRY(unifyAtomic(day, 	consNum(tm->tm_mday) ));
    TRY(unifyAtomic(hour, 	consNum(tm->tm_hour) ));
    TRY(unifyAtomic(minute, 	consNum(tm->tm_min) ));
    TRY(unifyAtomic(second, 	consNum(tm->tm_sec) ));
    TRY(unifyAtomic(usec, 	consNum(us) ));
    succeed;
  } else
    return warning("convert_time/8: instantiation fault");
}

word
pl_get_time(Word t)
{ real stime;
#ifndef HAVE_GETTIMEOFDAY
  stime = (real)time((time_t *)NULL);
#else
  struct timeval tp;

  gettimeofday(&tp, NULL);
  stime = (real)tp.tv_sec + (real)tp.tv_usec/1000000.0;
#endif

  return unifyAtomic(t, globalReal(stime));
}

word
pl_sleep(Word time)
{ real t;

  TRY( wordToReal(*time, &t) );
  Pause(t);
  
  succeed;
}
