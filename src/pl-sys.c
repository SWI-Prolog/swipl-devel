/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl
*/

#include "pl-incl.h"

word
pl_shell(command, status)
Word command, status;
{ char *cmd = primitiveToString(*command, FALSE);

  if ( cmd == (char *) NULL )
    return warning("shell/1: instantiation fault");

  return unifyAtomic(status, consNum(System(cmd)) );
}

word
pl_getenv(var, value)
Word var, value;
{ char *n, *v;

  if ( (n = primitiveToString(*var, FALSE)) == (char *) NULL )
    return warning("getenv/2: instantiation fault");

  if ((v = getenv(n)) == (char *) NULL)
    fail;

  return unifyAtomic(value, lookupAtom(v));
}  

word
pl_setenv(var, value)
Word var, value;
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
pl_unsetenv(var)
Word var;
{ char *n;

  if ( (n = primitiveToString(*var, FALSE)) == (char *) NULL )
    return warning("unsetenv/1: instantiation fault");

  Unsetenv(n);

  succeed;
}

word
pl_argv(list)
Word list;
{ int n;
  word w;

  for(n=0; n<mainArgc; n++)
  { w = (word) lookupAtom(mainArgv[n]);
    APPENDLIST(list, &w);
  }
  CLOSELIST(list);

  succeed;
}

#if LINK_THIEF
#define	POSTFIX	0
#define	PREFIX	1
#define	INFIX	2

int
GetOp(token, type, lhs, op, rhs)
char *token;
int type, *lhs, *op, *rhs;
{ Atom name = lookupAtom(token);
  int subtype;

  switch(type)
  { case PREFIX:
	if ( isPrefixOperator(name, &subtype, op) )
	{ *lhs = *rhs = (subtype == OP_FX ? *op - 1 : *op);
	  succeed;
	}
	fail;
    case POSTFIX:
	if ( isPostfixOperator(name, &subtype, op) )
	{ *lhs = *rhs = (subtype == OP_XF ? *op - 1 : *op);
	  succeed;
	}
	fail;
    case INFIX:
	if ( isInfixOperator(name, &subtype, op) )
	{ *lhs = (subtype == OP_XFY || subtype == OP_XFX ? *op - 1 : *op);
	  *rhs = (subtype == OP_XFX || subtype == OP_YFX ? *op - 1 : *op);
	  succeed;
	}
	fail;
  }
  return fatalError("Unknown operator type request from thief: %d", type);
}

word
pl_thief(args)
Word args;
{ int argc = 0;
  char *argv[50];
  extern int thief();

  argv[argc++] = "top";

  while( isList(*args) )
  { Word a = argTermP(*args, 0);
    deRef(a);
    if ( !isAtom(*a) )
      return warning("thief/1: illegal argument list");
    argv[argc++] = stringAtom(*a);
    args = argTermP(*args, 1);
    deRef(args);
  }
  if ( !isNil(*args) )
    return warning("thief/1: illegal argument list");

  if ( thief(argc, argv) == 0 )
    succeed;
  fail;
}
#endif /* LINK_THIEF */

word
pl_grep(file, search, line, h)
Word file, search, line;
word h;
{ char *fn;
  FILE *fd;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      { if ( (fn = primitiveToString(*file, FALSE)) == (char *) NULL )
	  return warning("$grep/3: instantiation fault");
	if ( (fn = ExpandOneFile(fn)) == (char *)NULL )
	  fail;
	if ( (fd = Fopen(fn, "r")) == (FILE *) NULL )
	  return warning("$grep/3: cannot open %s: %s", fn, OsError());
      }
      goto redo;
    case FRG_REDO:
      { char buf[1024];
	char *s;

	fd = (FILE *) ForeignContextAddress(h);
      redo:
	if ( (s = primitiveToString(*search, FALSE)) == (char *) NULL )
	  return warning("$grep/3: instantiation fault");
	while( fgets(buf, 1023, fd) != (char *) NULL )
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
	fclose(fd);

	fail;
      }
    case FRG_CUTTED:
    default:;
	fclose((FILE *)ForeignContextAddress(h));
	succeed;
  }
}

word
pl_convert_time(time, year, month, day, hour, minute, second, usec)
Word time, year, month, day, hour, minute, second, usec;
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
pl_get_time(t)
Word t;
{ struct timeval tp;
  real time;

  gettimeofday(&tp, NULL);
  time = (real)tp.tv_sec + (real)tp.tv_usec/1000000.0;
  
  return unifyAtomic(t, globalReal(time));
}

word
pl_sleep(time)
Word time;
{ real t;

  TRY( wordToReal(*time, &t) );
  Sleep(t);
  
  succeed;
}
