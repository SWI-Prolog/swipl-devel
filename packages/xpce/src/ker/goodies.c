/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>
#include "alloc.h"
#include <h/graphics.h>
#include <h/unix.h>
#include <unistd.h>

#ifdef SOME_MISSING_LIB_PROTOTYPES
extern int select (int width,
		   fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
		   struct timeval *timeout);
#endif

/* (AA)	isqrt(a).  Returns the square root of a as an integer.  The 
	algorithm only uses bit-shifts (multiplication by a power of 2) and
	3.5 integer multiplications on average.
 */

int
isqrt(long a)
{ register long n, d, m1, m2;

  if ( a < 0 )
    return errorPce(NAME_sqrt, NAME_domainError, toInt(a));

  for (m2=5; ((a<<1) & (0xffff << (m2<<1))); m2++);
  m1 = m2 - 1;
  n = a >> m1;
  for (;;)
  { d = a - n*n;
    if (d >= 0)
    { if (d <= (n<<1))
	return (d <= n? n : n+1);
    } else
    { if (-d <= (n<<1))
	return (-d<=n ? n : n-1);
    }
    n += (1 + (d>>m2));
  }
}


int
distance(int x1, int y1, int x2, int y2)
{ int dx = x1-x2, dy = y1-y2;

  return isqrt(dx*dx + dy*dy);
}


int
rfloat(float f)
{ if (f > 0.0)
    return (int) (f+0.4999999);

  return (int) (f-0.4999999);
}

		/********************************
		*           STRINGS             *
		*********************************/


char *
strcpyskip(char *t, char *f)
{ while( (*t = *f) )
    t++, f++;

  return t;
}


status
substr(register char *str, register char *sb)
{ register char *r, *q;

  for (; *str; str++)
  { for (r=str, q=sb; *r == *q && *r != '\0'; r++, q++) ;

    if (*q == '\0')
      succeed;
  }

  fail;
}


status
prefixstr(char *s1, char *s2)			/* S2 is a prefix of s1 */
{ while( *s1 == *s2 && *s2 )
   s1++, s2++;

  return *s2 == EOS;
}


status
substr_ignore_case(register char *str, register char *sb)
{ register char *r, *q;

  for (; *str; str++)
  { for (r=str, q=sb; tolower(*r) == tolower(*q) && *r != '\0'; r++, q++) ;

    if (*q == EOS)
      succeed;
  }

  fail;
}


status
prefixstr_ignore_case(char *s1, char *s2) /* S2 is a prefix of s1 */
{ while( tolower(*s1) == tolower(*s2) && *s2 )
   s1++, s2++;

  return *s2 == EOS;
}


status
streq_ignore_case(char *s1, char *s2)
{ while( tolower(*s1) == tolower(*s2) && *s2 )
   s1++, s2++;
  
  return *s1 == EOS && *s2 == EOS;
}


char *
upcasestr(char *s)
{ register char *q = s;

  for( ; *q; q++ )
    *q = toupper(*q);

  return s;
}


char *
downcasestr(char *s)
{ register char *q = s;

  for( ; *q; q++ )
    *q = tolower(*q);

  return s;
}


void
checkSummaryCharp(Name classname, Name name, char *s)
{ int l;

  for(l=0; l<70 && *s; l++, s++)
    if ( !((*s >= ' ' && *s < 127) || *s == '\t') )
      sysPce("%s (%s): Bad summary string", pp(classname), pp(name));

  if ( *s != EOS || (l != 0 && l < 5) )
    sysPce("%s (%s): Bad summary string: %s", pp(classname), pp(name), s);
}



Name
characterName(Any chr)
{ char buf[10];
  int c;

  if ( !isInteger(chr) )
    return chr;

  c = valInt(chr);

  if ( c >= META_OFFSET )
  { strcpy(buf, "\\e");
    c -= META_OFFSET;
  } else
    buf[0] = EOS;

  switch(c)
  { case ESC:
      strcat(buf, "\\e");
      break;
    case ' ':
      strcat(buf, "SPC");
      break;
    case '\t':
      strcat(buf, "TAB");
      break;
    case '\r':
      strcat(buf, "RET");
      break;
    case '\n':
      strcat(buf, "LFD");
      break;
    case DEL:
      strcat(buf, "DEL");
      break;
    default:
      if ( c < ' ' )
      {	int l;

	strcat(buf, "\\C-");
	buf[l=strlen(buf)] = tolower(c + '@');
	buf[l+1] = EOS;
      } else
      { int l;

        buf[l=strlen(buf)] = c;
	buf[l+1] = EOS;
      }
  }
  
  return CtoName(buf);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
				WRITEF
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
writef_arguments(char *fm, va_list args, int *argc, Any *argv)
{ int ac = 0;

  while(*fm)
  { switch(*fm)
    { case '\\':
	if ( *++fm )
	  fm++;
	continue;
      case '%':
	if ( *++fm == '%' )
	{ fm++;
	  continue;
	}
	if ( *fm == '+' || *fm == '-' || *fm == ' ' || *fm == '#' )
	  fm++;
	if ( *fm == '*' )
	  argv[ac++] = va_arg(args, Any);
	else
	{ while( (*fm >= '0' && *fm <= '9') || *fm == '.' )
	    fm++;
	}
	if ( *fm )
	{ fm++;
	  argv[ac++] = va_arg(args, Any);
	}
	continue;
      default:
      	fm++;
	continue;
    }
  }

  *argc = ac;
  succeed;
}


static void
vwritef(char *fm, va_list args)
{ Any argv[VA_PCE_MAX_ARGS];
  int argc;
  CharArray ca;

  writef_arguments(fm, args, &argc, argv);
  ca = CtoScratchCharArray(fm);
  Trace(TRACE_NEVER, formatPcev(PCE, ca, argc, argv));
  doneScratchCharArray(ca);
}


void
writef(char *fm, ...)
{ va_list args;
  
  va_start(args, fm);
  vwritef(fm, args);
  va_end(args);
}

  
#define NOT_SET PCE_MAX_INT
#define Skip(q)	while(*(q)) (q)++
#define DoPP() \
	{ char *_s = pp(argv[0]); \
	  argc--, argv++; \
	  *r++ = 's'; \
	  *r = EOS; \
	  if ( arg == NOT_SET ) \
	    sprintf(q, fmtbuf, _s); \
	  else \
	    sprintf(q, fmtbuf, arg, _s); \
	}

status
swritefv(char *buf, CharArray format, int argc, Any *argv)
{ char *fmt, *s, *q;

  fmt = strName(format);

  for( s=fmt, q=buf; *s; )
  { switch(*s)
    { case '\\':	
	switch(*++s)
	{ case 'r':	*q++ = '\r';	break;
	  case 'n':	*q++ = '\n';	break;
	  case 'b':	*q++ = '\b';	break;
	  case 'f':	*q++ = '\f';	break;
	  case 't':	*q++ = '\t';	break;
	  case EOS:	*q++ = '\\';	continue;
	  default:	*q++ = *s;	break;
	}
	s++;
	continue;
      case '%':
	s++;
	if ( *s == '%' )		/* `%%' */
	{ *q++ = '%';
	  s++;
	  continue;
	} else
	{ char fmtbuf[100];
	  char *r = fmtbuf;
	  int arg;

	  *r++ = '%';

	  if ( *s == '+' || *s == '-' || *s == ' ' || *s == '#' )
	    *r++ = *s++;

	  if ( *s == '*' )
	  { Int i;

	    if ( argc <= 0 )
	      arg = 0;
	    else if ( (i = toInteger(argv[0])) != FAIL )
	      arg = valInt(i);
	    else
	      arg = 0;

	    argc--, argv++;
	  } else
	  { arg = NOT_SET;
	    while( (*s >= '0' && *s <= '9') || *s == '.' )
	      *r++ = *s++;
	  }

	  switch(*s)
	  { case 'c':
	    case 'd':
	    case 'i':
	    case 'o':
	    case 'u':
	    case 'x':
	    case 'X':
	    { int a;
	      Int i;

	      if ( argc <= 0 )
	      	a = 0;
	      else
	      { if ( (i = toInteger(argv[0])) != FAIL )
		  a = valInt(i);
	        else
		{ DoPP();
		  continue;
		}
		argc--, argv++;
	      }

	      *r++ = *s++;
	      *r = EOS;

	      if ( arg == NOT_SET )
		sprintf(q, fmtbuf, a);
	      else
	      	sprintf(q, fmtbuf, arg, a);
	      Skip(q);
	      continue;
	    }
	    case 'f':
	    case 'e':
	    case 'E':
	    case 'g':
	    case 'G':
	    { float a;
	      Real f;

	      if ( argc <= 0 )
	      	a = 0.0;
	      else
	      { if ( (f = toReal(argv[0])) != FAIL )
		  a = f->value;
	        else
		{ DoPP();
		  continue;
		}
		argc--, argv++;
	      }

	      *r++ = *s++;
	      *r = EOS;
	      if ( arg == NOT_SET )
		sprintf(q, fmtbuf, a);
	      else
	      	sprintf(q, fmtbuf, arg, a);
	      Skip(q);
	      continue;
	    }
	    case 's':
	    { char *a;
	      int free = 0;

	      if ( argc <= 0 )
	      	a = "(nil)";
	      else if ( !(a = toCharp(argv[0])) )
	      { a = pp(argv[0]);
		free++;
	      }

	      *r++ = *s++;
	      *r = EOS;
	      if ( arg == NOT_SET )
		sprintf(q, fmtbuf, a);
	      else
	      	sprintf(q, fmtbuf, arg, a);
	      Skip(q);
	      if ( free ) 
		free_string(a);
	      argc--, argv++;
	      continue;
	    }
	    case 'O':			/* object via pp() */
	    { char *a;
	      int free = 0;

	      if ( argc <= 0 )
	      	a = save_string("(nil)");
	      else
	      { free++;
	      	a = pp(argv[0]);
	      }

	      *r++ = 's';
	      *r = EOS;
	      s++;
	      if ( arg == NOT_SET )
		sprintf(q, fmtbuf, a);
	      else
	      	sprintf(q, fmtbuf, arg, a);
	      Skip(q);
	      if ( free ) 
		free_string(a);
	      argc--, argv++;
	      continue;
	    }
	    case 'N':			/* object via <-print_name */
	    { char *a;
	      int free = 0;

	      if ( argc <= 0 )
	      	a = "(nil)";
	      else
	      { Any pn;

		Trace(TRACE_NEVER, pn = get(argv[0], NAME_printName, 0));
		if ( !(a = toCharp(pn)) )
		{ free++;
		  a = pp(argv[0]);
		}
	      }

	      *r++ = 's';
	      *r = EOS;
	      s++;
	      if ( arg == NOT_SET )
		sprintf(q, fmtbuf, a);
	      else
	      	sprintf(q, fmtbuf, arg, a);
	      Skip(q);
	      if ( free )
		free_string(a);
	      argc--, argv++;
	      continue;
	    }
	    case 'I':			/* ignore */
	    { s++;
	      argc--, argv++;
	      continue;
	    }
	    default:
	      *q++ = '%';
	      *q++ = *s++;
	      continue;
	  }
	}
      default:
	*q++ = *s++;
	continue;
      }
  }
  *q++ = EOS;

  succeed;
}


status
str_writefv(String s, CharArray format, int argc, Any *argv)
{ char buf[FORMATSIZE];

  TRY(swritefv(buf, format, argc, argv));
  s->size = strlen(buf);
  if ( s->size >= FORMATSIZE )
    return errorPce(format, NAME_formatBufferOverFlow, toInt(FORMATSIZE));
  s->b16 = FALSE;
  s->encoding = ENC_ASCII;
  str_alloc(s);
  memcpy(s->s_text, buf, s->size);

  succeed;
}


		/********************************
		*              SCANF		*
		********************************/

#define T_CHAR	    0			/* C-types returned */
#define T_INT       1
#define T_FLOAT     2
#define T_CHARP     3

#define S_UNSIGNED  8

#define L_SHORT    16
#define L_LONG     32

#ifdef __linux__			/* delete if conflict arrises */
extern int vsscanf(const char *, const char *, va_list);
#endif

Int scanstr(char *str, char *fmt, Any *r)
{ int types[SCAN_MAX_ARGS];
  void *ptrs [SCAN_MAX_ARGS];
  int argn = 0;
  int ar, n;
  char *s = fmt;

  for( ; *s; )
  { int supress = FALSE;
    int length;

    switch(*s)
    { case '\\':
	s += (s[1] == EOS ? 1 : 2);
	continue;
      case '%':
	s++;
	if ( *s == '%' )		/* %% */
	{ s++;
	  continue;
	}
	if ( isdigit(*s) && s[1] == '$' ) /* %digit$ */
	  s += 2;
	if ( *s == '*' )		/* %* */
	{ supress = TRUE;
	  s++;
	}
	while( *s && isdigit(*s) )	/* field width */
	  s++;
	if ( *s == EOS )		/* check for end */
	  continue;

	if ( *s == 'l' )		/* Integer size */
	{ length = L_LONG;
	  s++;
	} else if ( *s == 'h' )
	{ length = L_SHORT;
	  s++;
	} else
	  length = 0;
	
	switch(*s)			/* conversion char */
    	{ case 'u':
	    if ( !supress )
	    { types[argn] = length|T_INT|S_UNSIGNED;
	      ptrs[argn]  = alloca(sizeof(long));
	      argn++;
	    }
	    s++;
	    continue;
	  case 'd':
	  case 'o':
	  case 'x':
	  case 'i':
	  case 'n':
	    if ( !supress )
	    { types[argn] = length|T_INT;
	      ptrs[argn]  = alloca(sizeof(long));
	      argn++;
	    }
	    s++;
	    continue;
	  case 'e':
	  case 'f':
	  case 'g':
	    if ( !supress )
	    { types[argn] = length|T_FLOAT;
	      ptrs[argn]  = alloca(length == L_LONG ? sizeof(double)
				                    : sizeof(float));
	      argn++;
	    }
	    s++;
	    continue;
	  case '[':
	    s++;
	    if ( *s == '^' ) s++;
	    if ( *s == ']' ) s++;
	    while( *s && *s != ']' )
	      s++;
	  case 's':
	    if ( !supress )
	    { types[argn] = T_CHARP;
	      ptrs[argn]  = alloca(LINESIZE);
	      argn++;
	    }
	    s++;
	    continue;
	  case 'c':
	    if ( !supress )
	    { types[argn] = T_CHAR;
	      ptrs[argn]  = alloca(sizeof(char));
	      argn++; 
	    }
	    s++;
	    continue;
	  default:
	    s++;
	    continue;
	}
      default:
      	s++;
	continue;
    }
  }

  DEBUG(NAME_scan, printf("argn = %d\n", argn));
#ifndef HAVE_VSSCANF
  switch(argn)
  { case 0:	ar = sscanf(str, fmt); break;
    case 1:	ar = sscanf(str, fmt, ptrs[0]); break;
    case 2:	ar = sscanf(str, fmt, ptrs[0], ptrs[1]); break;
    case 3:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2]);
				      break;
    case 4:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3]); break;
    case 5:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4]); break;
    case 6:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5]);
    				      break;
    case 7:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6]); break;
    case 8:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7]); break;
    case 9:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8]);
				      break;
    case 10:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9]);
				      break;
    case 11:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10]);
				      break;
    case 12:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11]);
				      break;
    case 13:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11],
			    	      ptrs[12]);
				      break;
    case 14:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11],
			    	      ptrs[12], ptrs[13]);
				      break;
    case 15:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11],
			    	      ptrs[12], ptrs[13], ptrs[14]);
				      break;
    case 16:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11],
			    	      ptrs[12], ptrs[13], ptrs[14],
			    	      ptrs[15]);
				      break;
    case 17:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11],
			    	      ptrs[12], ptrs[13], ptrs[14],
			    	      ptrs[15], ptrs[16]);
				      break;
    case 18:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11],
			    	      ptrs[12], ptrs[13], ptrs[14],
			    	      ptrs[15], ptrs[16], ptrs[17]);
				      break;
    case 19:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11],
			    	      ptrs[12], ptrs[13], ptrs[14],
			    	      ptrs[15], ptrs[16], ptrs[17],
			    	      ptrs[18]);
				      break;
    case 20:	ar = sscanf(str, fmt, ptrs[0], ptrs[1], ptrs[2],
			              ptrs[3], ptrs[4], ptrs[5],
			              ptrs[6], ptrs[7], ptrs[8],
			    	      ptrs[9], ptrs[10], ptrs[11],
			    	      ptrs[12], ptrs[13], ptrs[14],
			    	      ptrs[15], ptrs[16], ptrs[17],
			              ptrs[18], ptrs[19]);
				      break;
    default:	errorPce(NIL, NAME_tooManyArguments);
    		fail;
  }
#else
  ar = vsscanf(str, fmt, (va_list) ptrs);
#endif  

  DEBUG(NAME_scan, printf("ar = %d\n", argn));

#define arg(n, tp) (*((tp *)(ptrs[n])))

  if ( ar < 0 )
    fail;

  for(n = 0; n<ar; n++)
  { Any val = NIL;

    switch(types[n])
    { case T_CHAR:
	val = toInt(arg(n, char));
	break;
      case T_INT|L_SHORT:
	val = toInt(arg(n, short));
	break;
      case T_INT:
	val = toInt(arg(n, int));
	break;
      case T_INT|L_LONG:
	val = toInt(arg(n, long));
	break;
      case T_INT|L_SHORT|S_UNSIGNED:
	val = toInt(arg(n, unsigned short));
	break;
      case T_INT|S_UNSIGNED:
	val = toInt(arg(n, unsigned int));
	break;
      case T_INT|L_LONG|S_UNSIGNED:
	val = toInt(arg(n, unsigned long));
	break;
      case T_FLOAT:
	val = CtoReal(arg(n, float));
	break;
      case T_FLOAT|L_LONG:
	val = CtoReal(arg(n, double));
	break;
      case T_CHARP:
	val = CtoString(ptrs[n]);
	break;
    }

    r[n] = val;
  }

#undef arg

  return toInt(ar);
}

		/********************************
		*         FATAL ERRORS		*
		********************************/

#include <h/interface.h>

status
sysPce(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  catchErrorSignalsPce(PCE, OFF);

  printf("[PCE system error: ");
  vprintf(fm, args);

#ifndef O_RUNTIME
  printf("\n\tStack:\n");
  pceTraceBack(20);
  printf("]\n");

  if ( PCE->print_c_stack == ON )
  { printf("Dumping C stack ...\n");
    pcePrintStack(50);
  }

  catchErrorSignalsPce(PCE, ON);
  printf("Requesting host to dump stack ...\n");
  hostAction(HOST_BACKTRACE, 10);
  hostAction(HOST_RECOVER_FROM_FATAL_ERROR);

  printf("[pid = %d]\n", (int) getpid());
  if (confirmTerminal("Continue", "n"))
    return PCE_FAIL;
  if (confirmTerminal("Save core image", "n"))
    abort();

  hostAction(HOST_HALT);
  exit(1);
#else /*O_RUNTIME*/
  hostAction(HOST_RECOVER_FROM_FATAL_ERROR);
  hostAction(HOST_HALT);
  exit(1);
#endif /*O_RUNTIME*/

  va_end(args);
  return PCE_FAIL;
}


		/********************************
		*            SLEEP		*
		********************************/

#if defined(__WATCOMC__)
#include <dos.h>

void
msleep(int time)
{ if ( time > 0 )
    delay(time);
}

#else  /* !defined(__WATCOMC__) */

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

void
msleep(int time)
{ struct timeval timeout;
  timeout.tv_sec = time/1000;
  timeout.tv_usec = (time%1000)*1000;

  DEBUG(NAME_flash,
	printf("waiting %d milliseconds ...", time); fflush(stdout));

#ifdef SELLIST
  { SELLIST(1,1) readbits = {0, 0};
    SELLIST(1,1) writebits = {0, 0};
    SELLIST(1,1) exceptbits = {0, 0};

    select(0, &readbits, &writebits, &exceptbits, &timeout);
  }
#else
  select(32, NULL, NULL, NULL, &timeout);
#endif

  DEBUG(NAME_flash, printf("ok\n"));
}

#endif /*__WATCOMC__*/

#ifdef __msdos__
int
getdtablesize()
{ return 32;
}
#endif

int
pceAssert(int expr, char *text, char* file, int line)
{ if ( !expr )
    sysPce("%s:%d: Assertion failed: %s", file, line, text);

  return 0;
}

