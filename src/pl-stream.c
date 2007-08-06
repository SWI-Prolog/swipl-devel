/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

#ifdef __WINDOWS__
#include <uxnt.h>
#ifdef WIN64
#define MD "config/win64.h"
#else
#define MD "config/win32.h"
#endif
#include <winsock2.h>
#include "pl-mswchar.h"
#define CRLF_MAPPING 1
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This modules defines the  SWI-Prolog  I/O   streams.  These  streams are
provided to gain common access to  any   type  of character data: files,
stdio streams, but also resources, strings, XPCE objects, etc.

MT:

Multithreading is supported through  Slock()   and  Sunlock(). These are
recursive locks. If a stream handle  might   be  known to another thread
locking is required. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef MD
#include MD
#else
#include <config.h>
#endif

#if _FILE_OFFSET_BITS == 64 || defined(_LARGE_FILES)
#define O_LARGEFILES 1		/* use for conditional code in Prolog */
#else
#undef O_LARGEFILES
#endif

#define PL_KERNEL 1
#include <wchar.h>
typedef wchar_t pl_wchar_t;
#include "pl-stream.h"
#include "pl-utf8.h"
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#include <errno.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#else
#ifdef HAVE_SYS_MALLOC_H
#include <sys/malloc.h>
#endif
#endif
#include <memory.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <limits.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/stat.h>
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>			/* sprintf() for numeric values */
#include <assert.h>
#ifdef SYSLIB_H
#include SYSLIB_H
#endif

#ifndef MB_LEN_MAX
#define MB_LEN_MAX 6
#endif

#define ROUND(p, n) ((((p) + (n) - 1) & ~((n) - 1)))
#define UNDO_SIZE ROUND(MB_LEN_MAX, sizeof(wchar_t))

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define char_to_int(c)	(0xff & (int)(c))

#define TMPBUFSIZE 256			/* Serror bufsize for Svfprintf() */

int Slinesize = SIO_LINESIZE;		/* Sgets() buffer size */

static ssize_t	S__flushbuf(IOSTREAM *s);
static void	run_close_hooks(IOSTREAM *s);
static int	S__removebuf(IOSTREAM *s);
static int	S__seterror(IOSTREAM *s);

#ifdef O_PLMT
#define SLOCK(s)    if ( s->mutex ) recursiveMutexLock(s->mutex)
#define SUNLOCK(s)  if ( s->mutex ) recursiveMutexUnlock(s->mutex)
inline int
STRYLOCK(IOSTREAM *s)
{ if ( s->mutex &&
       recursiveMutexTryLock(s->mutex) == EBUSY )
    return FALSE;

  return TRUE;
}
#else
#define SLOCK(s)
#define SUNLOCK(s)
#define STRYLOCK(s) (TRUE)
#endif

#include "pl-error.h"
extern int 			fatalError(const char *fm, ...);
extern int 			PL_error(const char *pred, int arity,
					 const char *msg, int id, ...);
extern int			PL_handle_signals();
extern IOENC			initEncoding(void);

		 /*******************************
		 *	      BUFFER		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that the  buffer  is  allocated   from  s->unbuffer,  which  starts
UNDO_SIZE before s->buffer, so we can always push-back a wide
character into a multibyte stream. We do not do this for SIO_USERBUF
case, but this is only used by the output stream Svfprintf() where it is
not needed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
S__setbuf(IOSTREAM *s, char *buffer, int size)
{ if ( size == 0 )
    size = SIO_BUFSIZE;

  S__removebuf(s);
  s->bufsize = size;

  if ( buffer )
  { s->unbuffer = s->buffer = buffer;
    s->flags |= SIO_USERBUF;
  } else
  { if ( !(s->unbuffer = malloc(s->bufsize+UNDO_SIZE)) )
    { errno = ENOMEM;
      return -1;
    }
    s->flags &= ~SIO_USERBUF;
    s->buffer = s->unbuffer + UNDO_SIZE;
  }

  s->limitp = &s->buffer[s->bufsize];
  s->bufp   = s->buffer;

  return size;
}


void
Ssetbuffer(IOSTREAM *s, char *buffer, size_t size)
{ S__setbuf(s, buffer, size);
  s->flags &= ~SIO_USERBUF;
}


static int
S__removebuf(IOSTREAM *s)
{ if ( s->buffer && s->unbuffer )
  { int rval = 0;

    if ( (s->flags & SIO_OUTPUT) && S__flushbuf(s) < 0 )
      rval = -1;

    if ( !(s->flags & SIO_USERBUF) )
      free(s->unbuffer);
    s->bufp = s->limitp = s->buffer = s->unbuffer = NULL;
    s->bufsize = 0;

    return rval;
  }

  return 0;
}


#ifdef DEBUG_IO_LOCKS
static char *
Sname(IOSTREAM *s)
{ if ( s == Serror ) return "error";
  if ( s == Sinput ) return "input";
  if ( s == Soutput ) return "output";
  return "?";
}


#include <execinfo.h>
#include <string.h>

static void
print_trace(void)
{ void *array[3];
  size_t size;
  char **strings;
  size_t i;
     
  size = backtrace(array, sizeof(array)/sizeof(void *));
  strings = backtrace_symbols(array, size);
     
  printf(" Stack:");
  for(i = 1; i < size; i++)
  { printf(" [%d] %s", i, strings[i]);
  }
  printf("\n");
       
  free(strings);
}
#endif /*DEBUG_IO_LOCKS*/


int
Slock(IOSTREAM *s)
{ SLOCK(s);

#ifdef DEBUG_IO_LOCKS
  printf("  Lock: %d: %s: %d locks", PL_thread_self(), Sname(s), s->locks+1);
  print_trace();
#endif

  if ( !s->locks++ )
  { if ( (s->flags & (SIO_NBUF|SIO_OUTPUT)) == (SIO_NBUF|SIO_OUTPUT) )
      return S__setbuf(s, NULL, TMPBUFSIZE);
  }

  return 0;
}


int
StryLock(IOSTREAM *s)
{ if ( !STRYLOCK(s) )
    return -1;

  if ( !s->locks++ )
  { if ( (s->flags & (SIO_NBUF|SIO_OUTPUT)) == (SIO_NBUF|SIO_OUTPUT) )
      return S__setbuf(s, NULL, TMPBUFSIZE);
  }

  return 0;
}


int
Sunlock(IOSTREAM *s)
{ int rval = 0;

#ifdef DEBUG_IO_LOCKS
  printf("Unlock: %d: %s: %d locks", PL_thread_self(), Sname(s), s->locks-1);
  print_trace();
#endif

  if ( s->locks )
  { if ( --s->locks == 0 )
    { if ( (s->flags & (SIO_NBUF|SIO_OUTPUT)) == (SIO_NBUF|SIO_OUTPUT) )
	rval = S__removebuf(s);
    }
  } else
  { assert(0);
  }

  SUNLOCK(s);

  return rval;
}


		 /*******************************
		 *	     FLUSH/FILL		*
		 *******************************/

/* return values: -1: error, else #bytes written */
			 
static ssize_t
S__flushbuf(IOSTREAM *s)
{ char *from, *to;
  ssize_t rc;

  SLOCK(s);
  from = s->buffer;
  to   = s->bufp;

  while ( from < to )
  { size_t size = (size_t)(to - from);
    ssize_t n = (*s->functions->write)(s->handle, from, size);

    if ( n > 0 )			/* wrote some */
    { from += n;
    } else if ( n < 0 )			/* error */
    { S__seterror(s);
      rc = -1;
      goto out;
    } else				/* wrote nothing? */
    { break;
    }
  }

  if ( to == from )			/* full flush */
  { rc = s->bufp - s->buffer;
    s->bufp = s->buffer;
  } else				/* partial flush */
  { size_t left = to - from;

    rc = from - s->buffer;
    memmove(s->buffer, from, left);
    s->bufp = s->buffer + left;
  }

out:
  SUNLOCK(s);
  return rc;
}


static int
S__flushbufc(int c, IOSTREAM *s)
{ if ( s->buffer )
  { if ( S__flushbuf(s) <= 0 )		/* == 0: no progress!? */
      c = -1;
    else
      *s->bufp++ = (c & 0xff);
  } else
  { if ( s->flags & SIO_NBUF )
    { char chr = (char)c;
    
      if ( (*s->functions->write)(s->handle, &chr, 1) != 1 )
      { s->flags |= SIO_FERR;
	c = -1;
      }
    } else
    { if ( S__setbuf(s, NULL, 0) < 0 )
      { s->flags |= SIO_FERR;
	c = -1;
      } else
	*s->bufp++ = (char)c;
    }
  }

  return c;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S__fillbuf() fills the read-buffer, returning the first character of it.
It also realises the SWI-Prolog timeout facility.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
S__fillbuf(IOSTREAM *s)
{ int c;

  if ( s->flags & (SIO_FEOF|SIO_FERR) )
  { s->flags |= SIO_FEOF2;		/* reading past eof */
    return -1;
  }

#ifdef HAVE_SELECT
  s->flags &= ~SIO_TIMEOUT;

  if ( s->timeout >= 0 )
  { int fd = Sfileno(s);

    if ( fd >= 0 )
    { fd_set wait;
      struct timeval time;
      int rc;
      
      time.tv_sec  = s->timeout / 1000;
      time.tv_usec = (s->timeout % 1000) * 1000;
      FD_ZERO(&wait);
#ifdef __WINDOWS__
      FD_SET((SOCKET)fd, &wait);
#else
      FD_SET(fd, &wait);
#endif

      for(;;)
      { rc = select(fd+1, &wait, NULL, NULL, &time);
	
	if ( rc < 0 && errno == EINTR )
	{ if ( PL_handle_signals() < 0 )
	  { errno = EPLEXCEPTION;
	    return -1;
	  }

	  continue;
	}

	break;
      }

      if ( rc == 0 )
      { s->flags |= (SIO_TIMEOUT|SIO_FERR);
	return -1;
      }
    } else
    { errno = EPERM;			/* no permission to select */
      s->flags |= SIO_FERR;
      return -1;
    }
  }
#endif


  if ( s->flags & SIO_NBUF )
  { char chr;
    ssize_t n;

    if ( (n=(*s->functions->read)(s->handle, &chr, 1)) == 1 )
    { c = char_to_int(chr);
      return c;
    } else if ( n == 0 )
    { if ( !(s->flags & SIO_NOFEOF) )
	s->flags |= SIO_FEOF;
      return -1;
    } else
    { S__seterror(s);
      return -1;
    }
  } else
  { ssize_t n;
    size_t len;

    if ( !s->buffer )
    { if ( S__setbuf(s, NULL, 0) < 0 )
	return -1;
      s->bufp = s->limitp = s->buffer;
      len = s->bufsize;
    } else if ( s->bufp < s->limitp )
    { len = s->limitp - s->bufp;
      memmove(s->buffer, s->bufp, s->limitp - s->bufp);
      s->bufp = s->buffer;
      s->limitp = &s->bufp[len];
      len = s->bufsize - len;
    } else
    { s->bufp = s->limitp = s->buffer;
      len = s->bufsize;
    }

    if ( (n=(*s->functions->read)(s->handle, s->limitp, len)) > 0 )
    { s->limitp += n;
      c = char_to_int(*s->bufp++);
      return c;
    } else
    { if ( n == 0 )
      { if ( !(s->flags & SIO_NOFEOF) )
	  s->flags |= SIO_FEOF;
	return -1;
#ifdef EWOULDBLOCK
      } else if ( errno == EWOULDBLOCK )
      { s->bufp = s->buffer;
	s->limitp = s->buffer;
	return -1;
#endif
      } else
      { S__seterror(s);
	return -1;
      }
    }
  }
}

		 /*******************************
		 *	   CHARACTER I/O	*
		 *******************************/


static inline void
update_linepos(IOSTREAM *s, int c)
{ IOPOS *p = s->position;

  if ( c > '\r' )			/* speedup the 99% case a bit */
  { p->linepos++;
    return;
  }

  switch(c)
  { case '\n':
      p->lineno++;
      p->linepos = 0;
      s->flags &= ~SIO_NOLINEPOS;
#ifdef __WINDOWS__
      if ( s->flags & O_TEXT )
	p->charno++;			/* writes one extra! */
#endif
      break;
    case '\r':
      p->linepos = 0;
      s->flags &= ~SIO_NOLINEPOS;
      break;
    case '\b':
      if ( p->linepos > 0 )
	p->linepos--;
      break;
    case EOF:
      break;
    case '\t':
      p->linepos |= 7;
    default:
      p->linepos++;
  }
}



int
S__fupdatefilepos_getc(IOSTREAM *s, int c)
{ IOPOS *p = s->position;

  update_linepos(s, c);
  p->byteno++;
  p->charno++;

  return c;
}


static inline int
S__updatefilepos(IOSTREAM *s, int c)
{ IOPOS *p = s->position;

  if ( p )
  { update_linepos(s, c);
    p->charno++;
  }

  return c;
}


static inline int
get_byte(IOSTREAM *s)
{ int c = Snpgetc(s);

  if ( s->position )
    s->position->byteno++;

  return c;
}


static int
put_byte(int c, IOSTREAM *s)
{ c &= 0xff;

  if ( s->bufp < s->limitp )
  { *s->bufp++ = c;
  } else
  { if ( S__flushbufc(c, s) < 0 )
    { s->lastc = EOF;
      return -1;
    }
  }

  if ( s->position )
    s->position->byteno++;

  return c;
}


int
Sputc(int c, IOSTREAM *s)
{ c &= 0xff;

  if ( put_byte(c, s) < 0 )
    return -1;

  s->lastc = c;

  if ( c == '\n' && (s->flags & SIO_LBUF) )
  { if ( S__flushbuf(s) < 0 )
      return -1;
  }

  return S__updatefilepos(s, c);
}


int
Sfgetc(IOSTREAM *s)
{ return Sgetc(s);
}


static inline void
unget_byte(int c, IOSTREAM *s)
{ IOPOS *p = s->position;

  *--s->bufp = c;
  if ( p )
  { p->charno--;
    p->byteno--;
    s->flags |= (SIO_NOLINENO|SIO_NOLINEPOS);
  }
}


int
Sungetc(int c, IOSTREAM *s)
{ if ( s->bufp > s->unbuffer )
  { unget_byte(c, s);

    return c;
  }

  return -1;
}


static int
reperror(int c, IOSTREAM *s)
{ if ( c >= 0 && (s->flags & (SIO_REPXML|SIO_REPPL)) )
  { char buf[16];
    const char *q;

    if ( (s->flags & SIO_REPPL) )
    { if ( c <= 0xffff )
	sprintf(buf, "\\u%04X", c);
      else
	sprintf(buf, "\\U%08X", c);
    } else
      sprintf(buf, "&#%d;", c);

    for(q = buf; *q; q++)
    { if ( put_byte(*q, s) < 0 )
	return -1;
    }
	
    return c;
  }

  Sseterr(s, SIO_FERR|SIO_CLEARERR, "Encoding cannot represent character");
  return -1;
}



int
Sputcode(int c, IOSTREAM *s)
{ if ( c < 0 )
    return reperror(c, s);

  if ( s->tee && s->tee->magic == SIO_MAGIC )
    Sputcode(c, s->tee);

#ifdef CRLF_MAPPING
  if ( c == '\n' && (s->flags&SIO_TEXT) )
  { if ( Sputcode('\r', s) < 0 )
      return -1;
  }
#endif

  switch(s->encoding)
  { case ENC_OCTET:
    case ENC_ISO_LATIN_1:
      if ( c >= 256 )
      { if ( reperror(c, s) < 0 )
	  return -1;
	break;
      }
    simple:
      if ( put_byte(c, s) < 0 )
	return -1;
      break;
    case ENC_ASCII:
      if ( c >= 128 )
      { if ( reperror(c, s) < 0 )
	  return -1;
	break;
      }
      goto simple;
    case ENC_ANSI:
    { char b[MB_LEN_MAX];
      size_t n;

      if ( !s->mbstate )
      { if ( !(s->mbstate = malloc(sizeof(*s->mbstate))) )
	  return EOF;			/* out of memory */
	memset(s->mbstate, 0, sizeof(*s->mbstate));
      }

      if ( (n = wcrtomb(b, (wchar_t)c, s->mbstate)) == (size_t)-1 )
      { if ( reperror(c, s) < 0 )
	  return -1;
      } else
      { size_t i;

	for(i=0; i<n; i++)
	{ if ( put_byte(b[i]&0xff, s) < 0 )
	    return -1;
	}
      }

      break;
    }
    case ENC_UTF8:
    { char buf[6];
      char *p, *end;
      
      if ( c < 128 )
	goto simple;

      end = utf8_put_char(buf, c);
      for(p=buf; p<end; p++)
      { if ( put_byte(*p&0xff, s) < 0 )
	  return -1;
      }

      break;
    }
    case ENC_UNICODE_BE:
      if ( put_byte(c>>8, s) < 0 )
	return -1;
      if ( put_byte(c&0xff, s) < 0 )
	return -1;
      break;
    case ENC_UNICODE_LE:
      if ( put_byte(c&0xff, s) < 0 )
	return -1;
      if ( put_byte(c>>8, s) < 0 )
	return -1;
      break;
    case ENC_WCHAR:
    { pl_wchar_t chr = c;
      unsigned char *q = (unsigned char *)&chr;
      unsigned char *e = &q[sizeof(pl_wchar_t)];

      while(q<e)
      { if ( put_byte(*q++, s) < 0 )
	  return -1;
      }
      
      break;
    }
    case ENC_UNKNOWN:
      return -1;
  }


  s->lastc = c;

  if ( c == '\n' && (s->flags & SIO_LBUF) )
  { if ( S__flushbuf(s) < 0 )
      return -1;
  }

  return S__updatefilepos(s, c);
}


int
Scanrepresent(int c, IOSTREAM *s)
{ switch(s->encoding)
  { case ENC_OCTET:
    case ENC_ISO_LATIN_1:
      if ( c <= 0xff )
	return 0;
      return -1;
    case ENC_ASCII:
      if ( c < 0x7f )
	return 0;
      return -1;
    case ENC_ANSI:
    { mbstate_t state;
      char b[MB_LEN_MAX];

      memset(&state, 0, sizeof(state));
      if ( wcrtomb(b, (wchar_t)c, &state) != (size_t)-1 )
	return 0;
      return -1;
    }
    case ENC_WCHAR:
      if ( sizeof(wchar_t) > 2 )
	return 0;
    /*FALLTHROUGH*/
    case ENC_UNICODE_BE:
    case ENC_UNICODE_LE:
      if ( c <= 0xffff )
	return 0;
      return -1;
    case ENC_UTF8:
      return 0;
    default:
      assert(0);
      return -1;
  }
}


int
Sgetcode(IOSTREAM *s)
{ int c;

#ifdef CRLF_MAPPING
retry:
#endif

  switch(s->encoding)
  { case ENC_OCTET:
    case ENC_ISO_LATIN_1:
      c = get_byte(s);
      break;
    case ENC_ASCII:
    { c = get_byte(s);
      if ( c > 128 )
	Sseterr(s, SIO_WARN, "non-ASCII character");
      break;
    }
    case ENC_ANSI:
    { char b[1];
      size_t rc, n = 0;
      wchar_t wc;

      if ( !s->mbstate )
      { if ( !(s->mbstate = malloc(sizeof(*s->mbstate))) )
	  return EOF;			/* out of memory */
	memset(s->mbstate, 0, sizeof(*s->mbstate));
      }

      for(;;)
      { if ( (c = get_byte(s)) == EOF )
	{ if ( n == 0 )
	    return EOF;
	  else
	  { Sseterr(s, SIO_WARN, "EOF in multibyte Sequence");
	    goto mberr;
	  }
	}
	b[0] = c;

	if ( (rc=mbrtowc(&wc, b, 1, s->mbstate)) == 1 )
	{ c = wc;
	  goto out;
	} else if ( rc == (size_t)-1 )
	{ Sseterr(s, SIO_WARN, "Illegal multibyte Sequence");
	  goto mberr;
	}				/* else -2: incomplete */
      }

    mberr:
      c = UTF8_MALFORMED_REPLACEMENT;
      goto out;
    }
    case ENC_UTF8:
    { c = get_byte(s);
      if ( c == EOF )
	break;

      if ( c & 0x80 )
      { int extra = UTF8_FBN(c);
	int code;

	if ( extra < 0 )
	{ Sseterr(s, SIO_WARN, "Illegal UTF-8 start");
	  c = UTF8_MALFORMED_REPLACEMENT;
	  goto out;
	}

	code = UTF8_FBV(c,extra);
	for( ; extra > 0; extra-- )
	{ int c2 = get_byte(s);
	  
	  if ( !ISUTF8_CB(c2) )
	  { Sseterr(s, SIO_WARN, "Illegal UTF-8 continuation");
	    c = UTF8_MALFORMED_REPLACEMENT;
	    Sungetc(c2, s);
	    goto out;
	  }
	  code = (code<<6)+(c2&0x3f);
	}
	c = code;
      }
      break;
    }
    case ENC_UNICODE_BE:
    case ENC_UNICODE_LE:
    { int c1, c2;

      c1 = get_byte(s);
      if ( c1 == EOF )
	return EOF;
      c2 = get_byte(s);

      if ( c2 == EOF )
      { Sseterr(s, SIO_WARN, "EOF in unicode character");
	c = UTF8_MALFORMED_REPLACEMENT;
      } else
      { if ( s->encoding == ENC_UNICODE_BE )
	  c = (c1<<8)+c2;
	else
	  c = (c2<<8)+c1;
      }

      break;
    }
    case ENC_WCHAR:
    { pl_wchar_t chr;
      char *p = (char*)&chr;
      size_t n;

      for(n=0; n<sizeof(pl_wchar_t); n++)
      { int c1 = get_byte(s);

	if ( c1 == EOF )
	{ if ( n == 0 )
	  { return EOF;
	  } else
	  { Sseterr(s, SIO_WARN, "EOF in UCS character");
	    c = UTF8_MALFORMED_REPLACEMENT; 
	    goto out;
	  }
	}

	*p++ = c1;
      }

      c = chr;
      break;
    }
    default:
      assert(0);
      c = -1;
  }

out:
#ifdef CRLF_MAPPING
  if ( c == '\r' && (s->flags&SIO_TEXT) && !(s->flags&SIO_ISATTY) )
    goto retry;
#endif

  if ( s->tee && s->tee->magic == SIO_MAGIC && c != -1 )
    Sputcode(c, s->tee);

  return S__updatefilepos(s, c);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) For ENC_ANSI there is  a  problem   as  this  deals with multi-modal
streams, streams that  may  hold  escape   sequences  to  move  from one
character set to another: ascii  ...   <esc1>  japanese <esc2> ascii ...
Suppose now we have two characters   [ascii, japanese]. When reading the
japanese character the  first  time,  the   system  will  translate  the
<esc><japanese> and the mode will be   japanese. When pushing back, only
the japanese character will be put back,   not the escape sequence. What
to do?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Sungetcode(int c, IOSTREAM *s)
{ switch(s->encoding)
  { case ENC_OCTET:
    case ENC_ISO_LATIN_1:
      if ( c >= 256 )
	return -1;			/* illegal */
    simple:
      if ( s->bufp > s->unbuffer )
      { unget_byte(c, s);
        return c;
      }
      return -1;			/* no room */
    case ENC_ASCII:
      if ( c >= 128 )
	return -1;			/* illegal */
      goto simple;
    case ENC_ANSI:			/* (*) See above */
    { char b[MB_LEN_MAX];
      size_t n;

      if ( !s->mbstate )		/* do we need a seperate state? */
      { if ( !(s->mbstate = malloc(sizeof(*s->mbstate))) )
	  return EOF;			/* out of memory */
	memset(s->mbstate, 0, sizeof(*s->mbstate));
      }

      if ( (n = wcrtomb(b, (wchar_t)c, s->mbstate)) != (size_t)-1 &&
	   s->bufp >= n + s->unbuffer )
      { size_t i;

	for(i=n; i-- > 0; )
	{ unget_byte(b[i], s);
	}

        return c;
      }

      return -1;
    }
    case ENC_UTF8:
    { if ( (unsigned)c >= 0x8000000 )
	return -1;

      if ( c < 0x80 )
      { goto simple;
      } else
      { char buf[6];
	char *p, *end;

	end = utf8_put_char(buf, c);
	if ( s->bufp - s->unbuffer >= end-buf )
	{ for(p=end-1; p>=buf; p--)
	  { unget_byte(*p, s);
	  }

          return c;
	}

	return -1;
      }
    }
    case ENC_UNICODE_BE:
    { if ( c >= 0x10000 )
	return -1;

      if ( s->bufp-1 > s->unbuffer )
      { unget_byte(c&0xff, s);
	unget_byte((c>>8)&0xff, s);

        return c;
      }
      return -1;
    }
    case ENC_UNICODE_LE:
    { if ( c >= 0x10000 )
	return -1;

      if ( s->bufp-1 > s->unbuffer )
      { unget_byte((c>>8)&0xff, s);
	unget_byte(c&0xff, s);

        return c;
      }
      return -1;
    }
    case ENC_WCHAR:
    { pl_wchar_t chr = c;

      if ( s->bufp-sizeof(chr) >= s->unbuffer )
      { char *p = (char*)&chr;
	int n;

	for(n=sizeof(chr); --n>=0; )
	  unget_byte(p[n], s);

	return c;
      }
      return -1;
    }
    case ENC_UNKNOWN:
      return -1;
  }

  assert(0);
  return -1;
}

		 /*******************************
		 *	    PUTW/GETW		*
		 *******************************/

int
Sputw(int w, IOSTREAM *s)
{ unsigned char *q = (unsigned char *)&w;
  unsigned int n;

  for(n=0; n<sizeof(w); n++)
  { if ( Sputc(*q++, s) < 0 )
      return -1;
  }

  return w;
}


int
Sgetw(IOSTREAM *s)
{ int w;
  unsigned char *q = (unsigned char *)&w;
  unsigned int n;

  for(n=0; n<sizeof(w); n++)
  { int c;

    if ( (c = Sgetc(s)) < 0 )
      return -1;
    *q++ = c & 0xff;
  }

  return w;
}

		 /*******************************
		 *	    FREAD/FWRITE	*
		 *******************************/

size_t
Sfread(void *data, size_t size, size_t elms, IOSTREAM *s)
{ size_t chars = size * elms;
  char *buf = data;

  for( ; chars > 0; chars-- )
  { int c;

    if ( (c = Sgetc(s)) == EOF )
      break;

    *buf++ = c & 0xff;
  }
  
  return (size*elms - chars)/size;
}


size_t
Sfwrite(const void *data, size_t size, size_t elms, IOSTREAM *s)
{ size_t chars = size * elms;
  const char *buf = data;

  for( ; chars > 0; chars-- )
  { if ( Sputc(*buf++, s) < 0 )
      break;
  }
  
  return (size*elms - chars)/size;
}


		 /*******************************
		 *	       PENDING		*
		 *******************************/

ssize_t
Sread_pending(IOSTREAM *s, char *buf, size_t limit, int flags)
{ int done = 0;
  size_t n;

  if ( s->bufp >= s->limitp && (flags & SIO_RP_BLOCK) )
  { int c = S__fillbuf(s);

    if ( c < 0 )
      return c;

    buf[0] = c;
    limit--;
    done = 1;
  }

  n = s->limitp - s->bufp;
  if ( n > limit )
    n = limit;
  memcpy(&buf[done], s->bufp, n);
  s->bufp += n;

  return done+n;
}


		 /*******************************
		 *               BOM		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check the stream for a BOM (Byte Order Marker).  If present (and known),
update the stream encoding.  Return value is one of

	-1:	error (check errno)
	 0:	ok.  If BOM, SIO_BOM is added to flags
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ IOENC encoding;
  unsigned int bomlen;
  const char *bom;
} bomdef;

static const bomdef bomdefs[] =
{ { ENC_UTF8,       3, "\357\273\277" }, /* 0xef, 0xbb, 0xbb */
  { ENC_UNICODE_BE, 2, "\376\377" },	 /* 0xfe, oxff */
  { ENC_UNICODE_LE, 2, "\377\376" },	 /* 0xff, oxfe */
  { ENC_UNKNOWN,    0, NULL }
};

int
ScheckBOM(IOSTREAM *s)
{ if ( (s->flags & SIO_NBUF) )
  { errno = EINVAL;
    return -1;
  }

  for(;;)
  { size_t avail = s->limitp - s->bufp;
    const bomdef *bd;

    for(bd=bomdefs; bd->bomlen; bd++)
    { if ( avail >= bd->bomlen && memcmp(s->bufp, bd->bom, bd->bomlen) == 0 )
      { s->encoding = bd->encoding;
	s->bufp += bd->bomlen;
	s->flags |= SIO_BOM;
	return 0;
      }
    }

    if ( avail >= 4 )			/* longest BOM */
      return 0;

    if ( S__fillbuf(s) == -1 )
      return 0;				/* empty stream */
    s->bufp--;
  }
}


int
SwriteBOM(IOSTREAM *s)
{ switch(s->encoding)
  { case ENC_UTF8:
    case ENC_UNICODE_LE:
    case ENC_UNICODE_BE:
    { if ( Sputcode(0xfeff, s) != -1 )
      { s->flags |= SIO_BOM;

	return 0;
      }
      return -1;
    }
    default:
      return 0;
  }
}


		 /*******************************
		 *               FLAGS		*
		 *******************************/

int
Sfeof(IOSTREAM *s)
{ if ( s->flags & SIO_FEOF )
    return TRUE;

  if ( s->bufp < s->limitp )
    return FALSE;

  if ( s->flags & SIO_NBUF )
  { errno = EINVAL;
    return -1;
  }

  if ( S__fillbuf(s) == -1 )
    return TRUE;

  s->bufp--;
  return FALSE;
}
    

static int
S__seterror(IOSTREAM *s)
{ if ( s->functions->control )
  { char *msg;

    if ( (*s->functions->control)(s->handle,
				  SIO_LASTERROR, 
				  (void *)&msg) == 0 )
    { Sseterr(s, SIO_FERR, msg);
      return 0;
    } 
  }

  s->flags |= SIO_FERR;
  return 0;
}


int
Sferror(IOSTREAM *s)
{ return (s->flags & SIO_FERR) != 0;
}
    

int
Sfpasteof(IOSTREAM *s)
{ return (s->flags & (SIO_FEOF2ERR|SIO_FEOF2)) == (SIO_FEOF2ERR|SIO_FEOF2);
}


void
Sclearerr(IOSTREAM *s)
{ s->flags &= ~(SIO_FEOF|SIO_WARN|SIO_FERR|SIO_FEOF2|SIO_TIMEOUT|SIO_CLEARERR);
  Sseterr(s, 0, NULL);
}


void
Sseterr(IOSTREAM *s, int flag, const char *message)
{ if ( s->message )
  { free(s->message);
    s->message = NULL;
    s->flags &= ~SIO_CLEARERR;
  }
  if ( message )
  { s->flags |= flag;
    s->message = strdup(message);
  } else
  { s->flags &= ~flag;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the encoding of a stream. The enc   argument is the new encoding. If
old is not NULL, the old encoding is written to the given location.

Please note that not all code changing  the encoding call Ssetenc at the
moment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Ssetenc(IOSTREAM *s, IOENC enc, IOENC *old)
{ if ( old )
    *old = s->encoding;
  if ( enc == s->encoding )
    return 0;

  if ( s->functions->control )
  { if ( (*s->functions->control)(s->handle,
				  SIO_SETENCODING, 
				  (void *)&enc) != 0 )
      return -1;
  }

  s->encoding = enc;
  return 0;
}

		 /*******************************
		 *	      FLUSH		*
		 *******************************/

int
Sflush(IOSTREAM *s)
{ if ( s->buffer && (s->flags & SIO_OUTPUT) )
  { if ( S__flushbuf(s) < 0 )
      return -1;
    if ( s->functions->control &&
	 (*s->functions->control)(s->handle, SIO_FLUSHOUTPUT, NULL) < 0 )
      return -1;
  }

  return 0;
}

		 /*******************************
		 *	      SEEK		*
		 *******************************/

int
Sunit_size(IOSTREAM *s)
{ switch(s->encoding)
  { case ENC_UNKNOWN:
    case ENC_OCTET:
    case ENC_ASCII:
    case ENC_ISO_LATIN_1:
    case ENC_ANSI:
    case ENC_UTF8:
      return 1;
    case ENC_UNICODE_BE:
    case ENC_UNICODE_LE:
      return 2;
    case ENC_WCHAR:
      return sizeof(wchar_t);
    default:
      assert(0);
      return 1;				/* not reached */
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return the size of the underlying data object.  Should be optimized;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

long
Ssize(IOSTREAM *s)
{ if ( s->functions->control )
  { long size;

    if ( (*s->functions->control)(s->handle, SIO_GETSIZE, (void *)&size) == 0 )
      return size;
  }
  if ( s->functions->seek )
  { long here = Stell(s);
    long end;

    if ( Sseek(s, 0, SIO_SEEK_END) == 0 )
      end = Stell(s);
    else
      end = -1;
    Sseek(s, here, SIO_SEEK_SET);

    return end;
  }

  errno = ESPIPE;
  return -1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sseek64(IOSTREAM *s, int64_t pos, int whence)

Re-position the stream to byte-no 'pos'.

Maybe we should optimise this to become block-aligned?  Or can we leave
this to read/write?

The first part checks whether  repositioning   the  read  pointer in the
buffer suffices to achieve the seek.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Sseek64(IOSTREAM *s, int64_t pos, int whence)
{ if ( (s->flags & SIO_INPUT) && s->limitp > s->buffer ) /* something there */
  { int64_t now = Stell64(s);

    if ( now != -1 )
    { int64_t newpos;
      char *nbufp = (char *)-1;
    
      if ( whence == SIO_SEEK_CUR )
      { nbufp = s->bufp + pos;
	newpos = now + pos;
      } else if ( whence == SIO_SEEK_SET )
      { nbufp = s->bufp + (pos - now);
	newpos = pos;
      } else
	newpos = -1;			/* should not happen */

      if ( nbufp >= s->buffer && nbufp < s->limitp )
      { s->bufp = nbufp;

	pos = newpos;
	goto update;
      }
    }
  }

  if ( !s->functions->seek && !s->functions->seek64 )
  { errno = ESPIPE;
    return -1;
  }

  Sflush(s);
    
  s->bufp   = s->buffer;
  if ( (s->flags & SIO_INPUT) )
    s->limitp = s->buffer;

  if ( whence == SIO_SEEK_CUR )
  { pos += Stell64(s);
    whence = SIO_SEEK_SET;
  }
  
  if ( s->functions->seek64 )
    pos = (*s->functions->seek64)(s->handle, pos, whence);
  else if ( pos <= LONG_MAX )
    pos = (*s->functions->seek)(s->handle, (long)pos, whence);
  else
  { errno = EINVAL;
    return -1;
  }
  
  if ( pos < 0 )
  { errno = EINVAL;
    return -1;
  }

update:
  s->flags &= ~SIO_FEOF;		/* not on eof of file anymore */

  if ( s->position )
  { s->flags |= (SIO_NOLINENO|SIO_NOLINEPOS); /* no update this */
    s->position->byteno = pos;
    s->position->charno = pos/Sunit_size(s); /* compatibility */
  }

  return 0;
}


int
Sseek(IOSTREAM *s, long pos, int whence)
{ return Sseek64(s, (int64_t)pos, whence);
}



/* Stell64(IOSTREAM *s) returns the current position in the file in
   bytes
*/

int64_t
Stell64(IOSTREAM *s)
{ if ( s->position )
  { return s->position->byteno;
  } else if ( s->functions->seek || s->functions->seek64 )
  { int64_t pos;

    if ( s->functions->seek64 )
      pos = (*s->functions->seek64)(s->handle, 0L, SIO_SEEK_CUR);
    else
      pos = (*s->functions->seek)(s->handle, 0L, SIO_SEEK_CUR);

    if ( s->buffer )			/* open */
    { int64_t off = s->bufp - s->buffer;

      if ( s->flags & SIO_INPUT )
	off -= s->limitp - s->buffer;

      pos += off;
    }

    return pos;
  } else
  { errno = EINVAL;
    return -1;
  }
}


long
Stell(IOSTREAM *s)
{ int64_t pos = Stell64(s);

  if ( pos <= LONG_MAX )
    return (long) pos;

  errno = EINVAL;
  return -1;
}


		 /*******************************
		 *	      CLOSE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) Sclose() can be called recursively. For example if an XPCE object is
only referenced from an open stream,  the close-function will delete the
object, which in turn calls the  ->unlink   which  may wish to close the
associated stream.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Sclose(IOSTREAM *s)
{ int rval = 0;

  if ( s->magic != SIO_MAGIC )		/* already closed!? */
  { errno = EINVAL;
    return -1;
  }

  if ( (s->flags&SIO_CLOSING) )		/* recursive (*) */
    return rval;

  if ( s->upstream )
  { errno = EPERM;
    return -1;
  }

  SLOCK(s);
  s->flags |= SIO_CLOSING;
  rval = S__removebuf(s);
  if ( s->mbstate )
    free(s->mbstate);

#ifdef __WINDOWS__
  if ( (s->flags & SIO_ADVLOCK) )
  { OVERLAPPED ov;
    HANDLE h = (HANDLE)_get_osfhandle((int)s->handle);

    memset(&ov, 0, sizeof(ov));
    UnlockFileEx(h, 0, 0, 0xffffffff, &ov);
    s->flags &= ~SIO_ADVLOCK;
  }
#endif
  if ( s->functions->close && (*s->functions->close)(s->handle) < 0 )
    rval = -1;
  run_close_hooks(s);
  while(s->locks > 0)			/* remove buffer-locks */
    rval = Sunlock(s);

  SUNLOCK(s);

#ifdef O_PLMT
  if ( s->mutex )
  { recursiveMutexDelete(s->mutex);
    free(s->mutex);
    s->mutex = NULL;
  }
#endif

  s->magic = SIO_CMAGIC;
  if ( !(s->flags & SIO_STATIC) )
    free(s);

  return rval;
}


		 /*******************************
		 *	     STRING I/O		*
		 *******************************/

char *
Sfgets(char *buf, int n, IOSTREAM *s)
{ char *q = buf;

  while( n-- > 0 )
  { int c = Sgetc(s);

    if ( c == EOF )
    { *q = '\0';
      if ( q == buf )
	buf = NULL;
      goto out;
    } else
    { *q++ = c;
      if ( c == '\n' )
      { if ( n > 0 )
	  *q = '\0';
	goto out;
      }
    }
  }

out:
  return buf;
}


char *
Sgets(char *buf)
{ char *s = Sfgets(buf, Slinesize, Sinput);
  char *q;

  if ( s )				/* delete trailing \n */
  { q = &s[strlen(s)];
    if ( q > s && q[-1] == '\n' )
      *--q = '\0';
  }

  return s;
}


int
Sfputs(const char *q, IOSTREAM *s)
{ 
  for( ; *q; q++)
  { if ( Sputcode(*q&0xff, s) < 0 )
      return EOF;
  }

  return 0;
}


int
Sputs(const char *q)
{ return Sfputs(q, Soutput);
}


		 /*******************************
		 *	       PRINTF		*
		 *******************************/

int
Sfprintf(IOSTREAM *s, const char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svfprintf(s, fm, args);
  va_end(args);

  return rval;
}


int
Sprintf(const char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svfprintf(Soutput, fm, args);
  va_end(args);

  return rval;
}


int
Svprintf(const char *fm, va_list args)
{ return Svfprintf(Soutput, fm, args);
}


#define NEXTCHR(s, c)	if ( utf8 ) \
			{ (s) = utf8_get_char((s), &(c)); \
			} else \
			{ c = *(s)++; c &= 0xff; \
			}

#define OUTCHR(s, c)	do { printed++; \
			     if ( Sputcode((c), (s)) < 0 ) goto error; \
			   } while(0)
#define valdigit(c)	((c) - '0')
#define A_LEFT	0			/* left-aligned field */
#define A_RIGHT 1			/* right-aligned field */

int
Svfprintf(IOSTREAM *s, const char *fm, va_list args)
{ intptr_t printed = 0;
  char buf[TMPBUFSIZE];
  int tmpbuf;

  SLOCK(s);

  if ( !s->buffer && (s->flags & SIO_NBUF) )
  { S__setbuf(s, buf, sizeof(buf));
    tmpbuf = TRUE;
  } else
    tmpbuf = FALSE;

  while(*fm)
  { if ( *fm == '%' )
    { fm++;

      if ( *fm == '%' )
      { OUTCHR(s, *fm);
	fm++;
	continue;
      } else
      { int align = A_RIGHT;
	int modified = FALSE;
	int has_arg1 = FALSE, has_arg2 = FALSE;
	int arg1=0, arg2=0;
	char fbuf[100], *fs = fbuf, *fe = fbuf;
	int islong = 0;
	int pad = ' ';
	int utf8 = FALSE;

	for(;;)
	{ switch(*fm)
	  { case '+':	align = A_RIGHT; fm++; continue;
	    case '-':	align = A_LEFT;  fm++; continue;
	    case '0':	pad = '0';	 fm++; continue;
	    case ' ':	pad = ' ';       fm++; continue;
	    case '#':   modified = TRUE; fm++; continue;
	  }
	  break;
	}

	if ( *fm == '*' )
	{ has_arg1++;
	  fm++;
	  arg1 = va_arg(args, int);
	} else if ( isdigit(char_to_int(*fm)) )
	{ if ( *fm == '0' )
	    pad = '0';
	  arg1 = valdigit(*fm);
	  has_arg1++;
	  for( fm++; isdigit(char_to_int(*fm)); fm++)
	    arg1 = arg1*10 + valdigit(*fm);
	}
	if ( *fm == '.' )
	{ has_arg2++;
	  fm++;
	  if ( *fm == '*' )
	  { arg2 = va_arg(args, int);
	  } else
	  { arg2 = 0;
	    for( ; isdigit(char_to_int(*fm)); fm++)
	      arg2 = arg2*10 + valdigit(*fm);
	  }
	}

	if ( *fm == 'l' )
	{ islong++;			/* 1: %ld */
	  fm++;
	}
	if ( *fm == 'l' )
	{ islong++;			/* 2: %lld */
	  fm++;
	}
	if ( *fm == 'U' )		/* %Us: UTF-8 string */
	{ utf8 = TRUE;
	  fm++;
	}

	switch(*fm)
	{ case 'c':
	    *fe++ = va_arg(args, int);
	    break;
	  case 'p':
	  { void *ptr = va_arg(args, void*);
	    char fmbuf[8], *fp=fmbuf;
	    *fp++ = '%';
	    if ( modified )
	      *fp++ = '#';
	    *fp++ = 'p';
	    *fp   = '\0';
	    sprintf(fs, fmbuf, ptr);
	    fe = &fs[strlen(fs)];

	    break;
	  }
	  case 'd':
	  case 'i':
	  case 'o':
	  case 'u':
	  case 'x':
	  case 'X':
	  { intptr_t v = 0;			/* make compiler silent */
	    int64_t vl = 0;
	    char fmbuf[8], *fp=fmbuf;

	    switch( islong )
	    { case 0:
		v = va_arg(args, int);
	        break;
	      case 1:
		v = va_arg(args, intptr_t);
	        break;
	      case 2:
	        vl = va_arg(args, int64_t);
		break;
	    }

	    *fp++ = '%';
	    if ( modified )
	      *fp++ = '#';
	    *fp++ = 'l';
	    if ( islong < 2 )
	    { *fp++ = *fm;
	      *fp   = '\0';
	      sprintf(fs, fmbuf, v);
	    } else
	    {
#ifdef __WINDOWS__
	      strcat(fp-1, "I64");	/* Synchronise with INT64_FORMAT! */
	      fp += strlen(fp);
#else
	      *fp++ = 'l';
#endif
	      *fp++ = *fm;
	      *fp   = '\0';
	      sprintf(fs, fmbuf, vl);
	    }
	    fe = &fs[strlen(fs)];

	    break;
	  }
	  case 'f':
	  case 'e':
	  case 'E':
	  case 'g':
	  case 'G':
	  { double v = va_arg(args, double);
	    char fmbuf[8], *fp=fmbuf;

	    *fp++ = '%';
	    if ( modified )
	      *fp++ = '#';
	    if ( has_arg2 )		/* specified percission */
	    { *fp++ = '.';
	      *fp++ = '*';
	      *fp++ = *fm;
	      *fp   = '\0';
	      sprintf(fs, fmbuf, arg2, v);
	    } else
	    { *fp++ = *fm;
	      *fp   = '\0';
	      sprintf(fs, fmbuf, v);
	    }
	    fe = &fs[strlen(fs)];

	    break;
	  }
	  case 's':
	    fs = va_arg(args, char *);
	    if ( !fs )
	      fs = "(null)";
	    break;
	}

	if ( has_arg1 )			/* aligned field */
	{ if ( fs == fbuf )
	    *fe = '\0';

	  if ( align == A_LEFT )
	  { int w = 0;
	    while(*fs)
	    { int c;
	      NEXTCHR(fs, c);
	      OUTCHR(s, c);
	      w++;
	    }
	    while(w < arg1)
	    { OUTCHR(s, pad);
	      w++;
	    }
	  } else /*if ( align == A_RIGHT ) */
	  { size_t w;

	    if ( fs == fbuf )
	      w = fe - fs;
	    else
	      w = strlen(fs);

	    if ( utf8 )
	      w = utf8_strlen(fs, w);

	    if ( (ssize_t)w < arg1 )
	    { w = arg1 - w;
	      while(w > 0 )
	      { OUTCHR(s, pad);
		w--;
	      }
	    }
	    while(*fs)
	    { int c;
	      NEXTCHR(fs, c);
	      OUTCHR(s, c);
	    }
	  }
	} else
	{ if ( fs == fbuf )		/* unaligned field, just output */
	  { while(fs < fe)
	      OUTCHR(s, *fs++);
	  } else
	  { while(*fs)
	    { int c;
	      NEXTCHR(fs, c);
	      OUTCHR(s, c);
	    }
	  }
	}
	fm++;
      }
    } else if ( *fm == '\\' && fm[1] )
    { OUTCHR(s, fm[1]);
      fm += 2;
    } else
    { OUTCHR(s, *fm);
      fm++;
    }
  }

  if ( tmpbuf )
  { if ( S__removebuf(s) < 0 )
      goto error;
  }

  SUNLOCK(s);
  return (int)printed;

error:
  SUNLOCK(s);
  return -1;
}


int
Ssprintf(char *buf, const char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svsprintf(buf, fm, args);
  va_end(args);

  return rval;
}


int
Svsprintf(char *buf, const char *fm, va_list args)
{ IOSTREAM s;
  int rval;

  memset(&s, 0, sizeof(s));
  s.bufp      = buf;
  s.limitp    = (char *)(~0L);
  s.buffer    = buf;
  s.flags     = SIO_FBUF|SIO_OUTPUT;
  s.encoding  = ENC_ISO_LATIN_1;
  
  if ( (rval = Svfprintf(&s, fm, args)) >= 0 )
    *s.bufp = '\0';

  return rval;
}


int
Svdprintf(const char *fm, va_list args)
{ int rval;
  IOSTREAM *s = Soutput;

  Slock(s);
  rval = Svfprintf(s, fm, args);
#if defined(_DEBUG) && defined(__WINDOWS__)
  Sputc('\0', s);
  s->bufp--;				/* `Unput' */
  OutputDebugString(s->buffer);
#endif
  if ( Sflush(s) != 0 )
    rval = -1;
  Sunlock(s);

  return rval;
}


int
Sdprintf(const char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svdprintf(fm, args);
  va_end(args);

  return rval;
}

#if 0
		 /*******************************
		 *	      SCANF		*
		 *******************************/

int
Svfscanf(IOSTREAM *s, const char *fm, va_list args)
{ int done = 0;				/* # items converted */
  int chread = 0;			/* # characters read */
  int c = GET(s);			/* current character */
  int supress;				/* if TRUE, don't assign (*) */
  int field_width;			/* max width of field */
  int tsize;				/* SZ_SHORT, SZ_NORMAL, SZ_LONG */

  while(*fm)
  { if ( *fm == ' ' )
    { while ( isblank(c) )
	c = GET(s);
      fm++;
      continue;
    } else if ( *fm == '%' && fm[1] != '%' )
    { supress = FALSE;
      field_width = -1;
      int size = SZ_STANDARD;

      for(;;)				/* parse modifiers */
      { fm++;
	if ( isdigit(*fm) )
	{ field_width = valdigit(*fm);
	  for(++fm; isdigit(*fm); fm++)
	    field_width = 10*field_width + valdigit(*fm);
	  fm--;
	  continue;
	}
	if ( *fm == '*' )
	{ supress++;
	  continue;
	}
	if ( *fm == 'l' )
	{ size = SZ_LONG;
	  continue;
	}
	if ( *fm == 'h' )
	{ size = SZ_SHORT;
	  continue;
	}
      }
	
      if ( *fm != '[' && *fm != c )
	while(isblank(c))
	  c = GET(s);

      switch(*fm)
      { { intptr_t v;			/* collect value here */
	  int negative;			/* true if < 0 */
	  int base;			/* base for conversion */
	  int ok;			/* successful */
	case 'd':
	  base = 10;

	do_int:
	  negative = FALSE;
	  if ( c == '+' )
	    c = GET(s);
	  else if ( c == '-' )
	  { negative++;
	    c = GET(s);
	  }
	do_unsigned:
	  ok = FALSE;
	  if ( base == 16 )		/* hexadecimal */
	  { if ( isxdigit(c) )
	    { v = valxdigit(c);
	      for(c = GET(s); isxdigit(c); c = GET(s))
		v = base*v + valxdigit(c);
	      ok++;
	    }
	  } else
	  { int cv;

	    if ( isdigit(c) && (cv=valdigit(c)) < base )
	    { v = cv;
	      for(c = GET(s); isdigit(c) && (cv=valdigit(c)) < base; c = GET(s))
		v = base*v + cv;
	      ok++;
	    }
	  }

	  if ( ok )
	  { if ( !supress )
	    { if ( negative )
		v = -v;
	      if ( tsize == SZ_SHORT )
	      { short *vp = va_arg(args, short *);
		*vp = v;
	      } else if ( tsize == SZ_LONG )
	      { intptr_t *vp = va_arg(args, intptr_t *);
		*vp = v;
	      } else
	      { int *vp = va_arg(args, int *);
		*vp = v;
	      }
	      done++;
	    }
	    continue;			/* with next */
	  } else
	    return done;
	case 'u':
	  base = 10;
	  negative = FALSE;
	  goto do_unsigned;
	case 'o':
	  base = 8;
	  goto do_int;
	case 'x':
	  base = 16;
	  goto do_int;
	case 'i':
	  if ( c == '0' )
	  { int c2 = GET(s);

	    if ( c2 == 'x' )
	    { base = 16;
	      c = GET(s);
	    } else
	    { UNGET(c2, s);
	      base = 8;
	    }
	    negative = FALSE;
	    goto do_unsigned;
	  }
	  base = 10;
	  goto do_int;
	}
	case 'n':
	  if ( !supress )
	  { if ( tsize == SZ_SHORT )
	    { short *vp = va_arg(args, short *);
	      *vp = chread;
	    } else if ( tsize == SZ_LONG )
	    { intptr_t *vp = va_arg(args, intptr_t *);
	      *vp = chread;
	    } else
	    { int *vp = va_arg(args, int *);
	      *vp = chread;
	    }
	    done++;
	  }
	  fm++;
	  continue;
	case 'E':
	case 'e':
	case 'f':
	case 'G':
	case 'g':
	{ char work[200];
	  char *w = work;
	  int ds = 0;
	  double v;

	  if ( c == '-' || c == '+' )	/* [+|-] */
	  { *w++ = c;
	    c = GET(s);
	  }
	  while(isdigit(c))		/* {digit} */
	  { *w++ = c;
	    c = GET(s);
	    ds++;
	  }
	  if ( c == '.' )		/* [.] */
	    *w++ = c;
	  while(isdigit(c))		/* {digit} */
	  { *w++ = c;
	    c = GET(s);
	    ds++;
	  }
	  if ( !ds )
	    SCAN_ERROR(s)
	  if ( c == 'e' || c == 'E' )	/* [e<digit>{<digit>}] */
	  { *w++ = c;
	    c = GET(s);
	    if ( !isdigit(c) )
	      SCAN_ERROR(s)
	    while(isdigit(c))
	    { *w++ = c;
	    c = GET(s);
	    }
	  }

	  if ( !supress )
	  { *w = '\0';
	    v = strtod(work, &w)
	    if ( w == work )
	      SCAN_ERROR(s);

	    switch(tsize)
	    { case SZ_NORMAL:
	      { float *fp = va_arg(args, float *);
		*fp = v;
		break;
	      }	
	      case SZ_LONG:
	      { double *fp = va_arg(args, double *);
		*fp = v;
		break;
	      }
	    }  
	    done++;
	  }

	  fm++;
	  continue;
	}
	case 's':
	  if ( !supress )
	  { char *sp = va_arg(args, char *);
	    
	    while(!isblank(c) && field_width-- != 0)
	    { *sp++ = c;
	      c = GET(s);
	    }
	  } else
	    while(!isblank(c) && field_width-- != 0)
	      c = GET(s);
	  fm++;
	  continue;
	case 'c':
	  if ( !supress )
	  { char *cp = va_arg(args, char *);
	    *cp = c;
	  }
	  c = GET(s);
	  fm++;
	  continue;
	case '[':
	{ char set[256];
	  
	  memset(set, 0, sizeof(set));
	  fm++;
	  if ( *fm == ']' )
	    set[*fm++]++;
	  else if ( *fm == '^' )
	  { fm++;
	    negate++;
	  }
	  while(*fm != ']')
	  { if ( *fm == '-' )
	      
	  }
	}
      }
    } else				/* normal character */
    { if ( c == *fm )
      { c = GET(s);
	fm++;
	continue;
      }

      break;
    }
  }

out:
  UNGET(c, s);

  return done;
}

#endif /*0*/


		 /*******************************
		 *	   FILTER STREAMS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Link two streams in a pipeline,  where   filter  filters data for stream
`parent'. If parent is an output steam we have

	application --> filter --> parent -->
	  
If parent is an input stream we have

	--> parent --> filter --> application
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Sset_filter(IOSTREAM *parent, IOSTREAM *filter)
{ if ( !parent || parent->magic != SIO_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  if ( filter )
  { if ( filter->magic != SIO_MAGIC )
    { errno = EINVAL;
      return -1;
    }
  }

  parent->upstream = filter;
  if ( filter )
    filter->downstream = parent;

  return 0;
}


		 /*******************************
		 *	    FILE STREAMS	*
		 *******************************/

static ssize_t
Sread_file(void *handle, char *buf, size_t size)
{ intptr_t h = (intptr_t) handle;
  ssize_t bytes;

  for(;;)
  {
#ifdef __WINDOWS__
    bytes = read((int)h, buf, (int)size);
#else
    bytes = read((int)h, buf, size);
#endif

    if ( bytes == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	return -1;
      }

      continue;
    }

    return bytes;
  }
}


static ssize_t
Swrite_file(void *handle, char *buf, size_t size)
{ intptr_t h = (intptr_t) handle;
  ssize_t bytes;

  for(;;)
  {
#ifdef __WINDOWS__
    bytes = write((int)h, buf, (int)size);
#else
    bytes = write((int)h, buf, size);
#endif

    if ( bytes == -1 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
      { errno = EPLEXCEPTION;
	return -1;
      }

      continue;
    }

    return bytes;
  }
}


static long
Sseek_file(void *handle, long pos, int whence)
{ intptr_t h = (intptr_t) handle;

					/* cannot do EINTR according to man */
  return lseek((int)h, pos, whence);
}


#ifdef O_LARGEFILES
static int64_t
Sseek_file64(void *handle, int64_t pos, int whence)
{ intptr_t h = (intptr_t) handle;

					/* cannot do EINTR according to man */
  return lseek((int)h, pos, whence);
}
#endif


static int
Sclose_file(void *handle)
{ intptr_t h = (intptr_t) handle;
  int rc;

  do
  { rc = close((int) h);
  }  while ( rc == -1 && errno == EINTR );

  return rc;
}


static int
Scontrol_file(void *handle, int action, void *arg)
{ intptr_t h = (intptr_t) handle;
  int fd = (int)h;

  switch(action)
  { case SIO_GETSIZE:
    { intptr_t *rval = arg;
      struct stat buf;

      if ( fstat(fd, &buf) == 0 )
      {	*rval = buf.st_size;
        return 0;
      }
      return -1;
    }
    case SIO_SETENCODING:
    case SIO_FLUSHOUTPUT:
      return 0;
    default:
      return -1;
  }
}


IOFUNCTIONS Sfilefunctions =
{ Sread_file,
  Swrite_file,
  Sseek_file,
  Sclose_file,
  Scontrol_file,
#ifdef O_LARGEFILES
  Sseek_file64
#else
  NULL
#endif
};


IOFUNCTIONS Sttyfunctions =
{ Sread_file,
  Swrite_file,
  NULL,
  Sclose_file,
  Scontrol_file,
#ifdef O_LARGEFILES
  Sseek_file64
#else
  NULL
#endif
};


IOSTREAM *
Snew(void *handle, int flags, IOFUNCTIONS *functions)
{ IOSTREAM *s;
  int fd;

  if ( !(s = malloc(sizeof(IOSTREAM))) )
  { errno = ENOMEM;
    return NULL;
  }
  memset((char *)s, 0, sizeof(IOSTREAM));
  s->magic         = SIO_MAGIC;
  s->lastc         = EOF;
  s->flags         = flags;
  s->handle        = handle;
  s->functions     = functions;
  s->timeout       = -1;		/* infinite */
  s->posbuf.lineno = 1;
  s->encoding      = ENC_ISO_LATIN_1;
  if ( flags & SIO_RECORDPOS )
    s->position = &s->posbuf;
#ifdef O_PLMT
  if ( !(flags & SIO_NOMUTEX) )
  { if ( !(s->mutex = malloc(sizeof(recursiveMutex))) )
    { free(s);
      return NULL;
    }
    recursiveMutexInit(s->mutex);
  }
#endif
  if ( (fd = Sfileno(s)) >= 0 && isatty(fd) )
    s->flags |= SIO_ISATTY;

  return s;
}


#ifndef O_BINARY
#define O_BINARY 0
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open a file. In addition to the normal  arguments, "lr" means get a read
(shared-) lock on the file and  "lw"   means  get  an write (exclusive-)
lock.  How much do we need to test here?

Note that the low-level open  is  always   binary  as  O_TEXT open files
result in lost and corrupted data in   some  encodings (UTF-16 is one of
them).  Sgetcode()  and  Sputcode()  do  the  LF  <->  CRLF  mapping  of
CRLF_MAPPING is defined.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

IOSTREAM *
Sopen_file(const char *path, const char *how)
{ int fd;
  int oflags = O_BINARY;
  int flags = SIO_FILE|SIO_TEXT|SIO_RECORDPOS;
  int op = *how++;
  intptr_t lfd;
  enum {lnone=0,lread,lwrite} lock = lnone;
  IOSTREAM *s;
  IOENC enc = ENC_UNKNOWN;

  for( ; *how; how++)
  { switch(*how)
    { case 'b':				/* binary */
	flags &= ~SIO_TEXT;
	enc = ENC_OCTET;
        break;
      case 'r':				/* no record */
	flags &= ~SIO_RECORDPOS;
        break;
      case 'l':				/* lock r: read, w: write */
	if ( *++how == 'r' )
	  lock = lread;
        else if ( *how == 'w' )
	  lock = lwrite;
        else
	{ errno = EINVAL;
	  return NULL;
	}
        break;
      default:
	errno = EINVAL;
        return NULL;
    }
  }

#if O_LARGEFILES && defined(O_LARGEFILE)
  oflags |= O_LARGEFILE;
#endif

  switch(op)
  { case 'w':
      fd = open(path, O_WRONLY|O_CREAT|O_TRUNC|oflags, 0666);
      flags |= SIO_OUTPUT;
      break;
    case 'a':
      fd = open(path, O_WRONLY|O_CREAT|O_APPEND|oflags, 0666);
      flags |= SIO_OUTPUT|SIO_APPEND;
      break;
    case 'u':
      fd = open(path, O_WRONLY|O_CREAT|oflags, 0666);
      flags |= SIO_OUTPUT|SIO_UPDATE;
      break;
    case 'r':
      fd = open(path, O_RDONLY|oflags);
      flags |= SIO_INPUT;
      break;
    default:
      errno = EINVAL;
      return NULL;
  }



  if ( fd < 0 )
    return NULL;

  if ( lock )
  { 
#ifdef FCNTL_LOCKS
    struct flock buf;

    memset(&buf, 0, sizeof(buf));
    buf.l_type = (lock == lread ? F_RDLCK : F_WRLCK);

    if ( fcntl(fd, F_SETLKW, &buf) < 0 )
    { int save = errno;
      close(fd);
      errno = save;
      return NULL;
    }
#else					/* we don't have locking */
#if __WINDOWS__
    HANDLE h = (HANDLE)_get_osfhandle(fd);
    OVERLAPPED ov;

    memset(&ov, 0, sizeof(ov));
    if ( !LockFileEx(h, (lock == lread ? 0 : LOCKFILE_EXCLUSIVE_LOCK),
		     0,
		     0, 0xfffffff,
		     &ov) )
    { close(fd);
      errno = EACCES;			/* TBD: proper error */
      return NULL;
    }
#else
    close(fd);
    errno = EINVAL;
    return NULL;
#endif
#endif
  }

  lfd = (intptr_t)fd;
  s = Snew((void *)lfd, flags, &Sfilefunctions);
  if ( enc != ENC_UNKNOWN )
    s->encoding = enc;
  if ( lock )
    s->flags |= SIO_ADVLOCK;

  return s;
}


IOSTREAM *
Sfdopen(int fd, const char *type)
{ int flags;
  intptr_t lfd;

  if ( fd < 0 )
  { errno = EINVAL;
    return NULL;
  }
#if defined(HAVE_FCNTL) && defined(F_GETFL)
  if ( fcntl(fd, F_GETFL) == -1 )
    return NULL;
#endif

  if ( *type == 'r' )
    flags = SIO_FILE|SIO_INPUT|SIO_RECORDPOS;
  else
    flags = SIO_FILE|SIO_OUTPUT|SIO_RECORDPOS;

  lfd = (intptr_t)fd;

  return Snew((void *)lfd, flags, &Sfilefunctions);
}

/* MT: as long as s is valid, this should be ok
*/

int
Sfileno(IOSTREAM *s)
{ int n;

  if ( s->flags & SIO_FILE )
  { intptr_t h = (intptr_t)s->handle;
    n = (int)h;
  } else if ( s->flags & SIO_PIPE )
  { n = fileno((FILE *)s->handle);
  } else if ( s->functions->control &&
	      (*s->functions->control)(s->handle,
				       SIO_GETFILENO,
				       (void *)&n)  == 0 )
  { ;
  } else
  { errno = EINVAL;
    n = -1;				/* no file stream */
  }

  return n;
}


		 /*******************************
		 *	       PIPES		*
		 *******************************/

#ifdef HAVE_POPEN
#ifdef __WINDOWS__
#include "popen.c"

#define popen(cmd, how) pt_popen(cmd, how)
#define pclose(fd)	pt_pclose(fd)
#endif

static ssize_t
Sread_pipe(void *handle, char *buf, size_t size)
{ FILE *fp = handle;

#ifdef __WINDOWS__
  return read(fileno(fp), buf, (unsigned int)size);
#else
  return read(fileno(fp), buf, size);
#endif
}


static ssize_t
Swrite_pipe(void *handle, char *buf, size_t size)
{ FILE *fp = handle;

#ifdef __WINDOWS__
  return write(fileno(fp), buf, (unsigned int)size);
#else
  return write(fileno(fp), buf, size);
#endif
}


static int
Sclose_pipe(void *handle)
{ FILE *fp = handle;

  pclose(fp);
  return 0;
}


IOFUNCTIONS Spipefunctions =
{ Sread_pipe,
  Swrite_pipe,
  (Sseek_function)0,
  Sclose_pipe
};


IOSTREAM *
Sopen_pipe(const char *command, const char *type)
{ char mode[2];
  FILE *fd;

#if 0
  Sdprintf("Opening \"%s\", mode \"%s\" --> %p (%d)\n",
	   command, type, fd, errno);
#endif

  mode[0] = type[0];
  mode[1] = '\0';

  if ( (fd = popen(command, mode)) )
  { int flags;

    if ( *type == 'r' )
      flags = SIO_PIPE|SIO_INPUT;
    else
      flags = SIO_PIPE|SIO_OUTPUT;

    return Snew((void *)fd, flags, &Spipefunctions);
  }

  return NULL;
}

#endif /*HAVE_POPEN*/

		 /*******************************
		 *	  MEMORY STREAMS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Memory streams form a replacement for   sprintf(), sscanf() and friends.
They allow regarding a piece of  (for output) malloc() maintained memory
to serve as a temporary buffer.

MT: we assume these handles are not passed between threads
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ size_t	here;			/* `here' location */
  size_t	size;			/* size of buffer */
  size_t       *sizep;			/* pointer to size */
  size_t	allocated;		/* allocated size */
  char	      **buffer;			/* allocated buffer */
  int		malloced;		/* malloc() maintained */
} memfile;


void
Sfree(void *ptr)			/* Windows: must free from same */
{ free(ptr);				/* DLL */
}


static size_t
S__memfile_nextsize(size_t needed)
{ size_t size = 512;

  while ( size < needed )
    size *= 2;

  return size;
}


static ssize_t
Swrite_memfile(void *handle, char *buf, size_t size)
{ memfile *mf = handle;

  if ( mf->here + size + 1 >= mf->allocated )
  { intptr_t ns = S__memfile_nextsize(mf->here + size + 1);
    char *nb;

    if ( mf->allocated == 0 || !mf->malloced )
    { if ( !(nb = malloc(ns)) )
      { errno = ENOMEM;
	return -1;
      }
      if ( !mf->malloced )
      { if ( *mf->buffer )
	  memcpy(nb, *mf->buffer, mf->allocated);
	mf->malloced = TRUE;
      }
    } else
    { if ( !(nb = realloc(*mf->buffer, ns)) )
      { errno = ENOMEM;
	return -1;
      }
    }

    mf->allocated = ns;
    *mf->buffer = nb;
  }

  memcpy(&(*mf->buffer)[mf->here], buf, size);
  mf->here += size;

  if ( mf->here > mf->size )
  { mf->size = mf->here;
    if ( mf->sizep )			/* make externally known */
      *mf->sizep = mf->size;
    (*mf->buffer)[mf->size] = '\0';
  }

  return size;
}


static ssize_t
Sread_memfile(void *handle, char *buf, size_t size)
{ memfile *mf = handle;

  if ( size + mf->here > mf->size )
  { if ( mf->here > mf->size )
      size = 0;
    else
      size = mf->size - mf->here;
  }
  
  memcpy(buf, &(*mf->buffer)[mf->here], size);
  mf->here += size;

  return size;
}


static long
Sseek_memfile(void *handle, long offset, int whence)
{ memfile *mf = handle;

  switch(whence)
  { case SIO_SEEK_SET:
      break;
    case SIO_SEEK_CUR:
      offset += (long)mf->here;		/* Win64: truncates */
      break;
    case SIO_SEEK_END:
      offset = (long)mf->size - offset;	/* Win64 */
      break;
    default:
      errno = EINVAL;
      return -1;
  }
  if ( offset < 0 || offset > (long)mf->size )
  { errno = EINVAL;
    return -1;
  }
  mf->here = offset;

  return offset;
}


static int
Sclose_memfile(void *handle)
{ memfile *mf = handle;

  if ( mf )
  { free(mf);
    return 0;
  }
	 
  errno = EINVAL;			/* not opened */
  return -1;
}


IOFUNCTIONS Smemfunctions =
{ Sread_memfile,
  Swrite_memfile,
  Sseek_memfile,
  Sclose_memfile
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sopenmem(char **buffer, size_t *sizep, const char* mode)
    Open a memory area as a stream.  Output streams will automatically
    resized using realloc() if *size = 0 or the stream is opened with mode
    "wa".

    If the buffer is allocated or enlarged, this is achieved using malloc()
    or realloc().  In this case the returned buffer should be freed by the
    caller when done.  Example:

    { char buf[1024];			(don't allocate for small stuff)
      char *s = buf;
      IOSTREAM *fd;
      size_t size = sizeof(buf);

      fd = Sopenmem(&s, &size, "w");
      ...
      Sclose(fd);
      ...
      if ( s != buf )			(appearently moved)
	Sfree(s);
    }

Note: Its is NOT allows to access   streams  created with this call from
multiple threads. This is ok for all   usage inside Prolog itself (often
through tellString()/toldString(). This call is   intented  to use write
and other output predicates to create strings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

IOSTREAM *
Sopenmem(char **buffer, size_t *sizep, const char *mode)
{ memfile *mf = malloc(sizeof(memfile));
  int flags = SIO_FBUF|SIO_RECORDPOS|SIO_NOMUTEX;
  size_t size;

  if ( !mf )
  { errno = ENOMEM;
    return NULL;
  }

  mf->malloced = FALSE;

  switch(*mode)
  { case 'r':
      flags |= SIO_INPUT;
      if ( sizep == NULL || *sizep == (size_t)-1 )
	size = (*buffer ? strlen(*buffer) : 0);
      else
	size = *sizep;
      mf->size = size;
      mf->allocated = size+1;
      break;
    case 'w':
      flags |= SIO_OUTPUT;
      mf->size = 0;
      mf->allocated = (sizep ? *sizep : 0);
      if ( *buffer == NULL || mode[1] == 'a' )
	mf->malloced = TRUE;
      if ( *buffer )
	*buffer[0] = '\0';
      if ( sizep )
	*sizep = mf->size;
      break;
    default:
      free(mf);
      errno = EINVAL;
      return NULL;
  }

  mf->sizep	= sizep;
  mf->here      = 0;
  mf->buffer    = buffer;

  return Snew(mf, flags, &Smemfunctions);
}

		 /*******************************
		 *	      STRINGS		*
		 *******************************/

/* MT: we assume these handles are not passed between threads
*/

static ssize_t
Sread_string(void *handle, char *buf, size_t size)
{ return 0;				/* signal EOF */
}

static ssize_t
Swrite_string(void *handle, char *buf, size_t size)
{ errno = ENOSPC;			/* signal error */
  return -1;
}

static int
Sclose_string(void *handle)
{ IOSTREAM *s = handle;

  if ( s->flags & SIO_OUTPUT )
  { if ( s->bufp < s->limitp )
    { *s->bufp++ = '\0';
      return 0;
    } else
    { errno = ENOSPC;			/* signal error */
      return -1;
    }
  } else
    return 0;				/* input string */
}

IOFUNCTIONS Sstringfunctions =
{ Sread_string,
  Swrite_string,
  (Sseek_function)0,
  Sclose_string
};


IOSTREAM *
Sopen_string(IOSTREAM *s, char *buf, size_t size, const char *mode)
{ int flags = SIO_FBUF|SIO_USERBUF;

  if ( !s )
  { if ( !(s = malloc(sizeof(IOSTREAM))) )
    { errno = ENOMEM;
      return NULL;
    }
  } else
    flags |= SIO_STATIC;

  memset((char *)s, 0, sizeof(IOSTREAM));
  s->timeout   = -1;
  s->buffer    = buf;
  s->bufp      = buf;
  s->unbuffer  = buf;
  s->handle    = s;			/* for Sclose_string() */
  s->functions = &Sstringfunctions;
  s->encoding  = ENC_ISO_LATIN_1;

  switch(*mode)
  { case 'r':
      if ( size == (size_t)-1 )
	size = strlen(buf);
      flags |= SIO_INPUT;
      break;
    case 'w':
      flags |= SIO_OUTPUT;
      break;
    default:
      errno = EINVAL;
      return NULL;
  }

  s->flags  = flags;
  s->limitp = &buf[size];
  s->magic  = SIO_MAGIC;

  return s;
}

		 /*******************************
		 *	 STANDARD HANDLES	*
		 *******************************/

#define STDIO(n, f) { NULL, NULL, NULL, NULL, \
		      EOF, SIO_MAGIC, 0, f, {0, 0, 0}, NULL, \
		      ((void *)(n)), &Sttyfunctions, \
		      0, NULL, \
		      (void (*)(void *))0, NULL, \
		      -1, \
		      0, \
		      ENC_ISO_LATIN_1 \
		    }

#define SIO_STDIO (SIO_FILE|SIO_STATIC|SIO_NOCLOSE|SIO_ISATTY|SIO_TEXT)
#define STDIO_STREAMS \
  STDIO(0, SIO_STDIO|SIO_LBUF|SIO_INPUT|SIO_NOFEOF),	/* Sinput */ \
  STDIO(1, SIO_STDIO|SIO_LBUF|SIO_OUTPUT|SIO_REPPL), 	/* Soutput */ \
  STDIO(2, SIO_STDIO|SIO_NBUF|SIO_OUTPUT|SIO_REPPL)	/* Serror */


IOSTREAM S__iob[] =
{ STDIO_STREAMS
};


static const IOSTREAM S__iob0[] =
{ STDIO_STREAMS
};


void
SinitStreams()
{ static int done;

  if ( !done++ )
  { int i;
    IOENC enc = initEncoding();

    for(i=0; i<=2; i++)
    { if ( !isatty(i) )
      { S__iob[i].flags &= ~SIO_ISATTY;
	S__iob[i].functions = &Sfilefunctions; /* Check for pipe? */
      }
      if ( S__iob[i].encoding == ENC_ISO_LATIN_1 )
	S__iob[i].encoding = enc;
#ifdef O_PLMT
      S__iob[i].mutex = malloc(sizeof(recursiveMutex));
      recursiveMutexInit(S__iob[i].mutex);
#endif
#ifdef __WINDOWS__
      pt_init();
#endif
    }
  }
}


IOSTREAM *
S__getiob()
{ return S__iob;
}


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This allows external packages (Prolog itself) to monitor the destruction
of streams.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct _close_hook
{ struct _close_hook *next;
  void (*hook)(IOSTREAM *s);
} close_hook;

static close_hook *close_hooks;

static void
run_close_hooks(IOSTREAM *s)
{ close_hook *p;

  for(p=close_hooks; p; p = p->next)
    (*p->hook)(s);

  if ( s->close_hook )
    (*s->close_hook)(s->closure);
}

int
Sclosehook(void (*hook)(IOSTREAM *s))
{ close_hook *h = malloc(sizeof(*h));

  if ( !h )
    return -1;
  h->next = close_hooks;
  h->hook = hook;
  close_hooks = h;

  return 0;
}


		 /*******************************
		 *	       CLEANUP		*
		 *******************************/

void
Sreset(void)
{ IOSTREAM *s = Sinput;

  if ( s && s->magic == SIO_MAGIC )
  { s->bufp = s->limitp = s->buffer;
  }
} 


void
Scleanup(void)
{ close_hook *p, *next;
  int i;

  for(p=close_hooks; p; p=next)
  { next = p->next;
    free(p);
  }

  close_hooks = NULL;

  for(i=0; i<=2; i++)
  { IOSTREAM *s = &S__iob[i];

    s->bufp = s->buffer;		/* avoid actual flush */
    S__removebuf(s);
    *s = S__iob0[i];			/* re-initialise */
  }
}
