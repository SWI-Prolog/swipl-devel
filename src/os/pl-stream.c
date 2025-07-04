/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2025, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
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

#ifdef __WINDOWS__
#define SWIPL_WINDOWS_NATIVE_ACCESS 1
#include "windows/uxnt.h"
#include "config/wincfg.h"
#include <winsock2.h>
#include "../pl-nt.h"
#include "../pl-ntconsole.h"
#define CRLF_MAPPING 1
#else
#include <config.h>
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

#if _FILE_OFFSET_BITS == 64 || defined(_LARGE_FILES)
#define O_LARGEFILES 1		/* use for conditional code in Prolog */
#else
#undef O_LARGEFILES
#endif

#ifdef HAVE_VISIBILITY_ATTRIBUTE
#define PL_SO_EXPORT __attribute__((visibility("default")))
#endif

#define PL_KERNEL 1
#define O_LOCALE 1
#include <wchar.h>
#define NEEDS_SWINSOCK
#define PL_STREAM_IMPL
#include "pl-stream.h"
#include "pl-utf8.h"
#include "../pl-mutex.h"
#include "SWI-Prolog.h"
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
#if defined(HAVE_POLL_H)
#include <poll.h>
#elif defined(HAVE_SYS_SELECT_H)
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

#define ROUND(p, n) ((((p) + (n) - 1) & ~((n) - 1)))
#define UNDO_SIZE ROUND(PL_MB_LEN_MAX, sizeof(wchar_t))

#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif

#define char_to_int(c)	(0xff & (int)(c))

#define TMPBUFSIZE 256			/* Serror bufsize for Svfprintf() */

int Slinesize = SIO_LINESIZE;		/* Sgets() buffer size */

static ssize_t	S__flushbuf(IOSTREAM *s);
static void	run_close_hooks(IOSTREAM *s);
static int	S__removebuf(IOSTREAM *s);
static int	S__seterror(IOSTREAM *s);
       void	unallocStream(IOSTREAM *s);
static int	S__fileno(IOSTREAM *s);

static IOSTREAM *	Sopen_buffer(IOSTREAM *s, char *buf, size_t size);
static void		Sclose_buffer(IOSTREAM *s);

#ifdef O_PLMT
#define SLOCK(s)    if ( s->mutex ) recursiveMutexLock(s->mutex)
#define SUNLOCK(s)  if ( s->mutex ) recursiveMutexUnlock(s->mutex)
static inline int
STRYLOCK(IOSTREAM *s)
{ if ( s->mutex &&
       recursiveMutexTryLock(s->mutex) == EBUSY )
    return false;

  return true;
}
#else
#define SLOCK(s)
#define SUNLOCK(s)
#define STRYLOCK(s) (true)
#endif

#include "pl-error.h"
#ifdef O_LOCALE
#include "os/pl-locale.h"
#endif

extern int			PL_handle_signals();
extern IOENC			initEncoding(void);
extern bool			reportStreamError(IOSTREAM *s);
extern record_t			PL_record(term_t t);
extern int			PL_thread_self(void);


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

static size_t
S__setbuf(IOSTREAM *s, char *buffer, size_t size)
{ char *newbuf, *newunbuf;
  int newflags = s->flags;

  if ( size == 0 )
    size = SIO_BUFSIZE;

  if ( (s->flags & SIO_OUTPUT) )
  { if ( S__removebuf(s) < 0 )
      return -1;
  }

  if ( buffer )
  { newunbuf = newbuf = buffer;
    newflags |= SIO_USERBUF;
  } else
  { if ( !(newunbuf = malloc(size+UNDO_SIZE)) )
    { errno = ENOMEM;
      S__seterror(s);
      return -1;
    }
    newflags &= ~SIO_USERBUF;
    newbuf = newunbuf + UNDO_SIZE;
  }

  if ( (s->flags & SIO_INPUT) )
  { size_t buffered = s->limitp - s->bufp;
    size_t copy = (buffered < size ? buffered : size);

    if ( size < buffered )
    { size_t offset = size - buffered;
      int64_t newpos;

      if ( s->functions->seek64 )
      { newpos = (*s->functions->seek64)(s->handle, offset, SIO_SEEK_CUR);
      } else if ( s->functions->seek )
      { newpos = (*s->functions->seek)(s->handle, (long)offset, SIO_SEEK_CUR);
      } else
      { newpos = -1;
	errno = ESPIPE;
      }

      if ( newpos == -1 )
      { if ( !(newflags & SIO_USERBUF) )
	{ int oldeno = errno;

	  free(newunbuf);
	  errno = oldeno;
	  S__seterror(s);
	  return -1;
	}
      }
    }

    if ( copy > 0 )
      memcpy(newbuf, s->bufp, copy);
    S__removebuf(s);
    s->unbuffer = newunbuf;
    s->bufp = s->buffer = newbuf;
    s->limitp = s->buffer+copy;
  } else
  { s->unbuffer = newunbuf;
    s->bufp = s->buffer = newbuf;
    s->limitp = &s->buffer[size];
  }
  s->bufsize = (int)size;
  s->flags = newflags;

  return size;
}


void
Ssetbuffer(IOSTREAM *s, char *buffer, size_t size)
{ if ( S__setbuf(s, buffer, size) != (size_t)-1 )
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
{ void *array[7];
  size_t size;
  char **strings;
  size_t i;

  size = backtrace(array, sizeof(array)/sizeof(void *));
  strings = backtrace_symbols(array, size);

  printf(" Stack:");
  for(i = 1; i < size; i++)
  { printf("\n\t[%ld] %s", (long)i, strings[i]);
  }
  printf("\n");

  free(strings);
}
#endif /*DEBUG_IO_LOCKS*/


static int
S__lock(IOSTREAM *s)
{ if ( s->erased )
  { SUNLOCK(s);
    return -1;
  }

  if ( s->locks == 0 )
  { if ( (s->flags & (SIO_NBUF|SIO_OUTPUT)) == (SIO_NBUF|SIO_OUTPUT) &&
	 S__setbuf(s, NULL, TMPBUFSIZE) == (size_t)-1 )
    { SUNLOCK(s);
      return -1;
    }
  }

  s->locks++;
  Sreference(s);

  return 0;
}

int
Slock(IOSTREAM *s)
{ SLOCK(s);

  return S__lock(s);
}


int
StryLock(IOSTREAM *s)
{ if ( !STRYLOCK(s) )
    return -1;

  return S__lock(s);
}


int
Sunlock(IOSTREAM *s)
{ int rval = 0;
  int unalloc;

#ifdef DEBUG_IO_LOCKS
  if ( s->locks > 3 )
  { printf("Unlock [%d]: %s: %d locks", PL_thread_self(), Sname(s), s->locks-1);
    print_trace();
  }
#endif

  if ( s->locks )
  { if ( --s->locks == 0 )
    { if ( (s->flags & (SIO_NBUF|SIO_OUTPUT)) == (SIO_NBUF|SIO_OUTPUT) )
	rval = S__removebuf(s);
    }
  } else
  { assert(0);
  }

  unalloc = (Sunreference(s) == 0 && s->erased);

  SUNLOCK(s);
  if ( unalloc )
    unallocStream(s);

  return rval;
}


IOSTREAM *
Sacquire(IOSTREAM *s)
{ Sreference(s);
  return s;
}

int
Srelease(IOSTREAM *s)
{ if ( Sunreference(s) == 0 && s->erased )
  { unallocStream(s);
    return true;
  }

  return false;
}


		 /*******************************
		 *		TIMEOUT		*
		 *******************************/

#ifdef HAVE_SELECT

#ifndef __WINDOWS__
typedef int SOCKET;
#define INVALID_SOCKET -1
#define Swinsock(s) Sfileno(s)
#define NFDS(n) (n+1)
#else
#define NFDS(n) (0)			/* 1st arg of select is ignored */
#ifdef HAVE_WSAPOLL
#define HAVE_POLL 1
static inline int
poll(struct pollfd *pfd, int nfds, int timeout)
{ return WSAPoll(pfd, nfds, timeout);
}
#endif
#endif


static int
S__wait(IOSTREAM *s)
{ SOCKET fd = Swinsock(s);
  int rc;
#ifdef HAVE_POLL
  struct pollfd fds[1];
#else
  fd_set wait;
  struct timeval time;

  time.tv_sec  = s->timeout / 1000;
  time.tv_usec = (s->timeout % 1000) * 1000;
  FD_ZERO(&wait);
  FD_SET(fd, &wait);
#endif

  if ( fd == INVALID_SOCKET )
  { errno = EPERM;			/* no permission to select */
    Sseterr(s, SIO_FERR, "not a socket");
    return -1;
  }

  for(;;)
  {
#ifdef HAVE_POLL
    fds[0].fd = fd;
    fds[0].events = (s->flags & SIO_INPUT) ? POLLIN : POLLOUT;

    rc = poll(fds, 1, s->timeout);
#else
    if ( (s->flags & SIO_INPUT) )
      rc = select(NFDS(fd), &wait, NULL, NULL, &time);
    else
      rc = select(NFDS(fd), NULL, &wait, NULL, &time);
#endif

    if ( rc < 0 && errno == EINTR )
    { if ( PL_handle_signals() < 0 )
      { Sset_exception(s, PL_exception(0));
	errno = EPLEXCEPTION;
	return -1;
      }

      continue;
    }

    break;
  }

  if ( rc == 0 )
  { Sseterr(s, SIO_TIMEOUT|SIO_FERR, NULL);
    return -1;
  }

  return 0;				/* ok, data available */
}

#endif /*HAVE_SELECT*/

int
Sset_timeout(IOSTREAM *s, int tmo)
{ IOSTREAM *us;

  for ( us=s; us; us=us->upstream )
  { if ( us->magic != SIO_MAGIC )
    { errno = EINVAL;
      return -1;
    }
    us->timeout = tmo;
  }
  for ( us=s; us; us=us->downstream )
  { if ( us->magic != SIO_MAGIC )
    { errno = EINVAL;
      return -1;
    }
    us->timeout = tmo;
  }

  return 0;
}


		 /*******************************
		 *	     FLUSH/FILL		*
		 *******************************/

/* return values: -1: error, else #bytes written */

static ssize_t
S__flushbuf(IOSTREAM *s)
{ char *from, *to;
  ssize_t rc;

  if ( s->magic != SIO_MAGIC )
  { errno = EINVAL;
    return -1;
  }
  SLOCK(s);
  from = s->buffer;
  to   = s->bufp;

  while ( from < to )
  { size_t size = (size_t)(to - from);
    ssize_t n;

#ifdef HAVE_SELECT
    s->flags &= ~SIO_TIMEOUT;

    if ( s->timeout >= 0 )
    { if ( (rc=S__wait(s)) < 0 )
	goto partial;
    }
#endif

  retry:
    n = (*s->functions->write)(s->handle, from, size);

    if ( n > 0 )			/* wrote some */
    { from += n;
    } else if ( n < 0 )			/* error */
    { if ( errno == EINTR )
      { if ( PL_handle_signals() < 0 )
	{ Sset_exception(s, PL_exception(0));
	  errno = EPLEXCEPTION;
	} else
	  goto retry;
      } else
	S__seterror(s);
      rc = -1;
      goto out;
    } else				/* wrote nothing? */
    { break;
    }
  }

#ifdef HAVE_SELECT
partial:
#endif
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
      *s->bufp++ = (char)c;
  } else
  { if ( s->flags & SIO_NBUF )
    { char chr = (char)c;

      if ( (*s->functions->write)(s->handle, &chr, 1) != 1 )
      { S__seterror(s);
	c = -1;
      }
    } else
    { if ( S__setbuf(s, NULL, 0) == (size_t)-1 )
	c = -1;
      else
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

  if ( s->magic != SIO_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  if ( s->flags & (SIO_FEOF|SIO_FERR) )	/* reading past eof */
  { if ( s->flags & SIO_FEOF2ERR )
      Sseterr(s, (SIO_FEOF2|SIO_FERR), NULL);
    else
      s->flags |= SIO_FEOF2;
    return -1;
  }

#ifdef HAVE_SELECT
  s->flags &= ~SIO_TIMEOUT;

  if ( s->timeout >= 0 && !s->downstream )
  { int rc;

    if ( (rc=S__wait(s)) < 0 )
      return rc;
  }
#endif

  if ( s->flags & SIO_NBUF )
  { char chr;
    ssize_t n;

  again:
    n = (*s->functions->read)(s->handle, &chr, 1);
    if ( n == 1 )
    { c = char_to_int(chr);
      return c;
    } else if ( n == 0 )
    { if ( !(s->flags & SIO_NOFEOF) )
	s->flags |= SIO_FEOF;
      return -1;
    } else
    { if ( errno == EINTR )
      { if ( PL_handle_signals() < 0 )
	{ Sset_exception(s, PL_exception(0));
	  errno = EPLEXCEPTION;
	  return -1;
	}
	goto again;
      }
      S__seterror(s);
      return -1;
    }
  } else
  { ssize_t n;
    size_t len;

    if ( !s->buffer )
    { if ( S__setbuf(s, NULL, 0) == (size_t)-1 )
	return -1;
      s->bufp = s->limitp = s->buffer;
      len = s->bufsize;
    } else if ( s->bufp < s->limitp )
    { len = s->limitp - s->bufp;
      if ( len == s->bufsize )
      { c = char_to_int(*s->bufp++);
	return c;
      }
      memmove(s->buffer, s->bufp, s->limitp - s->bufp);
      s->bufp = s->buffer;
      s->limitp = &s->bufp[len];
      len = s->bufsize - len;
    } else
    { s->bufp = s->limitp = s->buffer;
      len = s->bufsize;
    }

  again2:
    n = (*s->functions->read)(s->handle, s->limitp, len);
    if ( n > 0 )
    { s->limitp += n;
      c = char_to_int(*s->bufp++);
      return c;
    } else
    { if ( n == 0 )
      { if ( !(s->flags & SIO_NOFEOF) )
	  s->flags |= SIO_FEOF;
	return -1;
      } else if ( Sferror(s) )
      { return -1;
#ifdef EWOULDBLOCK
      } else if ( errno == EWOULDBLOCK )
      { s->bufp = s->buffer;
	s->limitp = s->buffer;
	S__seterror(s);
	return -1;
#endif
      } else if ( errno == EINTR )
      { if ( PL_handle_signals() < 0 )
	{ Sset_exception(s, PL_exception(0));
	  errno = EPLEXCEPTION;
	  return -1;
	}
	goto again2;
      } else
      {	S__seterror(s);
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

  if ( likely(c > '\r') )	/* speedup the 99% case a bit */
  { p->linepos++;
    return;
  }

  switch(c)
  { case '\n':
      p->lineno++;
      p->linepos = 0;
      s->flags &= ~SIO_NOLINEPOS;
      break;
    case '\r':
      p->linepos = 0;
      s->flags &= ~SIO_NOLINEPOS;
      break;
    case '\b':
      if ( p->linepos > 0 )
	p->linepos--;
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

  if ( c != EOF )
  { update_linepos(s, c);
    p->byteno++;
    p->charno++;
  }

  return c;
}


static inline int
S__updatefilepos(IOSTREAM *s, int c)
{ IOPOS *p = s->position;

  if ( p && c != EOF )
  { update_linepos(s, c);
    p->charno++;
  }

  return c;
}


static inline int
get_byte(IOSTREAM *s)
{ int c = Snpgetc(s);

  if ( s->position && c != EOF )
    s->position->byteno++;

  return c;
}


static int
put_byte(int c, IOSTREAM *s)
{ c &= 0xff;

  if ( s->bufp < s->limitp )
  { *s->bufp++ = (char)c;
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

  *--s->bufp = (char)c;
  if ( p )
  { p->charno--;			/* FIXME: not correct */
    p->byteno--;
    if ( c == '\n' )
      p->lineno--;
    s->flags |= SIO_NOLINEPOS;
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
{ if ( c >= 0 && (s->flags & (SIO_REPXML|SIO_REPPL|SIO_REPPLU)) )
  { char buf[16];
    const char *q;

    if ( (s->flags & SIO_REPPL) )
    { sprintf(buf, "\\x%X\\", c);
    } else if ( (s->flags & SIO_REPPLU) )
    { if ( c <= 0xffff )
	sprintf(buf, "\\u%04X", c);
      else
	sprintf(buf, "\\U%08X", c);
    } else
    { sprintf(buf, "&#%d;", c);
    }

    for(q = buf; *q; q++)
    { if ( put_byte(*q, s) < 0 )
	return -1;
    }

    return c;
  }

  Sseterr(s, SIO_FERR, "Encoding cannot represent character");
  return -1;
}

static int
put_usc2(int c, IOSTREAM *s, int be)
{ if ( be )
  { if ( put_byte(c>>8, s) < 0 ||
	 put_byte(c&0xff, s) < 0 )
      return -1;
  } else
  { if ( put_byte(c&0xff, s) < 0 ||
	 put_byte(c>>8, s) < 0 )
      return -1;
  }

  return 0;
}

static int
put_utf16(int c, IOSTREAM *s, int be)
{ if ( c > 0xffff )
  { int c1, c2;

    utf16_encode(c, &c1, &c2);
    if ( put_usc2(c1, s, s->encoding == ENC_UTF16BE) < 0 ||
	 put_usc2(c2, s, s->encoding == ENC_UTF16BE) < 0 )
      return -1;
  } else if ( IS_UTF16_SURROGATE(c) )
  { if ( reperror(c, s) < 0 )
      return -1;
  } else
  { if ( put_usc2(c, s, s->encoding == ENC_UTF16BE) < 0 )
      return -1;
  }

  return 0;
}


static int
put_code(int c, IOSTREAM *s)
{ switch(s->encoding)
  { case ENC_OCTET:
    case ENC_ISO_LATIN_1:
      if ( c >= 256 )
      { if ( reperror(c, s) < 0 )
	  return -1;
	break;
      }
      if ( put_byte(c, s) < 0 )
	return -1;
      break;
    case ENC_ASCII:
      if ( c >= 128 )
      { if ( reperror(c, s) < 0 )
	  return -1;
	break;
      }
      if ( put_byte(c, s) < 0 )
	return -1;
      break;
    case ENC_ANSI:
    { char b[PL_MB_LEN_MAX];
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

      if ( likely(c < 128) )
      { if ( put_byte(c, s) < 0 )
	  return -1;
	break;
      }

      end = utf8_put_char(buf, c);
      for(p=buf; p<end; p++)
      { if ( put_byte(*p&0xff, s) < 0 )
	  return -1;
      }

      break;
    }
    case ENC_UTF16BE:
      if ( put_utf16(c, s, true) < 0 )
	return -1;
      break;
    case ENC_UTF16LE:
      if ( put_utf16(c, s, false) < 0 )
	return -1;
      break;
    case ENC_WCHAR:
#if SIZEOF_WCHAR_T == 2
#ifdef WORDS_BIGENDIAN
      if ( put_utf16(c, s, true) < 0 )
	return -1;
#else
      if ( put_utf16(c, s, false) < 0 )
	return -1;
#endif
      break;
#else
    { pl_wchar_t chr = c;
      unsigned char *q = (unsigned char *)&chr;
      unsigned char *e = &q[sizeof(pl_wchar_t)];

      while(q<e)
      { if ( put_byte(*q++, s) < 0 )
	  return -1;
      }

      break;
    }
#endif
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
Sputcode(int c, IOSTREAM *s)
{ if ( c < 0 )
    return reperror(c, s);

  if ( s->tee && s->tee->magic == SIO_MAGIC )
    Sputcode(c, s->tee);

  if ( c == '\n' &&
       (s->flags&SIO_TEXT) &&
       s->newline == SIO_NL_DOS &&
       s->lastc != '\r' )
  { if ( put_code('\r', s) < 0 )
      return -1;
  }

  return put_code(c, s);
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
      char b[PL_MB_LEN_MAX];

      memset(&state, 0, sizeof(state));
      if ( wcrtomb(b, (wchar_t)c, &state) != (size_t)-1 )
	return 0;
      return -1;
    }
    case ENC_UTF16BE:
    case ENC_UTF16LE:
#if SIZEOF_WCHAR_T > 2
      if ( IS_UTF16_SURROGATE(c) )
	return -1;
#endif
    /*FALLTHROUGH*/
    case ENC_WCHAR:
#if SIZEOF_WCHAR_T == 2
      if ( IS_UTF16_SURROGATE(c) )
	return -1;
#endif
    /*FALLTHROUGH*/
    case ENC_UTF8:
      return 0;
    default:
      assert(0);
      return -1;
  }
}


static int
get_ucs2(IOSTREAM *s, int be)
{ int c1, c2;

  c1 = get_byte(s);
  if ( c1 == EOF )
    return -1;
  c2 = get_byte(s);

  if ( c2 == EOF )
  { Sseterr(s, SIO_WARN, "EOF in unicode character");
    return UTF8_MALFORMED_REPLACEMENT;
  } else
  { if ( be )
      return (c1<<8)+c2;
    else
      return (c2<<8)+c1;
  }
}


static int
get_utf16(IOSTREAM *s, int be)
{ int c = get_ucs2(s, be);

  if ( IS_UTF16_LEAD(c) )
  { int c2 = get_ucs2(s, be);

    if ( c2 == EOF )
    { Sseterr(s, SIO_WARN, "EOF in unicode character");
      return UTF8_MALFORMED_REPLACEMENT;
    } else if ( !IS_UTF16_TRAIL(c2) )
    { Sseterr(s, SIO_WARN, "Illegal UTF-16 continuation");
      return UTF8_MALFORMED_REPLACEMENT;
    } else
    { return utf16_decode(c, c2);
    }
  } else
  { return c;
  }
}


int
Sgetcode(IOSTREAM *s)
{ int c;

retry:
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
	  { goto out;
	  } else
	  { Sseterr(s, SIO_WARN, "EOF in multibyte Sequence");
	    goto mberr;
	  }
	}
	b[0] = (char)c;
	rc=mbrtowc(&wc, b, 1, s->mbstate);
	if ( rc == 1 || rc == 0)
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
    case ENC_UTF16BE:
      c = get_utf16(s, true);
      break;
    case ENC_UTF16LE:
      c = get_utf16(s, false);
      break;
    case ENC_WCHAR:
#if SIZEOF_WCHAR_T == 2
#ifdef WORDS_BIGENDIAN
      c = get_utf16(s, true);
#else
      c = get_utf16(s, false);
#endif
      break;
#else
    { pl_wchar_t chr;
      char *p = (char*)&chr;
      size_t n;

      for(n=0; n<sizeof(pl_wchar_t); n++)
      { int c1 = get_byte(s);

	if ( c1 == EOF )
	{ if ( n == 0 )
	  { c = -1;
	    goto out;
	  } else
	  { Sseterr(s, SIO_WARN, "EOF in UCS character");
	    c = UTF8_MALFORMED_REPLACEMENT;
	    goto out;
	  }
	}

	*p++ = (char)c1;
      }

      c = chr;
      break;
    }
#endif
    default:
      assert(0);
      c = -1;
  }

out:
  if ( c == '\r' && (s->flags&SIO_TEXT) )
  { switch(s->newline)
    { case SIO_NL_DETECT:
	s->newline = SIO_NL_DOS;
	/*FALLTHROUGH*/
      case SIO_NL_DOS:
	goto retry;
    }
  }

  if ( s->tee && s->tee->magic == SIO_MAGIC && c != -1 )
    Sputcode(c, s->tee);

  return S__updatefilepos(s, c);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
peek needs to keep track of the actual bytes processed because not doing
so might lead to an  incorrect  byte-count   in  the  position term. The
simplest example is that when  looking   at  \r\n in Windows, get_code/1
returns \n, but it returns the same for a single \n.

Often, we could keep track of bufp and reset this, but we must deal with
the case where we fetch a new buffer. In this case, we must copy the few
remaining bytes to the `unbuffer' area. If SIO_USERBUF is set, we do not
have this spare buffer space. This  is   used  for reading from strings,
which cannot fetch a new buffer anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Speekcode(IOSTREAM *s)
{ int c;
  char *start;
  size_t safe = (size_t)-1;

  if ( !s->buffer )
  { if ( (s->flags & SIO_NBUF) )
    { errno = EINVAL;
      return -1;
    }
    if ( S__setbuf(s, NULL, 0) == (size_t)-1 )
      return -1;
  }

  if ( (s->flags & SIO_FEOF) )
    return -1;

  if ( s->bufp + UNDO_SIZE > s->limitp && !(s->flags&SIO_USERBUF) )
  { safe = s->limitp - s->bufp;
    memcpy(s->buffer-safe, s->bufp, safe);
  }

  start = s->bufp;
  if ( s->position )
  { IOPOS *psave = s->position;
    s->position = NULL;
    c = Sgetcode(s);
    s->position = psave;
  } else
  { c = Sgetcode(s);
  }
  if ( Sferror(s) )
    return -1;

  s->flags &= ~(SIO_FEOF|SIO_FEOF2);

  if ( s->bufp > start )
  { s->bufp = start;
  } else if ( c != -1 )
  { assert(safe != (size_t)-1);
    s->bufp = s->buffer-safe;
  }

  return c;
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
    *q++ = (unsigned char)c;
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

  if ( s->position )
  { for( ; chars > 0; chars-- )
    { int c;

      if ( (c = Sgetc(s)) == EOF )
	break;

      *buf++ = (char)c;
    }
  } else
  { while(chars > 0)
    { int c;

      if ( s->bufp < s->limitp )
      { size_t avail = s->limitp - s->bufp;

	if ( chars <= avail )
	{ memcpy(buf, s->bufp, chars);
	  s->bufp += chars;
	  return elms;
	} else
	{ memcpy(buf, s->bufp, avail);
	  chars -= avail;
	  buf += avail;
	  s->bufp += avail;
	}
      }

      if ( (c = S__fillbuf(s)) == EOF )
	break;

      *buf++ = (char)c;
      chars--;
    }
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
    { if ( (s->flags & SIO_FEOF) && Sfpasteof(s) != true )
	return 0;
      return c;
    }

    buf[0] = (char)c;
    limit--;
    done = 1;
  }

  n = s->limitp - s->bufp;
  if ( n > limit )
    n = limit;
  memcpy(&buf[done], s->bufp, n);
  if ( s->position && !(flags&SIO_RP_NOPOS) )
  { IOPOS *p = s->position;
    char *f = buf;
    char *e = &buf[done+n];

    for(; f<e; f++)
    { update_linepos(s, f[0]&0xff);
      p->charno++;
    }
  }

  s->bufp += n;

  return done+n;
}


/* Spending() returns the number of pending bytes on the given stream.
*/

size_t
Spending(IOSTREAM *s)
{ if ( s->bufp < s->limitp )
    return s->limitp - s->bufp;

  if ( s->functions->control )
  { size_t pending;

    if ( (*s->functions->control)(s->handle,
				  SIO_GETPENDING,
				  &pending) == 0 )
      return pending;
  }

  return 0;
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
  { ENC_UTF16BE,    2, "\376\377" },	 /* 0xfe, 0xff */
  { ENC_UTF16LE,    2, "\377\376" },	 /* 0xff, 0xfe */
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
	if ( s->position )
	  s->position->byteno += bd->bomlen;
	return 0;
      }
    }

    if ( avail >= 4 )			/* longest BOM */
      return 0;

    if ( S__fillbuf(s) == -1 )
    { s->flags &= ~SIO_FEOF;
      return 0;				/* empty stream */
    }
    s->bufp--;
  }
}


int
SwriteBOM(IOSTREAM *s)
{ switch(s->encoding)
  { case ENC_UTF8:
    case ENC_UTF16LE:
    case ENC_UTF16BE:
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
    return true;

  if ( s->bufp < s->limitp )
    return false;

  if ( s->flags & SIO_NBUF )
  { errno = EINVAL;
    return -1;
  }

  if ( S__fillbuf(s) == -1 )
    return true;

  s->bufp--;
  return false;
}


static int
S__seterror(IOSTREAM *s)
{ if ( (s->flags & SIO_FERR) )
    return 0;				/* error already set */

  s->io_errno = errno;

  if ( !(s->flags&SIO_CLOSING) &&	/* s->handle is already invalid */
       s->functions->control )
  { char *msg;

    if ( (*s->functions->control)(s->handle,
				  SIO_LASTERROR,
				  (void *)&msg) == 0 )
    { Sseterr(s, SIO_FERR, msg);
      return 0;
    }
  }

  Sseterr(s, SIO_FERR, NULL);
  return 0;
}


int
Sferror(IOSTREAM *s)
{ if ( s->magic == SIO_MAGIC )
    return (s->flags & SIO_FERR) != 0;

  errno = EINVAL;
  return -1;
}


int
Sfpasteof(IOSTREAM *s)
{ if ( s->magic == SIO_MAGIC )
    return (s->flags & (SIO_FEOF2ERR|SIO_FEOF2)) == (SIO_FEOF2ERR|SIO_FEOF2);

  errno = EINVAL;
  return -1;
}


#define SIO_ERROR_FLAGS (SIO_FEOF|SIO_WARN|SIO_FERR| \
			 SIO_FEOF2|SIO_TIMEOUT)

void
Sclearerr(IOSTREAM *s)
{ for(; s && s->magic == SIO_MAGIC; s = s->downstream)
  { s->flags &= ~SIO_ERROR_FLAGS;
    s->io_errno = 0;
    Sseterr(s, 0, NULL);
    Sset_exception(s, 0);
  }
}

/** Sseterr(IOSTREAM *s, int flags, const char *message)
 *
 * Set error state of stream.
 */

int
Sseterr(IOSTREAM *s, int flags, const char *message)
{ for(; s && s->magic == SIO_MAGIC; s = s->upstream )
  { s->flags = (s->flags & ~(SIO_WARN|SIO_FERR)) | flags;

    if ( s->message )
    { free(s->message);
      s->message = NULL;
    }
    if ( message )
      s->message = strdup(message);

    if ( s->flags&SIO_WARN )
      assert(s->message);
  }

  if ( !s )
    return 0;

  errno = EINVAL;
  return -1;
}


int
Sset_exception(IOSTREAM *s, term_t ex)
{ record_t r = NULL;

  for(; s&&s->magic == SIO_MAGIC; s = s->upstream )
  { int nflags = ex ? ((s->flags & ~SIO_WARN) | SIO_FERR)
		    : ((s->flags & ~(SIO_FERR|SIO_WARN)));

    if ( s->exception )
    { PL_erase(s->exception);
      s->exception = NULL;
    }
    if ( ex )
    { if ( r )
      { s->exception = PL_duplicate_record(r);
      } else
      { r = s->exception = PL_record(ex);
      }

      /* If the current exception is associated with the
       * stream we should clear it.  It will be re-raised
       * by reportStreamError(), which clears the exception
       * from the stream again.
       */
      term_t pending = PL_exception(0);
      if ( pending && PL_compare(ex,pending) == CMP_EQUAL )
	PL_clear_exception();
    }

    s->flags = nflags;

    return 0;
  }

  if ( !s )
    return 0;

  errno = EINVAL;
  return -1;
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
  if ( enc == ENC_OCTET )
    s->flags &= ~SIO_TEXT;
  else
    s->flags |= SIO_TEXT;

  return 0;
}

#ifdef O_LOCALE
int
Ssetlocale(IOSTREAM *s, PL_locale *new, PL_locale **old)
{ PL_locale *lo = s->locale;

  if ( old )
    *old = s->locale;
  if ( new == s->locale )
    return 0;

  if ( new )
    s->locale = acquireLocale(new);
  else
    s->locale = NULL;
  if ( lo )
    releaseLocale(lo);

  return 0;
}
#endif

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
    case ENC_UTF16BE:
    case ENC_UTF16LE:
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

int64_t
Ssize(IOSTREAM *s)
{ if ( s->functions->control )
  { int64_t size;

    if ( (*s->functions->control)(s->handle, SIO_GETSIZE, (void *)&size) == 0 )
      return size;
  }
  if ( s->functions->seek )
  { int64_t here = Stell64(s);
    int64_t end;

    if ( Sseek64(s, 0, SIO_SEEK_END) == 0 )
      end = Stell64(s);
    else
      end = -1;
    Sseek64(s, here, SIO_SEEK_SET);

    return end;
  }

  errno = ESPIPE;
  S__seterror(s);
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
    S__seterror(s);
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
  else if ( s->functions->seek && pos <= LONG_MAX && pos >= LONG_MIN )
    pos = (*s->functions->seek)(s->handle, (long)pos, whence);
  else
  { errno = EINVAL;
    S__seterror(s);
    return -1;
  }

  if ( pos < 0 )
  { S__seterror(s);
    return -1;
  }

update:
  s->flags &= ~(SIO_FEOF|SIO_FEOF2);	/* not on eof of file anymore */

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
    S__seterror(s);
    return -1;
  }
}


long
Stell(IOSTREAM *s)
{ int64_t pos = Stell64(s);

  if ( pos == -1 )
    return -1;
  if ( pos <= LONG_MAX )
    return (long) pos;

  errno = EINVAL;
  S__seterror(s);
  return -1;
}


/**
 * Set/get the notion  of the size of the console.   Typically used on
 * `Suser_output`  on systems  where we  have  no other  means to  get
 * access to the size.  This  notably concerns Epilog on Windows which
 * communicates using  Windows pipes.  This  implies we have  no POSIX
 * tty/pty, not a Windows (pseudo) console.
 */

int
Ssetttysize(IOSTREAM *s, short cols, short rows)
{ if ( s->magic != SIO_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  s->tty_size = (cols<<16) + rows;
  return 0;
}

int
Sgetttysize(IOSTREAM *s, short *cols, short *rows)
{ if ( s->magic != SIO_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  *cols = s->tty_size >> 16;
  *rows = s->tty_size & 0xffff;
  return 0;
}

		 /*******************************
		 *	      CLOSE		*
		 *******************************/

void
unallocStream(IOSTREAM *s)
{ S__destroyed(s);

#ifdef O_PLMT
  if ( s->mutex )
  { recursiveMutexDelete(s->mutex);
    PL_free(s->mutex);
    s->mutex = NULL;
  }
#endif

  if ( s->context )
    Sdprintf("WARNING: unallocStream(): stream has context??\n");

  if ( s->exception )
  { PL_erase(s->exception);
    s->exception = NULL;
  }

  if ( !(s->flags & SIO_STATIC) )
    PL_free(s);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) Sclose() can be called recursively. For example if an XPCE object is
only referenced from an open stream,  the close-function will delete the
object, which in turn calls the  ->unlink   which  may wish to close the
associated stream.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SIO_CLOSE_GC	0x4

static int
S__close(IOSTREAM *s, int flags)
{ int rval = 0;

  if ( s->magic != SIO_MAGIC )		/* already closed!? */
  { s->io_errno = errno = EINVAL;	/* also deals with erased streams */
    return -1;
  }

  if ( (s->flags&SIO_CLOSING) )		/* recursive (*) */
    return rval;

  if ( s->upstream )
  { Sseterr(s, SIO_FERR, "Locked by upstream filter");
    reportStreamError(s);
    Sunlock(s);
    return -1;
  }

  if ( (flags & (SIO_CLOSE_TRYLOCK|SIO_CLOSE_FORCE)) )
  { if ( !STRYLOCK(s) )
    { if ( (flags&SIO_CLOSE_FORCE) )
      { PL_free(s->mutex);
	s->mutex = NULL;
      } else
      { errno = EDEADLK;
	return -1;
      }
    }
  } else
    SLOCK(s);

  s->flags |= SIO_CLOSING;
  rval = S__removebuf(s);
  if ( s->mbstate )
    free(s->mbstate);

#ifdef __WINDOWS__
  if ( (s->flags & SIO_ADVLOCK) )
  { OVERLAPPED ov;
    HANDLE h = (HANDLE)_get_osfhandle((int)((uintptr_t)s->handle));

    memset(&ov, 0, sizeof(ov));
    UnlockFileEx(h, 0, 0, 0xffffffff, &ov);
    s->flags &= ~SIO_ADVLOCK;
  }
#endif
  if ( s->functions->close && (*s->functions->close)(s->handle) < 0 )
  { S__seterror(s);
    rval = -1;
  }

  while(s->locks > 0)			/* remove buffer-locks */
  { int rc = Sunlock(s);

    if ( rval == 0 )
      rval = rc;
  }
  if ( rval < 0 && !(flags&SIO_CLOSE_GC) )
    reportStreamError(s);		/* Cannot throw error from AGC */
  run_close_hooks(s);			/* deletes Prolog registration */
  s->magic = SIO_CMAGIC;
  SUNLOCK(s);

  if ( s->message )
    free(s->message);
  if ( s->locale )
    releaseLocale(s->locale);
  if ( s->references == 0 )
    unallocStream(s);
  else
    s->erased = true;

  return rval;
}

int
Sclose(IOSTREAM *s)
{ return S__close(s, 0);
}

int
Sgcclose(IOSTREAM *s, int flags)
{ return S__close(s, SIO_CLOSE_GC|flags);
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
    { *q++ = (char)c;
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

  if ( s )				/* delete trailing \n */
  { char *q;

    for(q=s; q<s+Slinesize; q++)
    { if ( *q == '\n' )
      { *q = '\0';
	break;
      }
    }
  }

  return s;
}


int
Sfputs(const char *q, IOSTREAM *s)
{ for( ; *q; q++)
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

/* SfprintfX() is identical to Sfprintf() but its definition in
   SWI-Stream.h doesn't have the "check format" attribute. */

int
SfprintfX(IOSTREAM *s, const char *fm, ...)
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

static int
next_chr(const char **s, IOENC enc)
{ switch(enc)
  { case ENC_ANSI:
    case ENC_ISO_LATIN_1:
    { unsigned char c = (unsigned char)**s;
      ++(*s);
      return c;
    }
    case ENC_UTF8:
    { int c;
      PL_utf8_code_point(s, NULL, &(c));
      return c;
    }
    case ENC_WCHAR:
    { const wchar_t *w = (const wchar_t*)*s;
      int c;

      w = get_wchar(w, &c);
      *s = (const char*)w;
      return c;
    }
    default:
      assert(0);
      return -1;
  }
}

#define OUTCHR(s, c)	do { printed++; \
			     if ( Sputcode((c), (s)) < 0 ) goto error; \
			   } while(0)
#define OUTFS() \
	do \
	{ if ( fs == fbuf ) \
	  { while(fs < fe) \
	    { int c = next_chr((const char**)&fs, enc); \
	      OUTCHR(s, c); \
	    } \
	  } else \
	  { for(;;) \
	    { int c = next_chr((const char**)&fs, enc); \
	      if ( c ) \
		OUTCHR(s, c); \
	      else \
		break; \
	    } \
	  } \
	} while(0)

#define valdigit(c)	((c) - '0')
#define A_LEFT	0			/* left-aligned field */
#define A_RIGHT 1			/* right-aligned field */

#define SNPRINTF3(fm, a1) \
	{ size_t __r; \
	  assert(fs == fbuf); \
	  __r = snprintf(fs, sizeof(fbuf), fm, a1); \
	  if ( __r >= sizeof(fbuf) ) \
	  { if ( (fs_malloced = fs = malloc(__r+1)) == NULL ) goto error; \
	    __r = snprintf(fs, __r+1, fm, a1); \
	  } \
	  fe = fs+__r; \
	}
#define SNPRINTF4(fm, a1, a2) \
	{ size_t __r; \
	  assert(fs == fbuf); \
	  __r = snprintf(fs, sizeof(fbuf), fm, a1, a2); \
	  if ( __r >= sizeof(fbuf) ) \
	  { if ( (fs_malloced = fs = malloc(__r+1)) == NULL ) goto error; \
	    __r = snprintf(fs, __r+1, fm, a1, a2); \
	  } \
	  fe = fs+__r; \
	}

typedef enum
{ INT_INT       = 0,
  INT_LONG      = 1,
  INT_LONG_LONG = 2,
  INT_SIZE_T    = 3,
  INT_PTRDIFF_T = 4
} int_type;

int
Svfprintf(IOSTREAM *s, const char *fm, va_list args)
{ int printed = 0;
  char buf[TMPBUFSIZE];
  int tmpbuf;
  char *fs_malloced = NULL;

  SLOCK(s);

  if ( !s->buffer && (s->flags & SIO_NBUF) )
  { S__setbuf(s, buf, sizeof(buf));
    tmpbuf = true;
  } else
    tmpbuf = false;

  while(*fm)
  { if ( *fm == '%' )
    { fm++;

      if ( *fm == '%' )
      { OUTCHR(s, *fm);
	fm++;
	continue;
      } else
      { int align = A_RIGHT;
	int modified = false;
	int has_arg1 = false, has_arg2 = false;
	int arg1=0, arg2=0;
	char fbuf[100], *fs = fbuf, *fe = fbuf;
	int islong = 0;
	int pad = ' ';
	IOENC enc = ENC_ANSI;

	for(;;)
	{ switch(*fm)
	  { case '+':	align = A_RIGHT; fm++; continue;
	    case '-':	align = A_LEFT;  fm++; continue;
	    case '0':	pad = '0';	 fm++; continue;
	    case ' ':	pad = ' ';       fm++; continue;
	    case '#':   modified = true; fm++; continue;
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
	{ islong = INT_LONG;		/* 1: %ld */
	  fm++;
	}
	switch ( *fm )
	{ case 'l':
	    islong = INT_LONG_LONG;	/* 2: %lld */
	    fm++;
	    break;
	  case 'z':
	    islong = INT_SIZE_T;
	    fm++;
	    break;
	  case 't':
	    islong = INT_PTRDIFF_T;
	    fm++;
	    break;
	  case 'L':			/* %Ls: ISO-Latin-1 string */
	    enc = ENC_ISO_LATIN_1;
	    fm++;
	    break;
	  case 'U':			/* %Us: UTF-8 string */
	    enc = ENC_UTF8;
	    fm++;
	    break;
	  case 'W':			/* %Ws: wide string */
	    enc = ENC_WCHAR;
	    fm++;
	    break;
	}

	switch(*fm)
	{ case 'c':
	  { int c = va_arg(args, int);
	    fe = utf8_put_char(fe, c);
	    enc = ENC_UTF8;
	    break;
	  }
	  case 'p':
	  { void *ptr = va_arg(args, void*);
	    char fmbuf[8], *fp=fmbuf;
	    *fp++ = '%';
	    if ( modified )
	      *fp++ = '#';
	    *fp++ = 'p';
	    *fp   = '\0';
	    SNPRINTF3(fmbuf, ptr);

	    break;
	  }
	  case 'd':
	  case 'i':
	  case 'o':
	  case 'u':
	  case 'x':
	  case 'X':
	  { int      vi = 0;
	    long     vl = 0;			/* make compiler silent */
	    int64_t vll = 0;
	    char fmbuf[8], *fp=fmbuf;

	    switch( islong )
	    { case INT_INT:
		vi = va_arg(args, int);
		break;
	      case INT_LONG:
		vl = va_arg(args, long);
		break;
	      case INT_LONG_LONG:
		vll = va_arg(args, int64_t);
		break;
	      case INT_SIZE_T:
		vll = va_arg(args, size_t);
		break;
	      case INT_PTRDIFF_T:
		vll = va_arg(args, ptrdiff_t);
		break;
	      default:
		assert(0);
	    }

	    *fp++ = '%';
	    if ( modified )
	      *fp++ = '#';
	    switch( islong )
	    { case INT_INT:
		*fp++ = *fm;
		*fp   = '\0';
		SNPRINTF3(fmbuf, vi);
		break;
	      case INT_LONG:
		*fp++ = 'l';
		*fp++ = *fm;
		*fp   = '\0';
		SNPRINTF3(fmbuf, vl);
		break;
	      case INT_LONG_LONG:
	      case INT_SIZE_T:
	      case INT_PTRDIFF_T:
#ifdef __WINDOWS__
		*fp++ = 'I';		/* Synchronise with INT64_FORMAT! */
		*fp++ = '6';
		*fp++ = '4';
#else
		*fp++ = 'l';
		*fp++ = 'l';
#endif
		*fp++ = *fm;
		*fp   = '\0';
		SNPRINTF3(fmbuf, vll);
		break;
	    }

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
	    if ( has_arg2 )		/* specified precission */
	    { *fp++ = '.';
	      *fp++ = '*';
	      *fp++ = *fm;
	      *fp   = '\0';
	      SNPRINTF4(fmbuf, arg2, v);
	    } else
	    { *fp++ = *fm;
	      *fp   = '\0';
	      SNPRINTF3(fmbuf, v);
	    }
	    fe = &fs[strlen(fs)];

	    break;
	  }
	  case 's':
	    fs = va_arg(args, char *);
	    if ( !fs )
	    { fs = "(null)";
	      enc = ENC_ISO_LATIN_1;
	    }
	    break;
	}

	/* Now `fs` is either the result of some sub-formatting or the
	   argument of `%s`.  In the latter case it is by definition
	   0-terminated.  In the former case it may hold 0-bytes, e.g.,
	   `%c` using a 0 argument.  `fe` points at the end.
	*/

	if ( has_arg1 )			/* aligned field */
	{ if ( fs == fbuf )
	    *fe = '\0';

	  if ( align == A_LEFT )
	  { int printed0 = printed;
	    OUTFS();
	    int w = printed-printed0;
	    while(w < arg1)
	    { OUTCHR(s, pad);
	      w++;
	    }
	  } else /*if ( align == A_RIGHT ) */
	  { size_t w;

	    if ( fs == fbuf )
	    { w = fe - fs;
	    } else
	    { switch(enc)
	      { case ENC_ANSI:
		  w = strlen(fs);
		  break;
		case ENC_UTF8:
		  w = strlen(fs);
		  w = utf8_strlen(fs, w);
		  break;
		case ENC_WCHAR:
		  w = wcslen((wchar_t*)fs);
		  break;
		default:
		  assert(0);
		  w = 0;		/* make compiler happy */
		  break;
	      }
	    }

	    if ( (ssize_t)w < arg1 )
	    { w = arg1 - w;
	      while(w > 0 )
	      { OUTCHR(s, pad);
		w--;
	      }
	    }
	    OUTFS();
	  }
	} else
	{ OUTFS();
	}
	fm++;
	if ( fs_malloced )
	{ fs_malloced = NULL;
	  free(fs_malloced);
	}
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
  if ( fs_malloced )
    free(fs_malloced);

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
Ssnprintf(char *buf, size_t size, const char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svsnprintf(buf, size, fm, args);
  va_end(args);

  return rval;
}


/* SsnprintfX() is identical to Ssnprintf() but its definition in
   SWI-Stream.h doesn't have the "check format" attribute. */

int
SsnprintfX(char *buf, size_t size, const char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svsnprintf(buf, size, fm, args);
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
  s.encoding  = ENC_UTF8;

  if ( (rval = Svfprintf(&s, fm, args)) >= 0 )
    *s.bufp = '\0';

  return rval;
}


/* Svsnprintf() writes at most `size` bytes to `buf`, while the
   produced string is always 0-terminated (i.e., it emits at most
   `size-1` bytes from the specification.
*/

int
Svsnprintf(char *buf, size_t size, const char *fm, va_list args)
{ IOSTREAM s;
  int rval;

  Sopen_buffer(&s, buf, size);
  rval = Svfprintf(&s, fm, args);
  Sclose_buffer(&s);

  return rval;
}


int
Svdprintf(const char *fm, va_list args)
{ int rval;
  IOSTREAM *s = Serror;

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

#ifdef O_DEBUG
#undef Sdprintf
#endif

int
Sdprintf(const char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svdprintf(fm, args);
  va_end(args);

  return rval;
}

/* SdprintfX() is identical to Sdprintf() but its definition in
   SWI-Stream.h doesn't have the "check format" attribute. */

int
SdprintfX(const char *fm, ...)
{ va_list args;
  int rval;

  va_start(args, fm);
  rval = Svdprintf(fm, args);
  va_end(args);

  return rval;
}

#ifdef O_DEBUG

int
Sdprintf_ex(const char *channel, const char *file, int line, const char *fm, ...)
{ va_list args;
  int rval;

  if ( Serror->position && Serror->position->linepos == 0 && channel)
  { const char *logfmt = "[%s] %s:%d: ";

    if (strncmp(channel, "DBG_LEVEL", 9) == 0)
    { channel += 9;
      logfmt = "<%s> %s:%d: ";
    }
    if (strncmp(file, "../", 3) == 0)
      file += 3;
    Sdprintf(logfmt, channel, file, line);
  }

  va_start(args, fm);
  rval = Svdprintf(fm, args);
  va_end(args);

  return rval;
}

#define Sdprintf(fmt...) Sdprintf_ex(NULL, __FILE__, __LINE__, fmt)
#endif /*O_DEBUG*/

#if 0
		 /*******************************
		 *	      SCANF		*
		 *******************************/

int
Svfscanf(IOSTREAM *s, const char *fm, va_list args)
{ int done = 0;				/* # items converted */
  int chread = 0;			/* # characters read */
  int c = GET(s);			/* current character */
  int supress;				/* if true, don't assign (*) */
  int field_width;			/* max width of field */
  int tsize;				/* SZ_SHORT, SZ_NORMAL, SZ_LONG */

  while(*fm)
  { if ( *fm == ' ' )
    { while ( isblank(c) )
	c = GET(s);
      fm++;
      continue;
    } else if ( *fm == '%' && fm[1] != '%' )
    { supress = false;
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
	  negative = false;
	  if ( c == '+' )
	    c = GET(s);
	  else if ( c == '-' )
	  { negative++;
	    c = GET(s);
	  }
	do_unsigned:
	  ok = false;
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
	  negative = false;
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
	    negative = false;
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

The filter is referenced, so it won't be freed, even if it is closed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Sset_filter(IOSTREAM *parent, IOSTREAM *filter)
{ if ( !parent || parent->magic != SIO_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  if ( filter )				/* set filter */
  { if ( filter->magic != SIO_MAGIC )
    { errno = EINVAL;
      return -1;
    }
    Sreference(filter);
    Sreference(parent);
    assert(parent->upstream==NULL && filter->downstream==NULL);
    parent->upstream = filter;
    filter->downstream = parent;
    filter->timeout = parent->timeout;
  } else				/* clear filter */
  { if ( (filter=parent->upstream) )
    { assert(filter->downstream == parent);
      filter->downstream = NULL;
      parent->upstream = NULL;
      if ( Sunreference(filter) == 0 && filter->erased )
	unallocStream(filter);
      if ( Sunreference(parent) == 0 && parent->erased )
	unallocStream(parent);
    }
  }

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

#ifdef __WINDOWS__
  bytes = write((int)h, buf, (int)size);
#else
  bytes = write((int)h, buf, size);
#endif

  return bytes;
}


static long
Sseek_file(void *handle, long pos, int whence)
{ intptr_t h = (intptr_t) handle;
  off_t rc = lseek((int)h, pos, whence); /* cannot do EINTR according to man */

  if ( rc > LONG_MAX )
  { errno = EINVAL;
    return -1;
  }

  return (long)rc;
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
  } while ( rc == -1 && errno == EINTR );

  return rc;
}


static int
Scontrol_file(void *handle, int action, void *arg)
{ intptr_t h = (intptr_t) handle;
  int fd = (int)h;

  switch(action)
  { case SIO_GETSIZE:
    { int64_t *rval = arg;
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
    case SIO_GETFILENO:
    { int *p = arg;
      *p = fd;
      return 0;
    }
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


		 /*******************************
		 *	    TTY STREAMS		*
		 *******************************/

static ssize_t
Sread_tty(void *handle, char *buf, size_t size)
{ ssize_t bytes = Sread_file(handle, buf, size);

  if ( bytes < 0 && errno == EBADF )
  { errno = 0;
    return 0;
  }

  return bytes;
}


static ssize_t
Swrite_tty(void *handle, char *buf, size_t size)
{ ssize_t bytes = Swrite_file(handle, buf, size);

  if ( bytes == -1 && errno == EBADF )
  { errno = 0;
    return size;
  }

  return bytes;
}


static int
Sclose_tty(void *handle)
{ int rc = Sclose_file(handle);

  if ( rc == -1 && errno == EBADF )
  { errno = 0;
    return 0;
  }

  return rc;
}


IOFUNCTIONS Sttyfunctions =
{ Sread_tty,
  Swrite_tty,
  Sseek_file,
  Sclose_tty,
  Scontrol_file,
#ifdef O_LARGEFILES
  Sseek_file64
#else
  NULL
#endif
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For now, we use PL_malloc_uncollectable(). In   the  end, this is really
one of the object-types we want to leave to GC.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef FD_CLOEXEC			/* This is not defined in MacOS */
#define FD_CLOEXEC 1
#endif

IOSTREAM *
Snew(void *handle, int flags, IOFUNCTIONS *functions)
{ IOSTREAM *s;

  if ( !(s = PL_malloc_uncollectable(sizeof(IOSTREAM))) )
  { errno = ENOMEM;
    return NULL;
  }

  S__created(s);

  memset((char *)s, 0, sizeof(IOSTREAM));
  s->magic         = SIO_MAGIC;
  s->lastc         = EOF;
  s->flags         = flags;
  s->handle        = handle;
  s->functions     = functions;
  s->timeout       = -1;		/* infinite */
  s->posbuf.lineno = 1;
  if ( (flags&SIO_TEXT) )
  { s->encoding    = initEncoding();
  } else
  { s->encoding	   = ENC_OCTET;
  }
#if CRLF_MAPPING
  s->newline       = SIO_NL_DOS;
#endif
  if ( flags & SIO_RECORDPOS )
    s->position = &s->posbuf;
#ifdef O_PLMT
  if ( !(flags & SIO_NOMUTEX) )
  { if ( !(s->mutex = PL_malloc(sizeof(recursiveMutex))) )
    { PL_free(s);
      return NULL;
    }
    recursiveMutexInit(s->mutex);
  }
#endif

  s->fileno = S__fileno(s);
  if ( s->fileno >= 0 )
  { int fd = (int)s->fileno;

#ifdef __WINDOWS__
    if ( win_isconsole(s) )
#else
    if ( isatty(fd) )
#endif
      s->flags |= SIO_ISATTY;

#if defined(F_SETFD)
    fcntl(fd, F_SETFD, FD_CLOEXEC);
#elif defined(__WINDOWS__)
    SetHandleInformation((HANDLE)_get_osfhandle(fd),
			 HANDLE_FLAG_INHERIT, 0);
#endif
  }

#ifdef O_LOCALE
  initStreamLocale(s);
#endif

  return s;
}


#ifndef O_BINARY
#define O_BINARY 0
#endif

static int
get_mode(const char *s, int *mp)
{ int n, m = 0;

  for(n=0; n < 3; n++)
  { if ( *s >= '0' && *s <= '7' )
      m = (m<<3) + *s - '0';
    else
      return false;
  }

  *mp = m;
  return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open a file. In addition to the normal arguments, the following can come
after the [rw] argument:

  - "b" -- use binary mode
  - "l[rw]" -- use a read or write lock
  - "L[rw]" -- use a read or write lock and raise an exception if we
	       must wait
  - mOOO -- when creating the file, use 0OOO as mode.

Note that the low-level open  is  always   binary  as  O_TEXT open files
result in lost and corrupted data in   some  encodings (UTF-16 is one of
them).  Sgetcode()  and  Sputcode()  do  the  LF  <->  CRLF  mapping  of
CRLF_MAPPING is defined.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

IOSTREAM *
Sopen_file(const char *path, const char *how)
{ int fd;
  int oflags = O_BINARY;
  int flags = SIO_FILE|SIO_TEXT|SIO_RECORDPOS|SIO_FBUF;
  int op = *how++;
  intptr_t lfd;
  enum {lnone=0,lread,lwrite} lock = lnone;
  IOSTREAM *s;
  IOENC enc = ENC_UNKNOWN;
  int wait = true;
  int mode = 0666;

  for( ; *how; how++)
  { switch(*how)
    { case 'b':				/* binary */
	flags &= ~SIO_TEXT;
	enc = ENC_OCTET;
	break;
      case 'r':				/* no record */
	flags &= ~SIO_RECORDPOS;
	break;
      case 'L':				/* lock r: read, w: write */
	wait = false;
	/*FALLTHROUGH*/
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
      case 'm':
	if ( get_mode(how+1, &mode) )
	{ how += 3;
	  break;
	} else
	{ errno = EINVAL;
	  return NULL;
	}
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
      fd = open(path, O_WRONLY|O_CREAT|O_TRUNC|oflags, mode);
      flags |= SIO_OUTPUT;
      break;
    case 'a':
      fd = open(path, O_WRONLY|O_CREAT|O_APPEND|oflags, mode);
      flags |= SIO_OUTPUT|SIO_APPEND;
      break;
    case 'u':
      fd = open(path, O_WRONLY|O_CREAT|oflags, mode);
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
    buf.l_whence = SEEK_SET;
    buf.l_type   = (lock == lread ? F_RDLCK : F_WRLCK);

    while( fcntl(fd, wait ? F_SETLKW : F_SETLK, &buf) != 0 )
    { if ( errno == EINTR )
      { if ( PL_handle_signals() < 0 )
	{ close(fd);
	  return NULL;
	}
	continue;
      } else
      { int save = errno;

	close(fd);
	errno = save;
	return NULL;
      }
    }
#else					/* we don't have locking */
#if __WINDOWS__
    HANDLE h = (HANDLE)_get_osfhandle(fd);
    OVERLAPPED ov;
    int flags = 0;

    if ( lock == lwrite )
      flags |= LOCKFILE_EXCLUSIVE_LOCK;
    if ( !wait )
      flags |= LOCKFILE_FAIL_IMMEDIATELY;

    memset(&ov, 0, sizeof(ov));
    if ( !LockFileEx(h, flags,
		     0,
		     0, 0xfffffff,
		     &ov) )
    { close(fd);
      errno = (wait ? EACCES : EAGAIN);	/* TBD: proper error */
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
Sopen_iri_or_file(const char *path, const char *how)
{ int sl;
  IOSTREAM *s;

  if ( (sl=file_name_is_iri(path)) )
  { atom_t mname = how[0] == 'r' ? ATOM_read : ATOM_write;
    const char *m;

    if ( !iri_hook(path, IRI_OPEN, mname, 0, &s) )
      return NULL;

    for(m=&how[1]; *m; m++)
    { if ( *m == 'b' )
      { clear(s, SIO_TEXT);
	s->encoding = ENC_OCTET;
	s->newline  = SIO_NL_POSIX;
      } else if ( *m == 'r' )
      { clear(s, SIO_RECORDPOS);
	s->position = NULL;
      }
    }
  } else
  { s = Sopen_file(path, how);
  }

  return s;
}

#ifdef __WINDOWS__
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
[not sure] It seems the CRT library of Windows can operate in two modes:
one where the CRT state is  shared   between  the application and shared
objects loaded into it, and one where this  is not the case. The default
Windows build uses the shared view  and   the  Conda build the separated
view.  This means that CRT handles cannot cross a component boundary.

The clib/process.c component needs to turn an   I/O HANDLE into a Prolog
stream.  As  Prolog  streams  use   the    CRT   handle,  we  must  call
_open_osfhandle in the SWI-Prolog core.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Swin_open_osfhandle(HANDLE h, int flags)
{ return _open_osfhandle((intptr_t)h, flags);
}

IOSTREAM *
Swin_open_handle(HANDLE h, const char *mode)
{ int fd = Swin_open_osfhandle(h, 0);

  return Sfdopen(fd, mode);
}
#endif


IOSTREAM *
Sfdopen(int fd, const char *type)
{ intptr_t lfd;
  int flags = SIO_FILE|SIO_RECORDPOS|SIO_FBUF;

  if ( fd < 0 )
  { errno = EINVAL;
    return NULL;
  }
#if defined(HAVE_FCNTL) && defined(F_GETFL)
  if ( fcntl(fd, F_GETFL) == -1 )
    return NULL;
#endif

  if ( *type == 'r' )
  { flags |= SIO_INPUT;
  } else if ( *type == 'w' )
  { flags |= SIO_OUTPUT;
  } else
  { errno = EINVAL;
    return NULL;
  }
  if ( type[1] != 'b' )
    flags |= SIO_TEXT;

  lfd = (intptr_t)fd;

  return Snew((void *)lfd, flags, &Sfilefunctions);
}

/* MT: as long as s is valid, this should be ok
*/

static int
S__fileno(IOSTREAM *s)
{ if ( (s->flags & SIO_FILE) )
  { return (int)(intptr_t)s->handle;
  } else
  { IOFUNCTIONS *funcs = s->functions;
    int n;

    if ( s->magic == SIO_MAGIC &&
	 funcs->control &&
	 (*funcs->control)(s->handle,
			   SIO_GETFILENO,
			   (void *)&n) == 0 )
      return n;

    errno = EINVAL;
    return -1;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sfileno() returns  the cached file  no rather then  calling Scontrol()
using  SIO_GETFILENO.   This  is  to  ensure  safe  stream_property(S,
file_no(I)), which cannot lock the  stream.  The `fileno` field is set
by Snew().   If the stream is  modified such that the  associated file
handle  is   changed,  the  user   must  update  the   `fileno`  field
accordingly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
Sfileno(IOSTREAM *s)
{ if ( s->magic != SIO_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  DEBUG(0, assert(s->fileno == S__fileno(s)));
  if ( s->fileno >= 0 )
    return (int)s->fileno;

  return -1;
}


#ifdef __WINDOWS__

HANDLE
Swinhandle(IOSTREAM *s)
{ HANDLE h = NULL;

  if ( s->functions->control &&
       (*s->functions->control)(s->handle,
				SIO_GETWINHANDLE,
				(void *)&h) == 0 )
    return h;

  int fd = Sfileno(s);
  if ( fd >= 0 )
    return (HANDLE)_get_osfhandle(fd);

  return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On  Windows,  type  SOCKET  is   an    unsigned   int   and  all  values
[0..INVALID_SOCKET) are valid. It is  also   not  allowed  to run normal
file-functions on it or the application will crash. There seems to be no
way out except for introducing an extra function at this level :-(
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

SOCKET
Swinsock(IOSTREAM *s)
{ SOCKET n = INVALID_SOCKET;

  if ( s->functions->control &&
       (*s->functions->control)(s->handle,
				SIO_GETWINSOCK,
				(void *)&n) == 0 )
  { return n;
  }

  errno = EINVAL;
  return INVALID_SOCKET;
}
#endif

		 /*******************************
		 *	       PIPES		*
		 *******************************/

#ifdef HAVE_POPEN
#ifdef __WINDOWS__
#define popen _popen
#define pclose _pclose
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


static int
Scontrol_pipe(void *handle, int action, void *arg)
{ FILE *fp = handle;

  switch(action)
  { case SIO_GETFILENO:
    { int *ap = arg;
      *ap = fileno(fp);
      return 0;
    }
    case SIO_FLUSHOUTPUT:
    case SIO_SETENCODING:
      return 0;
    default:
      return -1;
  }
}


IOFUNCTIONS Spipefunctions =
{ Sread_pipe,
  Swrite_pipe,
  (Sseek_function)0,
  Sclose_pipe,
  Scontrol_pipe
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
  { int flags = SIO_RECORDPOS|SIO_FBUF|SIO_TEXT;

    for(; *type; type++)
    { switch(*type)
      { case 'r':
	  flags |= SIO_INPUT;
	  break;
	case 'w':
	  flags |= SIO_OUTPUT;
	  break;
	case 'b':
	  flags &= ~SIO_TEXT;
	  break;
	default:
	  assert(0);
      }
    }

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
  char	       *buffer;			/* allocated buffer */
  char	      **bufferp;		/* Write-back location */
  int		malloced;		/* malloc() maintained */
  int		free_on_close;		/* free allocated buffer on close */
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
  { size_t ns = S__memfile_nextsize(mf->here + size + 1);
    char *nb;

    if ( mf->allocated == 0 || !mf->malloced )
    { if ( !(nb = malloc(ns)) )
      { errno = ENOMEM;
	return -1;
      }
      if ( !mf->malloced )
      { if ( mf->buffer )
	  memcpy(nb, mf->buffer, mf->allocated);
	mf->malloced = true;
      }
    } else
    { if ( !(nb = realloc(mf->buffer, ns)) )
      { errno = ENOMEM;
	return -1;
      }
    }

    mf->allocated = ns;
    *mf->bufferp = mf->buffer = nb;
  }

  memcpy(&mf->buffer[mf->here], buf, size);
  mf->here += size;

  if ( mf->here > mf->size )
  { mf->size = mf->here;
    if ( mf->sizep )			/* make externally known */
      *mf->sizep = mf->size;
    mf->buffer[mf->size] = '\0';
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

  memcpy(buf, &mf->buffer[mf->here], size);
  mf->here += size;

  return size;
}


static int64_t
Sseek_memfile64(void *handle, int64_t offset, int whence)
{ memfile *mf = handle;

  switch(whence)
  { case SIO_SEEK_SET:
      break;
    case SIO_SEEK_CUR:
      offset += mf->here;
      break;
    case SIO_SEEK_END:
      offset = mf->size - offset;
      break;
    default:
      errno = EINVAL;
      return -1;
  }
  if ( offset < 0 || offset > mf->size )
  { errno = EINVAL;
    return -1;
  }
  mf->here = (size_t)offset;

  return offset;
}


static long
Sseek_memfile(void *handle, long offset, int whence)
{ memfile *mf = handle;
  size_t old = mf->here;

  int64_t rval = Sseek_memfile64(handle, (int64_t)offset, whence);
  long rc = (long)rval;

  if ( rval == rc )
  { return rc;
  } else
  { mf->here = old;
    errno = EINVAL;
    return -1;
  }
}


static int
Sclose_memfile(void *handle)
{ memfile *mf = handle;

  if ( mf )
  { if ( mf->free_on_close && mf->buffer )
      PL_free(mf->buffer);
    free(mf);
    return 0;
  }

  errno = EINVAL;			/* not opened */
  return -1;
}

static int
Scontrol_memfile(void *handle, int action, void *arg)
{ memfile *mf = handle;

  switch(action)
  { case SIO_SETENCODING:
      errno = EPERM;
      return -1;
    case SIO_FLUSHOUTPUT:
      return 0;
    case SIO_GETSIZE:
    { int64_t *szp = arg;

      *szp = mf->here;
      return 0;
    }
    case SIO_GETREPOSITION:
    { int *valp = arg;
      *valp = true;

      return 0;
    }
    default:
      errno = EINVAL;
      return -1;
  }
}


IOFUNCTIONS Smemfunctions =
{ Sread_memfile,
  Swrite_memfile,
  Sseek_memfile,
  Sclose_memfile,
  Scontrol_memfile,
  Sseek_memfile64
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

    Mode is "r" or "w".  The mode "rF" calls PL_free(*buffer) at when
    closed.

Note: Its is NOT allowed to access   streams created with this call from
multiple threads. This is ok for all   usage inside Prolog itself (often
through tellString()/toldString(). This call is   intented  to use write
and other output predicates to create strings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

IOSTREAM *
Sopenmem(char **bufp, size_t *sizep, const char *mode)
{ memfile *mf = malloc(sizeof(memfile));
  int flags = SIO_FBUF|SIO_RECORDPOS|SIO_NOMUTEX|SIO_TEXT;
  size_t size;
  IOSTREAM *s;

  if ( !mf )
  { errno = ENOMEM;
    return NULL;
  }

  mf->malloced      = false;
  mf->free_on_close = false;
  mf->bufferp       = bufp;
  mf->buffer        = *bufp;

  for(; *mode; mode++)
  { switch(*mode)
    { case 'r':
	flags |= SIO_INPUT;
	if ( sizep == NULL || *sizep == (size_t)-1 )
	  size = (mf->buffer ? strlen(mf->buffer) : 0);
	else
	  size = *sizep;
	mf->size = size;
	mf->allocated = size+1;
	break;
      case 'w':
	flags |= SIO_OUTPUT;
	mf->size = 0;
	mf->allocated = (sizep ? *sizep : 0);
	if ( mf->buffer == NULL || mode[1] == 'a' )
	  mf->malloced = true;
	if ( mf->buffer )
	  mf->buffer[0] = '\0';
	if ( sizep )
	  *sizep = mf->size;
	break;
      case 'b':
	flags &= ~SIO_TEXT;
	break;
      case 'F':
	mf->free_on_close = true;
	break;
      default:
	free(mf);
	errno = EINVAL;
	return NULL;
    }
  }

  mf->sizep	= sizep;
  mf->here      = 0;

  if ( (s=Snew(mf, flags, &Smemfunctions)) )
    s->newline = SIO_NL_POSIX;

  return s;
}

		 /*******************************
		 *	      STRINGS		*
		 *******************************/

/* MT: we assume these handles are not passed between threads
*/

static ssize_t
Sread_string(void *handle, char *buf, size_t size)
{ (void)handle;
  (void)buf;
  (void)size;

  return 0;				/* signal EOF */
}

static ssize_t
Swrite_string(void *handle, char *buf, size_t size)
{ (void)handle;
  (void)buf;
  (void)size;

  errno = ENOSPC;			/* signal error */
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
  { if ( !(s = PL_malloc_uncollectable(sizeof(IOSTREAM))) ) /* TBD: Use GC */
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
		 *        BUFFER STREAMS        *
		 *******************************/

static ssize_t
Swrite_buffer(void *handle, char *buf, size_t size)
{ (void)handle;
  (void)buf;
  (void)size;

  return -1;
}

IOFUNCTIONS Sbufferfunctions =
{ NULL, /* read */
  Swrite_buffer,
  NULL, /* seek */
  NULL  /* close */
};

static IOSTREAM *
Sopen_buffer(IOSTREAM *s, char *buf, size_t size)
{
  memset((char *)s, 0, sizeof(IOSTREAM));
  s->bufp      = buf;
  s->limitp    = &buf[size-1];
  s->buffer    = buf;
  s->flags     = SIO_FBUF|SIO_OUTPUT;
  s->functions = &Sbufferfunctions;
  s->encoding  = ENC_UTF8;
  s->magic     = SIO_MAGIC;

  return s;
}

static void
Sclose_buffer(IOSTREAM *s)
{ *s->bufp++ = '\0';
}


		 /*******************************
		 *	 STANDARD HANDLES	*
		 *******************************/

#define STDIO(n, f) \
	{ .bufp = NULL, .limitp = NULL, .buffer = NULL, .unbuffer = NULL,  \
	  .lastc = EOF, .magic = SIO_MAGIC, .flags = (f),		   \
	  .handle = (void *)(n), .fileno = n, .functions = &Sttyfunctions, \
	  .timeout = -1, .encoding = ENC_ISO_LATIN_1			   \
	}

#define SIO_STDIO (SIO_FILE|SIO_STATIC|SIO_NOCLOSE|SIO_ISATTY|SIO_TEXT)
#define STDIO_STREAMS \
  STDIO(0, SIO_STDIO|SIO_LBUF|SIO_INPUT|SIO_NOFEOF),	/* Sinput */ \
  STDIO(1, SIO_STDIO|SIO_LBUF|SIO_OUTPUT|SIO_REPPLU),	/* Soutput */ \
  STDIO(2, SIO_STDIO|SIO_NBUF|SIO_OUTPUT|SIO_REPPLU)	/* Serror */


IOSTREAM S__iob[] =
{ STDIO_STREAMS
};


static const IOSTREAM S__iob0[] =
{ STDIO_STREAMS
};


static bool S__initialised = false;

#ifdef __WINDOWS__
#define isatty(fd) win_isatty(fd)
static bool
win_isatty(int fd)
{ HANDLE h = (HANDLE)_get_osfhandle(fd);
  DWORD mode;
  return GetConsoleMode(h, &mode);
}
#endif

void
SinitStreams(void)
{ if ( !S__initialised )
  { int i;
    IOENC enc;

    S__initialised = true;
    enc = initEncoding();

    for(i=0; i<=2; i++)
    { IOSTREAM *s = &S__iob[i];

      if ( !isatty(i) && s->functions == &Sttyfunctions )
      { s->flags &= ~SIO_ISATTY;
	s->functions = &Sfilefunctions; /* Check for pipe? */
      }
      if ( s->encoding == ENC_ISO_LATIN_1 )
	s->encoding = enc;
#ifdef O_PLMT
      s->mutex = PL_malloc(sizeof(recursiveMutex));
      recursiveMutexInit(s->mutex);
#endif
#if CRLF_MAPPING
      _setmode(i, O_BINARY);
      s->newline = SIO_NL_DOS;
#endif
    }
  }
}


IOSTREAM *
S__getiob(void)
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

  if ( s->close_hook )
  { (*s->close_hook)(s->closure);
    s->close_hook = NULL;
  }

  for(p=close_hooks; p; p = p->next)
    (*p->hook)(s);
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
{ IOSTREAM *s;

  if ( (s=Sinput) && s->magic == SIO_MAGIC )
  { s->bufp = s->limitp = s->buffer;
  }
  if ( (s=Soutput) && s->magic == SIO_MAGIC )
  { s->bufp = s->buffer;
  }
  if ( (s=Serror) && s->magic == SIO_MAGIC )
  { s->bufp = s->buffer;
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

    if ( s->mbstate )
      free(s->mbstate);
    if ( s->locale )
      releaseLocale(s->locale);
    s->bufp = s->buffer;		/* avoid actual flush */
    S__removebuf(s);

#ifdef O_PLMT
    if ( S__iob[i].mutex )
    { recursiveMutex *m = S__iob[i].mutex;

      S__iob[i].mutex = NULL;
      recursiveMutexDelete(m);
      PL_free(m);
    }
#endif

    *s = S__iob0[i];			/* re-initialise */
  }

  S__initialised = false;
}
