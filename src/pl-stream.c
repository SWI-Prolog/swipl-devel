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

#if defined(WIN32) && !defined(__WIN32__)
#define __WIN32__ 1
#endif

#ifdef __WIN32__
#include <uxnt.h>
#define MD "config/win32.h"
#include <winsock2.h>
#endif

#include <wchar.h>
typedef wchar_t pl_wchar_t;


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

#if O_LARGEFILES
#define _FILE_OFFSET_BITS 64
#endif

#define PL_KERNEL 1
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

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#define char_to_int(c)	(0xff & (int)(c))

#define TMPBUFSIZE 256			/* Serror bufsize for Svfprintf() */

int Slinesize = SIO_LINESIZE;		/* Sgets() buffer size */

static int	S__flushbuf(IOSTREAM *s);
static void	run_close_hooks(IOSTREAM *s);

#define S__fupdatefilepos(s, c) S___fupdatefilepos(s, c)

#ifdef O_PLMT
#define SLOCK(s)    if ( s->mutex ) recursiveMutexLock(s->mutex)
#define SUNLOCK(s)  if ( s->mutex ) recursiveMutexUnlock(s->mutex)
inline int
STRYLOCK(IOSTREAM *s)
{ if ( s->mutex &&
       recursiveMutexTryLock(s->mutex) == MUTEX_BUSY )
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

		 /*******************************
		 *	      BUFFER		*
		 *******************************/

static int
S__setbuf(IOSTREAM *s, char *buffer, int size)
{ if ( size == 0 )
    size = SIO_BUFSIZE;

  s->bufsize = size;
  
  if ( s->buffer && !(s->flags & SIO_USERBUF) )
    free(s->buffer);

  if ( buffer )
  { s->buffer = buffer;
    s->flags |= SIO_USERBUF;
  } else
  { if ( !(s->buffer = malloc(s->bufsize)) )
    { errno = ENOMEM;
      return -1;
    }
    s->flags &= ~SIO_USERBUF;
  }

  s->unbuffer = s->buffer;
  s->limitp   = &s->buffer[s->bufsize];
  s->bufp     = s->buffer;

  return size;
}


static int
S__removebuf(IOSTREAM *s)
{ if ( s->buffer )
  { int rval = S__flushbuf(s);

    if ( !(s->flags & SIO_USERBUF) )
      free(s->buffer);
    s->bufp = s->limitp = s->buffer = NULL;
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

  if ( --s->locks == 0 )
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
  }

  SUNLOCK(s);

  return rval;
}


		 /*******************************
		 *	     FLUSH/FILL		*
		 *******************************/

static int
S__flushbuf(IOSTREAM *s)
{ int rval = 0;
  int size;

  while ( (size = s->bufp - s->buffer) > 0 )
  { int n = (*s->functions->write)(s->handle, s->buffer, size);

    if ( n >= 0 )
    { rval += n;
      s->bufp -= n;
    } else
    { s->flags |= SIO_FERR;
      return -1;
    }
  }

  return rval;
}


static int
S__flushbufc(int c, IOSTREAM *s)
{ if ( s->buffer )
  { if ( S__flushbuf(s) < 0 )
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
    goto error;
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
#ifdef WIN32
      FD_SET((SOCKET)fd, &wait);
#else
      FD_SET(fd, &wait);
#endif

      for(;;)
      { rc = select(fd+1, &wait, NULL, NULL, &time);
	
	if ( rc < 0 && errno == EINTR )
	{ if ( PL_handle_signals() < 0 )
	  { errno = EPLEXCEPTION;
	    goto error;
	  }

	  continue;
	}

	break;
      }

      if ( rc == 0 )
      { s->flags |= (SIO_TIMEOUT|SIO_FERR);
	goto error;
      }
    } else
    { errno = EPERM;			/* no permission to select */
      s->flags |= SIO_FERR;
      goto error;
    }
  }
#endif


  if ( s->flags & SIO_NBUF )
  { char chr;
    int n;

    if ( (n=(*s->functions->read)(s->handle, &chr, 1)) == 1 )
    { c = char_to_int(chr);
      goto ok;
    } else if ( n == 0 )
    { if ( !(s->flags & SIO_NOFEOF) )
	s->flags |= SIO_FEOF;
      goto error;
    } else
    { s->flags |= SIO_FERR;
      goto error;			/* error */
    }
  } else
  { int n;

    if ( !s->buffer )
    { if ( S__setbuf(s, NULL, 0) < 0 )
	goto error;
      s->limitp = s->buffer;
    }

    if ( (n=(*s->functions->read)(s->handle, s->buffer, s->bufsize)) > 0 )
    { s->bufp = s->buffer;
      s->limitp = &s->buffer[n];
      c = char_to_int(*s->bufp++);
      goto ok;
    } else
    { if ( n == 0 )
      { if ( !(s->flags & SIO_NOFEOF) )
	  s->flags |= SIO_FEOF;
	goto error;
#ifdef EWOULDBLOCK
      } else if ( errno == EWOULDBLOCK )
      { s->bufp = s->buffer;
	s->limitp = s->buffer;
	goto error;
#endif
      } else
      { s->flags |= SIO_FERR;
	goto error;
      }
    }
  }

error:
  c = -1;
ok:
  return c;
}

		 /*******************************
		 *	   CHARACTER I/O	*
		 *******************************/


inline int
S___fupdatefilepos(IOSTREAM *s, int c)
{ IOPOS *p;

#if 1
  if ( s->magic != SIO_MAGIC )
  { if ( s->magic == SIO_CMAGIC )
      PL_error(NULL, 0, NULL, ERR_CLOSED_STREAM, s);
					/* PL_error() should not return */
    fatalError("Did you load a pre-3.1.2 foreign package?"); 
  }
#endif

  if ( (p = s->position) )
  { switch(c)
    { case '\n':
	p->lineno++;
        p->linepos = 0;
	s->flags &= ~SIO_NOLINEPOS;
#ifdef __WIN32__
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
	return c;
      case '\t':
	p->linepos |= 7;
      default:
	p->linepos++;
    }
  
    p->charno++;
  }

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


int
Sungetc(int c, IOSTREAM *s)
{ if ( s->bufp > s->unbuffer )
  { *--s->bufp = c;
    return c;
  }

  return -1;
}


int
Sputcode(int c, IOSTREAM *s)
{ if ( c < 0 )
  { err:
    Sseterr(s, SIO_FERR|SIO_CLEARERR, "Encoding cannot represent character");
    return -1;
  }

  switch(s->encoding)
  { case ENC_NONE:
    case ENC_ISO_LATIN_1:
      if ( c >= 256 )
	goto err;
    simple:
      if ( s->bufp < s->limitp )
      { *s->bufp++ = (char)c;
      } else
      { if ( S__flushbufc(c, s) < 0 )
	{ s->lastc = EOF;
	  return -1;
	}
      }
      break;
    case ENC_ASCII:
      if ( c >= 128 )
	goto err;
      goto simple;
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
      
      return -1;
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
Sgetcode(IOSTREAM *s)
{ int c;

  switch(s->encoding)
  { case ENC_NONE:
    case ENC_ISO_LATIN_1:
      c = Snpgetc(s);
      break;
    case ENC_ASCII:
    { c = Snpgetc(s);
      if ( c > 128 )
	Sseterr(s, SIO_WARN, "non-ASCII character");
      break;
    }
    case ENC_UTF8:
    { c = Snpgetc(s);
      if ( c == EOF )
	break;

      if ( c & 0x80 )
      { int extra = UTF8_FBN(c);
	int code;

	code = UTF8_FBV(c,extra);
	for( ; extra > 0; extra-- )
	{ int c2 = Snpgetc(s);
	  
	  if ( !ISUTF8_CB(c2) )
	  { Sseterr(s, SIO_WARN, "Illegal UTF-8 Sequence");
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

      c1 = Snpgetc(s);
      c2 = Snpgetc(s);
      if ( c1 == EOF )
	return EOF;

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
    default:
      assert(0);
      c = -1;
  }

out:
  return S__updatefilepos(s, c);
}


int
Sungetcode(int c, IOSTREAM *s)
{ switch(s->encoding)
  { case ENC_NONE:
    case ENC_ISO_LATIN_1:
      if ( c >= 256 )
	return -1;			/* illegal */
    simple:
      if ( s->bufp > s->unbuffer )
      { *--s->bufp = c;
        return c;
      }
      return -1;			/* no room */
    case ENC_ASCII:
      if ( c >= 128 )
	return -1;			/* illegal */
      goto simple;
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
	  { *--s->bufp = *p;
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
      { *--s->bufp = c&0xff;
        *--s->bufp = (c>>8)&0xff;

        return c;
      }
      return -1;
    }
    case ENC_UNICODE_LE:
    { if ( c >= 0x10000 )
	return -1;

      if ( s->bufp-1 > s->unbuffer )
      { *--s->bufp = (c>>8)&0xff;
        *--s->bufp = c&0xff;

        return c;
      }
      return -1;
    }
    case ENC_WCHAR:			/* TBD */
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

int
Sfread(void *data, int size, int elms, IOSTREAM *s)
{ int chars = size * elms;
  char *buf = data;

  for( ; chars > 0; chars-- )
  { int c;

    if ( (c = Sgetc(s)) == EOF )
      break;

    *buf++ = c & 0xff;
  }
  
  return (size*elms - chars)/size;
}


int
Sfwrite(const void *data, int size, int elms, IOSTREAM *s)
{ int chars = size * elms;
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

int
Sread_pending(IOSTREAM *s, char *buf, int limit, int flags)
{ int done = 0;
  int n;

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


		 /*******************************
		 *	      FLUSH		*
		 *******************************/

int
Sflush(IOSTREAM *s)
{ if ( s->buffer && (s->flags & SIO_OUTPUT) )
  { if ( S__flushbuf(s) < 0 )
      return -1;
  }

  return 0;
}

		 /*******************************
		 *	      SEEK		*
		 *******************************/

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
    long end  = Sseek(s, 0, SIO_SEEK_END);

    Sseek(s, here, SIO_SEEK_SET);

    return end;
  }

  errno = ESPIPE;
  return -1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Maybe we should optimise this to become block-aligned?  Or can we leave
this to read/write?

The first part checks whether repositioning   the  read/write pointer in
the buffer suffices to achieve the seek.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

long
Sseek(IOSTREAM *s, long pos, int whence)
{ if ( s->limitp > s->buffer )		/* something there */
  { long now = Stell(s);

    if ( now != -1 )
    { long rval;
      char *nbufp = (char *)-1;
    
      if ( whence == SIO_SEEK_CUR )
      { nbufp = s->bufp + pos;
	rval = now + pos;
      } else if ( whence == SIO_SEEK_SET )
      { nbufp = s->bufp + (pos - now);
	rval = pos;
      } else
	rval = -1;			/* should not happen */

      if ( nbufp >= s->buffer && nbufp < s->limitp )
      { s->bufp = nbufp;

	pos = rval;
	goto update;
      }
    }
  }

  if ( !s->functions->seek )
  { errno = ESPIPE;
    return -1;
  }

  Sflush(s);
    
  s->bufp   = s->buffer;
  s->limitp = s->buffer;

  if ( whence == SIO_SEEK_CUR )
  { pos += Stell(s);
    whence = SIO_SEEK_SET;
  }
  pos = (*s->functions->seek)(s->handle, pos, whence);

update:
  s->flags &= ~SIO_FEOF;		/* not on eof of file anymore */

  if ( s->position )
  { s->flags |= (SIO_NOLINENO|SIO_NOLINEPOS); /* no update this */
    s->position->charno = pos;
  }

  return pos;
}


long
Stell(IOSTREAM *s)
{ if ( s->position )
  { return s->position->charno;
  } else if ( s->functions->seek )
  { long pos = (*s->functions->seek)(s->handle, 0L, SIO_SEEK_CUR);

    if ( s->buffer )			/* open */
    { if ( s->flags & SIO_INPUT )
      { pos -= s->limitp - s->buffer;
	pos += s->bufp - s->buffer;
      } else
      { pos += s->bufp - s->buffer;
      }
    }

    return pos;
  } else
  { errno = EINVAL;
    return -1;
  }
}


		 /*******************************
		 *	      CLOSE		*
		 *******************************/

int
Sclose(IOSTREAM *s)
{ int rval = 0;

  if ( s->magic != SIO_MAGIC )		/* already closed!? */
  { errno = EINVAL;
    return -1;
  }

  SLOCK(s);
  while(s->locks > 0)			/* remove buffer-locks */
    rval = Sunlock(s);

  if ( s->buffer )
  { if ( (s->flags & SIO_OUTPUT) && S__flushbuf(s) < 0 )
      rval = -1;

    if ( !(s->flags & SIO_USERBUF) )
      free(s->buffer);

    s->buffer = NULL;
  }

  s->flags |= SIO_CLOSING;
#ifdef __WIN32__
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
  { if ( Sputc(*q, s) < 0 )
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


#define OUTCHR(s, c)	do { printed++; \
			     if ( Sputc((c), (s)) < 0 ) goto error; \
			   } while(0)
#define valdigit(c)	((c) - '0')
#define A_LEFT	0			/* left-aligned field */
#define A_RIGHT 1			/* right-aligned field */

int
Svfprintf(IOSTREAM *s, const char *fm, va_list args)
{ long printed = 0;
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
	int islong = FALSE;
	int pad = ' ';

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
	{ islong++;
	  fm++;
	}

	switch(*fm)
	{ case 'c':
	    *fe++ = va_arg(args, int);
	    break;
	  case 'd':
	  case 'p':
	  case 'i':
	  case 'o':
	  case 'u':
	  case 'x':
	  case 'X':
	  { long v;
	    char fmbuf[8], *fp=fmbuf;

	    if ( islong )
	      v = va_arg(args, long);
	    else
	      v = va_arg(args, int);

	    *fp++ = '%';
	    if ( modified )
	      *fp++ = '#';
	    *fp++ = 'l';
	    *fp++ = *fm;
	    *fp   = '\0';
	    sprintf(fs, fmbuf, v);
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
	    { OUTCHR(s, *fs++);
	      w++;
	    }
	    while(w < arg1)
	    { OUTCHR(s, pad);
	      w++;
	    }
	  } else /*if ( align == A_RIGHT ) */
	  { int w;

	    if ( fs == fbuf )
	      w = fe - fs;
	    else
	      w = strlen(fs);

	    w = arg1 - w;
	    while(w > 0 )
	    { OUTCHR(s, pad);
	      w--;
	    }
	    while(*fs)
	      OUTCHR(s, *fs++);
	  }
	} else
	{ if ( fs == fbuf )		/* unaligned field, just output */
	  { while(fs < fe)
	      OUTCHR(s, *fs++);
	  } else
	  { while(*fs)
	      OUTCHR(s, *fs++);
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
  return printed;

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

  s.bufp      = buf;
  s.limitp    = (char *)(~0L);
  s.buffer    = buf;
  s.flags     = SIO_FBUF|SIO_OUTPUT;
  s.position  = NULL;
  s.handle    = NULL;
  s.functions = NULL;
  s.mutex     = NULL;
  
  if ( (rval = Svfprintf(&s, fm, args)) >= 0 )
    *s.bufp = '\0';

  return rval;
}


int
Svdprintf(const char *fm, va_list args)
{ int rval;
  IOSTREAM *s = Soutput;

  SLOCK(s);
  rval = Svfprintf(s, fm, args);
#if defined(_DEBUG) && defined(WIN32)
  Sputc('\0', s);
  s->bufp--;				/* `Unput' */
  OutputDebugString(s->buffer);
#endif
  Sflush(s);
  SUNLOCK(s);

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
      { { long v;			/* collect value here */
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
	      { long *vp = va_arg(args, long *);
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
	    { long *vp = va_arg(args, long *);
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
		 *	    FILE STREAMS	*
		 *******************************/

static int
Sread_file(void *handle, char *buf, int size)
{ long h = (long) handle;
  int bytes;

  for(;;)
  { bytes = read((int)h, buf, size);

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


static int
Swrite_file(void *handle, char *buf, int size)
{ long h = (long) handle;
  int bytes;

  for(;;)
  { bytes = write((int)h, buf, size);

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
{ long h = (long) handle;

					/* cannot do EINTR according to man */
  return lseek((int)h, pos, whence);
}


static int
Sclose_file(void *handle)
{ long h = (long) handle;
  int rc;

  do
  { rc = close((int) h);
  }  while ( rc == -1 && errno == EINTR );

  return rc;
}


static int
Scontrol_file(void *handle, int action, void *arg)
{ long h = (long) handle;
  int fd = (int)h;

  switch(action)
  { case SIO_GETSIZE:
    { long *rval = arg;
      struct stat buf;

      if ( fstat(fd, &buf) == 0 )
      {	*rval = buf.st_size;
        return 0;
      }
      return -1;
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
  Scontrol_file
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


IOSTREAM *
Sopen_file(const char *path, const char *how)
{ int fd;
  int oflags = 0, flags = SIO_FILE|SIO_TEXT|SIO_RECORDPOS;
  int op = *how++;
  long lfd;
  enum {lnone=0,lread,lwrite} lock = lnone;
  IOSTREAM *s;

  for( ; *how; how++)
  { switch(*how)
    { case 'b':				/* binary */
	flags &= ~SIO_TEXT;
        oflags = O_BINARY;
        break;
      case 'r':				/* no record */
	flags &= SIO_RECORDPOS;
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
  oflags |= O_LARGEFILE
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
#if __WIN32__
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

  lfd = (long)fd;
  s = Snew((void *)lfd, flags, &Sfilefunctions);
  if ( lock )
    s->flags |= SIO_ADVLOCK;

  return s;
}


IOSTREAM *
Sfdopen(int fd, const char *type)
{ int flags;
  long lfd;

  if ( fd < 0 )
    return NULL;

  if ( *type == 'r' )
    flags = SIO_FILE|SIO_INPUT|SIO_RECORDPOS;
  else
    flags = SIO_FILE|SIO_OUTPUT|SIO_RECORDPOS;

  lfd = (long)fd;

  return Snew((void *)lfd, flags, &Sfilefunctions);
}

/* MT: as long as s is valid, this should be ok
*/

int
Sfileno(IOSTREAM *s)
{ int n;

  if ( s->flags & SIO_FILE )
  { long h = (long)s->handle;
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


#define STDIO(n, f) { NULL, NULL, NULL, NULL, \
		      EOF, SIO_MAGIC, 0, f, {0, 0, 0}, NULL, \
		      ((void *)(n)), &Sfilefunctions, \
		      0, NULL, \
		      (void (*)(void *))0, NULL, \
		      -1, \
		      0, \
		      ENC_ISO_LATIN_1 \
		    }

#define SIO_STDIO (SIO_FILE|SIO_STATIC|SIO_NOCLOSE|SIO_ISATTY)
#define STDIO_STREAMS \
  STDIO(0, SIO_STDIO|SIO_LBUF|SIO_INPUT|SIO_NOFEOF),	/* Sinput */ \
  STDIO(1, SIO_STDIO|SIO_LBUF|SIO_OUTPUT), 		/* Soutput */ \
  STDIO(2, SIO_STDIO|SIO_NBUF|SIO_OUTPUT)		/* Serror */


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

    for(i=0; i<=2; i++)
    { if ( !isatty(i) )
	S__iob[i].flags &= ~SIO_ISATTY;
#ifdef O_PLMT
      S__iob[i].mutex = malloc(sizeof(recursiveMutex));
      recursiveMutexInit(S__iob[i].mutex);
#endif
    }
  }
}


IOSTREAM *
S__getiob()
{ return S__iob;
}

		 /*******************************
		 *	       PIPES		*
		 *******************************/

#ifdef HAVE_POPEN
#ifdef WIN32
#include "popen.c"

#define popen(cmd, how) pt_popen(cmd, how)
#define pclose(fd)	pt_pclose(fd)
#endif

static int
Sread_pipe(void *handle, char *buf, int size)
{ FILE *fp = handle;

  return read(fileno(fp), buf, size);
}


static int
Swrite_pipe(void *handle, char *buf, int size)
{ FILE *fp = handle;

  return write(fileno(fp), buf, size);
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
{ FILE *fd = popen(command, type);	/* HACK for now */

#if 0
  Sdprintf("Opening \"%s\", mode \"%s\" --> %p (%d)\n",
	   command, type, fd, errno);
#endif

  if ( fd )
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
{ long	here;				/* `here' location */
  long  size;				/* size of buffer */
  int  *sizep;				/* pointer to size */
  long	allocated;			/* allocated size */
  char **buffer;			/* allocated buffer */
  int	malloced;			/* malloc() maintained */
} memfile;


void
Sfree(void *ptr)			/* Windows: must free from same */
{ free(ptr);				/* DLL */
}


static long
S__memfile_nextsize(long needed)
{ long size = 512;

  while ( size < needed )
    size *= 2;

  return size;
}


static int
Swrite_memfile(void *handle, char *buf, int size)
{ memfile *mf = handle;

  if ( mf->here + size + 1 >= mf->allocated )
  { long ns = S__memfile_nextsize(mf->here + size + 1);
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


static int
Sread_memfile(void *handle, char *buf, int size)
{ memfile *mf = handle;

  if ( size + mf->here > mf->size )
  { size = mf->size - mf->here;
    if ( size < 0 )
      size = 0;
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
Sopenmem(char **buffer, int *size, const char* mode)
    Open a memory area as a stream.  Output streams will automatically
    resized using realloc() if *size = 0 or the stream is opened with mode
    "wa".

    If the buffer is allocated or enlarged, this is achieved using malloc()
    or realloc().  In this case the returned buffer should be freed by the
    caller when done.  Example:

    { char buf[1024];			(don't allocate for small stuff)
      char *s = buf;
      IOSTREAM *fd;
      int size = sizeof(buf);

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
Sopenmem(char **buffer, int *sizep, const char *mode)
{ memfile *mf = malloc(sizeof(memfile));
  int flags = SIO_FBUF|SIO_RECORDPOS|SIO_NOMUTEX;
  int size;

  if ( !mf )
  { errno = ENOMEM;
    return NULL;
  }

  mf->malloced = FALSE;

  switch(*mode)
  { case 'r':
      flags |= SIO_INPUT;
      if ( sizep == NULL || *sizep < 0 )
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

static int
Sread_string(void *handle, char *buf, int size)
{ return 0;				/* signal EOF */
}

static int
Swrite_string(void *handle, char *buf, int size)
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
Sopen_string(IOSTREAM *s, char *buf, int size, const char *mode)
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
      if ( size < 0 )
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

#undef S__fupdatefilepos

int
S__fupdatefilepos(IOSTREAM *s, int c)
{ return S___fupdatefilepos(s, c);
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
Scleanup(void)
{ close_hook *p, *next;
  int i;

  for(p=close_hooks; p; p=next)
  { next = p->next;
    free(p);
  }

  close_hooks = NULL;

  for(i=0; i<=2; i++)
  { if ( S__iob[i].buffer )
      free(S__iob[i].buffer);
    S__iob[i] = S__iob0[i];
  }
}
