/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: SWI-Prolog IO streams
*/

#ifdef __WIN32__
#include <uxnt.h>
#define MD "config/win32.h"
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
#include "config.h"
#endif

#define PL_KERNEL 1
#include "pl-stream.h"
#include <sys/types.h>
#include <errno.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <memory.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/stat.h>
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
#define SLOCK(s)   if ( s->mutex ) pthread_mutex_lock(s->mutex)
#define SUNLOCK(s) if ( s->mutex ) pthread_mutex_unlock(s->mutex)
#else
#define SLOCK(s)
#define SUNLOCK(s)
#endif



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


int
Slock(IOSTREAM *s)
{ SLOCK(s);

  if ( s->locks )
    s->locks++;
  else if ( (s->flags & (SIO_NBUF|SIO_OUTPUT)) == (SIO_NBUF|SIO_OUTPUT) )
  { s->locks = 1;
    return S__setbuf(s, NULL, TMPBUFSIZE);
  }

  return 0;
}


int
Sunlock(IOSTREAM *s)
{ SUNLOCK(s);

  if ( s->locks )
  { if ( --s->locks == 0 )
    { if ( (s->flags & (SIO_NBUF|SIO_OUTPUT)) == (SIO_NBUF|SIO_OUTPUT) )
	return S__removebuf(s);
    }
  }

  return 0;
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


#define ReadF(s) (s)->functions->read

int
S__fillbuf(IOSTREAM *s)
{ int c;

  if ( s->flags & (SIO_FEOF|SIO_FERR) )
  { s->flags |= SIO_FEOF2;		/* reading past eof */
    goto error;
  }

  if ( s->flags & SIO_NBUF )
  { char chr;
    int n;

    if ( (n=(*ReadF(s))(s->handle, &chr, 1)) == 1 )
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

    if ( (n=(*ReadF(s))(s->handle, s->buffer, s->bufsize)) > 0 )
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
#include "pl-error.h"
  extern int fatalError(const char *fm, ...);
  extern int PL_error(const char *pred,
		      int arity, const char *msg, int id, ...);

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

int
Sputc(int c, IOSTREAM *s)
{ c &= 0xff;

  if ( s->bufp < s->limitp )
    *s->bufp++ = (char)c;
  else
  { if ( S__flushbufc(c, s) < 0 )
    { s->lastc = EOF;
      return -1;
    }
  }
  s->lastc = c;

  if ( c == '\n' && (s->flags & SIO_LBUF) )
  { if ( S__flushbuf(s) < 0 )
      return -1;
  }

  return S__updatefilepos(s, c);
}


int
Sfgetc(IOSTREAM *s)
{ int c;

  if ( s->bufp < s->limitp )
    c = (int) *s->bufp++ & 0xff;
  else
    c = S__fillbuf(s);

  return S__updatefilepos(s, c);
}


int
Sungetc(int c, IOSTREAM *s)
{ if ( s->bufp > s->unbuffer )
  { *--s->bufp = c;
    return c;
  }

  return -1;				/* no room */
}

		 /*******************************
		 *	    PUTW/GETW		*
		 *******************************/

int
Sputw(int w, IOSTREAM *s)
{ unsigned char *q = (unsigned char *)&w;
  int n;

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
  int n;

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
Sfwrite(void *data, int size, int elms, IOSTREAM *s)
{ int chars = size * elms;
  char *buf = data;

  for( ; chars > 0; chars-- )
  { if ( Sputc(*buf++, s) < 0 )
      break;
  }
  
  return (size*elms - chars)/size;
}


		 /*******************************
		 *               FLAGS		*
		 *******************************/

int
Sfeof(IOSTREAM *s)
{ return (s->flags & SIO_FEOF) != 0;
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
{ s->flags &= ~(SIO_FEOF|SIO_FERR|SIO_FEOF2);
}


		 /*******************************
		 *	      FLUSH		*
		 *******************************/

int
Sflush(IOSTREAM *s)
{ if ( s->buffer && (s->flags & SIO_OUTPUT) )
    return S__flushbuf(s);

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
  run_close_hooks(s);

  if ( s->buffer )
  { if ( (s->flags & SIO_OUTPUT) && S__flushbuf(s) < 0 )
      rval = -1;

    if ( !(s->flags & SIO_USERBUF) )
      free(s->buffer);

    s->buffer = NULL;
  }

  if ( s->functions->close && (*s->functions->close)(s->handle) < 0 )
    rval = -1;
  SUNLOCK(s);

#ifdef O_PLMT
  if ( s->mutex )
    free(s->mutex);
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


#define OUT(s, c)	do { printed++; \
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
      { OUT(s, *fm);
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
	    break;
	}

	if ( has_arg1 )			/* aligned field */
	{ if ( fs == fbuf )
	    *fe = '\0';

	  if ( align == A_LEFT )
	  { int w = 0;
	    while(*fs)
	    { OUT(s, *fs++);
	      w++;
	    }
	    while(w < arg1)
	    { OUT(s, pad);
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
	    { OUT(s, pad);
	      w--;
	    }
	    while(*fs)
	      OUT(s, *fs++);
	  }
	} else
	{ if ( fs == fbuf )		/* unaligned field, just output */
	  { while(fs < fe)
	      OUT(s, *fs++);
	  } else
	  { while(*fs)
	      OUT(s, *fs++);
	  }
	}
	fm++;
      }
    } else
    { OUT(s, *fm);
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
  
  if ( (rval = Svfprintf(&s, fm, args)) >= 0 )
    *s.bufp = '\0';

  return rval;
}


int
Svdprintf(const char *fm, va_list args)
{ int rval;

  SLOCK(Soutput);
  rval = Svfprintf(Soutput, fm, args);
  Sflush(Soutput);
  SUNLOCK(Soutput);

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

  return read((int)h, buf, size);
}


static int
Swrite_file(void *handle, char *buf, int size)
{ long h = (long) handle;

  return write((int)h, buf, size);
}


static long
Sseek_file(void *handle, long pos, int whence)
{ long h = (long) handle;

  return lseek((int)h, pos, whence);
}


static int
Sclose_file(void *handle)
{ long h = (long) handle;

  return close((int) h);
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

  if ( !(s = malloc(sizeof(IOSTREAM))) )
  { errno = ENOMEM;
    return NULL;
  }
  memset((char *)s, 0, sizeof(IOSTREAM));
  s->magic     = SIO_MAGIC;
  s->lastc     = EOF;
  s->flags     = flags;
  s->handle    = handle;
  s->functions = functions;
  if ( flags & SIO_RECORDPOS )
  { s->position = &s->posbuf;
    s->posbuf.lineno = 1;
  }
#ifdef O_PLMT
  if ( !(s->mutex = malloc(sizeof(IOLOCK))) )
  { errno = ENOMEM;
    return NULL;
  }
  s->mutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#endif

  return s;
}


#ifndef O_BINARY
#define O_BINARY 0
#endif

IOSTREAM *
Sopen_file(const char *path, const char *how)
{ int fd;
  int oflags = 0, flags = SIO_FILE|SIO_TEXT|SIO_RECORDPOS;
  int op = *how++;
  long lfd;

  for( ; *how; how++)
  { switch(*how)
    { case 'b':				/* binary */
	flags &= ~SIO_TEXT;
        oflags = O_BINARY;
        break;
      case 'r':				/* no record */
	flags &= SIO_RECORDPOS;
        break;
      default:
	errno = EINVAL;
        return NULL;
    }
  }

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

  lfd = (long)fd;
  return Snew((void *)lfd, flags, &Sfilefunctions);
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


#ifdef O_PLMT
static IOLOCK S__locks[] =
{ PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP,
  PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP,
  PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
};
#define MUTEX(n) &S__locks[n]
#else
#define MUTEX(n) NULL
#endif

#define STDIO(n, f) { NULL, NULL, NULL, NULL, \
		      EOF, SIO_MAGIC, 0, f, {0, 0, 0}, NULL, \
		      ((void *)(n)), &Sfilefunctions, \
		      0, MUTEX(n) \
		    }

IOSTREAM S__iob[] =
{ 
#define SIO_STDIO (SIO_FILE|SIO_STATIC|SIO_NOCLOSE)

  STDIO(0, SIO_STDIO|SIO_LBUF|SIO_INPUT|SIO_NOFEOF),	/* Sinput */
  STDIO(1, SIO_STDIO|SIO_LBUF|SIO_OUTPUT), 		/* Soutput */
  STDIO(2, SIO_STDIO|SIO_NBUF|SIO_OUTPUT)		/* Serror */
};


IOSTREAM *
S__getiob()
{ return S__iob;
}

		 /*******************************
		 *	       PIPES		*
		 *******************************/

#ifdef HAVE_POPEN

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
  NULL,
  Sclose_pipe
};


IOSTREAM *
Sopen_pipe(const char *command, const char *type)
{ FILE *fd = popen(command, type);	/* HACK for now */

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


static int
S__memfile_nextsize(int needed)
{ needed += needed/4;

  return (needed + 255) & ~255;
}


static int
Swrite_memfile(void *handle, char *buf, int size)
{ memfile *mf = handle;

  if ( mf->here + size + 1 >= mf->allocated )
  { long ns = S__memfile_nextsize(mf->here + size);
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
    Open an memory area as a stream.  Output streams will automatically
    resized using realloc() if *size = 0 or the stream is opened with mode
    "wa".
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

IOSTREAM *
Sopenmem(char **buffer, int *sizep, const char *mode)
{ memfile *mf = malloc(sizeof(memfile));
  int flags = SIO_FBUF;
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
  NULL,
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

  s->buffer    = buf;
  s->bufp      = buf;
  s->unbuffer  = buf;
  s->position  = NULL;
  s->handle    = s;			/* for Sclose_string() */
  s->functions = &Sstringfunctions;

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
