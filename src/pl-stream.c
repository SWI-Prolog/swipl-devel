/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: SWI-Prolog IO streams
*/

#include "pl-stream.h"
#include <errno.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif HAVE_MALLOC_H
#include <memory.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <unistd.h>
#include <stdio.h>			/* sprintf() for numeric values */
#include <assert.h>

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

int Slinesize = SIO_LINESIZE;		/* Sgets() buffer size */

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

  return s->bufsize;
}

		 /*******************************
		 *	     FLUSH/FILL		*
		 *******************************/

static int
S__flushbuf(IOSTREAM *s)
{ int size = s->bufp - s->buffer;

  if ( (*s->functions->write)(s->handle, s->buffer, size) != size )
    return -1;

  s->bufp = s->buffer;

  return size;
}


static int
S__flushbufc(int c, IOSTREAM *s)
{ if ( s->flags & SIO_NBUF )
  { char chr = (char)c;
    
    if ( (*s->functions->write)(s->handle, &chr, 1) != 1 )
      return -1;
  } else
  { if ( !s->buffer )
    { if ( S__setbuf(s, NULL, 0) < 0 )
	return -1;
    } else
    { if ( S__flushbuf(s) < 0 )
	return -1;
    }
    *s->bufp++ = (char)c;
  }

  return c;
}

static int
S__fillbuf(IOSTREAM *s)
{ if ( s->flags & (SIO_FEOF|SIO_FERR) )
    return -1;

  if ( s->flags & SIO_NBUF )
  { char chr;
    int n;

    if ( (n=(*s->functions->read)(s->handle, &chr, 1)) == 1 )
    { return (int) chr;
    } else if ( n == 0 )
    { s->flags |= SIO_FEOF;
      return EOF;
    } else
    { s->flags |= SIO_FERR;
      return -1;			/* error */
    }
  } else
  { int n;

    if ( !s->buffer )
    { if ( S__setbuf(s, NULL, 0) < 0 )
	return -1;
    }

    if ( (n=(*s->functions->read)(s->handle, s->buffer, s->bufsize)) > 0 )
    { s->bufp = s->buffer;
      s->limitp = &s->buffer[n];
      return (int) *s->bufp++;
    } else
    { s->bufp = s->buffer;		/* empty the buffer */
      s->limitp = s->buffer;

      if ( n == 0 )
      { s->flags |= SIO_FEOF;
	return EOF;
      } else
      { s->flags |= SIO_FERR;
	return -1;
      }
    }
  }
}

		 /*******************************
		 *	   CHARACTER I/O	*
		 *******************************/


static void
S__updatefilepos(IOPOS *p, int c)
{ switch(c)
  { case '\n':
      p->lineno++;
      p->linepos = 0;
/*    p->flags &= ~SIO_NOLINEPOS; TBD*/
      break;
    case '\b':
      if ( p->linepos > 0 )
	p->linepos--;
      break;
    case EOF:
      return;
    case '\t':
      p->linepos |= 7;
    default:
      p->linepos++;
  }

  p->charno++;
}


int
Sputc(int c, IOSTREAM *s)
{ if ( s->bufp < s->limitp )
    *s->bufp++ = (char)c;
  else
  { if ( S__flushbufc(c, s) < 0 )
      return -1;
  }

  if ( c == '\n' && (s->flags & SIO_LBUF) )
  { if ( S__flushbuf(s) < 0 )
      return -1;
  }

  if ( s->position )
    S__updatefilepos(s->position, c);

  return c;
}


int
Sgetc(IOSTREAM *s)
{ int c;

  if ( s->bufp < s->limitp )
    c = *s->bufp++;
  else
    c = S__fillbuf(s);

  if ( s->position )
    S__updatefilepos(s->position, c);

  return c;
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
{ char *q = (char *)&w;
  int n;

  for(n=0; n<sizeof(w); n++)
    if ( Sputc(*q++, s) < 0 )
      return -1;

  return w;
}


int
Sgetw(IOSTREAM *s)
{ int w;
  char *q = (char *)&w;
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
    

void
Sclearerr(IOSTREAM *s)
{ s->flags &= ~(SIO_FEOF|SIO_FERR);
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
Maybe we should optimise this to become block-aligned?  Or can we leave
this to read/write?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

long
Sseek(IOSTREAM *s, long pos, int whence)
{ if ( !s->functions->seek )
  { errno = ESPIPE;
    return -1;
  }

  Sflush(s);
    
  s->bufp   = s->buffer;
  s->limitp = s->buffer;
  s->flags &= ~SIO_FEOF;		/* not on eof of file anymore */

  pos = (*s->functions->seek)(s->handle, pos, whence);

  if ( s->position )
  { s->flags |= (SIO_NOLINENO|SIO_NOLINEPOS); /* no update this */
    s->position->charno = pos;
  }

  return pos;
}


long
Stell(IOSTREAM *s)
{ if ( s->position )
    return s->position->charno;
  else if ( s->functions->seek )
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

  if ( s->buffer )
  { if ( S__flushbuf(s) < 0 )
      rval = -1;

    if ( !(s->flags & SIO_USERBUF) )
      free(s->buffer);

    s->buffer = NULL;
  }

  if ( s->functions->close && (*s->functions->close)(s->handle) < 0 )
    rval = -1;

  s->magic = 0;
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
      if ( q > buf )
	return buf;
      else
	return NULL;
    } else
    { *q++ = c;
      if ( c == '\n' )
      { if ( n > 0 )
	  *q = '\0';
	return buf;
      }
    }
  }

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
{ for( ; *q; q++)
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
			     if ( Sputc((c), (s)) < 0 ) return -1; \
			   } while(0)
#define valdigit(c)	((c) - '0')
#define A_LEFT	0			/* left-aligned field */
#define A_RIGHT 1			/* right-aligned field */

int
Svfprintf(IOSTREAM *s, const char *fm, va_list args)
{ long printed = 0;

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
	int arg1, arg2;
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
	} else if ( isdigit(*fm) )
	{ if ( *fm == '0' )
	    pad = '0';
	  arg1 = valdigit(*fm);
	  has_arg1++;
	  for( fm++; isdigit(*fm); fm++)
	    arg1 = arg1*10 + valdigit(*fm);
	}
	if ( *fm == '.' )
	{ has_arg2++;
	  fm++;
	  if ( *fm == '*' )
	  { arg2 = va_arg(args, int);
	  } else
	  { arg2 = 0;
	    for( ; isdigit(*fm); fm++)
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

  return printed;
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

  s.bufp      = buf;
  s.limitp    = (char *)0xffffffff;
  s.buffer    = buf;
/*s.unbuffer  = buf;			Won't be used anyway
  s.magic     = SIO_MAGIC;
  s.bufsize   = 0x7fffffff; */
  s.flags     = SIO_FBUF|SIO_OUTPUT;
  s.position  = NULL;
  s.handle    = NULL;
  s.functions = NULL;
  
  return Svfprintf(&s, fm, args);
}



		 /*******************************
		 *	    FILE STREAMS	*
		 *******************************/

static int
Sread_file(void *handle, char *buf, int size)
{ return read((int)handle, buf, size);
}


static int
Swrite_file(void *handle, char *buf, int size)
{ return write((int)handle, buf, size);
}


static long
Sseek_file(void *handle, long pos, int whence)
{ return lseek((int)handle, pos, whence);
}


static int
Sclose_file(void *handle)
{ return close((int) handle);
}


IOFUNCTIONS Sfilefunctions =
{ Sread_file,
  Swrite_file,
  Sseek_file,
  Sclose_file
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
  s->flags     = flags;
  s->handle    = handle;
  s->functions = functions;
  if ( flags & SIO_RECORDPOS )
  { s->position = &s->posbuf;
    s->posbuf.lineno = 1;
  }

  return s;
}


IOSTREAM *
Sopen_file(char *path, char *how)
{ int fd;
  int flags;

  switch(*how)
  { case 'w':
      fd = open(path, O_WRONLY|O_CREAT|O_TRUNC, 0666);
      flags = SIO_FILE|SIO_OUTPUT|SIO_RECORDPOS;
      break;
    case 'a':
      fd = open(path, O_WRONLY|O_CREAT|O_APPEND, 0666);
      flags = SIO_FILE|SIO_OUTPUT|SIO_RECORDPOS;
      break;
    case 'r':
      fd = open(path, O_RDONLY);
      flags = SIO_FILE|SIO_INPUT|SIO_RECORDPOS;
      break;
    default:
      errno = EINVAL;
      return NULL;
  }

  if ( fd < 0 )
    return NULL;

  return Snew((void *)fd, flags, &Sfilefunctions);
}


IOSTREAM *
Sfdopen(int fd, char *type)
{ int flags;

  if ( fd < 0 )
    return NULL;

  if ( *type == 'r' )
    flags = SIO_FILE|SIO_INPUT;
  else
    flags = SIO_FILE|SIO_OUTPUT;

  return Snew((void *)fd, flags, &Sfilefunctions);
}


int
Sfileno(IOSTREAM *s)
{ if ( s->flags & SIO_FILE )
    return (int)s->handle;
  if ( s->flags & SIO_PIPE )
    return fileno((FILE *)s->handle);

  errno = EINVAL;
  return -1;				/* no file stream */
}


#define STDIO(n, f) { NULL, NULL, NULL, NULL, \
		      SIO_MAGIC, 0, f, {0, 0, 0}, NULL, \
		      ((void *)(n)), &Sfilefunctions \
		    }

IOSTREAM S__iob[] =
{ STDIO(0, SIO_FILE|SIO_INPUT|SIO_LBUF|SIO_STATIC),  /* Sinput */
  STDIO(1, SIO_FILE|SIO_OUTPUT|SIO_LBUF|SIO_STATIC), /* Soutput */
  STDIO(2, SIO_FILE|SIO_OUTPUT|SIO_NBUF|SIO_STATIC)  /* Serror */
};

		 /*******************************
		 *	       PIPES		*
		 *******************************/

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


		 /*******************************
		 *	  MEMORY STREAMS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Memory streams form a replacement for   sprintf(), sscanf() and friends.
They allow regarding a piece of  (for output) malloc() maintained memory
to serve as a temporary buffer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ long	here;				/* `here' location */
  long  size;				/* size of buffer */
  int  *sizep;				/* pointer to size */
  long	allocated;			/* allocated size */
  char *buffer;				/* allocated buffer */
  int	malloced;			/* malloc() maintained */
} memfile;


static int
S__memfile_nextsize(int needed)
{ needed += needed/4;

  return (needed + 255)/256;
}


static int
Swrite_memfile(void *handle, char *buf, int size)
{ memfile *mf = handle;

  if ( mf->here + size >= mf->allocated )
  { if ( mf->malloced )
    { long ns = S__memfile_nextsize(mf->here + size);
      char *nb;

      if ( mf->allocated )
	nb = malloc(ns);
      else
	nb = realloc(mf->buffer, ns);

      if ( !nb )
      { errno = ENOMEM;
	return -1;
      }

      mf->allocated = ns;
      mf->buffer = nb;
    } else
    { errno = ENOMEM;
      return -1;
    }
  }

  memcpy(&mf->buffer[mf->here], buf, size);

  if ( mf->here + size > mf->size )
  { mf->size = mf->here + size;
    if ( mf->sizep )			/* make externally known */
      *mf->sizep = mf->size;
    mf->buffer[mf->size] = '\0';
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
  
  memcpy(buf, &mf->buffer[mf->here], size);
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
Sopenmem(char **buffer, int *size, char* mode)
    Open an memory area as a stream.  Output streams will automatically
    resized using realloc() of *size = 0 or the stream is opened with mode
    "wa".
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

IOSTREAM *
Sopenmem(char **buffer, int *sizep, char *mode)
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
  mf->buffer    = *buffer;

  return Snew(mf, flags, &Smemfunctions);
}

		 /*******************************
		 *	      STRINGS		*
		 *******************************/

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
Sopen_string(IOSTREAM *s, char *buf, int size, char *mode)
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

  return s;
}
