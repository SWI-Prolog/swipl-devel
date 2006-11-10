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

#define _ISOC99_SOURCE 1		/* fwprintf(), etc prototypes */

#define UTIL_H_IMPLEMENTATION
#include "util.h"
#include <ctype.h>
#include <wctype.h>
#include <stdlib.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>
#include "utf8.h"

size_t
istrlen(const ichar *s)
{ size_t len =0;
  
  while(*s++)
    len++;

  return len;
}


ichar *
istrdup(const ichar *s)
{ if ( s )
  { ichar *dup = sgml_malloc((istrlen(s)+1)*sizeof(ichar));
    ichar *d = dup;

    while(*s)
      *d++ = *s++;
    *d = 0;
    
    return dup;
  } else
  { return NULL;
  }
}


ichar *
istrndup(const ichar *s, int len)
{ ichar *dup = sgml_malloc((len+1)*sizeof(ichar));
  ichar *d = dup;

  while(--len >= 0)
    *d++ = *s++;
  *d = 0;

  return dup;
}


ichar *
istrcpy(ichar *d, const ichar *s)
{ ichar *r = d;

  while(*s)
    *d++ = *s++;
  *d = 0;

  return r;
}


ichar *
istrcat(ichar *d, const ichar *s)
{ ichar *r = d;

  d += istrlen(d);
  istrcpy(d, s);

  return r;
}


ichar *
istrncpy(ichar *d, const ichar *s, size_t len)
{ ichar *r = d;

  while(*s && len-- > 0)
    *d++ = *s++;

  return r;
}



int
istrcaseeq(const ichar *s1, const ichar *s2)
{ ichar c;

  while ((c = *s1++) != '\0')
  { if (towlower(*s2++) != towlower(c))
      return FALSE;
  }

  return *s2 == '\0';
}


int
istreq(const ichar *s1, const ichar *s2)
{ while(*s1 && *s1 == *s2)
    s1++, s2++;
  
  if ( *s1 == 0 && *s2 == 0 )
    return TRUE;
  
  return FALSE;
}


int
istrncaseeq(const ichar *s1, const ichar *s2, int len)
{ while(--len >= 0 && towlower(*s1) == towlower(*s2))
    s1++, s2++;
  
  if ( len < 0 )
    return TRUE;
  
  return FALSE;
}


int
istrprefix(const ichar *pref, const ichar *s)
{ while(*pref && *pref == *s)
    pref++, s++;
  
  if ( *pref == 0 )
    return TRUE;
  
  return FALSE;
}


ichar *
istrchr(const ichar *s, int c)
{ for( ; *s; s++ )
  { if ( c == *s )
      return (ichar *)s;
  }

  return NULL;
}


ichar *
istrupper(ichar *s)
{ ichar *r = s;

  for( ; *s; s++)
    *s = toupper(*s);

  return r;
}


ichar *
istrlower(ichar *s)
{ ichar *r = s;

  for( ; *s; s++)
    *s = towlower(*s);

  return r;
}


int
istrhash(const ichar *t, int tsize)
{ unsigned int value = 0;
  unsigned int shift = 5;

  while(*t)
  { unsigned int c = *t++;
    
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  value = value ^ (value >> 16);

  return value % tsize;
}


int
istrcasehash(const ichar *t, int tsize)
{ unsigned int value = 0;
  unsigned int shift = 5;

  while(*t)
  { unsigned int c = towlower(*t++);	/* case insensitive */
    
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  value = value ^ (value >> 16);

  return value % tsize;
}


int
istrtol(const ichar *s, long *val)
{ long v;
  ichar *e;

  if ( *s )
  { v = wcstol(s, &e, 10);
    if ( !e[0] && errno != ERANGE )
    { *val = v;
      return TRUE;
    }
  }

  return FALSE;
}



		 /*******************************
		 *    INPUT CHARACTER BUFFER	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Input character buffer is used to collect data between SGML markup, such
as <...>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

icharbuf *
new_icharbuf()
{ icharbuf *buf = sgml_malloc(sizeof(*buf));

  buf->allocated = 0;
  buf->size = 0;
  buf->data = NULL;

  return buf;
}


void
free_icharbuf(icharbuf *buf)
{ if ( buf->data )
    sgml_free(buf->data);

  sgml_free(buf);
}


void
__add_icharbuf(icharbuf *buf, int chr)
{ if ( buf->size == buf->allocated )
  { buf->allocated = (buf->allocated ? buf->allocated*2 : 128);

    if ( buf->data )
      buf->data = sgml_realloc(buf->data, buf->allocated*sizeof(ichar));
    else
      buf->data = sgml_malloc(buf->allocated*sizeof(ichar));
  }
  
  buf->data[buf->size++] = chr;
}


void
del_icharbuf(icharbuf *buf)
{ if ( buf->size > 0 )
    buf->size--;
}


void
terminate_icharbuf(icharbuf *buf)
{ add_icharbuf(buf, '\0');
  buf->size--;
}


void
empty_icharbuf(icharbuf *buf)
{ buf->size = 0;
}


		 /*******************************
		 *    OUTPUT CHARACTER BUFFER	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Output character buffer deals with two  representations: ISO Latin-1 and
UCS. It starts life as ISO Latin-1 and   is upgraded to UCS as the first
character that doesn't fit ISO Latin-1 is added to the buffer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ocharbuf *
init_ocharbuf(ocharbuf *buf)
{ buf->size      = 0;
  buf->allocated = sizeof(buf->localbuf)/sizeof(wchar_t);
  buf->data.w    = buf->localbuf;

  return buf;
}


ocharbuf *
new_ocharbuf()
{ ocharbuf *buf = sgml_malloc(sizeof(*buf));
  
  return init_ocharbuf(buf);
}


void
free_ocharbuf(ocharbuf *buf)
{ if ( buf->data.w && buf->data.w != buf->localbuf )
    sgml_free(buf->data.w);

  sgml_free(buf);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Make sure the data of the buffer is malloc'ed and nul-terminated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ocharbuf *
malloc_ocharbuf(ocharbuf *buf)
{ if ( buf->data.w == buf->localbuf )
  { int bytes = (buf->size+1) * sizeof(wchar_t);

    buf->data.w = sgml_malloc(bytes);
    memcpy(buf->data.w, buf->localbuf, bytes);
    buf->data.w[buf->size] = 0;
  } else
    terminate_ocharbuf(buf);

  return buf;
}


void
add_ocharbuf(ocharbuf *buf, int chr)
{ if ( buf->size == buf->allocated )
  { buf->allocated *= 2;

    if ( buf->data.w != (wchar_t*)buf->localbuf )
    { buf->data.w = sgml_realloc(buf->data.w, buf->allocated*sizeof(wchar_t));
    } else
    { buf->data.w = sgml_malloc(buf->allocated*sizeof(wchar_t));
      memcpy(buf->data.w, buf->localbuf, sizeof(buf->localbuf));
    }
  }
  buf->data.w[buf->size++] = chr;
}


void
del_ocharbuf(ocharbuf *buf)
{ if ( buf->size > 0 )
    buf->size--;
}


void
terminate_ocharbuf(ocharbuf *buf)
{ add_ocharbuf(buf, '\0');
  buf->size--;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
empty_ocharbuf() frees the associated buffer after   a big lump has been
in it. Otherwise it simply sets  the  size   to  0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
empty_ocharbuf(ocharbuf *buf)
{ buf->size = 0;

  if ( buf->allocated > 8192 )
  { assert(buf->data.w != buf->localbuf);
    sgml_free(buf->data.w);

    buf->allocated = sizeof(buf->localbuf)/sizeof(wchar_t);
    buf->data.w = buf->localbuf;
  }
}


		 /*******************************
		 *	   BUFFER RING		*
		 *******************************/

#define RINGSIZE 16
static void *ring[RINGSIZE];
static int  ringp;

wchar_t *
str2ring(const wchar_t *in)
{ wchar_t *copy = sgml_malloc((wcslen(in)+1)*sizeof(wchar_t));

  if ( !copy )
  { sgml_nomem();
    return NULL;
  }

  wcscpy(copy, in);
  if ( ring[ringp] )
    sgml_free(ring[ringp]);
  ring[ringp++] = copy;
  if ( ringp == RINGSIZE )
    ringp = 0;

  return copy;
}


void *
ringallo(size_t size)
{ char *result = sgml_malloc(size);
    
  if ( ring[ringp] )
    sgml_free(ring[ringp]);
  ring[ringp++] = result;
  if ( ringp == RINGSIZE )
    ringp = 0;

  return result;
}


               /*******************************
               *              MISC            *
               *******************************/

wchar_t const *
str_summary(wchar_t const *s, int len)
{ wchar_t *buf;
  size_t l = wcslen(s);

  if ( l < (size_t)len )
    return s;
  buf = ringallo((len + 10)*sizeof(wchar_t));
  wcsncpy(buf, s, len-5);
  wcscpy(&buf[len-5], L" ... ");
  wcscpy(&buf[len], &s[l-5]);

  return buf;
}


wchar_t *
utf8towcs(const char *in)
{ int sl = strlen(in);
  size_t len = utf8_strlen(in, sl);
  wchar_t *buf = sgml_malloc((len + 1)*sizeof(wchar_t));
  const char *e = in+sl;
  int i;

  for(i=0; in < e;)
  { int chr;

    in = utf8_get_char(in, &chr);
    buf[i++] = chr;
  }

  buf[i] = 0;
  return buf;
}


char *
wcstoutf8(const wchar_t *in)
{ int size = 0;
  const wchar_t *s;
  char *rc, *o;

  for(s=in; *s; s++)
  { char buf[6];

    if ( *s >= 0x80 )
    { char *o2 = utf8_put_char(buf, *s);
      size += o2-buf;
    } else
    { size++;
    }
  }
  
  rc = sgml_malloc(size+1);
  for(o=rc, s=in; *s; s++)
  { o = utf8_put_char(o, *s);
  }
  *o = '\0';

  return rc;
}


		 /*******************************
		 *	      FILES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load a file into memory. This would be so  easy if we didn't had to deal
with &#RE/&#RS handling that forces us to create the proper record start
and end.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef O_BINARY
#define O_BINARY 0
#endif

FILE *
wfopen(const wchar_t *name, const char *mode)
{ int mbl = wcstombs(NULL, name, 0);

  if ( mbl > 0 )
  { char *mbs = sgml_malloc(mbl+1);
    FILE *f;

    wcstombs(mbs, name, mbl+1);
    f = fopen(mbs, mode);
    sgml_free(mbs);

    return f;
  }

  return NULL;
}


static int
wopen(const wchar_t *name, int flags)
{ int mbl = wcstombs(NULL, name, 0);

  if ( mbl > 0 )
  { char *mbs = sgml_malloc(mbl+1);
    int fd;

    wcstombs(mbs, name, mbl+1);
    fd = open(mbs, flags);
    sgml_free(mbs);

    return fd;
  }

  return -1;
}


ichar *
load_sgml_file_to_charp(const ichar *file, int normalise_rsre, int *length)
{ int fd;

  if ( (fd = wopen(file, O_RDONLY|O_BINARY)) >= 0 )
  { struct stat buf;

    if ( fstat(fd, &buf) == 0 )
    { long len = buf.st_size;
      char *r = sgml_malloc(len+1);

      if ( r )
      { char *s = r;
	
	while(len>0)
	{ int n;

	  if ( (n=read(fd, s, len)) < 0 )
	  { close(fd);			/* I/O error */
	    sgml_free(r);
	    return NULL;
	  } else if ( n == 0 )
	    break;
	  len -= n;
	  s += n;
	}

	len = s-r;
	*s = '\0';			/* ensure closing EOS */
	close(fd);

	{ int nl;
	  int last_is_lf;
	  ichar *r2, *t;

	  if ( normalise_rsre )
	  { last_is_lf = (len > 0 && s[-1] == '\n');
	    for(s=r, nl=0; *s; s++)
	    { if ( *s == '\n' && s>r && s[-1] != '\r' )
		nl++;
	    }
	  } else
	  { nl = 0;
	    last_is_lf = 0;
	  }

	  r2 = sgml_malloc((len+nl+1)*sizeof(ichar));
	  for(s=r, t=r2; *s; s++)
	  { if ( *s == '\n' )
	    { if ( s>r && s[-1] != '\r' )
		*t++ = CR;
	      *t++ = LF;
	    } else
	      *t++ = *s;
	  }
	  len = t-r2;
	  *t = '\0';

	  if ( last_is_lf )
	    r2[--len] = '\0';		/* delete last LF */
	  
	  if ( length )
	    *length = len;
	  sgml_free(r);
	  return r2;
	}
      }
    }
  }

  return NULL;
}


		 /*******************************
		 *	     ALLOCATION		*
		 *******************************/

#ifdef _WINDOWS
#include <windows.h>
#endif

void
sgml_nomem()
{ fprintf(stderr, "SGML: Fatal: out of memory\n");

#ifdef _WINDOWS
   MessageBox(NULL, "SGML: Fatal: out of memory", "SGML", MB_OK|MB_TASKMODAL);
#endif

  exit(1);
}


void *
sgml_malloc(size_t size)
{ void *mem;

  if ( size == 0 )
    return NULL;

  if ( (mem = malloc(size)) )
    return mem;

  sgml_nomem();
  return NULL;
}


void *
sgml_realloc(void *old, size_t size)
{ void *mem;

  if ( old )
  { if ( (mem = realloc(old, size)) )
      return mem;
  } else
  { if ( (mem = malloc(size)) )
      return mem;
  }

  sgml_nomem();
  return NULL;
}


void *
sgml_calloc(size_t n, size_t size)
{ void *mem;

  if ( (mem=calloc(n, size)) )
    return mem;

  sgml_nomem();
  return NULL;
}


void
sgml_free(void *mem)
{ if ( mem )
    free(mem);
}


		 /*******************************
		 *	       DEBUG		*
		 *******************************/

void
wputs(ichar *s)
{ fwprintf(stderr, L"%ls", s);
}
