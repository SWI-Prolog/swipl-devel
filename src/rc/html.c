/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1998-2015, University of Amsterdam
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

#define RC_KERNEL 1
#include "rc.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include "html.h"
#include <stdlib.h>

#ifndef EOS
#define EOS '\0'
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Decode a HTML tag, returning a pointer to the end of it
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
skipblanks(const char *s)
{ while ( isspace((int)*s) )
    s++;

  return (char *)s;
}


static char *
skipidentifier(const char *s)
{ while ( isalnum((int)*s) || *s == '_' )
    s++;

  return (char *)s;
}


char *
html_find_tag(const char *data, const char *end, const char *tag)
{ size_t len = strlen(tag);

  for(;;)
  { while(data != end && *data != '<')
      data++;

    if ( data == end )
      return NULL;

    if ( strncmp(data+1, tag, len) == 0 )
    { int c = data[len+1];

      if ( isspace(c) || c == '>' )
	return (char *)data+len+1;
    }

    data++;
  }
}


char *
html_find_close_tag(const char *data, const char *tag)
{ size_t len = strlen(tag);

  while(data)
  { if ( (data = strchr(data, '<')) &&
	 data[1] == '/' &&
	 strncmp(data+2, tag, len) == 0 &&
	 data[len+2] == '>' )
      return (char *)data+len+3;
    if ( data )
      data++;
  }

  return NULL;
}


static int
fd_skip_blanks(FILE *fd)
{ for(;;)
  { int c = getc(fd);

    if ( c == EOF || !isspace(c) )
      return c;
  }
}


int
html_fd_next_tag(FILE *fd, char *tag, char *props)
{ for(;;)
  { int c = getc(fd);

    switch(c)
    { case EOF:
	return FALSE;
      case '<':
      { char *p = tag;
	int plen = MAXTAGLEN;

					/* read the tag */
	for(c=fd_skip_blanks(fd); ; c = getc(fd))
	{ if ( c == EOF )
	    return FALSE;
	  if ( c == '>' )
	  { *p = EOS;
	    props[0] = EOS;
	    return TRUE;
	  }
	  if ( isspace(c) )
	  { *p = EOS;
	    break;
	  }
	  if ( --plen <= 0 )
	    return FALSE;		/* or restart? */
	  *p++ = (char)c;
	}

					/* read the properties */
	p = props;
	plen = MAXTAGPROPLEN;

	for(c=fd_skip_blanks(fd); ; c = getc(fd))
	{ if ( c == EOF )
	    return FALSE;
	  if ( c == '>' )
	  { *p = EOS;
	    return TRUE;
	  }
	  if ( --plen <= 0 )
	    return FALSE;		/* or restart? */
	  *p++ = (char)c;
	}
      }
    }
  }
}


int
html_fd_find_close_tag(FILE *fd, const char *etag)
{ for(;;)
  { int c = getc(fd);

cont:
    switch(c)
    { case EOF:
	return FALSE;
      case '<':
      { const char *s = etag;

	c = getc(fd);

	if ( c != '/' )
	  goto cont;

	while( *s )
	{ c = getc(fd);

	  if ( tolower(c) != *s++ )
	    goto cont;
	}

	if ( (c=getc(fd)) != '>' )
	    goto cont;

	return TRUE;
      }
    }
  }
}


char *
html_decode_tag(const char *data, HtmlTagDef spec, void *dest)
{ const char *s = data;

  for(;;)
  { const char *si;			/* start-identifier */
    const char *ei;			/* end-identifier */
    const char *sv;			/* start-value */
    const char *ev;			/* end-value */
    HtmlTagDef d;

    s = skipblanks(s);
    if ( *s == '>' )
      return (char *)s+1;
    if ( *s == '\0' )
      return (char *)s;

    si = s;
    ei = skipidentifier(si);
    if ( si == ei )			/* end of the file */
      return (char *)s;

    if ( *ei == '=' )
    { sv = ei+1;

      if ( *sv == '"' )
      { ev = ++sv;
	while(*ev && *ev != '"')
	  ev++;
	s = (*ev ? ev+1 : ev);
      } else
	s = ev = skipidentifier(sv);
    } else
    { s = ei;
      ev = sv = NULL;
    }

    for(d=spec; d->tag; d++)
    { if ( strncmp(si, d->tag, ei-si) == 0 )
      { void *dst = (char *)dest + d->offset;

	(*d->convert)(sv, ev-sv, dst, d->closure);
	break;
      }
    }
  }
}


int
html_cvt_malloc_string(const char *data, size_t len, void *dst, void *closure)
{ (void)closure;

  if ( data )
  { char *tmp = malloc(len+1);

    if ( tmp )
    { char **d = dst;

      strncpy(tmp, data, len);
      tmp[len] = '\0';
      *d = tmp;

      return TRUE;
    }
  }

  return FALSE;
}


int
html_cvt_long(const char *data, size_t len, void *dst, void *closure)
{ (void)closure;

  if ( data )
  { char *e;
    intptr_t val;

    val = strtol(data, &e, 0);
    if ( data+len == e )
    { intptr_t *d = dst;

      *d = val;

      return TRUE;
    }
  }

  return FALSE;
}


int
html_cvt_date(const char *data, size_t len, void *dst, void *closure)
{ if ( data )
  { if ( html_cvt_long(data, len, dst, closure) ) /* POSIX time stamp */
      return TRUE;
  }

  return FALSE;
}


