/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "util.h"
#include <ctype.h>
#include <stdlib.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int
istrlen(const ichar *s)
{ int len =0;
  
  while(*s++)
    len++;

  return len;
}


ichar *
istrdup(const ichar *s)
{ ichar *dup = malloc((istrlen(s)+1)*sizeof(ichar));
  ichar *d = dup;

  while(*s)
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


int
istrcaseeq(const ichar *s1, const ichar *s2)
{ while(*s1 && tolower(*s1) == tolower(*s2))
    s1++, s2++;
  
  if ( *s1 == 0 && *s2 == 0 )
    return TRUE;
  
  return FALSE;
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
{ while(--len >= 0 && tolower(*s1) == tolower(*s2))
    s1++, s2++;
  
  if ( len < 0 )
    return TRUE;
  
  return FALSE;
}


ichar *
istrupper(ichar *s)
{ ichar *r = s;

  for( ; *s; s++)
    *s = toupper(*s);

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
  { unsigned int c = tolower(*t++);	/* case insensitive */
    
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  value = value ^ (value >> 16);

  return value % tsize;
}

		 /*******************************
		 *    INPUT CHARACTER BUFFER	*
		 *******************************/

icharbuf *
new_icharbuf()
{ icharbuf *buf = malloc(sizeof(*buf));

  buf->allocated = 0;
  buf->size = 0;
  buf->data = NULL;

  return buf;
}


void
free_icharbuf(icharbuf *buf)
{ if ( buf->data )
    free(buf->data);

  free(buf);
}


void
__add_icharbuf(icharbuf *buf, int chr)
{ if ( buf->size == buf->allocated )
  { buf->allocated = (buf->allocated ? buf->allocated*2 : 128);

    if ( buf->data )
      buf->data = realloc(buf->data, buf->allocated);
    else
      buf->data = malloc(buf->allocated);
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
		 *	OUTPUT CHARACTERS	*
		 *******************************/

int
ostrlen(const ochar *s)
{ int len =0;
  
  while(*s++)
    len++;

  return len;
}


ochar *
ostrdup(const ochar *s)
{ ochar *dup = malloc((ostrlen(s)+1)*sizeof(ochar));
  ochar *d = dup;

  while(*s)
    *d++ = *s++;
  *d = 0;

  return dup;
}


		 /*******************************
		 *    OUTPUT CHARACTER BUFFER	*
		 *******************************/

ocharbuf *
new_ocharbuf()
{ ocharbuf *buf = malloc(sizeof(*buf));

  buf->allocated = 0;
  buf->size = 0;
  buf->data = NULL;

  return buf;
}


void
free_ocharbuf(ocharbuf *buf)
{ if ( buf->data )
    free(buf->data);

  free(buf);
}


void
__add_ocharbuf(ocharbuf *buf, int chr)
{ if ( buf->size == buf->allocated )
  { buf->allocated = (buf->allocated ? buf->allocated*2 : 128);

    if ( buf->data )
      buf->data = realloc(buf->data, buf->allocated);
    else
      buf->data = malloc(buf->allocated);
  }
  
  buf->data[buf->size++] = chr;
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


void
empty_ocharbuf(ocharbuf *buf)
{ buf->size = 0;
}

		 /*******************************
		 *		MISC		*
		 *******************************/

const char *
str_summary(const char *s, int len)
{ ichar *buf = alloca(sizeof(ichar)*(len+10));
  int l = strlen(s);

  if ( l < len )
    return s;
  strncpy(buf, s, len-5);
  strcpy(&buf[len-5], " ... ");
  strcpy(&buf[len], &s[l-5]);

  return str2ring(buf);
}


		 /*******************************
		 *	   BUFFER RING		*
		 *******************************/

#define RINGSIZE 16
static char *ring[RINGSIZE];
static int  ringp;

char *
str2ring(const char *in)
{ char *copy = strdup(in);

  if ( ring[ringp] )
    free(ring[ringp]);
  ring[ringp++] = copy;
  if ( ringp == RINGSIZE )
    ringp = 0;

  return copy;
}


		 /*******************************
		 *	      FILES		*
		 *******************************/

char *
load_file_to_charp(const char *file)
{ int fd;

  if ( (fd = open(file, O_RDONLY)) >= 0 )
  { struct stat buf;

    if ( fstat(fd, &buf) == 0 )
    { long len = buf.st_size;
      char *r = malloc(len+1);

      if ( r )
      { char *s = r;
	
	while(len>0)
	{ int n;

	  if ( (n=read(fd, s, len)) < 0 )
	  { close(fd);			/* I/O error */
	    free(r);
	    return NULL;
	  }
	  len -= n;
	  s += n;
	}

	*s = '\0';			/* ensure closing EOS */
	close(fd);
	return r;
      }
    }
  }

  return NULL;
}
