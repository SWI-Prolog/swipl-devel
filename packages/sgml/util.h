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

#ifndef DTD_UTIL_H_INCLUDED
#define DTD_UTIL_H_INCLUDED
#include "sgmldefs.h"

#include <stdio.h>
#include <sys/types.h>
#include <wchar.h>

#ifdef _WINDOWS				/* get size_t */
#include <malloc.h>
#endif

typedef struct 
{ int allocated;
  int size;
  ichar *data;
} icharbuf;

typedef struct 
{ int allocated;
  int size;
  union 
  { wchar_t *w;				/* UCS */
  } data;
  wchar_t localbuf[256];		/* Initial local store */
} ocharbuf;

size_t		istrlen(const ichar *s);
ichar *         istrdup(const ichar *s);
ichar *         istrndup(const ichar *s, int len);
ichar *		istrcpy(ichar *d, const ichar *s);
ichar *		istrcat(ichar *d, const ichar *s);
ichar *		istrncpy(ichar *d, const ichar *s, size_t len);
ichar *		istrupper(ichar *s);
ichar *		istrlower(ichar *s);
int             istrprefix(const ichar *pref, const ichar *s);
int             istreq(const ichar *s1, const ichar *s2);
int             istrcaseeq(const ichar *s1, const ichar *s2);
int		istrncaseeq(const ichar *s1, const ichar *s2, int len);
int             istrhash(const ichar *t, int tsize);
int             istrcasehash(const ichar *t, int tsize);
ichar *		istrchr(const ichar *s, int c);
int		istrtol(const ichar *s, long *val);
void *		sgml_malloc(size_t size);
void *		sgml_calloc(size_t n, size_t size);
void		sgml_free(void *mem);
void *		sgml_realloc(void *old, size_t size);
void		sgml_nomem(void);

#define add_icharbuf(buf, chr) \
	do { if ( buf->size < buf->allocated && chr < 128 ) \
	       buf->data[buf->size++] = chr; \
	     else \
	       __add_icharbuf(buf, chr); \
	   } while(0)

icharbuf *	new_icharbuf(void);
void		free_icharbuf(icharbuf *buf);
void		__add_icharbuf(icharbuf *buf, int chr);
void		del_icharbuf(icharbuf *buf);
void		terminate_icharbuf(icharbuf *buf);
void		empty_icharbuf(icharbuf *buf);

ocharbuf *	init_ocharbuf(ocharbuf *buf);
ocharbuf *	new_ocharbuf(void);
void		free_ocharbuf(ocharbuf *buf);
ocharbuf *	malloc_ocharbuf(ocharbuf *buf);
void		add_ocharbuf(ocharbuf *buf, int chr);
void		del_ocharbuf(ocharbuf *buf);
void		terminate_ocharbuf(ocharbuf *buf);
void		empty_ocharbuf(ocharbuf *buf);
#define fetch_ocharbuf(buf, at) ((wint_t)buf->data.w[at])
#define poke_ocharbuf(buf, at, chr) \
	{ buf->data.w[at] = chr; \
	}

const wchar_t *	str_summary(const wchar_t *s, int len);
wchar_t *	str2ring(const wchar_t *in);
void *		ringallo(size_t);
wchar_t * 	utf8towcs(const char *in);
char *		wcstoutf8(const wchar_t *in);
ichar *		load_sgml_file_to_charp(const ichar *file, int normalise_rsre,
					int *len);
FILE *		wfopen(const wchar_t *name, const char *mode);

#if defined(USE_STRING_FUNCTIONS) && !defined(UTIL_H_IMPLEMENTATION)

#define istrlen(s1)   wcslen((s1))
#define istreq(s1,s2) (wcscmp((s1),(s2))==0)

#endif

#endif /*DTD_UTIL_H_INCLUDED*/
