/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef DTD_UTIL_H_INCLUDED
#define DTD_UTIL_H_INCLUDED
#include "sgmldefs.h"

typedef struct 
{ int allocated;
  int size;
  ichar *data;
} icharbuf;

typedef struct 
{ int allocated;
  int size;
  ochar *data;
} ocharbuf;

int		istrlen(const ichar *s);
ichar *         istrdup(const ichar *s);
ichar *		istrcpy(ichar *d, const ichar *s);
ichar *		istrupper(ichar *s);
ichar *		istrlower(ichar *s);
int             istrprefix(const ichar *pref, const ichar *s);
int             istreq(const ichar *s1, const ichar *s2);
int             istrcaseeq(const ichar *s1, const ichar *s2);
int		istrncaseeq(const ichar *s1, const ichar *s2, int len);
int             istrhash(const ichar *t, int tsize);
int             istrcasehash(const ichar *t, int tsize);

#define add_icharbuf(buf, chr) \
	do { if ( buf->size < buf->allocated ) \
	       buf->data[buf->size++] = chr; \
	     else \
	       __add_icharbuf(buf, chr); \
	   } while(0)
#define add_ocharbuf(buf, chr) \
	do { if ( buf->size < buf->allocated ) \
	       buf->data[buf->size++] = chr; \
	     else \
	       __add_ocharbuf(buf, chr); \
	   } while(0)

icharbuf *	new_icharbuf(void);
void		free_icharbuf(icharbuf *buf);
void		__add_icharbuf(icharbuf *buf, int chr);
void		del_icharbuf(icharbuf *buf);
void		terminate_icharbuf(icharbuf *buf);
void		empty_icharbuf(icharbuf *buf);

int		ostrlen(const ochar *s);
ochar *         ostrdup(const ochar *s);

ocharbuf *	new_ocharbuf(void);
void		free_ocharbuf(ocharbuf *buf);
void		__add_ocharbuf(ocharbuf *buf, int chr);
void		del_ocharbuf(ocharbuf *buf);
void		terminate_ocharbuf(ocharbuf *buf);
void		empty_ocharbuf(ocharbuf *buf);

const char *	str_summary(const char *s, int len);
char *		str2ring(const char *in);
char *		load_file_to_charp(const char *file);

#endif /*DTD_UTIL_H_INCLUDED*/
