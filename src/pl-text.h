/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
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

#ifndef PL_TEXT_H_INCLUDED
#define PL_TEXT_H_INCLUDED

typedef enum
{ PL_CHARS_MALLOC,			/* malloced data */
  PL_CHARS_RING,			/* stored in the buffer ring */
  PL_CHARS_HEAP,			/* stored in program area (atoms) */
  PL_CHARS_STACK,			/* stored on the global stack */
  PL_CHARS_LOCAL			/* stored in in-line buffer */
} PL_chars_alloc_t;


typedef struct
{ union
  { char *t;				/* tranditional 8-bit char* */
    pl_wchar_t *w;			/* wide character string */
  } text;
  unsigned int	length;
					/* private stuff */
  IOENC encoding;			/* how it is encoded */
  PL_chars_alloc_t storage;		/* how it is stored */
  int canonical;			/* TRUE: ENC_ISO_LATIN_1 or ENC_WCHAR */
  char buf[100];			/* buffer for simple stuff */
} PL_chars_t;

#define PL_init_text(txt) \
	{ txt->text.t    = NULL; \
	  txt->encoding  = ENC_UNKNOWN; \
	  txt->storage   = PL_CHARS_LOCAL; \
	  txt->canonical = FALSE; \
	}

int	PL_get_text(term_t l, PL_chars_t *text, int flags);
int	PL_unify_text(term_t term, PL_chars_t *text, int type);
int	PL_unify_text_range(term_t term, PL_chars_t *text,
			    unsigned int from, unsigned int len, int type);

int	PL_promote_text(PL_chars_t *text);
int	PL_demote_text(PL_chars_t *text);
int	PL_canonise_text(PL_chars_t *text);

int	PL_cmp_text(PL_chars_t *t1, unsigned o1, PL_chars_t *t2, unsigned o2,
		    unsigned len);
int	PL_concat_text(int n, PL_chars_t **text, PL_chars_t *result);

void	PL_free_text(PL_chars_t *text);
void	PL_save_text(PL_chars_t *text, int flags);

#endif /*PL_TEXT_H_INCLUDED*/
