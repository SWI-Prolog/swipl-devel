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

#define PL_CHARS_LATIN		0x0001	/* 8-bit ISO-Latin-1 text */
#define PL_CHARS_UCS		0x0002	/* 32-bit USC text */
#define PL_CHARS_ALLOC_MASK	0xff00	/* allocation mask */
#define PL_CHARS_MALLOC		0x0100	/* text is malloced */
#define PL_CHARS_RING   	0x0200	/* text is in buffer-ring */
#define PL_CHARS_HEAP   	0x0400	/* text is in heap (atom) */
#define PL_CHARS_STACK  	0x0800	/* text is in stack (string) */
#define PL_CHARS_LOCAL		0x1000	/* text is in stack (string) */

typedef struct
{ union
  { char *t;				/* tranditional 8-bit char* */
    pl_wchar_t *w;			/* wide character string */
  } text;
  unsigned int	length;
					/* private stuff */
  unsigned int  flags;
					/* PL_CHARS_LATIN */
					/* PL_CHARS_UCS */
					/* PL_CHARS_MALLOC */
					/* PL_CHARS_RING */
					/* PL_CHARS_HEAP */
					/* PL_CHARS_STACK */
					/* PL_CHARS_LOCAL */
  char buf[100];			/* buffer for simple stuff */
} PL_chars_t;


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
