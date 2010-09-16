/*  $Id$

    Part of the SWI-Prolog Semweb package

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef ATOM_H_INCLUDED
#define ATOM_H_INCLUDED

#define MAX_LIKE_CHOICES	100	/* max *'s in like pattern */

#define STR_MATCH_CASE		0x0	/* Default: perfect match */
#define STR_MATCH_PLAIN		0x1	/* Same, also match qualifier */
#define	STR_MATCH_EXACT		0x2	/* case-insensitive */
					/* keep after exact */
#define	STR_MATCH_SUBSTRING	0x3	/* substring */
#define	STR_MATCH_WORD		0x4	/* whole word */
#define	STR_MATCH_PREFIX	0x5	/* prefix */
#define STR_MATCH_LIKE		0x6	/* SeRQL *like* match */
					/* Keep after LIKE */
#define STR_MATCH_LE		0x7	/* =< */
#define STR_MATCH_GE		0x8	/* >= */
#define STR_MATCH_BETWEEN	0x9	/* X .. Y */
					/* MAX: 0xf (4 bits in triple) */

typedef unsigned char charA;
typedef wchar_t       charW;

typedef struct text
{ const charA *a;
  const charW *w;
  size_t length;
} text;


typedef struct atom_info
{ atom_t	handle;
  text		text;
  int		resolved;
  int		rc;			/* TRUE if text atom */
} atom_info;


int	cmp_atoms(atom_t a1, atom_t a2);
int	cmp_atom_info(atom_info *a1, atom_t a2);
atom_t	first_atom(atom_t a, int match);
int	match_atoms(int how, atom_t search, atom_t label);
unsigned int atom_hash_case(atom_t a);
int	atom_lang_matches(atom_t lang, atom_t pattern);

#endif /*ATOM_H_INCLUDED*/
