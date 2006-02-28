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
#define	STR_MATCH_EXACT		0x1	/* case-insensitive */
#define	STR_MATCH_SUBSTRING	0x2	/* substring */
#define	STR_MATCH_WORD		0x3	/* whole word */
#define	STR_MATCH_PREFIX	0x4	/* prefix */
#define STR_MATCH_LIKE		0x5	/* SeRQL *like* match */

int	cmp_atoms(atom_t a1, atom_t a2);
atom_t	first_atom(atom_t a, int match);
int	match_atoms(int how, atom_t search, atom_t label);

#endif /*ATOM_H_INCLUDED*/
