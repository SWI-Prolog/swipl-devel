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

#ifndef _ORDER_H_INCLUDED
#define _ORDER_H_INCLUDED

#define ORD_MAGIC	 372132363	/* just a number */

#define ORD_IGNORE	 3		/* ignore these */
#define ORD_BREAK	 2		/* break a word */
#define ORD_TAG	 	 1		/* foo (a bar) */
#define ORD_END		 0		/* end of word */

#define ORD(ot, c)	(ot->ords[(int)(c)&0xff])

typedef struct
{ int		magic;			/* ORD_MAGIC */
  atom_t	name;			/* name of the table */
  unsigned char ords[256];		/* mapping of the table */
} ordtable, *OrdTable;

OrdTable	findOrdTable(atom_t name);
int             compare_strings(const char *s1, const char *s2,
				int n, OrdTable ot);
install_t       install_order(void);

#endif /*_ORDER_H_INCLUDED*/
