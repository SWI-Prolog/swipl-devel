/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef OPTION_H_INCLUDED
#define OPTION_H_INCLUDED

#define OPT_BOOL	(0)		/* types */
#define OPT_INT		(1)
#define OPT_STRING	(2)
#define OPT_ATOM	(3)
#define OPT_TERM	(4)		/* arbitrary term */
#define OPT_LONG	(5)
#define OPT_NATLONG	(6)		/* > 0 */
#define OPT_SIZE	(7)		/* size_t */
#define OPT_DOUBLE	(8)
#define OPT_TYPE_MASK	0xff
#define OPT_INF		0x100		/* allow 'inf' */

#define OPT_ALL		0x1		/* flags */

typedef struct
{ atom_t	name;			/* Name of option */
  int		type;			/* Type of option */
} opt_spec, *OptSpec;

COMMON(int)		scan_options(term_t list, int flags, atom_t name,
				     const opt_spec *specs, ...);

#endif /*OPTION_H_INCLUDED*/
