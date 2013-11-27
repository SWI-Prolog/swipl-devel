/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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

#ifndef PL_DICT_H_INCLUDED
#define PL_DICT_H_INCLUDED

#define DICT_SORTED	0x1		/* Sort dict entries */

COMMON(int)	PL_is_dict(term_t t);
COMMON(int)	PL_for_dict(term_t dict,
			   int (*func)(term_t key,
				       term_t value,
				       int last,
				       void *closure),
			   void *closure,
			   int flags);

COMMON(functor_t) dict_functor(int pairs);
COMMON(int)	  dict_order_term_refs(term_t *av, int *indexes, int count ARG_LD);
COMMON(int)	  resortDictsInClause(Clause clause);
COMMON(void)	  resortDictsInTerm(term_t t);

#endif /*PL_DICT_H_INCLUDED*/
