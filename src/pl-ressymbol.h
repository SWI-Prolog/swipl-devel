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

#ifndef PL_METAATOM_H_INCLUDED
#define PL_METAATOM_H_INCLUDED 1

#ifdef O_RESERVED_SYMBOLS

COMMON(void)	initReservedSymbols(void);
COMMON(int)	isReservedSymbol(word w);
COMMON(atom_t)	textToReservedSymbol(PL_chars_t *text);

#else /*O_RESERVED_SYMBOLS*/

#define isReservedSymbol(a) FALSE

#endif /*O_RESERVED_SYMBOLS*/

#endif /*PL_METAATOM_H_INCLUDED*/
