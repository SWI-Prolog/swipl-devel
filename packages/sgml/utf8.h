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

#ifndef UTF8_H_INCLUDED
#define UTF8_H_INCLUDED

#define ISUTF8_MB(c) ((unsigned)(c) >= 0xc0 && (unsigned)(c) <= 0xfd)

#define utf8_get_char(in, chr) \
	(*(in) & 0x80 ? __utf8_get_char(in, chr) : *chr=*in, ++in)

extern char *__utf8_get_char(const char *in, int *chr);

#endif /*UTF8_H_INCLUDED*/
