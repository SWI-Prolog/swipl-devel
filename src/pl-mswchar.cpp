/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

#include <wchar.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
You don't want to know ...

MSVC (at least version 6) documents   wcrtomb()  and mbrtowc(), but only
defines the in the  C++  runtime   library,  of  course `decorated'. For
short, we cannot use  the  functions  from   plain  C.  To  minimise the
consequences, I've added this tiny C++ file creating the C versions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern "C" size_t
ms_wcrtomb(char *s, wchar_t wc, mbstate_t *ps)
{ return wcrtomb(s, wc, ps);
}


extern "C" size_t
ms_mbrtowc(wchar_t *pwc, const char *s, size_t n, mbstate_t *ps)
{ return mbrtowc(pwc, s, n, ps);
}
