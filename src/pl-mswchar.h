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
See pl-mswchar.cpp for  the  motivation  for   this  nonsense.  Used  in
pl-fli.c and pl-text.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32
#define wcrtomb(s, wc, ps)	ms_wcrtomb(s, wc, ps)
#define mbrtowc(pwc, s, n, ps)	ms_mbrtowc(pwc, s, n, ps)

extern size_t ms_wcrtomb(char *s, wchar_t wc, mbstate_t *ps);
extern size_t ms_mbrtowc(wchar_t *pwc, const char *s, size_t n, mbstate_t *ps);
#endif
