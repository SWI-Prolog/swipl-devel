/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam

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

#ifndef PL_STRING_H_INCLUDED
#define PL_STRING_H_INCLUDED

COMMON(char *) 		store_string(const char *s);
COMMON(void) 		remove_string(char *s);
COMMON(char) 		digitName(int n, int small);
COMMON(int) 		digitValue(int b, int c);
COMMON(bool) 		strprefix(const char *string, const char *prefix);
COMMON(bool) 		strpostfix(const char *string, const char *postfix);
COMMON(bool) 		stripostfix(const char *string, const char *postfix);
#ifndef HAVE_STRCASECMP
COMMON(int) 		strcasecmp(const char *s1, const char *s2);
#endif
#ifndef HAVE_STRLWR
COMMON(char *) 		strlwr(char *s);
#endif
#ifndef HAVE_MBSCOLL
COMMON(int)		mbscoll(const char *s1, const char *s2);
#endif
#ifndef HAVE_MBSCASECOLL
COMMON(int)		mbscasecoll(const char *s1, const char *s2);
#endif

#endif /*PL_STRING_H_INCLUDED*/
