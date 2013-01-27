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

#ifndef PL_LOCALE_H_INCLUDED
#define PL_LOCALE_H_INCLUDED

#define LOCALE_MAGIC 37838743

typedef struct PL_locale
{ atom_t	alias;		/* named alias (if any) */
  atom_t	symbol;		/* blob handle */
  int		magic;		/* LOCALE_MAGIC */
  int		references;	/* Reference count */
				/* POSIX locale properties */
  wchar_t      *decimal_point;	/* Radix character */
  wchar_t      *thousands_sep;	/* Separator for digit group left of radix character */
  char	       *grouping;	/* Grouping  */
} PL_locale;

#define PL_HAVE_PL_LOCALE 1

COMMON(void)		initLocale(void);
COMMON(void)		updateLocale(int category, const char *locale);
COMMON(PL_locale *)	acquireLocale(PL_locale *l);
COMMON(void)		releaseLocale(PL_locale *l);
COMMON(int)		initStreamLocale(IOSTREAM *s);
COMMON(int)		unifyLocale(term_t t, PL_locale *l, int alias);
COMMON(int)		getLocale(term_t t, PL_locale **lp);
COMMON(int)		getLocaleEx(term_t t, PL_locale **lp);

#endif /*PL_LOCALE_H_INCLUDED*/
