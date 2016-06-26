/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
