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

extern char _PL_char_types[];	/* array of character types */

#define CT  0			/* control-character */
#define SP  1			/* space */
#define SO  2			/* solo character */
#define SY  3			/* symbol character */
#define PU  4			/* Punctuation character */
#define DQ  5			/* Double quote */
#define SQ  6			/* Single quote */
#define BQ  7			/* Back quote */
#define UC  8			/* Uppercase character */
#define LC  9			/* Lowercase character */
#define DI 10			/* Digit */

#define isControl(c)	(_PL_char_types[(unsigned)(c) & 0xff] == CT)
#define isBlank(c)	(_PL_char_types[(unsigned)(c) & 0xff] <= SP)
#define isDigit(c)	(_PL_char_types[(unsigned)(c) & 0xff] == DI)
#define isLower(c)	(_PL_char_types[(unsigned)(c) & 0xff] == LC)
#define isUpper(c)	(_PL_char_types[(unsigned)(c) & 0xff] == UC)
#define isSymbol(c)	(_PL_char_types[(unsigned)(c) & 0xff] == SY)
#define isPunct(c)	(_PL_char_types[(unsigned)(c) & 0xff] == PU)
#define isSolo(c)	(_PL_char_types[(unsigned)(c) & 0xff] == SO)
#define isAlpha(c)	(_PL_char_types[(unsigned)(c) & 0xff] >= UC)
#define isLetter(c)	(isLower(c) || isUpper(c))

#define toLower(c)	((c) + 'a' - 'A')
#define makeLower(c)	((c) >= 'A' && (c) <= 'Z' ? toLower(c) : (c))

#define matchingBracket(c)	((c) == '[' ? ']' :\
				        '{' ? '}' :\
				        '(' ? ')' : EOS)
#define Control(c)		((c) == '?' ? 127 : (c) - '@')


		 /*******************************
		 *    WIDE CHARACTER SUPPORT	*
		 *******************************/

#include <wctype.h>

#define PlCharType(c, t, w) \
	((unsigned)(c) <= 0xff ? (_PL_char_types[(unsigned)(c)] t) : w)

#define isControlW(c)	PlCharType(c, == CT, iswcntrl(c))
#define isBlankW(c)	PlCharType(c, <= SP, iswspace(c))
#define isDigitW(c)	PlCharType(c, == DI, FALSE)
#define isLowerW(c)	PlCharType(c, == LC, iswlower(c))
#define isUpperW(c)	PlCharType(c, == UC, iswupper(c))
#define isSymbolW(c)	PlCharType(c, == SY, FALSE)
#define isPunctW(c)	PlCharType(c, == PU, FALSE)
#define isSoloW(c)	PlCharType(c, == SO, FALSE)
#define isAlphaW(c)	PlCharType(c, >= UC, iswalnum(c))
#define isLetterW(c)	(PlCharType(c, == LC, iswalpha(c)) || \
			 PlCharType(c, == UC, FALSE))

#define toLowerW(c)	((unsigned)(c) <= 'Z' ? (c) + 'a' - 'A' : towlower(c))
#define makeLowerW(c)	((c) >= 'A' && (c) <= 'Z' ? toLower(c) : towlower(c))
