/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2013, University of Amsterdam
                              VU University Amsterdam
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

extern const char _PL_char_types[];	/* array of character types */

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
#define isBlank(c)	(_PL_char_types[(unsigned)(c) & 0xff] == SP)
#define isGraph(c)	(_PL_char_types[(unsigned)(c) & 0xff]  > SP)
#define isDigit(c)	(_PL_char_types[(unsigned)(c) & 0xff] == DI)
#define isLower(c)	(_PL_char_types[(unsigned)(c) & 0xff] == LC)
#define isUpper(c)	(_PL_char_types[(unsigned)(c) & 0xff] == UC)
#define isSymbol(c)	(_PL_char_types[(unsigned)(c) & 0xff] == SY)
#define isPunct(c)	(_PL_char_types[(unsigned)(c) & 0xff] == PU)
#define isSolo(c)	(_PL_char_types[(unsigned)(c) & 0xff] == SO)
#define isAlpha(c)	(_PL_char_types[(unsigned)(c) & 0xff] >= UC)
#define isLetter(c)	(isLower(c) || isUpper(c))
#define isSign(c)	((c) == '-' || (c) == '+')

#define toLower(c)	((c) + 'a' - 'A')
#define makeLower(c)	((c) >= 'A' && (c) <= 'Z' ? toLower(c) : (c))

#define matchingBracket(c)	((c) == '[' ? ']' :\
				 (c) == '{' ? '}' :\
				 (c) == '(' ? ')' : EOS)
#define Control(c)		((c) == '?' ? 127 : (c) - '@')


		 /*******************************
		 *    WIDE CHARACTER SUPPORT	*
		 *******************************/

#include <wctype.h>
#include <wchar.h>

#define PlCharType(c, t, w) \
	((unsigned)(c) <= 0xff ? (_PL_char_types[(unsigned char)(c)] t) : w)

#define isControlW(c)	PlCharType(c, == CT, iswcntrl((wint_t)c))
#define isBlankW(c)	PlCharType(c, == SP, iswspace((wint_t)c))
#define isDigitW(c)	PlCharType(c, == DI, FALSE)
#define isLowerW(c)	PlCharType(c, == LC, iswlower((wint_t)c))
#define isUpperW(c)	PlCharType(c, == UC, iswupper((wint_t)c))
#define isSymbolW(c)	PlCharType(c, == SY, FALSE)
#define isPunctW(c)	PlCharType(c, == PU, FALSE)
#define isSoloW(c)	PlCharType(c, == SO, FALSE)
#define isAlphaW(c)	PlCharType(c, >= UC, iswalnum((wint_t)c))
#define isLetterW(c)	(PlCharType(c, == LC, iswalpha((wint_t)c)) || \
			 PlCharType(c, == UC, FALSE))

#define toLowerW(c)	((unsigned)(c) <= 'Z' ? (c) + 'a' - 'A' : towlower(c))
#define makeLowerW(c)	((c) >= 'A' && (c) <= 'Z' ? toLower(c) : towlower(c))
