/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2026, University of Amsterdam
                              VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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

#ifndef _PL_CTYPE_H
#define _PL_CTYPE_H
#include "../pl-incl.h"

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

IOENC		initEncoding(void);
void		initCharTypes(void);
access_level_t	setAccessLevel(access_level_t new_level);

extern const char _PL_char_types[];	/* array of character types (0..127) */

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

/* The is* macros classify ASCII (0..127) only; they return false for any
 * code point >= 0x80. Use the corresponding *W (wide) macros below or the
 * PlBlankW / PlIdStartW / ... macros in pl-read.c (which dispatch on the
 * u_category enum stored in src/pl-umap.c) for non-ASCII handling.
 */
#define isControl(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] == CT)
#define isBlank(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] == SP)
#define isGraph(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)]  > SP)
#define isDigit(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] == DI)
#define isLower(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] == LC)
#define isUpper(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] == UC)
#define isSymbol(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] == SY)
#define isPunct(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] == PU)
#define isSolo(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] == SO)
#define isAlpha(c)	((unsigned)(c) < 0x80 && _PL_char_types[(unsigned)(c)] >= UC)
#define isLetter(c)	(isLower(c) || isUpper(c))
#define isSign(c)	((c) == '-' || (c) == '+')
#define isDecimal(zero, c) ((c) >= (zero) && (c) <= (zero)+9)

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
	((unsigned)(c) < 0x80 ? (_PL_char_types[(unsigned)(c)] t) : w)

/* isBlankW() is the only class here that must answer for non-ASCII.  It
 * gets that answer from the Unicode tables in src/pl-umap.c through
 * PL_ctype_flags(), so it agrees with code_type/2 and does not depend
 * on LC_CTYPE.  The other three are deliberately ASCII-only: they
 * implement Prolog *syntax* decisions (number scanning, symbol atoms,
 * format/2 column specifications, write/1 spacing) for which the
 * non-ASCII cases are handled by the Pl*W macros in pl-read.c that
 * dispatch on the u_category enum.
 *
 * isControlW(), isLowerW(), isUpperW(), isSoloW(), isAlphaW() and
 * isLetterW() completed this set from <wctype.h>.  They had no users
 * and were removed rather than converted.
 */

#define isBlankW(c)	PlCharType(c, == SP, \
				   (PL_ctype_flags(c) & PL_CTYPE_SPACE))
#define isDigitW(c)	PlCharType(c, == DI, false)
#define isSymbolW(c)	PlCharType(c, == SY, false)
#define isPunctW(c)	PlCharType(c, == PU, false)

/* Used for case insensitive file name matching (pl-glob.c, pl-os.c).
 * PL_tolower() is the Unicode simple case mapping from src/pl-umap.c,
 * so which files a pattern matches no longer depends on LC_CTYPE.
 */

#define makeLowerW(c)	((c) >= 'A' && (c) <= 'Z' ? toLower(c) : PL_tolower(c))

#endif /*_PL_CTYPE_H*/
