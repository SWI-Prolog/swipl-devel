/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

extern char _PL_char_types[];	/* array of character types */

#define CT  0			/* control-character */
#define SP  1			/* space */
#define SO  2			/* solo character */
#define SY  3			/* symbol character */
#define PU  4			/* Punctuation character */
#define DQ  5			/* Double quote */
#define SQ  6			/* Single quote */
#define UC  7			/* Uppercase character */
#define LC  8			/* Lowercase character */
#define DI  9			/* Digit */

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
