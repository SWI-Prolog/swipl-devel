/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Character types
*/

extern char _PL_char_types[];	/* array of character types */

#define SP  0			/* space */
#define SO  1			/* solo character */
#define SY  2			/* symbol character */
#define PU  3			/* Punctuation character */
#define DQ  4			/* Double quote */
#define SQ  5			/* Single quote */
#define UC  6			/* Uppercase character */
#define LC  7			/* Lowercase character */
#define DI  8			/* Digit */

#define isBlank(c)	(_PL_char_types[(unsigned)(c) & 0xff] == SP)
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
