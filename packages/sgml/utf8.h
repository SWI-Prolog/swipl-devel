/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef UTF8_H_INCLUDED
#define UTF8_H_INCLUDED

#define ISUTF8_MB(c) ((unsigned)(c) >= 0xc0 && (unsigned)(c) <= 0xfd)

#define utf8_get_char(in, chr) \
	(*(in) & 0x80 ? __utf8_get_char(in, chr) : *chr=*in, ++in)

extern char *__utf8_get_char(const char *in, int *chr);

#endif /*UTF8_H_INCLUDED*/
