/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#ifndef _STR_H_INCLUDED
#define _STR_H_INCLUDED

#undef char8				/* from pce-include.h */

typedef unsigned char char8;		/* 8-bit character */
typedef unsigned short char16;		/* 16-bit character */
typedef unsigned int wchar;		/* char as passed around */

#define STR_SIZE_BITS 27
#define STR_MAX_SIZE ((1L<<STR_SIZE_BITS)-1)

typedef struct _string
{ unsigned	size : STR_SIZE_BITS;	/* size indication (128 MB) */
  unsigned	encoding : 2;		/* character encoding used */
  unsigned	b16 : 1;		/* 8- or 16-bit wide characters */
  unsigned	readonly : 1;		/* storage is externally managed */
  unsigned	pad : 1;		/* padding to word-boundary */
  union
  { char8 *	text8;
    char16 *	text16;
  } text_union;
} string;

#define s_text		text_union.text8
#define s_text8		text_union.text8
#define s_text16	text_union.text16

#define ENC_ASCII (0)			/* standard 7-bits ASCII encoding */
#define ENC_UNICODE (1)			/* 16-bits unicode encoding */

#define isstr8(s) ((s)->b16 == 0)	/* 8-bit string */
#define isstr16(s) ((s)->b16 == 1)	/* 16-bit string */

#define str_len(s) ((s)->size)		/* length of the string */
#define str_wsize(s) ((((s)->b16 ? (s)->size * 2 \
		      	         : (s)->size) + sizeof(wchar) - 1) \
			/ sizeof(wchar))
#define str_fetch8(s, i)	(s->s_text8[(i)])
#define str_fetch16(s, i)	(s->s_text16[(i)])
#define str_store8(s, i, c)	(s->s_text8[(i)] = (char8)(c))
#define str_store16(s, i, c)	(s->s_text16[(i)] = (char16)(c))

#define str_cphdr(t, f) do { *(unsigned long *)(t) = *(unsigned long *)(f); \
			   } while(0)
#define str_inithdr(s, e) do { *(unsigned long *)(s) = 0L; \
			       (s)->encoding = (e); \
			       (s)->b16	     = ((e) == ENC_UNICODE ? 1 : 0); \
			     } while(0)

#define str_datasize(s) (isstr8(s) ? (s)->size : (s)->size * 2)

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

		 /*******************************
		 *	      ENCODING		*
		 *******************************/

typedef struct
{ char8 newline;
  char8 *tolower;
  char8 *toupper;
} str8_encoding, *Str8Encoding;


typedef struct
{ char16 newline;
  int min_byte1;
  int max_byte1;
  char16 **tolower;
  char16 **toupper;
} str16_encoding, *Str16Encoding;

#endif /*_STR_H_INCLUDED*/
