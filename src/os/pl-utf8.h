/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      Vu University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301 USA
*/


#ifndef UTF8_H_INCLUDED
#define UTF8_H_INCLUDED

#define PL_MB_LEN_MAX 16

#define UTF8_MALFORMED_REPLACEMENT 0xfffd

#define ISUTF8_MB(c) ((unsigned)(c) >= 0xc0 && (unsigned)(c) <= 0xfd)

#define ISUTF8_CB(c)  (((c)&0xc0) == 0x80) /* Is continuation byte */
#define ISUTF8_FB2(c) (((c)&0xe0) == 0xc0)
#define ISUTF8_FB3(c) (((c)&0xf0) == 0xe0)
#define ISUTF8_FB4(c) (((c)&0xf8) == 0xf0)
#define ISUTF8_FB5(c) (((c)&0xfc) == 0xf8)
#define ISUTF8_FB6(c) (((c)&0xfe) == 0xfc)

#define UTF8_FBN(c) (!(c&0x80)     ? 0 : \
		     ISUTF8_FB2(c) ? 1 : \
		     ISUTF8_FB3(c) ? 2 : \
		     ISUTF8_FB4(c) ? 3 : \
		     ISUTF8_FB5(c) ? 4 : \
		     ISUTF8_FB6(c) ? 5 : -1)
#define UTF8_FBV(c,n) ( n == 0 ? c : (c & ((0x01<<(6-n))-1)) )

#define utf8_get_char(in, chr) \
	(*(in) & 0x80 ? _PL__utf8_get_char(in, chr) \
		      : (*(chr) = *(in), (char *)(in)+1))
#define utf8_put_char(out, chr) \
	((chr) < 0x80 ? out[0]=(char)(chr), out+1 \
		      : _PL__utf8_put_char(out, (chr)))

extern char *_PL__utf8_get_char(const char *in, int *chr);
extern char *_PL__utf8_put_char(char *out, int chr);
extern char *_PL__utf8_skip_char(const char *out);

extern size_t utf8_strlen(const char *s, size_t len);
extern size_t utf8_strlen1(const char *s);
extern const char *utf8_skip(const char *s, size_t n);
extern int    utf8_strncmp(const char *s1, const char *s2, size_t n);

typedef enum {
  S_ASCII,
  S_LATIN,
  S_WIDE
} unicode_type_t;

extern unicode_type_t _PL__utf8_type(const char *in0, size_t len);


		 /*******************************
		 *	 INLINE FUNCTIONS	*
		 *******************************/

static inline char *
utf8_skip_char(const char *in)
{ if ( !(in[0]&0x80) )
  { return (char*)in+1;
  } else
  { in++;
    while ( ISUTF8_CB(in[0]) )
      in++;
    return (char*)in;
  }
}


static inline char *
utf8_backskip_char(const char *start, const char *s)
{ for(s--; s>start && ISUTF8_CB(*s); s--)
    ;
  return (char*)s;
}

#endif /*UTF8_H_INCLUDED*/
