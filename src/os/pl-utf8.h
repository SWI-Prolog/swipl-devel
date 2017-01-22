/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2013, University of Amsterdam
                              Vu University Amsterdam
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

static inline int
utf8_code_bytes(int chr)
{ if ( chr < 0x80 ) return 1;
  if ( chr < 0x800 ) return 2;
  if ( chr < 0x10000 ) return 3;
  if ( chr < 0x200000 ) return 4;
  if ( chr < 0x4000000 ) return 5;
  if ( chr < 0x80000000 ) return 6;
  return -1;
}


#endif /*UTF8_H_INCLUDED*/
