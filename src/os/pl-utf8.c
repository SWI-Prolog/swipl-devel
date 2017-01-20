/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2014, University of Amsterdam
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

#include <string.h>			/* get size_t */
#include "pl-utf8.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UTF-8 Decoding, based on http://www.cl.cam.ac.uk/~mgk25/unicode.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CONT(i)   ISUTF8_CB(in[i])
#define VAL(i, s) ((in[i]&0x3f) << s)

#define IS_UTF8_2BYTE(in) \
	((in[0]&0xe0) == 0xc0 && CONT(1))
#define IS_UTF8_3BYTE(in) \
	((in[0]&0xf0) == 0xe0 && CONT(1)&&CONT(2))
#define IS_UTF8_4BYTE(in) \
	((in[0]&0xf8) == 0xf0 && CONT(1)&&CONT(2)&&CONT(3))
#define IS_UTF8_5BYTE(in) \
	((in[0]&0xfc) == 0xf8 && CONT(1)&&CONT(2)&&CONT(3)&&CONT(4))
#define IS_UTF8_6BYTE(in) \
	((in[0]&0xfe) == 0xfc && CONT(1)&&CONT(2)&&CONT(3)&&CONT(4)&&CONT(5))


char *
_PL__utf8_get_char(const char *in, int *chr)
{ if ( IS_UTF8_2BYTE(in) )		/* 2-byte, 0x80-0x7ff */
  { *chr = ((in[0]&0x1f) << 6)|VAL(1,0);
    return (char *)in+2;
  }

  if ( IS_UTF8_3BYTE(in) )		/* 3-byte, 0x800-0xffff */
  { *chr = ((in[0]&0xf) << 12)|VAL(1,6)|VAL(2,0);
    return (char *)in+3;
  }

  if ( IS_UTF8_4BYTE(in) )		/* 4-byte, 0x10000-0x1FFFFF */
  { *chr = ((in[0]&0x7) << 18)|VAL(1,12)|VAL(2,6)|VAL(3,0);
    return (char *)in+4;
  }

  if ( IS_UTF8_5BYTE(in) )		/* 5-byte, 0x200000-0x3FFFFFF */
  { *chr = ((in[0]&0x3) << 24)|VAL(1,18)|VAL(2,12)|VAL(3,6)|VAL(4,0);
    return (char *)in+5;
  }

  if ( IS_UTF8_6BYTE(in) )		/* 6-byte, 0x400000-0x7FFFFFF */
  { *chr = ((in[0]&0x1) << 30)|VAL(1,24)|VAL(2,18)|VAL(3,12)|VAL(4,6)|VAL(5,0);
    return (char *)in+6;
  }

  *chr = (*in)&0xff;			/* Error */

  return (char *)in+1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_PL__utf8_type() finds the type that we  minimally need to represent the
data. This should be extended to accomodate Windows UTF-16 wchar_t
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

unicode_type_t
_PL__utf8_type(const char *in, size_t len)
{ const char *end = &in[len];
  int type = S_ASCII;

  while ( in < end )
  { int chr;
    in = utf8_get_char(in, &chr);

    if (chr > 255) return S_WIDE;
    if (chr > 127) type = S_LATIN;
  }

  return type;
}


char *
_PL__utf8_put_char(char *out, int chr)
{ if ( chr < 0x80 )
  { *out++ = chr;
  } else if ( chr < 0x800 )
  { *out++ = 0xc0|((chr>>6)&0x1f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x10000 )
  { *out++ = 0xe0|((chr>>12)&0x0f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x200000 )
  { *out++ = 0xf0|((chr>>18)&0x07);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x4000000 )
  { *out++ = 0xf8|((chr>>24)&0x03);
    *out++ = 0x80|((chr>>18)&0x3f);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( (unsigned)chr < 0x80000000 )
  { *out++ = 0xfc|((chr>>30)&0x01);
    *out++ = 0x80|((chr>>24)&0x3f);
    *out++ = 0x80|((chr>>18)&0x3f);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  }

  return out;
}


size_t
utf8_strlen(const char *s, size_t len)
{ const char *e = &s[len];
  unsigned int l = 0;

  while(s<e)
  { s = utf8_skip_char(s);
    l++;
  }

  return l;
}


size_t
utf8_strlen1(const char *s)
{ unsigned int l = 0;

  while(s[0])
  { s = utf8_skip_char(s);
    l++;
  }

  return l;
}


const char *
utf8_skip(const char *s, size_t n)
{ while(n--)
    s = utf8_skip_char(s);

  return s;
}


int
utf8_strncmp(const char *s1, const char *s2, size_t n)
{ while ( n-- > 0 )
  { int chr1, chr2;

    s1 = utf8_get_char(s1, &chr1);
    s2 = utf8_get_char(s2, &chr2);
    if (chr1-chr2) return chr1-chr2;
    if (!chr1) return 0;
  }

  return 0;
}
