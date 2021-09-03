/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2021, University of Amsterdam
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

#include <string.h>			/* get size_t */
#include "pl-utf8.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UTF-8 Decoding, based on http://www.cl.cam.ac.uk/~mgk25/unicode.html

_PL_utf8_code_point() advanced `i` but not beyond   `e`.  It returns the
number of bytes consumed or `-1`  if   the  sequence is invalid. In that
case it consumes one char.  Contributed by Abramo Bagnara.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
_PL__utf8_code_point(const char **i, const char *e, int *cp)
{ unsigned char c = (unsigned char)**i;
  int code;
  int n;

  ++(*i);
  *cp = c;
  if ( c < 0x80 )
    return 1;
  if ( c < 0xc0 )
    return -1;

  if ( c < 0xe0)
  { code = c & 0x1f;
    n = 1;
  } else if ( c < 0xf0 )
  { code = c & 0x0f;
    n = 2;
  } else if ( c < 0xf8 )
  { code = c & 0x07;
    n = 3;
  } else if ( c < 0xfc )
  { code = c & 0x03;
    n = 4;
  } else if (c < 0xfe)
  { code = c & 0x01;
    n = 5;
  } else
  { return -1;
  }

  for (int k = 0; k < n; ++k)
  { if ( *i + k == e )
      return -1;

    c = (*i)[k];
    if ( c < 0x80 || c >= 0xc0 )
      return -1;

    code = (code << 6) | (c & 0x3f);
  }

  *i += n;
  *cp = code;

  return n + 1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deprecated old API for decoding UTF-8 strings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
_PL__utf8_get_char(const char *in, int *chr)
{ const char *i = in;

  _PL__utf8_code_point(&i, NULL, chr);

  return (char*)i;
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
  { s = utf8_skip_char_e(s, e);
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
