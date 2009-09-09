/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University Amsterdam

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Prolog.h>
#include <string.h>
#include <stdio.h>
#include <wchar.h>
#include <wctype.h>
#include <assert.h>

static size_t removed_dot_segments(size_t len, const pl_wchar_t *in,
				   pl_wchar_t *out);
static pl_wchar_t *remove_last_segment(const pl_wchar_t *base,
				       const pl_wchar_t *o);
static char *_utf8_put_char(char *out, int chr);

#define ISUTF8_MB(c) ((unsigned)(c) >= 0xc0 && (unsigned)(c) <= 0xfd)
#define utf8_put_char(out, chr) \
	((chr) < 0x80 ? out[0]=(char)(chr), out+1 \
		      : _utf8_put_char(out, (chr)))


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static atom_t ATOM_query_value;
static atom_t ATOM_fragment;
static atom_t ATOM_path;

static functor_t FUNCTOR_equal2;	/* =/2 */
static functor_t FUNCTOR_pair2;		/* -/2 */
static functor_t FUNCTOR_uri_components5;
static functor_t FUNCTOR_uri_authority4;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_syntax_error1;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;


static int
syntax_error(const char *culprit)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_syntax_error1,
		        PL_CHARS, culprit,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
type_error(const char *expected, term_t found)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_type_error2,
		        PL_CHARS, expected,
			PL_TERM, found,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
domain_error(const char *expected, term_t found)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_domain_error2,
		        PL_CHARS, expected,
			PL_TERM, found,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


		 /*******************************
		 *	      ESCAPING		*
		 *******************************/

#define	ESC_PATH       (CH_PCHAR|CH_EX_PATH)
#define	ESC_QUERY      (CH_PCHAR|CH_EX_QF)
#define	ESC_QVALUE     (CH_UNRESERVED|CH_QSUBDELIM|CH_EX_PCHAR|CH_EX_QF)
#define	ESC_QNAME      (CH_PCHAR)
#define	ESC_FRAGMENT   (CH_PCHAR|CH_EX_QF)
#define	ESC_AUTH       (CH_PCHAR)
#define	ESC_PASSWD     (CH_PCHAR)
#define ESC_USER       (CH_PCHAR)
#define	ESC_SCHEME     (CH_SCHEME)
#define ESC_PORT       (CH_DIGIT)
#define ESC_HOST       (CH_UNRESERVED|CH_SUBDELIM)

#define CH_ALPHA      0x0001
#define CH_DIGIT      0x0002
#define CH_EX_UNRES   0x0004
#define CH_GENDELIM   0x0008
#define CH_SUBDELIM   0x0010
#define CH_URL	      0x0020
#define CH_EX_PCHAR   0x0040
#define CH_EX_QF      0x0080		/* Extra query and fragment chars */
#define CH_EX_SCHEME  0x0100
#define CH_QSUBDELIM  0x0200
#define CH_EX_PATH    0x0400

#define CH_SCHEME	(CH_ALPHA|CH_DIGIT|CH_EX_SCHEME)
#define CH_UNRESERVED	(CH_ALPHA|CH_DIGIT|CH_EX_UNRES)
#define CH_PCHAR	(CH_UNRESERVED|CH_SUBDELIM|CH_EX_PCHAR)

static int  charflags[128] = {0};
static int  flags_done = 0;

static void
set_flags(const char *from, int flag)
{ for(; *from; from++)
    charflags[from[0]&0xff] |= flag;
}

static void
fill_flags()
{ if ( !flags_done )
  { int c;

    for(c='a'; c<='z'; c++)
      charflags[c] |= CH_ALPHA;
    for(c='A'; c<='Z'; c++)
      charflags[c] |= CH_ALPHA;
    for(c='0'; c<='9'; c++)
      charflags[c] |= CH_DIGIT;

    set_flags("-._~",        CH_EX_UNRES);
    set_flags(":/?#[]@",     CH_GENDELIM);
    set_flags("!$&'()+*,;=", CH_SUBDELIM);
    set_flags("!$'()*,;",    CH_QSUBDELIM); /* = CH_SUBDELIM - "&=+" */
    set_flags(":@",          CH_EX_PCHAR);
    set_flags("/",           CH_EX_PATH);
    set_flags("/?",          CH_EX_QF);
    set_flags("+-.",	     CH_EX_SCHEME);

    set_flags("/:?#&=", CH_URL);

    flags_done = TRUE;
  }
}

#define no_escape(c, f) ((c < 128) && (charflags[(int)c] & (f)))
#define iri_no_escape(c, f) ((c > 128) || (charflags[(int)c] & (f)))


/* hex(const pl_wchar_t *in, int digits, int *value)

   Get <digits> characters from in and interpret them as a hexadecimal
   integer.  Returns pointer to the end on success or NULL if error.
*/

static const pl_wchar_t *
hex(const pl_wchar_t *in, int digits, int *value)
{ int v = 0;

  while(digits-- > 0)
  { int c = *in++;

    if ( c >= '0' && c <= '9' )
      v = (v<<4) + c - '0';
    else if ( c >= 'A' && c <= 'F' )
      v = (v<<4) + c + 10 - 'A';
    else if ( c >= 'a' && c <= 'f' )
      v = (v<<4) + c + 10 - 'a';
    else
      return NULL;
  }

  *value = v;
  return in;
}


static const pl_wchar_t *
get_encoded_utf8_cont_1(const pl_wchar_t *in, int *val)
{ int c;

  if ( in[0] == '%' && hex(in+1, 2, &c) )
  { if ( (c&0xc0) == 0x80 )
    { *val = (c&0x3f);
      return in+3;
    }
  }

  return NULL;
}


static const pl_wchar_t *
get_encoded_utf8_cont(const pl_wchar_t *in, int cnt, int *val)
{ int shift = cnt*6;

  *val <<= shift;
  shift -= 6;

  while(cnt-->0)
  { int v0;

    if ( (in = get_encoded_utf8_cont_1(in, &v0)) )
    { *val |= (v0<<shift);
      shift -= 6;
    } else
      return NULL;
  }

  return in;
}


static const pl_wchar_t *
get_encoded_utf8(const pl_wchar_t *in, int *chr)
{ int c1;

  if ( in[0] == '%' && hex(in+1, 2, &c1) )
  { in += 3;

    if ( ISUTF8_MB(c1) )
    { if ( (c1&0xe0) == 0xc0 )		/* 2-byte */
      { *chr = (c1&0x1f);
	return get_encoded_utf8_cont(in, 1, chr);
      } else if ( (c1&0xf0) == 0xe0 )	/* 3-byte */
      { *chr = (c1&0xf);
	return get_encoded_utf8_cont(in, 2, chr);
      } else if ( (c1&0xf8) == 0xf0 )	/* 4-byte */
      { *chr = (c1&0x7);
	return get_encoded_utf8_cont(in, 3, chr);
      } else if ( (c1&0xfc) == 0xf8 )	/* 5-byte */
      { *chr = (c1&0x3);
	return get_encoded_utf8_cont(in, 4, chr);
      } else if ( (c1&0xfe) == 0xfc )	/* 6-byte */
      { *chr = (c1&0x1);
	return get_encoded_utf8_cont(in, 5, chr);
      } else
	return NULL;
    } else
    { *chr = c1;
      return in;			/* Encoded ASCII character */
    }
  }

  return NULL;
}


		 /*******************************
		 *	      RANGES		*
		 *******************************/

typedef struct range
{ const pl_wchar_t *start;
  const pl_wchar_t *end;
} range;


		 /*******************************
		 *	 CHARACTER BUFFER	*
		 *******************************/

typedef struct charbuf
{ pl_wchar_t *base;
  pl_wchar_t *here;
  pl_wchar_t *end;
  pl_wchar_t tmp[256];
} charbuf;


static void
init_charbuf(charbuf *cb)
{ cb->base = cb->here = cb->tmp;
  cb->end = &cb->tmp[sizeof(cb->tmp)/sizeof(pl_wchar_t)];
}


static int
init_charbuf_at_size(charbuf *cb, size_t size)
{ size++;

  if ( size < sizeof(cb->tmp)/sizeof(pl_wchar_t) )
    cb->base = cb->here = cb->tmp;
  else
    cb->base = cb->here = PL_malloc(size*sizeof(pl_wchar_t));

  return TRUE;
}


static int
add_charbuf(charbuf *cb, int c)
{ if ( cb->here < cb->end )
  { *cb->here++ = c;
  } else
  { size_t len = (cb->end-cb->base);

    if ( cb->base == cb->tmp )
    { pl_wchar_t *n = PL_malloc(len*2*sizeof(pl_wchar_t));
      memcpy(n, cb->base, sizeof(cb->tmp));
      cb->base = n;
    } else
    { cb->base = PL_realloc(cb->base, len*2*sizeof(pl_wchar_t));
    }
    cb->here = &cb->base[len];
    cb->end = &cb->base[len*2];
    *cb->here++ = c;
  }

  return TRUE;
}


static inline int
hexdigit(int val)
{ if ( val < 10 )
    return '0'+val;
  return 'A'-10+val;
}


static int
add_encoded_charbuf(charbuf *cb, int c, int flags)
{ if ( no_escape(c, flags) )
  { add_charbuf(cb, c);
  } else
  { char tmp[6];
    const char *end = utf8_put_char(tmp, c);
    const char *s;

    for(s=tmp; s<end; s++)
    { int b = s[0]&0xff;

      add_charbuf(cb, '%');
      add_charbuf(cb, hexdigit(b>>4));
      add_charbuf(cb, hexdigit(b&0xf));
    }
  }

  return TRUE;
}


static int
iri_add_encoded_charbuf(charbuf *cb, int c, int flags)
{ if ( iri_no_escape(c, flags) )
  { add_charbuf(cb, c);
  } else
  { assert(c < 128);
    add_charbuf(cb, '%');
    add_charbuf(cb, hexdigit(c>>4));
    add_charbuf(cb, hexdigit(c&0xf));
  }

  return TRUE;
}



static int
add_nchars_charbuf(charbuf *cb, size_t len, const pl_wchar_t *s)
{ if ( cb->here+len <= cb->end )
  { wcsncpy(cb->here, s, len);
    cb->here += len;
  } else
  { size_t n;

    for(n=0; n<len; n++)
      add_charbuf(cb, s[n]);
  }

  return TRUE;
}


static int
range_has_escape(const range *r, int flags)
{ const pl_wchar_t *s = r->start;

  for(; s<r->end; s++)
  { if ( s[0] == '%' || (s[0] == '+' && flags == ESC_QVALUE) )
      return TRUE;
  }

  return FALSE;
}


static int
range_is_unreserved(const range *r, int iri, int flags)
{ const pl_wchar_t *s = r->start;

  if ( iri )
  { for(; s<r->end; s++)
    { if ( !iri_no_escape(s[0], flags) )
	return FALSE;
    }
  } else
  { for(; s<r->end; s++)
    { if ( !no_escape(s[0], flags) )
	return FALSE;
    }
  }

  return TRUE;
}


static int
add_verb_range_charbuf(charbuf *cb, const range *r)
{ return add_nchars_charbuf(cb, r->end-r->start, r->start);
}


static int
add_decoded_range_charbuf(charbuf *cb, const range *r, int flags)
{ const pl_wchar_t *s = r->start;

  while(s<r->end)
  { int c;

    if ( *s == '%' )
    { const pl_wchar_t *e;

      if ( (e=get_encoded_utf8(s, &c)) )
      { s = e;
      } else if (hex(s+1, 2, &c) )
      { s += 3;
      } else
      { c = *s++;
      }
    } else if ( *s == '+' && flags == ESC_QVALUE )
    { s++;
      c = ' ';
    } else
    { c = *s++;
    }

    add_charbuf(cb, c);
  }

  return TRUE;
}


static int
add_normalized_range_charbuf(charbuf *cb, const range *r, int iri, int flags)
{ const pl_wchar_t *s = r->start;

  while(s<r->end)
  { int c;

    if ( *s == '%' )
    { const pl_wchar_t *e;

      if ( (e=get_encoded_utf8(s, &c)) )
      { s = e;
      } else if (hex(s+1, 2, &c) )
      { s += 3;
      } else
      { c = *s++;
      }
    } else if ( *s == '+' && flags == ESC_QVALUE )
    { s++;
      c = ' ';
    } else
    { c = *s++;
    }

    if ( iri )
    { iri_add_encoded_charbuf(cb, c, flags);
    } else
    { add_encoded_charbuf(cb, c, flags);
    }
  }

  return TRUE;
}


/* add_range_charbuf(charbuf *cb, const range *r, int iri, int flags)

   Add a range of characters while normalizing %-encoding.  This
   implies not to use encoding if it is not needed and upcase
   %xx to %XX otherwise.

   If iri == TRUE, values >= 128 are not escaped.  Otherwise they
   use %-encoded UTF-8
*/

static int
add_range_charbuf(charbuf *cb, const range *r, int iri, int flags)
{ if ( range_has_escape(r, flags) )
  { return add_normalized_range_charbuf(cb, r, iri, flags);
  } else if ( range_is_unreserved(r, iri, flags) )
  { add_nchars_charbuf(cb, r->end-r->start, r->start);
  } else
  { const pl_wchar_t *s = r->start;

    if ( iri )
    { while(s<r->end)
	iri_add_encoded_charbuf(cb, *s++, flags);
    } else
    { while(s<r->end)
	add_encoded_charbuf(cb, *s++, flags);
    }
  }

  return TRUE;
}


/* add_lwr_range_charbuf(charbuf *cb, const range *r, int iri, int flags)

   Add a range of characters while normalizing %-encoding and
   mapping all characters to lowercase.

   FIXME: encoding and decoding compatible to add_range_charbuf();
*/


static int
add_lwr_range_charbuf(charbuf *cb, const range *r, int iri, int flags)
{ const pl_wchar_t *s = r->start;

  while(s<r->end)
  { int c;

    if ( *s == '%' )
    { const pl_wchar_t *e;

      if ( (e=get_encoded_utf8(s, &c)) )
      { s = e;
      } else if (hex(s+1, 2, &c) )
      { s += 3;
      } else
      { c = *s++;
      }
    } else
    { c = *s++;
    }

    add_encoded_charbuf(cb, towlower(c), flags);
  }

  return TRUE;
}


static void
free_charbuf(charbuf *cb)
{ if ( cb->base != cb->tmp )
    PL_free(cb->base);
}


#define TXT_EX_TEXT (CVT_ATOM|CVT_STRING|CVT_EXCEPTION)

static int
get_text_arg(term_t term, int pos, size_t *len, pl_wchar_t **s, int flags)
{ term_t tmp = PL_new_term_ref();

  _PL_get_arg(pos, term, tmp);
  if ( PL_is_variable(tmp) )
    return FALSE;
  if ( !PL_get_wchars(tmp, len, s, flags) )
    return -1;

  return TRUE;
}


/** uri_components(+URI, -Components)

Based on RFC-3986 regular expression:

    ==
    ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
     12            3  4          5       6  7        8 9
    ==
*/

typedef struct uri_component_ranges
{ range scheme;
  range authority;
  range path;
  range query;
  range fragment;
} uri_component_ranges;


static const pl_wchar_t *
skip_not(const pl_wchar_t *in, const pl_wchar_t *end, const pl_wchar_t *chars)
{ if ( !chars[1] )
  { for(; in < end; in++)
    { if ( chars[0] == in[0] )
	return in;
    }
  } else
  { for(; in < end; in++)
    { if ( wcschr(chars, in[0]) )
	return in;
    }
  }
  return in;
}


static int
unify_range(term_t t, const range *r)
{ if ( r->start )
    return PL_unify_wchars(t, PL_ATOM, r->end - r->start, r->start);

  return TRUE;
}


static int
parse_uri(uri_component_ranges *ranges, size_t len, const pl_wchar_t *s)
{ const pl_wchar_t *end = &s[len];
  const pl_wchar_t *here = s;
  const pl_wchar_t *e;

  memset(ranges, 0, sizeof(*ranges));

  e = skip_not(here, end, L":/?#");
  if ( e > s && e[0] == ':' )			/* 1&2 */
  { ranges->scheme.start = s;
    ranges->scheme.end = e;
    here = e+1;
  }

  if ( here[0] == '/' && here[1] == '/' )	/* 3 */
  { here += 2;				/* 4 */
    e = skip_not(here, end, L"/?#");
    ranges->authority.start = here;
    ranges->authority.end   = e;
    here = e;					/* 5 */
  }

  e = skip_not(here, end, L"?#");
  ranges->path.start = here;
  ranges->path.end   = e;
  here = e;					/* 6 */

  if ( here[0] == '?' )
  { here++;					/* 7 */
    e = skip_not(here, end, L"#");
    ranges->query.start = here;
    ranges->query.end = e;
    here = e;					/* 8 */
  }

  if ( here[0] == '#' )
  { here++;					/* 9 */
    ranges->fragment.start = here;
    ranges->fragment.end   = end;
  }

  return TRUE;
}


static foreign_t
uri_components(term_t URI, term_t components)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(URI, &len, &s, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { uri_component_ranges ranges;
    term_t rt = PL_new_term_refs(6);
    term_t av = rt+1;

    parse_uri(&ranges, len, s);

    unify_range(av+0, &ranges.scheme);
    unify_range(av+1, &ranges.authority);
    unify_range(av+2, &ranges.path);
    unify_range(av+3, &ranges.query);
    unify_range(av+4, &ranges.fragment);

    PL_cons_functor_v(rt, FUNCTOR_uri_components5, av);
    return PL_unify(components, rt);
  } else if ( PL_is_functor(components, FUNCTOR_uri_components5) )
  { charbuf b;
    int rc;

    init_charbuf(&b);
					/* schema */
    if ( (rc=get_text_arg(components, 1, &len, &s, TXT_EX_TEXT)) == TRUE )
    { add_nchars_charbuf(&b, len, s);
      add_charbuf(&b, ':');
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return FALSE;
    }
					/* authority */
    if ( (rc=get_text_arg(components, 2, &len, &s, TXT_EX_TEXT)) == TRUE )
    { add_charbuf(&b, '/');
      add_charbuf(&b, '/');
      add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return FALSE;
    }
					/* path */
    if ( (rc=get_text_arg(components, 3, &len, &s, TXT_EX_TEXT)) == TRUE )
    { add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return FALSE;
    }
					/* query */
    if ( (rc=get_text_arg(components, 4, &len, &s, TXT_EX_TEXT)) == TRUE )
    { add_charbuf(&b, '?');
      add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return FALSE;
    }
					/* fragment */
    if ( (rc=get_text_arg(components, 5, &len, &s, TXT_EX_TEXT)) == TRUE )
    { add_charbuf(&b, '#');
      add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return FALSE;
    }

    rc = PL_unify_wchars(URI, PL_ATOM, b.here-b.base, b.base);
    free_charbuf(&b);

    return rc;
  } else				/* generate an error */
  { return PL_get_wchars(URI, &len, &s,
			 CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION);
  }
}


/** uri_is_global(+URI) is semidet.
*/

static foreign_t
uri_is_global(term_t URI)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(URI, &len, &s,
		     CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { const pl_wchar_t *e;
    const pl_wchar_t *end = &s[len];
    range r;

    e = skip_not(s, end, L":/?#");
    if ( e > s && e[0] == ':' )
    { r.start = s;
      r.end = e;
      if ( range_is_unreserved(&r, TRUE, CH_SCHEME) )
	return TRUE;
    }
  }

  return FALSE;
}


		 /*******************************
		 *	   QUERY-STRING		*
		 *******************************/

static int
unify_decoded_atom(term_t t, range *r, int flags)
{ if ( range_has_escape(r, flags) )
  { charbuf b;
    int rc;

    init_charbuf(&b);
    add_decoded_range_charbuf(&b, r, flags);
    rc = PL_unify_wchars(t, PL_ATOM, b.here - b.base, b.base);
    free_charbuf(&b);
    return rc;
  } else
  { return unify_range(t, r);
  }
}


static int
unify_query_string_components(term_t list, size_t len, const pl_wchar_t *qs)
{ if ( len == 0 )
  { return PL_unify_nil(list);
  } else
  { term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();
    term_t eq   = PL_new_term_refs(3);
    term_t nv   = eq+1;
    const pl_wchar_t *end = &qs[len];

    while(qs < end)
    { range name, value;

      name.start = qs;
      name.end   = skip_not(qs, end, L"=");
      if ( name.end < end )
      { value.start = name.end+1;
	value.end   = skip_not(value.start, end, L"&");

	qs = value.end+1;
      } else
      { return syntax_error("illegal_uri_query");
      }

      PL_put_variable(nv+0);
      PL_put_variable(nv+1);
      unify_decoded_atom(nv+0, &name, ESC_QNAME);
      unify_decoded_atom(nv+1, &value, ESC_QVALUE);
      PL_cons_functor_v(eq, FUNCTOR_equal2, nv);

      if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify(head, eq) )
	return FALSE;
    }

    return PL_unify_nil(tail);
  }
}


static int
add_encoded_term_charbuf(charbuf *cb, term_t value, int flags)
{ pl_wchar_t *s;
  range r;
  size_t len;

  if ( !PL_get_wchars(value, &len, &s, CVT_ATOMIC|CVT_EXCEPTION) )
    return FALSE;

  r.start = s;
  r.end = r.start+len;
  if ( range_is_unreserved(&r, TRUE, flags) )
  { add_nchars_charbuf(cb, r.end-r.start, r.start);
  } else
  { const pl_wchar_t *s = r.start;

    while(s<r.end)
      add_encoded_charbuf(cb, *s++, flags);
  }

  return TRUE;
}


/** uri_query_components(+QueryString, -ValueList) is det.
*/

static foreign_t
uri_query_components(term_t string, term_t list)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(string, &len, &s, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { return  unify_query_string_components(list, len, s);
  } else if ( PL_is_list(list) )
  { term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();
    term_t nv   = PL_new_term_refs(2);
    charbuf out;
    int rc;

    fill_flags();
    init_charbuf(&out);
    while( PL_get_list(tail, head, tail) )
    { atom_t fname;
      int arity;

      if ( PL_is_functor(head, FUNCTOR_equal2) ||
	   PL_is_functor(head, FUNCTOR_pair2) )
      {	PL_get_arg(1, head, nv+0);
	PL_get_arg(2, head, nv+1);
      } else if ( PL_get_name_arity(head, &fname, &arity) && arity == 1 )
      { PL_put_atom(nv+0, fname);
	PL_get_arg(1, head, nv+1);
      } else
      { free_charbuf(&out);
	return type_error("name_value", head);
      }

      if ( out.here != out.base )
	add_charbuf(&out, '&');
      if ( !add_encoded_term_charbuf(&out, nv+0, ESC_QNAME) )
      { free_charbuf(&out);
	return FALSE;
      }
      add_charbuf(&out, '=');
      if ( !add_encoded_term_charbuf(&out, nv+1, ESC_QVALUE) )
      { free_charbuf(&out);
	return FALSE;
      }
    }

    rc = PL_unify_wchars(string, PL_ATOM, out.here-out.base, out.base);
    free_charbuf(&out);
    return rc;
  } else
  { return PL_get_wchars(string, &len, &s,
			 CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION);
  }

  return FALSE;
}


/** uri_encoded(+What, +String, -Encoded)
*/

static foreign_t
uri_encoded(term_t what, term_t qv, term_t enc)
{ pl_wchar_t *s;
  size_t len;
  atom_t w;
  int flags;

  if ( !PL_get_atom(what, &w) )
    return type_error("atom", what);
  if ( w == ATOM_query_value )
    flags = ESC_QVALUE;
  else if ( w == ATOM_fragment )
    flags = ESC_FRAGMENT;
  else if ( w == ATOM_path )
    flags = ESC_PATH;
  else
    return domain_error("uri_component", what);

  fill_flags();

  if ( !PL_is_variable(qv) )
  { charbuf out;
    int rc;

    init_charbuf(&out);
    if ( !add_encoded_term_charbuf(&out, qv, flags) )
    { free_charbuf(&out);
      return FALSE;
    }
    rc = PL_unify_wchars(enc, PL_ATOM, out.here-out.base, out.base);
    free_charbuf(&out);
    return rc;
  } else if ( PL_get_wchars(enc, &len, &s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
  { range r;

    r.start = s;
    r.end = s+len;

    return unify_decoded_atom(qv, &r, flags);
  } else
  { return FALSE;
  }
}


		 /*******************************
		 *	      AUTHORITY		*
		 *******************************/

static int
unify_uri_authority_components(term_t components,
			       size_t len, const pl_wchar_t *s)
{ const pl_wchar_t *end = &s[len];
  const pl_wchar_t *e;
  range user   = {0};
  range passwd = {0};
  range host   = {0};
  range port   = {0};
  term_t t = PL_new_term_refs(5);
  term_t av = t+1;

  if ( (e=skip_not(s, end, L"@")) && e<end )
  { user.start = s;
    user.end = e;
    s = e+1;
    if ( (e=skip_not(user.start, user.end, L":")) && e<user.end )
    { passwd.start = e+1;
      passwd.end   = user.end;
      user.end     = e;
    }
  }
  host.start = s;
  host.end = skip_not(s, end, L":");
  if ( host.end < end )
  { port.start = host.end+1;
    port.end = end;
  }

  if ( user.start )
    unify_decoded_atom(av+0, &user, ESC_USER);
  if ( passwd.start )
    unify_decoded_atom(av+1, &passwd, ESC_PASSWD);
  unify_decoded_atom(av+2, &host, ESC_HOST);
  if ( port.start )
  { wchar_t *ep;
    long pn = wcstol(port.start, &ep, 10);

    if ( ep == port.end )
      PL_put_integer(av+3, pn);
    else
      unify_decoded_atom(av+3, &port, ESC_PORT);
  }

  PL_cons_functor_v(t, FUNCTOR_uri_authority4, av);
  return PL_unify(components, t);
}


/** uri_authority_components(+Authority, -Components) is det.
    uri_authority_components(-Authority, +Components) is det.
*/

static foreign_t
uri_authority_components(term_t Authority, term_t components)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(Authority, &len, &s, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { return  unify_uri_authority_components(components, len, s);
  } else if ( PL_is_functor(components, FUNCTOR_uri_authority4) )
  { charbuf b;
    int rc;

    init_charbuf(&b);
    if ( (rc=get_text_arg(components, 1, &len, &s, TXT_EX_TEXT)) == TRUE )
    { add_nchars_charbuf(&b, len, s);
      if ( (rc=get_text_arg(components, 2, &len, &s, TXT_EX_TEXT)) == TRUE )
      { add_charbuf(&b, ':');
	add_nchars_charbuf(&b, len, s);
      } else if ( rc == -1 )
      { free_charbuf(&b);
	return FALSE;
      }
      add_charbuf(&b, '@');
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return FALSE;
    }
    if ( (rc=get_text_arg(components, 3, &len, &s, TXT_EX_TEXT)) == TRUE )
    { add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return FALSE;
    }
    if ( (rc=get_text_arg(components, 4, &len, &s,
			  TXT_EX_TEXT|CVT_INTEGER)) == TRUE )
    { add_charbuf(&b, ':');
      add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return FALSE;
    }

    rc = PL_unify_wchars(Authority, PL_ATOM, b.here-b.base, b.base);
    free_charbuf(&b);

    return rc;
  } else
  { return PL_get_wchars(Authority, &len, &s,
			 CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION);
  }
}



		 /*******************************
		 *	  NORMALIZATION		*
		 *******************************/

static int
normalize_in_charbuf(charbuf *cb, uri_component_ranges *ranges, int iri)
{ fill_flags();

  if ( ranges->scheme.start )
  { add_lwr_range_charbuf(cb, &ranges->scheme, iri, ESC_SCHEME);
    add_charbuf(cb, ':');
  }
  if ( ranges->authority.start )
  { add_charbuf(cb, '/');
    add_charbuf(cb, '/');
    add_lwr_range_charbuf(cb, &ranges->authority, iri, ESC_AUTH);
  }
  if ( ranges->path.end > ranges->path.start )
  { charbuf pb;
    charbuf path;
    size_t len;

    init_charbuf(&pb);
    add_range_charbuf(&pb, &ranges->path, iri, ESC_PATH);
    init_charbuf_at_size(&path, pb.here-pb.base);
    len = removed_dot_segments(pb.here-pb.base, pb.base, path.base);
    add_nchars_charbuf(cb, len, path.base);
    free_charbuf(&path);
    free_charbuf(&pb);
  }
  if ( ranges->query.start )
  { add_charbuf(cb, '?');
    add_range_charbuf(cb, &ranges->query, iri, ESC_QUERY);
  }
  if ( ranges->fragment.start )
  { add_charbuf(cb, '#');
    add_range_charbuf(cb, &ranges->fragment, iri, ESC_FRAGMENT);
  }

  return TRUE;
}


static foreign_t
normalized(term_t URI, term_t CannonicalURI, int iri)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(URI, &len, &s,
		     CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { uri_component_ranges ranges;
    charbuf b;
    int rc;

    parse_uri(&ranges, len, s);
    init_charbuf(&b);
    normalize_in_charbuf(&b, &ranges, iri);

    rc = PL_unify_wchars(CannonicalURI, PL_ATOM, b.here-b.base, b.base);
    free_charbuf(&b);

    return rc;
  }

  return FALSE;
}


/** uri_normalized(+URI, -CannonicalURI)
*/

static foreign_t
uri_normalized(term_t URI, term_t CannonicalURI)
{ return normalized(URI, CannonicalURI, FALSE);
}


/** uri_normalized_iri(+URI, -CannonicalIRI)
*/

static foreign_t
uri_normalized_iri(term_t URI, term_t CannonicalURI)
{ return normalized(URI, CannonicalURI, TRUE);
}


static int
ranges_in_charbuf(charbuf *cb, uri_component_ranges *ranges)
{ if ( ranges->scheme.start )
  { add_verb_range_charbuf(cb, &ranges->scheme);
    add_charbuf(cb, ':');
  }
  if ( ranges->authority.start )
  { add_charbuf(cb, '/');
    add_charbuf(cb, '/');
    add_verb_range_charbuf(cb, &ranges->authority);
  }
  add_verb_range_charbuf(cb, &ranges->path);
  if ( ranges->query.start )
  { add_charbuf(cb, '?');
    add_verb_range_charbuf(cb, &ranges->query);
  }
  if ( ranges->fragment.start )
  { add_charbuf(cb, '#');
    add_verb_range_charbuf(cb, &ranges->fragment);
  }

  return TRUE;
}


static foreign_t
resolve(term_t Rel, term_t Base, term_t URI, int normalize, int iri)
{ pl_wchar_t *s;
  size_t slen;
  pl_wchar_t *b;
  size_t blen;
  uri_component_ranges s_ranges, t_ranges;
  int rc;
  size_t len;
  charbuf out, pb, path;

  init_charbuf(&pb);			/* path-buffer */

  if ( PL_get_wchars(Rel, &slen, &s,
		     CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { parse_uri(&s_ranges, slen, s);
    if ( s_ranges.scheme.start )
    { t_ranges = s_ranges;
    } else
    { if ( PL_get_wchars(Base, &blen, &b,
			 CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
      { uri_component_ranges b_ranges;

	parse_uri(&b_ranges, blen, b);
	memset(&t_ranges, 0, sizeof(t_ranges));
	if ( s_ranges.authority.start )
	{ t_ranges.authority = s_ranges.authority;
	  t_ranges.path      = s_ranges.path;
	  t_ranges.query     = s_ranges.query;
	} else
	{ if ( s_ranges.path.start == s_ranges.path.end )
	  { t_ranges.path = b_ranges.path;
	    if ( s_ranges.query.start )
	      t_ranges.query = s_ranges.query;
	    else
	      t_ranges.query = b_ranges.query;
	  } else
	  { if ( s_ranges.path.start[0] == '/' )
	    { t_ranges.path = s_ranges.path;
	    } else
	    { if ( b_ranges.authority.start &&
		   b_ranges.path.start == b_ranges.path.end )
	      { add_charbuf(&pb, '/');
		add_verb_range_charbuf(&pb, &s_ranges.path);
	      } else
	      { b_ranges.path.end = remove_last_segment(b_ranges.path.start,
							b_ranges.path.end);
		add_verb_range_charbuf(&pb, &b_ranges.path);
		add_verb_range_charbuf(&pb, &s_ranges.path);
		t_ranges.path.start = pb.base;
		t_ranges.path.end = pb.here;
	      }
	    }
	    t_ranges.query = s_ranges.query;
	  }
	  t_ranges.authority = b_ranges.authority;
	}
	t_ranges.scheme = b_ranges.scheme;
	t_ranges.fragment = s_ranges.fragment;
      } else
	return FALSE;			/* type error */
    }
  } else
    return FALSE;

  init_charbuf(&out);			/* output buffer */

  if ( normalize )
  { normalize_in_charbuf(&out, &t_ranges, iri);
  } else
  { init_charbuf_at_size(&path, t_ranges.path.end - t_ranges.path.start);
    len = removed_dot_segments(t_ranges.path.end - t_ranges.path.start,
			       t_ranges.path.start,
			       path.base);
    t_ranges.path.start = path.base;
    t_ranges.path.end   = path.base+len;
    free_charbuf(&pb);

    ranges_in_charbuf(&out, &t_ranges);
  }

  rc = PL_unify_wchars(URI, PL_ATOM, out.here-out.base, out.base);
  free_charbuf(&out);

  return rc;
}

/** uri_resolve(+Relative, +Base, -Absolute) is det.
*/

static foreign_t
uri_resolve(term_t Rel, term_t Base, term_t URI)
{ return resolve(Rel, Base, URI, FALSE, FALSE);
}


/** uri_normalized(+Relative, +Base, -Absolute) is det.
*/

static foreign_t
uri_normalized3(term_t Rel, term_t Base, term_t URI)
{ return resolve(Rel, Base, URI, TRUE, FALSE);
}


/** uri_normalized_iri(+Relative, +Base, -Absolute) is det.
*/

static foreign_t
uri_normalized_iri3(term_t Rel, term_t Base, term_t IRI)
{ return resolve(Rel, Base, IRI, TRUE, TRUE);
}


		 /*******************************
		 *	    PATH LOGIC		*
		 *******************************/

/* http://labs.apache.org/webarch/uri/rfc/rfc3986.html#relative-dot-segments
*/

static pl_wchar_t *
remove_last_segment(const pl_wchar_t *base, const pl_wchar_t *o)
{ while(o>base && o[-1] != '/' )
    o--;

  return (pl_wchar_t*) o;
}


static inline int
fetch(const pl_wchar_t *in, const pl_wchar_t *end, int at)
{ if ( in+at>=end )
    return 0;
  return in[at];
}

static size_t
removed_dot_segments(size_t len, const pl_wchar_t *in, pl_wchar_t *out)
{ const pl_wchar_t *end = &in[len];
  pl_wchar_t *o = out;

  while(in<end)
  { if ( in[0] == '.' )
    { if ( fetch(in, end, 1) == '/' ||
	   (fetch(in, end, 1) == '.' && fetch(in, end, 2) == '/') )
      { in += 2;			/* 2A */
	continue;
      }
    }
    if ( in[0] == '/' && fetch(in, end, 1) == '.' )
    { if ( fetch(in, end, 2) == '/' )
      { in += 2;			/* 2B "/./" --> "/" */
	continue;
      }
      if ( !fetch(in, end, 2) )
      { *o++ = '/';			/* 2B "/." --> "/" (and close) */
	in += 2;
	continue;
      }
      if ( fetch(in, end, 2) == '.' )
      { if ( fetch(in, end, 3) == '/' )
	{ in += 3;			/* 2C "/../" --> "/" */
	  o = remove_last_segment(out, o);
	  if ( o>out ) o--;		/* delete / */
	  continue;
	}
	if ( !fetch(in, end, 3) )
	{ o = remove_last_segment(out, o);
	  if ( o>out ) o--;		/* delete / */
	  *o++ = '/';
	  in += 3;
	  continue;
	}
      }
    }
    if ( in[0] == '.' )
    { if ( !fetch(in, end, 1) )
      { in++;				/* 3D */
	continue;
      }
      if ( fetch(in, end, 1) == '.' && !fetch(in, end, 2) )
      { in += 2;			/* 3D */
	continue;
      }
    }
    if ( in[0] == '/' )
      *o++ = *in++;
    while( in < end && in[0] != '/' )
      *o++ = *in++;
  }

  return o-out;
}


		 /*******************************
		 *	    IRI HANDLING	*
		 *******************************/

#define utf8_put_char(out, chr) \
	((chr) < 0x80 ? out[0]=(char)(chr), out+1 \
		      : _utf8_put_char(out, (chr)))


static char *
_utf8_put_char(char *out, int chr)
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


/** uri_iri(+URI, -IRI) is det.
    uri_iri(-URI, +IRI) is det.

Perform %- and UTF-8 encoding/decoding to translate between a URI and
IRI
*/

static foreign_t
uri_iri(term_t URI, term_t IRI)
{ pl_wchar_t *uri;
  char *iri;
  size_t ulen, ilen;

  if ( PL_get_wchars(URI, &ulen, &uri, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { char tmp[1024];
    char *out;
    const pl_wchar_t *in;
    int rc;

    if ( ulen*6 > sizeof(tmp) )
      iri = PL_malloc(ulen*6);		/* 6 is max UTF-8 sequence lenght */
    else
      iri = tmp;

    for(in=uri,out=iri; in<&uri[ulen];)
    { if ( in[0] == '%' )
      { int c;

	if ( in+2 < &uri[ulen] && (in = hex(in+1, 2, &c)) )
	{ *out++ = c;
	} else
	{ if ( iri != tmp )
	    PL_free(iri);
	  return syntax_error("illegal_percent_sequence");
	}
      } else
      { out = utf8_put_char(out, in[0]);
	in++;
      }
    }

    rc = PL_unify_chars(IRI, PL_ATOM|REP_UTF8, out-iri, iri);
    if ( iri != tmp )
      PL_free(iri);

    return rc;
  } else if ( PL_get_nchars(IRI, &ilen, &iri, CVT_ATOM|CVT_STRING|CVT_LIST|REP_UTF8) )
  { char tmp[1024];
    char *uri, *out;
    const char *in;
    int rc;

    if ( ilen*3 > sizeof(tmp) )
      uri = PL_malloc(ilen*3);		/* percent-encoding is max *3 */
    else
      uri = tmp;

    fill_flags();

    for(out=uri, in=iri; in < &iri[ilen]; in++)
    { int c = in[0]&0xff;

      if ( no_escape(c, CH_UNRESERVED|CH_URL) )
      { *out++ = c;
      } else
      { sprintf(out, "%%%02x", c);
	out += 3;
      }
    }

    rc = PL_unify_chars(URI, PL_ATOM, out-uri, uri);

    if ( uri != tmp )
      PL_free(uri);

    return rc;
  } else return PL_get_wchars(URI, &ulen, &uri,
			      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION);
}


		 /*******************************
		 *	   REGISTRATION		*
		 *******************************/

#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_uri()
{ MKATOM(query_value);
  MKATOM(fragment);
  MKATOM(path);

  MKFUNCTOR(uri_components, 5);
  MKFUNCTOR(uri_authority, 4);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(syntax_error, 1);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(domain_error, 2);
  FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_pair2 = PL_new_functor(PL_new_atom("-"), 2);

  PL_register_foreign("uri_components",	      2, uri_components,       0);
  PL_register_foreign("uri_is_global",	      1, uri_is_global,	       0);
  PL_register_foreign("uri_normalized",	      2, uri_normalized,       0);
  PL_register_foreign("uri_normalized_iri",   2, uri_normalized_iri,   0);
  PL_register_foreign("uri_resolve",	      3, uri_resolve,	       0);
  PL_register_foreign("uri_normalized",	      3, uri_normalized3,      0);
  PL_register_foreign("uri_normalized_iri",   3, uri_normalized_iri3,  0);
  PL_register_foreign("uri_query_components", 2, uri_query_components, 0);
  PL_register_foreign("uri_authority_components",
					      2, uri_authority_components, 0);
  PL_register_foreign("uri_encoded",	      3, uri_encoded,	       0);
  PL_register_foreign("uri_iri",	      2, uri_iri,	       0);
}
