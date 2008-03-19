/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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


#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <string.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See http://xkr.us/articles/javascript/encode-compare/
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char escape_map[128];
static char escape_uri_map[128];
static char escape_uri_component_map[128];
static char hex[] = "0123456789ABCDEF";


static void
fill_map(char *map, const char *keep)
{ int c;

  memset(map,    FALSE, 128);

  for(c='0'; c<='9'; )
    map[c++] = TRUE;
  for(c='a'; c<='z'; )
    map[c++] = TRUE;
  for(c='A'; c<='Z'; )
    map[c++] = TRUE;

  while(*keep)
    map[(int)*keep++] = TRUE;
}


static void
init_maps()
{ fill_map(escape_map, "@*/+");
  fill_map(escape_uri_map, "~!@#$&*()=:/,;?+'");
  fill_map(escape_uri_component_map, "~!*()'");
}


static int
escape(term_t in, term_t out, term_t tail, const char *map)
{ char *s, *q;
  size_t len;
  size_t needs_escape;
  char tmp[256];
  char *buf, *o;
  int rc, flags;

  if ( !PL_get_nchars(in, &len, &s,
		      CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8) )
    return FALSE;

  for(needs_escape=0,q=s; *q; q++)
  { int c = *q & 0xff;

    if ( c < 128 && map[c] )
      continue;
    needs_escape++;
  }

  if ( needs_escape == 0 )
    return PL_unify(in, out);

  len += 2*needs_escape;

  if ( len < sizeof(tmp) )
  { buf = tmp;
  } else
  { buf = PL_malloc(len);
  }

  for(o=buf,q=s; *q; q++)
  { int c = *q & 0xff;

    if ( c < 128 && map[c] )
    { *o++ = c;
    } else
    { *o++ = '%';
      *o++ = hex[c>>4];
      *o++ = hex[c&0xf];
    }
  }

  flags = REP_ISO_LATIN_1;
  if ( tail )
  { term_t av = PL_new_term_refs(2);

    PL_put_term(av+0, out);
    rc = PL_unify_chars(av, REP_ISO_LATIN_1|PL_CODE_LIST|PL_DIFF_LIST, o-buf, buf);
    if ( rc )
      rc = PL_unify(tail, av+1);
  } else
  { rc = PL_unify_chars(out, REP_ISO_LATIN_1|PL_ATOM, o-buf, buf);
  }

  if ( buf != tmp )
    PL_free(buf);

  return rc;
}


static foreign_t
js_escape(term_t uri, term_t url)
{ return escape(uri, url, 0, escape_map);
}


static foreign_t
encode_uri(term_t uri, term_t url)
{ return escape(uri, url, 0, escape_uri_map);
}


static foreign_t
encode_uri_component(term_t uri, term_t url)
{ return escape(uri, url, 0, escape_uri_component_map);
}


static foreign_t
encode_uri3(term_t uri, term_t url, term_t tail)
{ return escape(uri, url, tail, escape_uri_map);
}


static foreign_t
encode_uri_component3(term_t uri, term_t url, term_t tail)
{ return escape(uri, url, tail, escape_uri_component_map);
}


		 /*******************************
		 *	       DECODING		*
		 *******************************/

static int
dehex(int c)
{ if ( c >= '0' && c <= '9' )
    return c - '0';
  if ( c >= 'A' && c <= 'F' )
    return c - ('A' - 10);
  if ( c >= 'a' && c <= 'f' )
    return c - ('a' - 10);

  return -1;
}


static int
non_ascii(term_t t)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "domain_error", 2,
		        PL_CHARS, "url",
			PL_TERM, t,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
decode(term_t in, term_t out, int plustoo)
{ char *s, *q;
  size_t len;
  char tmp[256];
  char *buf, *o;
  int rc;

  if ( !PL_get_nchars(in, &len, &s, CVT_LIST) )
  { if ( !PL_get_nchars(in, &len, &s,
			CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_ISO_LATIN_1) )
      return FALSE;
  }

  if ( len < sizeof(tmp) )
  { buf = tmp;
  } else
  { buf = PL_malloc(len+1);
  }

  for(o=buf,q=s; *q; q++)
  { int c = *q & 0xff;

    if ( c >= 128 )
      return non_ascii(in);
    if ( c == '%' )
    { int v1, v2;
      
      if ( (v1=dehex(*++q)) >= 0 && (v2=dehex(*++q)) >= 0 )
      { *o++ = (v1<<4)|v2;
      } else
      { return non_ascii(in);
      }
    } else if ( c == '+' && plustoo )
    { *o++ = ' ';
    } else
    { *o++ = c;
    }
  }
  *o = '\0';

  rc = PL_unify_chars(out, REP_UTF8|PL_ATOM, (size_t)-1, buf);

  if ( buf != tmp )
    PL_free(buf);

  return rc;
}


static foreign_t
unescape(term_t in, term_t out)
{ return decode(in, out, TRUE);
}


static foreign_t
decode_uri(term_t in, term_t out)
{ return decode(in, out, FALSE);
}


static void
install_escape()
{ init_maps();

/*  PL_register_foreign("escape", 2, js_escape, 0); */
  PL_register_foreign("encode_uri", 2, encode_uri, 0);
  PL_register_foreign("encode_uri_component", 2, encode_uri_component, 0);
  PL_register_foreign("encode_uri", 3, encode_uri3, 0);
  PL_register_foreign("encode_uri_component", 3, encode_uri_component3, 0);
/*  PL_register_foreign("unescape", 2, unescape, 0); */
  PL_register_foreign("decode_uri", 2, decode_uri, 0);
  PL_register_foreign("decode_uri_component", 2, decode_uri, 0);
}
