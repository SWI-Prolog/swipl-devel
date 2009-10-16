/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, VU University Amsterdam

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
#ifdef __WINDOWS__
#define inline __inline
#endif

#include "turtle_chars.c"

		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static atom_t	 ATOM_;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_syntax_error1;
static functor_t FUNCTOR_representation_error1;

static int
type_error(term_t actual, const char *expected)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_type_error2,
		         PL_CHARS, expected,
		         PL_TERM, actual,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
syntax_error(const char *culprit)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_syntax_error1,
		         PL_CHARS, culprit,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
representation_error(const char *culprit)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_representation_error1,
		         PL_CHARS, culprit,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


		 /*******************************
		 *	       PROLOG		*
		 *******************************/

/** turtle_name(+Atom) is semidet.

True if Atom is a valid Turtle identifier
*/

static inline int
wcis_name_char(int c)
{ return wcis_name_start_char(c) ||
         wcis_name_extender_char(c);
}

/** turtle_name_start_char(+Int) is semidet.
*/

static foreign_t
turtle_name_start_char(term_t Code)
{ int c;

  if ( !PL_get_integer(Code, &c) )
    return type_error(Code, "code");
  if ( !wcis_name_start_char(c) )
    return FALSE;

  return TRUE;
}


/** turtle_name(+Atom) is semidet.
*/

static foreign_t
turtle_name(term_t name)
{ char *s;
  pl_wchar_t *w;
  size_t len;

  if ( PL_get_nchars(name, &len, &s, CVT_ATOM) )
  { const char *e = &s[len];

    if ( !wcis_name_start_char(s[0]&0xff) )
      return FALSE;
    for(s++; s<e; s++)
    { if ( !wcis_name_char(s[0]&0xff) )
	return FALSE;
    }
    return TRUE;
  } else if ( PL_get_wchars(name, &len, &w, CVT_ATOM|CVT_EXCEPTION) )
  { const pl_wchar_t *e = &w[len];

    if ( !wcis_name_start_char(w[0]) )
      return FALSE;
    for(w++; w<e; w++)
    { if ( !wcis_name_char(w[0]) )
	return FALSE;
    }
    return TRUE;
  } else
    return FALSE;
}


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


static void
free_charbuf(charbuf *cb)
{ if ( cb->base != cb->tmp )
    PL_free(cb->base);
}


/** turtle_read_name(+C0, +Stream, -C, -Name) is semidet.
*/

static foreign_t
turtle_read_name(term_t C0, term_t Stream, term_t C, term_t Name)
{ int c;
  charbuf b;
  IOSTREAM *in;

  if ( !PL_get_integer(C0, &c) )
    return type_error(C0, "code");
  if ( !wcis_name_start_char(c) )
    return FALSE;

  if ( !PL_get_stream_handle(Stream, &in) )
    return FALSE;

  init_charbuf(&b);
  add_charbuf(&b, c);

  for(;;)
  { int c = Sgetcode(in);

    if ( wcis_name_char(c) )
    { add_charbuf(&b, c);
    } else
    { int rc = ( PL_unify_integer(C, c) &&
		 PL_unify_wchars(Name, PL_ATOM, b.here-b.base, b.base) );

      free_charbuf(&b);
      PL_release_stream(in);

      return rc;
    }
  }
}


static int
read_hN(IOSTREAM *in, int digits, int *value)
{ int d = digits;
  int v = 0;

  while ( d-- > 0 )
  { int c = Sgetcode(in);

    if ( c >= '0' && c <= '9' )
      v = (v<<4) + c - '0';
    else if ( c >= 'A' && c <= 'F' )
      v = (v<<4) + c + 10 - 'A';
    else if ( c >= 'a' && c <= 'f' )
      v = (v<<4) + c + 10 - 'a';
    else
    { if ( digits == 4 )
	return syntax_error("Illegal \\uNNNN in string");
      else
	return syntax_error("Illegal \\UNNNNNNNN in string");
    }
  }

  *value = v;
  return TRUE;
}


static int
string_escape(IOSTREAM *in, int c, int *value)
{ int esc;

  switch(c)
  { case 'n': esc = '\n'; break;
    case '"': esc = '"';  break;
    case '\\':esc = '\\'; break;
    case 't': esc = '\t'; break;
    case 'r': esc = '\r'; break;
    case 'u':
      if ( !read_hN(in, 4, &esc) )
	return FALSE;
      break;
    case 'U':
      if ( !read_hN(in, 8, &esc) )
	return FALSE;
      break;
    default:
      return syntax_error("illegal escape in string");
  }

  *value = esc;
  return TRUE;
}


/** turtle_read_string(+C0, +Stream, -C, -Value:atom) is semidet.
*/

static foreign_t
turtle_read_string(term_t C0, term_t Stream, term_t C, term_t Value)
{ int c;
  charbuf b;
  IOSTREAM *in;
  int endlen = 1;

  if ( !PL_get_integer(C0, &c) )
    return type_error(C0, "code");
  if ( c != '"' )
    return FALSE;

  if ( !PL_get_stream_handle(Stream, &in) )
    return FALSE;

  init_charbuf(&b);

  c = Sgetcode(in);
  if ( c == '"' )
  { c = Sgetcode(in);
    if ( c == '"' )			/* """...""" */
    { endlen = 3;
      c = Sgetcode(in);
    } else
    { PL_release_stream(in);
      return (PL_unify_integer(C, c) &&
	      PL_unify_atom(Value, ATOM_));
    }
  }

  for(;;c = Sgetcode(in))
  { if ( c == -1 )
    { free_charbuf(&b);
      PL_release_stream(in);
      return syntax_error("eof_in_string");
    } else if ( c == '"' )
    { int count = 1;

      for(count=1; count<endlen; )
      { if ( (c=Sgetcode(in)) == '"' )
	  count++;
	else
	  break;
      }

      if ( count == endlen )
      { int rc;

	c = Sgetcode(in);
	rc = (PL_unify_integer(C, c) &&
	      PL_unify_wchars(Value, PL_ATOM, b.here-b.base, b.base));
	free_charbuf(&b);
	PL_release_stream(in);
	return rc;
      }

      while(count-- > 0)
	add_charbuf(&b, '"');
      add_charbuf(&b, c);
    } else if ( c == '\\' )
    { int esc;

      c = Sgetcode(in);
      if ( !string_escape(in, c, &esc) )
      { free_charbuf(&b);
	PL_release_stream(in);
	return FALSE;
      }
      add_charbuf(&b, esc);
    } else
    { add_charbuf(&b, c);
    }
  }
}


/** turtle_read_relative_uri(+C0, +Stream, -C, -Value:atom) is semidet.
*/

static foreign_t
turtle_read_relative_uri(term_t C0, term_t Stream, term_t C, term_t Value)
{ int c;
  charbuf b;
  IOSTREAM *in;

  if ( !PL_get_integer(C0, &c) )
    return type_error(C0, "code");
  if ( c != '<' )
    return FALSE;

  if ( !PL_get_stream_handle(Stream, &in) )
    return FALSE;

  init_charbuf(&b);
  c = Sgetcode(in);
  for(; ; c = Sgetcode(in))
  { if ( c == '>' )
    { int rc;

      c = Sgetcode(in);
      rc = (PL_unify_integer(C, c) &&
	    PL_unify_wchars(Value, PL_ATOM, b.here-b.base, b.base));
      PL_release_stream(in);
      free_charbuf(&b);
      return rc;
    } else if ( c == '\\' )
    { int esc;

      c = Sgetcode(in);
      if ( c == '>' )
      { add_charbuf(&b, c);
      } else if ( string_escape(in, c, &esc) )
      { add_charbuf(&b, esc);
      } else
      { free_charbuf(&b);
	PL_release_stream(in);
	return FALSE;
      }
    } else if ( c == -1 )
    { free_charbuf(&b);
      PL_release_stream(in);
      return syntax_error("eof_in_uri");
    } else
    { add_charbuf(&b, c);
    }
  }
}

		 /*******************************
		 *	     WRITING		*
		 *******************************/

static int
ttl_put_uesc(IOSTREAM *s, int c)
{ if ( c <= 0xffff )
    return Sfprintf(s, "\\u%04x", (unsigned)c);
  else
    return Sfprintf(s, "\\U%08x", (unsigned)c);
}


static int
ttl_put_character(IOSTREAM *s, int c)
{ if ( c >= 32 && c <= 126 )
    return Sputcode(c, s);
  if ( c <= 31 )
    return ttl_put_uesc(s, c);
  if ( c >= 127 && c < 0x10ffff )
  { if ( s->encoding == ENC_ASCII )
      return ttl_put_uesc(s, c);
    if ( s->encoding == ENC_ISO_LATIN_1 && c > 255 )
      return ttl_put_uesc(s, c);
    return Sputcode(c, s);
  }

  representation_error("turtle_character");
  return -1;
}


static int
ttl_put_echaracter(IOSTREAM *s, int c)
{ int c2;

  switch(c)
  { case '\t': c2 = 't'; break;
    case '\n': c2 = 'n'; break;
    case '\r': c2 = 'r'; break;
    default:
      return ttl_put_character(s, c);
  }

  Sputcode('\\', s);

  return Sputcode(c2, s);
}


static int
ttl_put_scharacter(IOSTREAM *s, int c)
{ switch(c)
  { case '"':
      Sputcode('\\', s);
      return Sputcode('"', s);
    case '\\':
      Sputcode('\\', s);
      return Sputcode('\\', s);
    default:
      return ttl_put_echaracter(s, c);
  }
}


static foreign_t
turtle_write_quoted_string(term_t Stream, term_t Value)
{ size_t len;
  char *s;
  pl_wchar_t *w;
  IOSTREAM *out;

  if ( !PL_get_stream_handle(Stream, &out) )
    return FALSE;

  if ( PL_get_nchars(Value, &len, &s, CVT_ATOM|CVT_STRING) )
  { const char *e = &s[len];

    Sputcode('"', out);
    for(; s<e; s++)
    { if ( ttl_put_scharacter(out, s[0]&0xff) < 0 )
	break;
    }
    Sputcode('"', out);
    return PL_release_stream(out);
  } else if ( PL_get_wchars(Value, &len, &w, CVT_ATOM|CVT_EXCEPTION) )
  { const pl_wchar_t *e = &w[len];

    Sputcode('"', out);
    for(; w<e; w++)
    { if ( ttl_put_scharacter(out, w[0]) < 0 )
	break;
    }
    Sputcode('"', out);
    return PL_release_stream(out);
  } else
  { PL_release_stream(out);
    return FALSE;
  }
}


static int
ttl_put_ucharacter(IOSTREAM *s, int c)
{ switch(c)
  { case '>':
      Sputcode('\\', s);
      return Sputcode('>', s);
    default:
      return ttl_put_character(s, c);
  }
}


/** turtle_write_uri(+Stream, +URI) is det.
*/

static foreign_t
turtle_write_uri(term_t Stream, term_t Value)
{ size_t len;
  char *s;
  pl_wchar_t *w;
  IOSTREAM *out;

  if ( !PL_get_stream_handle(Stream, &out) )
    return FALSE;

  if ( PL_get_nchars(Value, &len, &s, CVT_ATOM|CVT_STRING) )
  { const char *e = &s[len];

    Sputcode('<', out);
    for(; s<e; s++)
    { if ( ttl_put_ucharacter(out, s[0]&0xff) < 0 )
	break;
    }
    Sputcode('>', out);
    return PL_release_stream(out);
  } else if ( PL_get_wchars(Value, &len, &w, CVT_ATOM|CVT_EXCEPTION) )
  { const pl_wchar_t *e = &w[len];

    Sputcode('<', out);
    for(; w<e; w++)
    { if ( ttl_put_ucharacter(out, w[0]) < 0 )
	break;
    }
    Sputcode('>', out);
    return PL_release_stream(out);
  } else
  { PL_release_stream(out);
    return FALSE;
  }
}



		 /*******************************
		 *	    REGISTRATION	*
		 *******************************/

#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)

install_t
install_turtle()
{ MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(syntax_error, 1);
  MKFUNCTOR(representation_error, 1);
  ATOM_ = PL_new_atom("");

  PL_register_foreign("turtle_name_start_char",
		      			    1, turtle_name_start_char, 0);
  PL_register_foreign("turtle_name",        1, turtle_name,        0);
  PL_register_foreign("turtle_read_name",   4, turtle_read_name,   0);
  PL_register_foreign("turtle_read_string", 4, turtle_read_string, 0);
  PL_register_foreign("turtle_read_relative_uri",
					    4, turtle_read_relative_uri, 0);
  PL_register_foreign("turtle_write_quoted_string",
					    2, turtle_write_quoted_string, 0);
  PL_register_foreign("turtle_write_uri",   2, turtle_write_uri,   0);
}
