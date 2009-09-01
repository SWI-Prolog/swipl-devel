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
#include "turtle_chars.c"

		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;

static int
type_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_type_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
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

      return rc;
    }
  }
}


#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_turtle()
{ MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);

  PL_register_foreign("turtle_name_start_char",
		      			  1, turtle_name_start_char, 0);
  PL_register_foreign("turtle_name",      1, turtle_name,      0);
  PL_register_foreign("turtle_read_name", 4, turtle_read_name, 0);
}
