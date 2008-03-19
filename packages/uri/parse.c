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


#include <SWI-Prolog.h>
#include <uriparser/Uri.h>

static functor_t FUNCTOR_uri8;
static atom_t    ATOM_;

static void
put_text_range(term_t t, UriTextRangeW *text)
{ if ( text->afterLast > text->first )
    PL_unify_wchars(t, PL_ATOM, text->afterLast - text->first, text->first);
  else
    PL_put_atom(t, ATOM_);
}


static foreign_t
parse_uri(term_t text, term_t parts)
{ UriParserStateW state = { 0 };
  UriUriW uri = { {0} };
  wchar_t *in;
  size_t len;

  if ( !PL_get_wchars(text, &len, &in, CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    return FALSE;

  state.uri = &uri;
  if ( uriParseUriW(&state, in) != URI_SUCCESS)
  { uriFreeUriMembersW(&uri);
    return FALSE;			/* TBD: Exception */
  }
  
  term_t av = PL_new_term_refs(8);
  put_text_range(av+0, &uri.scheme);

  term_t f = PL_new_term_ref();
  PL_cons_functor_v(f, FUNCTOR_uri8, av);

  uriFreeUriMembersW(&uri);

  return PL_unify(parts, f);
}


static void
install_parse()
{ FUNCTOR_uri8 = PL_new_functor(PL_new_atom("uri"), 8);
  ATOM_ = PL_new_atom("");

  PL_register_foreign("parse_uri", 2, parse_uri, 0);
}
