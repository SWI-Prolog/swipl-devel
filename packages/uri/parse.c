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
#include <stdlib.h>
#include <uriparser/Uri.h>

static functor_t FUNCTOR_uri7;
static atom_t    ATOM_;

static void
put_text_range(term_t t, UriTextRangeW *text)
{ if ( text->afterLast > text->first )
    PL_unify_wchars(t, PL_ATOM, text->afterLast - text->first, text->first);
  else
    PL_put_atom(t, ATOM_);
}


static void
put_num_text_range(term_t t, UriTextRangeW *text)
{ if ( text->afterLast > text->first )
  { unsigned long v;
    wchar_t *e;

    v = wcstoul(text->first, &e, 10);
    if ( e == text->afterLast )
      PL_put_integer(t, v);
    else
      PL_unify_wchars(t, PL_ATOM, text->afterLast - text->first, text->first);
  }
}



static void
put_path_segments(term_t t, UriPathSegmentW *segment)
{ size_t len = 0;
  UriPathSegmentW *s;
  wchar_t tmp[256];
  wchar_t *buf, *o;

  for(s=segment; s; s=s->next)
  { len += (s->text.afterLast -  s->text.first) + 1;
  }
  if ( len == 0 )
    len = 1;				/* single / */

  if ( len < 256 )
    buf = tmp;
  else
    buf = PL_malloc((len+1)*sizeof(wchar_t));

  for(o=buf,s=segment; s; s=s->next)
  { size_t l = s->text.afterLast -  s->text.first;

    *o++ = '/';
    wcsncpy(o, s->text.first, l);
    o+= l;
  }
  if ( o == buf )
    *o++ = '/';

  PL_unify_wchars(t, PL_ATOM, o-buf, buf);
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
  
  term_t av = PL_new_term_refs(7);
  put_text_range(av+0, &uri.scheme);
  put_text_range(av+1, &uri.userInfo);
  put_text_range(av+2, &uri.hostText);
  put_num_text_range(av+3, &uri.portText);
  put_path_segments(av+4, uri.pathHead);
  put_text_range(av+5, &uri.query);
  put_text_range(av+6, &uri.fragment);

  term_t f = PL_new_term_ref();
  PL_cons_functor_v(f, FUNCTOR_uri7, av);

  uriFreeUriMembersW(&uri);

  return PL_unify(parts, f);
}


static void
install_parse()
{ FUNCTOR_uri7 = PL_new_functor(PL_new_atom("uri"), 7);
  ATOM_ = PL_new_atom("");

  PL_register_foreign("parse_uri", 2, parse_uri, 0);
}
