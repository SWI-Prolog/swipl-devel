/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
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
static functor_t FUNCTOR_eq2;		/* =/2 */
static atom_t    ATOM_;
static atom_t	 ATOM_normalize;
static atom_t	 ATOM_base;

static void
put_text_range(term_t t, UriTextRangeA *text)
{ if ( text->afterLast > text->first )
    PL_unify_chars(t, PL_ATOM, text->afterLast - text->first, text->first);
  else
    PL_put_atom(t, ATOM_);
}


static void
put_num_text_range(term_t t, UriTextRangeA *text)
{ if ( text->afterLast > text->first )
  { unsigned long v;
    char *e;

    v = strtoul(text->first, &e, 10);
    if ( e == text->afterLast )
      PL_put_integer(t, v);
    else
      PL_unify_chars(t, PL_ATOM, text->afterLast - text->first, text->first);
  }
}



static void
put_path_segments(term_t t, UriPathSegmentA *segment)
{ size_t len = 0;
  UriPathSegmentA *s;
  char tmp[256];
  char *buf, *o;

  for(s=segment; s; s=s->next)
  { len += (s->text.afterLast -  s->text.first) + 1;
  }
  if ( len == 0 )
    len = 1;				/* single / */

  if ( len < 256 )
    buf = tmp;
  else
    buf = PL_malloc((len+1)*sizeof(char));

  for(o=buf,s=segment; s; s=s->next)
  { size_t l = s->text.afterLast -  s->text.first;

    *o++ = '/';
    strncpy(o, s->text.first, l);
    o+= l;
  }
  if ( o == buf )
    *o++ = '/';

  PL_unify_chars(t, PL_ATOM, o-buf, buf);
}


static int
put_query(term_t t, UriUriA *uri)
{ if ( uri->query.first == uri->query.afterLast )
  { PL_put_nil(t);
    return TRUE;
  } else
  { UriQueryListA *ql, *qe;
    int itemCount;

    if ( uriDissectQueryMallocA(&ql, &itemCount,
				uri->query.first, uri->query.afterLast) == URI_SUCCESS)
    { term_t tail = PL_copy_term_ref(t);
      term_t head = PL_new_term_ref();
      term_t qel  = PL_new_term_refs(3);
      int i;

      for(i=0,qe=ql; i<itemCount; i++,qe=qe->next)
      { PL_unify_list(tail, head, tail);
	if ( qe->value )
	{ PL_put_atom_chars(qel+1, qe->key);
	  PL_put_atom_chars(qel+2, qe->value);
	  PL_cons_functor_v(qel+0, FUNCTOR_eq2, qel+1);
	  PL_unify(head, qel+0);
	} else
	{ PL_put_atom_chars(qel, qe->key);
	  PL_unify(head, qel);
	}
      }      
      PL_unify_nil(tail);
    } else
    { return FALSE;			/* TBD: exception */
    }

    uriFreeQueryListA(ql);
    return TRUE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
parse_uri(+Text, -URI:term, +Options) is det.

Options include:

	* normalize(+Bool)
	* base(+BaseURI)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static int
parse_uri_options(UriParserStateA *state, UriUriA *uri, term_t options)
{ if ( options && !PL_get_nil(options) )
  { int normalize = FALSE;
    char *base = NULL;
    term_t tail  = PL_copy_term_ref(options);
    term_t head  = PL_new_term_ref();
    term_t ov    = PL_new_term_ref();
    term_t bterm = 0;

    while( PL_get_list(tail, head, tail) )
    { atom_t oname;
      int oarity;

      if ( !PL_get_name_arity(head, &oname, &oarity) || oarity != 1 )
	return type_error(head, "option");
      PL_get_arg(1, head, ov);

      if ( oname == ATOM_normalize )
      { if ( !PL_get_bool(ov, &normalize) )
	  return type_error(ov, "bool");
      } else if ( oname == ATOM_base )
      { if ( !PL_get_chars(ov, &base, CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
	  return FALSE;
	bterm = PL_copy_term_ref(ov);
      } else
      { return domain_error(head, "parse_uri_option");
      }
    }
    if ( !PL_get_nil(tail) )
      return type_error(tail, "list");

    if ( base )
    { UriUriA buri = { {0} };
      UriUriA absuri = { {0} };
      
      state->uri = &buri;
      if ( uriParseUriA(state, base) != URI_SUCCESS)
      { uriFreeUriMembersA(&buri);
	return syntax_error(bterm, "uri");
      }

      if ( uriAddBaseUriA(&absuri, uri, &buri) != URI_SUCCESS)
      { uriFreeUriMembersA(&buri);
	uriFreeUriMembersA(&absuri);
	return FALSE;			/* TBD: Exception */
      }

      uriFreeUriMembersA(&buri);
      uriFreeUriMembersA(uri);
      *uri = absuri;
    }
    if ( normalize )
    { if ( uriNormalizeSyntaxA(uri) != URI_SUCCESS )
	return FALSE;			/* TBD: error */
    }
  }

  return TRUE;
}


static int
get_uri(term_t text, UriParserStateA *state, UriUriA *uri, term_t options)
{ char *in;

  if ( !PL_get_chars(text, &in, CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    return FALSE;

  state->uri = uri;
  if ( uriParseUriA(state, in) != URI_SUCCESS)
  { uriFreeUriMembersA(uri);
    return syntax_error(text, "uri");
  }
  
  if ( !parse_uri_options(state, uri, options) )
    return FALSE;

  return TRUE;
}


static foreign_t
parse_uri(term_t text, term_t parts, term_t options)
{ UriParserStateA state = { 0 };
  UriUriA uri = { {0} };

  if ( !get_uri(text, &state, &uri, options) )
    return FALSE;

  term_t av = PL_new_term_refs(7);
  put_text_range(av+0, &uri.scheme);
  put_text_range(av+1, &uri.userInfo);
  put_text_range(av+2, &uri.hostText);
  put_num_text_range(av+3, &uri.portText);
  put_path_segments(av+4, uri.pathHead);
  put_query(av+5, &uri);
  put_text_range(av+6, &uri.fragment);

  term_t f = PL_new_term_ref();
  PL_cons_functor_v(f, FUNCTOR_uri7, av);

  uriFreeUriMembersA(&uri);

  return PL_unify(parts, f);
}


static foreign_t
is_absolute_uri(term_t text)
{ UriParserStateA state = { 0 };
  UriUriA uri = { {0} };
  int rc;

  if ( !get_uri(text, &state, &uri, 0) )
    return FALSE;
  rc = (uri.scheme.first < uri.scheme.afterLast);
  uriFreeUriMembersA(&uri);

  return rc;
}


static foreign_t
uri_iri(term_t uri_string, term_t iri, term_t options)
{ if ( !PL_is_variable(uri_string) )
  { UriParserStateA state = { 0 };
    UriUriA uri = { {0} };
    int charsRequired;

    if ( !get_uri(uri_string, &state, &uri, options) )
      return FALSE;
    if ( uriToStringCharsRequiredA(&uri, &charsRequired) == URI_SUCCESS)
    { char tmp[1024];
      char *buf;
      int rc;

      charsRequired++;
      if ( charsRequired > sizeof(tmp) )
	buf = malloc(charsRequired);
      else
	buf = tmp;

      rc = uriToStringA(buf, &uri, charsRequired, NULL);
      uriFreeUriMembersA(&uri);

      if ( rc == URI_SUCCESS )
      { rc = decode_chars(charsRequired-1, buf, iri, FALSE);
      } else
      { rc = FALSE;
      }
      if ( buf && buf != tmp )
	free(buf);

      return rc;
    }
  } else
  { assert(0);
  }

  return FALSE;
}


static void
install_parse()
{ FUNCTOR_uri7 = PL_new_functor(PL_new_atom("uri"), 7);
  FUNCTOR_eq2  = PL_new_functor(PL_new_atom("="), 2);
  ATOM_ = PL_new_atom("");
  ATOM_normalize = PL_new_atom("normalize");
  ATOM_base = PL_new_atom("base");

  PL_register_foreign("parse_uri",       3, parse_uri,       0);
  PL_register_foreign("uri_iri",         3, uri_iri,         0);
  PL_register_foreign("is_absolute_uri", 1, is_absolute_uri, 0);
}
