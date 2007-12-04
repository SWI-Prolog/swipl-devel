/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

#define TRYPUTC(c, s) if ( Sputcode(c, s) < 0 ) { rc = FALSE; goto out; }

static foreign_t
json_write_string(term_t stream, term_t text)
{ IOSTREAM *out;
  char *a;
  pl_wchar_t *w;
  size_t len;
  int rc = TRUE;
  static char escape[128];
  static int escape_initialized = FALSE;

  if ( !escape_initialized )
  { memset(escape, 0, sizeof(escape));

    escape['"']  = '"';
    escape['\\'] = '\\';
    escape['\b'] = 'b';
    escape['\f'] = 'f';
    escape['\n'] = 'n';
    escape['\r'] = 'r';
    escape['\t'] = 't';

    escape_initialized = TRUE;
  }

  if ( !PL_get_stream_handle(stream, &out) )
    return FALSE;

  if ( PL_get_nchars(text, &len, &a, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { const char *ap;
    size_t todo;

    TRYPUTC('"', out);
    for(todo=len, ap=a; todo-- > 0; ap++)
    { int c = *ap&0xff;

      if ( c < 128 && escape[c] && !(c=='\n' && todo >= 40) )
      { TRYPUTC('\\', out);
	TRYPUTC(escape[c], out);
      } else
      { TRYPUTC(c, out);
      }
    }
    TRYPUTC('"', out);
  } else if ( PL_get_wchars(text, &len, &w, CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { const pl_wchar_t *wp;
    size_t todo;

    TRYPUTC('"', out);
    for(todo=len, wp=w; todo-- > 0; wp++)
    { int c = *wp;

      if ( c < 128 && escape[c] && !(c=='\n' && todo >= 40) )
      { TRYPUTC('\\', out);
	TRYPUTC(escape[c], out);
      } else
      { TRYPUTC(c, out);
      }
    }
    TRYPUTC('"', out);
  } else
  { rc = FALSE;
  }

out:
  PL_release_stream(out);

  return rc;
}


static foreign_t
json_write_indent(term_t stream, term_t indent, term_t tab)
{ int i, t, n;
  IOSTREAM *out;

  if ( !PL_get_integer(indent, &i) ||
       !PL_get_integer(tab, &t) )
    return FALSE;

  if ( PL_get_stream_handle(stream, &out) )
  { int rc = TRUE;
    
    if ( !out->position || out->position->linepos > 0 )
    { TRYPUTC('\n', out);
    }
    for(n=0; n<i/t; n++)
      TRYPUTC('\t', out);
    for(n=0; n<i%t; n++)
      TRYPUTC(' ', out);
out:
    PL_release_stream(out);
    return rc;
  }

  return FALSE;
}



install_t
install_json()
{ PL_register_foreign("json_write_string", 2, json_write_string, 0);
  PL_register_foreign("json_write_indent", 3, json_write_indent, 0);
}
