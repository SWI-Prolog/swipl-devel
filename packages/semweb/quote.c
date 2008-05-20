/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

#ifndef PLVERSION
#include <SWI-Prolog.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Ideally we should have various of these tables and distinguish the host,
path, query and fragment parts. See RFC3986 for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
uri_ok()
{ static char ok[128];
  int done = FALSE;
  const char *s;

  if ( !done )
  { int i;

    for(i='a'; i<='z'; i++)
      ok[i] = TRUE;
    for(i='A'; i<='Z'; i++)
      ok[i] = TRUE;
    for(i='0'; i<='9'; i++)
      ok[i] = TRUE;
    for(s="-_.!~*'()"; *s; s++)		/* used to have [], but these general delimiters */
      ok[(int)*s] = TRUE;		/* cannot be in a fragment or path */
    for(s=";/&?:@=#"; *s; s++)
      ok[(int)*s] = TRUE;

    done = TRUE;
  }

  return ok;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RDF (Unicode) URIs are  first  mapped  to   UTF-8  and  then  unsafe and
characters outside the printable US-ASCII range   are represented as %XX
where XX is the hexadecimal version of the  octed. We moved this to C to
exploit much faster character operations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_quote_uri(term_t uri, term_t quoted)
{ char *in;
  const char *s;
  const char *ok = uri_ok();
  size_t len;
  int nok;

  if ( !PL_get_nchars(uri, &len, &in, CVT_ATOM|REP_UTF8|CVT_EXCEPTION) )
    return FALSE;

  for(s=in, nok=0; *s; s++)
  { int c = *s&0xff;

    if ( c >= 128 || !ok[c] )
    { nok++;
    }
  }

  if ( nok )
  { char *buf = alloca(len+nok*2+1);
    char *o = buf;
    static char xdigit[] = "0123456789ABCDEF";

    for(s=in; *s; s++)
    { int c = *s&0xff;

      if ( c >= 128 || !ok[c] )
      { *o++ = '%';
        *o++ = xdigit[(c>>4)&0xf];
        *o++ = xdigit[c&0xf];
      } else
      { *o++ = c;
      }
    }
    *o = '\0';

    return PL_unify_atom_nchars(quoted, len+nok*2, buf);
  }

  return PL_unify(uri, quoted);
}
