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

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include "turtle_chars.c"

/** turtle_name(+Atom) is semidet.

True if Atom is a valid Turtle identifier
*/

static inline int
wcis_name_char(int c)
{ return wcis_name_start_char(c) ||
         wcis_name_extender_char(c);
}

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



install_t
install_turtle()
{ PL_register_foreign("turtle_name", 1, turtle_name, 0);
}
