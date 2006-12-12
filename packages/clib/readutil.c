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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#define BUFSIZE 256

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library is dynamically picked up  by library(readline), which falls
back to a pure Prolog implementation if this library cannot be found.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static atom_t ATOM_end_of_file;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read_line_to_codes(+Stream, -Codes, ?Tail)

Read a line, upto the  next  '\n'   from  Stream.  Normally  the line is
returned as a difference list  Codes-Tail.   If  EOF is encountered, the
Codes list is closed and Tail is unified with [].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
read_line_to_codes3(term_t stream, term_t codes, term_t tail)
{ wchar_t buf[BUFSIZE];
  wchar_t *o = buf, *e = &buf[BUFSIZE];
  IOSTREAM *s;
  term_t cl = PL_copy_term_ref(codes);

  if ( !PL_get_stream_handle(stream, &s) )
    return FALSE;

  for(;;)
  { int	c = Sgetcode(s);

    if ( c == EOF )
    { if ( !PL_release_stream(s) )
	return FALSE;			/* error */

      if ( tail == 0 && o == buf )
	return PL_unify_atom(codes, ATOM_end_of_file);
      if ( PL_unify_wchars(cl, PL_CODE_LIST, o-buf, buf) &&
           (tail == 0 || PL_unify_nil(tail)) )
	return TRUE;

      return FALSE;
    }

    if ( o == e )
    { if ( !PL_unify_wchars_diff(cl, cl, PL_CODE_LIST, o-buf, buf) )
      { PL_release_stream(s);
	return FALSE;
      }
      o = buf;
    }

    *o++ = c;
    if ( c == '\n' )
    { if ( tail )
      { if ( PL_unify_wchars_diff(cl, cl, PL_CODE_LIST, o-buf, buf) &&
	     PL_unify(cl, tail) )
	  return TRUE;
      } else
      { o--;
	if ( o>buf && o[-1] == '\r' )
	  o--;
	return PL_unify_wchars(cl, PL_CODE_LIST, o-buf, buf);
      }

      PL_release_stream(s);
      return FALSE;
    }
  }
}


static foreign_t
read_line_to_codes2(term_t stream, term_t codes)
{ return read_line_to_codes3(stream, codes, 0);
}


static foreign_t
read_stream_to_codes3(term_t stream, term_t codes, term_t tail)
{ wchar_t buf[BUFSIZE];
  wchar_t *o = buf, *e = &buf[BUFSIZE];
  IOSTREAM *s;
  term_t cl = PL_copy_term_ref(codes);

  if ( !PL_get_stream_handle(stream, &s) )
    return FALSE;

  for(;;)
  { int	c = Sgetcode(s);

    if ( c == EOF )
    { if ( !PL_release_stream(s) )
	return FALSE;			/* error */

      if ( tail )
      { if ( PL_unify_wchars_diff(cl, cl, PL_CODE_LIST, o-buf, buf) &&
	     PL_unify(cl, tail) )
	  return TRUE;
	return FALSE;
      }	else
      { return PL_unify_wchars(cl, PL_CODE_LIST, o-buf, buf);
      }
    }

    if ( o == e )
    { if ( !PL_unify_wchars_diff(cl, cl, PL_CODE_LIST, o-buf, buf) )
      { PL_release_stream(s);
	return FALSE;
      }
      o = buf;
    }

    *o++ = c;
  }
}


static foreign_t
read_stream_to_codes2(term_t stream, term_t codes)
{ return read_stream_to_codes3(stream, codes, 0);
}


install_t
install_readutil()
{ ATOM_end_of_file = PL_new_atom("end_of_file");

  PL_register_foreign("read_line_to_codes", 3, read_line_to_codes3, 0);
  PL_register_foreign("read_line_to_codes", 2, read_line_to_codes2, 0);
  PL_register_foreign("read_stream_to_codes", 3, read_stream_to_codes3, 0);
  PL_register_foreign("read_stream_to_codes", 2, read_stream_to_codes2, 0);
}
