/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

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
#include "error.h"
#include <ctype.h>

static int
print_byte(int value)
{ if ( isgraph(value) || isspace(value) )
  { Sdprintf("%c", value);
  } else
  { Sdprintf("\\\\%02x", value);
  }

  return 0;
}

static int
print_buffer(const char *data, size_t len)
{ size_t i;

  Sdprintf("----------------\n");
  for(i=0; i<len; i++)
  { if ( data[i] == 0 )
    { size_t zeros;

      for(zeros = 0; i+zeros < len && data[i+zeros] == 0; zeros++)
	;
      if ( zeros > 10 )
      { Sdprintf("<%d 0-bytes>", zeros);
      }
      i += zeros;
    } else
    { print_byte(data[i]&0xff);
    }
  }
  if ( data[len-1] != '\n' )
    Sdprintf("\n");
  Sdprintf("----------------\n");

  return 0;
}


static foreign_t
stream_info(term_t stream)
{ IOSTREAM *s;

  if ( !PL_get_stream_handle(stream, &s) )
  { return pl_error("stream_info", 2, NULL, ERR_ARGTYPE, 1,
		    stream, "stream");
  }

  if ( (s->flags & SIO_INPUT) )
  { if ( s->buffer )
    { if ( s->bufp > s->buffer )
      { Sdprintf("Processed input:\n");
	print_buffer(s->buffer, s->bufp-s->buffer);
      }

      if ( s->bufp < s->limitp )
      { Sdprintf("Unprocessed input:\n");
	print_buffer(s->bufp, s->limitp-s->bufp);
      }
    }
  } else if ( (s->flags & SIO_OUTPUT) )
  { if ( s->buffer )
    { if ( s->bufp > s->buffer )
      { Sdprintf("Buffered output:\n");
	print_buffer(s->buffer, s->bufp-s->buffer);
      }

      if ( s->bufp < s->limitp )
      { Sdprintf("Possibly sent output (or junk):\n");
	print_buffer(s->bufp, s->limitp-s->bufp);
      }
    }
  }

  return PL_release_stream(s);
}

install_t
install_streaminfo()
{ PL_register_foreign("$stream_info", 1, stream_info, 0);
}
