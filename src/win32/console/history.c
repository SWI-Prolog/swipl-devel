/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

#define _MAKE_DLL 1
#undef _export
#include <windows.h>
#include "console.h"			/* public stuff */
#include "console_i.h"			/* internal stuff */
#include <string.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

static __inline int
next(RlcData b, int i)
{ if ( ++i == b->history.size )
    return 0;

  return i;
}


static __inline int
prev(RlcData b, int i)
{ if ( --i < 0 )
    return b->history.size-1;

  return i;
}


void
rlc_init_history(rlc_console c, int size)
{ RlcData b = rlc_get_data(c);
  int oldsize;
  int i;

  if ( b->history.lines )
  { b->history.lines = rlc_realloc(b->history.lines, sizeof(char *) * size);
    oldsize = b->history.size;
  } else
  { b->history.lines = rlc_malloc(sizeof(char *) * size);
    oldsize = 0;
  }

  for(i=oldsize; i<size; i++)
    b->history.lines[i] = NULL;

  b->history.size    = size;
  b->history.current = -1;
}


void
rlc_add_history(rlc_console c, const char *line)
{ RlcData b = rlc_get_data(c);

  if ( b->history.size )
  { int i = next(b, b->history.head);
    int len = strlen(line);

    while(*line && *line <= ' ')	/* strip leading white-space */
      line++;
    len = strlen(line);
					/* strip trailing white-space */
    while ( len > 0 && line[len-1] <= ' ' )
      len--;

    if ( len == 0 )
    { b->history.current = -1;
      return;
    }

    if ( b->history.lines[b->history.head] &&
	 strncmp(b->history.lines[b->history.head], line, len) == 0 )
    { b->history.current = -1;
      return;				/* same as last line added */
    }
    
    if ( i == b->history.tail )		/* this one is lost */
      b->history.tail = next(b, b->history.tail);
    b->history.head = i;
    b->history.current = -1;

    if ( b->history.lines[i] )
      b->history.lines[i] = rlc_realloc(b->history.lines[i], len+1);
    else
      b->history.lines[i] = rlc_malloc(len+1);

    if ( b->history.lines[i] )
    { memcpy(b->history.lines[i], line, len);
      b->history.lines[i][len] = '\0';
    }
  }
}


int
rlc_at_head_history(RlcData b)
{ return b->history.current == -1 ? TRUE : FALSE;
}


const char *
rlc_bwd_history(RlcData b)
{ if ( b->history.size )
  { if ( b->history.current == -1 )
      b->history.current = b->history.head;
    else if ( b->history.current == b->history.tail )
      return NULL;
    else
      b->history.current = prev(b, b->history.current);

    return b->history.lines[b->history.current];
  }

  return NULL;
}


const char *
rlc_fwd_history(RlcData b)
{ if ( b->history.size && b->history.current != -1 )
  { const char *s;

    b->history.current = next(b, b->history.current);
    s = b->history.lines[b->history.current];
    if ( b->history.current == b->history.head )
      b->history.current = -1;

    return s;
  }

  return NULL;
}
