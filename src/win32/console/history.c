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
#include "console.h"
#include <string.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

typedef struct _history
{ int		size;			/* size of the history */
  int		tail;			/* oldest position */
  int		head;			/* newest position */
  int		current;		/* for retrieval */
  char **	lines;			/* the lines */
} history, *History;

static history	hist;			/* the history */
int 		_rlc_auto_history = TRUE;

static __inline int
next(int i)
{ if ( ++i == hist.size )
    return 0;

  return i;
}


static __inline int
prev(int i)
{ if ( --i < 0 )
    return hist.size-1;

  return i;
}


void
rlc_init_history(int auto_add, int size)
{ int oldsize;
  int i;

  _rlc_auto_history = auto_add;

  if ( hist.lines )
  { hist.lines = rlc_realloc(hist.lines, sizeof(char *) * size);
    oldsize = hist.size;
  } else
  { hist.lines = rlc_malloc(sizeof(char *) * size);
    oldsize = 0;
  }

  for(i=oldsize; i<size; i++)
    hist.lines[i] = NULL;

  hist.size    = size;
  hist.current = -1;
}


void
rlc_add_history(const char *line)
{ if ( hist.size )
  { int i = next(hist.head);
    int len = strlen(line);

    while(*line && *line <= ' ')	/* strip leading white-space */
      line++;
    len = strlen(line);
					/* strip trailing white-space */
    while ( len > 0 && line[len-1] <= ' ' )
      len--;

    if ( len == 0 )
    { hist.current = -1;
      return;
    }

    if ( hist.lines[hist.head] &&
	 strncmp(hist.lines[hist.head], line, len) == 0 )
    { hist.current = -1;
      return;				/* same as last line added */
    }
    
    if ( i == hist.tail )		/* this one is lost */
      hist.tail = next(hist.tail);
    hist.head = i;
    hist.current = -1;

    if ( hist.lines[i] )
      hist.lines[i] = rlc_realloc(hist.lines[i], len+1);
    else
      hist.lines[i] = rlc_malloc(len+1);

    if ( hist.lines[i] )
    { memcpy(hist.lines[i], line, len);
      hist.lines[i][len] = '\0';
    }
  }
}


int
rlc_at_head_history()
{ return hist.current == -1 ? TRUE : FALSE;
}


const char *
rlc_bwd_history()
{ if ( hist.size )
  { if ( hist.current == -1 )
      hist.current = hist.head;
    else if ( hist.current == hist.tail )
      return NULL;
    else
      hist.current = prev(hist.current);

    return hist.lines[hist.current];
  }

  return NULL;
}


const char *
rlc_fwd_history()
{ if ( hist.size && hist.current != -1 )
  { const char *s;

    hist.current = next(hist.current);
    s = hist.lines[hist.current];
    if ( hist.current == hist.head )
      hist.current = -1;

    return s;
  }

  return NULL;
}
