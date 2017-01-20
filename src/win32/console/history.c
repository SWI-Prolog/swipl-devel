/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2012, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <windows.h>
#include <tchar.h>
#define _MAKE_DLL 1
#undef _export
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
  { b->history.lines = rlc_realloc(b->history.lines, sizeof(TCHAR *) * size);
    oldsize = b->history.size;
  } else
  { b->history.lines = rlc_malloc(sizeof(TCHAR *) * size);
    oldsize = 0;
  }

  for(i=oldsize; i<size; i++)
    b->history.lines[i] = NULL;

  b->history.size    = size;
  b->history.current = -1;
}


void
rlc_add_history(rlc_console c, const TCHAR *line)
{ RlcData b = rlc_get_data(c);

  if ( b->history.size )
  { int i = next(b, b->history.head);
    size_t len = _tcslen(line);

    while(*line && *line <= ' ')	/* strip leading white-space */
      line++;
    len = _tcslen(line);
					/* strip trailing white-space */
    while ( len > 0 && line[len-1] <= ' ' )
      len--;

    if ( len == 0 )
    { b->history.current = -1;
      return;
    }

    if ( b->history.lines[b->history.head] &&
	 _tcsncmp(b->history.lines[b->history.head], line, len) == 0 )
    { b->history.current = -1;
      return;				/* same as last line added */
    }

    if ( i == b->history.tail )		/* this one is lost */
      b->history.tail = next(b, b->history.tail);
    b->history.head = i;
    b->history.current = -1;

    if ( b->history.lines[i] )
      b->history.lines[i] = rlc_realloc(b->history.lines[i],
					(len+1)*sizeof(TCHAR));
    else
      b->history.lines[i] = rlc_malloc((len+1)*sizeof(TCHAR));

    if ( b->history.lines[i] )
    { _tcsncpy(b->history.lines[i], line, len);
      b->history.lines[i][len] = '\0';
    }
  }
}


int
rlc_for_history(rlc_console c,
		int (*handler)(void *ctx, int no, const TCHAR *line),
		void *ctx)
{ RlcData b = rlc_get_data(c);
  int here = b->history.head;
  int no = 1;

  for( ; here != b->history.tail; here = prev(b, here))
  { int rc;

    if ( (rc=(*handler)(ctx, no++, b->history.lines[here])) != 0 )
      return rc;
  }

  return 0;
}


int
rlc_at_head_history(RlcData b)
{ return b->history.current == -1 ? TRUE : FALSE;
}


const TCHAR *
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


const TCHAR *
rlc_fwd_history(RlcData b)
{ if ( b->history.size && b->history.current != -1 )
  { const TCHAR *s;

    b->history.current = next(b, b->history.current);
    s = b->history.lines[b->history.current];
    if ( b->history.current == b->history.head )
      b->history.current = -1;

    return s;
  }

  return NULL;
}
