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

#include "console.h"
#include <windows.h>
#include <string.h>

#ifndef EOS
#define EOS 0
#endif

static char *completion_chars = "~:\\/-.";

static int
complete_scan_backwards(Line ln, int from)
{ while( from > 0 )
  { int c = ln->data[from-1];

    if ( rlc_is_word_char(c) || strchr(completion_chars, c) )
      from--;
    else
      break;
  }

  return from;
}


static __inline int
close_quote(int c)
{ return (c == '\'' || c == '"') ? c : 0;
}


int
rlc_complete_file_function(RlcCompleteData data)
{ Line ln = data->line;
  WIN32_FIND_DATA fdata;

  switch(data->call_type)
  { case COMPLETE_INIT:
    { int start = complete_scan_backwards(ln, ln->point);
      char *pattern = data->buf_handle;
      char *s = pattern;
      int n = start;
      int ld = start;
      HANDLE h;

      if ( ln->point - start > 200 )
	return FALSE;

      for( ; n < ln->point; n++)
      { int c = ln->data[n];
      
	if ( c == '/' )
	  c = '\\';
	if ( c == '\\' )
	  ld = n + 1;
	*s++ = c;
      }
      *s++ = '*';
      *s = EOS;

      if ( (h=FindFirstFile(pattern, &fdata)) != INVALID_HANDLE_VALUE )
      { data->replace_from = ld;
	if ( start > 0 &&
	     !(fdata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) )
	  data->quote = close_quote(ln->data[start-1]);
	strcpy(data->candidate, fdata.cFileName);
	data->ptr_handle = h;
	data->case_insensitive = TRUE;
	data->function = rlc_complete_file_function;

	return TRUE;
      }

      return FALSE;
    }

    case COMPLETE_ENUMERATE:
    { if ( FindNextFile(data->ptr_handle, &fdata) )
      { strcpy(data->candidate, fdata.cFileName);
	return TRUE;
      }
      
      return FALSE;
    }

    case COMPLETE_CLOSE:
    { FindClose(data->ptr_handle);

      return FALSE;
    }

    default:
      return FALSE;
  }
}
