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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <windows.h>
#include <tchar.h>
#include "console.h"

#ifndef EOS
#define EOS 0
#endif

#ifndef _TINT
typedef wint_t _TINT;
#endif

static TCHAR *completion_chars = TEXT("~:\\/-.");

static size_t
complete_scan_backwards(Line ln, size_t from)
{ while( from > 0 )
  { _TINT c = ln->data[from-1];

    if ( rlc_is_word_char(c) ||
	 _tcschr(completion_chars, c) )
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
    { size_t start = complete_scan_backwards(ln, ln->point);
      TCHAR *pattern = data->buf_handle;
      TCHAR *s = pattern;
      size_t n = start;
      size_t ld = start;
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
      { data->replace_from = (int)ld;
	if ( start > 0 &&
	     !(fdata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) )
	  data->quote = close_quote(ln->data[start-1]);
	_tcscpy(data->candidate, fdata.cFileName);
	data->ptr_handle = h;
	data->case_insensitive = TRUE;
	data->function = rlc_complete_file_function;

	return TRUE;
      }

      return FALSE;
    }

    case COMPLETE_ENUMERATE:
    { if ( FindNextFile(data->ptr_handle, &fdata) )
      { _tcscpy(data->candidate, fdata.cFileName);
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
