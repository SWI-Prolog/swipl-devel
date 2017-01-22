/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2011, University of Amsterdam
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
