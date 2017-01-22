/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2014, University of Amsterdam
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

#include "pl-incl.h"
#include "../pl-codelist.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
codes_or_chars_to_buffer(term_t l, unsigned int flags, int wide, CVT_code *status)

If l represents a list of codes   or characters, return a buffer holding
the characters. If wide == TRUE  the   buffer  contains  objects of type
pl_wchar_t. Otherwise it contains traditional characters.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Buffer
codes_or_chars_to_buffer(term_t l, unsigned int flags, int wide, CVT_result *result)
{ GET_LD
  Buffer b;
  word list = valHandle(l);
  word slow;
  Word arg, tail;
  int step_slow = TRUE;
  enum { CHARS, CODES } type;

  if ( isList(list) )
  { intptr_t c = -1;

    arg = argTermP(list, 0);
    deRef(arg);

    if ( isTaggedInt(*arg) )
    { c = valInt(*arg);
      type = CODES;
    } else
    { c = charCode(*arg);
      type = CHARS;
    }

    result->culprit = *arg;
    if ( c < 0 || c > 0x10ffff || (!wide && c > 0xff) )
    { if ( canBind(*arg) )
	result->status = CVT_partial;
      else if ( c < 0 || c > 0x10ffff )
	result->status = CVT_nocode;
#if SIZEOF_WCHAR_T == 2
      else if ( c > PLMAXWCHAR )
	result->status = CVT_representation;
#endif
      else if ( c > 0xff )
	result->status = CVT_wide;
      return NULL;
    }
  } else if ( isNil(list) )
  { return findBuffer(flags);
  } else
  { if ( canBind(list) )
      result->status = CVT_partial;
    else
      result->status = CVT_nolist;

    return NULL;
  }

  b = findBuffer(flags);

  slow = list;
  while( isList(list) )
  { intptr_t c = -1;

    arg = argTermP(list, 0);
    deRef(arg);

    switch(type)
    { case CODES:
	if ( isTaggedInt(*arg) )
	  c = valInt(*arg);
        break;
      case CHARS:
	c = charCode(*arg);
        break;
    }

    if ( c < 0 || c > 0x10ffff || (!wide && c > 0xff) )
    { result->culprit = *arg;

      unfindBuffer(flags);
      if ( canBind(*arg) )
	result->status = CVT_partial;
      else if ( c < 0 || c > 0x10ffff )
	result->status = (type == CODES ? CVT_nocode : CVT_nochar);
#if SIZEOF_WCHAR_T == 2
      else if ( c > PLMAXWCHAR )
	result->status = CVT_representation;
#endif
      else if ( c > 0xff )
	result->status = CVT_wide;
      return NULL;
    }

    if ( wide )
      addBuffer(b, (pl_wchar_t)c, pl_wchar_t);
    else
      addBuffer(b, (unsigned char)c, unsigned char);

    tail = argTermP(list, 1);
    deRef(tail);
    list = *tail;
    if ( list == slow )		/* cyclic */
    { unfindBuffer(flags);
      result->status = CVT_nolist;
      return NULL;
    }
    if ( (step_slow = !step_slow) )
    { tail = argTermP(slow, 1);
      deRef(tail);
      slow = *tail;
    }
  }
  if ( !isNil(list) )
  { unfindBuffer(flags);
    if ( canBind(list) )
      result->status = CVT_partial;
    else
      result->status = CVT_nolist;
    return NULL;
  }

  result->status = CVT_ok;

  return b;
}
