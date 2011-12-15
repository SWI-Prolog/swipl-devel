/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam

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
    if ( c < 0 || (!wide && c > 0xff) )
    { if ( canBind(*arg) )
	result->status = CVT_partial;
      else if ( c < 0 )
	result->status = CVT_nocode;
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

    if ( c < 0 || (!wide && c > 0xff) )
    { result->culprit = *arg;

      unfindBuffer(flags);		/* TBD: check unicode range */
      if ( canBind(*arg) )
	result->status = CVT_partial;
      else if ( c < 0 )
	result->status = (type == CODES ? CVT_nocode : CVT_nochar);
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
