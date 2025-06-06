/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011, University of Amsterdam
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

#ifndef PL_CODELIST_H_INCLUDED
#define PL_CODELIST_H_INCLUDED
#include "pl-fli.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD: This seems a  duplication  of   pl-privitf.[ch],  which  is used in
pl-file.c. These functions are used in pl-text.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	    (CODE) LISTS	*
		 *******************************/

#ifndef setHandle
#define setHandle(h, w)		(*valTermRef(h) = (w))
#endif
/* valHandle(term_t r) moved to pl-inline.h */


#define INIT_SEQ_STRING(n) LDFUNC(INIT_SEQ_STRING, n)
static inline Word
INIT_SEQ_STRING(DECL_LD size_t n)
{ return allocGlobal(n*3);
}


#define EXTEND_SEQ_CODES(p, c) LDFUNC(EXTEND_SEQ_CODES, p, c)
static inline Word
EXTEND_SEQ_CODES(DECL_LD Word p, int c)
{ *p++ = FUNCTOR_dot2;
  *p++ = consInt(c);
  *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);

  return p+1;
}


#define EXTEND_SEQ_CHARS(p, c) LDFUNC(EXTEND_SEQ_CHARS, p, c)
static inline Word
EXTEND_SEQ_CHARS(DECL_LD Word p, int c)
{ *p++ = FUNCTOR_dot2;
  *p++ = codeToAtom(c);
  *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);

  return p+1;
}


#define CLOSE_SEQ_STRING(p, p0, tail, term, l) LDFUNC(CLOSE_SEQ_STRING, p, p0, tail, term, l)
static inline bool
CLOSE_SEQ_STRING(DECL_LD Word p, Word p0, term_t tail, term_t term, term_t l)
{ setHandle(l, consPtr(p0, TAG_COMPOUND|STG_GLOBAL));
  p--;
  if ( tail )
  { setVar(*p);
    if ( PL_unify(l, term) )
    { setHandle(tail, makeRefG(p));
      return true;
    }

    return false;
  } else
  { *p = ATOM_nil;
    return PL_unify(l, term);
  }
}

Buffer		codes_or_chars_to_buffer(term_t l, unsigned int flags,
					 int wide, CVT_result *status);

#endif /*PL_CODELIST_H_INCLUDED*/
