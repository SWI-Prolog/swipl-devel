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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD: This seems a  duplication  of   pl-privitf.[ch],  which  is used in
pl-file.c. These functions are used in pl-text.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	    (CODE) LISTS	*
		 *******************************/

#define setHandle(h, w)		(*valTermRef(h) = (w))

static inline word
valHandle__LD(term_t r ARG_LD)
{ Word p = valTermRef(r);

  deRef(p);
  return *p;
}

#define valHandle(r) valHandle__LD(r PASS_LD)


#define INIT_SEQ_STRING(n) INIT_SEQ_STRING__LD(n PASS_LD)
#define EXTEND_SEQ_CODES(p, c) EXTEND_SEQ_CODES__LD(p, c PASS_LD)
#define EXTEND_SEQ_CHARS(p, c) EXTEND_SEQ_CHARS__LD(p, c PASS_LD)
#define CLOSE_SEQ_STRING(p, p0, tail, term, l) \
	CLOSE_SEQ_STRING__LD(p, p0, tail, term, l PASS_LD)

static inline Word
INIT_SEQ_STRING__LD(size_t n ARG_LD)
{ return allocGlobal(n*3);
}


static inline Word
EXTEND_SEQ_CODES__LD(Word p, int c ARG_LD)
{ *p++ = FUNCTOR_dot2;
  *p++ = consInt(c);
  *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);

  return p+1;
}


static inline Word
EXTEND_SEQ_CHARS__LD(Word p, int c ARG_LD)
{ *p++ = FUNCTOR_dot2;
  *p++ = codeToAtom(c);
  *p = consPtr(p+1, TAG_COMPOUND|STG_GLOBAL);

  return p+1;
}


static inline int
CLOSE_SEQ_STRING__LD(Word p, Word p0, term_t tail, term_t term, term_t l ARG_LD)
{ setHandle(l, consPtr(p0, TAG_COMPOUND|STG_GLOBAL));
  p--;
  if ( tail )
  { setVar(*p);
    if ( PL_unify(l, term) )
    { setHandle(tail, makeRefG(p));
      return TRUE;
    }

    return FALSE;
  } else
  { *p = ATOM_nil;
    return PL_unify(l, term);
  }
}

COMMON(Buffer)		codes_or_chars_to_buffer(term_t l, unsigned int flags,
						 int wide, CVT_result *status);

#endif /*PL_CODELIST_H_INCLUDED*/
