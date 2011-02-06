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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef PL_CODELIST_H_INCLUDED
#define PL_CODELIST_H_INCLUDED

		 /*******************************
		 *	    (CODE) LISTS	*
		 *******************************/

#define setHandle(h, w)		(*valTermRef(h) = (w))

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

#endif /*PL_CODELIST_H_INCLUDED*/
