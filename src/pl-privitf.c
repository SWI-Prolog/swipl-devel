/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2016, University of Amsterdam
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
#undef LD
#define LD LOCAL_LD

#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines extensions to pl-fli.c that are used internally, but
not exported to the SWI-Prolog user. Most   of them are too specific for
the public interface.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *     CHARACTER GET/UNIFY	*
		 *******************************/

/** PL_get_char(term_t c, int *p, int eof)

Get a character code from a term and   store  in over p. Returns TRUE if
successful. On failure it returns a  type   error.  If  eof is TRUE, the
integer -1 or the atom end_of_file can used to specify and EOF character
code.
*/

int
PL_get_char(term_t c, int *p, int eof)
{ GET_LD
  int chr;
  atom_t name;
  PL_chars_t text;

  if ( PL_get_integer(c, &chr) )
  { if ( chr >= 0 )
    { *p = chr;
      return TRUE;
    }
    if ( eof && chr == -1 )
    { *p = chr;
      return TRUE;
    }
  } else if ( PL_get_text(c, &text, CVT_ATOM|CVT_STRING|CVT_LIST) &&
	      text.length == 1 )
  { *p = text.encoding == ENC_ISO_LATIN_1 ? text.text.t[0]&0xff
					  : text.text.w[0];
    return TRUE;
  } else if ( eof && PL_get_atom(c, &name) && name == ATOM_end_of_file )
  { *p = -1;
    return TRUE;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_character, c);
}


/** PL_unify_char(term_t chr, int c, int how)

Unify a character.  Try to be as flexible as possible, only binding a
variable `chr' to a code or one-char-atom.  E.g., this succeeds:

    PL_unify_char('a', 97, PL_CODE)
*/

int
PL_unify_char(term_t chr, int c, int how)
{ GET_LD
  int c2 = -1;

  if ( PL_is_variable(chr) )
  { switch(how)
    { case PL_CHAR:
      { atom_t a = (c == -1 ? ATOM_end_of_file : codeToAtom(c));

	return PL_unify_atom(chr, a);
      }
      case PL_CODE:
      case PL_BYTE:
      default:
	return PL_unify_integer(chr, c);
    }
  } else if ( PL_get_char(chr, &c2, TRUE) )
    return c == c2;

  return FALSE;
}


		 /*******************************
		 *	  LIST BUILDING		*
		 *******************************/

int
allocList(size_t maxcells, list_ctx *ctx)
{ GET_LD
  Word p = allocGlobal(1+maxcells*3);

  if ( p )
  { ctx->lp = ctx->gstore = p;

    return TRUE;
  }

  return FALSE;
}

int
unifyList(term_t term, list_ctx *ctx)
{ GET_LD
  Word a;

  ctx->gstore[0] = ATOM_nil;
  gTop = &ctx->gstore[1];

  a = valTermRef(term);
  deRef(a);
  if ( !unify_ptrs(a, ctx->lp, 0 PASS_LD) )
  { gTop = ctx->lp;
    return FALSE;
  }

  return TRUE;
}

int
unifyDiffList(term_t head, term_t tail, list_ctx *ctx)
{ GET_LD
  Word a;

  setVar(ctx->gstore[0]);
  gTop = &ctx->gstore[1];

  a = valTermRef(head);
  deRef(a);
  if ( !unify_ptrs(a, ctx->lp, 0 PASS_LD) )
  { gTop = ctx->lp;
    return FALSE;
  }
  a = valTermRef(tail);
  deRef(a);
  if ( !unify_ptrs(a, ctx->gstore, 0 PASS_LD) )
  { gTop = ctx->lp;
    return FALSE;
  }

  return TRUE;
}
