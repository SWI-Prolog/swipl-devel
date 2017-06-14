/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2017, VU University Amsterdam
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
#ifdef O_RESERVED_SYMBOLS
#include "pl-ressymbol.h"

		 /*******************************
		 *	  RESERVED SYMBOLS	*
		 *******************************/

static int	compareReservedSymbol(atom_t h1, atom_t h2);

static PL_blob_t reserved_symbol =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
					/* unique representation of text */
  "reserved_symbol",
  NULL,					/* release */
  compareReservedSymbol,			/* compare */
  writeReservedSymbol,			/* write */
  NULL,					/* acquire */
  NULL,					/* save load to/from .qlf files */
  NULL,
  sizeof(char)				/* padding */
};


static const atom_t special_atoms[] =
{ ATOM_nil,				/* 0: [] */
  ATOM_dot,				/* 1: .(_|_) or '[|]'(_,_) */
  ATOM_dict,				/* 2: <dict> */
  ATOM_trienode,			/* 3: <trienode> */
  (atom_t)0
};


const atom_t *
_PL_atoms(void)
{ return special_atoms;
}


static const atom_t reserved_symbols[] =
{ ATOM_nil,				/* 0: `[]` */

  (atom_t)0
};


void
initReservedSymbols(void)
{ PL_register_blob_type(&reserved_symbol);
  atomValue(ATOM_dict)->type     = &reserved_symbol;
  atomValue(ATOM_trienode)->type = &reserved_symbol;

  if ( !GD->options.traditional )
  { const atom_t *ap;

    for(ap=reserved_symbols; *ap; ap++)
    { Atom a = atomValue(*ap);
      a->type = &reserved_symbol;
    }
  }
}


int
isReservedSymbol(word w)
{ return isAtom(w) && atomValue(w)->type == &reserved_symbol;
}


static atom_t
new_reserved_symbol(size_t len, const char *s)
{ int new;

  return lookupBlob(s, len, &reserved_symbol, &new);
}


atom_t
PL_new_reserved_symbol(const char *s)
{ return new_reserved_symbol(strlen(s), s);
}


static int
compareReservedSymbol(atom_t h1, atom_t h2)
{ return strcmp(stringAtom(h1), stringAtom(h2));
}


atom_t
textToReservedSymbol(PL_chars_t *text)
{ if ( !PL_canonicalise_text(text) )
    return 0;

  if ( text->encoding == ENC_ISO_LATIN_1 )
  { return new_reserved_symbol(text->length, text->text.t);
  } else
  { return 0;
  }
}


#endif /* O_RESERVED_SYMBOLS */
