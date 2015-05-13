/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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
  atomValue(ATOM_dict)->type = &reserved_symbol;

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
