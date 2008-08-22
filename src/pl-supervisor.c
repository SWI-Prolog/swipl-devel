/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006-2008, University of Amsterdam

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

#include "pl-incl.h"

#define MAX_FLI_ARGS 10			/* extend switches on change */

static Code
allocCodes(int n)
{ Code codes = allocHeap(sizeof(code)*(n+1));
  
  *codes++ = (code)n;

  return codes;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Foreign supervisors.  Creates one of:

DET code:  I_FOPEN,     I_FCALLDETVA|I_FCALLDET<N>,   I_FEXITDET
NDET code: I_FOPENNDET, I_FCALLNDETVA|I_FCALLNDET<N>, I_FEXITNDET, I_FREDO
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
createForeignSupervisor(Definition def, Func f)
{ assert(true(def, FOREIGN));
  
  if ( false(def, P_VARARG) )
  { if ( def->functor->arity > MAX_FLI_ARGS )
      sysError("Too many arguments to foreign function %s (>%d)", \
	       predicateName(def), MAX_FLI_ARGS); \
  }

  if ( false(def, NONDETERMINISTIC) )
  { Code codes = allocCodes(4);
    
    codes[0] = encode(I_FOPEN);
    if ( true(def, P_VARARG) )
      codes[1] = encode(I_FCALLDETVA);
    else
      codes[1] = encode(I_FCALLDET0+def->functor->arity);
    codes[2] = (code)f;
    codes[3] = encode(I_FEXITDET);

    def->codes = codes;
  } else
  { Code codes = allocCodes(5);
    
    codes[0] = encode(I_FOPENNDET);
    if ( true(def, P_VARARG) )
      codes[1] = encode(I_FCALLNDETVA);
    else
      codes[1] = encode(I_FCALLNDET0+def->functor->arity);
    codes[2] = (code)f;
    codes[3] = encode(I_FEXITNDET);
    codes[4] = encode(I_FREDO);

    def->codes = codes;
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
createSingleClauseSupervisor() creates a supervisor to call the one and
only clause of the predicate.  Creates

	S_TRUSTME <ClauseRef>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
createSingleClauseSupervisor(Definition def)
{ ClauseRef cref;

  for(cref = def->definition.clauses; cref; cref = cref->next)
  { if ( visibleClause(cref->clause, GD->generation) )
    { Code codes = allocCodes(2);

      DEBUG(1, Sdprintf("Single clause supervisor for %s\n",
			predicateName(def)));

      codes[0] = encode(S_TRUSTME);
      codes[1] = (code)cref->clause;

      succeed;
    }
  }

  fail;
}


