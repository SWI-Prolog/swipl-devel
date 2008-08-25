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

#define O_DEBUG 1
#include "pl-incl.h"

#define MAX_FLI_ARGS 10			/* extend switches on change */

static Code
allocCodes(size_t n)
{ Code codes = allocHeap(sizeof(code)*(n+1));
  
  *codes++ = (code)n;

  return codes;
}


void
freeCodes(Code codes)
{ if ( codes )
  { size_t size = (size_t)codes[-1];

    freeHeap(&codes[-1], (size+1)*sizeof(code));
  }
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


		 /*******************************
		 *	   PROLOG CASES		*
		 *******************************/

static int
getClauses(Definition def, ClauseRef *refp0)
{ ClauseRef cref, *refp = refp0;

  for(cref = def->definition.clauses; cref; cref = cref->next)
  { if ( visibleClause(cref->clause, GD->generation) )
      *refp++ = cref;
  }

  return refp - refp0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
createSingleClauseSupervisor() creates a supervisor to call the one and
only clause of the predicate.  Creates

	S_TRUSTME <ClauseRef>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
createSingleClauseSupervisor(Definition def)
{ if ( def->number_of_clauses == 1 )
  { ClauseRef cref;
    Code codes = allocCodes(2);
  
    getClauses(def, &cref);
    DEBUG(1, Sdprintf("Single clause supervisor for %s\n",
		      predicateName(def)));
  
    codes[0] = encode(S_TRUSTME);
    codes[1] = (code)cref;
  
    def->codes = codes;
    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
createListSuperVisor() creates a supervisor for predicates that have two
clauses (possibly swapped):
	
	pred([], ....)
	pred([H|T], ...)

The code is

	S_LIST <nilclause> <listclause>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
createListSupervisor(Definition def)
{ if ( def->number_of_clauses == 2 )
  { ClauseRef cref[2];
    word c[2];

    getClauses(def, cref);
    if ( arg1Key(cref[0]->clause, TRUE, &c[0]) &&
	 arg1Key(cref[1]->clause, TRUE, &c[1]) &&
	 ( (c[0] == ATOM_nil && c[1] == FUNCTOR_dot2) ||
	   (c[1] == ATOM_nil && c[0] == FUNCTOR_dot2) ) )
    { Code codes = allocCodes(3);

      DEBUG(1, Sdprintf("List supervisor for %s\n", predicateName(def)));

      codes[0] = encode(S_LIST);
      if ( c[0] == ATOM_nil )
      { codes[1] = (code)cref[0];
	codes[2] = (code)cref[1];
      } else
      { codes[1] = (code)cref[1];
	codes[2] = (code)cref[0];
      }

      def->codes = codes;
      succeed;
    }
  }

  fail;
}
