/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Functor (re) allocation
*/

#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functor (name/arity) handling.  A functor is a unique object (like atoms).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static FunctorDef functorDefTable[FUNCTORHASHSIZE];

FunctorDef
lookupFunctorDef(register Atom atom, register int arity)
{ int v = pointerHashValue(atom, FUNCTORHASHSIZE);
  register FunctorDef f;

  DEBUG(9, printf("Lookup functor %s/%d = ", stringAtom(atom), arity));
  for(f = functorDefTable[v]; f && !isRef((word)f); f = f->next)
  { if (atom == f->name && f->arity == arity)
    { DEBUG(9, printf("%ld (old)\n", f));
      return f;
    }
  }
  f = (FunctorDef) allocHeap(sizeof(struct functorDef));
  f->next = functorDefTable[v];
  f->type = FUNCTOR_TYPE;
  f->name = atom;
  f->arity = arity;
  functorDefTable[v] = f;
  statistics.functors++;

  DEBUG(9, printf("%ld (new)\n", f));

  return f;
}


FunctorDef
isCurrentFunctor(Atom atom, int arity)
{ int v = pointerHashValue(atom, FUNCTORHASHSIZE);
  FunctorDef f;

  for(f = functorDefTable[v]; f && !isRef((word)f); f = f->next)
  { if (atom == f->name && f->arity == arity)
      return f;
  }

  return (FunctorDef) NULL;
}


struct functorDef functors[] = {
#include "pl-funct.ic"
{ (FunctorDef)NULL,	FUNCTOR_TYPE,	(Atom) NULL, 0 }
};

void
initFunctors(void)
{ register int n;

  { register FunctorDef *f;
    for(n=0, f=functorDefTable; n < (FUNCTORHASHSIZE-1); n++, f++)
      *f = (FunctorDef)makeRef(f+1);
    *f = (FunctorDef) NULL;
  }

  { register FunctorDef f;
    register int v;

    for( f = &functors[0]; f->name; f++ )
    { v = pointerHashValue(f->name, FUNCTORHASHSIZE);
      f->next = functorDefTable[v];
      functorDefTable[v] = f;
      statistics.functors++;
    }
  }
}

#if TEST
checkFunctors()
{ register FunctorDef f;
  int n;

  for( n=0; n < FUNCTORHASHSIZE; n++ )
  { f = functorDefTable[n];
    for( ;f && !isRef((word)f); f = f->next )
    { if ( f->type != FUNCTOR_TYPE )
        printf("[ERROR: Functor %ld has bad type: %ld]\n", f, f->type);
      if ( f->arity < 0 || f->arity > 10 )	/* debugging only ! */
        printf("[ERROR: Functor %ld has dubious arity: %d]\n", f, f->arity);
      if ( !inCore(f->name) || f->name->type != ATOM_TYPE )
        printf("[ERROR: Functor %ld has illegal name: %ld]\n", f, f->name);
      if ( !( f->next == (FunctorDef) NULL ||
	      isRef((word)f->next) ||
	      inCore(f->next)) )
	printf("[ERROR: Functor %ld has illegal next: %ld]\n", f, f->next);
    }
    if ( (isRef((word)f) &&
	 ((FunctorDef *) unRef((word)f) != &functorDefTable[n+1])) )
      printf("[ERROR: Bad continuation pointer (fDef, n=%d)]\n", n);
    if ( f == (FunctorDef) NULL && n != (FUNCTORHASHSIZE-1) )
      printf("[ERROR: illegal end pointer (fDef, n=%d)]\n", n);
  }
}
#endif

word
pl_current_functor(Word name, Word arity, word h)
{ FunctorDef fdef;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( (!isAtom(*name) && !isVar(*name))
	|| (!isInteger(*arity) && !isVar(*arity)))
	return warning("current_functor/2: instantiation fault");

      if (isInteger(*arity) && isAtom(*name))
	if (isCurrentFunctor((Atom)*name, (int)valNum(*arity)) != (FunctorDef) NULL)
	  succeed;
	else
	  fail;

      fdef = functorDefTable[0];
      break;
    case FRG_REDO:
      fdef = (FunctorDef) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  DEBUG(9, printf("current_functor(): fdef = %ld\n", fdef));
  for(; fdef; fdef = fdef->next)
  { while( isRef((word)fdef) )
    { fdef = *((FunctorDef *)unRef(fdef));
      if (fdef == (FunctorDef) NULL)
	fail;
    }
    if (arity == 0)
      continue;
    if ( unifyAtomic(name, fdef->name) == FALSE ||
	 unifyAtomic(arity, consNum(fdef->arity)) == FALSE)
      continue;
    DEBUG(9, printf("Returning backtrack point %ld\n", fdef->next));

    return_next_table(FunctorDef, fdef);
  }

  fail;
}
