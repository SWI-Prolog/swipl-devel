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
lookupFunctorDef(atom, arity)
register Atom atom;
register int arity;
{ int v = pointerHashValue(atom, FUNCTORHASHSIZE);
  register FunctorDef f;

  DEBUG(9, printf("Lookup functor %s/%d = ", stringAtom(atom), arity));
  for(f = functorDefTable[v]; f && !isRef((word)f); f = f->next)
  { if (atom == f->name && f->arity == arity)
    { DEBUG(9, printf("%D (old)\n", f));
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

  DEBUG(9, printf("%D (new)\n", f));

  return f;
}


int
atomIsFunctor(atom)
Atom atom;
{ int v = pointerHashValue(atom, FUNCTORHASHSIZE);
  FunctorDef f;

  for(f = functorDefTable[v]; f && !isRef((word)f); f = f->next)
  { if ( atom == f->name )
      return f->arity;
  }

  return -1;
}


FunctorDef
isCurrentFunctor(atom, arity)
Atom atom;
int arity;
{ int v = pointerHashValue(atom, FUNCTORHASHSIZE);
  FunctorDef f;

  for(f = functorDefTable[v]; f && !isRef((word)f); f = f->next)
  { if (atom == f->name && f->arity == arity)
      return f;
  }

  return (FunctorDef) NULL;
}


bool
atomIsProcedureModule(atom, m)
Atom atom;
Module m;
{ int v = pointerHashValue(atom, FUNCTORHASHSIZE);
  FunctorDef f;
  Procedure proc;

  for(f = functorDefTable[v]; f && !isRef((word)f); f = f->next)
  { if ( atom == f->name &&
	 (proc = isCurrentProcedure(f, m)) != (Procedure)NULL &&
	 isDefinedProcedure(proc) )
      succeed;
  }

  fail;
}


bool
atomIsProcedure(atom)
Atom atom;
{ Symbol s;

  for_table(s, moduleTable)
    if ( atomIsProcedureModule(atom, (Module)s->value) )         
      succeed;

  fail;
}


struct functorDef functors[] = {
#include "pl-funct.ic"
{ (FunctorDef)NULL,	FUNCTOR_TYPE,	(Atom) NULL, 0 }
};

void
initFunctors()
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
        printf("[ERROR: Functor %D has bad type: %D]\n", f, f->type);
      if ( f->arity < 0 || f->arity > 10 )	/* debugging only ! */
        printf("[ERROR: Functor %D has dubious arity: %d]\n", f, f->arity);
      if ( !inCore(f->name) || f->name->type != ATOM_TYPE )
        printf("[ERROR: Functor %D has illegal name: %D]\n", f, f->name);
      if ( !( f->next == (FunctorDef) NULL ||
	      isRef((word)f->next) ||
	      inCore(f->next)) )
	printf("[ERROR: Functor %D has illegal next: %D]\n", f, f->next);
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
pl_current_functor(name, arity, h)
Word name, arity;
word h;
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

  DEBUG(9, printf("current_functor(): fdef = %D\n", fdef));
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
    DEBUG(9, printf("Returning backtrack point %D\n", fdef->next));

    return_next_table(FunctorDef, fdef);
  }

  fail;
}
