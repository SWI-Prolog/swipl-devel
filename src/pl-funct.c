/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Functor (re) allocation
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functor (name/arity) handling.  A functor is a unique object (like atoms).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int	  functor_buckets = FUNCTORHASHSIZE;
static int	  functor_locked;
static FunctorDef *functorDefTable;
static void	  allocFunctorTable();
static void	  rehashFunctors();

#define lockFunctors() { functor_locked++; }
#define unlockFunctors() if ( --functor_locked == 0 && \
			      functor_buckets * 2 < statistics.functors ) \
			   rehashFunctors()

FunctorDef
lookupFunctorDef(register Atom atom, register int arity)
{ int v = pointerHashValue(atom, functor_buckets);
  register FunctorDef f;

  DEBUG(9, Sdprintf("Lookup functor %s/%d = ", stringAtom(atom), arity));
  for(f = functorDefTable[v]; f && !isRef((word)f); f = f->next)
  { if (atom == f->name && f->arity == arity)
    { DEBUG(9, Sdprintf("%ld (old)\n", f));
      return f;
    }
  }
  f = (FunctorDef) allocHeap(sizeof(struct functorDef));
  f->next = functorDefTable[v];
  f->type = FUNCTOR_TYPE;
  f->name = atom;
  f->arity = arity;
  f->flags = 0;
  functorDefTable[v] = f;
  statistics.functors++;

  DEBUG(9, Sdprintf("%ld (new)\n", f));

  if ( functor_buckets * 2 < statistics.functors && !functor_locked )
    rehashFunctors();

  return f;
}


static void
rehashFunctors()
{ FunctorDef *oldtab = functorDefTable;
  int oldbucks       = functor_buckets;
  FunctorDef f, n;
  int done = 0;

  startCritical;
  functor_buckets *= 2;
  allocFunctorTable();

  DEBUG(0, Sdprintf("Rehashing functor-table to %d entries\n",
		    functor_buckets));

  for(f = oldtab[0]; f; f = n)
  { int v;

    while(isRef((word)f) )
    { f = *((FunctorDef *)unRef(f));
      if ( f == NULL )
	goto out;
    }
    n = f->next;
    done++;
    v = pointerHashValue(f->name, functor_buckets);
    f->next = functorDefTable[v];
    functorDefTable[v] = f;
  }

out:
  assert(done == statistics.functors);
  freeHeap(oldtab, oldbucks * sizeof(FunctorDef));
  endCritical;
}



FunctorDef
isCurrentFunctor(Atom atom, int arity)
{ int v = pointerHashValue(atom, functor_buckets);
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

static void
allocFunctorTable()
{ FunctorDef *f;
  int n;

  functorDefTable = allocHeap(functor_buckets * sizeof(FunctorDef));

  for(n=0, f=functorDefTable; n < (functor_buckets-1); n++, f++)
    *f = (FunctorDef)makeRef(f+1);
  *f = (FunctorDef) NULL;
}


void
initFunctors(void)
{ allocFunctorTable();

  { register FunctorDef f;
    register int v;

    for( f = &functors[0]; f->name; f++ )
    { v = pointerHashValue(f->name, functor_buckets);
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

  for( n=0; n < functor_buckets; n++ )
  { f = functorDefTable[n];
    for( ;f && !isRef((word)f); f = f->next )
    { if ( f->type != FUNCTOR_TYPE )
        Sdprintf("[ERROR: Functor %ld has bad type: %ld]\n", f, f->type);
      if ( f->arity < 0 || f->arity > 10 )	/* debugging only ! */
        Sdprintf("[ERROR: Functor %ld has dubious arity: %d]\n", f, f->arity);
      if ( !inCore(f->name) || f->name->type != ATOM_TYPE )
        Sdprintf("[ERROR: Functor %ld has illegal name: %ld]\n", f, f->name);
      if ( !( f->next == (FunctorDef) NULL ||
	      isRef((word)f->next) ||
	      inCore(f->next)) )
	Sdprintf("[ERROR: Functor %ld has illegal next: %ld]\n", f, f->next);
    }
    if ( (isRef((word)f) &&
	 ((FunctorDef *) unRef((word)f) != &functorDefTable[n+1])) )
      Sdprintf("[ERROR: Bad continuation pointer (fDef, n=%d)]\n", n);
    if ( f == (FunctorDef) NULL && n != (functor_buckets-1) )
      Sdprintf("[ERROR: illegal end pointer (fDef, n=%d)]\n", n);
  }
}
#endif

word
pl_current_functor(Word name, Word arity, word h)
{ FunctorDef fdef;
  int name_is_atom;
  mark m;

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

      if ( (name_is_atom = isAtom(*name)) )
      { int v = pointerHashValue((Atom)*name, functor_buckets);
	
	fdef = functorDefTable[v];
      } else
	fdef = functorDefTable[0];
      lockFunctors();
      break;
    case FRG_REDO:
      fdef = (FunctorDef) ForeignContextAddress(h);
      name_is_atom = isAtom(*name);
      break;
    case FRG_CUTTED:
    default:
      unlockFunctors();
      succeed;
  }

  DoMark(m);
  DEBUG(9, Sdprintf("current_functor(): fdef = %ld\n", fdef));
  for(; fdef; fdef = fdef->next)
  { if ( isRef((word)fdef) )
    { if ( name_is_atom )
	goto out;

      do
      { fdef = *((FunctorDef *)unRef(fdef));
	if (fdef == (FunctorDef) NULL)
	  fail;
      } while( isRef((word)fdef) );
    }
    if ( arity == 0 )
      continue;
    DoUndo(m);
    if ( !unifyAtomic(name, fdef->name) ||
	 !unifyAtomic(arity, consNum(fdef->arity)))
      continue;
    DEBUG(9, Sdprintf("Returning backtrack point %ld\n", fdef->next));

    return_next_table(FunctorDef, fdef, unlockFunctors());
  }

out:
  unlockFunctors();
  fail;
}
