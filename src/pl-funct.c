/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functor (name/arity) handling.  A functor is a unique object (like atoms).
See pl-atom.c for many useful comments on the representation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LOCK()   PL_LOCK(L_FUNCTOR)
#define UNLOCK() PL_UNLOCK(L_FUNCTOR)

#define functor_buckets (GD->functors.buckets)
#define functorDefTable (GD->functors.table)

static void	  allocFunctorTable();
static void	  rehashFunctors();

#define maxFunctorIndex() entriesBuffer(&functor_array, FunctorDef)
#define getFunctorByIndex(i) baseBuffer(&functor_array, FunctorDef)[(i)]


static void
registerFunctor(FunctorDef fd)
{ int n = maxFunctorIndex();
  int amask = (fd->arity < F_ARITY_MASK ? fd->arity : F_ARITY_MASK);
    
  fd->functor = MK_FUNCTOR(n, amask);
  addBuffer(&functor_array, fd, FunctorDef);

  DEBUG(0, assert(fd->arity == arityFunctor(fd->functor)));
}


functor_t
lookupFunctorDef(atom_t atom, int arity)
{ int v;
  FunctorDef f;

  LOCK();
  v = pointerHashValue(atom, functor_buckets);

  DEBUG(9, Sdprintf("Lookup functor %s/%d = ", stringAtom(atom), arity));
  for(f = functorDefTable[v]; f; f = f->next)
  { if (atom == f->name && f->arity == arity)
    { DEBUG(9, Sdprintf("%p (old)\n", f));
      UNLOCK();
      return f->functor;
    }
  }
  f = (FunctorDef) allocHeap(sizeof(struct functorDef));
  f->functor = 0L;
  f->name    = atom;
  f->arity   = arity;
  f->flags   = 0;
  f->next    = functorDefTable[v];
  functorDefTable[v] = f;
  registerFunctor(f);
  GD->statistics.functors++;
  PL_register_atom(atom);

  DEBUG(9, Sdprintf("%p (new)\n", f));

  if ( functor_buckets * 2 < GD->statistics.functors )
    rehashFunctors();

  UNLOCK();

  return f->functor;
}


static void
rehashFunctors()
{ FunctorDef *oldtab = functorDefTable;
  int oldbucks       = functor_buckets;
  int i, mx = maxFunctorIndex();

  startCritical;
  functor_buckets *= 2;
  allocFunctorTable();

  DEBUG(0, Sdprintf("Rehashing functor-table to %d entries\n",
		    functor_buckets));

  for(i = 0; i<mx; i++)
  { FunctorDef f = getFunctorByIndex(i);
    int v = pointerHashValue(f->name, functor_buckets);

    f->next = functorDefTable[v];
    functorDefTable[v] = f;
  }

  freeHeap(oldtab, oldbucks * sizeof(FunctorDef));
  endCritical;
}



functor_t
isCurrentFunctor(atom_t atom, int arity)
{ int v;
  FunctorDef f;

  LOCK();
  v = pointerHashValue(atom, functor_buckets);
  for(f = functorDefTable[v]; f; f = f->next)
  { if ( atom == f->name && f->arity == arity )
    { UNLOCK();
      return f->functor;
    }
  }
  UNLOCK();

  return 0;
}

typedef struct
{ atom_t name;
  char   arity;
} builtin_functor;

#define FUNCTOR(n, a) { n, a }
static const builtin_functor functors[] = {
#include "pl-funct.ic"
FUNCTOR(NULL_ATOM, 0)
};
#undef FUNCTOR


static void
allocFunctorTable()
{ int size = functor_buckets * sizeof(FunctorDef);

  functorDefTable = allocHeap(size);
  memset(functorDefTable, 0, size);
}


static void
registerBuiltinFunctors()
{ int size = sizeof(functors)/sizeof(builtin_functor) - 1;
  FunctorDef f = allocHeap(size * sizeof(struct functorDef));
  const builtin_functor *d;

  GD->statistics.functors = size;

  for(d = functors; d->name; d++, f++)
  { int v = pointerHashValue(d->name, functor_buckets);

    f->name             = d->name;
    f->arity            = d->arity;
    f->next             = functorDefTable[v];
    functorDefTable[v]  = f;
    registerFunctor(f);
  }
}


void
initFunctors(void)
{ static int done = FALSE;

  LOCK();
  if ( !done )
  { done = TRUE;

    initAtoms();
    functor_buckets = FUNCTORHASHSIZE;
    initBuffer(&functor_array);
    allocFunctorTable();
    registerBuiltinFunctors();
  }
  UNLOCK();
}


#if TEST
checkFunctors()
{ register FunctorDef f;
  int n;

  for( n=0; n < functor_buckets; n++ )
  { f = functorDefTable[n];
    for( ;f ; f = f->next )
    { if ( f->arity < 0 || f->arity > 10 )	/* debugging only ! */
        Sdprintf("[ERROR: Functor %ld has dubious arity: %d]\n", f, f->arity);
      if ( !isArom(f->name) )
        Sdprintf("[ERROR: Functor %ld has illegal name: %ld]\n", f, f->name);
      if ( !( f->next == (FunctorDef) NULL ||
	      inCore(f->next)) )
	Sdprintf("[ERROR: Functor %ld has illegal next: %ld]\n", f, f->next);
    }
  }
}
#endif

word
pl_current_functor(term_t name, term_t arity, word h)
{ atom_t nm = 0;
  int  ar;
  mark m;
  int mx, i;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_get_atom(name, &nm) &&
	   PL_get_integer(arity, &ar) )
	return isCurrentFunctor(nm, ar) ? TRUE : FALSE;

      if ( !(PL_is_integer(arity) || PL_is_variable(arity)) )
	return PL_error("current_functor", 2, NULL, ERR_DOMAIN,
			ATOM_integer, arity);

      if ( !PL_is_variable(name) )
	return PL_error("current_functor", 2, NULL, ERR_DOMAIN,
			ATOM_atom, name);
      i = 0;
      break;
    case FRG_REDO:
      PL_get_atom(name, &nm);
      i = ForeignContextInt(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }
  DEBUG(9, Sdprintf("current_functor(): i = %d\n", i));

  LOCK();
  mx = maxFunctorIndex();
  Mark(m);
  for(; i<mx; i++)
  { FunctorDef fdef = getFunctorByIndex(i);

    if ( fdef->arity == 0 ||
         (nm && nm != fdef->name) )
      continue;
    if ( !PL_unify_atom(name, fdef->name) ||
	 !PL_unify_integer(arity, fdef->arity) )
    { Undo(m);

      continue;
    }
    DEBUG(9, Sdprintf("Returning backtrack point %ld\n", fdef->next));

    UNLOCK();
    ForeignRedoInt(i+1);
  }

  UNLOCK();
  fail;
}
