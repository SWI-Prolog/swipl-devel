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


#define arityMask(n) (((n) < F_ARITY_MASK ? (n) : F_ARITY_MASK) << LMASK_BITS)

static void
registerFunctor(FunctorDef fd)
{ int n = entriesBuffer(&functor_array, FunctorDef);
    
  fd->functor = ((n<<(LMASK_BITS+F_ARITY_BITS)) |
		 arityMask(fd->arity) |
		 TAG_ATOM|STG_GLOBAL);
  addBuffer(&functor_array, fd, FunctorDef);
}


FunctorDef
lookupFunctorDef(atom_t atom, int arity)
{ int v = pointerHashValue(atom, functor_buckets);
  register FunctorDef f;

  DEBUG(9, Sdprintf("Lookup functor %s/%d = ", stringAtom(atom), arity));
  for(f = functorDefTable[v]; f && !isTableRef(f); f = f->next)
  { if (atom == f->name && f->arity == arity)
    { DEBUG(9, Sdprintf("%ld (old)\n", f));
      return f;
    }
  }
  f = (FunctorDef) allocHeap(sizeof(struct functorDef));
  f->next    = functorDefTable[v];
  f->functor = 0L;
  f->name    = atom;
  f->arity   = arity;
  f->flags   = 0;
  functorDefTable[v] = f;
  statistics.functors++;

  DEBUG(9, Sdprintf("%ld (new)\n", f));

  if ( functor_buckets * 2 < statistics.functors && !functor_locked )
    rehashFunctors();

  registerFunctor(f);
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

    while(isTableRef(f) )
    { f = unTableRef(FunctorDef, f);
      if ( !f )
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
isCurrentFunctor(atom_t atom, int arity)
{ int v = pointerHashValue(atom, functor_buckets);
  FunctorDef f;

  for(f = functorDefTable[v]; f && !isTableRef(f); f = f->next)
  { if (atom == f->name && f->arity == arity)
      return f;
  }

  return (FunctorDef) NULL;
}


#define FUNCTOR(n, a) { NULL, 0L, n, a }
struct functorDef functors[] = {
#include "pl-funct.ic"
FUNCTOR(NULL_ATOM, 0)
};
#undef FUNCTOR


static void
allocFunctorTable()
{ FunctorDef *f;
  int n;

  functorDefTable = allocHeap(functor_buckets * sizeof(FunctorDef));

  for(n=0, f=functorDefTable; n < (functor_buckets-1); n++, f++)
    *f = makeTableRef(f+1);
  *f = NULL;
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
      registerFunctor(f);
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
    for( ;f && !isTableRef(f); f = f->next )
    { if ( f->arity < 0 || f->arity > 10 )	/* debugging only ! */
        Sdprintf("[ERROR: Functor %ld has dubious arity: %d]\n", f, f->arity);
      if ( !isArom(f->name) )
        Sdprintf("[ERROR: Functor %ld has illegal name: %ld]\n", f, f->name);
      if ( !( f->next == (FunctorDef) NULL ||
	      isTableRef(f->next) ||
	      inCore(f->next)) )
	Sdprintf("[ERROR: Functor %ld has illegal next: %ld]\n", f, f->next);
    }
    if ( (isTableRef(f) &&
	 (unTableRef(FunctorDef, f) != &functorDefTable[n+1])) )
      Sdprintf("[ERROR: Bad continuation pointer (fDef, n=%d)]\n", n);
    if ( f == (FunctorDef) NULL && n != (functor_buckets-1) )
      Sdprintf("[ERROR: illegal end pointer (fDef, n=%d)]\n", n);
  }
}
#endif

word
pl_current_functor(term_t name, term_t arity, word h)
{ FunctorDef fdef;
  atom_t nm;
  int  ar;
  int name_is_atom;
  mark m;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( (name_is_atom = PL_get_atom(name, &nm)) &&
	   PL_get_integer(arity, &ar) )
	return isCurrentFunctor(nm, ar) ? FALSE : TRUE;

      if ( !(PL_is_integer(arity) || PL_is_variable(arity)) )
	return warning("current_functor/2: instantiation fault");

      if ( name_is_atom )
      { int v = pointerHashValue(nm, functor_buckets);
	
	fdef = functorDefTable[v];
      } else
      { if ( !PL_is_variable(name) )
	  return warning("current_functor/2: instantiation fault");

	fdef = functorDefTable[0];
      }
      lockFunctors();
      break;
    case FRG_REDO:
      fdef = ForeignContextPtr(h);
      name_is_atom = PL_is_atom(name);
      break;
    case FRG_CUTTED:
    default:
      unlockFunctors();
      succeed;
  }

  Mark(m);
  DEBUG(9, Sdprintf("current_functor(): fdef = %ld\n", fdef));
  for(; fdef; fdef = fdef->next)
  { if ( isTableRef(fdef) )
    { if ( name_is_atom )
	goto out;

      do
      { fdef = unTableRef(FunctorDef, fdef);
	if ( !fdef )
	  goto out;
      } while( isTableRef(fdef) );
    }
    if ( fdef->arity == 0 )
      continue;
    Undo(m);
    if ( !PL_unify_atom(name, fdef->name) ||
	 !PL_unify_integer(arity, fdef->arity) )
      continue;
    DEBUG(9, Sdprintf("Returning backtrack point %ld\n", fdef->next));

    return_next_table(FunctorDef, fdef, unlockFunctors());
  }

out:
  unlockFunctors();
  fail;
}
