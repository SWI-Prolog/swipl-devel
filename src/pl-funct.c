/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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

/*#define O_DEBUG 1*/
#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functor (name/arity) handling.  A functor is a unique object (like atoms).
See pl-atom.c for many useful comments on the representation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LOCK()   PL_LOCK(L_FUNCTOR)
#define UNLOCK() PL_UNLOCK(L_FUNCTOR)
#undef LD
#define LD LOCAL_LD

#define functor_buckets (GD->functors.buckets)
#define functorDefTable (GD->functors.table)

static void	  allocFunctorTable(void);
static void	  rehashFunctors(void);

static void
putFunctorArray(size_t where, FunctorDef fd)
{ int idx = MSB(where);

  assert(where >= 0);

  if ( !GD->functors.array.blocks[idx] )
  { PL_LOCK(L_MISC);
    if ( !GD->functors.array.blocks[idx] )
    { size_t bs = (size_t)1<<idx;
      FunctorDef *newblock = PL_malloc_uncollectable(bs*sizeof(FunctorDef));

      GD->functors.array.blocks[idx] = newblock-bs;
    }
    PL_UNLOCK(L_MISC);
  }

  GD->functors.array.blocks[idx][where] = fd;
}


static void
registerFunctor(FunctorDef fd)
{ size_t index = GD->functors.highest++;
  int amask = (fd->arity < F_ARITY_MASK ? fd->arity : F_ARITY_MASK);

  fd->functor = MK_FUNCTOR(index, amask);
  putFunctorArray(index, fd);

  DEBUG(CHK_SECURE, assert(fd->arity == arityFunctor(fd->functor)));
}


functor_t
lookupFunctorDef(atom_t atom, unsigned int arity)
{ int v;
  FunctorDef f;

  LOCK();
  v = (int)pointerHashValue(atom, functor_buckets);

  DEBUG(9, Sdprintf("Lookup functor %s/%d = ", stringAtom(atom), arity));
  for(f = functorDefTable[v]; f; f = f->next)
  { if (atom == f->name && f->arity == arity)
    { DEBUG(9, Sdprintf("%p (old)\n", f));
      UNLOCK();
      return f->functor;
    }
  }
  f = (FunctorDef) allocHeapOrHalt(sizeof(struct functorDef));
  f->functor = 0L;
  f->name    = atom;
  f->arity   = arity;
  if ( atom == ATOM_call && arity > 8 )
    f->flags = CONTROL_F;
  else
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
rehashFunctors(void)
{ FunctorDef *oldtab = functorDefTable;
  int oldbucks       = functor_buckets;
  size_t index;
  int i, last = FALSE;

  functor_buckets *= 2;
  allocFunctorTable();

  DEBUG(MSG_HASH_STAT,
	Sdprintf("Rehashing functor-table to %d entries\n", functor_buckets));

  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    FunctorDef *b = GD->functors.array.blocks[i];

    if ( upto >= GD->functors.highest )
    { upto = GD->functors.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { FunctorDef f = b[index];

      if ( f )
      { size_t v = pointerHashValue(f->name, functor_buckets);

	f->next = functorDefTable[v];
	functorDefTable[v] = f;
      }
    }
  }

  freeHeap(oldtab, oldbucks * sizeof(FunctorDef));
}



functor_t
isCurrentFunctor(atom_t atom, unsigned int arity)
{ unsigned int v;
  FunctorDef f;

  LOCK();
  v = (unsigned int)pointerHashValue(atom, functor_buckets);
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
allocFunctorTable(void)
{ int size = functor_buckets * sizeof(FunctorDef);

  functorDefTable = allocHeapOrHalt(size);
  memset(functorDefTable, 0, size);
}


static void
registerBuiltinFunctors(void)
{ int size = sizeof(functors)/sizeof(builtin_functor) - 1;
  FunctorDef f = allocHeapOrHalt(size * sizeof(struct functorDef));
  const builtin_functor *d;

  GD->statistics.functors = size;

  for(d = functors; d->name; d++, f++)
  { size_t v = pointerHashValue(d->name, functor_buckets);

    f->name             = d->name;
    f->arity            = d->arity;
    f->flags		= 0;
    f->next             = functorDefTable[v];
    functorDefTable[v]  = f;
    registerFunctor(f);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functors are compiled with compileBody().   Make sure this is kept
consistent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerControlFunctors()
{ static functor_t control[] =
  { FUNCTOR_comma2,
    FUNCTOR_semicolon2,
    FUNCTOR_bar2,
    FUNCTOR_ifthen2,
    FUNCTOR_softcut2,
    FUNCTOR_not_provable1,
    FUNCTOR_colon2,			/* Module:Goal */
#ifdef O_CALL_AT_MODULE
    FUNCTOR_xpceref2,			/* Goal@Module */
#endif
    (functor_t) 0
  };
  functor_t *f;

  for(f	= control; *f; f++)
  { valueFunctor(*f)->flags |= CONTROL_F;
  }
}


static void
registerArithFunctors()
{ static functor_t arith[] =
  { FUNCTOR_ar_equals2,
    FUNCTOR_ar_not_equal2,
    FUNCTOR_smaller2,
    FUNCTOR_larger2,
    FUNCTOR_smaller_equal2,
    FUNCTOR_larger_equal2,
    FUNCTOR_is2,
    (functor_t) 0
  };
  functor_t *f;

  for(f	= arith; *f; f++)
  { valueFunctor(*f)->flags |= ARITH_F;
  }
}


void
initFunctors(void)
{ LOCK();
  if ( !functorDefTable )
  { initAtoms();
    functor_buckets = FUNCTORHASHSIZE;
    allocFunctorTable();
    GD->functors.highest = 1;
    registerBuiltinFunctors();
    registerControlFunctors();
    registerArithFunctors();
  }
  UNLOCK();
}


void
cleanupFunctors(void)
{ if ( functorDefTable )
  { int i;
    int builtin_count      = sizeof(functors)/sizeof(builtin_functor) - 1;
    FunctorDef builtin     = GD->functors.array.blocks[0][1];
    FunctorDef builtin_end = builtin+builtin_count;
    FunctorDef *fp0;

    freeHeap(builtin, builtin_count * sizeof(struct functorDef));

    for(i=0; (fp0=GD->functors.array.blocks[i]); i++)
    { size_t bs = (size_t)1<<i;
      size_t upto = (size_t)2<<i;
      FunctorDef *fp, *ep;

      fp0 += bs;
      fp = fp0;
      ep=fp+bs;
      if ( upto > GD->functors.highest )
	ep -= upto-GD->functors.highest;

      for(; fp<ep; fp++)
      { FunctorDef f = *fp;

	if ( !(f>=builtin && f<=builtin_end) )
	  freeHeap(f, sizeof(*f));
      }

      GD->functors.array.blocks[i] = NULL;
      PL_free(fp0);
    }

    freeHeap(functorDefTable, functor_buckets*sizeof(FunctorDef));
    functorDefTable = NULL;
    functor_buckets = 0;
  }
}


#if TEST
checkFunctors()
{ FunctorDef f;
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
pl_current_functor(term_t name, term_t arity, control_t h)
{ GET_LD
  atom_t nm = 0;
  size_t index;
  int i, last=FALSE;
  int  ar;
  fid_t fid;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_get_atom(name, &nm) &&
	   PL_get_integer(arity, &ar) )
	return isCurrentFunctor(nm, ar) ? TRUE : FALSE;

      if ( !(PL_is_integer(arity) || PL_is_variable(arity)) )
	return PL_error("current_functor", 2, NULL, ERR_DOMAIN,
			ATOM_integer, arity);

      if ( !(PL_is_atom(name) || PL_is_variable(name)) )
	return PL_error("current_functor", 2, NULL, ERR_DOMAIN,
			ATOM_atom, name);
      index = 1;
      break;
    case FRG_REDO:
      PL_get_atom(name, &nm);
      index = ForeignContextInt(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  fid = PL_open_foreign_frame();
  LOCK();
  for(i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    FunctorDef *b = GD->functors.array.blocks[i];

    if ( upto >= GD->functors.highest )
    { upto = GD->functors.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { FunctorDef fd = b[index];

      if ( fd && fd->arity > 0 && (!nm || nm == fd->name) )
      { if ( PL_unify_atom(name, fd->name) &&
	     PL_unify_integer(arity, fd->arity) )
	{ UNLOCK();
	  ForeignRedoInt(index+1);
	} else
	{ PL_rewind_foreign_frame(fid);
	}
      }
    }
  }

  UNLOCK();
  return FALSE;
}
