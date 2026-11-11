/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Keri Harris
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-funct.h"
#include "pl-fli.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functor (name/arity) handling.  A functor is a unique object (like atoms).
See pl-atom.c for many useful comments on the representation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef LD
#define LD LOCAL_LD

#define functorDefTable (GD->functors.table)

#ifdef O_PLMT

#define acquire_functor_table(t, b) \
  { FunctorTable _ft; \
    do \
    { _ft = LOAD_SEQ_CST(functorDefTable); \
      PUBLISH_HAZARD(LD->thread.info->access.functor_table, _ft); \
    } while ( _ft != LOAD_SEQ_CST(functorDefTable) ); \
    t = _ft->table; \
    b = _ft->buckets; \
  }

#define release_functor_table() \
  { LD->thread.info->access.functor_table = NULL; \
  }

#else

#define acquire_functor_table(t, b) \
  { t = functorDefTable->table; \
    b = functorDefTable->buckets; \
  }

#define release_functor_table() (void)0

#endif

static void	  allocFunctorTable(void);
static void	  rehashFunctors(void);

static void
allocateFunctorBlock(int idx)
{ PL_LOCK(L_MISC);

  if ( !GD->functors.array.blocks[idx] )
  { size_t bs = (size_t)1<<idx;
    FunctorDef *newblock;

    if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(FunctorDef))) )
      outOfCore();

    memset(newblock, 0, bs*sizeof(FunctorDef));
    GD->functors.array.blocks[idx] = newblock-bs;
  }

  PL_UNLOCK(L_MISC);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) The first two may  not  be   reordered  because  lookup  will return
fd->functor if it finds a valid functor. The second barrier ensures only
valid functors appear in the array.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerFunctor(FunctorDef fd)
{ size_t index;
  int idx;
  size_t amask;

  index = ATOMIC_INC(&GD->functors.highest) - 1;
  idx = MSB(index);

  if ( !GD->functors.array.blocks[idx] )
  { allocateFunctorBlock(idx);
  }

  amask = (fd->arity < F_ARITY_MASK ? fd->arity : F_ARITY_MASK);
  fd->functor = MK_FUNCTOR(index, amask);
  GD->functors.array.blocks[idx][index] = fd;
  MEMORY_RELEASE();			/* See (*) */
  fd->flags |= VALID_F;

  DEBUG(CHK_SECURE, assert(fd->arity == arityFunctor(fd->functor)));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lookup is lock-free.  Creating a functor  and rehashing the table happen
under L_FUNCTOR.  A new functor is  added   to  the  array (which is what
rehashFunctors() uses) before it is published   in  the table.  A lookup
may miss an existing functor while  rehashFunctors()  relinks the chains.
Therefore we search again under L_FUNCTOR before   we  conclude that the
functor does not exist.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define find_functor(atom, arity) LDFUNC(find_functor, atom, arity)

static functor_t
find_functor(DECL_LD atom_t atom, size_t arity)
{ FunctorDef *table;
  int buckets;
  functor_t rc = 0;

  acquire_functor_table(table, buckets);
  int v = (int)pointerHashValue(atom, buckets);
  for(FunctorDef f = table[v]; f; f = f->next)
  { if ( atom == f->name && f->arity == arity &&
	 FUNCTOR_IS_VALID(f->flags) )
    { rc = f->functor;
      break;
    }
  }
  release_functor_table();

  return rc;
}

/* Find a functor in the current table.  Caller holds L_FUNCTOR */

static FunctorDef
find_functor_locked(atom_t atom, size_t arity)
{ FunctorTable t = functorDefTable;
  int v = (int)pointerHashValue(atom, t->buckets);

  for(FunctorDef f = t->table[v]; f; f = f->next)
  { if ( atom == f->name && f->arity == arity )
      return f;
  }

  return NULL;
}


functor_t
lookupFunctorDef(DECL_LD atom_t atom, size_t arity)
{ functor_t rc;
  FunctorDef f;

  if ( (rc=find_functor(atom, arity)) )
    return rc;

  PL_LOCK(L_FUNCTOR);
  if ( (f=find_functor_locked(atom, arity)) )
  { rc = f->functor;
  } else
  { if ( functorDefTable->buckets * 2 < GD->statistics.functors )
      rehashFunctors();

    FunctorTable t = functorDefTable;
    int v = (int)pointerHashValue(atom, t->buckets);

    f = (FunctorDef) allocHeapOrHalt(sizeof(struct functorDef));
    f->functor = 0L;
    f->name    = atom;
    f->arity   = arity;
    f->flags   = 0;
    f->next    = t->table[v];
    registerFunctor(f);
    MEMORY_RELEASE();			/* publish complete functor */
    t->table[v] = f;
    rc = f->functor;

    ATOMIC_INC(&GD->statistics.functors);
    PL_register_atom(atom);
    DEBUG(9, Sdprintf("%p (new)\n", f));
  }
  PL_UNLOCK(L_FUNCTOR);

  return rc;
}


static void
maybe_free_functor_tables(void)
{ FunctorTable t = functorDefTable;

  while ( t )
  { FunctorTable t2 = t->prev;
    if ( t2 && !pl_functor_table_in_use(t2) )
    { t->prev = t2->prev;
      freeHeap(t2->table, t2->buckets * sizeof(FunctorDef));
      freeHeap(t2, sizeof(functor_table));
    }
    t = t->prev;
  }
}


static void
rehashFunctors(void)
{ FunctorTable newtab;
  size_t index;
  int i, last = false;

  if ( functorDefTable->buckets * 2 >= GD->statistics.functors )
    return;

  newtab = allocHeapOrHalt(sizeof(*newtab));
  newtab->buckets = functorDefTable->buckets * 2;
  newtab->table = allocHeapOrHalt(newtab->buckets * sizeof(FunctorDef));
  memset(newtab->table, 0, newtab->buckets * sizeof(FunctorDef));
  newtab->prev = functorDefTable;

  DEBUG(MSG_HASH_STAT,
	Sdprintf("Rehashing functor-table (%d --> %d)\n",
		 functorDefTable->buckets, newtab->buckets));

  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    FunctorDef *b;

    if ( (b=GD->functors.array.blocks[i]) )
    { size_t high = GD->functors.highest;

      if ( upto >= high )
      { upto = high;
	last = true;
      }

      for(; index<upto; index++)
      { FunctorDef f = b[index];

	if ( f && FUNCTOR_IS_VALID(f->flags) )
	{ size_t v = pointerHashValue(f->name, newtab->buckets);

	  f->next = newtab->table[v];
	  newtab->table[v] = f;
	}
      }
    }
  }

  MEMORY_RELEASE();			/* publish filled table */
  functorDefTable = newtab;
  maybe_free_functor_tables();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is lookupFunctorDef(), but failing (returns   0)  if the functor is
not known.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

functor_t
isCurrentFunctor(atom_t atom, size_t arity)
{ GET_LD
  functor_t rc;

  if ( !(rc=find_functor(atom, arity)) )
  { FunctorDef f;

    PL_LOCK(L_FUNCTOR);			/* we may miss it during a rehash */
    if ( (f=find_functor_locked(atom, arity)) )
      rc = f->functor;
    PL_UNLOCK(L_FUNCTOR);
  }

  return rc;
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
{ functorDefTable = allocHeapOrHalt(sizeof(*functorDefTable));
  functorDefTable->buckets = FUNCTORHASHSIZE;
  functorDefTable->table = allocHeapOrHalt(FUNCTORHASHSIZE * sizeof(FunctorDef));
  memset(functorDefTable->table, 0, FUNCTORHASHSIZE * sizeof(FunctorDef));
  functorDefTable->prev = NULL;
}


static void
registerBuiltinFunctors(void)
{ int size = sizeof(functors)/sizeof(builtin_functor) - 1;
  FunctorDef f = allocHeapOrHalt(size * sizeof(struct functorDef));
  const builtin_functor *d;

  GD->statistics.functors = size;

  for(d = functors; d->name; d++, f++)
  { size_t v = pointerHashValue(d->name, functorDefTable->buckets);

    f->name             = d->name;
    f->arity            = d->arity;
    f->flags		= 0;
    f->next             = functorDefTable->table[v];
    functorDefTable->table[v]  = f;
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
    FUNCTOR_dollar1,			/* $(Goal) */
#ifdef O_CALL_AT_MODULE
    FUNCTOR_at_sign2,			/* Goal@Module */
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
{ PL_LOCK(L_FUNCTOR);
  if ( !functorDefTable )
  { initAtoms();
    allocFunctorTable();
    GD->functors.highest = 1;
    registerBuiltinFunctors();
    registerControlFunctors();
    registerArithFunctors();
  }
  PL_UNLOCK(L_FUNCTOR);
}


void
cleanupFunctors(void)
{ FunctorTable table = functorDefTable;

  if ( table )
  { int i;
    int builtin_count      = sizeof(functors)/sizeof(builtin_functor) - 1;
    FunctorDef builtin     = GD->functors.array.blocks[0][1];
    FunctorDef builtin_end = builtin+builtin_count;
    FunctorDef *fp0;

    freeHeap(builtin, builtin_count * sizeof(struct functorDef));

    for(i=0; (fp0=GD->functors.array.blocks[i]); i++)
    { size_t bs = (size_t)1<<i;
      size_t upto = (size_t)2<<i;
      size_t high = GD->functors.highest;
      FunctorDef *fp, *ep;

      fp0 += bs;
      fp = fp0;
      ep=fp+bs;
      if ( upto > high )
	ep -= upto-high;

      for(; fp<ep; fp++)
      { FunctorDef f = *fp;

	if ( !(f>=builtin && f<=builtin_end) )
	  freeHeap(f, sizeof(*f));
      }

      GD->functors.array.blocks[i] = NULL;
      PL_free(fp0);
    }

    while ( table )
    { FunctorTable prev = table->prev;
      freeHeap(table->table, table->buckets * sizeof(FunctorDef));
      freeHeap(table, sizeof(functor_table));
      table = prev;
    }
    table = NULL;
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

foreign_t
pl_current_functor(term_t name, term_t arity, control_t h)
{ GET_LD
  atom_t nm = 0;
  size_t index;
  int i, last=false;
  int  ar;
  fid_t fid;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_get_atom(name, &nm) &&
	   PL_get_integer(arity, &ar) )
	return isCurrentFunctor(nm, ar) ? true : false;

      if ( !(PL_is_integer(arity) || PL_is_variable(arity)) )
	return PL_error("current_functor", 2, NULL, ERR_TYPE,
			ATOM_integer, arity);

      if ( !(PL_is_atom(name) || PL_is_variable(name)) )
	return PL_error("current_functor", 2, NULL, ERR_TYPE,
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
  PL_LOCK(L_FUNCTOR);
  for(i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->functors.highest;
    FunctorDef *b = GD->functors.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = true;
    }

    for(; index<upto; index++)
    { FunctorDef fd = b[index];

      if ( fd && FUNCTOR_IS_VALID(fd->flags) &&
           (!nm || nm == fd->name) )
      { if ( PL_unify_atom(name, fd->name) &&
	     PL_unify_integer(arity, fd->arity) )
	{ PL_UNLOCK(L_FUNCTOR);
	  ForeignRedoInt(index+1);
	} else
	{ PL_rewind_foreign_frame(fid);
	}
      }
    }
  }

  PL_UNLOCK(L_FUNCTOR);
  return false;
}


size_t
functor_space(void)
{ size_t size = ((size_t)2<<MSB(GD->functors.highest))*sizeof(FunctorDef);

  size += GD->functors.highest * sizeof(struct functorDef);

  return size;
}
