/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: indexing support
*/

#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clause indexing.  Clauses store an  `index  structure',  which  provides
summary information on the unification behaviour of the clause (e.i. its
head  arguments.   This  structure  consists  of  two words: a key and a
varmask.  Indexing can be done with upto 4 arguments.   Both  words  are
divided  into  the  same  number  of  bit  groups  as  there are indexed
arguments.  If an argument  is  indexable  (atom,  integer  or  compound
term),  the  corresponding  bit group is filled with bits taken from the
atom  pointer,  integer  or  functor  pointer.    In   this   case   all
corresponding  bits  in  the varmask field are 1.  Otherwise the bits in
both the varmask and the key are all 0.

To find a clause using indexing, we calculate an  index  structure  from
the  calling arguments to the goal using the same rules.  Now, we can do
a mutual `and' using the varmasks on the keys and  compare  the  result.
If  equal  a  good  chance  for a possible unification exists, otherwise
unification will definitely fail.  See matchIndex() and findClause().

Care has been taken to get this code as fast as  possible,  notably  for
indexing only on the first argument as this is default.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* 1 <= c <= 4 */

#define SHIFT(c, a)	((32/(c)) * a)
#define MASK(c)		(c == 1 ? ~0L : ((1L << (32/(c))) - 1))
#define VM(c, a)	(~(MASK(c) << SHIFT(c, a)))

#define Shift(c, a)	(mask_shift[c][a])
#define Mask(c)		(mask_mask[c])
#define varMask(c, a)	(variable_mask[c][a])

#define matchIndex(i1, i2)	(((i1).key & (i2).varmask) ==\
				  ((i2).key & (i1).varmask))

static unsigned long variable_mask[][4] =
  { { 0,        0,        0,        0 }, 
/*  { VM(1, 0), 0,        0,        0 }, */
    { ~0L,      0,        0,        0 }, 
    { VM(2, 0), VM(2, 1), 0,        0 }, 
    { VM(3, 0), VM(3, 1), VM(3, 2), 0 }, 
    { VM(4, 0), VM(4, 1), VM(4, 2), VM(4, 3) }
  };

static int mask_shift[][4] =
  { { 0,           0,           0,           0 }, 
    { SHIFT(1, 0), 0,           0,           0 }, 
    { SHIFT(2, 0), SHIFT(2, 1), 0,           0 }, 
    { SHIFT(3, 0), SHIFT(3, 1), SHIFT(3, 2), 0 }, 
    { SHIFT(4, 0), SHIFT(4, 1), SHIFT(4, 2), SHIFT(4, 3) }
  };

static unsigned long mask_mask[] =
  { 0, /*MASK(1)*/ ~0L, MASK(2), MASK(3), MASK(4)
  };

/*  Determine cardinality (= # 1's) of bit pattern.

 ** Sun Sep 11 13:19:41 1988  jan@swivax.UUCP (Jan Wielemaker)  */

int
cardinalityPattern(register unsigned long pattern)
{ register int result = 0;

  for(; pattern; pattern >>= 1)
    if (pattern & 0x1)
      result++;

  return result;
}

struct index
getIndex(register Word argv, register unsigned long pattern, int card)
{ static struct index result;

  if ( pattern == 0x1L )
  { deRef(argv);
    if (isVar(*argv) || isIndirect(*argv) )
    { result.key = result.varmask = 0;
      return result;
    }
    result.key = (isTerm(*argv) ? (word) functorTerm(*argv) : *argv);
    result.varmask = ~0L;

    return result;
  } else
  { register Word k;
    register word key;
    register int a;

    result.key = 0;
    result.varmask = ~0L;			/* all 1s */

    for(a = 0; a < card; a++, pattern >>= 1, argv++)
    { for(;(pattern & 0x1) == 0; pattern >>= 1)
	argv++;

      deRef2(argv, k);
      if (isVar(*k) || isIndirect(*k) )
      { result.varmask &= varMask(card, a);
	continue;
      }
      key = (isTerm(*k) ? (word) functorTerm(*k) : *k);
      key = key >> 2;
      result.key |= ((key & Mask(card)) << Shift(card, a) );
    }
  }

  return result;
}

Clause
findClause(register Clause cl, register Word argv, register Definition def, bool *deterministic)
{ *deterministic = FALSE;

  if ( def->indexPattern == 0x0L )
  { DEBUG(9, printf("Not indexed.\n"));
    while(cl && true(cl, ERASED))
    { DEBUG(9, printf("Skipping erased clause.\n"));
      cl = cl->next;
    }
    DEBUG(9, printf("Returning clause 0x%lx\n", (unsigned long) cl));
    if ( cl && !cl->next )
      *deterministic = TRUE;
    return cl;
  } else if ( def->indexPattern == 0x1L )
  { register word key;

    deRef(argv);
    if (isVar(*argv) || isIndirect(*argv))
    { while(cl && true(cl, ERASED))
	cl = cl->next;
      if ( cl && !cl->next )
	*deterministic = TRUE;
      return cl;
    }
    key = (isTerm(*argv) ? (word) functorTerm(*argv) : *argv);
    for(;cl ; cl = cl->next)
    { if ((key & cl->index.varmask) == cl->index.key && false(cl, ERASED))
      { Clause result = cl;
      
	for( cl = cl->next; cl; cl = cl->next )
	{ if ((key & cl->index.varmask) == cl->index.key && false(cl, ERASED))
	    return result;
	}
	*deterministic = TRUE;

	return result;
      }
    }
    return (Clause) NULL;
  } else
  { struct index argIndex;

    argIndex = getIndex(argv, def->indexPattern, def->indexCardinality);
    for(; cl; cl = cl->next)
    { if (matchIndex(argIndex, cl->index) && false(cl, ERASED))
      { Clause result = cl;
      
	for( cl = cl->next; cl; cl = cl->next )
	{ if (matchIndex(argIndex, cl->index) && false(cl, ERASED))
	    return result;
	}
	*deterministic = TRUE;

	return result;
      }
    }
    return (Clause) NULL;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Recalculate the index of  a  clause  after  the  index  pattern  on  the
predicate  has been changed.  The head of the clause is decompiled.  The
resulting term is simply discarded as it cannot have links to any  other
part of the stacks (e.g. backtrailing is not needed).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
reindexClause(Clause clause)
{ Word head;
  Procedure proc = clause->procedure;
  mark m;

  if (proc->definition->indexPattern == 0x0)
    succeed;

  Mark(m);
  head = newTerm();
  if ( !decompileHead(clause, head) )
  { sysError("Failed to decompile head of %s", procedureName(proc));
    fail;
  }

  clause->index = getIndex(argTermP(*head, 0),
			   proc->definition->indexPattern, 
			   proc->definition->indexCardinality);
  Undo(m);

  succeed;
}

bool
indexPatternToTerm(Procedure proc, Word value)
{ Word argp;
  unsigned long pattern = proc->definition->indexPattern;
  int n, arity = proc->functor->arity;

  if (pattern == 0)
    fail;

  deRef(value);
  TRY(unifyFunctor(value, proc->functor) );
  argp = argTermP(*value, 0);

  for(n=0; n<arity; n++, argp++, pattern >>= 1)
    TRY(unifyAtomic(argp, consNum((pattern & 0x1) ? 1 : 0) ));

  succeed;
}
