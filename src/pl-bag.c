/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Support predicates for bagof
*/

/*#define O_SECURE 1*/
#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines support  predicates  for  the  Prolog  all-solutions
predicates findall/3, bagof/3 and setof/3.  These predicates are:

	$record_bag(Key, Value)		Record a value under a key.
    	$collect_bag(Bindings, Values)	Retract all Solutions matching
					Bindings.

The (toplevel) remainder of the all-solutions predicates is  written  in
Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct assoc * Assoc;

struct assoc
{ Record	binding;
  Assoc		next;			/* next in chain */
};

static Assoc bags = (Assoc) NULL;	/* chain of value pairs */

forwards void freeAssoc(Assoc, Assoc);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$record_bag(Key, Value)

Record a solution of bagof.  Key is a term  v(V0,  ...Vn),  holding  the
variable binding for solution `Gen'.  Key is ATOM_mark for the mark.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_record_bag(Word key, Word value)
{ Assoc a = (Assoc) allocHeap(sizeof(struct assoc));
  word t = globalFunctor(FUNCTOR_minus2);

  pl_unify(argTermP(t, 0), key);
  pl_unify(argTermP(t, 1), value);

  a->next  = bags;
  bags = a;

  a->binding = copyTermToHeap(&t);

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This predicate will fail if no more records are left before the mark.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_SECURE
checkBags()
{ Assoc a;

  for(a=bags; a; a = a->next)
  { checkData(&a->key->term, TRUE);
    checkData(&a->value->term, TRUE);
  }
}
#endif

static word
globalTerm(FunctorDef fdef, ...)
{ va_list args;
  word rval = globalFunctor(fdef);
  int n;
  
  va_start(args, fdef);
  for(n=0; n<fdef->arity; n++)
    argTerm(rval, n) = va_arg(args, word);
  va_end(args);

  return rval;
}

word
pl_collect_bag(Word bindings, Word bag)
{ Word var_term = NULL;			/* v() term on global stack */
  word list = (word) ATOM_nil;		/* result list */
  word binding = 0;
  register Assoc a, next;
  Assoc prev = (Assoc) NULL;
  
  if ( (a = bags) == (Assoc) NULL )
    fail;
  if ( !a || argTerm(a->binding->term, 0) == (word) ATOM_mark )
  { freeAssoc(prev, a);
    fail;				/* trapped the mark */
  }

  lockp(&bag);
  lockp(&bindings);
  lockp(&var_term);
  lockw(&list);
  lockw(&binding);
					/* get variable term on global stack */
  binding  = copyTermToGlobal(a->binding);
  var_term = argTermP(binding, 0);
  pl_unify(bindings, var_term);
  list = globalTerm(FUNCTOR_dot2, argTerm(binding, 1), list);

  next = a->next;
  freeAssoc(prev, a);  

  if ( next != NULL )
  { for( a = next, next = a->next; next; a = next, next = a->next )
    { word t;
      Word key = argTermP(a->binding->term, 0);

      if ( *key == (word) ATOM_mark )
	break;
      if ( !pl_structural_equal(var_term, key) )
      { prev = a;
	continue;
      }

      t = copyTermToGlobal(a->binding);
      pl_unify(argTermP(t, 0), bindings); /* can this fail (no)? */
      list = globalTerm(FUNCTOR_dot2, argTerm(t, 1), list);
      SECURE(checkData(&list, FALSE));
      freeAssoc(prev, a);
    }
  }

  unlockw(&binding);
  unlockw(&list);
  unlockp(&var_term);
  unlockp(&bindings);
  unlockp(&bag);

  SECURE(checkData(var_term, FALSE));

  return pl_unify(bag, &list);
}


static
void
freeAssoc(Assoc prev, Assoc a)
{ if ( prev == NULL )
    bags = a->next;
  else
    prev->next = a->next;
  freeRecord(a->binding);
  freeHeap(a, sizeof(struct assoc));
}
