/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Support predicates for bagof
*/

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
{ Record	key;			/* key binding */
  Record	value;			/* generator binding */
  Assoc		next;			/* next in chain */
};

Assoc bags = (Assoc) NULL;		/* chain of value pairs */

forwards word appendBag P((word, word));
forwards void freeAssoc P((Assoc, Assoc));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$record_bag(Key, Value)

Record a solution of bagof.  Key is a term  v(V0,  ...Vn),  holding  the
variable biding for solution `Gen'.  Key is ATOM_mark for the mark.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_record_bag(key, value)
register Word key, value;
{ register Assoc a = (Assoc) allocHeap(sizeof(struct assoc));

  a->next  = bags;
  bags = a;
  a->key   = copyTermToHeap(key);
  a->value = copyTermToHeap(value);

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This predicate will fail if no more records are left before the mark.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_collect_bag(bindings, bag)
Word bindings, bag;
{ word var_term;			/* v() term on global stack */
  word list = (word) ATOM_nil;		/* result list */
  register Assoc a, next;
  Assoc prev = (Assoc) NULL;
  
  if ( (a = bags) == (Assoc) NULL )
    fail;
  if ( !a || a->key->term == (word) ATOM_mark )
  { freeAssoc(prev, a);
    fail;				/* trapped the mark */
  }

  var_term = copyTermToGlobal(a->key);	/* get variable term on global stack */
  list = appendBag(list, copyTermToGlobal(a->value));

  next = a->next;
  freeAssoc(prev, a);  

  if ( next != (Assoc) NULL )
  { for( a = next, next = a->next; next; a = next, next = a->next )
    { if ( a->key->term == (word) ATOM_mark )
	break;
      if ( pl_structural_equal(&var_term, &a->key->term) == FALSE )
      { prev = a;
	continue;
      }

      list = appendBag(list, copyTermToGlobal(a->value));
      freeAssoc(prev, a);
    }
  }

  TRY( pl_unify(bindings, &var_term) );

  return pl_unify(bag, &list);
}


static
void
freeAssoc(prev, a)
Assoc prev, a;
{ if ( prev == NULL )
    bags = a->next;
  else
    prev->next = a->next;
  freeRecord(a->key);
  freeRecord(a->value);
  freeHeap(a, sizeof(struct assoc));
}


static word
appendBag(bag, term)
register word bag;
register word term;
{ register word result = globalFunctor(FUNCTOR_dot2);

  argTerm(result, 0) = term;
  argTerm(result, 1) = bag;

  return result;
}
