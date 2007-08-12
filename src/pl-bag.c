/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*#define O_SECURE 1*/
/*#define O_DEBUG 1*/
#include "pl-incl.h"

#undef LD
#define LD LOCAL_LD


		 /*******************************
		 *      NEW IMPLEMENTATION	*
		 *******************************/

#define FINDALL_MAGIC	0x37ac78fe

typedef struct findall_bag
{ long		magic;			/* FINDALL_MAGIC */
  segstack	answers;		/* list of ansers */
  size_t	solutions;		/* count # solutions */
  size_t	gsize;			/* required size on stack */
} findall_bag;


static int
get_bag(term_t t, findall_bag **bag ARG_LD)
{ findall_bag *b;

  if ( PL_get_pointer(t, (void**)&b) && b->magic == FINDALL_MAGIC )
  { *bag = b;
    return TRUE;
  } else
  { PL_error(NULL, 0, NULL, ERR_CHARS_TYPE, "pointer", t);
    return FALSE;
  }
}

static 
PRED_IMPL("$new_findall_bag", 1, new_findall_bag, 0)
{ PRED_LD
  findall_bag *bag = allocHeap(sizeof(*bag));

  memset(bag, 0, sizeof(*bag));
  bag->magic = FINDALL_MAGIC;
  bag->answers.unit_size = sizeof(Record);

  return PL_unify_pointer(A1, bag);
}


static
PRED_IMPL("$add_findall_bag", 2, add_findall_bag, 0)
{ PRED_LD
  findall_bag *bag;
  Record r;

  if ( !get_bag(A1, &bag PASS_LD) )
    return FALSE;

  r = compileTermToHeap(A2, 0);
  pushSegStack(&bag->answers, &r);
  bag->gsize += r->gsize;
  bag->solutions++;

  if ( bag->gsize + bag->solutions*3 > limitStack(global)/sizeof(word) )
    return outOfStack(&LD->stacks.global, STACK_OVERFLOW_RAISE);

  return TRUE;
}


static 
PRED_IMPL("$collect_findall_bag", 3, collect_findall_bag, 0)
{ PRED_LD
  findall_bag *bag;
  Record r;
  term_t list = PL_copy_term_ref(A3);
  term_t answer = PL_new_term_ref();

  if ( !get_bag(A1, &bag PASS_LD) )
    return FALSE;

  if ( bag->gsize + bag->solutions*3 > spaceStack(global)/sizeof(word) )
  { garbageCollect(NULL, NULL);

    if ( bag->gsize + bag->solutions*3 > spaceStack(global)/sizeof(word) )
      return outOfStack(&LD->stacks.global, STACK_OVERFLOW_RAISE);
  }

  while(popSegStack(&bag->answers, &r))
  { copyRecordToGlobal(answer, r PASS_LD);
    PL_cons_list(list, answer, list);

    freeRecord(r);
  }

  clearSegStack(&bag->answers);
  bag->magic = 0;
  freeHeap(bag, sizeof(*bag));

  return PL_unify(A2, list);
}


static
PRED_IMPL("$destroy_findall_bag", 1, destroy_findall_bag, 0)
{ PRED_LD
  findall_bag *bag;

  if ( PL_get_pointer(A1, (void**)&bag) && bag->magic == FINDALL_MAGIC )
  { Record r; 

    bag->magic = 0;
    while(popSegStack(&bag->answers, &r))
      freeRecord(r);

    clearSegStack(&bag->answers);
    freeHeap(bag, sizeof(*bag));
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bind_bagof_keys(+Vars, +List:Key-Terms) is det
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$bind_bagof_keys", 2, bind_bagof_keys, 0)
{ PRED_LD
  term_t list = PL_copy_term_ref(A2);
  term_t head = PL_new_term_ref();
  term_t a = PL_new_term_ref();
  term_t key = PL_new_term_ref();
  term_t vars;
  int i, arity;

  if ( !PL_get_name_arity(A1, NULL, &arity) )
    fail;
  vars = PL_new_term_refs(arity);
  for(i=1; i<=arity; i++)
  { _PL_get_arg(i, A1, vars+i-1);
  }
  
  while(PL_get_list(list, head, list))
  { if ( !PL_get_arg(1, head, key) )
      fail;

    for(i=1; i<=arity; i++)
    { _PL_get_arg(i, key, a);
      if ( PL_is_variable(a) )
      { PL_unify(a, vars+i-1);
      }
    }
  }

  succeed;
}



		 /*******************************
		 *     OLD IMPLEMENTATION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines support  predicates  for  the  Prolog  all-solutions
predicates findall/3, bagof/3 and setof/3.  These predicates are:

	$record_bag(Key, Value)		Record a value under a key.
    	$collect_bag(Bindings, Values)	Retract all Solutions matching
					Bindings.

The (toplevel) remainder of the all-solutions predicates is  written  in
Prolog.

Possible enhancements:

	* Create bag as object-by-handle (no more global stuff needed)
	* Store global size accumulated sofar, so we can
		- Throw an error if we have too many solutions for the
		  stack
		- Allocate stack at once, so we can create the list
		  without checking and unification.
	* Collect setof/3 results in an avl tree, so we can kill
	  duplicates as we go along.
	* Make versions limiting time and/or # solutions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define alist LD->bags.bags		/* Each thread has its own */
					/* storage for this */

typedef struct assoc
{ Record record;
  struct assoc *next;
} *Assoc;


static void
freeAssoc(Assoc prev, Assoc a ARG_LD)
{ if ( prev == NULL )
  { alist = a->next;
  } else
    prev->next = a->next;

  if ( a->record )
    freeRecord(a->record);

  freeHeap(a, sizeof(*a));
}


void
resetBags()				/* cleanup after an abort */
{ GET_LD
  Assoc a, next;

  for( a=alist; a; a = next )
  { next = a->next;
    if ( a->record )
      freeRecord(a->record);
    freeHeap(a, sizeof(*a));
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$record_bag(Key-Value)

Record a solution of bagof.  Key is a term  v(V0,  ...Vn),  holding  the
variable binding for solution `Gen'.  Key is ATOM_mark for the mark.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$record_bag", 1, record_bag, 0)
{ PRED_LD
  Assoc a = allocHeap(sizeof(*a));

  if ( PL_is_atom(A1) )			/* mark */
  { a->record = 0;
  } else
    a->record = compileTermToHeap(A1, 0);

  DEBUG(1, { Sdprintf("Recorded %p: ", a->record);
	     PL_write_term(Serror, A1, 1200, PL_WRT_ATTVAR_WRITE);
	     Sdprintf("\n");
	   });

  a->next    = alist;
  alist      = a;

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This predicate will fail if no more records are left before the mark.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$collect_bag", 3, collect_bag, 0)
{ PRED_LD
  
  term_t bindings = A1;
  term_t bag = A2;
  term_t tail = A3;

  term_t var_term = PL_new_term_refs(4);	/* v() term on global stack */
  term_t list     = var_term+1;			/* list to construct */
  term_t binding  = var_term+2;			/* binding */
  term_t tmp      = var_term+3;
  Assoc a, next;
  Assoc prev = NULL;
  
  if ( !(a = alist) )
    fail;
  if ( !a->record )
  { freeAssoc(prev, a PASS_LD);
    fail;				/* trapped the mark */
  }

  PL_put_term(list, tail);
					/* get variable term on global stack */
  copyRecordToGlobal(binding, a->record PASS_LD);
  DEBUG(9, Sdprintf("First binding (%p): ", a->record);
	   PL_write_term(Serror, binding, 1200, PL_WRT_ATTVAR_WRITE);
	   Sdprintf("\n"));
  _PL_get_arg(1, binding, var_term);
  PL_unify(bindings, var_term);
  _PL_get_arg(2, binding, tmp);
  PL_cons_list(list, tmp, list);

  next = a->next;
  freeAssoc(prev, a PASS_LD);  

  if ( next != NULL )
  { for( a = next, next = a->next; next; a = next, next = a->next )
    { if ( !a->record )
	break;

      if ( !structuralEqualArg1OfRecord(var_term, a->record PASS_LD) )
      { prev = a;
	continue;
      }

      copyRecordToGlobal(binding, a->record PASS_LD);
      _PL_get_arg(1, binding, tmp);
      PL_unify(tmp, bindings);
      _PL_get_arg(2, binding, tmp);
      PL_cons_list(list, tmp, list);
      SECURE(checkData(valTermRef(list)));
      freeAssoc(prev, a PASS_LD);
    }
  }

  SECURE(checkData(valTermRef(var_term)));

  return PL_unify(bag, list);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An exception was generated during  the   execution  of  the generator of
findall/3, bagof/3 or setof/3. Reclaim  all   records  and  re-throw the
exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
discardBag(ARG1_LD)
{ Assoc a, next;

  for( a=alist; a; a = next )
  { if ( a->record )
    { DEBUG(1, Sdprintf("\tFree %p\n", a->record));
      freeRecord(a->record);
      next = a->next;
    } else
    { alist = a->next;
      next = NULL;
    }

    freeHeap(a, sizeof(*a));
  }
}


static
PRED_IMPL("$discard_bag", 0, discard_bag, 0)
{ PRED_LD

  discardBag(PASS_LD1);

  succeed;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(bag)
  PRED_DEF("$new_findall_bag", 1, new_findall_bag, 0)
  PRED_DEF("$add_findall_bag", 2, add_findall_bag, 0)
  PRED_DEF("$collect_findall_bag", 3, collect_findall_bag, 0)
  PRED_DEF("$destroy_findall_bag", 1, destroy_findall_bag, 0)
  PRED_DEF("$bind_bagof_keys", 2, bind_bagof_keys, 0)

					/* old stuff */
  PRED_DEF("$record_bag", 1, record_bag, 0)
  PRED_DEF("$collect_bag", 3, collect_bag, 0)
  PRED_DEF("$discard_bag", 0, discard_bag, 0)
EndPredDefs
