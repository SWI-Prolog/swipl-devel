/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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
{ struct findall_bag *parent;		/* parent bag */
  long		magic;			/* FINDALL_MAGIC */
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
  PL_LOCK(L_AGC);
  bag->parent = LD->bags.bags;
  LD->bags.bags = bag;
  PL_UNLOCK(L_AGC);

  return PL_unify_pointer(A1, bag);
}


static
PRED_IMPL("$add_findall_bag", 2, add_findall_bag, 0)
{ PRED_LD
  findall_bag *bag;
  Record r;

  if ( !get_bag(A1, &bag PASS_LD) )
    return FALSE;

  r = compileTermToHeap(A2, R_NOLOCK);
  pushSegStack(&bag->answers, &r);
  bag->gsize += r->gsize;
  bag->solutions++;

  if ( bag->gsize + bag->solutions*3 > limitStack(global)/sizeof(word) )
    return outOfStack(&LD->stacks.global, STACK_OVERFLOW_RAISE);

  PL_LOCK(L_AGC);			/* see queue_message() in */
  PL_UNLOCK(L_AGC);			/* pl-thread.c for the motivation */

  return TRUE;
}


static inline void
freeBag(findall_bag *bag ARG_LD)
{ bag->magic = 0;
  clearSegStack(&bag->answers);
  freeHeap(bag, sizeof(*bag));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we much lock  L_AGC,  as   otherwise  AGC  may  get in between
popSegStack and copyRecordToGlobal(). Alternatively  we   could  make an
interface that gets the top of the stack, processed it and then pops it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

  PL_LOCK(L_AGC);
  while(popSegStack(&bag->answers, &r))
  { copyRecordToGlobal(answer, r PASS_LD);
    PL_cons_list(list, answer, list);

    freeRecord(r);
  }

  assert(LD->bags.bags == bag);
  LD->bags.bags = bag->parent;
  PL_UNLOCK(L_AGC);

  freeBag(bag PASS_LD);

  return PL_unify(A2, list);
}


static
PRED_IMPL("$destroy_findall_bag", 1, destroy_findall_bag, 0)
{ PRED_LD
  findall_bag *bag;

  if ( PL_get_pointer(A1, (void**)&bag) && bag->magic == FINDALL_MAGIC )
  { Record r; 

    while(popSegStack(&bag->answers, &r))
      freeRecord(r);

    PL_LOCK(L_AGC);
    assert(LD->bags.bags == bag);
    LD->bags.bags = bag->parent;
    PL_UNLOCK(L_AGC);

    freeBag(bag PASS_LD);
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
		 *	  ATOM-GC SUPPORT	*
		 *******************************/

static void
markAtomsAnswers(void *data)
{ Record r = *((Record*)data);

  markAtomsRecord(r);
}


void
markAtomsFindall(PL_local_data_t *ld)
{ findall_bag *bag = ld->bags.bags;

  for( ; bag; bag = bag->parent )
    scanSegStack(&bag->answers, markAtomsAnswers);
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
EndPredDefs
