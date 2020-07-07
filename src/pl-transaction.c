/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
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

#include "pl-incl.h"
#include "pl-transaction.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module implements _transactions_, notably _isolating_ transactions.
This reuses most of the visibility stuff we need anyway for the _logical
updates semantics_ that  dictate  that  a   running  goal  on  a dynamic
predicate does not see changes to the predicate.

Transactions take this a step further:

  - The generation space 1..GEN_TRANSACTION_BASE is used for _globally_
    visible data.
  - The remainder is separated in 2^31 segments of 2^32 generations, as
    defined by GEN_TRANSACTION_SIZE (2^32).
  - A thread starting a transaction reserves as space starting at
    GEN_TRANSACTION_BASE+TID*GEN_TRANSACTION_SIZE and sets its
    LD->transaction.generation to the base.
  - setGenerationFrame() sets the frame generation to the transaction
    (if active) or the global generation.

Clause updates are handles as follows inside a transaction:

  - The clause is added to the LD->transaction.clauses table, where
    the value is GEN_ASSERTED or the local offset of the deleted
    generation (gen-LD->transaction.gen_base).
  - If the clause was retracted, clause.tr_erased_no is incremented.

A clause is visible iff

  - Its it is visible in the frame.generation.  In a transaction, this
    means it is created inside the transaction.
  - If we are in a transaction (gen >= GEN_TRANSACTION_BASE) and the
    clause was visible in the transaction start generation *and* the
    clause was not retracted in this transaction, it is visible.  The
    latter is the case if `clause.tr_erased_no > 0`, the clause is in
    our LD->transaction.clauses table at a generation smaller than the
    current.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* Avoid conflict with HTABLE_TOMBSTONE and HTABLE_SENTINEL */
#define GEN_ASSERTED ((uintptr_t)(void*)-3)

typedef struct tr_stack
{ struct tr_stack *parent;		/* parent transaction */
  gen_t            generation;		/* Parent generation */
  Table		   clauses;		/* Parent changed clauses */
  term_t	   id;			/* Parent goal */
} tr_stack;

static void
tr_free_clause_symbol(void *k, void *v)
{ Clause cl = k;
  (void)v;

  release_clause(cl);
}

static Table
tr_clause_table(ARG1_LD)
{ Table t;

  if ( !(t=LD->transaction.clauses) )
  { t = newHTable(16);
    t->free_symbol = tr_free_clause_symbol;
    LD->transaction.clauses = t;
  }

  return t;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This distinguishes two cases. If we retract a globally visible clause we
may need to commit and we  need  to   know  the  generation  at which we
retracted the clause.

If we added the clause ourselves, we no   longer  have to commit it. Its
visibility is guaranteed by the created and erased generations, so there
is no need to keep it in our tables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
transaction_retract_clause(Clause clause ARG_LD)
{ if ( clause->generation.created < LD->transaction.gen_base )
  { uintptr_t lgen = ( next_generation(clause->predicate PASS_LD) -
		       LD->transaction.gen_base
		     );

    acquire_clause(clause);
    ATOMIC_INC(&clause->tr_erased_no);
    addHTable(tr_clause_table(PASS_LD1), clause, (void*)lgen);
  } else if ( LD->transaction.clauses )
  { deleteHTable(LD->transaction.clauses, clause);
  }

  return TRUE;
}

int
transaction_assert_clause(Clause clause ARG_LD)
{ acquire_clause(clause);
  addHTable(tr_clause_table(PASS_LD1), clause, (void*)GEN_ASSERTED);

  return TRUE;
}


int
transaction_visible_clause(Clause cl, gen_t gen ARG_LD)
{ if ( cl->generation.created <= LD->transaction.gen_start &&
       cl->generation.erased   > LD->transaction.gen_start )
  { intptr_t lgen;

    if ( cl->tr_erased_no && LD->transaction.clauses &&
	 (lgen = (intptr_t)lookupHTable(LD->transaction.clauses, cl)) &&
	 lgen != GEN_ASSERTED )
    { if ( lgen+LD->transaction.gen_base <= gen )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


static int
transaction_commit(ARG1_LD)
{ if ( LD->transaction.clauses )
  { gen_t gen_commit;

    PL_LOCK(L_GENERATION);
    gen_commit = global_generation()+1;

    for_table(LD->transaction.clauses, n, v,
	      { Clause cl = n;

		if ( v != (void*)GEN_ASSERTED ) /* we erased the clause */
		{ DEBUG(MSG_COMMIT,
			Sdprintf("Commit erased clause for %s\n",
				 predicateName(cl->predicate)));
		  ATOMIC_DEC(&cl->tr_erased_no);
		  retract_clause(cl, gen_commit PASS_LD);
		} else
		{ if ( false(cl, CL_ERASED) )
		  { cl->generation.created = gen_commit;
		    DEBUG(MSG_COMMIT,
			Sdprintf("Commit added clause for %s\n",
				 predicateName(cl->predicate)));
		  } else
		  { DEBUG(MSG_COMMIT,
			  Sdprintf("Discarded in-transaction clause for %s\n",
				   predicateName(cl->predicate)));
		    cl->generation.created = 0;
		    cl->generation.erased  = 0;
		  }
		}
	      });
    GD->_generation = gen_commit;
    PL_UNLOCK(L_GENERATION);

    destroyHTable(LD->transaction.clauses);
    LD->transaction.clauses = NULL;
  }

  return TRUE;
}

static int
transaction_discard(ARG1_LD)
{ if ( LD->transaction.clauses )
  { for_table(LD->transaction.clauses, n, v,
	      { Clause cl = n;

		if ( v != (void*)GEN_ASSERTED )
		{ ATOMIC_DEC(&cl->tr_erased_no);
		} else
		{ if ( false(cl, CL_ERASED) )
		  { retract_clause(cl, GD->_generation-1 PASS_LD);
		  } else
		  { cl->generation.created = 0;
		    cl->generation.erased  = 0;
		  }
		}
	      });
    destroyHTable(LD->transaction.clauses);
    LD->transaction.clauses = NULL;
  }

  return TRUE;
}

static void
merge_tables(Table into, Table from)
{ for_table(from, n, v,
	    { Clause cl = n;

	      acquire_clause(cl);
	      addHTable(into, n, v);
	    });
}


		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

#define TR_TRANSACTION		0x0001
#define TR_SNAPSHOT		0x0002

static int
transaction(term_t goal, int flags ARG_LD)
{ int rc;

  if ( LD->transaction.generation )
  { tr_stack parent = { .generation = LD->transaction.generation,
			.clauses    = LD->transaction.clauses,
			.parent     = LD->transaction.stack,
			.id         = LD->transaction.id
		      };

    LD->transaction.clauses = NULL;
    LD->transaction.id      = goal;
    LD->transaction.stack   = &parent;
    rc=callProlog(NULL, goal, PL_Q_PASS_EXCEPTION, NULL);
    if ( rc && (flags&TR_TRANSACTION) && LD->transaction.clauses )
    { if ( parent.clauses )
      { merge_tables(parent.clauses, LD->transaction.clauses);
	destroyHTable(LD->transaction.clauses);
      } else
      { parent.clauses = LD->transaction.clauses;
      }
    } else
    { transaction_discard(PASS_LD1);
      LD->transaction.generation = parent.generation;
    }
    LD->transaction.clauses = parent.clauses;
    LD->transaction.stack   = parent.parent;
    LD->transaction.id      = parent.id;
  } else
  { int tid = PL_thread_self();

    LD->transaction.gen_start  = global_generation();
    LD->transaction.gen_base   = GEN_TRANSACTION_BASE + tid*GEN_TRANSACTION_SIZE;
    LD->transaction.gen_max    = LD->transaction.gen_base+GEN_TRANSACTION_SIZE-1;
    LD->transaction.generation = LD->transaction.gen_base;
    LD->transaction.id         = goal;
    rc=callProlog(NULL, goal, PL_Q_PASS_EXCEPTION, NULL);
    LD->transaction.id         = 0;
    LD->transaction.generation = 0;
    LD->transaction.gen_max    = 0;
    LD->transaction.gen_base   = GEN_INFINITE;
    LD->transaction.gen_start  = 0;

    if ( rc && (flags&TR_TRANSACTION) )
      rc = transaction_commit(PASS_LD1);
    else
      transaction_discard(PASS_LD1);
  }

  return rc;
}

static
PRED_IMPL("transaction", 1, transaction, PL_FA_TRANSPARENT)
{ PRED_LD

  return transaction(A1, TR_TRANSACTION PASS_LD);
}

static
PRED_IMPL("snapshot", 1, snapshot, PL_FA_TRANSPARENT)
{ PRED_LD

  return transaction(A1, TR_SNAPSHOT PASS_LD);
}

static
PRED_IMPL("current_transaction", 1, current_transaction, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  tr_stack *stack;
  term_t id0, id;
  Module m0 = contextModule(environment_frame);

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { if ( !LD->transaction.id )
	return FALSE;
      id0 = LD->transaction.id;
      stack = LD->transaction.stack;
      break;
    }
    case FRG_REDO:
    { stack = CTX_PTR;
      id0 = stack->id;
      break;
    }
    default:
      return TRUE;
  }

  id = PL_new_term_ref();
  Mark(fli_context->mark);
  for(;;)
  { Module m = NULL;
    int rc;

    if ( !PL_strip_module(id0, &m, id) )
      return FALSE;
    if ( m == m0 )
      rc = PL_unify(A1, id);
    else
      rc = PL_unify(A1, id0);

    if ( rc )
    { if ( stack )
	ForeignRedoPtr(stack);
      else
	return TRUE;
    }
    Undo(fli_context->mark);

    if ( stack )
    { id0 = stack->id;
      stack = stack->parent;
    } else
      return FALSE;
  }
}


#define META PL_FA_TRANSPARENT
#define NDET PL_FA_NONDETERMINISTIC

BeginPredDefs(transaction)
  PRED_DEF("$transaction",        1, transaction,         META)
  PRED_DEF("$snapshot",           1, snapshot,            META)
  PRED_DEF("current_transaction", 1, current_transaction, NDET|META)
EndPredDefs

