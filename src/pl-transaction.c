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
#include "pl-dbref.h"

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
    the value is GEN_ASSERTA or GEN_ASSERTZ or the local offset of the
    deleted generation (gen-LD->transaction.gen_base). - If the clause
    was retracted, clause.tr_erased_no is incremented.

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
#define GEN_ASSERTA		((uintptr_t)(void*)-3)
#define GEN_ASSERTZ		((uintptr_t)(void*)-4)
#define GEN_NESTED_RETRACT	((uintptr_t)(void*)-5)

#define IS_ASSERT_GEN(g) ((g)==GEN_ASSERTA||(g)==GEN_ASSERTZ)

typedef struct tr_stack
{ struct tr_stack *parent;		/* parent transaction */
  gen_t		   gen_nest;		/* Saved nesting generation */
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

    return TRUE;
  } else if ( clause->generation.created <= LD->transaction.gen_nest )
  { gen_t egen = next_generation(clause->predicate PASS_LD);
    if ( !egen )
      return PL_representation_error("transaction_generations"),-1;
    clause->generation.erased = egen;
    acquire_clause(clause);
    addHTable(tr_clause_table(PASS_LD1), clause, (void*)GEN_NESTED_RETRACT);

    return TRUE;
  } else if ( LD->transaction.clauses )
  { deleteHTable(LD->transaction.clauses, clause);
  }
  DEBUG(MSG_TRANSACTION,
	Sdprintf("Deleting locally asserted clause for %s %s..%s\n",
		 predicateName(clause->predicate),
		 generationName(clause->generation.created),
		 generationName(clause->generation.erased)));

  return FALSE;
}

int
transaction_assert_clause(Clause clause, ClauseRef where ARG_LD)
{ uintptr_t lgen = (where == CL_START ? GEN_ASSERTA : GEN_ASSERTZ);

  acquire_clause(clause);
  addHTable(tr_clause_table(PASS_LD1), clause, (void*)lgen);

  return TRUE;
}


int
transaction_visible_clause(Clause cl, gen_t gen ARG_LD)
{ if ( cl->generation.created <= LD->transaction.gen_start &&
       cl->generation.erased   > LD->transaction.gen_start )
  { intptr_t lgen;

    if ( cl->tr_erased_no && LD->transaction.clauses &&
	 (lgen = (intptr_t)lookupHTable(LD->transaction.clauses, cl)) &&
	 !IS_ASSERT_GEN(lgen) )
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
		uintptr_t lgen = (uintptr_t)v;

		if ( IS_ASSERT_GEN(lgen) )
		{ if ( false(cl, CL_ERASED) )
		  { cl->generation.created = gen_commit;
		    cl->generation.erased  = GEN_MAX;
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
		} else if ( lgen == GEN_NESTED_RETRACT )
		{ retract_clause(cl, gen_commit PASS_LD);
		} else
		{ DEBUG(MSG_COMMIT,
			Sdprintf("Commit erased clause for %s\n",
				 predicateName(cl->predicate)));
		  ATOMIC_DEC(&cl->tr_erased_no);
		  retract_clause(cl, gen_commit PASS_LD);
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
		uintptr_t lgen = (uintptr_t)v;

		if ( IS_ASSERT_GEN(lgen) )
		{ if ( false(cl, CL_ERASED) )
		  { retract_clause(cl, GD->_generation-1 PASS_LD);
		  } else
		  { cl->generation.created = 0;
		    cl->generation.erased  = 0;
		  }
		} else if ( lgen == GEN_NESTED_RETRACT )
		{ cl->generation.erased = max_generation(PASS_LD1);
		} else
		{ ATOMIC_DEC(&cl->tr_erased_no);
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
		 *	      UPDATES		*
		 *******************************/

typedef struct tr_update
{ Clause    clause;
  functor_t update;
} tr_update;

static gen_t
update_gen(const tr_update *u)
{ return u->update == FUNCTOR_erased1 ?
		u->clause->generation.erased :
		u->clause->generation.created;
}

static int
cmp_updates(const void *ptr1, const void *ptr2)
{ const tr_update *u1 = ptr1;
  const tr_update *u2 = ptr2;
  gen_t g1 = update_gen(u1);
  gen_t g2 = update_gen(u2);

  return g1 < g2 ? -1 : g1 > g2 ? 1 : 0;
}


static int
transaction_updates(Buffer b ARG_LD)
{ if ( LD->transaction.clauses )
  { for_table(LD->transaction.clauses, n, v,
	      { Clause cl = n;
		uintptr_t lgen = (uintptr_t)v;

		if ( IS_ASSERT_GEN(lgen) )
		{ if ( false(cl, CL_ERASED) )
		  { tr_update u;
		    u.clause = cl;
		    if ( lgen == GEN_ASSERTA )
		      u.update = FUNCTOR_asserta1;
		    else
		      u.update = FUNCTOR_assertz1;
		    addBuffer(b, u, tr_update);
		  }
		} else
		{ tr_update u;
		  u.clause = cl;
		  u.update = FUNCTOR_erased1;
		  addBuffer(b, u, tr_update);
		}
	      });
    qsort(baseBuffer(b, void),
	  entriesBuffer(b, tr_update), sizeof(tr_update),
	  cmp_updates);
  }

  return TRUE;
}


		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

#define TR_TRANSACTION		0x0001
#define TR_SNAPSHOT		0x0002

static int
transaction(term_t goal, term_t constraint, term_t lock, int flags ARG_LD)
{ int rc;

  if ( LD->transaction.generation )
  { tr_stack parent = { .generation = LD->transaction.generation,
			.gen_nest   = LD->transaction.gen_nest,
			.clauses    = LD->transaction.clauses,
			.parent     = LD->transaction.stack,
			.id         = LD->transaction.id
		      };

    LD->transaction.clauses  = NULL;
    LD->transaction.id       = goal;
    LD->transaction.stack    = &parent;
    LD->transaction.gen_nest = LD->transaction.generation;
    rc = callProlog(NULL, goal, PL_Q_PASS_EXCEPTION, NULL);
    if ( rc && constraint )
      rc = callProlog(NULL, constraint, PL_Q_PASS_EXCEPTION, NULL);
    if ( rc && (flags&TR_TRANSACTION) )
    { if ( LD->transaction.clauses )
      { if ( parent.clauses )
	{ merge_tables(parent.clauses, LD->transaction.clauses);
	  destroyHTable(LD->transaction.clauses);
	} else
	{ parent.clauses = LD->transaction.clauses;
	}
      }
    } else
    { transaction_discard(PASS_LD1);
      LD->transaction.generation = parent.generation;
    }
    LD->transaction.gen_nest = parent.gen_nest;
    LD->transaction.clauses  = parent.clauses;
    LD->transaction.stack    = parent.parent;
    LD->transaction.id       = parent.id;
  } else
  { int tid = PL_thread_self();
#ifdef O_PLMT
    pl_mutex *mutex = NULL;
    if ( lock && !get_mutex(lock, &mutex, TRUE) )
      return FALSE;
#define TR_LOCK() PL_mutex_lock(mutex)
#define TR_UNLOCK() PL_mutex_unlock(mutex)
#else
#define TR_LOCK() (void)0
#define TR_UNLOCK() (void)0
#endif

    LD->transaction.gen_start  = global_generation();
    LD->transaction.gen_base   = GEN_TRANSACTION_BASE + tid*GEN_TRANSACTION_SIZE;
    LD->transaction.gen_max    = LD->transaction.gen_base+GEN_TRANSACTION_SIZE-1;
    LD->transaction.generation = LD->transaction.gen_base;
    LD->transaction.id         = goal;
    rc = callProlog(NULL, goal, PL_Q_PASS_EXCEPTION, NULL);
    if ( rc && (flags&TR_TRANSACTION) )
    { if ( constraint )
      { TR_LOCK();
	LD->transaction.gen_start = global_generation();
	rc = callProlog(NULL, constraint, PL_Q_PASS_EXCEPTION, NULL);
      }
      if ( rc )
      { rc = transaction_commit(PASS_LD1);
	if ( constraint ) TR_UNLOCK();
      } else
      { if ( constraint ) TR_UNLOCK();
	transaction_discard(PASS_LD1);
      }
    } else
    { transaction_discard(PASS_LD1);
    }
    LD->transaction.id         = 0;
    LD->transaction.generation = 0;
    LD->transaction.gen_max    = 0;
    LD->transaction.gen_base   = GEN_INFINITE;
    LD->transaction.gen_start  = 0;
  }

  return rc;
}

static
PRED_IMPL("$transaction", 1, transaction, PL_FA_TRANSPARENT)
{ PRED_LD

  return transaction(A1, 0, 0, TR_TRANSACTION PASS_LD);
}

static
PRED_IMPL("$transaction", 3, transaction, PL_FA_TRANSPARENT)
{ PRED_LD

  return transaction(A1, A2, A3, TR_TRANSACTION PASS_LD);
}

static
PRED_IMPL("$snapshot", 1, snapshot, PL_FA_TRANSPARENT)
{ PRED_LD

  return transaction(A1, 0, 0, TR_SNAPSHOT PASS_LD);
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

static int
add_update(Clause cl, functor_t action,
	   term_t tail, term_t head, term_t tmp ARG_LD)
{ return ( PL_put_clref(tmp, cl) &&
	   PL_cons_functor(tmp, action, tmp) &&
	   PL_unify_list(tail, head, tail) &&
	   PL_unify(head, tmp)
	 );
}

static
PRED_IMPL("transaction_updates", 1, transaction_updates, 0)
{ PRED_LD

  if ( !LD->transaction.generation )
    return FALSE;			/* error? */

  if ( LD->transaction.clauses )
  { tmp_buffer buf;
    tr_update *u, *e;
    term_t tail = PL_copy_term_ref(A1);
    term_t head = PL_new_term_ref();
    term_t tmp  = PL_new_term_ref();
    int rc = TRUE;

    initBuffer(&buf);
    transaction_updates((Buffer)&buf PASS_LD);
    u = baseBuffer(&buf, tr_update);
    e = topBuffer(&buf, tr_update);

    for(; u<e; u++)
    { if ( !add_update(u->clause, u->update, tail, head, tmp PASS_LD) )
      { rc = FALSE;
	break;
      }
    }
    discardBuffer(&buf);

    return rc && PL_unify_nil(tail);
  } else
  { return PL_unify_nil(A1);
  }
}

#define META PL_FA_TRANSPARENT
#define NDET PL_FA_NONDETERMINISTIC

BeginPredDefs(transaction)
  PRED_DEF("$transaction",        1, transaction,         META)
  PRED_DEF("$transaction",        3, transaction,         META)
  PRED_DEF("$snapshot",           1, snapshot,            META)
  PRED_DEF("current_transaction", 1, current_transaction, NDET|META)
  PRED_DEF("transaction_updates", 1, transaction_updates, 0)
EndPredDefs

