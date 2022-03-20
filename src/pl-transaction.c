/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020-2022, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
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
#include "pl-event.h"
#include "pl-dbref.h"
#include "pl-tabling.h"
#include "pl-proc.h"
#include "pl-util.h"
#include "pl-pro.h"
#include "pl-wam.h"

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
{ struct tr_stack  *parent;		/* parent transaction */
  gen_t		    gen_nest;		/* Saved nesting generation */
  gen_t             generation;		/* Parent generation */
  Table		    clauses;		/* Parent changed clauses */
  Table		    predicates;		/* Parent changed predicates */
  struct tbl_trail *table_trail;	/* Parent changes to tables */
  term_t	    id;			/* Parent goal */
  unsigned int	    flags;		/* TR_* flags */
} tr_stack;

static void
tr_free_clause_symbol(void *k, void *v)
{ Clause cl = k;
  (void)v;

  release_clause(cl);
}

#define tr_clause_table(_) LDFUNC(tr_clause_table, _)
static Table
tr_clause_table(DECL_LD)
{ Table t;

  if ( !(t=LD->transaction.clauses) )
  { t = newHTable(16);
    t->free_symbol = tr_free_clause_symbol;
    LD->transaction.clauses = t;
  }

  return t;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This distinguishes three cases.

  - If we retract a globally visible clause we may need to commit and we
    need to know the generation at which we retracted the clause.

  - If the clause was added in an outer transaction we can update
    the generation and put it into our table such that we can undo
    the generation update (discard) or retract the clause (commit)

  - If we added the clause ourselves, we no longer have to commit it.
    Its visibility is guaranteed by the created and erased generations,
    so there is no need to keep it in our tables.

    (*) This is not true.  Commit/discard need to move the generations
    out of the transaction or later transactions will see this clause.
    We cannot do that now as the clause may be visible from a choice
    point in the transaction.  What to do?  I see two options:

    - Make clause CG set a flag and trigger us, so we can check the
      table and removed GCed clauses from it.
    - Scan here to see whether the clause is involved in a choicepoint.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
transaction_retract_clause(DECL_LD Clause clause)
{ if ( clause->generation.created < LD->transaction.gen_base )
  { uintptr_t lgen = ( next_generation(clause->predicate) -
		       LD->transaction.gen_base
		     );

    acquire_clause(clause);
    ATOMIC_INC(&clause->tr_erased_no);
    addHTable(tr_clause_table(), clause, (void*)lgen);

    return TRUE;
  } else if ( clause->generation.created <= LD->transaction.gen_nest )
  { gen_t egen = next_generation(clause->predicate);
    if ( !egen )
      return PL_representation_error("transaction_generations"),-1;
    clause->generation.erased = egen;
    acquire_clause(clause);
    addHTable(tr_clause_table(), clause, (void*)GEN_NESTED_RETRACT);

    return TRUE;
#if 0					/* see (*) */
  } else if ( LD->transaction.clauses )
  { deleteHTable(LD->transaction.clauses, clause);
#endif
  }
  DEBUG(MSG_TRANSACTION,
	Sdprintf("Deleting locally asserted clause for %s %s..%s\n",
		 predicateName(clause->predicate),
		 generationName(clause->generation.created),
		 generationName(clause->generation.erased)));

  return FALSE;
}

int
transaction_assert_clause(DECL_LD Clause clause, ClauseRef where)
{ uintptr_t lgen = (where == CL_START ? GEN_ASSERTA : GEN_ASSERTZ);

  acquire_clause(clause);
  addHTable(tr_clause_table(), clause, (void*)lgen);

  return TRUE;
}


int
transaction_visible_clause(DECL_LD Clause cl, gen_t gen)
{ if ( cl->generation.created <= LD->transaction.gen_start &&
       cl->generation.erased   > LD->transaction.gen_start )
  { if ( cl->tr_erased_no )
    { tr_stack s = { .clauses = LD->transaction.clauses,
                     .parent  = LD->transaction.stack
		   };
      tr_stack *stack;

      for(stack=&s; stack; stack=stack->parent)
      { intptr_t lgen;

	if ( stack->clauses &&
	     (lgen = (intptr_t)lookupHTable(stack->clauses, cl)) &&
	     !IS_ASSERT_GEN(lgen) )
	{ if ( lgen+LD->transaction.gen_base <= gen )
	    return FALSE;
	}
      }
    }

    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Notion of last modified for a predicate   when  inside a transaction. If
the transaction did not modify the predicate  this should be at most our
global start transaction. If we did modify   we define the last modified
to be the global start plus the local offset. Alternatively we could use
the real transaction number, but that cannot  be represented as a 64-bit
signed integer and thus is either  negative (upsetting simple arithmetic
on modification stamps) or is a  GMP   number.  The latter is not always
present and in any case, using GMP is relatively costly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LGEN(gen)		  ((uintptr_t)((gen)-LD->transaction.gen_base))
#define GGEN(lgen)		  ((lgen)+LD->transaction.gen_base)
#define LGEN_FLAGS_PTR(gen,flags) ((void*)((LGEN(gen)<<2)|flags))
#define PTR_LGEN(ptr)		  ((uintptr_t)(ptr)>>2)
#define PTR_GEN(ptr)		  GGEN((uintptr_t)(ptr)>>2)
#define PTR_GEN_FLAGS(ptr)	  ((uintptr_t)(ptr)&0x3)
#define PTR_ADD_FLAGS(ptr, flags) ((void*)((uintptr_t)(ptr)|(flags)))

gen_t
transaction_last_modified_predicate(DECL_LD Definition def)
{ Table table;

  if ( (table=LD->transaction.predicates) )
  { void *lgen;

    if ( (lgen = lookupHTable(table, def)) )
    { uintptr_t lmod = PTR_LGEN(lgen);
      return LD->transaction.gen_start + lmod;
    }
  }

  return def->last_modified > LD->transaction.gen_start
		? LD->transaction.gen_start
		: def->last_modified;
}

void
transaction_set_last_modified(Definition def, gen_t gen, int flags)
{ GET_LD
  void *lgen0;
  int oflags;

  if ( !LD->transaction.predicates )
    LD->transaction.predicates = newHTable(16);

  lgen0  = lookupHTable(LD->transaction.predicates, def);
  oflags = PTR_GEN_FLAGS(lgen0);

  updateHTable(LD->transaction.predicates, def,
	       LGEN_FLAGS_PTR(gen, oflags|flags));
}


#define set_modified(gen) LDFUNC(set_modified, gen)
static void
set_modified(DECL_LD gen_t gen)
{ if ( LD->transaction.predicates )
  { for_table(LD->transaction.predicates, n, v,
	      { Definition def = n;
		int flags = PTR_GEN_FLAGS(v);

		setLastModifiedPredicate(def, gen, flags);
	      });

    destroyHTable(LD->transaction.predicates);
    LD->transaction.predicates = NULL;
  }
}

#define transaction_commit(_) LDFUNC(transaction_commit, _)
static int
transaction_commit(DECL_LD)
{ if ( LD->transaction.clauses )
  { gen_t gen_commit;

    PL_LOCK(L_GENERATION);
    gen_commit = global_generation()+1;

    FOR_TABLE(LD->transaction.clauses, n, v)
    { Clause cl = n;
      uintptr_t lgen = (uintptr_t)v;

      if ( IS_ASSERT_GEN(lgen) )
      { if ( false(cl, CL_ERASED) )
	{ cl->generation.erased  = GEN_MAX;
	  MEMORY_RELEASE();
	  cl->generation.created = gen_commit;
	  DEBUG(MSG_COMMIT,
	      Sdprintf("Commit added clause %p for %s\n",
		       cl, predicateName(cl->predicate)));
	} else
	{ DEBUG(MSG_COMMIT,
		Sdprintf("Discarded in-transaction clause %p for %s\n",
			 cl, predicateName(cl->predicate)));
	  cl->generation.erased  = GEN_TR_ASSERT_ERASE;
	  MEMORY_RELEASE();
	  cl->generation.created = GEN_TR_ASSERT_ERASE;
	}
      } else if ( lgen == GEN_NESTED_RETRACT )
      { retract_clause(cl, gen_commit);
      } else
      { DEBUG(MSG_COMMIT,
	      Sdprintf("Commit erased clause %p for %s\n",
		       cl, predicateName(cl->predicate)));
	ATOMIC_DEC(&cl->tr_erased_no);
	retract_clause(cl, gen_commit);
      }
    }
    MEMORY_RELEASE();
    GD->_generation = gen_commit;
    PL_UNLOCK(L_GENERATION);

    set_modified(gen_commit);
    destroyHTable(LD->transaction.clauses);
    LD->transaction.clauses = NULL;
  }

  return TRUE;
}

#define transaction_discard(_) LDFUNC(transaction_discard, _)
static int
transaction_discard(DECL_LD)
{ int rc = TRUE;

  if ( LD->transaction.clauses )
  { for_table(LD->transaction.clauses, n, v,
	      { Clause cl = n;
		Definition def = cl->predicate;
		uintptr_t lgen = (uintptr_t)v;
		atom_t action = 0;

		if ( IS_ASSERT_GEN(lgen) )
		{ if ( false(cl, CL_ERASED) )
		  { cl->generation.erased  = GEN_TR_DISCARD_ASSERT;
		    MEMORY_RELEASE();
		    cl->generation.created = GEN_TR_DISCARD_ASSERT;
		    retract_clause(cl, cl->generation.created);
		    DEBUG(MSG_COMMIT,
			  Sdprintf("Discarded asserted clause %p for %s\n",
				   cl, predicateName(cl->predicate)));
		    action = (lgen==GEN_ASSERTA ? ATOM_asserta : ATOM_assertz);
		  } else
		  { cl->generation.erased  = GEN_TR_DISCARD_ASSERT_ERASE;
		    MEMORY_RELEASE();
		    cl->generation.created = GEN_TR_DISCARD_ASSERT_ERASE;
		    DEBUG(MSG_COMMIT,
			  Sdprintf("Discarded asserted&retracted "
				   "clause %p for %s\n",
				   cl, predicateName(cl->predicate)));
		  }
		} else if ( lgen == GEN_NESTED_RETRACT )
		{ cl->generation.erased = LD->transaction.gen_max;
		  action = ATOM_retract;
		} else
		{ ATOMIC_DEC(&cl->tr_erased_no);
		  action = ATOM_retract;
		}

		if ( def && def->events && action &&
		     !(LD->transaction.flags&TR_BULK) )
		{ if ( !predicate_update_event(def, action, cl,
					       P_EVENT_ROLLBACK) )
		    rc = FALSE;
		}
	      });
    destroyHTable(LD->transaction.clauses);
    LD->transaction.clauses = NULL;
    if ( LD->transaction.predicates )
    { destroyHTable(LD->transaction.predicates);
      LD->transaction.predicates = NULL;
    }
  }

  return rc;
}

static void
merge_clause_tables(Table into, Table from)
{ FOR_TABLE(from, n, v)
  { Clause cl = n;

    if ( addHTable(into, cl, v) == v )
      acquire_clause(cl);		/* new in outer table */
    else
      updateHTable(into, cl, v);	/* already in outer table */
  };
}

#define merge_pred_tables(into, from) LDFUNC(merge_pred_tables, into, from)
static void
merge_pred_tables(DECL_LD Table into, Table from)
{ FOR_TABLE(from, n, v)
  { Definition def = n;
    void *lgen = v;
    void *lgen0 = lookupHTable(into, def);
    int oflags = PTR_GEN_FLAGS(lgen0);


    updateHTable(into, def, PTR_ADD_FLAGS(lgen, oflags));
  };
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
is_trie_clause(const Clause cl)
{ return ( cl->predicate == GD->procedures.trie_gen_compiled3->definition ||
	   cl->predicate == GD->procedures.trie_gen_compiled2->definition );
}


#define transaction_updates(b) LDFUNC(transaction_updates, b)
static int
transaction_updates(DECL_LD Buffer b)
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
		} else if ( !is_trie_clause(cl) )
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


#define announce_updates(updates) LDFUNC(announce_updates, updates)
static int
announce_updates(DECL_LD Buffer updates)
{ tr_update *u, *e;

  u = baseBuffer(updates, tr_update);
  e = topBuffer(updates, tr_update);

  for(; u<e; u++)
  { Definition def = u->clause->predicate;

    if ( !predicate_update_event(def, nameFunctor(u->update), u->clause, 0) )
      return FALSE;
  }

  return TRUE;
}


		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

#define transaction(goal, constraint, lock, flags) LDFUNC(transaction, goal, constraint, lock, flags)
static int
transaction(DECL_LD term_t goal, term_t constraint, term_t lock, int flags)
{ int rc;
  buffer updates;

  initBuffer(&updates);

  if ( LD->transaction.generation )	/* nested transaction */
  { tr_stack parent = { .generation  = LD->transaction.generation,
			.gen_nest    = LD->transaction.gen_nest,
			.clauses     = LD->transaction.clauses,
			.predicates  = LD->transaction.predicates,
			.table_trail = LD->transaction.table_trail,
			.parent      = LD->transaction.stack,
			.id          = LD->transaction.id,
			.flags       = LD->transaction.flags
		      };

    LD->transaction.clauses     = NULL;
    LD->transaction.predicates  = NULL;
    LD->transaction.id          = goal;
    LD->transaction.stack       = &parent;
    LD->transaction.gen_nest    = LD->transaction.generation;
    LD->transaction.flags       = flags;
    LD->transaction.table_trail = NULL;
    rc = callProlog(NULL, goal, PL_Q_PASS_EXCEPTION, NULL);
    if ( rc && constraint )
      rc = callProlog(NULL, constraint, PL_Q_PASS_EXCEPTION, NULL);
    if ( rc && (flags&TR_TRANSACTION) )
    { if ( LD->transaction.table_trail )
      { if ( parent.table_trail )
	{ merge_tabling_trail(parent.table_trail, LD->transaction.table_trail);
	} else
	{ parent.table_trail = LD->transaction.table_trail;
	}
      }

      if ( LD->transaction.predicates )
      { if ( parent.predicates )
	{ merge_pred_tables(parent.predicates,
			    LD->transaction.predicates);
	  destroyHTable(LD->transaction.predicates);
	} else
	{ parent.predicates = LD->transaction.predicates;
	}
      }

      if ( LD->transaction.clauses )
      {	if ( parent.clauses )
	{ if ( (flags&TR_BULK) )
	  { transaction_updates(&updates);
	    if ( !announce_updates(&updates) )
	      goto nested_discard;
	  }
	  merge_clause_tables(parent.clauses, LD->transaction.clauses);
	  destroyHTable(LD->transaction.clauses);
	} else
	{ parent.clauses = LD->transaction.clauses;
	}
      }
    } else
    { nested_discard:
      rc = transaction_discard() && rc;
      rc = transaction_rollback_tables() && rc;
      LD->transaction.generation = parent.generation;
    }
    LD->transaction.gen_nest    = parent.gen_nest;
    LD->transaction.clauses     = parent.clauses;
    LD->transaction.predicates  = parent.predicates;
    LD->transaction.table_trail = parent.table_trail;
    LD->transaction.stack       = parent.parent;
    LD->transaction.id          = parent.id;
    LD->transaction.flags	= parent.flags;
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
    LD->transaction.gen_max    = LD->transaction.gen_base+GEN_TRANSACTION_SIZE-6;
    LD->transaction.generation = LD->transaction.gen_base;
    LD->transaction.id         = goal;
    rc = callProlog(NULL, goal, PL_Q_PASS_EXCEPTION, NULL);
    if ( rc && (flags&TR_TRANSACTION) )
    { if ( constraint )
      { TR_LOCK();
	LD->transaction.gen_start = global_generation();
	rc = callProlog(NULL, constraint, PL_Q_PASS_EXCEPTION, NULL);
      }
      if ( rc && (flags&TR_BULK) )
      { transaction_updates(&updates);
	rc = announce_updates(&updates);
      }
      if ( rc )
      { rc = ( transaction_commit_tables() &&
	       transaction_commit() );
	if ( constraint ) TR_UNLOCK();
      } else
      { if ( constraint ) TR_UNLOCK();
	LD->transaction.generation = 0;	/* avoid recording the rollback */
	transaction_discard();
	transaction_rollback_tables();
      }
    } else
    { LD->transaction.generation = 0;  /* avoid recording the rollback */
      rc = transaction_discard() && rc;
      rc = transaction_rollback_tables() && rc;
    }
    LD->transaction.id         = 0;
    LD->transaction.generation = 0;
    LD->transaction.gen_max    = 0;
    LD->transaction.gen_base   = GEN_INFINITE;
    LD->transaction.gen_start  = 0;
  }

  if ( (flags&TR_BULK) )
    discardBuffer(&updates);

  return rc;
}

static const opt_spec transaction_options[] =
{ { ATOM_bulk,		 OPT_BOOL },
  { NULL_ATOM,		 0 }
};

static
PRED_IMPL("$transaction", 2, transaction, PL_FA_TRANSPARENT)
{ PRED_LD
  int flags = TR_TRANSACTION;
  int bulk = FALSE;

  if ( !scan_options(A2, 0,
		     ATOM_transaction_option, transaction_options,
		     &bulk) )
    return FALSE;
  if ( bulk )
    flags |= TR_BULK;

  return transaction(A1, 0, 0, flags);
}

static
PRED_IMPL("$transaction", 3, transaction, PL_FA_TRANSPARENT)
{ PRED_LD

  return transaction(A1, A2, A3, TR_TRANSACTION);
}

static
PRED_IMPL("$snapshot", 1, snapshot, PL_FA_TRANSPARENT)
{ PRED_LD

  return transaction(A1, 0, 0, TR_SNAPSHOT);
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

#define add_update(cl, action, tail, head, tmp) LDFUNC(add_update, cl, action, tail, head, tmp)
static int
add_update(DECL_LD Clause cl, functor_t action,
	   term_t tail, term_t head, term_t tmp)
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
    transaction_updates((Buffer)&buf);
    u = baseBuffer(&buf, tr_update);
    e = topBuffer(&buf, tr_update);

    for(; u<e; u++)
    { if ( !add_update(u->clause, u->update, tail, head, tmp) )
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

#ifdef O_DEBUG
static
PRED_IMPL("pred_generations", 1, pred_generations, PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure proc;

  if ( get_procedure(A1, &proc, 0, GP_NAMEARITY) )
  { Definition def = getProcDefinition(proc);
    size_t count = 0;
    gen_t gen = current_generation(def);
    ClauseRef c;

    Sdprintf("Clauses for %s at %s (gen_start = %s)\n",
	     predicateName(def), generationName(gen),
	     generationName(LD->transaction.gen_start));

    count = 0;
    acquire_def(def);
    for(c = def->impl.clauses.first_clause; c; c = c->next)
    { Clause cl = c->value.clause;

      Sdprintf("  %s %p %s..%s%s\n",
	       visibleClause(cl, gen) ? "V" : "i",
	       cl,
	       generationName(cl->generation.created),
	       generationName(cl->generation.erased),
	       true(cl, CL_ERASED) ? " (erased)" : "");

      if ( visibleClause(cl, gen) )
        count++;
    }
    release_def(def);

    return TRUE;
  }

  return FALSE;
}
#endif


#define META PL_FA_TRANSPARENT
#define NDET PL_FA_NONDETERMINISTIC

BeginPredDefs(transaction)
  PRED_DEF("$transaction",        2, transaction,         META)
  PRED_DEF("$transaction",        3, transaction,         META)
  PRED_DEF("$snapshot",           1, snapshot,            META)
  PRED_DEF("current_transaction", 1, current_transaction, NDET|META)
  PRED_DEF("transaction_updates", 1, transaction_updates, 0)
#ifdef O_DEBUG
  PRED_DEF("pred_generations",    1, pred_generations,    META)
#endif
EndPredDefs

