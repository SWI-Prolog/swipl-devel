/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, University of Amsterdam
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

#include "pl-coverage.h"
#if O_COVERAGE
#include "pl-proc.h"
#include "pl-dbref.h"
#include "pl-comp.h"
#include "pl-wam.h"
#include "os/pl-table.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Record coverage data by intercepting the tracer.   We  have two types of
coverage:

  - A program counter (PC)
    This is used to record calls at this PC as well as exits to
    this PC.  Not that the called goal is _before_ this PC.
  - A clause pointer ClauseRef
    This is used to record enter and exit from a clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum
{ COV_ANY = 0,
  COV_CLAUSE,
  COV_PC
} cov_obj_type;

typedef struct cov_obj
{ cov_obj_type	type;
  atom_t        cref;
  Code		pc;
  int64_t	enter;
  int64_t	exits;
} cov_obj;


static int
running_static_predicate(LocalFrame fr)
{ Definition def = fr->predicate;

  return false(def, P_DYNAMIC|P_FOREIGN);
}

static cov_obj *
call_site(LocalFrame fr)
{ LocalFrame parent = fr->parent;

  if ( running_static_predicate(parent) )
  { void *key = (void*)fr->programPointer;
    cov_obj *cov = lookupHTable(LD->coverage.table, key);

    if ( !cov )
    { cov = PL_malloc(sizeof(*cov));
      memset(cov, 0, sizeof(*cov));
      cov->type = COV_PC;
      cov->cref = lookup_clref(parent->clause->value.clause);
      cov->pc   = fr->programPointer;

      cov_obj *cov2 = addHTable(LD->coverage.table, key, cov);
      if ( cov2 != cov )
      { PL_unregister_atom(cov->cref);
	PL_free(cov);
	cov = cov2;
      }
    }

    return cov;
  }

  return NULL;
}

static cov_obj *
clause_site(ClauseRef cref)
{ void    *key = cref->value.clause;
  cov_obj *cov = lookupHTable(LD->coverage.table, key);

  if ( !cov )
  { cov = PL_malloc(sizeof(*cov));
    memset(cov, 0, sizeof(*cov));
    cov->type = COV_CLAUSE;
    cov->cref = lookup_clref(cref->value.clause);

    cov_obj *cov2 = addHTable(LD->coverage.table, key, cov);
    if ( cov2 != cov )
    { PL_unregister_atom(cov->cref);
      PL_free(cov);
      cov = cov2;
    }
  }

  return cov;
}


static void
bump(cov_obj *cov, int port)
{ if ( cov )
  { switch(port)
    { case CALL_PORT:
	ATOMIC_INC(&cov->enter);
        break;
      case EXIT_PORT:
	ATOMIC_INC(&cov->exits);
        break;
      default:
	assert(0);
    }
  }
}


void
record_coverage(LocalFrame fr, int port)
{ switch(port)
  { case CALL_PORT:
      bump(call_site(fr), port);
      break;
    case UNIFY_PORT:
      if ( running_static_predicate(fr) )
	bump(clause_site(fr->clause), CALL_PORT);
      break;
    case EXIT_PORT:
      if ( running_static_predicate(fr) )
	bump(clause_site(fr->clause), port);
      bump(call_site(fr), port);
      break;
    default:
      ;
  }
}


		 /*******************************
		 *	   PROLOG BINDING	*
		 *******************************/

static int
unify_cov(term_t t, const cov_obj *cov)
{ switch( cov->type )
  { case COV_PC:
    { ClauseRef cref = clause_clref(cov->cref);
      Clause    cl   = cref->value.clause;

      if ( cref )
      { term_t  pi = 0;
	int64_t pc = cov->pc - cl->codes;

	if ( (pi=PL_new_term_ref()) &&
	     unify_definition(MODULE_system, pi, cl->predicate,
			      0, GP_NAMEARITY) &&
	     PL_unify_term(t, PL_FUNCTOR, FUNCTOR_call_site3,
		                PL_ATOM,  cov->cref,
		                PL_INT64, pc,
		                PL_TERM,  pi) )
	{ PL_reset_term_refs(pi);
	  return TRUE;
	}
      }
      return FALSE;
    }
    case COV_CLAUSE:
      return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_clause1,
			        PL_ATOM, cov->cref);
    default:
      assert(0);
      return FALSE;
  }
}


/** '$cov_data'(?Obj, -Entered, -Exited) is nondet.
 *
 * Read  coverage  information.  Obj  is  one  of  clause(ClauseRef)  or
 * call_site(ClauseRef, PC, PI). Partial information in   Obj is used to
 * speedup the process. Deterministic and  fast   lookup  is provided if
 * ClauseRef and (for call_site/3) PC are specified.
 *
 * Either or both Entered and Exited are non-zero.  Note that Exited may
 * be larger than Entered due to non-determinism.
 */

typedef struct cov_data_state
{ cov_obj_type	type;
  TableEnum	e;
  atom_t	cref;
  int		saved;
} cov_data_state;

static void
free_state(cov_data_state *state)
{ if ( state->e )
    freeTableEnum(state->e);
  if ( state->cref )
    PL_unregister_atom(state->cref);
  if ( state->saved )
    freeForeignState(state, sizeof(*state));
}

static cov_data_state *
save_state(cov_data_state *state)
{ if ( state->saved )
    return state;

  cov_data_state *s = allocForeignState(sizeof(*s));
  *s = *state;
  s->saved = TRUE;
  return s;
}

static int
is_candidate(cov_data_state *state, cov_obj *cov)
{ if ( state->type != COV_ANY && cov->type != state->type )
    return FALSE;
  if ( state->cref && cov->cref != state->cref )
    return FALSE;
  if ( cov->enter == 0 && cov->exits == 0 )
    return FALSE;

  return TRUE;
}


static
PRED_IMPL("$cov_data", 3, cov_data, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  cov_data_state state_buf = {0};
  cov_data_state *state = &state_buf;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { Table cov_table;

      if ( !(cov_table = LD->coverage.table) )
	return FALSE;

      if ( PL_is_variable(A1) )
      {	state->e = newTableEnum(cov_table);
	break;
      } else if ( PL_is_functor(A1, FUNCTOR_call_site3) )
      {	term_t a = PL_new_term_ref();
	Clause cl = 0;
	int64_t pc_offset = -1;

	_PL_get_arg(1, A1, a);
	if ( !PL_is_variable(a) && !PL_get_clref(a, &cl) )
	  return FALSE;
	_PL_get_arg(2, A1, a);
	if ( !PL_is_variable(a) && !PL_get_int64_ex(a, &pc_offset) )
	  return FALSE;

	if ( cl && pc_offset != -1 )
	{ Code pc = cl->codes+pc_offset;
	  cov_obj *cov = lookupHTable(LD->coverage.table, pc);

	  return ( cov &&
		   unify_cov(A1, cov) &&
		   PL_unify_int64(A2, cov->enter) &&
		   PL_unify_int64(A3, cov->exits) );
	}

	state->e = newTableEnum(cov_table);
	if ( cl )
	  state->cref = lookup_clref(cl);
	state->type = COV_PC;
	break;
      } else if ( PL_is_functor(A1, FUNCTOR_clause1) )
      { if ( PL_is_ground(A1) )
	{ term_t a = PL_new_term_ref();
	  Clause cl;

	  if ( PL_get_arg(1, A1, a) &&
	       PL_get_clref(a, &cl) )
	  { cov_obj *cov = lookupHTable(cov_table, cl);

	    return ( cov &&
		     PL_unify_int64(A2, cov->enter) &&
		     PL_unify_int64(A3, cov->exits) );
	  } else
	    return FALSE;
	} else
	{ state->e = newTableEnum(cov_table);
	  state->type = COV_CLAUSE;
	  break;
	}
      }
    }
    case FRG_REDO:
      state = CTX_PTR;
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      free_state(state);
      return TRUE;
    default:
      assert(0);
  }

  void *value;
  fid_t fid = PL_open_foreign_frame();

  while( advanceTableEnum(state->e, NULL, &value ) )
  { cov_obj *cov = value;

    if ( is_candidate(state, cov) &&
	 unify_cov(A1, cov) &&
	 PL_unify_int64(A2, cov->enter) &&
	 PL_unify_int64(A3, cov->exits) )
    { PL_close_foreign_frame(fid);
      ForeignRedoPtr(save_state(state));
    } else
    { if ( PL_exception(0) )
      { break;
      } else
      { PL_rewind_foreign_frame(fid);
      }
    }
  }

  PL_close_foreign_frame(fid);
  free_state(state);
  return FALSE;
}

int
free_coverage_data(PL_local_data_t *ld)
{ WITH_LD(ld)
  { if ( !debugstatus.debugging )
      setPrologRunMode(RUN_MODE_NORMAL);
    LD->coverage.active = 0;
    updateAlerted(LD);

    Table t = LD->coverage.table;
    if ( t && COMPARE_AND_SWAP_PTR(&LD->coverage.table, t, NULL) )
      destroyHTable(t);
  }

  return TRUE;
}


/** '$cov_reset' is det.
 *
 * Reclaims collected coverage data. Coverage data collection may not be
 * active.
 */

static
PRED_IMPL("$cov_reset", 0, cov_reset, 0)
{ PRED_LD

  if ( LD->coverage.active )
  { term_t ex = PL_new_term_ref();

    return ( PL_unify_thread_id(ex, PL_thread_self()) &&
	     PL_permission_error("clean_coverage_data", "thread", ex) );
  }

  return free_coverage_data(LD);
}

/** '$cov_start'
 *
 * Start collecting coverage information.  If coverage data is already
 * being collected this increments the _active_ count.
 */

static void
free_cov_symbol(void *name, void *value)
{ cov_obj *cov = value;

  PL_unregister_atom(cov->cref);
  PL_free(cov);
}


static
PRED_IMPL("$cov_start", 0, cov_start, 0)
{ PRED_LD

  if ( !LD->coverage.table )
  { Table t = newHTable(1024);

    t->free_symbol = free_cov_symbol;
    LD->coverage.table = t;
  }

  clearPrologRunMode(RUN_MODE_NORMAL);
  LD->coverage.active++;
  updateAlerted(LD);

  return TRUE;
}


/** '$cov_stop' is semidet.
 *
 * Stop collecting coverage data.  Must  be   called  as  many  times as
 * '$cov_start' has been called to actually disable collecting data.
 *
 * Fails silently if no data is being recorded.
 */

static
PRED_IMPL("$cov_stop", 0, cov_stop, 0)
{ PRED_LD

  if ( !LD->coverage.active )
    return FALSE;

  if ( --LD->coverage.active == 0 )
  { if ( !debugstatus.debugging )
      setPrologRunMode(RUN_MODE_NORMAL);
    LD->coverage.active = FALSE;
    updateAlerted(LD);
  }

  return TRUE;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(coverage)
  PRED_DEF("$cov_data",	 3, cov_data,  PL_FA_NONDETERMINISTIC)
  PRED_DEF("$cov_reset", 0, cov_reset, 0)
  PRED_DEF("$cov_start", 0, cov_start, 0)
  PRED_DEF("$cov_stop",	 0, cov_stop,  0)
EndPredDefs

#endif /*O_COVERAGE*/
