/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023-2024, SWI-Prolog Solutions b.v.
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
Record coverage data. We  have two types of coverage:

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


static void
free_cov_symbol(table_key_t name, table_value_t value)
{ cov_obj *cov = val2ptr(value);
  (void)name;

  PL_unregister_atom(cov->cref);
  PL_free(cov);
}

static coverage *
newCoverageData(void)
{ coverage *cov = malloc(sizeof(*cov));

  if ( cov )
  { memset(cov, 0, sizeof(*cov));

    TablePP t = newHTablePP(1024);
    if ( t )
    { t->free_symbol = free_cov_symbol;
      cov->table      = t;
      cov->references = 1;
      cov->flags      = COV_TRACK_THREADS;
    } else
    { free(cov);
      cov = NULL;
    }
  }

  return cov;
}


coverage *
share_coverage_data(coverage *cov)
{ if ( cov )
    ATOMIC_INC(&cov->references);

  return cov;
}


static void
unshare_coverage_data(coverage *cov)
{ if ( ATOMIC_DEC(&cov->references)  == 0 )
  { destroyHTablePP(cov->table);
    free(cov);
  }
}


static int
running_static_predicate(LocalFrame fr)
{ Definition def = fr->predicate;

  return isoff(def, P_DYNAMIC|P_FOREIGN);
}

static cov_obj*
pc_call_site(coverage *cov, const Clause cl, Code pc)
{ cov_obj *obj = lookupHTablePP(cov->table, pc);

  if ( !obj )
  { obj = PL_malloc(sizeof(*obj));
    memset(obj, 0, sizeof(*obj));
    obj->type = COV_PC;
    obj->cref = lookup_clref(cl);
    obj->pc   = pc;

    cov_obj *obj2 = addHTablePP(cov->table, pc, obj);
    if ( obj2 != obj )
    { PL_unregister_atom(obj->cref);
      PL_free(obj);
      obj = obj2;
    }
  }

  return obj;
}


static cov_obj *
call_site(LocalFrame fr)
{ LocalFrame parent = fr->parent;
  coverage *cov;

  if ( (cov=LD->coverage.data) &&
       running_static_predicate(parent) )
    return pc_call_site(cov, parent->clause->value.clause, fr->programPointer);

  return NULL;
}

static cov_obj *
clause_site(coverage *cov, Clause cl)
{ if ( cov )
  { cov_obj *obj = lookupHTablePP(cov->table, cl);

    if ( !obj )
    { obj = PL_malloc(sizeof(*obj));
      memset(obj, 0, sizeof(*obj));
      obj->type = COV_CLAUSE;
      obj->cref = lookup_clref(cl);

      cov_obj *obj2 = addHTablePP(cov->table, cl, obj);
      if ( obj2 != obj )
      { PL_unregister_atom(obj->cref);
	PL_free(obj);
	obj = obj2;
      }
    }

    return obj;
  }

  return NULL;
}


static cov_obj *
clref_site(ClauseRef cref)
{ return clause_site(LD->coverage.data, cref->value.clause);
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
	bump(clref_site(fr->clause), CALL_PORT);
      break;
    case EXIT_PORT:
      if ( running_static_predicate(fr) )
	bump(clref_site(fr->clause), port);
      bump(call_site(fr), port);
      break;
    default:
      ;
  }
}


		 /*******************************
		 *	   PROLOG BINDING	*
		 *******************************/

/** '$cov_add'(+Site, +EnterCount, +ExitCount) is det.
 *
 *  Add EnterCount and ExitCount to the statistics for Site.  Site
 *  is either call_site(ClauseRef, PC) or clause(ClauseRef).
 */

static
PRED_IMPL("$cov_add", 3, cov_add, 0)
{ PRED_LD
  int64_t enter, exit;
  Clause cl;
  int64_t pc_offset = -1;
  term_t tmp = PL_new_term_ref();

  if ( !PL_get_int64_ex(A2, &enter) ||
       !PL_get_int64_ex(A3, &exit) )
    return false;

  if ( PL_is_functor(A1, FUNCTOR_call_site2) )
  { _PL_get_arg(2, A1, tmp);
    if ( !PL_get_int64_ex(tmp, &pc_offset) )
      return false;
  } else if ( !PL_is_functor(A1, FUNCTOR_clause1) )
  { return PL_domain_error("cov_data", A1);
  }

  _PL_get_arg(1, A1, tmp);
  int rc;
  if ( (rc=PL_get_clref(tmp, &cl)) != true )
  { if ( rc == -1 )
      return PL_existence_error("db_reference", tmp);
  }

  cov_obj *obj;
  coverage *cov = LD->coverage.data;

  if ( !cov )
    cov = LD->coverage.data = newCoverageData();

  if ( pc_offset != -1 )
    obj = pc_call_site(cov, cl, cl->codes+pc_offset);
  else
    obj = clause_site(cov, cl);

  if ( obj )
  { ATOMIC_ADD(&obj->enter, enter);
    ATOMIC_ADD(&obj->exits, exit);
  }

  return true;
}


static int
unify_cov(term_t t, const cov_obj *cov)
{ switch( cov->type )
  { case COV_PC:
    { ClauseRef cref = clause_clref(cov->cref);

      if ( cref )
      { Clause  cl = cref->value.clause;
	int64_t pc = cov->pc - cl->codes;

	return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_call_site2,
				  PL_ATOM,  cov->cref,
				  PL_INT64, pc);
      }
      return false;
    }
    case COV_CLAUSE:
      return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_clause1,
				PL_ATOM, cov->cref);
    default:
      assert(0);
      return false;
  }
}


/** '$cov_data'(?Obj, -Entered, -Exited) is nondet.
 *
 * Read  coverage information.   Obj  is one  of clause(ClauseRef)  or
 * call_site(ClauseRef,  PC). Partial  information in  Obj is  used to
 * speedup the process.  Deterministic and fast lookup  is provided if
 * ClauseRef and (for call_site/2) PC are specified.
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
  s->saved = true;
  return s;
}

static int
is_candidate(cov_data_state *state, cov_obj *cov)
{ if ( state->type != COV_ANY && cov->type != state->type )
    return false;
  if ( state->cref && cov->cref != state->cref )
    return false;
  if ( cov->enter == 0 && cov->exits == 0 )
    return false;

  return true;
}


static
PRED_IMPL("$cov_data", 3, cov_data, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  cov_data_state state_buf = {0};
  cov_data_state *state = &state_buf;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { TablePP cov_table;
      coverage *cov;

      if ( !(cov=LD->coverage.data) ||
	   !(cov_table=cov->table) )
	return false;

      if ( PL_is_variable(A1) )
      {	state->e = newTableEnumPP(cov_table);
	break;
      } else if ( PL_is_functor(A1, FUNCTOR_call_site2) )
      {	term_t a = PL_new_term_ref();
	Clause cl = 0;
	int64_t pc_offset = -1;

	_PL_get_arg(1, A1, a);
	if ( !PL_is_variable(a) && !PL_get_clref(a, &cl) )
	  return false;
	_PL_get_arg(2, A1, a);
	if ( !PL_is_variable(a) && !PL_get_int64_ex(a, &pc_offset) )
	  return false;

	if ( cl && pc_offset != -1 )
	{ Code pc = cl->codes+pc_offset;
	  cov_obj *cov = lookupHTablePP(cov_table, pc);

	  return ( cov &&
		   unify_cov(A1, cov) &&
		   PL_unify_int64(A2, cov->enter) &&
		   PL_unify_int64(A3, cov->exits) );
	}

	state->e = newTableEnumPP(cov_table);
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
	  { cov_obj *cov = lookupHTablePP(cov_table, cl);

	    return ( cov &&
		     PL_unify_int64(A2, cov->enter) &&
		     PL_unify_int64(A3, cov->exits) );
	  } else
	    return false;
	} else
	{ state->e = newTableEnumPP(cov_table);
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
      return true;
    default:
      assert(0);
  }

  table_value_t value;
  fid_t fid = PL_open_foreign_frame();

  while( advanceTableEnum(state->e, NULL, &value ) )
  { cov_obj *cov = val2ptr(value);

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
  return false;
}

int
free_coverage_data(PL_local_data_t *ld)
{ WITH_LD(ld)
  { if ( !debugstatus.debugging )
      setPrologRunMode(RUN_MODE_NORMAL);
    LD->coverage.active = 0;
    updateAlerted(LD);

    coverage *cov = LD->coverage.data;
    if ( cov && COMPARE_AND_SWAP_PTR(&LD->coverage.data, cov, NULL) )
      unshare_coverage_data(cov);
  }

  return true;
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

static
PRED_IMPL("$cov_start(-Nesting)", 1, cov_start, 0)
{ PRED_LD

  if ( !LD->coverage.data )
  { if ( !PL_unify_integer(A1, 1) )
      return false;
    LD->coverage.data = newCoverageData();
  } else if ( !PL_unify_integer(A1, LD->coverage.active+1) )
    return false;

  clearPrologRunMode(RUN_MODE_NORMAL);
  LD->coverage.active++;
  updateAlerted(LD);

  return true;
}


/** '$cov_stop'(+Nesting) is semidet.
 *
 * Stop collecting  coverage data.   Resets the Nesting  to Nesting-1.
 * If  this changes  the activation  and the  new nesting  is 0,  data
 * collection is stopped.
 */

static
PRED_IMPL("$cov_stop", 1, cov_stop, 0)
{ PRED_LD

  if ( !LD->coverage.active )
    return false;

  int active;
  if ( !PL_get_integer_ex(A1, &active) )
    return false;
  active--;

  if ( LD->coverage.active != active )
  { LD->coverage.active = active;

    if ( !active )
    { if ( !debugstatus.debugging )
	setPrologRunMode(RUN_MODE_NORMAL);
      updateAlerted(LD);
    }
  }

  return true;
}


static
PRED_IMPL("$cov_active", 1, cov_active, 0)
{ PRED_LD

  if ( !LD->coverage.active )
    return false;

  return PL_unify_integer(A1, LD->coverage.active);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(coverage)
  PRED_DEF("$cov_data",	  3, cov_data,   PL_FA_NONDETERMINISTIC)
  PRED_DEF("$cov_add",    3, cov_add,    0)
  PRED_DEF("$cov_reset",  0, cov_reset,  0)
  PRED_DEF("$cov_start",  1, cov_start,  0)
  PRED_DEF("$cov_stop",	  1, cov_stop,   0)
  PRED_DEF("$cov_active", 1, cov_active, 0)
EndPredDefs

#endif /*O_COVERAGE*/
