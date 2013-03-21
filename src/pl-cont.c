/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "pl-incl.h"
#include "pl-dbref.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Implementation of `delimited continuation'.  Implements

  * reset(:Goal, -Cont, ?Templ)
  * shift(+Ball)
  * call_continuation(+Cont)

reset/3 is simply implemented as

  reset(Goal, _, _) :-
	call(Goal).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static term_t
findReset(LocalFrame fr, term_t ball ARG_LD)
{ Definition reset3  = PROCEDURE_reset3->definition;

  for(; fr; fr = fr->parent)
  { int rc;
    term_t tref;

    if ( fr->predicate != reset3 )
      continue;

    tref = consTermRef(fr);
    rc = PL_unify(consTermRef(argFrameP(fr, 2)), ball);
    fr = (LocalFrame)valTermRef(tref);

    if ( rc )
    { return consTermRef(fr);
    }
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_environment() puts a term into env   that represents the environment
for fr when started from pc.

Note that if the environment contains a  variable on the local stack, we
must trail this. This is not needed   for  variables on the global stack
because the environment structure we create is newer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_environment(term_t env, LocalFrame fr, Code pc)
{ GET_LD
  int i, slots    = fr->clause->value.clause->prolog_vars;
  size_t bv_bytes = sizeof_bitvector(slots);
  union
  { char       bv_store[bv_bytes];
    bit_vector bv;
  } store;
  bit_vector *active = &store.bv;
  term_t argv = PL_new_term_refs(2);

  init_bitvector(active, slots);
  mark_active_environment(active, fr, pc);

  PL_put_nil(env);
  for(i=0; i<slots; i++)
  { if ( true_bit(active, i) )
    { term_t fr_ref = consTermRef(fr);
      Word p = argFrameP(fr, i);

      deRef(p);
      if ( isVar(*p) && p > (Word)lBase )
	LTrail(p);
					/* internal one is void */
      PL_put_term(argv+1, consTermRef(argFrameP(fr, i)));

      if ( !PL_put_integer(argv+0, i) ||
	   !PL_cons_functor_v(argv+0, FUNCTOR_minus2, argv) ||
	   !PL_cons_list(env, argv+0, env) )
	return FALSE;			/* resource error */

      fr = (LocalFrame)valTermRef(fr_ref);
    }
  }

  return TRUE;
}


static int
unify_continuation(term_t cont, LocalFrame resetfr, LocalFrame fr, Code pc)
{ GET_LD
  term_t frame     = PL_new_term_ref();
  term_t argv      = PL_new_term_refs(3);
  term_t reset_ref = consTermRef(resetfr);

  for( ; fr != resetfr; pc = fr->programPointer, fr=fr->parent)
  { Clause     cl = fr->clause->value.clause;
    long pcoffset = pc - cl->codes;
    term_t fr_ref = consTermRef(fr);

    if ( !PL_put_clref(argv+0, cl) ||
	 !PL_put_integer(argv+1, pcoffset) ||
	 !put_environment(argv+2, fr, pc) ||
	 !PL_cons_functor_v(frame, FUNCTOR_dcont3, argv) ||
	 !PL_cons_list(cont, frame, cont) )
      return FALSE;

    resetfr = (LocalFrame)valTermRef(reset_ref);
    fr      = (LocalFrame)valTermRef(fr_ref);
  }

  return PL_cons_functor_v(cont, FUNCTOR_call_continuation1, cont);
}


/** shift(+Ball)

Performs the following steps:

  1. Search the stack, similar to throw/1 for reset/3 and
     unify Ball.
  2. Collect the continuation as a list of terms, each
     term is of the form

	cont(Clause, PC, Env)

     Here, Clause is the clause, PC is the program counter inside
     the clause, Env is a list of Offset-Value pairs, containing
     the reachable part of the environment.
  3. Unify Cont of the reset/2 goal with the continuation
  4. Modify the saved PC of current frame to return to the exit
     of reset/0
*/

static
PRED_IMPL("shift", 1, shift, 0)
{ PRED_LD
  term_t ball = A1;
  term_t reset;

  if ( (reset=findReset(environment_frame, ball PASS_LD)) )
  { term_t cont = PL_new_term_ref();
    LocalFrame resetfr;

    DEBUG(MSG_CONTINUE, Sdprintf("Found reset/3 at %ld\n", reset));
    PL_put_nil(cont);
    resetfr = (LocalFrame)valTermRef(reset);
    if ( !unify_continuation(cont, resetfr,
			     environment_frame->parent,
			     environment_frame->programPointer) )
    { DEBUG(MSG_CONTINUE, Sdprintf("Failed to collect continuation\n"));
      return FALSE;			/* resource error */
    }

    resetfr = (LocalFrame)valTermRef(reset);
    if ( !PL_unify(consTermRef(argFrameP(resetfr, 1)), cont) )
    { DEBUG(MSG_CONTINUE, Sdprintf("Failed to unify continuation\n"));
      if ( PL_exception(0) )
	return FALSE;
      else
	return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION,
			0, consTermRef(argFrameP(resetfr, 1)));
    }
    resetfr = (LocalFrame)valTermRef(reset);

					/* return as from reset/3 */
    environment_frame->programPointer = resetfr->programPointer;
    environment_frame->parent         = resetfr->parent;
    return TRUE;
  }

  return PL_existence_error("reset/3", ball);
}


/** call_continuation(+Cont)

Reactivate a continuation. See shift for the representation of the
continuation.
*/

static
PRED_IMPL("call_continuation", 1, call_continuation, 0)
{ PRED_LD
  term_t tail = PL_copy_term_ref(A1);
  term_t head = PL_new_term_ref();
  term_t env  = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();
  LocalFrame top, fr, parent;
  Code pc;

retry:
  top    = lTop;
  fr     = environment_frame;
  parent = fr->parent;
  pc     = fr->programPointer;

  while( PL_get_list_ex(tail, head, tail) )
  { if ( PL_is_functor(head, FUNCTOR_dcont3) )
    { Clause cl;
      ClauseRef cref;
      long pcoffset;
      size_t lneeded, lroom;
      Word ap;
      int i;

      _PL_get_arg(1, head, arg);
      if ( !PL_get_clref(arg, &cl) )
	return FALSE;
      _PL_get_arg(2, head, arg);
      if ( !PL_get_long_ex(arg, &pcoffset) )
	return FALSE;
      _PL_get_arg(3, head, env);

      lneeded = SIZEOF_CREF_CLAUSE +
	        (size_t)argFrameP((LocalFrame)NULL, cl->variables);
      lroom   = (char*)lMax - (char*)top;
      if ( lroom < lneeded )		/* resize the stack */
      { int rc;

	if ( (rc = ensureLocalSpace(roomStack(local)*2, ALLOW_SHIFT)) != TRUE )
	  return raiseStackOverflow(rc);
	PL_put_term(tail, A1);
	goto retry;
      }

      cref = (ClauseRef)top;
      fr   = addPointer(cref, SIZEOF_CREF_CLAUSE);
      top  = addPointer(top, lneeded);

      for(ap = argFrameP(fr, 0), i=cl->prolog_vars; i-- > 0; )
	*ap++ = ATOM_garbage_collected;
      for(i=cl->variables-cl->prolog_vars; i-- > 0; )
	*ap++ = 0;

      while( PL_get_list_ex(env, head, env) )
      { int offset;

	if ( !PL_is_functor(head, FUNCTOR_minus2) )
	  return PL_type_error("pair", head);

	_PL_get_arg(1, head, arg);
	if ( !PL_get_integer_ex(arg, &offset) )
	  return FALSE;
	_PL_get_arg(2, head, arg);
	argFrame(fr, offset) = linkVal(valTermRef(arg));
      }
      if ( !PL_get_nil_ex(env) )
	return FALSE;

      cref->next = NULL;
      cref->key  = 0;
      cref->value.clause = cl;

      fr->programPointer = pc;
      fr->parent         = parent;
      fr->clause         = cref;
      fr->predicate      = cl->procedure->definition;
      fr->context	 = fr->predicate->module;
      fr->level          = parent->level+1;
      fr->flags          = 0;		/* TBD: anything needed? */
#ifdef O_PROFILE
      fr->prof_node      = NULL;
#endif
      setGenerationFrame(fr, GD->generation);

      pc     = cl->codes+pcoffset;
      parent = fr;
    } else
    { return PL_type_error("continuation", head);
    }
  }

  if ( PL_get_nil_ex(tail) )
  { fr = environment_frame;
    lTop = top;

    fr->parent = parent;
    fr->programPointer = pc;
    set(fr, FR_KEEPLTOP);

    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(cont)
  PRED_DEF("shift",             1, shift,             0)
  PRED_DEF("call_continuation", 1, call_continuation, 0)
EndPredDefs
