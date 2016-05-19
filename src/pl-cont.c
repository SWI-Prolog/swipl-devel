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

  * reset(:Goal, ?Ball, -Cont)
  * shift(+Ball)
  * call_continuation(+Cont)

reset/3 is implemented as

  reset(Goal, Cont, Ball) :-
	'$start_reset',
	call(Goal),
	Cont = 0, Ball = 0.

## Future optimizations

  - Continuations probably appear in a relatively small number of
    places.  We can have a hash table for each program pointer
    that can act as a continuation.  The value can include the
    variable activation map.
  - As put_environment() documents, we should also find the
    active non-Prolog slots.
  - It is probably better to represent the continuation frame
    as a compound term rather than a list.  This is generally
    more compact and quicker to process.  The abovew table can
    provide the functor with the correct arity.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$start_reset", 0, start_reset, 0)
{ PRED_LD
  LocalFrame fr = environment_frame;

  assert(fr->parent);
  set(fr->parent, FR_INRESET);

  return TRUE;
}


static term_t
findReset(LocalFrame fr, term_t ball ARG_LD)
{ Definition reset3  = PROCEDURE_reset3->definition;

  for(; fr; fr = fr->parent)
  { int rc;
    term_t tref;

    if ( fr->predicate != reset3 )
      continue;

    tref = consTermRef(fr);
    rc = PL_unify(consTermRef(argFrameP(fr, 1)), ball);
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

(*) This loop stores all  non-prolog   variables.  These are pointers to
choice points for if-then-else and related control structures. These are
stored as offsets  to  the  local  stack   base.  We  put  them  in  the
environment as integers.

TBD: We can extend mark_active_environment() to also find the non-Prolog
slots that are  active,  i.e.,  can  be   used  by  one  of  the pruning
operations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_environment(term_t env, LocalFrame fr, Code pc)
{ GET_LD
  Clause cl       = fr->clause->value.clause;
  int i, slots    = cl->prolog_vars;
  size_t bv_bytes = sizeof_bitvector(slots);
  char tmp[128];
  char *buf;
  bit_vector *active;
  term_t argv = PL_new_term_refs(2);
  int rc = TRUE;

  if ( bv_bytes <= sizeof(tmp) )
    buf = tmp;
  else
    buf = PL_malloc(bv_bytes);

  active = (bit_vector*)buf;
  init_bitvector(active, slots);

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
      { rc = FALSE;			/* resource error */
	break;
      }

      fr = (LocalFrame)valTermRef(fr_ref);
    }
  }
					/* Store choice points (*) */
  if ( rc )
  { for(i=cl->prolog_vars; i<cl->variables; i++)
    { DEBUG(MSG_CONTINUE,
	    Sdprintf("%s: add choice-point reference from slot %d\n",
		     predicateName(fr->predicate), i));
      PL_put_integer(argv+1, argFrame(fr, i));

      if ( !PL_put_integer(argv+0, i) ||
	   !PL_cons_functor_v(argv+0, FUNCTOR_minus2, argv) ||
	   !PL_cons_list(env, argv+0, env) )
      { rc = FALSE;			/* resource error */
	break;
      }
    }
  }

  if ( buf != tmp )
    PL_free(buf);

  return rc;
}


static int
unify_continuation(term_t cont, LocalFrame resetfr, LocalFrame fr, Code pc)
{ GET_LD
  term_t argv      = PL_new_term_refs(3);
  term_t reset_ref = consTermRef(resetfr);
  term_t contv;
  LocalFrame fr2;
  int depth = 0;

  for(fr2=fr; fr2 != resetfr; fr2=fr2->parent)
    depth++;
  if ( !(contv = PL_new_term_refs(depth)) )
    return FALSE;

  for( depth=0;
       fr != resetfr;
       pc = fr->programPointer, fr=fr->parent, depth++)
  { Clause     cl = fr->clause->value.clause;
    long pcoffset = pc - cl->codes;
    term_t fr_ref = consTermRef(fr);

    assert(!onStackArea(local, cl));

    if ( !PL_put_clref(argv+0, cl) ||
	 !PL_put_integer(argv+1, pcoffset) ||
	 !put_environment(argv+2, fr, pc) ||
	 !PL_cons_functor_v(contv+depth, FUNCTOR_dcont3, argv)  )
      return FALSE;

    resetfr = (LocalFrame)valTermRef(reset_ref);
    fr      = (LocalFrame)valTermRef(fr_ref);
  }

  return ( PL_cons_list_v(cont, depth, contv) &&
	   PL_cons_functor_v(cont, FUNCTOR_call_continuation1, cont)
	 );
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
    LocalFrame fr;

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
    if ( !PL_unify(consTermRef(argFrameP(resetfr, 2)), cont) )
    { DEBUG(MSG_CONTINUE, Sdprintf("Failed to unify continuation\n"));
      if ( PL_exception(0) )
	return FALSE;
      else
	return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION,
			0, consTermRef(argFrameP(resetfr, 1)));
    }
    resetfr = (LocalFrame)valTermRef(reset);

					/* Find parent to keep and trim lTop */
					/* Note that I_EXIT does not touch this */
					/* (FR_KEEPLTOP) */
    for( fr = environment_frame->parent; ; fr = fr->parent )
    { if ( fr <= (LocalFrame)LD->choicepoints )
      { lTop = (LocalFrame)(LD->choicepoints+1);
	break;				/* found newer choicepoint */
      } else if ( fr == resetfr )
      { lTop = (LocalFrame)argFrameP(fr, fr->clause->value.clause->variables);
        break;
      }
      assert(fr > resetfr);
    }
					/* return as from reset/3 */
    fr = environment_frame;
    fr->programPointer = resetfr->programPointer;
    fr->parent         = resetfr->parent;
    set(fr, FR_KEEPLTOP);

    return TRUE;
  }

  return PL_existence_error("reset/3", ball);
}


/** '$call_one_tail_body'(+Cont)

Reactivate a single tail body from a continuation. See shift for the
representation of the continuation.
*/


static
PRED_IMPL("$call_one_tail_body", 1, call_one_tail_body, 0)
{ PRED_LD
  term_t cont = A1;
  LocalFrame me, top, fr;
  Code pc;

retry:
  top    = lTop;
  me     = environment_frame;

  if ( PL_is_functor(cont, FUNCTOR_dcont3) )
  { term_t env  = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();
    term_t head = PL_new_term_ref();
    Clause cl;
    ClauseRef cref;
    long pcoffset;
    size_t lneeded, lroom;
    Word ap;
    int i;

    _PL_get_arg(1, cont, arg);
    if ( !PL_get_clref(arg, &cl) )
      return FALSE;
    _PL_get_arg(2, cont, arg);
    if ( !PL_get_long_ex(arg, &pcoffset) )
      return FALSE;
    _PL_get_arg(3, cont, env);

    lneeded = SIZEOF_CREF_CLAUSE +
	      (size_t)argFrameP((LocalFrame)NULL, cl->variables);
    lroom   = roomStack(local);
    if ( lroom < lneeded )		/* resize the stack */
    { int rc;

      if ( (rc = ensureLocalSpace(roomStack(local)*2, ALLOW_SHIFT)) != TRUE )
	return raiseStackOverflow(rc);
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

      if ( offset < cl->prolog_vars )
      { argFrame(fr, offset) = linkVal(valTermRef(arg));
      } else
      { intptr_t i;

	if ( !PL_get_intptr_ex(arg, &i) )
	  return FALSE;

	argFrame(fr, offset) = i;
      }
    }
    if ( !PL_get_nil_ex(env) )
      return FALSE;

    lTop = top;

    cref->next         = NULL;
    cref->d.key        = 0;
    cref->value.clause = cl;

    fr->programPointer = me->programPointer;
    fr->parent         = me->parent;
    fr->level          = me->level;
    fr->clause         = cref;
    fr->predicate      = cl->predicate;
    fr->context	       = fr->predicate->module;
    fr->flags          = FR_MAGIC;		/* TBD: anything else needed? */
#ifdef O_PROFILE
    fr->prof_node      = NULL;
#endif
    setGenerationFrame(fr, GD->generation);
    enterDefinition(fr->predicate);

    pc     = cl->codes+pcoffset;

    me->parent = fr;
    me->programPointer = pc;
    set(me, FR_KEEPLTOP);

    return TRUE;
  } else
  { return PL_type_error("continuation", cont);
  }
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(cont)
  PRED_DEF("$start_reset",        0, start_reset,        0)
  PRED_DEF("shift",               1, shift,              0)
  PRED_DEF("$call_one_tail_body", 1, call_one_tail_body, 0)
EndPredDefs
