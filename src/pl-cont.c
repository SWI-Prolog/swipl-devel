/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016-2021, VU University Amsterdam
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

#include "pl-cont.h"
#include "pl-comp.h"
#include "pl-dbref.h"
#include "pl-util.h"
#include "pl-gc.h"
#include "pl-wam.h"
#include "pl-pro.h"
#include "pl-fli.h"
#include "pl-modul.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Implementation of `delimited continuation'.  Implements

  * reset(:Goal, ?Ball, -Cont)
  * shift(+Ball)
  * call_continuation(+Cont)

reset/3 is implemented as:

  reset(_Goal, _Cont, _Ball) :-
	'$reset'.

Where the compiler translates '$reset' into

  I_RESET
  I_EXITRESET
  I_EXIT

## Future optimizations

  - Continuations probably appear in a relatively small number of
    places.  We can have a hash table for each program pointer
    that can act as a continuation.  The value can include the
    variable activation map.
  - As put_environment() documents, we should also find the
    active non-Prolog slots.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FRESET_NO_FRAME -1
#define FRESET_FINDALL  -2

#define findReset(fr, ball, rframe) LDFUNC(findReset, fr, ball, rframe)
static int
findReset(DECL_LD LocalFrame fr, term_t ball, term_t *rframe)
{ Definition reset3  = PROCEDURE_reset3->definition;

  for(; fr; fr = fr->parent)
  { int rc;
    term_t tref;
    Procedure proc_fl = _PL_predicate("findall_loop", 4, "$bags",
				      &GD->procedures.findall_loop4);

    if ( fr->predicate == proc_fl->definition )
      return FRESET_FINDALL;

    if ( fr->predicate != reset3 )
      continue;

    tref = consTermRef(fr);
    rc = PL_unify(consTermRef(argFrameP(fr, 1)), ball);
    fr = (LocalFrame)valTermRef(tref);

    if ( rc )
    { *rframe = consTermRef(fr);
      return TRUE;
    }
  }

  return FRESET_NO_FRAME;
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
environment as integers. This works if   the continuation is immediately
called. I the continuation is saved   (assert/1,  etc.) as, for example,
for tabling, this does not work.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FAST_FUNCTORS 256

static functor_t fast_functors[FAST_FUNCTORS] = {0};

static functor_t
env_functor(int slots)
{ int arity = slots+3;				/* Context, Clause and PC */

  if ( arity < FAST_FUNCTORS )
  { if ( likely(fast_functors[arity]) )
      return fast_functors[arity];
    fast_functors[arity] = PL_new_functor(ATOM_dcont, arity);
    return fast_functors[arity];
  }

  return PL_new_functor(ATOM_dcont, arity);
}


static int
put_environment(term_t env, LocalFrame fr, Code pc, int for_copy)
{ GET_LD
  term_t fr_ref   = consTermRef(fr);
  const Clause cl = fr->clause->value.clause;
  int i, slots    = cl->variables;
  size_t bv_bytes = sizeof_bitvector(slots);
  char tmp[128];
  char *buf;
  bit_vector *active;
  int rc = TRUE;
  Word p;
  atom_t cref;

  if ( bv_bytes <= sizeof(tmp) )
    buf = tmp;
  else
    buf = PL_malloc(bv_bytes);

  DEBUG(MSG_CONTINUE,
	Sdprintf("put_environment() for %s, clause %d@PC=%ld\n",
		 predicateName(fr->predicate),
		 clauseNo(cl, 0),
		 (long)(pc-cl->codes)));

  active = (bit_vector*)buf;
  init_bitvector(active, slots);
  mark_active_environment(active, fr, pc);

  if ( gTop+1+slots >= gMax )
  { int rc;
    term_t fr_ref = consTermRef(fr);

    if ( (rc=ensureGlobalSpace(1+slots, ALLOW_GC|ALLOW_SHIFT)) != TRUE )
      return raiseStackOverflow(rc);

    fr = (LocalFrame)valTermRef(fr_ref);
  }

  fr = (LocalFrame)valTermRef(fr_ref);
  p = gTop;

  cref = lookup_clref(cl);
  *p++ = env_functor(slots);
  if ( false(fr->predicate, P_TRANSPARENT) )
    *p++ = ATOM_nil;
  else
    *p++ = contextModule(fr)->name;
  *p++ = cref;
  *p++ = consInt(pc - cl->codes);

  for(i=0; i<cl->prolog_vars; i++, p++)
  { if ( true_bit(active, i) )
    { Word vp = argFrameP(fr, i);
      DEBUG(CHK_SECURE, checkData(vp));

      deRef(vp);
      if ( isVar(*vp) && vp > (Word)lBase )
      { setVar(*p);
	LTrail(vp);
	*vp = makeRefG(p);
      } else
      { *p = linkValI(vp);
      }
    } else
    { *p = ATOM_cont_inactive;
    }
  }
					/* Store choice points (*) */
  if ( rc )
  { for(i=cl->prolog_vars; i<cl->variables; i++)
    { if ( true_bit(active, i) )
      { if ( for_copy )
	{ if ( truePrologFlag(PLFLAG_SHIFT_CHECK) )
	  { Sdprintf("Shift: clause %d of %s: active "
		     "choice-point in slot %d.  Stack:\n",
		     clauseNo(cl, 0),
		     predicateName(fr->predicate), i);
	    PL_backtrace(10,1);
	  };

	  *p++ = 0;
	} else
	{ *p++ = consInt(argFrame(fr, i));
	}
      } else
	*p++ = 0;
    }
  }

  if ( buf != tmp )
    PL_free(buf);

  if ( rc )
  { Word tp = gTop;
    gTop = p;

    *valTermRef(env) = consPtr(tp, TAG_COMPOUND|STG_GLOBAL);
  }

  PL_unregister_atom(cref);

  return rc;
}


static functor_t
env3_predicate(Definition def)
{ if ( def == PROCEDURE_catch3->definition )
    return FUNCTOR_catch3;
  if ( def == PROCEDURE_reset3->definition )
    return FUNCTOR_reset3;
  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The continuation may be empty. This is  typically the case in debug mode
when last-call optimization is enabled. It  can also happen in optimized
mode if the last call was not optimized   away due to a choice point. As
the choice point is not part of the   continuation we do not need to add
an empty continuation. Samer  Abdallah   discovered  that  without this,
continuations are quite often empty.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
is_last_call(Code PC)
{ for( ; ; PC = stepPC(PC) )
  { code c = fetchop(PC);

  again:
    switch( c )
    { case I_EXIT:
      case I_EXITFACT:
	return TRUE;
      case C_JMP:
	PC += (int)PC[1]+2;
        c = fetchop(PC);
	goto again;
      default:
	return FALSE;
    }
  }
}


static int
put_continuation(term_t cont, LocalFrame resetfr, LocalFrame fr, Code pc,
		 int for_copy)
{ GET_LD
  term_t reset_ref = consTermRef(resetfr);
  term_t fr_ref    = consTermRef(fr);
  term_t contv;
  LocalFrame fr2;
  int depth = 0;

  resetfr = (LocalFrame)valTermRef(reset_ref);
  fr      = (LocalFrame)valTermRef(fr_ref);
  for(fr2=fr; fr2 != resetfr; fr2=fr2->parent)
    depth++;
  if ( !(contv = PL_new_term_refs(depth)) )
    return FALSE;
  resetfr = (LocalFrame)valTermRef(reset_ref);
  fr      = (LocalFrame)valTermRef(fr_ref);

  for( depth=0;
       fr != resetfr;
       pc = fr->programPointer, fr=fr->parent)
  { Clause cl = fr->clause->value.clause;

    if ( !is_last_call(pc) )
    { functor_t env_f;

      if ( onStackArea(local, cl) )
	return PL_representation_error("continuation");
      fr_ref = consTermRef(fr);

      if ( (env_f=env3_predicate(fr->predicate)) )
      { term_t av = PL_new_term_refs(2);

	fr = (LocalFrame)valTermRef(fr_ref);
	PL_put_term(av+0, consTermRef(argFrameP(fr, 1)));
	PL_put_term(av+1, consTermRef(argFrameP(fr, 2)));

	if ( PL_cons_list_v(contv, depth, contv) &&
	     PL_cons_functor(contv, FUNCTOR_call_continuation1, contv) &&
	     PL_cons_functor(contv, env_f, contv, av+0, av+1) &&
	     PL_cons_functor(contv, FUNCTOR_call1, contv) )
	  depth = 0;
	else
	  return FALSE;
      } else if ( !put_environment(contv+depth, fr, pc, for_copy) )
	return FALSE;

      resetfr = (LocalFrame)valTermRef(reset_ref);
      fr      = (LocalFrame)valTermRef(fr_ref);
      depth++;
    }
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

	'$cont'(Clause, PC, EnvArg1, ...)

     Here, Clause is the clause, PC is the program counter inside
     the clause, EnvArg1 is an array holding the frame arguments
     in the same order as the frame layout.  The atom '<inactive>'
     is used for frame slots that are not accessed by the remainder
     of the continuation.
  3. Unify Cont of the reset/2 goal with the continuation
  4. Return the program counter for contueing in the parent of the
     reset/3. Sets environment_frame to the parent of the reset frame.
     These parameters are used by I_SHIFT to continue in the reset
     frame.
*/

Code
shift(DECL_LD term_t ball, int for_copy)
{ term_t reset;
  int rc;

  if ( (rc=findReset(environment_frame, ball, &reset)) == TRUE )
  { term_t cont = PL_new_term_ref();
    LocalFrame resetfr;
    LocalFrame fr;

    DEBUG(MSG_CONTINUE, Sdprintf("Found reset/3 at %ld\n", reset));
    PL_put_nil(cont);
    resetfr = (LocalFrame)valTermRef(reset);
    if ( !put_continuation(cont, resetfr,
			   environment_frame->parent,
			   environment_frame->programPointer,
			   for_copy) )
    { DEBUG(MSG_CONTINUE, Sdprintf("Failed to collect continuation\n"));
      return FALSE;			/* resource error */
    }

    DEBUG(CHK_SECURE, PL_check_data(cont));

    resetfr = (LocalFrame)valTermRef(reset);
    if ( !PL_unify(consTermRef(argFrameP(resetfr, 2)), cont) )
    { DEBUG(MSG_CONTINUE, Sdprintf("Failed to unify continuation\n"));
      if ( !PL_exception(0) )
	PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION,
		 0, consTermRef(argFrameP(resetfr, 1)));
      return NULL;
    }
    resetfr = (LocalFrame)valTermRef(reset);

					/* Find parent to keep and trim lTop */
    for( fr = environment_frame->parent; ; fr = fr->parent )
    { if ( fr <= (LocalFrame)LD->choicepoints )
      { lTop = (LocalFrame)(LD->choicepoints+1);
	break;				/* found newer choicepoint */
      } else if ( fr == resetfr )
      { fr = fr->parent;
	lTop = (LocalFrame)argFrameP(fr, fr->clause->value.clause->variables);
        break;
      }
      assert(fr > resetfr);
    }
					/* return as from reset/3 */
    environment_frame = resetfr->parent;
    return resetfr->programPointer;
  }

  { static const char *msg[] =
    { "No matching reset/3 call",
      "Cannot catch continuation through findall/3"
    };

    return PL_error("shift", 1, msg[-1-rc],
		    ERR_EXISTENCE, ATOM_reset, ball),NULL;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a new stack frame to restart a continuation.

(*) The continuation holds a clause reference   (blob)  to the clause to
restart. This protects this clause from   being garbage collected. After
we restarted the continuation though, it may become subject to the stack
garbage collector and subsequently the blob  becomes subject to the atom
garbage collector, both destroying the clause   reference and the clause
if it was retracted.

We prevent this by pushing the blob onto the environment stack, where it
will be found by the atom  garbage   collector.  This  protects both the
reference and the clause itself as long   as the continuation depends on
it. Note that the atom is simply pushed  onto the stack where it resides
between stack frames. This is fine as  atom-gc performs a linear scan of
the environment stack. Also note that as we push this, it is above lTop.
This is fine as in  this  stage  the   blob  is  still  protected by the
continuation.

A pushVolatileAtom() is not needed as this   protection only needs to be
effective after the next GC call and GC won't run concurrently with AGC.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Code
push_continuation(DECL_LD term_t continuation, LocalFrame pfr, Code pcret)
{ LocalFrame top, fr;
  Word cont;

retry:
  cont = valTermRef(continuation);
  top  = lTop;

  deRef(cont);

  if ( isTerm(*cont) )
  { Functor f = valueTerm(*cont);
    Word ep = f->arguments;
    Word ap;
    ClauseRef cref;
    Clause cl;
    intptr_t pcoffset;
    size_t lneeded, lroom;
    int i;
    atom_t mname = *ep++;
    word blob = *ep++;

    if ( !(cref = clause_clref(blob)) ||
	 arityFunctor(f->definition) != cref->value.clause->variables + 3 )
    { PL_type_error("continuation", continuation);
      return NULL;
    }

    cl = cref->value.clause;
    pcoffset = valInt(*ep++);

    lneeded = sizeof(word) +
	      (size_t)argFrameP((LocalFrame)NULL, cl->variables);
    lroom   = roomStack(local);
    if ( unlikely(lroom < lneeded) )	/* resize the stack */
    { int rc;

      if ( (rc=growLocalSpace(roomStack(local)*2, ALLOW_SHIFT))
	   != TRUE )
      { raiseStackOverflow(rc);
	return NULL;
      }
      goto retry;
    }

    *(Word)top = blob;			/* see (*) */
    fr   = addPointer(top, sizeof(word));
    top  = addPointer(top, lneeded);

    ap = argFrameP(fr, 0);

    for(i=0; i<cl->prolog_vars; i++, ep++, ap++)
    { *ap = linkValI(ep);
    }

    for(; i<cl->variables; i++, ep++, ap++)
    { if ( isTaggedInt(*ep) )
      { intptr_t ichp = valInt(*ep);
	Choice ch, chp;

	ch = (Choice)valTermRef(ichp);
	for ( chp = LD->choicepoints; chp > ch; chp = chp->parent )
	  ;
	if ( ch == chp )
	{ *ap = ichp;

	  DEBUG(MSG_CONTINUE,
		Sdprintf("Restored choicepoint for slot %d\n", ichp));
	} else
	{ *ap = consTermRef(LD->choicepoints);
	}
      } else
      { *ap = consTermRef(LD->choicepoints);
      }
    }

    assert(ap == (Word)top);
    lTop = top;

    fr->programPointer = pcret;
    fr->parent         = pfr;
    setNextFrameFlags(fr, pfr);
    fr->clause         = cref;
    setFramePredicate(fr, cl->predicate);
    if ( mname == ATOM_nil )
    { fr->context      = fr->predicate->module;
    } else
    { fr->context      = lookupModule(mname);
      set(fr, FR_CONTEXT);
    }
#ifdef O_PROFILE
    fr->prof_node      = NULL;
#endif
    setGenerationFrame(fr);
    enterDefinition(fr->predicate);
    environment_frame = fr;

    DEBUG(MSG_CONTINUE,
	  Sdprintf("Resume clause %d of %s at PC=%ld\n",
		   clauseNo(cl, 0),
		   predicateName(fr->predicate),
		   pcoffset));

    return cl->codes + pcoffset;
  } else
  { PL_type_error("continuation", continuation);
    return NULL;
  }
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(cont)
EndPredDefs

void
cleanupCont(void)
{ memset(fast_functors, 0, sizeof(fast_functors));
}
