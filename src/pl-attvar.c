/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-inline.h"
#ifdef O_ATTVAR

#undef LD
#define LD LOCAL_LD

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines basic attributed variable   support  as described in
"Dynamic  attributes,  their  hProlog  implementation,    and   a  first
evaluation" by Bart  Demoen,  Report   CW350,  October  2002, Katholieke
Universiteit Leuven.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG
static char *
vName(Word adr)
{ GET_LD
  static char name[32];

  deRef(adr);

  if (adr > (Word) lBase)
    Ssprintf(name, "_L%ld", (Word)adr - (Word)lBase);
  else
    Ssprintf(name, "_G%ld", (Word)adr - (Word)gBase);

  return name;
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) Although this is an assignment from var   to value, we use a trailed
assignment  to  exploit  mergeTrailedAssignments()   in  GC,  discarding
multiple  assignments  in  the  same  segment,  needed  to  ensure  that
deterministic wakeup does not leak  space.   The  test  program is this,
which must run in constant space.

	loop :- freeze(X, true), X = a, loop.

SHIFT-SAFE: Caller must ensure 6 global and 4 trail-cells
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerWakeup(Word name, Word value ARG_LD)
{ Word wake;
  Word tail = valTermRef(LD->attvar.tail);

  assert(gTop+6 <= gMax && tTop+4 <= tMax);

  wake = gTop;
  gTop += 4;
  wake[0] = FUNCTOR_wakeup3;
  wake[1] = needsRef(*name) ? makeRef(name) : *name;
  wake[2] = needsRef(*value) ? makeRef(value) : *value;
  wake[3] = ATOM_nil;

  if ( *tail )
  { Word t;				/* Non-empty list */

    deRef2(tail, t);
    TrailAssignment(t);
    *t = consPtr(wake, TAG_COMPOUND|STG_GLOBAL);
    TrailAssignment(tail);		/* on local stack! */
    *tail = makeRef(wake+3);
    DEBUG(1, Sdprintf("appended to wakeup\n"));
  } else				/* empty list */
  { Word head = valTermRef(LD->attvar.head);

    assert(isVar(*head));
    TrailAssignment(head);		/* See (*) */
    *head = consPtr(wake, TAG_COMPOUND|STG_GLOBAL);
    TrailAssignment(tail);
    *tail = makeRef(wake+3);
    LD->alerted |= ALERT_WAKEUP;
    DEBUG(1, Sdprintf("new wakeup\n"));
  }
}


		 /*******************************
		 *	     ASSIGNMENT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
assignAttVar(Word var, Word value)		(var := value)

Assign  value  to  the  given  attributed    variable,   adding  a  term
wake(Attribute, Value, Tail) to the global variable resembling the goals
that should be awoken.

Before calling, av *must* point to   a  dereferenced attributed variable
and value to a legal value.

The predicate unifiable/3 relies on  the   trailed  pattern left by this
function. If you change this you must also adjust unifiable/3.

SHIFT-SAFE: returns TRUE, GLOBAL_OVERFLOW or TRAIL_OVERFLOW
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
assignAttVar(Word av, Word value ARG_LD)
{ Word a;

  assert(isAttVar(*av));
  assert(!isRef(*value));
  assert(gTop+7 <= gMax && tTop+6 <= tMax);

  DEBUG(1, Sdprintf("assignAttVar(%s)\n", vName(av)));

  if ( isAttVar(*value) )
  { if ( value > av )
    { Word tmp = av;
      av = value;
      value = tmp;
    } else if ( av == value )
      return;
  }

  a = valPAttVar(*av);
  registerWakeup(a, value PASS_LD);

  TrailAssignment(av);
  if ( isAttVar(*value) )
  { DEBUG(1, Sdprintf("Unifying two attvars\n"));
    *av = makeRef(value);
  } else
    *av = *value;

  return;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SHIFT-SAFE: Requires 2+0
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
make_new_attvar(Word p ARG_LD)
{ Word gp;

  assert(gTop+2 <= gMax && tTop+1 <= tMax);

  if ( p >= (Word)lBase )
  { gp = gTop;
    gp[1] = ATOM_nil;
    gp[0] = consPtr(&gp[1], TAG_ATTVAR|STG_GLOBAL);
    *p = makeRefG(gp);
    gTop += 2;
    LTrail(p);
  } else
  { gp = gTop;
    gp[0] = ATOM_nil;
    *p = consPtr(&gp[0], TAG_ATTVAR|STG_GLOBAL);
    gTop += 1;
    GTrail(p);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SHIFT-SAFE: Requires 6 global + 1 trail
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
put_new_attvar(Word p, atom_t name, Word value ARG_LD)
{ Word gp, at;

  assert(gTop+6 <= gMax && tTop+1 <= tMax);

  gp = gTop;
  if ( p >= (Word)lBase )
  { gTop += 6;
    at = &gp[1];
    setVar(*at);
    gp[0] = consPtr(&gp[1], TAG_ATTVAR|STG_GLOBAL);
    *p = makeRefG(&gp[0]);
    LTrail(p);
  } else
  { gTop += 5;
    at = &gp[0];
    setVar(*at);
    *p = consPtr(&gp[0], TAG_ATTVAR|STG_GLOBAL);
    GTrail(p);
  }

  at[1] = FUNCTOR_att3;
  at[2] = name;
  at[3] = linkVal(value);
  at[4] = ATOM_nil;
  at[0] = consPtr(&at[1], TAG_COMPOUND|STG_GLOBAL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int find_attr(Word av, atom_t name, Word *vp)

Find the location of the value for   the  attribute named `name'. Return
TRUE if found or FALSE if not found, leaving vp pointing at the ATOM_nil
of the end of the list.  Returns FALSE with *vp == NULL if the attribute
list is invalid.

Caller must ensure 4 cells space on global stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
find_attr(Word av, atom_t name, Word *vp ARG_LD)
{ Word l;

  deRef(av);
  assert(isAttVar(*av));
  l = valPAttVar(*av);

  for(;;)
  { deRef(l);

    if ( isNil(*l) )
    { *vp = l;
      fail;
    } else if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      if ( f->definition == FUNCTOR_att3 )
      { Word n;

	deRef2(&f->arguments[0], n);
	if ( *n == name )
	{ *vp = &f->arguments[1];

	  succeed;
	} else
	{ l = &f->arguments[2];
	}
      } else
      { *vp = NULL;			/* bad attribute list */
	fail;
      }
    } else
    { *vp = NULL;			/* bad attribute list */
      fail;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_attr(Word attvar, atom_t name, Word value)

Destructive assignment or adding in a list  of the form att(Name, Value,
Rest).

SHIFT-SAFE: Requires max 5 global + 2 trail
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_attr(Word av, atom_t name, Word value ARG_LD)
{ Word vp;

  assert(gTop+5 <= gMax && tTop+2 <= tMax);

  if ( find_attr(av, name, &vp PASS_LD) )
  { TrailAssignment(vp);
    *vp = linkVal(value);
  } else if ( vp )
  { Word at = gTop;

    gTop += 4;
    at[0] = FUNCTOR_att3;
    at[1] = name;
    at[2] = linkVal(value);
    at[3] = ATOM_nil;

    TrailAssignment(vp);
    *vp = consPtr(at, TAG_COMPOUND|STG_GLOBAL);
  } else
    return FALSE;			/* Bad attribute list */

  return TRUE;
}


static int
get_attr(term_t list, atom_t name, term_t value ARG_LD)
{ Word l = valTermRef(list);

  for(;;)
  { deRef(l);

    if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      if ( f->definition == FUNCTOR_att3 )
      { Word n;

	deRef2(&f->arguments[0], n);
	if ( *n == name )
	{ return unify_ptrs(valTermRef(value), &f->arguments[1],
			    ALLOW_GC|ALLOW_SHIFT PASS_LD);
	} else
	{ l = &f->arguments[2];
	}
      } else
	fail;
    } else
      fail;
  }
}


static int
del_attr(Word av, atom_t name ARG_LD)
{ Word l, prev;

  deRef(av);
  assert(isAttVar(*av));
  l = valPAttVar(*av);
  deRef(l);
  prev = l;

  for(;;)
  { if ( isNil(*l) )
    { fail;
    } else if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      if ( f->definition == FUNCTOR_att3 )
      { Word n;

	deRef2(&f->arguments[0], n);
	if ( *n == name )
	{ TrailAssignment(prev);			/* SHIFT: 1+2 */

	  *prev = f->arguments[2];
	  succeed;
	} else
	{ l = &f->arguments[2];
	  deRef(l);
	  prev = l;
	}
      } else
      { fail;
      }
    } else
    { fail;
    }
  }
}


		 /*******************************
		 *	 CONTROLLING  WAKEUP    *
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Some callbacks should *not* do call the   wakeup list as their execution
does not contribute to the truth result   of  the computation. There are
two ways out:

	- Save/restore the wakeup list
	- Make sure the wakeup list is processed (i.e. empty).

Points requiring attention are:

	- Tracer
	- portray
	- interrupt (Control-C), signals in general	(S/W)
	- event hook.					(S/W)

The ones marked (S/W) should not affect execution and therefore must use
the save/restore approach. Effectively, forcing  execution of the wakeup
list from foreign code is very  hard   as  explained  in the determinism
handling of foreignWakeup(), so we will use save/restore in all places.

The functions below provide a way to   realise the save/restore. It must
be nicely nested in the  same  way   and  using  the same constraints as
PL_open_foreign_frame/PL_close_foreign_frame.

NOTE: Now we also save pending  exceptions,   for  which  the same rules
apply. The environment has size 1 if there  is a pending exception, 2 if
a wakeup was saved and 3 if both where saved.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
saveWakeup(wakeup_state *state, int forceframe, ARG1_LD)
{ Word h;

  state->flags = 0;

  if ( *(h=valTermRef(LD->attvar.head)) ||
       exception_term ||
       forceframe )
  { term_t s;

    if ( !(state->fid = PL_open_foreign_frame()) )
      return FALSE;			/* no space! */

    if ( exception_term )
    { state->flags |= WAKEUP_STATE_EXCEPTION;
      s = PL_new_term_ref();
      *valTermRef(s) = *valTermRef(exception_term);
      exception_term = 0;
    }

    if ( *h )
    { state->flags |= WAKEUP_STATE_WAKEUP;
      s = PL_new_term_refs(2);

      DEBUG(1, pl_write(LD->attvar.head); pl_nl());

      *valTermRef(s+0) = *h;
      setVar(*h);
      h = valTermRef(LD->attvar.tail);
      *valTermRef(s+1) = *h;
      setVar(*h);
      DEBUG(1, Sdprintf("Saved wakeup to %p\n", valTermRef(s)));
    }

    return TRUE;
  } else
  { state->fid = 0;
    return TRUE;
  }
}


static void
restore_exception(Word p ARG_LD)
{ DEBUG(1, Sdprintf("Restore exception from %p\n", p));

  *valTermRef(exception_bin) = p[0];
  exception_term = exception_bin;

  DEBUG(1, pl_writeln(exception_term));
}


static void
restore_wakeup(Word p ARG_LD)
{ *valTermRef(LD->attvar.head) = p[0];
  *valTermRef(LD->attvar.tail) = p[1];

  DEBUG(1, pl_writeln(LD->attvar.head));
}


void
restoreWakeup(wakeup_state *state ARG_LD)
{ if ( state->fid )
  { if ( state->flags )
    { FliFrame fr = (FliFrame) valTermRef(state->fid);
      Word p = (Word)(fr+1);

      if ( (state->flags & WAKEUP_STATE_EXCEPTION) )
      { if ( !(state->flags & WAKEUP_STATE_SKIP_EXCEPTION) )
	  restore_exception(p PASS_LD);
        p++;
      }
      if ( (state->flags & WAKEUP_STATE_WAKEUP) )
      { restore_wakeup(p PASS_LD);
      }
    }

    PL_discard_foreign_frame(state->fid);
  }
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

static
PRED_IMPL("attvar", 1, attvar, 0)
{ PRED_LD
  term_t t = A1;
  Word p = valTermRef(t);

  deRef(p);
  if ( isAttVar(*p) )
    succeed;

  fail;
}


static
PRED_IMPL("get_attrs", 2, get_attrs, 0)
{ PRED_LD
  term_t al = PL_new_term_ref();

  if ( !PL_get_attr(A1, al) )
    fail;

  return PL_unify(al, A2);
}


static
PRED_IMPL("get_attr", 3, get_attr, 0) /* +Var, +Name, -Value */
{ PRED_LD
  term_t al = PL_new_term_ref();
  atom_t name;

  if ( !PL_get_atom_ex(A2, &name) )
    fail;
  if ( !PL_get_attr(A1, al) )
    fail;

  return get_attr(al, name, A3 PASS_LD);
}


static
PRED_IMPL("put_attr", 3, put_attr, 0)	/* +Var, +Name, +Value */
{ PRED_LD
  Word av, vp;
  atom_t name;

  if ( !hasGlobalSpace(0) )		/* 0 means enough for attvars */
  { int rc;

    if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  if ( !PL_get_atom_ex(A2, &name) )
    fail;

  vp = valTermRef(A3);
  deRef(vp);

  if ( isVar(*vp) && vp >= (Word)lBase )/* attribute values on global */
  { Word p = gTop;

    gTop += 1;
    setVar(*p);
    LTrail(vp);
    *vp = makeRefG(p);
    vp = p;
  }

  av = valTermRef(A1);
  deRef(av);

  if ( isVar(*av) )
  { put_new_attvar(av, name, vp PASS_LD);
    return TRUE;
  } else if ( isAttVar(*av) )
  { if ( put_attr(av, name, vp PASS_LD) )
      return TRUE;
    return PL_error("put_attr", 3, "invalid attribute structure",
		    ERR_TYPE, ATOM_attributes, A1);
  } else
  { return PL_error("put_attr", 3, NULL, ERR_MUST_BE_VAR, 1, A1);
  }
}


static
PRED_IMPL("put_attrs", 2, put_attrs, 0)
{ PRED_LD
  Word av, vp;

  if ( !hasGlobalSpace(0) )		/* 0 means enough for attvars */
  { int rc;

    if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  av = valTermRef(A1);
  deRef(av);

  if ( isVar(*av) )
  { make_new_attvar(av PASS_LD);			/* SHIFT: 2+0 */
    deRef(av);
  } else if ( !isAttVar(*av) )
  { return PL_error("put_attrs", 2, NULL, ERR_MUST_BE_VAR, 1, A1);
  }

  vp = valPAttVar(*av);
  TrailAssignment(vp);					/* SHIFT: 1+2 */
  *vp = linkVal(valTermRef(A2));

  return TRUE;
}


static
PRED_IMPL("del_attr", 2, del_attr2, 0)	/* +Var, +Name */
{ PRED_LD
  Word av;
  atom_t name;

  if ( !hasGlobalSpace(0) )
  { int rc;

    if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  if ( !PL_get_atom_ex(A2, &name) )
    return FALSE;

  av = valTermRef(A1);
  deRef(av);

  if ( isAttVar(*av) )
  { if ( del_attr(av, name PASS_LD) )			/* SHIFT: 1+2 */
    { Word l = valPAttVar(*av);

      deRef(l);
      if ( isNil(*l) )
      { TrailAssignment(av);				/* SHIFT: 1+2 */
	setVar(*av);
      }
    }
  }

  return TRUE;
}


static
PRED_IMPL("del_attrs", 1, del_attrs, 0)	/* +Var */
{ PRED_LD
  Word av;

  if ( !hasGlobalSpace(0) )
  { int rc;

    if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  av = valTermRef(A1);
  deRef(av);

  if ( isAttVar(*av) )
  { TrailAssignment(av);				/* SHIFT: 1+2  */
    setVar(*av);
  }

  return TRUE;
}

		 /*******************************
		 *	       FREEZE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Freeze support predicate. This predicate succeeds   if  Goal needs to be
frozen, leading to the simple implementation of freeze/2:

freeze(X, Goal) :-
	$freeze(X, Goal), !.
freeze(_, Goal) :-
	Goal.

Note that Goal is qualified because freeze is a declared meta-predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$freeze", 2, freeze, 0)
{ PRED_LD
  Word v;

  if ( !hasGlobalSpace(0) )
  { int rc;

    if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  v = valTermRef(A1);
  deRef(v);
  if ( isVar(*v) || isAttVar(*v) )
  { Word goal;

    goal = valTermRef(A2);
    deRef(goal);

    if ( isVar(*v) )
    { put_new_attvar(v, ATOM_freeze, goal PASS_LD);	/* SHIFT: 6+1 */
    } else
    { Word vp;

      if ( find_attr(v, ATOM_freeze, &vp PASS_LD) )
      { Word gc = gTop;

	gTop += 3;
	gc[0] = FUNCTOR_dand2;
	gc[1] = linkVal(vp);
	gc[2] = *goal;

	TrailAssignment(vp);				/* SHIFT: 1+2 */
	*vp = consPtr(gc, TAG_COMPOUND|STG_GLOBAL);
      } else if ( vp )					/* vp points to [] */
      { Word at = gTop;

	gTop += 4;
	at[0] = FUNCTOR_att3;
	at[1] = ATOM_freeze;
	at[2] = *goal;
	at[3] = ATOM_nil;

	assert(*vp == ATOM_nil);
	TrailAssignment(vp);				/* SHIFT: 1+2 */
	*vp = consPtr(at, TAG_COMPOUND|STG_GLOBAL);
      } else
	assert(0);					/* bad attributes */
    }

    succeed;
  }

  fail;
}

#ifdef O_CALL_RESIDUE

		 /*******************************
		 *	   CALL RESIDUE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$new_choice_point(-Chp) is det.

Unify Chp with a reference to a new choicepoint.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$get_choice_point", 1, get_choice_point, 0)
{ PRED_LD
  Choice ch;

  for(ch=LD->choicepoints; ch; ch=ch->parent)
  { if ( ch->type == CHP_CLAUSE )
    { intptr_t off = (Word)ch - (Word)lBase;

      if ( PL_unify_integer(A1, off) )
	succeed;
    }
  }

  fail;
}


static inline size_t
offset_cell(Word p)
{ word m = *p;				/* was get_value(p) */
  size_t offset;

  if ( storage(m) == STG_LOCAL )
    offset = wsizeofInd(m) + 1;
  else
    offset = 0;

  return offset;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
has_attributes_after(Word av, Choice  ch)  is   true  if  the attributed
variable av has attributes created after   the choicepoint ch. Note that
the current implementation only deals with  attributes created after the
ch or an attribute value  set  to   a  compound  term  created after the
choicepoint ch.  Notably atomic value-changes are *not* tracked.

1 ?- put_attr(X, a, 1), call_residue_vars(put_attr(X, a, 2), V).

V = []
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
has_attributes_after(Word av, Choice ch ARG_LD)
{ Word l;

  DEBUG(1, Sdprintf("has_attributes_after(%s, %p)\n",
		    vName(av), ch->mark.globaltop));

  deRef(av);
  assert(isAttVar(*av));
  l = valPAttVar(*av);

  for(;;)
  { deRef(l);

    if ( isNil(*l) )
    { fail;
    } else if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      DEBUG(1, Sdprintf("\tterm at %p\n", f));

      if ( (Word)f >= ch->mark.globaltop )
	succeed;

      if ( f->definition == FUNCTOR_att3 )
      { if ( isTerm(f->arguments[1]) &&
	     (Word)valueTerm(f->arguments[1]) >= ch->mark.globaltop )
	  succeed;

	l = &f->arguments[2];
      } else
      { DEBUG(0, Sdprintf("Illegal attvar\n"));
	fail;
      }
    } else
    { DEBUG(0, Sdprintf("Illegal attvar\n"));
      fail;
    }
  }
}


static void
scan_trail(int set)
{ GET_LD
  TrailEntry te;

  for(te=tTop-1; te>=tBase; te--)
  { if ( isTrailVal(te->address) )
    { Word p = trailValP(te->address);

      te--;
      if ( isAttVar(*p) )
      {	DEBUG(1, Sdprintf("Trailed attvar assignment at %p\n", p));
	if ( set )
	  *p |= MARK_MASK;
	else
	  *p &= ~MARK_MASK;
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'$attvars_after_choicepoint'(+Chp, -Vars) is det.

Find all attributed variables that got   new  attributes after Chp. Note
that the trailed assignment of  an   attributed  variable  creates a new
attributed variable, which is why we must scan the trail stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$attvars_after_choicepoint", 2, attvars_after_choicepoint, 0)
{ PRED_LD
  intptr_t off;
  Choice ch;
  Word gp, gend, list, tailp;

retry:
  if ( !PL_get_intptr_ex(A1, &off) )
    fail;

  ch = (Choice)((Word)lBase+off);
  list = tailp = allocGlobalNoShift(1);
  if ( !list )
    goto grow;
  setVar(*list);

  startCritical;
  scan_trail(TRUE);

  for(gp=gBase, gend = gTop; gp<gend; gp += offset_cell(gp)+1)
  { if ( isAttVar(*gp) &&
	 !is_marked(gp) &&
	 has_attributes_after(gp, ch PASS_LD) )
    { Word p = allocGlobalNoShift(3);

      if ( p )
      { p[0] = FUNCTOR_dot2;
	p[1] = makeRefG(gp);
	setVar(p[2]);
	*tailp = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
	tailp = &p[2];
      } else
      { gTop = gend;
	scan_trail(FALSE);
	if ( !endCritical )
	  return FALSE;
	goto grow;
      }
    }
  }

  scan_trail(FALSE);
  if ( !endCritical )
    return FALSE;

  if ( list == tailp )
  { gTop = gend;
    return PL_unify_nil(A2);
  } else
  { *tailp = ATOM_nil;
    return PL_unify(A2, wordToTermRef(list));
  }

grow:
  if ( !makeMoreStackSpace(GLOBAL_OVERFLOW, ALLOW_SHIFT|ALLOW_GC) )
    return FALSE;
  goto retry;
}

#endif /*O_CALL_RESIDUE*/


		 /*******************************
		 *	    REGISTRATION	*
		 *******************************/

BeginPredDefs(attvar)
  PRED_DEF("attvar",    1, attvar,    0)
  PRED_DEF("put_attr",  3, put_attr,  0)
  PRED_DEF("get_attr",  3, get_attr,  0)
  PRED_DEF("del_attr",  2, del_attr2, 0)
  PRED_DEF("del_attrs", 1, del_attrs, 0)
  PRED_DEF("get_attrs", 2, get_attrs, 0)
  PRED_DEF("put_attrs", 2, put_attrs, 0)
  PRED_DEF("$freeze",   2, freeze,    0)
#ifdef O_CALL_RESIDUE
  PRED_DEF("$get_choice_point", 1, get_choice_point, 0)
  PRED_DEF("$attvars_after_choicepoint", 2, attvars_after_choicepoint, 0)
#endif
EndPredDefs

#endif /*O_ATTVAR*/
