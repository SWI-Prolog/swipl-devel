/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2017, University of Amsterdam
                              VU University Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-attvar.h"
#include "pl-wam.h"
#include "pl-gc.h"
#include "pl-write.h"
#include "pl-prims.h"
#include "pl-fli.h"
#include "pl-write.h"
#ifdef O_ATTVAR

#undef LD
#define LD LOCAL_LD

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines basic attributed variable   support  as described in
"Dynamic  attributes,  their  hProlog  implementation,    and   a  first
evaluation" by Bart  Demoen,  Report   CW350,  October  2002, Katholieke
Universiteit Leuven.

An attributed is represented as a cell   pointing  with an TAG_ATTVAR to
the linked list of attributes:


  ----------
  | refptr | <--- newer attvars <--- LD->attvar.attvars
  ----------
  | attvar | --\
  ----------   | TAG_ATTVAR|STG_GLOBAL pointer
  | att/3  | <-/
  ----------
  | name   |
  ----------
  | value  |
  ----------
  | <tail> |
  ----------

Binding the attvar places the new  value   in  <attvar>  using a trailed
assignment. The attribute list remains   accessible  through the trailed
assignment until this is GC'ed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_get_attr(DECL_LD term_t t, term_t a)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isAttVar(*p) )
  { Word ap = valPAttVar(*p);

    *valTermRef(a) = makeRefG(ap);	/* reference, so we can assign */
    succeed;
  }

  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) Although this is an assignment from var   to value, we use a trailed
assignment  to  exploit  mergeTrailedAssignments()   in  GC,  discarding
multiple  assignments  in  the  same  segment,  needed  to  ensure  that
deterministic wakeup does not leak  space.   The  test  program is this,
which must run in constant space.

	loop :- freeze(X, true), X = a, loop.

SHIFT-SAFE: Caller must ensure 6 global and 4 trail-cells
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define registerWakeup(name, value) LDFUNC(registerWakeup, name, value)
static void
registerWakeup(DECL_LD Word name, Word value)
{ Word wake;
  Word tail = valTermRef(LD->attvar.tail);

  assert(gTop+6 <= gMax && tTop+4 <= tMax);

  wake = gTop;
  gTop += 4;
  wake[0] = FUNCTOR_wakeup3;
  wake[1] = needsRef(*name) ? makeRefG(name) : *name;
  wake[2] = needsRef(*value) ? makeRefG(value) : *value;
  wake[3] = ATOM_nil;

  if ( *tail )
  { Word t;				/* Non-empty list */

    deRef2(tail, t);
    TrailAssignment(t);
    *t = consPtr(wake, TAG_COMPOUND|STG_GLOBAL);
    TrailAssignment(tail);		/* on local stack! */
    *tail = makeRefG(wake+3);
    DEBUG(MSG_WAKEUP,
	  { char buf[32];
	    Sdprintf("appended wakeup %s\n", print_addr(wake, buf));
	  });
  } else				/* empty list */
  { Word head = valTermRef(LD->attvar.head);

    assert(isVar(*head));
    TrailAssignment(head);		/* See (*) */
    *head = consPtr(wake, TAG_COMPOUND|STG_GLOBAL);
    TrailAssignment(tail);
    *tail = makeRefG(wake+3);
    LD->alerted |= ALERT_WAKEUP;
    DEBUG(MSG_WAKEUP,
	  { char buf[32];
	    Sdprintf("new wakeup %s\n", print_addr(wake, buf));
	  });
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
assignAttVar(DECL_LD Word av, Word value)
{ Word a;

  assert(isAttVar(*av));
  assert(!isRef(*value));
  assert(gTop+7 <= gMax && tTop+6 <= tMax);
  DEBUG(CHK_SECURE, assert(on_attvar_chain(av)));

  DEBUG(MSG_WAKEUP,
	{ char buf[32]; char buf2[64];
	  Sdprintf("assignAttVar(%s) at %s\n",
		   var_name_ptr(av, buf),
		   print_addr(av, buf2));
	});

  if ( isAttVar(*value) )
  { if ( value > av )
    { Word tmp = av;
      av = value;
      value = tmp;
    } else if ( av == value )
      return;
  }

  a = valPAttVar(*av);
  registerWakeup(a, value);

  TrailAssignment(av);
  if ( isAttVar(*value) )
  { DEBUG(MSG_WAKEUP, Sdprintf("  Unified two attvars\n"));
    *av = makeRefG(value);
  } else
    *av = *value;

  return;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Link known attributes variables into a reference list.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define link_attvar(_) LDFUNC(link_attvar, _)
static Word
link_attvar(DECL_LD)
{ Word gp = gTop++;

  register_attvar(gp);

  return gp+1;
}


Word
alloc_attvar(DECL_LD)
{ Word gp = allocGlobalNoShift(3);

  if ( gp )
  { register_attvar(&gp[0]);
    gp[1] = consPtr(&gp[2], TAG_ATTVAR|STG_GLOBAL);
    gp[2] = ATOM_nil;
    return &gp[1];
  }

  return NULL;
}


int
on_attvar_chain(Word avp)
{ GET_LD
  Word p, next;

  for(p = LD->attvar.attvars; p; p = next)
  { Word avp0 = p+1;
    next = isRef(*p) ? unRef(*p) : NULL;

    if ( avp0 == avp )
      return TRUE;
  }

  DEBUG(0, char buf[256];
	Sdprintf("%s: not on attvar chain\n", print_addr(avp, buf)));

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The creation of an attributed variable is trailed if call_residue_vars/2
is active. This is needed to avoid   that an attributed variable that is
destroyed on backtracking (and thus should not be reported) survives due
to a frozen stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define trail_new_attvar(p) LDFUNC(trail_new_attvar, p)
static inline void
trail_new_attvar(DECL_LD Word p)
{ if ( LD->attvar.call_residue_vars_count )
  { tTop->address = p;
    tTop++;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SHIFT-SAFE: Requires 3 global + 2 trail
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define make_new_attvar(p) LDFUNC(make_new_attvar, p)
static void
make_new_attvar(DECL_LD Word p)
{ Word gp;

  assert(gTop+3 <= gMax && tTop+1 <= tMax);

  gp = link_attvar();
  gp[1] = ATOM_nil;
  gp[0] = consPtr(&gp[1], TAG_ATTVAR|STG_GLOBAL);
  gTop += 2;

  trail_new_attvar(gp);
  Trail(p, makeRefG(gp));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SHIFT-SAFE: Requires 7 global + 2 trail
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define put_new_attvar(p, name, value) LDFUNC(put_new_attvar, p, name, value)
static void
put_new_attvar(DECL_LD Word p, atom_t name, Word value)
{ Word gp, at;

  assert(gTop+7 <= gMax && tTop+1 <= tMax);

  gp = link_attvar();
  gTop += 6;
  at = &gp[1];
  setVar(*at);
  gp[0] = consPtr(&gp[1], TAG_ATTVAR|STG_GLOBAL);

  at[1] = FUNCTOR_att3;
  at[2] = name;
  at[3] = linkValI(value);
  at[4] = ATOM_nil;
  at[0] = consPtr(&at[1], TAG_COMPOUND|STG_GLOBAL);

  trail_new_attvar(gp);
  Trail(p, makeRefG(gp));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int find_attr(Word av, atom_t name, Word *vp)

Find the location of the value for   the  attribute named `name'. Return
TRUE if found or FALSE if not found, leaving vp pointing at the ATOM_nil
of the end of the list.  Returns FALSE with *vp == NULL if the attribute
list is invalid.

Caller must ensure 4 cells space on global stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define find_attr(av, name, vp) LDFUNC(find_attr, av, name, vp)
static int
find_attr(DECL_LD Word av, atom_t name, Word *vp)
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
Word
SHIFT-SAFE: Requires max 5 global + 2 trail
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define put_att_value(vp, name, value) LDFUNC(put_att_value, vp, name, value)
static inline void
put_att_value(DECL_LD Word vp, atom_t name, Word value)
{ Word at = gTop;

  gTop += 4;
  at[0] = FUNCTOR_att3;
  at[1] = name;
  at[2] = linkValI(value);
  at[3] = ATOM_nil;

  TrailAssignment(vp);
  *vp = consPtr(at, TAG_COMPOUND|STG_GLOBAL);
}


#define put_attr(av, name, value) LDFUNC(put_attr, av, name, value)
static int
put_attr(DECL_LD Word av, atom_t name, Word value)
{ Word vp;

  assert(gTop+5 <= gMax && tTop+2 <= tMax);

  if ( find_attr(av, name, &vp) )
  { TrailAssignment(vp);
    *vp = linkValI(value);
  } else if ( vp )
  { put_att_value(vp, name, value);
  } else
    return FALSE;			/* Bad attribute list */

  return TRUE;
}


#define get_attr(l, name, value) LDFUNC(get_attr, l, name, value)
static int
get_attr(DECL_LD Word l, atom_t name, term_t value)
{ for(;;)
  { deRef(l);

    if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      if ( f->definition == FUNCTOR_att3 )
      { Word n;

	deRef2(&f->arguments[0], n);
	if ( *n == name )
	{ return unify_ptrs(valTermRef(value), &f->arguments[1],
			    ALLOW_GC|ALLOW_SHIFT);
	} else
	{ l = &f->arguments[2];
	}
      } else
	fail;
    } else
      fail;
  }
}


#define del_attr(av, name) LDFUNC(del_attr, av, name)
static int
del_attr(DECL_LD Word av, atom_t name)
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
saveWakeup(DECL_LD wakeup_state *state, int forceframe)
{ state->flags = 0;
  state->outofstack = LD->outofstack;

  if ( *valTermRef(LD->attvar.head) ||
       exception_term ||
       forceframe )
  { term_t s;
    Word h;

    if ( !(state->fid = PL_open_foreign_frame()) )
      return FALSE;			/* no space! */

    if ( exception_term )
    { state->flags |= WAKEUP_STATE_EXCEPTION;
      s = PL_new_term_ref();
      *valTermRef(s) = *valTermRef(exception_term);
      exception_term = 0;
    }

    if ( *(h=valTermRef(LD->attvar.head)) )
    { state->flags |= WAKEUP_STATE_WAKEUP;
      s = PL_new_term_refs(2);

      DEBUG(1, pl_writeln(LD->attvar.head));

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


#define restore_exception(p) LDFUNC(restore_exception, p)
static void
restore_exception(DECL_LD Word p)
{ DEBUG(1, Sdprintf("Restore exception from %p\n", p));

  *valTermRef(exception_bin) = p[0];
  exception_term = exception_bin;

  DEBUG(1, pl_writeln(exception_term));
}


#define restore_wakeup(p) LDFUNC(restore_wakeup, p)
static void
restore_wakeup(DECL_LD Word p)
{ *valTermRef(LD->attvar.head) = p[0];
  *valTermRef(LD->attvar.tail) = p[1];

  DEBUG(1, pl_writeln(LD->attvar.head));
}


void
restoreWakeup(DECL_LD wakeup_state *state)
{ LD->outofstack = state->outofstack;

  if ( state->fid )
  { if ( state->flags )
    { FliFrame fr = (FliFrame) valTermRef(state->fid);
      Word p = (Word)(fr+1);

      if ( (state->flags & WAKEUP_STATE_EXCEPTION) )
      { if ( true(state, WAKEUP_KEEP_URGENT_EXCEPTION) )
	{ if ( classify_exception_p(p) >= classify_exception(exception_term) )
	    restore_exception(p);
	} else if ( !(state->flags & WAKEUP_STATE_SKIP_EXCEPTION) )
	{ restore_exception(p);
	}
        p++;
      }
      if ( (state->flags & WAKEUP_STATE_WAKEUP) )
      { restore_wakeup(p);
      }
    }

    PL_discard_foreign_frame(state->fid);
    updateAlerted(LD);
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
  Word a1;

  a1 = valTermRef(A1);
  deRef(a1);
  if ( isAttVar(*a1) )
  { Word p = valPAttVar(*a1);
    atom_t name;

    if ( !PL_get_atom_ex(A2, &name) )
      fail;

    return get_attr(p, name, A3);
  }

  fail;
}


static
PRED_IMPL("put_attr", 3, put_attr, 0)	/* +Var, +Name, +Value */
{ PRED_LD
  Word av, vp;
  atom_t name;

  if ( !hasGlobalSpace(1) )		/* 0 means enough for attvars */
  { int rc;

    if ( (rc=ensureGlobalSpace(1, ALLOW_GC)) != TRUE )
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
  { put_new_attvar(av, name, vp);
    return TRUE;
  } else if ( isAttVar(*av) )
  { if ( put_attr(av, name, vp) )
      return TRUE;
    return PL_error("put_attr", 3, "invalid attribute structure",
		    ERR_TYPE, ATOM_attributes, A1);
  } else
  { return PL_error("put_attr", 3, NULL, ERR_UNINSTANTIATION, 1, A1);
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
  { make_new_attvar(av);			/* SHIFT: 3+0 */
    deRef(av);
  } else if ( !isAttVar(*av) )
  { return PL_error("put_attrs", 2, NULL, ERR_UNINSTANTIATION, 1, A1);
  }

  vp = valPAttVar(*av);
  TrailAssignment(vp);					/* SHIFT: 1+2 */
  *vp = linkValI(valTermRef(A2));

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
  { if ( del_attr(av, name) )			/* SHIFT: 1+2 */
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
    { put_new_attvar(v, ATOM_freeze, goal);	/* SHIFT: 6+2 */
    } else
    { Word vp;

      if ( find_attr(v, ATOM_freeze, &vp) )
      { Word gc = gTop;

	gTop += 3;
	gc[0] = FUNCTOR_dand2;
	gc[1] = linkValI(vp);
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


		 /*******************************
		 *	      WHEN		*
		 *******************************/

typedef enum
{ E_NOSPACE = -4,
  E_CYCLIC = -3,
  E_DOMAIN_ERROR = -2,
  E_INSTANTIATION_ERROR = -1,
  E_OK = 0
} when_status;

typedef struct
{ Word gSave;				/* Saved global top */
  int  depth;				/* Recursion depth */
} when_state;


#define is_or(c) LDFUNC(is_or, c)
static int
is_or(DECL_LD word c)
{ return isTerm(c) && functorTerm(c) == FUNCTOR_or1;
}


#define add_or(or, c2) LDFUNC(add_or, or, c2)
static int
add_or(DECL_LD word or, word c2)
{ Word tail = argTermP(or, 0);

  deRef(tail);
  while(isList(*tail))
  { tail = TailList(tail);
    deRef(tail);
  }
  assert(*tail == ATOM_nil);

  if ( is_or(c2) )
  { Word l = argTermP(c2, 0);
    *tail = *l;
  } else
  { Word n;

    if ( (n=allocGlobalNoShift(3)) )
    { n[0] = FUNCTOR_dot2;
      n[1] = c2;
      n[2] = ATOM_nil;
      *tail = consPtr(n, TAG_COMPOUND|STG_GLOBAL);
    } else
      return E_NOSPACE;
  }

  return E_OK;
}


#define make_disj_list(c1, c2, result) LDFUNC(make_disj_list, c1, c2, result)
static when_status
make_disj_list(DECL_LD word c1, word c2, Word result)
{ int rc;

  if ( is_or(c1) )
  { if ( (rc=add_or(c1, c2)) < 0 )
      return rc;
    *result = c1;
  } else if ( is_or(c2) )
  { if ( (rc=add_or(c2, c1)) < 0 )
      return rc;
    *result = c2;
  } else
  { Word t;

    if ( (t = allocGlobalNoShift(8)) )
    { t[0] = FUNCTOR_or1;
      t[1] = consPtr(&t[2], TAG_COMPOUND|STG_GLOBAL);
      t[2] = FUNCTOR_dot2;
      t[3] = c1;
      t[4] = consPtr(&t[5], TAG_COMPOUND|STG_GLOBAL);
      t[5] = FUNCTOR_dot2;
      t[6] = c2;
      t[7] = ATOM_nil;
      *result = consPtr(t, TAG_COMPOUND|STG_GLOBAL);
    } else
      return E_NOSPACE;
  }

  return E_OK;
}


#define when_condition(cond, result, state) LDFUNC(when_condition, cond, result, state)
static when_status
when_condition(DECL_LD Word cond, Word result, when_state *state)
{ deRef(cond);

  if ( state->depth++ == 100 )
  { int rc = PL_is_acyclic(pushWordAsTermRef(cond));

    popTermRef();
    if ( !rc )
      return E_CYCLIC;
  }

  if ( isTerm(*cond) )
  { Functor term = valueTerm(*cond);
    functor_t f = term->definition;

    if ( f == FUNCTOR_unify_determined2 ) /* ?=/2 */
    { *result = *cond;
    } else if ( f == FUNCTOR_nonvar1 )
    { Word a1;

      deRef2(&term->arguments[0], a1);
      if ( canBind(*a1) )
	*result = *cond;
      else
	*result = ATOM_true;
    } else if ( f == FUNCTOR_ground1 )
    { Word a1;

      deRef2(&term->arguments[0], a1);
      if ( ground(a1) == NULL )
	*result = ATOM_true;
      else
	*result = *cond;
    } else if ( f == FUNCTOR_comma2 )
    { word c1, c2;
      int rc;

      if ( (rc=when_condition(&term->arguments[0], &c1, state)) < 0 )
	return rc;
      if ( (rc=when_condition(&term->arguments[1], &c2, state)) < 0 )
	return rc;

      if ( c1 == ATOM_true )
      { *result = c2;
      } else if ( c2 == ATOM_true )
      { *result = c1;
      } else
      { Word t;

	if ( (t = allocGlobalNoShift(3)) )
	{ t[0] = FUNCTOR_comma2;
	  t[1] = c1;
	  t[2] = c2;

	  *result = consPtr(t, TAG_COMPOUND|STG_GLOBAL);
	} else
	  return E_NOSPACE;
      }
    } else if ( f == FUNCTOR_semicolon2 )
    { word c1, c2;
      int rc;

      if ( (rc=when_condition(&term->arguments[0], &c1, state)) < 0 )
	return rc;
      if ( c1 == ATOM_true )
      { *result = c1;
      } else
      { if ( (rc=when_condition(&term->arguments[1], &c2, state)) < 0 )
	  return rc;
	if ( c2 == ATOM_true )
	{ *result = c2;
	} else
	{ return make_disj_list(c1, c2, result);
	}
      }
    } else
      return E_DOMAIN_ERROR;

    return E_OK;
  }

  if ( isVar(*cond) )
    return E_INSTANTIATION_ERROR;

  return E_DOMAIN_ERROR;
}


/** '$eval_when_condition'(+Condition, -Simplified)
 *
 * Simplify the when/2 condition by eliminating already fullfilled
 * conditions and join disjunctions in a term or(ListOfCond).
 */

static
PRED_IMPL("$eval_when_condition", 2, eval_when_condition, 0)
{ PRED_LD
  when_state state;
  term_t cond;
  int rc;

retry:
  cond = PL_new_term_ref();
  state.gSave = gTop;
  state.depth = 0;

  if ( (rc=when_condition(valTermRef(A1), valTermRef(cond), &state)) < 0 )
  { gTop = state.gSave;
    PL_put_variable(cond);

    switch( rc )
    { case E_INSTANTIATION_ERROR:
	return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
      case E_DOMAIN_ERROR:
	return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_when_condition, A1);
      case E_CYCLIC:
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_acyclic_term, A1);
      case E_NOSPACE:
	if ( !makeMoreStackSpace(GLOBAL_OVERFLOW, ALLOW_SHIFT|ALLOW_GC) )
	  return FALSE;
        goto retry;
      default:
	assert(0);
    }
  }

  return PL_unify(A2, cond);
}


/** '$suspend'(+Var, +Attr, :Goal) is semidet.

Add Goal to an attribute with the value call(Goal).  This is the same
as:

    ==
    '$suspend'(Var, Attr, Goal) :-
	(   get_attr(Var, Attr, call(G0))
	->  put_attr(Var, Attr, call((G0,Goal)))
	;   put_attr(Var, Attr, call(Goal))
	).
    ==
*/

static
PRED_IMPL("$suspend", 3, suspend, PL_FA_TRANSPARENT)
{ PRED_LD
  atom_t name;
  Word v, g;

  if ( !hasGlobalSpace(6) )		/* 0 means enough for attvars */
  { int rc;

    if ( (rc=ensureGlobalSpace(6, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  if ( !PL_get_atom_ex(A2, &name) )
    return FALSE;

  g = valTermRef(A3);
  if ( !isTerm(*g) || functorTerm(*g) != FUNCTOR_colon2 )
  { Word t = gTop;
    term_t g2 = PL_new_term_ref();

    gTop += 3;
    t[0] = FUNCTOR_colon2;
    t[1] = contextModule(PL__ctx->engine->environment)->name;
    t[2] = linkValNoG(g);
    g = valTermRef(g2);
    *g = consPtr(t, STG_GLOBAL|TAG_COMPOUND);
  }

  v = valTermRef(A1); deRef(v);

  if ( isVar(*v) )
  { Word t = gTop;

    gTop += 3;
    t[0] = consPtr(&t[1], STG_GLOBAL|TAG_COMPOUND);
    t[1] = FUNCTOR_call1,
    t[2] = linkValI(g);
    put_new_attvar(v, name, t);
    return TRUE;
  } else if ( isAttVar(*v) )
  { Word vp;

    if ( find_attr(v, name, &vp) )
    { Word g0;

      deRef2(vp, g0);
      if ( isTerm(*g0) && functorTerm(*g0) == FUNCTOR_call1 )
      { Word t = gTop;
	Word ap = argTermP(*g0,0);

	gTop += 3;
	t[0] = FUNCTOR_comma2;
	t[1] = linkValI(ap);
	t[2] = linkValI(g);
	TrailAssignment(ap);
	*ap = consPtr(t, TAG_COMPOUND|STG_GLOBAL);

	return TRUE;
      }

      return FALSE;
    } else if ( vp )
    { Word t = gTop;

      gTop += 3;
      t[0] = consPtr(&t[1], STG_GLOBAL|TAG_COMPOUND);
      t[1] = FUNCTOR_call1,
      t[2] = linkValI(g);

      put_att_value(vp, name, t);
      return TRUE;
    }
  } else
    return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION, 1, A1);

  assert(0);
  return FALSE;
}



#ifdef O_CALL_RESIDUE

		 /*******************************
		 *	   CALL RESIDUE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
has_attributes_after(Word av, Choice  ch)  is   true  if  the attributed
variable av has attributes created or modified after the choicepoint ch.

We compute this by marking  all   trailed  assignments  and scanning the
attribute list for terms that are newer than the choicepoint or having a
value that is changed due to a trailed assignment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline word
get_value(Word p)
{ return (*p) & ~MARK_MASK;
}

#define deRefM(p, pv) LDFUNC(deRefM, p, pv)
static Word
deRefM(DECL_LD Word p, Word pv)
{ for(;;)
  { word w = get_value(p);

    if ( isRef(w) )
    { p = unRef(w);
    } else
    { *pv = w;
      return p;
    }
  }
}


#define has_attributes_after(av, ch) LDFUNC(has_attributes_after, av, ch)
static int
has_attributes_after(DECL_LD Word av, Choice ch)
{ Word l;
  word w;

  DEBUG(MSG_CALL_RESIDUE_VARS,
	{ char buf[64];
	  char vname[32];
	  Sdprintf("has_attributes_after(%s, %s)\n",
		   var_name_ptr(av, vname), print_addr(ch->mark.globaltop, buf));
	});

  av = deRefM(av, &w);
  assert(isAttVar(w));
  l = valPAttVar(w);

  for(;;)
  { l = deRefM(l, &w);

    if ( isNil(w) )
    { fail;
    } else if ( isTerm(w) )
    { Functor f = valueTerm(w);

      DEBUG(MSG_CALL_RESIDUE_VARS,
	    { char buf[64];
	      Sdprintf("  att/3 at %s\n", print_addr((Word)f, buf));
	    });

      if ( (Word)f >= ch->mark.globaltop )
	return TRUE;			/* created after choice */

      if ( f->definition == FUNCTOR_att3 )
      { Word pv = &f->arguments[1];	/* pointer to value */

	DEBUG(MSG_CALL_RESIDUE_VARS,
	{ char buf1[64]; char buf2[64];
	  Sdprintf("    value at %s: %s\n",
		   print_addr(pv, buf1), print_val(*pv, buf2));
	});

	if ( is_marked(pv) )
	  return TRUE;			/* modified after choice point */
	(void)deRefM(pv, &w);
	if ( isTerm(w) &&
	     (Word)valueTerm(w) >= ch->mark.globaltop )
	  return TRUE;			/* argument term after choice point */

	l = pv+1;
      } else
      { DEBUG(0, Sdprintf("Illegal attvar\n"));
	return FALSE;
      }
    } else
    { DEBUG(0, Sdprintf("Illegal attvar\n"));
      return FALSE;
    }
  }
}


#define scan_trail(ch, set) LDFUNC(scan_trail, ch, set)
static void
scan_trail(DECL_LD Choice ch, int set)
{ TrailEntry te, base;

  base = ch->mark.trailtop;

  for(te=tTop-1; te>=base; te--)
  { if ( isTrailVal(te->address) )
    { te--;
      if ( set )
      { DEBUG(MSG_CALL_RESIDUE_VARS,
	      { char buf1[64]; char buf2[64];
		word old = trailVal(te[1].address);
		Sdprintf("Mark %s (%s)\n",
			 var_name_ptr(te->address, buf1), print_val(old, buf2));
	      });
	*te->address |= MARK_MASK;
      } else
      { *te->address &= ~MARK_MASK;
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
  Word p, next, gend, list, tailp;

  if ( !PL_get_intptr_ex(A1, &off) )
    return FALSE;

retry:
  ch = (Choice)((Word)lBase+off);
  if ( !existingChoice(ch) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_choice, A1);

  if ( !LD->attvar.attvars )
    return PL_unify_nil(A2);

  list = tailp = allocGlobalNoShift(1);
  if ( !list )
    goto grow;
  setVar(*list);

  scan_trail(ch, TRUE);

  gend = gTop;
  for(p=LD->attvar.attvars; p; p=next)
  { Word pav = p+1;
    next = isRef(*p) ? unRef(*p) : NULL;

    if ( isAttVar(*pav) &&
	 has_attributes_after(pav, ch) )
    { Word p = allocGlobalNoShift(3);

      if ( p )
      { p[0] = FUNCTOR_dot2;
	p[1] = makeRefG(pav);
	setVar(p[2]);
	*tailp = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
	tailp = &p[2];
      } else
      { gTop = gend;
	scan_trail(ch, FALSE);
	goto grow;
      }
    }
  }

  scan_trail(ch, FALSE);

  if ( list == tailp )
  { gTop = gend;
    return PL_unify_nil(A2);
  } else
  { int rc;

    *tailp = ATOM_nil;
    rc = PL_unify(A2, pushWordAsTermRef(list));
    popTermRef();

    return rc;
  }

grow:
  if ( !makeMoreStackSpace(GLOBAL_OVERFLOW, ALLOW_SHIFT|ALLOW_GC) )
    return FALSE;
  goto retry;
}

static
PRED_IMPL("$call_residue_vars_start", 0, call_residue_vars_start, 0)
{ PRED_LD

  LD->attvar.call_residue_vars_count++;
  return TRUE;
}

static
PRED_IMPL("$call_residue_vars_end", 0, call_residue_vars_end, 0)
{ PRED_LD

  assert(LD->attvar.call_residue_vars_count>0);
  LD->attvar.call_residue_vars_count--;

  return TRUE;
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
  PRED_DEF("$eval_when_condition", 2, eval_when_condition, 0)
  PRED_DEF("$suspend", 3, suspend, PL_FA_TRANSPARENT)
#ifdef O_CALL_RESIDUE
  PRED_DEF("$attvars_after_choicepoint", 2, attvars_after_choicepoint, 0)
  PRED_DEF("$call_residue_vars_start", 0, call_residue_vars_start, 0)
  PRED_DEF("$call_residue_vars_end", 0, call_residue_vars_end, 0)
#endif
EndPredDefs

#endif /*O_ATTVAR*/
