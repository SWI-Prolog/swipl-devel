/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

The predicate unifyable/3 relies on  the   trailed  pattern left by this
function. If you change this you must also adjust unifyable/3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
assignAttVar(Word av, Word value ARG_LD)
{ Word wake, a;				/*  */
  Word tail = valTermRef(LD->attvar.tail);

  assert(isAttVar(*av));
  assert(!isRef(*value));

  DEBUG(1, Sdprintf("assignAttVar(%s)\n", vName(av)));

  if ( isAttVar(*value) )
  { if ( value > av )
    { Word tmp = av;
      av = value;
      value = tmp;
    } else if ( av == value )
      succeed;
  }

  a = valPAttVar(*av);
  wake    = allocGlobalNoShift(4);	/* may NOT shift the stacks!!! */
  wake[0] = FUNCTOR_wakeup3;
  wake[1] = needsRef(*a) ? makeRef(a) : *a;
  wake[2] = needsRef(*value) ? makeRef(value) : *value;
  wake[3] = ATOM_nil;

  if ( *tail )
  { Word t;				/* Non-empty list */

    deRef2(tail, t);
    TrailAssignment(t);
    *t = consPtr(wake, TAG_COMPOUND|STG_GLOBAL);
    DEBUG(1, Sdprintf("appended to wakeup\n"));
  } else				/* empty list */
  { Word head = valTermRef(LD->attvar.head);
    
    assert(isVar(*head));
    *head = consPtr(wake, TAG_COMPOUND|STG_GLOBAL);
    Trail(head);
    DEBUG(1, Sdprintf("new wakeup\n"));
  }

  TrailAssignment(tail);		/* on local stack! */
  *tail = makeRef(wake+3);

  TrailAssignment(av);
  if ( isAttVar(*value) )
  { DEBUG(1, Sdprintf("Unifying two attvars\n"));
    *av = makeRef(value);
  } else
    *av = *value;

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: caller must require 2 words on global stack
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
make_new_attvar(Word p ARG_LD)
{ Word gp;

  if ( onStackArea(local, p) )
  { gp = allocGlobal(2);
    gp[1] = ATOM_nil;
    gp[0] = consPtr(&gp[1], TAG_ATTVAR|STG_GLOBAL);
    *p = makeRef(&gp[0]);
  } else
  { gp = allocGlobal(1);
    gp[0] = ATOM_nil;
    *p = consPtr(&gp[0], TAG_ATTVAR|STG_GLOBAL);
  }

  Trail(p);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int find_attr(Word av, atom_t name, Word *vp)

Find the location of the value for   the  attribute named `name'. Return
TRUE if found or FALSE if not found, leaving vp pointing at the ATOM_nil
of the end of the list.

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
Rest). Caller must call requireStack(global, 4  * sizeof(word)) to avoid
this predicate from shifting the stacks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_attr(Word av, atom_t name, Word value ARG_LD)
{ Word vp;

  if ( find_attr(av, name, &vp PASS_LD) )
  { TrailAssignment(vp);
    *vp = linkVal(value);
  } else if ( vp )
  { Word at = allocGlobal(4);

    at[0] = FUNCTOR_att3;
    at[1] = name;
    at[2] = linkVal(value);
    at[3] = ATOM_nil;

    TrailAssignment(vp);
    *vp = consPtr(at, TAG_COMPOUND|STG_GLOBAL);
  } else
    fail;

  succeed;
}


static int
get_attr(term_t list, atom_t name, term_t value)
{ GET_LD
  Word l = valTermRef(list);

  for(;;)
  { deRef(l);
    
    if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      if ( f->definition == FUNCTOR_att3 )
      { Word n;

	deRef2(&f->arguments[0], n);
	if ( *n == name )
	{ return unify_ptrs(valTermRef(value), &f->arguments[1] PASS_LD);
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
	{ TrailAssignment(prev);

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
PRED_IMPL("get_attr", 3, get_attr3, 0) /* +Var, +Name, -Value */
{ PRED_LD
  term_t al = PL_new_term_ref();
  atom_t name;

  if ( !PL_get_atom_ex(A2, &name) )
    fail;
  if ( !PL_get_attr(A1, al) )
    fail;
  
  return get_attr(al, name, A3);
}


static
PRED_IMPL("put_attr", 3, put_attr3, 0)	/* +Var, +Name, +Value */
{ PRED_LD
  Word av;
  atom_t name;

  if ( !PL_get_atom_ex(A2, &name) )
    fail;

  requireStack(global, 4*sizeof(word));

  av = valTermRef(A1);
  deRef(av);

  if ( isVar(*av) )
  { make_new_attvar(av PASS_LD);
    return put_attr(av, name, valTermRef(A3) PASS_LD);
  } else if ( isAttVar(*av) )
  { return put_attr(av, name, valTermRef(A3) PASS_LD);
  } else
  { return PL_error("put_attr", 3, NULL, ERR_TYPE, ATOM_var, A1);
  }
}


static
PRED_IMPL("put_attrs", 2, put_attrs, 0)
{ PRED_LD
  Word av, vp;

  requireStack(global, 4*sizeof(word));
  av = valTermRef(A1);
  deRef(av);

  if ( isVar(*av) )
  { make_new_attvar(av PASS_LD);
    deRef(av);
  } else if ( !isAttVar(*av) )
  { return PL_error("put_attrs", 2, NULL, ERR_TYPE, ATOM_var, A1);
  }

  vp = valPAttVar(*av);
  TrailAssignment(vp);
  *vp = linkVal(valTermRef(A2));

  succeed;
}


static
PRED_IMPL("del_attr", 2, del_attr2, 0)	/* +Var, +Name */
{ PRED_LD
  Word av;
  atom_t name;

  if ( !PL_get_atom_ex(A2, &name) )
    fail;

  av = valTermRef(A1);
  deRef(av);

  if ( isAttVar(*av) )
  { if ( del_attr(av, name PASS_LD) )
    { Word l = valPAttVar(*av);

      deRef(l);
      if ( isNil(*l) )
      { TrailAssignment(av);
	setVar(*av);
      }
    }
  }

  succeed;
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

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$freeze", 2, freeze, PL_FA_TRANSPARENT)
{ PRED_LD
  Word v;

  requireStack(global, 9*sizeof(word));

  v = valTermRef(A1);
  deRef(v);
  if ( isVar(*v) || isAttVar(*v) )
  { Module m = NULL;
    term_t g = PL_new_term_ref();
    Word gt = allocGlobal(3);			/* 3 cells global */
    word goal = consPtr(gt, TAG_COMPOUND|STG_GLOBAL);

    PL_strip_module(A2, &m, g);
    gt[0] = FUNCTOR_colon2;
    gt[1] = m->name;
    gt[2] = *valTermRef(g);

    if ( isVar(*v) )
    { make_new_attvar(v PASS_LD);		/* <= 2 cells global */
      put_attr(v, ATOM_freeze, &goal PASS_LD);	/* <= 4 cells global */
    } else
    { Word vp;

      if ( find_attr(v, ATOM_freeze, &vp PASS_LD) )
      { Word gc = allocGlobal(3);		/* 3 cells global */

	gc[0] = FUNCTOR_comma2;
	gc[1] = linkVal(vp);
	gc[2] = goal;

	TrailAssignment(vp);
	*vp = consPtr(gc, TAG_COMPOUND|STG_GLOBAL);
      } else if ( vp )				/* vp points to [] */
      { Word at = allocGlobal(4);		/* 4 cells global  */

	at[0] = FUNCTOR_att3;
	at[1] = ATOM_freeze;
	at[2] = goal;
	at[3] = ATOM_nil;

	assert(*vp == ATOM_nil);
	TrailAssignment(vp);
	*vp = consPtr(at, TAG_COMPOUND|STG_GLOBAL);
      } else
	assert(0);				/* bad attribute list */
    }

    succeed;
  }

  fail;
}

		 /*******************************
		 *	    REGISTRATION	*
		 *******************************/

BeginPredDefs(attvar)
  PRED_DEF("attvar",    1, attvar,    0)
  PRED_DEF("put_attr",  3, put_attr3, 0)
  PRED_DEF("get_attr",  3, get_attr3, 0)
  PRED_DEF("del_attr",  2, del_attr2, 0)
  PRED_DEF("get_attrs", 2, get_attrs, 0)
  PRED_DEF("put_attrs", 2, put_attrs, 0)
  PRED_DEF("$freeze",   2, freeze,    PL_FA_TRANSPARENT)
EndPredDefs

#endif /*O_ATTVAR*/
