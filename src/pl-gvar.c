/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#ifdef O_GVAR


		 /*******************************
		 * NON-BACKTRACKABLE GLOBAL VARS*
		 *******************************/

void
freezeGlobal(ARG1_LD)
{ LD->frozen_bar = LD->mark_bar = gTop;
  DEBUG(2, Sdprintf("*** frozen bar to %p at freezeGlobal()\n",
		    LD->frozen_bar));
}


void
destroyGlobalVars()
{ GET_LD

  if ( LD->gvar.nb_vars )
  { destroyHTable(LD->gvar.nb_vars);
    LD->gvar.nb_vars = NULL;
  }

  LD->gvar.grefs = 0;
  LD->frozen_bar = NULL;
}


static void
free_nb_linkval_symbol(Symbol s)
{ word w = (word)s->value;

  if ( isAtom(w) )
    PL_unregister_atom(w);
  else if ( storage(w) == STG_GLOBAL )
  { GET_LD
    LD->gvar.grefs--;
  }

  PL_unregister_atom((atom_t)s->name);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Assign  a  global  variable.  For    backtrackable   variables  we  need
TrailAssignment(), but we can only call that  on addresses on the global
stack. Therefore we must make  a  reference   to  the  real value if the
variable is not already a reference.

SHIFT-SAFE: TrailAssignment() takes at most g+t=1+2.  One more Trail and
	    2 more allocGlobal(1) makes g+t<3+3
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
setval(term_t var, term_t value, int backtrackable ARG_LD)
{ atom_t name;
  Word p;
  word w, old;
  Symbol s;

  if ( !PL_get_atom_ex(var, &name) )
    fail;

  if ( !LD->gvar.nb_vars )
  { LD->gvar.nb_vars = newHTable(32|TABLE_UNLOCKED);
    LD->gvar.nb_vars->free_symbol = free_nb_linkval_symbol;
  }

  if ( !hasGlobalSpace(3) )		/* also ensures trail for */
  { int rc;				/* TrailAssignment() */

    if ( (rc=ensureGlobalSpace(3, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  p = valTermRef(value);
  deRef(p);
  w = *p;

  if ( canBind(w) )
  { if ( onStackArea(local, p) )
    { Word p2 = allocGlobal(1);

      setVar(*p2);
      w = *p = makeRef(p2);
      LTrail(p);
    } else
    { w = makeRef(p);
    }
  }

  if ( !(s=lookupHTable(LD->gvar.nb_vars, (void*)name)) )
  { s = addHTable(LD->gvar.nb_vars, (void*)name, (void*)ATOM_nil);
    PL_register_atom(name);
    PL_register_atom(ATOM_nil);
  }
  assert(s);

  old = (word)s->value;
  if ( w == old )
    succeed;
  if ( isAtom(old) )
    PL_unregister_atom(old);

  if ( backtrackable )
  { Word p;

    if ( isRef(old) )
    { p = unRef(old);
    } else
    { p = allocGlobal(1);
      *p = old;
      freezeGlobal(PASS_LD1);		/* The value location must be */
      if ( storage(old) != STG_GLOBAL )	/* preserved */
	LD->gvar.grefs++;
      s->value = (void*)makeRefG(p);
    }

    TrailAssignment(p);
    *p = w;
  } else
  { if ( storage(old) == STG_GLOBAL )
      LD->gvar.grefs--;

    s->value = (void *)w;

    if ( storage(w) == STG_GLOBAL )
    { freezeGlobal(PASS_LD1);
      LD->gvar.grefs++;
    } else if ( isAtom(w) )
      PL_register_atom(w);
  }

  succeed;
}


typedef enum
{ gvar_fail,
  gvar_retry,
  gvar_error
} gvar_action;


static gvar_action
auto_define_gvar(atom_t name)
{ GET_LD
  static predicate_t pred;
  fid_t fid;
  term_t av;
  gvar_action rc = gvar_error;

  if ( !pred )
    pred = PL_predicate("exception", 3, "user");

  if ( !(fid = PL_open_foreign_frame()) )
    return gvar_error;
  av = PL_new_term_refs(3);
  PL_put_atom(av+0, ATOM_undefined_global_variable);
  PL_put_atom(av+1, name);

  if ( PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) )
  { atom_t action;			/* retry, error, fail */

    if ( (rc=PL_get_atom_ex(av+2, &action)) )
    { if ( action == ATOM_retry )
	rc = gvar_retry;
      else if ( action == ATOM_fail )
	rc = gvar_fail;
    }
  }


  PL_close_foreign_frame(fid);

  return rc;
}


static int
getval(term_t var, term_t value ARG_LD)
{ atom_t name;
  int i;

  if ( !PL_get_atom_ex(var, &name) )
    fail;
  if ( !hasGlobalSpace(0) )
  { int rc;

    if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }


  for(i=0; i<2; i++)
  { if ( LD->gvar.nb_vars )
    { Symbol s = lookupHTable(LD->gvar.nb_vars, (void*)name);

      if ( s )
      { term_t tmp = PL_new_term_ref();
	word w = (word)s->value;

	*valTermRef(tmp) = w;
	return PL_unify(value, tmp);
      }
    }

    switch(auto_define_gvar(name))
    { case gvar_fail:
	fail;
      case gvar_retry:
	continue;
      case gvar_error:
	if ( exception_term )
	  fail;				/* error from handler */
        goto error;
    }
  }

error:
  return PL_error(NULL, 0, NULL, ERR_EXISTENCE,
		  ATOM_variable, var);
}


static
PRED_IMPL("nb_linkval", 2, nb_linkval, 0)
{ PRED_LD

  return setval(A1, A2, FALSE PASS_LD);
}


static
PRED_IMPL("nb_getval", 2, nb_getval, 0)
{ PRED_LD

  return getval(A1, A2 PASS_LD);
}


static
PRED_IMPL("b_setval", 2, b_setval, 0)
{ PRED_LD

  return setval(A1, A2, TRUE PASS_LD);
}

static
PRED_IMPL("b_getval", 2, b_getval, 0)
{ PRED_LD

  return getval(A1, A2 PASS_LD);
}


static
PRED_IMPL("nb_delete", 1, nb_delete, 0)
{ PRED_LD
  atom_t name;

  if ( !PL_get_atom_ex(A1, &name) )
    fail;

  if ( LD->gvar.nb_vars )
  { Symbol s = lookupHTable(LD->gvar.nb_vars, (void*)name);

    if ( s )
    { free_nb_linkval_symbol(s);
      deleteSymbolHTable(LD->gvar.nb_vars, s);
    }
  }

  succeed;
}


static
PRED_IMPL("nb_current", 2, nb_current, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  TableEnum e;
  Symbol s;
  fid_t fid;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
      if ( LD->gvar.nb_vars )
      { e = newTableEnum(LD->gvar.nb_vars);
	break;
      } else
      { fail;
      }
    case FRG_REDO:
      e =  CTX_PTR;
      break;
    case FRG_CUTTED:
      e =  CTX_PTR;
      freeTableEnum(e);
      succeed;
    default:
      assert(0);
      fail;
  }

  if ( !(fid = PL_open_foreign_frame()) )
  { freeTableEnum(e);
    return FALSE;
  }
  while( (s=advanceTableEnum(e)) )
  { atom_t name = (atom_t)s->name;
    word   val = (word)s->value;

    if ( PL_unify_atom(A1, name) &&
	 unify_ptrs(valTermRef(A2), &val, 0 PASS_LD) )
    { PL_close_foreign_frame(fid);
      ForeignRedoPtr(e);
    } else
    { PL_rewind_foreign_frame(fid);
    }
  }
  PL_close_foreign_frame(fid);

  freeTableEnum(e);
  fail;
}


		 /*******************************
		 *	    REGISTRATION	*
		 *******************************/

BeginPredDefs(gvar)
  PRED_DEF("b_setval",   2, b_setval,   0)
  PRED_DEF("b_getval",   2, b_getval,   0)
  PRED_DEF("nb_linkval", 2, nb_linkval, 0)
  PRED_DEF("nb_getval",  2, nb_getval,  0)
  PRED_DEF("nb_current", 2, nb_current, PL_FA_NONDETERMINISTIC)
  PRED_DEF("nb_delete",  1, nb_delete,  0)
EndPredDefs

#endif /*O_ATTVAR*/
