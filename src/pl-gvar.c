/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2015, University of Amsterdam
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
free_nb_linkval_name(atom_t name)
{ PL_unregister_atom(name);
}


static void
free_nb_linkval_value(word value)
{
  if ( isAtom(value) )
    PL_unregister_atom(value);
  else if ( storage(value) == STG_GLOBAL )
  { GET_LD
    LD->gvar.grefs--;
  }
}


static void
free_nb_linkval_symbol(void *name, void* value)
{ free_nb_linkval_value((word)value);
  free_nb_linkval_name((atom_t)name);
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

  if ( !PL_get_atom_ex(var, &name) )
    fail;

  if ( !LD->gvar.nb_vars )
  { LD->gvar.nb_vars = newHTable(32);
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

  if ( !(old = (word)lookupHTable(LD->gvar.nb_vars, (void*)name)) )
  { addNewHTable(LD->gvar.nb_vars, (void*)name, (void*)ATOM_nil);
    PL_register_atom(name);
    PL_register_atom(ATOM_nil);
    old = ATOM_nil;
  }
  assert(old);

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
      updateHTable(LD->gvar.nb_vars, (void*)name, (void*)makeRefG(p));
    }

    TrailAssignment(p);
    *p = w;
  } else
  { if ( storage(old) == STG_GLOBAL )
      LD->gvar.grefs--;

    updateHTable(LD->gvar.nb_vars, (void*)name, (void*)w);

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


/* gvar_value__LD() is a quick and dirty way to get a global variable.
   It is used to get '$variable_names' for compiler warnings.

   Note that this function does *not* call auto_define_gvar().  This
   is on purpose because we cannot call Prolog from the compiler and
   there is no need for this hook for this variable.  Be careful to
   fix this if this function is to be used for other purposes.
*/

int
gvar_value__LD(atom_t name, Word p ARG_LD)
{ if ( LD->gvar.nb_vars )
  { word w;
    if ( (w = (word)lookupHTable(LD->gvar.nb_vars, (void*)name)) )
    { *p = w;
      return TRUE;
    }
  }

  return FALSE;
}


static int
getval(term_t var, term_t value, int raise_error ARG_LD)
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
    { word w;
      if ( (w = (word)lookupHTable(LD->gvar.nb_vars, (void*)name)) )
      { term_t tmp = PL_new_term_ref();

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
  if ( raise_error )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE,
		    ATOM_variable, var);
  else
    return FALSE;
}


static
PRED_IMPL("nb_linkval", 2, nb_linkval, 0)
{ PRED_LD

  return setval(A1, A2, FALSE PASS_LD);
}


static
PRED_IMPL("nb_getval", 2, nb_getval, 0)
{ PRED_LD

  return getval(A1, A2, TRUE PASS_LD);
}


static
PRED_IMPL("b_setval", 2, b_setval, 0)
{ PRED_LD

  return setval(A1, A2, TRUE PASS_LD);
}

static
PRED_IMPL("b_getval", 2, b_getval, 0)
{ PRED_LD

  return getval(A1, A2, TRUE PASS_LD);
}


static
PRED_IMPL("nb_delete", 1, nb_delete, 0)
{ PRED_LD
  atom_t name;

  if ( !PL_get_atom_ex(A1, &name) )
    fail;

  if ( LD->gvar.nb_vars )
  { word w;
    if ( (w = (word)lookupHTable(LD->gvar.nb_vars, (void*)name)) )
    { deleteHTable(LD->gvar.nb_vars, (void*)name);
      free_nb_linkval_name(name);
      free_nb_linkval_value(w);
    }
  }

  succeed;
}


static
PRED_IMPL("nb_current", 2, nb_current, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  TableEnum e;
  atom_t name;
  word val;
  fid_t fid;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
      if ( PL_is_atom(A1) )
	return getval(A1, A2, FALSE PASS_LD);
      if ( !PL_is_variable(A1) )
	return PL_type_error("atom", A1);
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
  while( advanceTableEnum(e, (void**)&name, (void**)&val) )
  { if ( PL_unify_atom(A1, name) &&
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
