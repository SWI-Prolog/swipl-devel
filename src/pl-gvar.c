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
#ifdef O_GVAR

#undef LD
#define LD LOCAL_LD


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

  requireStack(global, sizeof(word));
  p = valTermRef(value);
  deRef(p);
  w = *p;

  if ( canBind(w) )
  { if ( onStackArea(local, p) )
    { Word p2 = allocGlobal(1);

      setVar(*p2);
      w = *p = makeRef(p2);
      Trail(p);
    } else
    { w = makeRef(p);
    }
  }

  if ( !(s=lookupHTable(LD->gvar.nb_vars, (void*)name)) )
  { s = addHTable(LD->gvar.nb_vars, (void*)name, (void*)ATOM_nil);
    PL_register_atom(name);
  }
  assert(s);

  old = (word)s->value;
  if ( w == old )
    succeed;

  if ( backtrackable )
  { if ( isRef(old) )
    { Word p = unRef(old);

      TrailAssignment(p);
      *p = w;
    } else
    { Word p = allocGlobal(1);
      *p = old;
      freezeGlobal(PASS_LD1);
      if ( storage(old) != STG_GLOBAL )
	LD->gvar.grefs++;
      s->value = (void*)makeRefG(p);
      TrailAssignment(p);
      *p = w;
    }
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


static int
getval(term_t var, term_t value ARG_LD)
{ atom_t name;

  if ( !PL_get_atom_ex(var, &name) )
    fail;

  if ( LD->gvar.nb_vars )
  { Symbol s = lookupHTable(LD->gvar.nb_vars, (void*)name);
    
    if ( s )
    { word w = (word)s->value;

      return unify_ptrs(valTermRef(value), &w PASS_LD);
    }
  }

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

  fid = PL_open_foreign_frame();
  while( (s=advanceTableEnum(e)) )
  { atom_t name = (atom_t)s->name;
    word   val = (word)s->value;

    if ( PL_unify_atom(A1, name) &&
	 unify_ptrs(valTermRef(A2), &val PASS_LD) )
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
