/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
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

#include "pl-incl.h"

typedef struct flag *	Flag;

#define FLG_ATOM	0
#define FLG_INTEGER	1
#define FLG_REAL	2

struct flag
{ word	key;				/* key to the flag */
  int	type;				/* type (atom, int, real */
  union
  { atom_t a;				/* atom */
    long   i;				/* integer */
    double f;				/* float */
  } value;				/* value of the flag */
};

#define flagTable (GD->flags.table)
#define LOCK()   PL_LOCK(L_FLAG)
#define UNLOCK() PL_UNLOCK(L_FLAG)

#undef LD
#define LD LOCAL_LD

void
initFlags(void)
{ flagTable = newHTable(FLAGHASHSIZE);
}


static Flag
lookupFlag(word key ARG_LD)
{ Symbol symb;
  Flag f;

  if ( (symb = lookupHTable(flagTable, (void *)key)) )
    return (Flag)symb->value;

  f = (Flag) allocHeap(sizeof(struct flag));
  f->key = key;
  if ( isAtom(key) )
    PL_register_atom(key);
  f->type = FLG_INTEGER;
  f->value.i = 0;
  addHTable(flagTable, (void *)key, f);

  return f;
}


static void
freeFlagValue(Flag f)
{ if ( f->type == FLG_ATOM )
    PL_unregister_atom(f->value.a);
}


static
PRED_IMPL("flag", 3, flag, 0)
{ PRED_LD
  Flag f;
  word key;
  atom_t a;
  number n;
  word rval;

  term_t name = A1;
  term_t old = A2;
  term_t new = A3;

  if ( !getKeyEx(name, &key PASS_LD) )
    fail;
  rval = FALSE;

  LOCK();
  f = lookupFlag(key PASS_LD);
  switch(f->type)
  { case FLG_ATOM:
      if ( !PL_unify_atom(old, f->value.a) )
	goto out;
      break;
    case FLG_INTEGER:
      if ( !PL_unify_integer(old, f->value.i) )
	goto out;
      break;
    case FLG_REAL:
    { 
#ifdef DOUBLE_ALIGNMENT
      double v;
      doublecpy(&v, &f->value.f);
      if ( !PL_unify_float(old, v) )
	goto out;
#else
      if ( !PL_unify_float(old, f->value.f) )
	goto out;
#endif
      break;
    }
    default:
      assert(0);
  }

  rval = TRUE;
  if ( PL_get_atom(new, &a) )
  { freeFlagValue(f);
    f->type = FLG_ATOM;
    f->value.a = a;
    PL_register_atom(a);
  } else if ( valueExpression(new, &n PASS_LD) )
  { if ( n.type == V_REAL && !trueFeature(ISO_FEATURE) )
      canoniseNumber(&n);

    if ( n.type == V_INTEGER )
    { freeFlagValue(f);
      f->type = FLG_INTEGER;
      f->value.i = n.value.i;
    } else
    { freeFlagValue(f);
      f->type = FLG_REAL;
#ifdef DOUBLE_ALIGNMENT
      doublecpy(&f->value.f, &n.value.f);
#else
      f->value.f = n.value.f;
#endif
    }
  } else
    rval = PL_error("flag", 3, NULL, ERR_TYPE, ATOM_flag_value, new);

out:
  UNLOCK();

  return rval;
}


word
pl_current_flag(term_t k, control_t h)
{ GET_LD
  Symbol symb;
  TableEnum e;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { word key;

      if ( PL_is_variable(k) )
      {	e = newTableEnum(flagTable);
	break;
      }
      if ( getKeyEx(k, &key PASS_LD) &&
	   lookupHTable(flagTable, (void *)key) )
	succeed;
      fail;
    }
    case FRG_REDO:
      e = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
      e = ForeignContextPtr(h);
      freeTableEnum(e);
    default:
      succeed;
  }

  while( (symb = advanceTableEnum(e)) )
  { Flag f = symb->value;

    if ( !unifyKey(k, f->key) )
      continue;

    ForeignRedoPtr(e);
  }

  freeTableEnum(e);
  fail;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(flag)
  PRED_DEF("flag", 3, flag, 0)
EndPredDefs
