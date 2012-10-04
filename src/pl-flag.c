/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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

typedef enum flag_type
{ FLG_ATOM,
  FLG_INTEGER,
  FLG_FLOAT
} flag_type;

typedef struct flag
{ word	key;				/* key to the flag */
  int	type;				/* type (atom, int, float */
  union
  { atom_t  a;				/* atom */
    int64_t i;				/* integer */
    double  f;				/* float */
  } value;				/* value of the flag */
} *Flag;

static void	freeFlagValue(Flag f);

#define flagTable (GD->flags.table)
#define LOCK()   PL_LOCK(L_FLAG)
#define UNLOCK() PL_UNLOCK(L_FLAG)

#undef LD
#define LD LOCAL_LD

static void
freeFlagSymbol(Symbol s)
{ Flag f = s->value;

  freeFlagValue(f);
  freeHeap(f, sizeof(*f));
}


void
initFlags(void)
{ flagTable = newHTable(FLAGHASHSIZE);
  flagTable->free_symbol = freeFlagSymbol;
}


void
cleanupFlags(void)
{ Table t;

  if ( (t=flagTable) )
  { flagTable = NULL;
    destroyHTable(t);
  }
}


static Flag
lookupFlag(word key)
{ Symbol symb;
  Flag f;

  if ( (symb = lookupHTable(flagTable, (void *)key)) )
    return (Flag)symb->value;

  f = (Flag) allocHeapOrHalt(sizeof(struct flag));
  f->key = key;
  if ( isTextAtom(key) )
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
PRED_IMPL("flag", 3, flag, PL_FA_TRANSPARENT)
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
  f = lookupFlag(key);
  switch(f->type)
  { case FLG_ATOM:
      if ( !PL_unify_atom(old, f->value.a) )
	goto out;
      break;
    case FLG_INTEGER:
      if ( !PL_unify_int64(old, f->value.i) )
	goto out;
      break;
    case FLG_FLOAT:
      if ( !PL_unify_float(old, f->value.f) )
	goto out;
      break;
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
  { switch(n.type)
    { case V_INTEGER:
      { freeFlagValue(f);
	f->type = FLG_INTEGER;
	f->value.i = n.value.i;
	break;
      }
#ifdef O_GMP
      case V_MPZ:
      case V_MPQ:
	goto type_error;
#endif
      case V_FLOAT:
      { freeFlagValue(f);
	f->type = FLG_FLOAT;
        f->value.f = n.value.f;
	break;
      }
    }
  } else
  {
#ifdef O_GMP
    type_error:
#endif
    rval = PL_error("flag", 3, NULL, ERR_TYPE, ATOM_flag_value, new);
  }

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
  PRED_DEF("flag", 3, flag, PL_FA_TRANSPARENT)
EndPredDefs
