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
freeFlagSymbol(void *name, void *value)
{ Flag f = value;

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
{ GET_LD
  Flag f, of;

  if ( (f = lookupHTable(flagTable, (void *)key)) )
    return f;

  f = (Flag) allocHeapOrHalt(sizeof(struct flag));
  f->key = key;
  if ( isAtom(key) )
    PL_register_atom(key);
  f->type = FLG_INTEGER;
  f->value.i = 0;
  if ( (of=addHTable(flagTable, (void *)key, f)) != f )
  { freeHeap(f, sizeof(*f));
    f = of;
  }

  return f;
}


static void
freeFlagValue(Flag f)
{ if ( f->type == FLG_ATOM )
    PL_unregister_atom(f->value.a);
}


static
PRED_IMPL("get_flag", 2, get_flag, 0)
{ PRED_LD
  Flag f;
  word key;
  int rc;

  term_t name  = A1;
  term_t value = A2;

  if ( !getKeyEx(name, &key PASS_LD) )
    return FALSE;

  f = lookupFlag(key);
  LOCK();
  switch(f->type)
  { case FLG_ATOM:
      rc = PL_unify_atom(value, f->value.a);
      break;
    case FLG_INTEGER:
      rc = PL_unify_int64(value, f->value.i);
      break;
    case FLG_FLOAT:
      rc = PL_unify_float(value, f->value.f);
      break;
    default:
      rc = FALSE;
      assert(0);
  }
  UNLOCK();

  return rc;
}


static
PRED_IMPL("set_flag", 2, set_flag, 0)
{ PRED_LD
  Flag f;
  word key;
  atom_t a;
  number n;

  term_t name  = A1;
  term_t value = A2;

  if ( !getKeyEx(name, &key PASS_LD) )
    return FALSE;
  f = lookupFlag(key);

  if ( PL_get_atom(value, &a) )
  { LOCK();
    freeFlagValue(f);
    f->type = FLG_ATOM;
    f->value.a = a;
    PL_register_atom(a);
    UNLOCK();
    return TRUE;
  } else if ( PL_get_number(value, &n) )
  { switch(n.type)
    { case V_INTEGER:
      { LOCK();
	freeFlagValue(f);
	f->type = FLG_INTEGER;
	f->value.i = n.value.i;
	UNLOCK();
	return TRUE;
      }
#ifdef O_GMP
      case V_MPZ:
	return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_int64_t);
      case V_MPQ:
	goto type_error;
#endif
      case V_FLOAT:
      { LOCK();
	freeFlagValue(f);
	f->type = FLG_FLOAT;
        f->value.f = n.value.f;
	UNLOCK();
	return TRUE;
      }
      default:
	goto type_error;
    }
  } else
  {
  type_error:
    return PL_error("flag", 3, NULL, ERR_TYPE, ATOM_flag_value, value);
  }
}


word
pl_current_flag(term_t k, control_t h)
{ GET_LD
  Flag f;
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

  while( advanceTableEnum(e, NULL, (void**)&f) )
  { if ( !unifyKey(k, f->key) )
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
  PRED_DEF("get_flag", 2, get_flag, 0)
  PRED_DEF("set_flag", 2, set_flag, 0)
EndPredDefs
