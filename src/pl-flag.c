/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: implement flag/3
    MT-status: SAFE
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

void
initFlags(void)
{ flagTable = newHTable(FLAGHASHSIZE);
}


static Flag
lookupFlag(word key)
{ Symbol symb;
  Flag f;

  if ( (symb = lookupHTable(flagTable, (void *)key)) )
    return (Flag)symb->value;

  f = (Flag) allocHeap(sizeof(struct flag));
  f->key = key;
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


word
pl_flag(term_t name, term_t old, term_t new)
{ Flag f;
  word key;
  atom_t a;
  number n;
  word rval;

  if ( !(key = getKey(name)) )
    return PL_error("flag", 3, NULL, ERR_TYPE, ATOM_key, key);
  rval = FALSE;

  LOCK();
  f = lookupFlag(key);
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
  } else if ( valueExpression(new, &n) )
  { canoniseNumber(&n);

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
pl_current_flag(term_t k, term_t h)
{ Symbol symb;
  TableEnum e;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { word key;

      if ( (key = getKey(k)) )
      { if ( lookupHTable(flagTable, (void *)key) )
	  succeed;
	fail;
      }
      if ( PL_is_variable(k) )
      {	e = newTableEnum(flagTable);
	break;
      }
      return PL_error("current_flag", 2, NULL, ERR_TYPE, ATOM_key, key);
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


