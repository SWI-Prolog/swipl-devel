/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: implement flag/3
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

forwards Flag lookupFlag(word);

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

word
pl_flag(term_t name, term_t old, term_t new)
{ Flag f;
  word key;
  atom_t a;
  number n;

  if ( !(key = getKey(name)) )
    return warning("flag/2: illegal key");

  f = lookupFlag(key);
  switch(f->type)
  { case FLG_ATOM:
      TRY(PL_unify_atom(old, f->value.a));
      break;
    case FLG_INTEGER:
      TRY(PL_unify_integer(old, f->value.i));
      break;
    case FLG_REAL:
    { 
#ifdef DOUBLE_ALIGNMENT
      double v;
      doublecpy(&v, &f->value.f);
      TRY(PL_unify_float(old, v));
#else
      TRY(PL_unify_float(old, f->value.f));
#endif
      break;
    }
    default:
      assert(0);
  }

  if ( PL_get_atom(new, &a) )
  { f->type = FLG_ATOM;
    f->value.a = a;

    succeed;
  } else if ( valueExpression(new, &n) )
  { canoniseNumber(&n);

    if ( n.type == V_INTEGER )
    { f->type = FLG_INTEGER;
      f->value.i = n.value.i;
    } else
    { 
      f->type = FLG_REAL;
#ifdef DOUBLE_ALIGNMENT
      doublecpy(&f->value.f, &n.value.f);
#else
      f->value.f = n.value.f;
#endif
    }

    succeed;
  }

  return warning("flag/2: value should be an atom, integer or expression");
}

word
pl_current_flag(term_t k, term_t h)
{ Symbol symb;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { word key;

      if ( (key = getKey(k)) )
      { if ( lookupHTable(flagTable, (void *)key) )
	  succeed;
	fail;
      }
      if ( PL_is_variable(k) )
      {	symb = firstHTable(flagTable);
	break;
      }
      return warning("current_flag/2: illegal key");
    }
    case FRG_REDO:
      symb = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for( ; symb; symb = nextHTable(flagTable, symb) )
  { Flag f = (Flag)symb->value;

    if ( !unifyKey(k, f->key) )
      continue;

    if ( !(symb = nextHTable(flagTable, symb)) )
      succeed;

    ForeignRedoPtr(symb);
  }

  fail;
}
