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
{ Flag  next;
  word	key;				/* key to the flag */
  int	type;				/* type (atom, int, real */
  union
  { atom_t a;				/* atom */
    long   i;				/* integer */
    double f;				/* float */
  } value;				/* value of the flag */
};

static Flag flagTable[FLAGHASHSIZE];

forwards Flag lookupFlag(word);

void
initFlags(void)
{ register Flag *f;
  register int n;

  for(n=0, f=flagTable; n < (FLAGHASHSIZE-1); n++, f++)
    *f = makeTableRef(f+1);
}

static Flag
lookupFlag(word key)
{ int v = pointerHashValue(key, FLAGHASHSIZE);
  Flag f;

  for(f=flagTable[v]; f && !isTableRef(f); f=f->next)
  { if (f->key == key)
      return f;
  }
  f = (Flag) allocHeap(sizeof(struct flag) );
  f->next = flagTable[v];
  flagTable[v] = f;
  f->key = key;
  f->type = FLG_INTEGER;
  f->value.i = 0;

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
      TRY(PL_unify_float(old, f->value.f));
      break;
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
    { f->type = FLG_REAL;
      f->value.f = n.value.f;
    }

    succeed;
  }

  return warning("flag/2: value should be an atom, integer or expression");
}

word
pl_current_flag(term_t k, term_t h)
{ Flag f;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      f = flagTable[0];
      break;
    case FRG_REDO:
      f = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(; f; f = f->next)
  { while(isTableRef(f) )
    { f = unTableRef(Flag, f);
      if ( !f )
	fail;
    }
    if ( !unifyKey(k, f->key) )
      continue;

    return_next_table(Flag, f, ;);
  }

  fail;
}
