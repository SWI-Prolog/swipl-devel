/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: implement flag/3
*/

#include "pl-incl.h"

typedef struct flag *	Flag;

struct flag
{ Flag  next;
  word	key;			/* key to the flag */
  word	value;			/* value of the flag */
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
  f->value = consInt(0);

  return f;
}

word
pl_flag(term_t name, term_t old, term_t new)
{ Flag f;
  word key;
  Word n;

  if ( !(key = getKey(name)) )
    return warning("flag/2: illegal key");

  f = lookupFlag(key);
  TRY(_PL_unify_atomic(old, f->value));

  n = valTermRef(new);
  deRef(n);
  if ( isAtom(*n) || isTaggedInt(*n) )
  { f->value = *n;
    succeed;
  } else
  { number n;

    if ( valueExpression(new, &n) &&
	 toIntegerNumber(&n) &&
	 inTaggedNumRange(n.value.i) )
    { f->value = consInt(n.value.i);
      succeed;
    }
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
