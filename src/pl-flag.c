/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: implement flag/3
*/

#include "pl-incl.h"

typedef struct flag *	Flag;

struct flag
{ Flag	next;
  word	key;				/* key to the flag */
  word	value;				/* value of the flag */
};

static Flag flagTable[FLAGHASHSIZE];

forwards Flag lookupFlag P((word));

void
initFlags()
{ register Flag *f;
  register int n;

  for(n=0, f=flagTable; n < (FLAGHASHSIZE-1); n++, f++)
    *f = (Flag) makeRef(f+1);
}

static Flag
lookupFlag(key)
word key;
{ int v = pointerHashValue(key, FLAGHASHSIZE);
  Flag f;

  for(f=flagTable[v]; f && !isRef((word)f); f=f->next)
  { if (f->key == key)
      return f;
  }
  f = (Flag) allocHeap(sizeof(struct flag) );
  f->next = flagTable[v];
  flagTable[v] = f;
  f->key = key;
  f->value = consNum(0);

  return f;
}

word
pl_flag(name, old, new)
Word name, old, new;
{ Flag f;
  word key;

  if ((key = getKey(name)) == (word) NULL)
    return warning("flag/2: illegal key");

  f = lookupFlag(key);
  TRY(unifyAtomic(old, f->value) );
  if (isAtom(*new) || isInteger(*new))
  { f->value = *new;
    succeed;
  } else
  { word value = evaluate(new);
    if ( isInteger(value) )
    { f->value = value;
      succeed;
    }
  }

  return warning("flag/2: value should be an atom, integer or expression");
}

word
pl_current_flag(k, h)
Word k;
word h;
{ Flag f;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      f = flagTable[0];
      break;
    case FRG_REDO:
      f = (Flag) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(; f; f = f->next)
  { while(isRef((word)f) )
    { f = *((Flag *)unRef(f));
      if (f == (Flag) NULL)
	fail;
    }
    if ( unifyKey(k, f->key) == FALSE )
      continue;

    return_next_table(Flag, f);
  }

  fail;
}
