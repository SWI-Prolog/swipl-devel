/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: generic support for (numeric) hashTables
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Anonymous hash-tables.  This  is  rather  straightforward.   A  peculiar
thing  on these and a number of dedicated hashtables build in the system
is that the last symbol of  a  specific  hash  value  is  not  the  NULL
pointer,  but  a reference pointer to the next entry.  This allows us to
enumerate  all  entries  of  the  table  with  only  one   word   status
information.   This  is  more  efficient  and  simple to handle with the
interface for non-deterministic C functions (current_predicate/2, ...).

This module also can allocate from the local stack for temporary  tables
needed by foreign language functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
allocHTableEntries(Table ht)
{ int n;
  Symbol *p;

  ht->entries = allocHeap(ht->buckets * sizeof(Symbol));

  for(n=0, p = &ht->entries[0]; n < ht->buckets-1; n++, p++)
    *p = (Symbol) makeRef(p+1);
  *p = (Symbol) NULL;
}


Table
newHTable(int buckets)
{ Table ht;

  ht = (Table) allocHeap(sizeof(struct table));
  ht->buckets = buckets;
  ht->size    = 0;
  ht->locked  = 0;

  allocHTableEntries(ht);
  return ht;
}


void
destroyHTable(Table ht)
{ clearHTable(ht);
  freeHeap(ht->entries, ht->buckets * sizeof(Symbol));
  freeHeap(ht, sizeof(struct table));
}


#if O_DEBUG
static int lookups;
static int cmps;

void
exitTables(void *arg)
{ Sdprintf("hashstat: Anonymous tables: %d lookups using %d compares\n",
	   lookups, cmps);
}
#endif /*O_DEBUG*/


void
initTables()
{
  DEBUG(0, PL_on_halt(exitTables, NULL));
}


Symbol
lookupHTable(Table ht, Void name)
{ register Symbol s = ht->entries[pointerHashValue(name, ht->buckets)];

  DEBUG(0, lookups++);
  for(;s && !isRef((word)s); s = s->next)
  { DEBUG(0, cmps++);
    if (s->name == (word)name)
      return s;
  }

  return (Symbol) NULL;
}


static void
rehashHTable(Table ht)
{ Symbol *oldtab  = ht->entries;
  int    oldbucks = ht->buckets;
  Symbol s, n;
  int done;

  startCritical;
  ht->buckets *= 2;
  allocHTableEntries(ht);

  DEBUG(0, Sdprintf("Rehashing table 0x%x to %d entries\n",
		    ht, ht->buckets));

  for(s = oldtab[0]; s; s = n)
  { int v;

    while(isRef((word)s) )
    { s = *((Symbol *)unRef(s));
      if ( s == NULL )
	goto out;
    }
    done++;
    n = s->next;
    v = pointerHashValue(s->name, ht->buckets);
    s->next = ht->entries[v];
    ht->entries[v] = s;
  }

out:
  assert(done = ht->size);
  freeHeap(oldtab, oldbucks * sizeof(Symbol));
  endCritical;
}


bool
addHTable(Table ht, Void name, Void value)
{ register Symbol s;
  register int v = pointerHashValue(name, ht->buckets);

  if (lookupHTable(ht, name) != (Symbol) NULL)
    fail;
  s = (Symbol) allocHeap(sizeof(struct symbol));
  s->name = (word)name;
  s->value = (word)value;
  s->next = ht->entries[v];
  ht->entries[v] = s;
  ht->size++;

  if ( ht->buckets * 2 < ht->size && !ht->locked )
    rehashHTable(ht);

  succeed;
}  


bool
deleteHTable(Table ht, Void name)
{ register int v = pointerHashValue(name, ht->buckets);
  register Symbol *s = &ht->entries[v];
  Symbol symb = *s;

  for(;symb && !isRef((word)symb); s = &symb->next)
  { symb = *s;
    if (symb->name == (word)name)
    { *s = symb->next;
      freeHeap(symb, sizeof(struct symbol));
      succeed;
    }
  }

  fail;
}


Symbol
nextHTable(Table ht, register Symbol s)
{ s = s->next;
  while(s != (Symbol) NULL && isRef((word)s) )
    s = *((Symbol *)unRef(s));

  return s;
}

Symbol
firstHTable(Table ht)
{ register Symbol s = ht->entries[0];

  while(s != (Symbol) NULL && isRef((word)s) )
    s = *((Symbol *)unRef(s));

  return s;
}  

void
clearHTable(Table ht)
{ int n;
  register Symbol s;

  for(n=0; n < ht->buckets; n++)
  { s = ht->entries[n];
    while(s && !isRef((word)s))
    { register Symbol q = s->next;
      freeHeap(s, sizeof(struct symbol));
      s = q;
    }
    ht->entries[n] = s;
  }
}

		/********************************
		*     TABLES ON LOCAL STACK     *
		*********************************/

Table
newLocalTable(int buckets)
{ Symbol *p;
  int n;
  Table ht;

  ht = (Table) allocLocal(sizeof(struct table));
  ht->buckets = buckets;
  ht->locked  = 0;
  ht->entries = allocLocal(buckets * sizeof(Symbol));

  for(n=0, p = &ht->entries[0]; n < buckets; n++, p++)
    *p = (Symbol) NULL;

  return ht;
}

Symbol
lookupLocalTable(Table ht, Void name)
{ register Symbol s = ht->entries[pointerHashValue(name, ht->buckets)];

  for( ; s; s = s->next )
    if ( s->name == (word)name )
      return s;

  return NULL;
}

bool
addLocalTable(Table ht, Void name, Void value)
{ register Symbol s;
  register int v = pointerHashValue(name, ht->buckets);

  if (lookupLocalTable(ht, name) != (Symbol) NULL)
    fail;
  s = (Symbol) allocLocal(sizeof(struct symbol));
  s->name = (word)name;
  s->value = (word)value;
  s->next = ht->entries[v];
  ht->entries[v] = s;

  succeed;
}
