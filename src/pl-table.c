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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
allocHTableEntries(Table ht)
{ int n;
  Symbol *p;

  ht->entries = allocHeap(ht->buckets * sizeof(Symbol));

  for(n=0, p = &ht->entries[0]; n < ht->buckets-1; n++, p++)
    *p = makeTableRef(p+1);
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
exitTables(int status, void *arg)
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
{ Symbol s = ht->entries[pointerHashValue(name, ht->buckets)];

  DEBUG(0, lookups++);
  for(;s && !isTableRef(s); s = s->next)
  { DEBUG(0, cmps++);
    if (s->name == (word)name)
    { DEBUG(9, Sdprintf("lookupHTable(0x%x, 0x%x --> 0x%x\n",
			ht, name, s->value));
      return s;
    }
  }

  DEBUG(9, Sdprintf("lookupHTable(0x%x, 0x%x --> FAIL\n", ht, name));
  return (Symbol) NULL;
}


static void
rehashHTable(Table ht)
{ Symbol *oldtab  = ht->entries;
  int    oldbucks = ht->buckets;
  Symbol s, n;
  int done = 0;
  int i = 0;

  startCritical;
  ht->buckets *= 2;
  allocHTableEntries(ht);

  DEBUG(1, Sdprintf("Rehashing table 0x%x to %d entries\n",
		    ht, ht->buckets));

  for(s = oldtab[0]; s; s = n)
  { int v;

    while( isTableRef(s) )
    { s = unTableRef(Symbol, s);
      assert(s == oldtab[++i]);
      if ( !s )
	goto out;
    }
    done++;
    n = s->next;
    v = pointerHashValue(s->name, ht->buckets);
    s->next = ht->entries[v];
    ht->entries[v] = s;
  }

out:
  assert(done == ht->size);
  freeHeap(oldtab, oldbucks * sizeof(Symbol));
  endCritical;
}


bool
addHTable(Table ht, Void name, Void value)
{ Symbol s;
  int v = pointerHashValue(name, ht->buckets);

  if (lookupHTable(ht, name) != (Symbol) NULL)
    fail;
  s = (Symbol) allocHeap(sizeof(struct symbol));
  s->name = (word)name;
  s->value = (word)value;
  s->next = ht->entries[v];
  ht->entries[v] = s;
  ht->size++;
  DEBUG(9, Sdprintf("addHTable(0x%x, 0x%x, 0x%x) --> size = %d\n",
		    ht, name, value, ht->size));

  if ( ht->buckets * 2 < ht->size && !ht->locked )
    rehashHTable(ht);

  succeed;
}  


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: s must be in the table!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
deleteSymbolHTable(Table ht, Symbol s)
{ int v = pointerHashValue(s->name, ht->buckets);
  Symbol *h = &ht->entries[v];

  for( ; *h != s; h = &(*h)->next )
  { if ( *h == s )
    { *h = (*h)->next;

      freeHeap(s, sizeof(struct symbol));
      ht->size--;

      return;
    }
  }
}


Symbol
nextHTable(Table ht, Symbol s)
{ s = s->next;
  while(s != (Symbol) NULL && isTableRef(s) )
    s = unTableRef(Symbol, s);

  return s;
}

Symbol
firstHTable(Table ht)
{ Symbol s = ht->entries[0];

  while(s != (Symbol) NULL && isTableRef(s) )
    s = unTableRef(Symbol, s);

  return s;
}  

void
clearHTable(Table ht)
{ int n;
  Symbol s;

  for(n=0; n < ht->buckets; n++)
  { s = ht->entries[n];
    while(s && !isTableRef(s))
    { Symbol q = s->next;
      freeHeap(s, sizeof(struct symbol));
      s = q;
    }
    ht->entries[n] = s;
  }

  ht->size = 0;
}

