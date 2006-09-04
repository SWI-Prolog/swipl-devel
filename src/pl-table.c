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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#ifdef O_PLMT
#define LOCK_TABLE(t)   if ( t->mutex ) simpleMutexLock(t->mutex)
#define UNLOCK_TABLE(t)	if ( t->mutex ) simpleMutexUnlock(t->mutex)
#else
#define LOCK_TABLE(t) (void)0
#define UNLOCK_TABLE(t) (void)0
#endif

static inline Symbol rawAdvanceTableEnum(TableEnum e);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file provides generic hash-tables. Most   of  the implementation is
rather  straightforward.  Special  are  the  *TableEnum()  functions  to
create, advance over and destroy enumerator   objects. These objects are
used to enumerate the symbols of these   tables,  used primarily for the
pl_current_* predicates.

The  enumerators  cause  two  things:  (1)    as  long  enumerators  are
associated, the table will not  be  rehashed   and  (2)  if  symbols are
deleted  that  are  referenced  by  an  enumerator,  the  enumerator  is
automatically advanced to the next free symbol.  This, in general, makes
the enumeration of hash-tables safe.

TODO: abort should delete  any  pending   enumerators.  This  should  be
thread-local, as thread_exit/1 should do the same.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
allocHTableEntries(Table ht)
{ int n;
  Symbol *p;

  ht->entries = allocHeap(ht->buckets * sizeof(Symbol));

  for(n=0, p = &ht->entries[0]; n < ht->buckets; n++, p++)
    *p = NULL;
}


Table
newHTable(int buckets)
{ Table ht;

  ht		  = allocHeap(sizeof(struct table));
  ht->buckets	  = (buckets & ~TABLE_MASK);
  ht->size	  = 0;
  ht->enumerators = NULL;
  ht->free_symbol = NULL;
  ht->copy_symbol = NULL;
#ifdef O_PLMT
  if ( (buckets & TABLE_UNLOCKED) )
    ht->mutex = NULL;
  else
  { ht->mutex     = allocHeap(sizeof(simpleMutex));
    simpleMutexInit(ht->mutex);
  }
#endif

  allocHTableEntries(ht);
  return ht;
}


void
destroyHTable(Table ht)
{
#ifdef O_PLMT
  if ( ht->mutex )
  { simpleMutexDelete(ht->mutex);
    freeHeap(ht->mutex, sizeof(*ht->mutex));
	ht->mutex = NULL;
  }
#endif

  clearHTable(ht);
  freeHeap(ht->entries, ht->buckets * sizeof(Symbol));
  freeHeap(ht, sizeof(struct table));
}


#if O_DEBUG || O_HASHSTAT
#define HASHSTAT(c) c
static int lookups;
static int cmps;

void
exitTables(int status, void *arg)
{ Sdprintf("hashstat: Anonymous tables: %d lookups using %d compares\n",
	   lookups, cmps);
}
#else
#define HASHSTAT(c)
#endif /*O_DEBUG*/


void
initTables()
{ static int done = FALSE;

  if ( !done )
  { done = TRUE;
    
    HASHSTAT(PL_on_halt(exitTables, NULL));
  }
}


Symbol
lookupHTable(Table ht, void *name)
{ Symbol s = ht->entries[pointerHashValue(name, ht->buckets)];

  HASHSTAT(lookups++);
  for( ; s; s = s->next)
  { HASHSTAT(cmps++);
    if ( s->name == name )
      return s;
  }

  return NULL;
}

#ifdef O_DEBUG
void
checkHTable(Table ht)
{ int i;
  int n = 0;

  for(i=0; i<ht->buckets; i++)
  { Symbol s;

    for(s=ht->entries[i]; s; s=s->next)
    { assert(lookupHTable(ht, s->name) == s);
      n++;
    }
  }

  assert(n == ht->size);
}
#endif

/* MT: Locked by calling addHTable()
*/

static void
rehashHTable(Table ht)
{ Symbol *oldtab;
  int    oldbucks;
  int    i;

  oldtab   = ht->entries;
  oldbucks = ht->buckets;
  ht->buckets *= 2;
  allocHTableEntries(ht);

  DEBUG(1, Sdprintf("Rehashing table %p to %d entries\n", ht, ht->buckets));

  for(i=0; i<oldbucks; i++)
  { Symbol s, n;

    for(s=oldtab[i]; s; s = n)
    { int v = pointerHashValue(s->name, ht->buckets);

      n = s->next;
      s->next = ht->entries[v];
      ht->entries[v] = s;
    }
  }

  freeHeap(oldtab, oldbucks * sizeof(Symbol));
  DEBUG(0, checkHTable(ht));
}


Symbol
addHTable(Table ht, void *name, void *value)
{ Symbol s;
  int v;

  LOCK_TABLE(ht);
  v = pointerHashValue(name, ht->buckets);
  if ( lookupHTable(ht, name) )
  { UNLOCK_TABLE(ht);
    return NULL;
  }
  s = allocHeap(sizeof(struct symbol));
  s->name  = name;
  s->value = value;
  s->next  = ht->entries[v];
  ht->entries[v] = s;
  ht->size++;
  DEBUG(9, Sdprintf("addHTable(0x%x, 0x%x, 0x%x) --> size = %d\n",
		    ht, name, value, ht->size));

  if ( ht->buckets * 2 < ht->size && !ht->enumerators )
    rehashHTable(ht);
  UNLOCK_TABLE(ht);

  DEBUG(1, checkHTable(ht));
  return s;
}  


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: s must be in the table!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
deleteSymbolHTable(Table ht, Symbol s)
{ int v;
  Symbol *h;
  TableEnum e;

  LOCK_TABLE(ht);
  v = pointerHashValue(s->name, ht->buckets);
  h = &ht->entries[v];

  for( e=ht->enumerators; e; e = e->next )
  { if ( e->current == s )
      rawAdvanceTableEnum(e);
  }

  for( ; *h; h = &(*h)->next )
  { if ( *h == s )
    { *h = (*h)->next;

      freeHeap(s, sizeof(struct symbol));
      ht->size--;

      break;
    }
  }

  UNLOCK_TABLE(ht);
}


void
clearHTable(Table ht)
{ int n;
  TableEnum e;

  LOCK_TABLE(ht);
  for( e=ht->enumerators; e; e = e->next )
  { e->current = NULL;
    e->key     = ht->buckets;
  }

  for(n=0; n < ht->buckets; n++)
  { Symbol s, q;

    for(s = ht->entries[n]; s; s = q)
    { q = s->next;

      if ( ht->free_symbol )
	(*ht->free_symbol)(s);

      freeHeap(s, sizeof(struct symbol));
    }

    ht->entries[n] = NULL;
  }

  ht->size = 0;
  UNLOCK_TABLE(ht);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Table copyHTable(Table org)
    Make a copy of a hash-table.  This is used to realise the copy-on-write
    as defined by SharedTable.  The table is copied to have exactly the
    same dimensions as the original.  If the copy_symbol function is
    provided, it is called to allow duplicating the symbols name or value
    fields.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Table
copyHTable(Table org)
{ Table ht;
  int n;

  ht = allocHeap(sizeof(struct table));
  LOCK_TABLE(org);
  *ht = *org;				/* copy all attributes */
#ifdef O_PLMT
  ht->mutex = NULL;
#endif
  allocHTableEntries(ht);

  for(n=0; n < ht->buckets; n++)
  { Symbol s, *q;

    q = &ht->entries[n];
    for(s = org->entries[n]; s; s = s->next)
    { Symbol s2 = allocHeap(sizeof(*s2));

      *q = s2;
      q = &s2->next;
      s2->name = s->name;
      s2->value = s->value;

      if ( ht->copy_symbol )
	(*ht->copy_symbol)(s2);
    }
    *q = NULL;
  }
#ifdef O_PLMT  
  if ( org->mutex )
  { ht->mutex = allocHeap(sizeof(simpleMutex));
    simpleMutexInit(ht->mutex);
  }
#endif
  UNLOCK_TABLE(org);

  return ht;
}


		 /*******************************
		 *	    ENUMERATING		*
		 *******************************/

TableEnum
newTableEnum(Table ht)
{ TableEnum e = allocHeap(sizeof(struct table_enum));
  Symbol n;

  LOCK_TABLE(ht);
  e->table	  = ht;
  e->key	  = 0;
  e->next	  = ht->enumerators;
  ht->enumerators = e;

  n = ht->entries[0];
  while(!n && ++e->key < ht->buckets)
    n=ht->entries[e->key];
  e->current = n;
  UNLOCK_TABLE(ht);

  return e;
}


void
freeTableEnum(TableEnum e)
{ TableEnum *ep;
  Table ht;

  if ( !e )
    return;

  ht = e->table;
  LOCK_TABLE(ht);
  for( ep=&ht->enumerators; *ep ; ep = &(*ep)->next )
  { if ( *ep == e )
    { *ep = (*ep)->next;

      freeHeap(e, sizeof(*e));
      break;
    }
  }
  UNLOCK_TABLE(ht);
}


static inline Symbol
rawAdvanceTableEnum(TableEnum e)
{ Symbol s, n;
  Table ht = e->table;

  if ( !(s = e->current) )
    return s;
  n = s->next;

  while(!n)
  { if ( ++e->key >= ht->buckets )
    { e->current = NULL;
      return s;
    }

    n=ht->entries[e->key];
  }
  e->current = n;

  return s;
}


Symbol
advanceTableEnum(TableEnum e)
{ Symbol s;
#ifdef O_PLMT
  Table ht = e->table;
#endif

  LOCK_TABLE(ht);
  s = rawAdvanceTableEnum(e);
  UNLOCK_TABLE(ht);

  return s;
}
