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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file  provides  lock-free  hash-tables.  All  table  operations are
thread-safe and do not involve locking.  Table accesses do not block and
concurrent accesses can overlap, including concurrent writers.

Table resizing is automatic and  occurs when a table is either very full
or if  the number of reprobes  becomes excessive.  While resizing occurs
readers and writers can access the table unhindered.

Table enumerators  exist for  iterating over  a table.  Iterating over a
table  will  not  block  accesses  or  resizes.  Table  enumerators  are
'weakly consistent'  -  all table entries  that existed at  the time the
enumerator  was created  will be seen,  and subsequent  modifications to
entries may be seen.

Internally the table is a closed 2^N table with stride-1 reprobing.

Each table entry  (a key-value pair)  can exist  in any of the following
states:

state ID   key-value    transitions    description
--------------------------------------------------
 1        <NULL,NULL>   2,5            empty entry

 2        <K,NULL>      3              partially inserted entry

 3        <K,V>         4,6            standard inserted entry

 4        <K,T>         6              deleted entry

 5        <S,NULL>                     dead entry
 6        <K,S>                        dead entry; check next map

Transitioning between states is performed using CAS.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#define HTABLE_NORMAL 0x1
#define HTABLE_RESIZE 0x2

#define HTABLE_TOMBSTONE &htable_tombstone
#define HTABLE_SENTINEL &htable_sentinel

static int htable_tombstone = 0;
static int htable_sentinel = 0;



inline void *htable_name(KVS kvs, int idx)
{ return kvs->entries[idx].name;
}

inline void *htable_value(KVS kvs, int idx)
{ return kvs->entries[idx].value;
}

inline int htable_cas_name(KVS kvs, int idx, void *exp, void *name)
{ return COMPARE_AND_SWAP(&kvs->entries[idx].name, exp, name);
}

inline int htable_cas_value(KVS kvs, int idx, void *exp, void *value)
{ return COMPARE_AND_SWAP(&kvs->entries[idx].value, exp, value);
}

inline int htable_cas_new_kvs(KVS kvs, KVS new_kvs)
{ return COMPARE_AND_SWAP(&kvs->next, NULL, new_kvs);
}

inline int htable_cas_cleanup(Table ht, int exp, int cleanup)
{ return COMPARE_AND_SWAP(&ht->cleanup, exp, cleanup);
}


static KVS
htable_alloc_kvs(int len)
{ size_t bytes;
  KVS kvs;

  bytes = sizeof(struct kvs);
  kvs = allocHeapOrHalt(bytes);
  memset(kvs, 0, bytes);

  kvs->len = len;

  bytes = len * sizeof(struct symbol);
  kvs->entries = allocHeapOrHalt(bytes);
  memset(kvs->entries, 0, bytes);

  return kvs;
}


static void
htable_free_kvs(KVS kvs)
{
  DEBUG(MSG_HASH_TABLE_KVS,
        Sdprintf("Freeing KV map: kvs: %p\n", kvs));

  if ( kvs->next )
  { kvs->next->prev = NULL;
  }

  freeHeap(kvs->entries, kvs->len * sizeof(struct symbol));
  freeHeap(kvs, sizeof(struct kvs));
}


static void
htable_free_all_kvs(Table ht)
{
  KVS kvs = ht->kvs;

  while ( kvs->prev )
  { kvs = kvs->prev;
  }

  while ( kvs )
  { KVS next = kvs->next;
    htable_free_kvs(kvs);
    kvs = next;
  }
}


void htable_maybe_free_kvs(Table ht)
{
  KVS kvs;

  if ( !htable_cas_cleanup(ht, FALSE, TRUE) )
  { return;
  }

  kvs = ht->kvs;

  while( kvs->prev )
  { kvs = kvs->prev;
  }

  while ( (!kvs->accesses) &&
          (kvs != ht->kvs) && (kvs != ht->kvs->prev) )
  { KVS next = kvs->next;
    htable_free_kvs(kvs);
    kvs = next;
  }

  htable_cas_cleanup(ht, TRUE, FALSE);
}


static void
htable_copy_kvs(Table ht, KVS old_kvs, KVS new_kvs)
{
  int idx = 0;
  void *n;
  void *v;

  while ( idx < old_kvs->len )
  {
    n = htable_name(old_kvs, idx);
    v = htable_value(old_kvs, idx);

    while ( !(n = htable_name(old_kvs, idx)) )
    { htable_cas_name(old_kvs, idx, NULL, HTABLE_SENTINEL);
      n = HTABLE_SENTINEL;
    }

    if ( n == HTABLE_SENTINEL )
    { idx++;
      continue;
    }

    while ( TRUE )
    {
      if ( v != HTABLE_TOMBSTONE )
      { htable_put(ht, new_kvs, n, v, HTABLE_RESIZE);
      }

      if ( htable_cas_value(old_kvs, idx, v, HTABLE_SENTINEL) )
      { break;
      }

      v = htable_value(old_kvs, idx);
    }

    idx++;
  }
}


static KVS
htable_resize(Table ht, KVS kvs)
{
  KVS new_kvs;
  int new_len = kvs->len;

  if ( ht->size >= (kvs->len >> 2) )
  { new_len = kvs->len << 1;
    if ( ht->size >= (kvs->len >> 1) )
    { new_len = kvs->len << 2;
    }
  }
  else if ( kvs->resizing )
  { new_len = kvs->len << 1;
  }

  new_kvs = kvs->next;
  if ( new_kvs )
  { return new_kvs;
  }

  new_kvs = htable_alloc_kvs(new_len);
  new_kvs->prev = kvs;

  if ( htable_cas_new_kvs(kvs, new_kvs) )
  {
    DEBUG(MSG_HASH_TABLE_KVS,
          Sdprintf("Rehashing table %p to %d entries. kvs: %p -> new_kvs: %p\n", ht, new_len, kvs, new_kvs));

    new_kvs->resizing = TRUE;
    htable_copy_kvs(ht, kvs, new_kvs);
    new_kvs->resizing = FALSE;

    ht->kvs = new_kvs;

    htable_maybe_free_kvs(ht);
  }
  else
  { htable_free_kvs(new_kvs);
    new_kvs = kvs->next;
    assert(new_kvs);
  }

  return new_kvs;
}


void*
htable_get(Table ht, KVS kvs, void *name)
{
  int idx = (int)pointerHashValue(name, kvs->len);
  void *n;
  void *v;
  int reprobe_count = 0;

  assert(name != NULL);

  while ( TRUE )
  {
    n = htable_name(kvs, idx);
    v = htable_value(kvs, idx);

    if ( !n )
      return NULL;

    if ( n == name )
    {
      if ( v == HTABLE_TOMBSTONE )
      { return NULL;
      } else if ( v == HTABLE_SENTINEL )
      { return htable_get(ht, kvs->next, name);
      } else
      { return v;
      }
    }

    if ( (++reprobe_count >= (10 + (kvs->len>>3))) || (n == HTABLE_SENTINEL) )
    { KVS new_kvs = kvs->next;
      if ( new_kvs )
      { return htable_get(ht, new_kvs, name);
      } else
      { return NULL;
      }
    }

    idx = (idx+1)&(kvs->len-1);
  }

  return NULL;
}


void*
htable_put(Table ht, KVS kvs, void *name, void *value, int flags)
{
  int idx = (int)pointerHashValue(name, kvs->len);
  void *n;
  void *v;
  int reprobe_count = 0;

  assert(name != NULL);
  assert(value != NULL);

  while( TRUE )
  {
    n = htable_name(kvs, idx);
    v = htable_value(kvs, idx);

    if ( !n )
    {
      if ( value == HTABLE_TOMBSTONE ) return value;
      if ( htable_cas_name(kvs, idx, NULL, name) )
      { n = name;
        break;
      }
      n = htable_name(kvs, idx);
      assert(n);
    }

    if ( n == name )
    { break;
    }

    if ( (++reprobe_count >= (10 + (kvs->len>>3))) || (n == HTABLE_SENTINEL) )
    { KVS new_kvs = htable_resize(ht, kvs);
      return htable_put(ht, new_kvs, name, value, flags);
    }

    idx = (idx+1)&(kvs->len-1);
  }

  if ( value == v ) return v;

  while( TRUE )
  {
    if ( v == HTABLE_SENTINEL )
    { return htable_put(ht, kvs->next, name, value, flags);
    }

    if ( htable_cas_value(kvs, idx, v, value) )
    { break;
    }

    v = htable_value(kvs, idx);
  }

  if ( flags & HTABLE_NORMAL )
  {
    if ( ((!v) || (v == HTABLE_TOMBSTONE)) && (value != HTABLE_TOMBSTONE) )
    { ATOMIC_INC(&ht->size);
    } else if ( !((!v) || (v == HTABLE_TOMBSTONE)) && (value == HTABLE_TOMBSTONE) )
    { ATOMIC_DEC(&ht->size);
    }
  }

  return v;
}


int
htable_iter(Table ht, KVS kvs, int *index, void **name, void **value)
{
  int idx = *index;
  void *n = NULL;
  void *v = NULL;

  while ( idx < kvs->len )
  {
    n = htable_name(kvs, idx);
    v = htable_value(kvs, idx++);

    if ( (!n) || (n == HTABLE_SENTINEL) )
    { continue;
    }

    if ( v == HTABLE_SENTINEL )
    { v = htable_get(ht, kvs->next, n);
    }

    if ( (v) && (v != HTABLE_TOMBSTONE) )
    { break;
    }
  }

  if ( (n == HTABLE_SENTINEL) || (v == HTABLE_TOMBSTONE) )
  { n = NULL;
    v = NULL;
  }

  *index = idx;

  if ( name )
  { *name = n;
  }
  if ( value )
  { *value = v;
  }

  return (v != NULL);
}



		 /*******************************
		 *             API              *
		 *******************************/

Table
newHTable(int len)
{ Table ht;

  ht		  = allocHeapOrHalt(sizeof(struct table));
  ht->size	  = 0;
  ht->free_symbol = NULL;
  ht->copy_symbol = NULL;

  ht->kvs = htable_alloc_kvs(len);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("newHTable(). ht: %p, len: %d\n", ht, len));
  DEBUG(MSG_HASH_TABLE_KVS,
        Sdprintf("New KV map: ht: %p, kvs: %p len: %d\n", ht, ht->kvs, len));

  return ht;
}


void
destroyHTable(Table ht)
{
  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("destroyHTable(). ht: %p\n", ht));

  clearHTable(ht);
  htable_free_all_kvs(ht);
  freeHeap(ht, sizeof(struct table));
}


void*
lookupHTable(Table ht, void *name)
{ KVS kvs;
  void *v;

  kvs = ht->kvs;
  ATOMIC_INC(&kvs->accesses);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("lookupHTable(). ht: %p, kvs: %p, name: %p\n", ht, kvs, name));

  v = htable_get(ht, kvs, name);
  ATOMIC_DEC(&kvs->accesses);

  return v;
}


void*
addHTable(Table ht, void *name, void *value)
{ KVS kvs;
  void *v;

  kvs = ht->kvs;
  ATOMIC_INC(&kvs->accesses);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("addHTable(). ht: %p, kvs: %p, name: %p, value: %p\n", ht, kvs, name, value));

  v = htable_put(ht, ht->kvs, name, value, HTABLE_NORMAL);
  ATOMIC_DEC(&kvs->accesses);

  return v;
}


int
deleteHTable(Table ht, void *name)
{ KVS kvs;
  void *v;

  kvs = ht->kvs;
  ATOMIC_INC(&kvs->accesses);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("deleteHTable(). ht: %p, kvs: %p, name: %p\n", ht, kvs, name));

  v = htable_put(ht, ht->kvs, name, HTABLE_TOMBSTONE, HTABLE_NORMAL);
  ATOMIC_DEC(&kvs->accesses);

  return (v != NULL);
}


void
clearHTable(Table ht)
{ KVS kvs;
  int idx = 0;
  void *n = NULL;
  void *v = NULL;

  kvs = ht->kvs;
  ATOMIC_INC(&kvs->accesses);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("ClearHTable(). ht: %p, kvs: %p\n", ht, kvs));

  if ( !ht->free_symbol )
  { return;
  }

  while ( idx < kvs->len )
  {
    n = htable_name(kvs, idx);
    v = htable_value(kvs, idx);

    if ( (!n) || (n == HTABLE_SENTINEL) )
    { idx++;
      continue;
    }

    if ( v == HTABLE_SENTINEL )
    { v = htable_get(ht, kvs->next, n);
    }

    if ( (v) && (v != HTABLE_TOMBSTONE) )
    { kvs->entries[idx].value = HTABLE_TOMBSTONE;
      (*ht->free_symbol)(n, v);
      ht->size--;
    }

    idx++;
  }

  ATOMIC_DEC(&kvs->accesses);
}


Table
copyHTable(Table src_ht)
{ Table dest_ht;
  KVS src_kvs, dest_kvs;
  int idx = 0;
  void *n = NULL;
  void *v = NULL;

  src_kvs = src_ht->kvs;
  ATOMIC_INC(&src_kvs->accesses);
  dest_ht = newHTable(src_kvs->len);
  dest_kvs = dest_ht->kvs;

  dest_ht->copy_symbol = src_ht->copy_symbol;
  dest_ht->free_symbol = src_ht->free_symbol;

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("copyHTable(). src_ht: %p, src_kvs: %p, dest_ht: %p, dest_kvs: %p\n",
                 src_ht, src_kvs, dest_ht, dest_kvs));

  while ( idx < src_kvs->len )
  {
    n = htable_name(src_kvs, idx);
    v = htable_value(src_kvs, idx++);

    if ( (!n) || (n == HTABLE_SENTINEL) )
    { continue;
    }

    if ( v == HTABLE_SENTINEL )
    { v = htable_get(src_ht, src_kvs->next, n);
    }

    if ( (v) && (v != HTABLE_TOMBSTONE) )
    {
      if ( src_ht->copy_symbol )
      { (*src_ht->copy_symbol)(n, &v);
      }

      htable_put(dest_ht, dest_kvs, n, v, HTABLE_NORMAL);
    }
  }

  ATOMIC_DEC(&src_kvs->accesses);

  return dest_ht;
}


TableEnum
newTableEnum(Table ht)
{
  TableEnum e = allocHeapOrHalt(sizeof(struct table_enum));
  KVS kvs;

  kvs = ht->kvs;
  ATOMIC_INC(&kvs->accesses);

  DEBUG(MSG_HASH_TABLE_ENUM,
        Sdprintf("newTableEnum(). e: %p, ht: %p, kvs: %p\n",
                 e, e->table, e->kvs));

  e->table = ht;
  e->kvs = kvs;
  e->idx   = 0;

  return e;
}


void
freeTableEnum(TableEnum e)
{
  if ( !e )
  { return;
  }

  DEBUG(MSG_HASH_TABLE_ENUM,
        Sdprintf("freeTableEnum(). e: %p, ht: %p, kvs: %p\n",
                 e, e->table, e->kvs));

  ATOMIC_DEC(&e->kvs->accesses);

  htable_maybe_free_kvs(e->table);

  freeHeap(e, sizeof(*e));
}


int
advanceTableEnum(TableEnum e, void **name, void **value)
{
  DEBUG(MSG_HASH_TABLE_ENUM,
        Sdprintf("advanceTableEnum(). e: %p, ht: %p, kvs: %p, idx: %d\n",
                 e, e->table, e->kvs, e->idx));

  return htable_iter(e->table, e->kvs, &e->idx, name, value);
}
