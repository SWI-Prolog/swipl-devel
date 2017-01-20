/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2016, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include "pl-incl.h"
#include "pl-cstack.h"


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

Only threads  which have  an attached Prolog engine  can access the hash
tables.  This is due to  hash table internal book-keeping  requiring use
of the thread's local state.

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


#ifdef O_PLMT

#define acquire_kvs(t, k) \
  { LD->thread.info->access.kvs = t->kvs; \
    k = LD->thread.info->access.kvs; \
  }

#define release_kvs() \
  { LD->thread.info->access.kvs = NULL; \
  }

#else

#define acquire_kvs(t, k) \
  { k = t->kvs; \
  }

#define release_kvs() (void)0

#endif

static void *	htable_put(Table ht, KVS kvs, void *name, void *value, int flags);


#define HTABLE_NORMAL   0x1
#define HTABLE_RESIZE   0x2
#define HTABLE_PRESERVE 0x4

#define HTABLE_TOMBSTONE ((void*)-1)
#define HTABLE_SENTINEL  ((void*)-2)


static inline void *htable_name(KVS kvs, int idx)
{ return kvs->entries[idx].name;
}

static inline void *htable_value(KVS kvs, int idx)
{ return kvs->entries[idx].value;
}

static inline int htable_cas_name(KVS kvs, int idx, void *exp, void *name)
{ return COMPARE_AND_SWAP(&kvs->entries[idx].name, exp, name);
}

static inline int htable_cas_value(KVS kvs, int idx, void *exp, void *value)
{ return COMPARE_AND_SWAP(&kvs->entries[idx].value, exp, value);
}

static inline int htable_cas_new_kvs(KVS kvs, KVS new_kvs)
{ return COMPARE_AND_SWAP(&kvs->next, NULL, new_kvs);
}

static inline int htable_cas_cleanup(Table ht, int exp, int cleanup)
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


static void
htable_maybe_free_kvs(Table ht)
{ KVS kvs;

  if ( !htable_cas_cleanup(ht, FALSE, TRUE) )
  { return;
  }

  kvs = ht->kvs;

  while( kvs->prev )
  { kvs = kvs->prev;
  }

  while ( (!kvs->accesses) && (!pl_kvs_in_use(kvs)) &&
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
      if ( v && v != HTABLE_TOMBSTONE )
      { htable_put(ht, new_kvs, n, v, HTABLE_RESIZE);
      }

      if ( v && htable_cas_value(old_kvs, idx, v, HTABLE_SENTINEL) )
      { break;
      }

      v = htable_value(old_kvs, idx);

      if ( v == HTABLE_TOMBSTONE )
      { htable_put(ht, new_kvs, n, v, HTABLE_RESIZE);
      }
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


static void*
htable_get(Table ht, KVS kvs, void *name)
{
  void *n;
  void *v;
  int idx, reprobe_count;

  assert(name != NULL);

redo:

  idx = (int)pointerHashValue(name, kvs->len);
  reprobe_count = 0;

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
      { kvs = kvs->next;
        goto redo;
      } else
      { return v;
      }
    }

    if ( (++reprobe_count >= (10 + (kvs->len>>2))) || (n == HTABLE_SENTINEL) )
    { kvs = kvs->next;
      if ( kvs )
      { goto redo;
      } else
      { return NULL;
      }
    }

    idx = (idx+1)&(kvs->len-1);
  }

  return NULL;
}


static void*
htable_put(Table ht, KVS kvs, void *name, void *value, int flags)
{
  void *n;
  void *v;
  int idx, reprobe_count;

  assert(name != NULL);
  assert(value != NULL);

redo:

  idx = (int)pointerHashValue(name, kvs->len);
  reprobe_count = 0;

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

    if ( (++reprobe_count >= (10 + (kvs->len>>2))) || (n == HTABLE_SENTINEL) )
    { kvs = htable_resize(ht, kvs);
      goto redo;
    }

    idx = (idx+1)&(kvs->len-1);
  }

  if ( value == v ) return v;

  while( TRUE )
  {
    if ( v == HTABLE_SENTINEL )
    { kvs = kvs->next;
      goto redo;
    }

    if ( v && (v != HTABLE_TOMBSTONE) && (flags & HTABLE_PRESERVE) )
    { return v;
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

  return (value == HTABLE_TOMBSTONE ? v : value);
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
  ht->cleanup     = FALSE;
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
lookupHTable__LD(Table ht, void *name ARG_LD)
{ KVS kvs;
  void *v;

  acquire_kvs(ht, kvs);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("lookupHTable(). ht: %p, kvs: %p, name: %p\n", ht, kvs, name));

  v = htable_get(ht, kvs, name);
  release_kvs();

  return v;
}


/* returns value associated with name after adding to the table, i.e.
   `value` if `name` was not in the table and the existing association
   for `name` if it was.
 */
void*
addHTable(Table ht, void *name, void *value)
{ GET_LD
  KVS kvs;
  void *v;

  acquire_kvs(ht, kvs);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("addHTable(). ht: %p, kvs: %p, name: %p, value: %p\n", ht, kvs, name, value));

  v = htable_put(ht, kvs, name, value, HTABLE_NORMAL|HTABLE_PRESERVE);
  release_kvs();

  return v;
}


void
addNewHTable(Table ht, void *name, void *value)
{ void *new = addHTable(ht, name, value);
  if ( new != value )
  { Sdprintf("WARNING: Race condition detected.  Please report at:\n"
	     "WARNING:   https://github.com/SWI-Prolog/swipl-devel/issues\n");
    save_backtrace("addNewHTable");
    print_backtrace_named("addNewHTable");
    updateHTable(ht, name, value);
  }
}


void*
updateHTable(Table ht, void *name, void *value)
{ GET_LD
  KVS kvs;
  void *v;

  acquire_kvs(ht, kvs);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("updateHTable(). ht: %p, kvs: %p, name: %p, value: %p\n", ht, kvs, name, value));

  v = htable_put(ht, kvs, name, value, HTABLE_NORMAL);
  release_kvs();

  return v;
}


void*
deleteHTable(Table ht, void *name)
{ GET_LD
  KVS kvs;
  void *v;

  acquire_kvs(ht, kvs);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("deleteHTable(). ht: %p, kvs: %p, name: %p\n", ht, kvs, name));

  v = htable_put(ht, kvs, name, HTABLE_TOMBSTONE, HTABLE_NORMAL);
  release_kvs();

  return (v == HTABLE_TOMBSTONE ? NULL : v);
}


void
clearHTable(Table ht)
{ GET_LD
  KVS kvs;
  int idx = 0;
  void *n = NULL;
  void *v = NULL;

  acquire_kvs(ht, kvs);

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

  release_kvs();
}


Table
copyHTable(Table src_ht)
{ GET_LD
  Table dest_ht;
  KVS src_kvs, dest_kvs;
  int idx = 0;
  void *n = NULL;
  void *v = NULL;

  acquire_kvs(src_ht, src_kvs);
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

  release_kvs();

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


size_t
sizeofTable(Table ht)				/* memory usage in bytes */
{ return ( sizeof(struct table) +
	   sizeof(struct kvs) +
	   ht->kvs->len * sizeof(struct symbol) );
}
