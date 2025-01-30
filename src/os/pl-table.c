/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

static table_value_t htable_put(Table ht, KVS kvs, table_key_t name, table_value_t value, int flags);


#define HTABLE_NORMAL   0x1
#define HTABLE_RESIZE   0x2
#define HTABLE_PRESERVE 0x4

#define HTABLE_TOMBSTONE ((table_value_t)-1)
#define HTABLE_SENTINEL  ((table_key_t)-2)


static inline table_key_t htable_name(KVS kvs, size_t idx)
{ return kvs->entries[idx].name;
}

static inline table_value_t htable_value(KVS kvs, size_t idx)
{ return kvs->entries[idx].value;
}

static inline int htable_cas_name(KVS kvs, size_t idx, table_key_t exp, table_key_t name)
{ return COMPARE_AND_SWAP_UINT64(&kvs->entries[idx].name, exp, name);
}

static inline int htable_cas_value(KVS kvs, size_t idx, table_key_t exp, table_key_t value)
{ return COMPARE_AND_SWAP_UINT64(&kvs->entries[idx].value, exp, value);
}

static inline int htable_cas_new_kvs(KVS kvs, KVS new_kvs)
{ return COMPARE_AND_SWAP_PTR(&kvs->next, NULL, new_kvs);
}

static inline int htable_cas_cleanup(Table ht, int exp, int cleanup)
{ return COMPARE_AND_SWAP_INT(&ht->cleanup, exp, cleanup);
}


static KVS
htable_alloc_kvs(size_t len)
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

  if ( !htable_cas_cleanup(ht, false, true) )
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

  htable_cas_cleanup(ht, true, false);
}


static void
htable_copy_kvs(Table ht, KVS old_kvs, KVS new_kvs)
{
  size_t idx = 0;
  table_key_t n;
  table_value_t v;

  while ( idx < old_kvs->len )
  {
    n = htable_name(old_kvs, idx);
    v = htable_value(old_kvs, idx);

    while ( !(n = htable_name(old_kvs, idx)) )
    { htable_cas_name(old_kvs, idx, NULL_KEY, HTABLE_SENTINEL);
      n = HTABLE_SENTINEL;
    }

    if ( n == HTABLE_SENTINEL )
    { idx++;
      continue;
    }

    while ( true )
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
  size_t new_len = kvs->len;

  if ( ht->size >= (kvs->len >> 2) )
  { new_len = kvs->len << 1;
    if ( ht->size >= (kvs->len >> 1) )
    { new_len = kvs->len << 2;
    }
  } else if ( kvs->resizing )
  { new_len = kvs->len << 1;
  }

  new_kvs = kvs->next;
  if ( new_kvs )
  { return new_kvs;
  }

  new_kvs = htable_alloc_kvs(new_len);
  new_kvs->prev = kvs;

  if ( htable_cas_new_kvs(kvs, new_kvs) )
  { DEBUG(MSG_HASH_TABLE_KVS,
          Sdprintf("Rehashing table %p to %d entries. kvs: %p -> new_kvs: %p\n", ht, new_len, kvs, new_kvs));

    new_kvs->resizing = true;
    htable_copy_kvs(ht, kvs, new_kvs);
    new_kvs->resizing = false;

    ht->kvs = new_kvs;

    htable_maybe_free_kvs(ht);
  } else
  { htable_free_kvs(new_kvs);
    new_kvs = kvs->next;
    assert(new_kvs);
  }

  return new_kvs;
}


static table_value_t
htable_get(Table ht, KVS kvs, table_key_t name)
{ table_key_t n;
  table_value_t v;
  size_t idx;
  int reprobe_count;

  assert(name);

redo:
  idx = (size_t)pointerHashValue(name, kvs->len);
  reprobe_count = 0;

  while ( true )
  { n = htable_name(kvs, idx);
    v = htable_value(kvs, idx);

    if ( !n )
      return NULL_VALUE;

    if ( n == name )
    { if ( v == HTABLE_TOMBSTONE )
      { return NULL_VALUE;
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
      { return NULL_VALUE;
      }
    }

    idx = (idx+1)&(kvs->len-1);
  }
}


static table_value_t
htable_put(Table ht, KVS kvs, table_key_t name, table_value_t value, int flags)
{ table_key_t n;
  table_value_t v;
  size_t idx;
  int reprobe_count;

  assert(name);
  assert(value);

redo:
  idx = (size_t)pointerHashValue(name, kvs->len);
  reprobe_count = 0;

  while( true )
  { n = htable_name(kvs, idx);
    v = htable_value(kvs, idx);

    if ( !n )
    { if ( value == HTABLE_TOMBSTONE ) return value;
      if ( htable_cas_name(kvs, idx, NULL_KEY, name) )
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

  while( true )
  { if ( v == HTABLE_SENTINEL )
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
  { if ( ((!v) || (v == HTABLE_TOMBSTONE)) && (value != HTABLE_TOMBSTONE) )
    { ATOMIC_INC(&ht->size);
    } else if ( !((!v) || (v == HTABLE_TOMBSTONE)) && (value == HTABLE_TOMBSTONE) )
    { ATOMIC_DEC(&ht->size);
    }
  }

  return (value == HTABLE_TOMBSTONE ? v : value);
}


int
htable_iter(Table ht, KVS kvs, size_t *index, table_key_t *name, table_value_t *value)
{ size_t idx = *index;
  table_key_t n = NULL_KEY;
  table_value_t v = NULL_VALUE;

  while ( idx < kvs->len )
  { n = htable_name(kvs, idx);
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
  { n = NULL_KEY;
    v = NULL_VALUE;
  }

  *index = idx;

  if ( name )
  { *name = n;
  }
  if ( value )
  { *value = v;
  }

  return !!v;
}



		 /*******************************
		 *             API              *
		 *******************************/

Table
newHTable(size_t len)
{ Table ht;

  ht		  = allocHeapOrHalt(sizeof(struct table));
  ht->size	  = 0;
  ht->cleanup     = false;
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
{ DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("destroyHTable(). ht: %p\n", ht));

  clearHTable(ht);
  htable_free_all_kvs(ht);
  freeHeap(ht, sizeof(struct table));
}


table_value_t
lookupHTable(DECL_LD Table ht, table_key_t name)
{ KVS kvs;
  table_value_t v;

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
table_value_t
addHTable(DECL_LD Table ht, table_key_t name, table_value_t value)
{ KVS kvs;
  table_value_t v;

  acquire_kvs(ht, kvs);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("addHTable(). ht: %p, kvs: %p, name: %p, value: %p\n", ht, kvs, name, value));

  v = htable_put(ht, kvs, name, value, HTABLE_NORMAL|HTABLE_PRESERVE);
  release_kvs();

  return v;
}


table_value_t
addNewHTable(DECL_LD Table ht, table_key_t name, table_value_t value)
{ table_value_t new = addHTable(ht, name, value);
  if ( new == value )
  { return value;
  } else
  { Sdprintf("WARNING: Race condition detected.  Please report at:\n"
	     "WARNING:   https://github.com/SWI-Prolog/swipl-devel/issues\n");
    save_backtrace("addNewHTable");
    print_backtrace_named("addNewHTable");
    return updateHTable(ht, name, value);
  }
}


table_value_t
updateHTable(DECL_LD Table ht, table_key_t name, table_value_t value)
{ KVS kvs;
  table_value_t v;

  acquire_kvs(ht, kvs);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("updateHTable(). ht: %p, kvs: %p, name: %p, value: %p\n",
		 ht, kvs, name, value));

  v = htable_put(ht, kvs, name, value, HTABLE_NORMAL);
  release_kvs();

  return v;
}


table_value_t
deleteHTable(DECL_LD Table ht, table_key_t name)
{ KVS kvs;
  table_value_t v;

  acquire_kvs(ht, kvs);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("deleteHTable(). ht: %p, kvs: %p, name: %p\n", ht, kvs, name));

  v = htable_put(ht, kvs, name, HTABLE_TOMBSTONE, HTABLE_NORMAL);
  release_kvs();

  return (v == HTABLE_TOMBSTONE ? NULL_VALUE : v);
}


void
clearHTable(Table ht)
{
#if O_PLMT
  GET_LD
#endif
  KVS kvs;
  size_t idx = 0;
  table_key_t n = NULL_KEY;
  table_value_t v = NULL_VALUE;

  acquire_kvs(ht, kvs);

  DEBUG(MSG_HASH_TABLE_API,
        Sdprintf("ClearHTable(). ht: %p, kvs: %p\n", ht, kvs));

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
      if ( ht->free_symbol )
      { (*ht->free_symbol)(n, v);
      }
      ht->size--;
    }

    idx++;
  }

  release_kvs();
}


Table
copyHTable(Table src_ht)
{
#if O_PLMT
  GET_LD
#endif
  Table dest_ht;
  KVS src_kvs, dest_kvs;
  int idx = 0;
  table_key_t n = NULL_KEY;
  table_value_t v = NULL_VALUE;

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
{ TableEnum e = allocHeapOrHalt(sizeof(struct table_enum));
  KVS kvs;

  kvs = ht->kvs;
  ATOMIC_INC(&kvs->accesses);

  DEBUG(MSG_HASH_TABLE_ENUM,
        Sdprintf("newTableEnum(). e: %p, ht: %p, kvs: %p\n",
                 e, e->table, e->kvs));

  e->table = ht;
  e->kvs   = kvs;
  e->idx   = 0;

  return e;
}


void
freeTableEnum(TableEnum e)
{ if ( e )
  { DEBUG(MSG_HASH_TABLE_ENUM,
	  Sdprintf("freeTableEnum(). e: %p, ht: %p, kvs: %p\n",
		   e, e->table, e->kvs));

    ATOMIC_DEC(&e->kvs->accesses);
    htable_maybe_free_kvs(e->table);

    freeHeap(e, sizeof(*e));
  }
}


int
advanceTableEnum(TableEnum e, table_key_t *name, table_value_t *value)
{ DEBUG(MSG_HASH_TABLE_ENUM,
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


		 /*******************************
		 *	    PUBLIC API		*
		 *******************************/

#ifdef O_PLMT
#define NEED_LD  GET_LD if ( !LD ) return false;
#else
#define NEED_LD (void)0;
#endif

hash_table_t
PL_new_hash_table(size_t size, void (*free_symbol)(table_key_t n, table_value_t v))
{ NEED_LD
  hash_table_t ht = newHTable(size);

  if ( ht )
    ht->free_symbol = free_symbol;

  return ht;
}

int
PL_free_hash_table(hash_table_t table)
{ NEED_LD

  destroyHTable(table);
  return true;
}

table_value_t
PL_lookup_hash_table(hash_table_t table, table_key_t key)
{ NEED_LD

  return lookupHTable(table, key);
}

table_value_t
PL_add_hash_table(hash_table_t table, table_key_t key, table_value_t value, int flags)
{ NEED_LD

  if ( !(flags&(PL_HT_NEW|PL_HT_UPDATE)) )
    return addHTable(table, key, value);
  else if ( flags&PL_HT_NEW )
    return addNewHTable(table, key, value);
  else
    return updateHTable(table, key, value);
}

table_value_t
PL_del_hash_table(hash_table_t table, table_key_t key)
{ NEED_LD

  return deleteHTable(table, key);
}

int
PL_clear_hash_table(hash_table_t table)
{ NEED_LD

  clearHTable(table);
  return true;
}

hash_table_enum_t
PL_new_hash_table_enum(hash_table_t table)
{ NEED_LD

  return newTableEnum(table);
}

void
PL_free_hash_table_enum(hash_table_enum_t e)
{ freeTableEnum(e);
}

int
PL_advance_hash_table_enum(hash_table_enum_t e, table_key_t *key, table_value_t *value)
{ return advanceTableEnum(e, key, value);
}
