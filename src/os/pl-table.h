/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Keri Harris
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

#ifndef TABLE_H_INCLUDED
#define TABLE_H_INCLUDED

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog's thread-safe  and lock-free  hash tables.   We had  only a
single  table,  mapping a  pointer  into  another pointer.   With  the
planned move to  64 bit Prolog data on 32-bit  systems, this no longer
works.  Now the  actual table implementation is `word`  -> `word`.  To
make this work, we implement 4 table types:

  - TableWW (word -> word)
  - TablePP (ptr -> ptr)
  - TableWP (word -> ptr)
  - TablePW (ptr -> word)

Then we use  a set of inline functions  to map all of this  to the one
actual implementation.  There  is still a lot of casting  as the table
enumerators, copy and free hooks and FOR_TABLE() pass the raw uint64_t
values.  Eventually  we may decide to  generate a pure TablePP  for 32
bit systems  as that saves  a lot  of memory, notably  for incremental
tabling.  This can be achieved  by compiling pl-table.c multiple times
using different type declarations.  C++ template programming for C :)

For now, we need a lot less  casting and we get more warnings from the
compiler on invalid use of the table functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct table *		Table;		/* (numeric) hash table */
typedef Table TableWW;				/* word -> word tables */
typedef struct table_pp *       TablePP;	/* Pointer -> Pointer */
typedef struct table_wp *       TableWP;	/* word -> Pointer */
typedef struct table_pw *       TablePW;	/* Pointer -> word */

typedef struct kvs *		KVS;		/* map of key-values */
typedef struct symbol *		Symbol;		/* symbol of hash table */
typedef struct table_enum *	TableEnum;	/* Enumerate table entries */
typedef uint64_t table_key_t;
typedef uint64_t table_value_t;

#define NULL_KEY   ((table_key_t)0)
#define NULL_VALUE ((table_value_t)0)

#define TABLE_STRUCT(name)						\
  struct name								\
  { int		size;		/* # symbols in the table */		\
    int		cleanup;	/* true when KVS cleanup in progress */ \
    void	(*copy_symbol)(table_key_t n, table_value_t *v);	\
    void	(*free_symbol)(table_key_t n, table_value_t v);		\
    KVS		kvs;		/* map of key-value pairs */		\
  }

TABLE_STRUCT(table);		/* word -> word */
TABLE_STRUCT(table_pp);		/* ptr -> ptr */
TABLE_STRUCT(table_wp);		/* word -> ptr */
TABLE_STRUCT(table_pw);		/* ptr -> word */

struct kvs
{ size_t len;			/* size of key-value map */
  int resizing;			/* true while resizing */
  int accesses;			/* number of accesses */
  KVS next;			/* next map */
  KVS prev;			/* last map */
  Symbol entries;		/* array of hash symbols */
};

struct symbol
{ table_key_t	name;		/* name entry of symbol */
  table_value_t	value;		/* associated value with name */
};

struct table_enum
{ Table		table;		/* Table we are working on */
  KVS		kvs;		/* kvs we are iterating over */
  size_t	idx;		/* Index of current symbol-chain */
};

#if USE_LD_MACROS
#define	lookupHTable(ht, name)		LDFUNC(lookupHTable, ht, name)
#define	addHTable(ht, name, value)	LDFUNC(addHTable, ht, name, value)
#define	addNewHTable(ht, name, value)	LDFUNC(addNewHTable, ht, name, value)
#define	updateHTable(ht, name, value)	LDFUNC(updateHTable, ht, name, value)
#define	deleteHTable(ht, name)		LDFUNC(deleteHTable, ht, name)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		initTables(void);
Table		newHTable(size_t size);
void		destroyHTable(Table ht);
table_value_t	lookupHTable(Table ht, table_key_t name);
table_value_t	addHTable(Table ht, table_key_t name, table_value_t value);
table_value_t	addNewHTable(Table ht, table_key_t name, table_value_t value);
table_value_t	updateHTable(Table ht, table_key_t name, table_value_t value);
table_value_t	deleteHTable(Table ht, table_key_t name);
void		clearHTable(Table ht);
Table		copyHTable(Table org);
TableEnum	newTableEnum(Table ht);
void		freeTableEnum(TableEnum e);
int		advanceTableEnum(TableEnum e, table_key_t *name, table_value_t *value);
int		htable_iter(Table ht, KVS kvs, size_t *idx,
			    table_key_t *name, table_value_t *value);
size_t		sizeofTable(Table ht);

#undef LDFUNC_DECLARATIONS

#define ptr2key(ptr) ((table_key_t)(uintptr_t)(ptr))
#define ptr2val(ptr) ((table_value_t)(uintptr_t)(ptr))
#define key2ptr(ptr) ((void*)(uintptr_t)(ptr))
#define val2ptr(ptr) ((void*)(uintptr_t)(ptr))

#define tokey(k) _Generic((k), void*: val2ptr(k), default: (k))

/* Pointer -> Pointer tables */

static inline TablePP
newHTablePP(size_t size)
{ return (TablePP) newHTable(size);
}

static inline void
destroyHTablePP(TablePP ht)
{ destroyHTable((Table)ht);
}

static inline void
clearHTablePP(TablePP ht)
{ clearHTable((Table)ht);
}

static inline size_t
sizeofTablePP(TablePP ht)
{ return sizeofTable((Table) ht);
}

#define lookupHTablePP(ht, name) LDFUNC(lookupHTablePP, ht, name)
static inline void*
lookupHTablePP(DECL_LD TablePP ht, void *name)
{ return val2ptr(lookupHTable((Table)ht, ptr2key(name)));
}

#define addHTablePP(ht, name, value) LDFUNC(addHTablePP, ht, name, value)
static inline void*
addHTablePP(DECL_LD TablePP ht, void *name, void *value)
{ return val2ptr(addHTable((Table)ht, ptr2key(name), ptr2val(value)));
}

#define addNewHTablePP(ht, name, value) LDFUNC(addNewHTablePP, ht, name, value)
static inline void*
addNewHTablePP(DECL_LD TablePP ht, void *name, void *value)
{ return val2ptr(addNewHTable((Table)ht, ptr2key(name), ptr2val(value)));
}

#define updateHTablePP(ht, name, value) LDFUNC(updateHTablePP, ht, name, value)
static inline void*
updateHTablePP(DECL_LD TablePP ht, void *name, void *value)
{ return val2ptr(updateHTable((Table)ht, ptr2key(name), ptr2val(value)));
}

#define deleteHTablePP(ht, name) LDFUNC(deleteHTablePP, ht, name)
static inline void*
deleteHTablePP(DECL_LD TablePP ht, void *name)
{ return val2ptr(deleteHTable((Table)ht, ptr2key(name)));
}

static inline TableEnum
newTableEnumPP(TablePP ht)
{ return newTableEnum((Table)ht);
}


/* Pointer -> word tables */

static inline TablePW
newHTablePW(size_t size)
{ return (TablePW) newHTable(size);
}

static inline void
destroyHTablePW(TablePW ht)
{ destroyHTable((Table)ht);
}

static inline void
clearHTablePW(TablePW ht)
{ clearHTable((Table)ht);
}

#define lookupHTablePW(ht, name) LDFUNC(lookupHTablePW, ht, name)
static inline table_value_t
lookupHTablePW(DECL_LD TablePW ht, void *name)
{ return lookupHTable((Table)ht, ptr2key(name));
}

#define addHTablePW(ht, name, value) LDFUNC(addHTablePW, ht, name, value)
static inline table_value_t
addHTablePW(DECL_LD TablePW ht, void *name, table_value_t value)
{ return addHTable((Table)ht, ptr2key(name), value);
}

#define addNewHTablePW(ht, name, value) LDFUNC(addNewHTablePW, ht, name, value)
static inline table_value_t
addNewHTablePW(DECL_LD TablePW ht, void *name, table_value_t value)
{ return addNewHTable((Table)ht, ptr2key(name), value);
}

#define updateHTablePW(ht, name, value) LDFUNC(updateHTablePW, ht, name, value)
static inline table_value_t
updateHTablePW(DECL_LD TablePW ht, void *name, table_value_t value)
{ return updateHTable((Table)ht, ptr2key(name), value);
}

#define deleteHTablePW(ht, name) LDFUNC(deleteHTablePW, ht, name)
static inline table_value_t
deleteHTablePW(DECL_LD TablePW ht, void *name)
{ return deleteHTable((Table)ht, ptr2key(name));
}

static inline TableEnum
newTableEnumPW(TablePW ht)
{ return newTableEnum((Table)ht);
}


/* word -> Pointer tables */

static inline TableWP
newHTableWP(size_t size)
{ return (TableWP) newHTable(size);
}

static inline TableWP
copyHTableWP(TableWP ht)
{ return (TableWP) copyHTable((Table)ht);
}


static inline void
destroyHTableWP(TableWP ht)
{ destroyHTable((Table)ht);
}

static inline void
clearHTableWP(TableWP ht)
{ clearHTable((Table)ht);
}

static inline size_t
sizeofTableWP(TableWP ht)
{ return sizeofTable((Table) ht);
}

#define lookupHTableWP(ht, name) LDFUNC(lookupHTableWP, ht, name)
static inline void*
lookupHTableWP(DECL_LD TableWP ht, table_key_t name)
{ return val2ptr(lookupHTable((Table)ht, name));
}

#define addHTableWP(ht, name, value) LDFUNC(addHTableWP, ht, name, value)
static inline void*
addHTableWP(DECL_LD TableWP ht, table_key_t name, void *value)
{ return val2ptr(addHTable((Table)ht, name, ptr2val(value)));
}

#define addNewHTableWP(ht, name, value) LDFUNC(addNewHTableWP, ht, name, value)
static inline void*
addNewHTableWP(DECL_LD TableWP ht, table_key_t name, void *value)
{ return val2ptr(addNewHTable((Table)ht, name, ptr2val(value)));
}

#define updateHTableWP(ht, name, value) LDFUNC(updateHTableWP, ht, name, value)
static inline void*
updateHTableWP(DECL_LD TableWP ht, table_key_t name, void *value)
{ return val2ptr(updateHTable((Table)ht, name, ptr2val(value)));
}

#define deleteHTableWP(ht, name) LDFUNC(deleteHTableWP, ht, name)
static inline void*
deleteHTableWP(DECL_LD TableWP ht, table_key_t name)
{ return val2ptr(deleteHTable((Table)ht, name));
}

static inline TableEnum
newTableEnumWP(TableWP ht)
{ return newTableEnum((Table)ht);
}

		 /*******************************
		 *             UTIL             *
		 *******************************/

static inline int
htable_valid_kv(void *kv)
{ intptr_t kvi = (intptr_t)kv;		/* avoid NULL, HTABLE_TOMBSTONE */
  return kvi > 0 || kvi	< -2;		/* and HTABLE_SENTINEL */
}

#define pointerHashValue(p, size) ((((intptr_t)(p) >> LMASK_BITS) ^ \
				    ((intptr_t)(p) >> (LMASK_BITS+5)) ^ \
				    ((intptr_t)(p))) & \
				   ((size)-1))

#define FOR_TABLE(ht, n, v)					     \
  for( table_key_t n = NULL_KEY,				     \
	 v = NULL_VALUE,					     \
	 __ft_ht = ptr2key(ht),					     \
	 __ft_idx = 0,						     \
	 __ft_kvs = ptr2key((ht)->kvs),				     \
	 __ft_start = 1						     \
	 ;							     \
       __ft_start						     \
	 ? (ATOMIC_INC(&((KVS)(void*)(uintptr_t)__ft_kvs)->accesses) || 1) \
	 : (ATOMIC_DEC(&((KVS)(void*)(uintptr_t)__ft_kvs)->accesses) && 0) \
	 ;							     \
       __ft_start = 0						     \
    )								     \
    while ( htable_iter((Table)(void*)(uintptr_t)__ft_ht, \
			(KVS)(void*)(uintptr_t)__ft_kvs,  \
			(size_t *)&__ft_idx, &n, &v) )

#endif /*TABLE_H_INCLUDED*/
