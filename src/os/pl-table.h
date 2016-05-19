/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Keri Harris
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2015, University of Amsterdam
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

#ifndef TABLE_H_INCLUDED
#define TABLE_H_INCLUDED

typedef struct table *		Table;		/* (numeric) hash table */
typedef struct kvs *		KVS;		/* map of key-values */
typedef struct symbol *		Symbol;		/* symbol of hash table */
typedef struct table_enum *	TableEnum;	/* Enumerate table entries */

struct table
{ int		size;		/* # symbols in the table */
  int		cleanup;	/* TRUE when KVS cleanup in progress */
  void		(*copy_symbol)(void *n, void **v);
  void		(*free_symbol)(void *n, void *v);
  KVS		kvs;		/* map of key-value pairs */
};

struct kvs
{ int len;			/* size of key-value map */
  int resizing;			/* TRUE while resizing */
  int accesses;			/* number of accesses */
  KVS next;			/* next map */
  KVS prev;			/* last map */
  Symbol entries;		/* array of hash symbols */
};

struct symbol
{ void *	name;		/* name entry of symbol */
  void *	value;		/* associated value with name */
};

struct table_enum
{ Table		table;		/* Table we are working on */
  KVS		kvs;		/* kvs we are iterating over */
  int		idx;		/* Index of current symbol-chain */
};

COMMON(void)		initTables(void);
COMMON(Table)		newHTable(int size);
COMMON(void)		destroyHTable(Table ht);
COMMON(void*)		lookupHTable__LD(Table ht, void *name ARG_LD);
COMMON(void*)		addHTable(Table ht, void *name, void *value);
COMMON(void)		addNewHTable(Table ht, void *name, void *value);
COMMON(void*)		updateHTable(Table ht, void *name, void *value);
COMMON(void*)		deleteHTable(Table ht, void *name);
COMMON(void)		clearHTable(Table ht);
COMMON(Table)		copyHTable(Table org);
COMMON(TableEnum)	newTableEnum(Table ht);
COMMON(void)		freeTableEnum(TableEnum e);
COMMON(int)		advanceTableEnum(TableEnum e, void **name, void **value);
					/* used by for_table() macro */
COMMON(int)		htable_iter(Table ht, KVS kvs, int *idx,
				    void **name, void **value);
COMMON(size_t)		sizeofTable(Table ht);

static inline int
htable_valid_kv(void *kv)
{ intptr_t kvi = (intptr_t)kv;		/* avoid NULL, HTABLE_TOMBSTONE */
  return kvi > 0 || kvi	< -2;		/* and HTABLE_SENTINEL */
}

#define pointerHashValue(p, size) ((((intptr_t)(p) >> LMASK_BITS) ^ \
				    ((intptr_t)(p) >> (LMASK_BITS+5)) ^ \
				    ((intptr_t)(p))) & \
				   ((size)-1))

#define for_table(ht, n, v, code) \
	{ int idx = 0; \
          KVS kvs = ht->kvs; \
          ATOMIC_INC(&kvs->accesses); \
	  void *n = NULL; \
          void *v = NULL; \
          while ( htable_iter(ht, kvs, &idx, &n, &v) ) \
	  { code; \
	  } \
          ATOMIC_DEC(&kvs->accesses); \
	}

#endif /*TABLE_H_INCLUDED*/
