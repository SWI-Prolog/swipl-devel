/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Keri Harris
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

#if USE_LD_MACROS
#define	lookupHTable(ht, name)	LDFUNC(lookupHTable, ht, name)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		initTables(void);
Table		newHTable(int size);
void		destroyHTable(Table ht);
void*		lookupHTable(Table ht, void *name);
void*		addHTable(Table ht, void *name, void *value);
void*		addNewHTable(Table ht, void *name, void *value);
void*		updateHTable(Table ht, void *name, void *value);
void*		deleteHTable(Table ht, void *name);
void		clearHTable(Table ht);
Table		copyHTable(Table org);
TableEnum	newTableEnum(Table ht);
void		freeTableEnum(TableEnum e);
int		advanceTableEnum(TableEnum e, void **name, void **value);
					/* used by for_table() macro */
int		htable_iter(Table ht, KVS kvs, int *idx,
			    void **name, void **value);
size_t		sizeofTable(Table ht);

#undef LDFUNC_DECLARATIONS

static inline int
htable_valid_kv(void *kv)
{ intptr_t kvi = (intptr_t)kv;		/* avoid NULL, HTABLE_TOMBSTONE */
  return kvi > 0 || kvi	< -2;		/* and HTABLE_SENTINEL */
}

#define pointerHashValue(p, size) ((((intptr_t)(p) >> LMASK_BITS) ^ \
				    ((intptr_t)(p) >> (LMASK_BITS+5)) ^ \
				    ((intptr_t)(p))) & \
				   ((size)-1))

#define FOR_TABLE(ht, n, v) \
	for \
	( void *n = NULL, \
	       *v = NULL, \
	       *__ft_ht = (ht), \
	       *__ft_idx = NULL, \
	       *__ft_kvs = ((Table)ht)->kvs, \
	       *__ft_start = (void*)1 \
	; \
	  __ft_start \
	  ? (ATOMIC_INC(&((KVS)__ft_kvs)->accesses) || 1) \
	  : (ATOMIC_DEC(&((KVS)__ft_kvs)->accesses) && 0) \
	; \
	  __ft_start = NULL \
	) \
	  while ( htable_iter((Table)__ft_ht, (KVS)__ft_kvs, (int *)&__ft_idx, &n, &v) )

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

#define for_table_as_long_as(ht, n, v, code) \
	{ int idx = 0; \
          KVS kvs = ht->kvs; \
          ATOMIC_INC(&kvs->accesses); \
	  void *n = NULL; \
          void *v = NULL; \
          while ( htable_iter(ht, kvs, &idx, &n, &v) ) \
	  { if ( !(code) ) \
	      break; \
	  } \
          ATOMIC_DEC(&kvs->accesses); \
	}

#endif /*TABLE_H_INCLUDED*/
