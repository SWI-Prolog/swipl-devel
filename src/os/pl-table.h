/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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
typedef struct symbol *		Symbol;		/* symbol of hash table */
typedef struct table_enum *	TableEnum;	/* Enumerate table entries */

struct table
{ int		buckets;	/* size of hash table */
  int		size;		/* # symbols in the table */
  TableEnum	enumerators;	/* Handles for enumeration */
#ifdef O_PLMT
  simpleMutex  *mutex;		/* Mutex to guard table */
#endif
  void		(*copy_symbol)(Symbol s);
  void		(*free_symbol)(Symbol s);
  Symbol	*entries;	/* array of hash symbols */
};

struct symbol
{ Symbol	next;		/* next in chain */
  void *	name;		/* name entry of symbol */
  void *	value;		/* associated value with name */
};

struct table_enum
{ Table		table;		/* Table we are working on */
  int		key;		/* Index of current symbol-chain */
  Symbol	current;	/* The current symbol */
  TableEnum	next;		/* More choice points */
};

COMMON(void)		initTables(void);
COMMON(Table)		newHTable(int size);
COMMON(void)		destroyHTable(Table ht);
COMMON(Symbol)		lookupHTable(Table ht, void *name);
COMMON(Symbol)		addHTable(Table ht, void *name, void *value);
COMMON(void)		deleteSymbolHTable(Table ht, Symbol s);
COMMON(void)		clearHTable(Table ht);
COMMON(Table)		copyHTable(Table org);
COMMON(TableEnum)	newTableEnum(Table ht);
COMMON(void)		freeTableEnum(TableEnum e);
COMMON(Symbol)		advanceTableEnum(TableEnum e);

#define TABLE_UNLOCKED		0x10000000L /* do not create mutex for table */
#define TABLE_MASK		0xf0000000UL

#define pointerHashValue(p, size) ((((intptr_t)(p) >> LMASK_BITS) ^ \
				    ((intptr_t)(p) >> (LMASK_BITS+5)) ^ \
				    ((intptr_t)(p))) & \
				   ((size)-1))

#define for_table(ht, s, code) \
	{ int _k; \
	  PL_LOCK(L_TABLE); \
	  for(_k = 0; _k < (ht)->buckets; _k++) \
	  { Symbol _n, s; \
	    for(s=(ht)->entries[_k]; s; s = _n) \
	    { _n = s->next; \
	      code; \
	    } \
	  } \
          PL_UNLOCK(L_TABLE); \
	}
#define for_unlocked_table(ht, s, code) \
	{ int _k; \
	  for(_k = 0; _k < (ht)->buckets; _k++) \
	  { Symbol _n, s; \
	    for(s=(ht)->entries[_k]; s; s = _n) \
	    { _n = s->next; \
	      code; \
	    } \
	  } \
	}

#endif /*TABLE_H_INCLUDED*/
