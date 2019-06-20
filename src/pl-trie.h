/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2019, VU University Amsterdam
			      CWI, Amsterdam
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

#ifndef _PL_TRIE_H
#define _PL_TRIE_H
#include "pl-indirect.h"
#include "pl-copyterm.h"

/* keep statistics on trie accesses */
#define O_TRIE_STATS 1

#define TRIE_MAGIC  0x4bcbcf87
#define TRIE_CMAGIC 0x4bcbcf88

typedef enum
{ TN_KEY,				/* Single key */
  TN_HASHED				/* Hashed */
} tn_node_type;

typedef struct try_children_any
{ tn_node_type type;
} try_children_any;

typedef struct trie_children_hashed
{ tn_node_type	type;			/* TN_HASHED */
  Table		table;			/* Key --> child map */
  unsigned	var_keys;		/* # variable keys */
} trie_children_hashed;

typedef struct trie_children_key
{ tn_node_type type;
  word key;
  struct trie_node *child;
} trie_children_key;

typedef union trie_children
{ try_children_any     *any;
  trie_children_key    *key;
  trie_children_hashed *hash;
} trie_children;


typedef struct trie_node
{ word value;
  word key;
  struct trie_node *parent;
  trie_children children;
  struct
  { struct delay_info *delayinfo;
  } data;
} trie_node;


typedef struct trie_allocation_pool
{ size_t	size;			/* # nodes in use */
  size_t	limit;			/* Limit of the pool */
} trie_allocation_pool;

typedef struct trie
{ atom_t		symbol;		/* The associated symbol */
  int			magic;		/* TRIE_MAGIC */
  int			references;	/* access count */
  unsigned int		node_count;	/* # nodes */
  unsigned int		value_count;	/* # nodes with a value */
  trie_node	        root;		/* the root node */
  indirect_table       *indirects;	/* indirect values */
  void		      (*release_node)(struct trie *, trie_node *);
  trie_allocation_pool *alloc_pool;	/* Node allocation pool */
#ifdef O_TRIE_STATS
  struct
  { uint64_t		lookups;	/* trie_lookup */
    uint64_t		gen_call;	/* trie_gen calls */
    uint64_t		gen_exit;	/* trie_gen unifications succeeded */
    uint64_t		gen_fail;	/* trie_gen unifications failed */
  } stats;
#endif
  struct
  { struct worklist *worklist;		/* tabling worklist */
    trie_node	    *variant;		/* node in variant trie */
  } data;
} trie;

#define acquire_trie(t) ATOMIC_INC(&(t)->references)
#define release_trie(t) do { if ( ATOMIC_DEC(&(t)->references) == 0 ) \
			       trie_clean(t); \
			   } while(0)

COMMON(void)	initTries(void);
COMMON(trie *)	trie_create(void);
COMMON(void)	trie_empty(trie *trie);
COMMON(void)	trie_clean(trie *trie);
COMMON(void)	trie_delete(trie *trie, trie_node *node, int prune);
COMMON(void)	prune_node(trie *trie, trie_node *n);
COMMON(trie *)	get_trie_from_node(trie_node *node);
COMMON(int)	is_ground_trie_node(trie_node *node);
COMMON(int)	get_trie(term_t t, trie **tp);
COMMON(int)	get_trie_noex(term_t t, trie **tp);
COMMON(int)	unify_trie_term(trie_node *node, term_t term ARG_LD);
COMMON(int)	trie_lookup(trie *trie, trie_node **nodep, Word k,
			    int add, TmpBuffer vars ARG_LD);
COMMON(int)	trie_error(int rc, term_t culprit);
COMMON(atom_t)	trie_symbol(trie *trie);
COMMON(trie *)	symbol_trie(atom_t symbol);
COMMON(int)	put_trie_value(term_t t, trie_node *node ARG_LD);
COMMON(int)	set_trie_value(trie *trie, trie_node *node, term_t value ARG_LD);
COMMON(int)	set_trie_value_word(trie *trie, trie_node *node, word val);
COMMON(foreign_t) trie_gen(term_t Trie, term_t Key, term_t Value,
			   term_t Data,
			   int (*unify_data)(term_t, trie_node*, void* ARG_LD),
			   void *ctx, control_t PL__ctx);
COMMON(void *)	map_trie_node(trie_node *n,
			      void* (*map)(trie_node *n, void *ctx), void *ctx);

#endif /*_PL_TRIE_H*/
