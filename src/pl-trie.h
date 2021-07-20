/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2021, VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v
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

typedef struct trie_children_key
{ tn_node_type type;
  word key;
  struct trie_node *child;
} trie_children_key;

typedef struct trie_children_hashed
{ tn_node_type	type;			/* TN_HASHED */
  Table		table;			/* Key --> child map */
  unsigned	var_mask;		/* Variables in this place */
  trie_children_key *old_single;	/* Old single node */
} trie_children_hashed;

typedef union trie_children
{ try_children_any     *any;
  trie_children_key    *key;
  trie_children_hashed *hash;
} trie_children;


#define TN_PRIMARY			0x0001	/* Primary value node */
#define TN_SECONDARY			0x0002	/* Secondary value node */
#define TN_PRUNED			0x0004	/* Node path was pruned */
#define TN_IDG_DELETED			0x0008	/* IDG pre-evaluation */
#define TN_IDG_ADDED			0x0010	/* IDG recovery */
#define TN_IDG_UNCONDITIONAL		0x0020	/* IDG: previous cond state */
#define TN_IDG_AS_LAST			0x0040	/* IDG: answer subsumption node */
#define TN_IDG_MASK \
	(TN_IDG_DELETED|TN_IDG_ADDED| \
	 TN_IDG_UNCONDITIONAL|TN_IDG_AS_LAST)

typedef struct trie_node
{ word			value;
  word			key;
  struct trie_node     *parent;
  trie_children		children;
  struct
  { struct delay_info  *delayinfo;	/* can be unified with children */
  } data;
  unsigned		flags;		/* TN_* */
} trie_node;

#define TRIE_ISSET	0x0001		/* Trie nodes have no value */
#define TRIE_ISMAP	0x0002		/* Trie nodes have a value */
#define TRIE_ISSHARED	0x0004		/* This is a shared answer trie */
#define TRIE_COMPLETE	0x0008		/* Answer trie is complete */
#define TRIE_ABOLISH_ON_COMPLETE 0x0010	/* Abolish the table when completed */
#define TRIE_ISTRACKED  0x0020		/* Trie changes are tracked */

typedef struct trie
{ atom_t		symbol;		/* The associated symbol */
  int			magic;		/* TRIE_MAGIC */
  int			references;	/* access count */
  unsigned int		node_count;	/* # nodes */
  unsigned int		value_count;	/* # nodes with a value */
  unsigned int		flags;		/* misc flags */
#ifdef O_PLMT
  int			tid;		/* thread id doing completion or re-evaluation */
#endif
  trie_node	        root;		/* the root node */
  indirect_table       *indirects;	/* indirect values */
  void		      (*release_node)(struct trie *, trie_node *);
  alloc_pool	       *alloc_pool;	/* Node allocation pool */
  atom_t		clause;		/* Compiled representation */
#ifdef O_TRIE_STATS
  struct
  { uint64_t		lookups;	/* trie_lookup */
    uint64_t		gen_call;	/* trie_gen calls */
#ifdef O_PLMT
    unsigned int	deadlock;	/* times involved in a deadlock */
    unsigned int	wait;		/* times waited for */
#endif
  } stats;
#endif
  struct
  { struct worklist *worklist;		/* tabling worklist */
    trie_node	    *variant;		/* node in variant trie */
    struct idg_node *IDG;		/* Node in the IDG graph */
    Definition	     predicate;		/* Associated predicate */
  } data;
} trie;

typedef struct size_abstract
{ int		from_depth;		/* start below depth */
  size_t	size;			/* limit each term to size */
} size_abstract;

#define acquire_trie(t) ATOMIC_INC(&(t)->references)
#define release_trie(t) do { if ( ATOMIC_DEC(&(t)->references) == 0 ) \
			       trie_clean(t); \
			   } while(0)

#define TRIE_ARGS	3
#define TRIE_VAR_OFFSET (TRIE_ARGS+3)

#ifdef O_TRIE_STATS
#define TRIE_STAT_INC(t, v) ATOMIC_INC(&t->stats.v)
#else
#define TRIE_STAT_INC(t, v) ((void)0)
#endif

/* trie_lookup_abstract() return values (< 0: error) */
#define TRIE_ABSTRACTED			  2
#define TRIE_LOOKUP_CONTAINS_ATTVAR	-10
#define TRIE_LOOKUP_CYCLIC		-11

#if USE_LD_MACROS
#define	unify_trie_term(node, parent, term)					LDFUNC(unify_trie_term, node, parent, term)
#define	trie_lookup_abstract(trie, root, nodep, k, add, abstract, vars)		LDFUNC(trie_lookup_abstract, trie, root, nodep, k, add, abstract, vars)
#define	put_trie_value(t, node)							LDFUNC(put_trie_value, t, node)
#define	set_trie_value(trie, node, value)					LDFUNC(set_trie_value, trie, node, value)
#define	compile_trie(def, trie)							LDFUNC(compile_trie, def, trie)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void	initTries(void);
trie *	trie_create(alloc_pool *pool);
void	trie_destroy(trie *trie);
void	trie_empty(trie *trie);
void	trie_clean(trie *trie);
void	trie_delete(trie *trie, trie_node *node, int prune);
void	prune_node(trie *trie, trie_node *n);
void	prune_trie(trie *trie, trie_node *root,
		   void (*free)(trie_node *node, void *ctx), void *ctx);
trie *	get_trie_from_node(trie_node *node);
int	is_ground_trie_node(trie_node *node);
int	is_leaf_trie_node(trie_node *n);
int	get_trie(term_t t, trie **tp);
int	get_trie_noex(term_t t, trie **tp);
int	unify_trie_term(trie_node *node, trie_node **parent,
			term_t term);
int	trie_lookup_abstract(trie *trie,
			     trie_node *root, trie_node **nodep, Word k,
			     int add, size_abstract *abstract,
			     TmpBuffer vars);
int	trie_error(int rc, term_t culprit);
int	trie_trie_error(int rc, trie *trie);
atom_t	trie_symbol(trie *trie);
trie *	symbol_trie(atom_t symbol);
int	put_trie_value(term_t t, trie_node *node);
int	set_trie_value(trie *trie, trie_node *node, term_t value);
int	set_trie_value_word(trie *trie, trie_node *node, word val);
foreign_t trie_gen_raw(
	      trie *trie, trie_node *root,
	      term_t Key, term_t Value,
	      term_t Data,
	      int LDFUNCP (*unify_data)(term_t, trie_node*, void*),
	      void *ctx, control_t PL__ctx);
foreign_t clear_trie_gen_state(void *ctx);
foreign_t trie_gen(term_t Trie, term_t Root, term_t Key, term_t Value,
		   term_t Data,
		   int LDFUNCP (*unify_data)(term_t, trie_node*, void*),
		   void *ctx, control_t PL__ctx);
void *	map_trie_node(trie_node *n,
		      void* (*map)(trie_node *n, void *ctx), void *ctx);
atom_t	compile_trie(Definition def, trie *trie);
void	trie_discard_clause(trie *trie);

#undef LDFUNC_DECLARATIONS

#ifndef NO_TRIE_GEN_HELPERS
/* The common case is passing in the name of an LDFUNC, so this adds the LDFUNC_REF()
 * to get proper mangling. To use this without the mangling either define the
 * NO_TRIE_GEN_HELPERS macro (like pl-trie.c does) or put (trie_gen_raw) in parentheses. */
#define trie_gen_raw(trie, root, Key, Value, Data, unify_func, ctx, PL__ctx) \
	trie_gen_raw(trie, root, Key, Value, Data, LDFUNC_REF(unify_func), ctx, PL__ctx)
#define trie_gen(Trie, Root, Key, Value, Data, unify_func, ctx, PL__ctx) \
	trie_gen(Trie, Root, Key, Value, Data, LDFUNC_REF(unify_func), ctx, PL__ctx)
#endif

#define trie_lookup(trie, node, nodep, k, add, vars) LDFUNC(trie_lookup, trie, node, nodep, k, add, vars)
static inline int
trie_lookup(DECL_LD trie *trie, trie_node *node, trie_node **nodep,
	    Word k, int add, TmpBuffer vars)
{ return trie_lookup_abstract(trie, node, nodep, k, add,
			      NULL, vars);
}

#endif /*_PL_TRIE_H*/
