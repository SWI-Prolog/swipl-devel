/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016-2022, VU University Amsterdam
			      CWI, Amsterdam
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

#ifndef _PL_TABLING_H
#define _PL_TABLING_H
#include "pl-trie.h"

typedef enum
{ CLUSTER_ANSWERS,
  CLUSTER_SUSPENSIONS
} cluster_type;

#define DELAY_MAGIC	0x67e9124d
#define WORKLIST_MAGIC	0x67e9124e
#define COMPONENT_MAGIC	0x67e9124f

#define TF_MONOTONIC_LAZY	0x0001


		 /*******************************
		 *     GLOBAL ENTRY POINTS	*
		 *******************************/

typedef struct worklist_set
{ buffer members;
} worklist_set;

typedef struct component_set
{ buffer members;
} component_set;

typedef enum
{ SCC_ACTIVE=0,
  SCC_MERGED,
  SCC_COMPLETED
} scc_status;

typedef enum
{ SCC_NEG_NONE=0,				/* no negative nodes */
  SCC_NEG_DELAY,
  SCC_NEG_SIMPLIFY
} scc_neg_status;

typedef struct tbl_component
{ int			magic;			/* COMPONENT_MAGIC */
  scc_status	        status;			/* SCC_* */
  scc_neg_status	neg_status;		/* SCC_NEG_* */
  size_t		simplifications;        /* # simplifications */
  struct tbl_component *parent;
  component_set        *children;		/* Child components */
  component_set        *merged;			/* Child components */
  worklist_set         *worklist;		/* Worklist of current query */
  worklist_set         *created_worklists;	/* Worklists created */
  worklist_set	       *delay_worklists;	/* Worklists in need for delays */
  trie		       *leader;			/* Leading variant */
} tbl_component;

typedef struct tbl_status
{ tbl_component *scc;				/* The SCC we are working on */
  int		 hsc;				/* We have an active SCC */
  int		 iac;				/* We are in answer completion */
  int		 iap;				/* We are in assert propagation */
} tbl_status;

void save_tabling_status(tbl_status *stat);
void restore_tabling_status(tbl_status *stat);


		 /*******************************
		 *	   TABLE WORKLIST	*
		 *******************************/

typedef struct cluster
{ cluster_type type;
  struct cluster *next;
  struct cluster *prev;
  buffer members;
} cluster;

typedef struct worklist
{ cluster      *head;			/* answer and dependency clusters */
  cluster      *tail;
  cluster      *riac;			/* rightmost inner answer cluster */
  cluster      *free_clusters;		/* clusters to reuse */
  int		magic;			/* WORKLIST_MAGIC */
  unsigned	ground : 1;		/* Ground call (early completion) */
  unsigned	executing : 1;		/* $tbl_wkl_work/3 in progress */
  unsigned	in_global_wl : 1;	/* already in global worklist */
  unsigned	negative : 1;		/* this is a suspended negation */
  unsigned	neg_delayed : 1;	/* Negative node was delayed */
  unsigned	has_answers : 1;	/* At least one unconditional answer */
  unsigned	answer_completed : 1;	/* Is answer completed */
  unsigned	depend_abolish : 1;	/* Scheduled for depending abolish */
  unsigned	needs_answer_gc : 1;	/* Contains garbage answers */
  size_t	undefined;		/* #undefined answers */

  tbl_component*component;		/* component I belong to */
  trie	       *table;			/* My answer table */
  Definition	predicate;		/* Predicate we are associated with */

  buffer	delays;			/* Delayed answers */
  buffer	pos_undefined;		/* Positive undefined */
} worklist;


		 /*******************************
		 *	    DELAY LISTS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A `delay_list` represents a conjunction of conditions. Each condition is
either a negative literal <~L> or  a   positive  literal  with an answer
<L,A>.

Delay lists are associated with an answer. An   answer can have a set of
delay lists (`delay_list_set`) and are combined in a `delay_info` struct
that provides information  about  the  variant   for  which  this  is  a
condition.

A `worklist` points at  the  delay  list   elements  for  which  it is a
condition. After resolving a worklist (answer  for positive literals) we
should go over the places where  its   associated  variant  is used as a
condition and

  - Delete the condition from the delay list if the condition is
    satified. If all conditions are satified propagate using the
    resolved answer.
  - Delete the answer from the answer trie and propage that.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct delay_usage
{ buffer	     answers;		/* trie_node * to conditional answers */
} delay_usage;

typedef struct delay
{ trie	            *variant;		/* Answer trie */
  trie_node         *answer;		/* Answer in there (NULL for negative) */
} delay;

typedef struct delay_set
{ unsigned	     offset;		/* offset in delays */
  unsigned	     size;		/* size of the conjunction */
  unsigned	     active;		/* active members of conjunction */
} delay_set;

typedef struct delay_info
{ trie_node      *variant;		/* Variant trie node */
  unsigned	  has_share_records;	/* We have variable sharing records */
  buffer          delay_sets;		/* The disjunctive conditions */
  buffer	  delays;		/* Store for the delays */
} delay_info;


		 /*******************************
		 *	       IDG		*
		 *******************************/

#define IDG_NODE_MAGIC 0x347e54d2
#define IDG_MDEP_MAGIC 0x745af3b0

typedef struct idg_node
{ int           magic;			/* IDG_NODE_MAGIC */
  trie	       *atrie;			/* answer trie */
  Table		affected;		/* parent IDG nodes */
  Table		dependent;		/* child IDG nodes */
  size_t	answer_count;		/* #answers in previous complete state */
  unsigned	new_answer : 1;		/* Update generated a new answer */
  unsigned	reevaluating : 1;	/* currently re-evaluating */
  unsigned	aborted : 1;		/* re-evaluation was aborted */
  unsigned	monotonic : 1;		/* Associated predicate is monotonic */
  unsigned	lazy : 1;		/* Lazy monotonic node */
  unsigned	lazy_queued : 1;	/* There are answers queued */
  unsigned	force_reeval : 1;	/* Forced reevaluation for monotonic */
  unsigned	mono_reevaluating : 1;	/* Monotonic reevaluation in progress */
  unsigned	tt_notrail : 1;		/* Do not trail new monotonic answers */
  unsigned	tt_new_dep : 1;		/* New dependency on transition mod */
  int		falsecount;		/* Invalidate count */
#ifdef O_TRIE_STATS
  struct
  { uint64_t	invalidated;		/* # times it was invalidated */
    uint64_t	reevaluated;		/* # times it was re-evaluated */
  } stats;
#endif
} idg_node;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Linked list of dependencies for monotonic tabling.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct idg_mdep
{ int            magic;			/* IDG_MDEP_MAGIC */
  unsigned	 lazy : 1;		/* Dependency is lazy */
  unsigned int	 hash;			/* Murmur hash of dependency */
  fastheap_term *dependency;		/* dependency structure */
  union
  { idg_node    *child;			/* Final child node */
    struct idg_mdep *dep;		/* Other dependency */
    void *any;
  } next;
  Buffer	 queue;			/* Unprocessed answers */
} idg_mdep;

#define TRIE_ARRAY_PREALLOCATED 7

typedef struct trie_array
{ trie **blocks[MAX_BLOCKS];
  trie *preallocated[TRIE_ARRAY_PREALLOCATED];
} trie_array;


		 /*******************************
		 *    TRANSACTION RECORDING	*
		 *******************************/

#define TT_TBL_INVALIDATE	0x001

typedef struct tbl_trail
{ Table		tables;			/* Affected tables */
  buffer	actions;
} tbl_trail;

typedef enum tbl_trail_type
{ TT_ANSWER = 1
} tbl_trail_type;

typedef struct tbl_trail_any
{ tbl_trail_type	type;		/* TT_* */
} tbl_trail_any;

typedef struct tbl_trail_table
{ tbl_trail_type	type;		/* TT_TABLE */
  atom_t		symbol;
} tbl_trail_table;

typedef struct tbl_trail_answer
{ tbl_trail_type	type;		/* TT_ANSWER */
  trie                 *atrie;
  trie_node	       *answer;
} tbl_trail_answer;


		 /*******************************
		 *     PREDICATE PROPERTIES	*
		 *******************************/

#define TP_TABLED	(0x0001)	/* Predicate is tabled */
#define TP_MONOTONIC	(0x0002)	/* Monotonic tabling */
#define TP_SHARED	(0x0004)	/* Shared tabling */
#define TP_OPAQUE	(0x0008)	/* Declared opaque */
#define TP_LAZY		(0x0010)	/* Lazy (monotonic) */
#define TP_INCREMENTAL	(0x0020)	/* Incremental tabling */

typedef struct table_props
{ unsigned int	flags;			/* TP_* flags */
  size_t	abstract;		/* IDG abstraction */
  size_t	subgoal_abstract;	/* Subgoal abstraction */
  size_t	answer_abstract;	/* Answer abstraction */
  size_t	max_answers;		/* Answer count limit */
  Buffer	lazy_queue;		/* Queued clauses for monotonic tabling */
} table_props;


		 /*******************************
		 *	     PROTOTYPES		*
		 *******************************/

#if USE_LD_MACROS
#define		transaction_commit_tables(_)		LDFUNC(transaction_commit_tables, _)
#define		transaction_rollback_tables(_)		LDFUNC(transaction_rollback_tables, _)
#define		tbl_push_delay(atrie, wrapper, answer)	LDFUNC(tbl_push_delay, atrie, wrapper, answer)
#define		idg_add_dyncall(def, ctrie, variant)	LDFUNC(idg_add_dyncall, def, ctrie, variant)
#define		tbl_get_restraint_flag(t, key)		LDFUNC(tbl_get_restraint_flag, t, key)
#define		tbl_set_restraint_flag(t, key)		LDFUNC(tbl_set_restraint_flag, t, key)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int	transaction_commit_tables(void);
int	transaction_rollback_tables(void);
void	merge_tabling_trail(tbl_trail *into, tbl_trail *from);

void	clearThreadTablingData(PL_local_data_t *ld);
term_t	init_delay_list(void);
void	tbl_push_delay(atom_t atrie, Word wrapper,
		       trie_node *answer);
int	answer_is_conditional(trie_node *answer);
void	untable_from_clause(Clause cl);
void	initTabling(void);
void	cleanupTabling(void);
int	idg_add_dyncall(Definition def, trie *ctrie,
			term_t variant);
int	tbl_is_predicate_attribute(atom_t key);
void	tbl_reset_tabling_attributes(Definition def);
int	tbl_get_predicate_attribute(Definition def,
				    atom_t att, term_t value);
int	tbl_set_predicate_attribute(Definition def,
				    atom_t att, term_t value);
int	tbl_is_restraint_flag(atom_t key);
int	tbl_get_restraint_flag(term_t t, atom_t key);
int	tbl_set_restraint_flag(term_t t, atom_t key);
int	setMonotonicMode(atom_t a);
void	tbl_set_incremental_predicate(Definition def, int val);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_TABLING_H*/
