/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016-2025, VU University Amsterdam
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

#define NO_TRIE_GEN_HELPERS 1
#include "pl-incl.h"
#include "pl-comp.h"
#include "pl-trie.h"
#include "pl-tabling.h"
#include "pl-indirect.h"
#include "pl-proc.h"
#include "pl-prims.h"
#include "pl-wam.h"
#include "pl-gc.h"
#include "pl-fli.h"
#include "pl-rec.h"
#include "pl-util.h"
#include "pl-attvar.h"
#include "pl-pro.h"
#include "os/pl-buffer.h"
#define NO_AC_TERM_WALK 1
#define AC_TERM_WALK_POP 1
#include "pl-termwalk.c"
#include "pl-dbref.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file implements tries of  terms.  The   trie  itself  lives  in the
program space and is represented by a (symbol) handle. This implies that
tries are subject to garbage collection.

A path through a trie represents a  sequence of tokens. For representing
terms, these tokens are functor symbols,   variables  and atomic values.
The _value_ associated with a  term  always   appears  in  a _leaf_ node
because a sequence that represents a term   is  _never_ the prefix of of
the sequence of another term.

TODO
  - Limit size of the tries
  - Avoid using a hash-table for small number of branches
  - Thread safe reclaiming
    - Reclaim single-child node after moving to a hash
    - Make pruning the trie thread-safe
  - Provide deletion from a trie
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define RESERVED_TRIE_VAL(n) (((word)((uintptr_t)n)<<LMASK_BITS) | \
			      TAG_VAR|STG_LOCAL)
#define TRIE_ERROR_VAL       RESERVED_TRIE_VAL(1)
#define TRIE_KEY_ABSENT      RESERVED_TRIE_VAL(2)	/* key of no stored value */
#define TRIE_KEY_POP(n)      RESERVED_TRIE_VAL(10+(n))

#define IS_TRIE_KEY_POP(w)   ((size_t)((tagex(w) == (TAG_VAR|STG_LOCAL) && \
					((w)>>LMASK_BITS) > 10) ? ((w)>>LMASK_BITS) - 10 \
								: 0))

#define NVARS_FAST 100

/* Will eventually be shared in pl-wam.c */
typedef enum
{ uread = 0,				/* Unification in read-mode */
  uwrite				/* Unification in write mode */
} unify_mode;

typedef struct ukey_state
{ trie	       *trie;			/* Trie for indirects */
  Word		ptr;			/* current location */
  size_t        a_offset;		/* For resetting the argument stack */
  unify_mode	umode;			/* unification mode */
  tmp_buffer	vars;
} ukey_state;

#if O_TRIE_ATTVAR
typedef struct
{ Word attvar;
  Word value;
} ukey_attvar;
#endif

#if USE_LD_MACROS
#define	unify_key(state, key)		LDFUNC(unify_key, state, key)
#define	init_ukey_state(state, trie, p, reinit) \
	LDFUNC(init_ukey_state, state, trie, p, reinit)
#define	destroy_ukey_state(state)	LDFUNC(destroy_ukey_state, state)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS
static boolex_t	unify_key(ukey_state *state, word key);
static void	init_ukey_state(ukey_state *state, trie *trie, Word p,
				bool reinit);
static void	destroy_ukey_state(ukey_state *state);
static void	set_trie_clause_general_undefined(Clause cl);
static void	trie_destroy(trie *trie);
#undef LDFUNC_DECLARATIONS


		 /*******************************
		 *	       SYMBOL		*
		 *******************************/

typedef struct tref
{ trie *trie;				/* represented trie */
} tref;

static int
write_trie_ref(IOSTREAM *s, atom_t aref, int flags)
{ tref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<trie>(%p)", ref->trie);
  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC a trie. Note that the  Prolog predicate trie_destroy/1 merely empties
the trie, leaving its destruction to   the  atom garbage collector. This
function is also called from pl-tabling.c   when  we accidentally create
the same variant trie concurrently and  need   to  dispose  of one. As a
result, this must be public and thread-safe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
release_trie_ref(atom_t aref)
{ tref *ref = PL_blob_data(aref, NULL, NULL);
  trie *t = ref->trie;

  if ( t && COMPARE_AND_SWAP_PTR(&ref->trie, t, NULL) )
    trie_destroy(t);			/* can be called twice */

  return true;
}


static int
save_trie(atom_t aref, IOSTREAM *fd)
{ tref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <trie>(%p)", ref->trie);
}


static atom_t
load_trie(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-trie-ref>");
}


static PL_blob_t trie_blob =
{ PL_BLOB_MAGIC,
  0,
  "trie",
  release_trie_ref,
  NULL,
  write_trie_ref,
  NULL,
  save_trie,
  load_trie
};

		 /*******************************
		 *	     THE TRIE		*
		 *******************************/

static trie_node       *new_trie_node(trie *trie, word key);
static void		destroy_node(trie *trie, trie_node *n);
static void		clear_node(trie *trie, trie_node *n, int dealloc);
static inline void	release_value(word value);


static inline void
acquire_key(word key)
{ if ( isAtom(key) )
    PL_register_atom(word2atom(key));
}

static inline void
release_key(word key)
{ if ( isAtom(key) )
    PL_unregister_atom(word2atom(key));
}


trie *
trie_create(alloc_pool *pool)
{ trie *trie;

  if ( (trie = alloc_from_pool(pool, sizeof(*trie))) )
  { memset(trie, 0, sizeof(*trie));
    trie->magic = TRIE_MAGIC;
    trie->node_count = 1;		/* the root */
    trie->alloc_pool = pool;
  }

  return trie;
}


static void
trie_destroy(trie *trie)
{ DEBUG(MSG_TRIE_GC, Sdprintf("Destroying trie %p\n", trie));
  trie->magic = TRIE_CMAGIC;
  trie_empty(trie);
  free_to_pool(trie->alloc_pool, trie, sizeof(*trie));
}


void
trie_discard_clause(trie *trie)
{ atom_t dbref;

  if ( (dbref=trie->clause) )
  { if ( COMPARE_AND_SWAP_ATOM(&trie->clause, dbref, 0) &&
	 GD->halt.cleaning == CLN_NORMAL )		/* otherwise reclaims clause */
    { ClauseRef cref = clause_clref(dbref);	/* from two ends */

      if ( cref )
      { Clause cl = cref->value.clause;
	set_trie_clause_general_undefined(cl);	/* TBD: only if undefined */
	retractClauseDefinition(cl->predicate, cl, false);
      }
      PL_unregister_atom(dbref);
    }
  }
}


void
trie_empty(trie *trie)
{ trie_discard_clause(trie);

  if ( !trie->references )
  { indirect_table *it = trie->indirects;

    clear_node(trie, &trie->root, false);	/* TBD: verify not accessed */
    if ( it && COMPARE_AND_SWAP_PTR(&trie->indirects, it, NULL) )
      destroy_indirect_table(it);
    trie->node_count = 1;
    trie->value_count = 0;
  }
}


void
trie_clean(trie *trie)
{ if ( trie->magic == TRIE_CMAGIC )
    trie_empty(trie);
}


#define get_child(n, key) LDFUNC(get_child, n, key)
static trie_node *
get_child(DECL_LD trie_node *n, word key)
{ trie_children children = n->children;

  if ( children.any )
  { switch( children.any->type )
    { case TN_KEY:
	if ( children.key->key == key )
	  return children.key->child;
        return NULL;
      case TN_HASHED:
	return lookupHTableWP(children.hash->table, key);
      default:
	assert(0);
    }
  }

  return NULL;
}


bool
is_leaf_trie_node(trie_node *n)
{ trie_children children = n->children;

  if ( children.any )
  { switch( children.any->type )
    { case TN_KEY:
	return false;
      case TN_HASHED:
	return children.hash->table->size == 0;
      default:
	assert(0);
    }
  }

  return true;
}


static trie_node *
new_trie_node(trie *trie, word key)
{ trie_node *n;

  if ( (n = alloc_from_pool(trie->alloc_pool, sizeof(*n))) )
  { ATOMIC_INC(&trie->node_count);
    memset(n, 0, sizeof(*n));
    acquire_key(key);
    n->key = key;
  }

  return n;
}


static void
clear_node(trie *trie, trie_node *n, int dealloc)
{ trie_children children;

next:
  children = n->children;

  if ( trie->release_node )
    (*trie->release_node)(trie, n);

  release_key(n->key);
  if ( n->value )
    release_value(n->value);

  if ( dealloc )
  { ATOMIC_DEC(&trie->node_count);
    free_to_pool(trie->alloc_pool, n, sizeof(trie_node));
  } else
  { n->children.any = NULL;
    clear(n, TN_PRIMARY|TN_SECONDARY);
  }

  if ( children.any )
  { switch( children.any->type )
    { case TN_KEY:
      { n = children.key->child;
	free_to_pool(trie->alloc_pool, children.key, sizeof(*children.key));
	dealloc = true;
	goto next;
      }
      case TN_HASHED:
      { TableWP table = children.hash->table;
	TableEnum e = newTableEnumWP(table);
	trie_children_key *os;

	if ( (os=children.hash->old_single) )	/* see insert_child() (*) note */
	  free_to_pool(trie->alloc_pool, os, sizeof(*os));
	free_to_pool(trie->alloc_pool, children.hash, sizeof(*children.hash));

	table_value_t tv;
	while(advanceTableEnum(e, NULL, &tv))
	  clear_node(trie, val2ptr(tv), true);

	freeTableEnum(e);
	destroyHTableWP(table);
	break;
      }
    }
  }
}

static void
destroy_node(trie *trie, trie_node *n)
{ clear_node(trie, n, true);
}


/*
 * Prune a branch of the trie that does not end in a node.  This should
 * be used after deletion or unsuccessful insertion, e.g., by trying to
 * insert a cyclic term
 *
 * TBD: Need to think about concurrency here.
 */

void
prune_node(trie *trie, trie_node *n)
{ trie_node *p;
  int empty = true;

  for(; empty && n->parent && isoff(n, TN_PRIMARY|TN_SECONDARY); n = p)
  { trie_children children;

    p = n->parent;
    children = p->children;

    if ( children.any )
    { switch( children.any->type )
      { case TN_KEY:
	  if ( COMPARE_AND_SWAP_PTR(&p->children.any, children.any, NULL) )
	  { if ( !trie )
	      trie = get_trie_from_node(n);
	    free_to_pool(trie->alloc_pool, children.key, sizeof(*children.key));
	  }
	  break;
	case TN_HASHED:
	  deleteHTableWP(children.hash->table, n->key);
	  empty = children.hash->table->size == 0;
	  break;
      }
    }

    if ( !trie )
      trie = get_trie_from_node(n);
    destroy_node(trie, n);
  }
}

/** Prune all branches below `root` that do not end in a value.
 *
 * @param `free` is called for each removed _leaf_ node.
*/

typedef struct prune_state
{ TableEnum  e;
  trie_node *n;
} prune_state;

void
prune_trie(trie *trie, trie_node *root,
	   void (*free)(trie_node *node, void *ctx), void *ctx)
{ segstack stack;
  prune_state buffer[64];
  trie_children children;
  trie_node *n = root;
  trie_node *p;
  prune_state ps = { .e = NULL };

  initSegStack(&stack, sizeof(prune_state), sizeof(buffer), buffer);

  for(;;)
  { children = n->children;

    if ( children.any )
    { switch( children.any->type )
      { case TN_KEY:
	{ n = children.key->child;
	  continue;
	}
	case TN_HASHED:
	{ TableWP table = children.hash->table;
	  TableEnum e = newTableEnumWP(table);
	  table_value_t v;

	  if ( advanceTableEnum(e, NULL, &v) )
	  { if ( !pushSegStack(&stack, ps, prune_state) )
	      outOfCore();
	    ps.e = e;
	    ps.n = n;

	    n = val2ptr(v);
	    continue;
	  } else
	  { freeTableEnum(e);
	    break;
	  }
	}
      }
    } else
    { if ( free )
	(*free)(n, ctx);
    }

  prune:
    for(; n != root && isoff(n, TN_PRIMARY|TN_SECONDARY); n = p)
    { trie_children children;
      int choice = false;

      p = n->parent;
      children = p->children;

      if ( children.any )
      { switch( children.any->type )
	{ case TN_KEY:
	    if ( COMPARE_AND_SWAP_PTR(&p->children.any, children.any, NULL) )
	      free_to_pool(trie->alloc_pool, children.key, sizeof(*children.key));
	    break;
	  case TN_HASHED:
	    deleteHTableWP(children.hash->table, n->key);
	    choice = true;
	    break;
	}
      }

      destroy_node(trie, n);
      if ( choice )
	break;
    }

  next_choice:
    if ( ps.e )
    { table_value_t v;

      if ( advanceTableEnum(ps.e, NULL, &v) )
      { n = val2ptr(v);
	continue;
      } else
      { n = ps.n;
	freeTableEnum(ps.e);
	popSegStack(&stack, &ps, prune_state);
	assert(n->children.any->type == TN_HASHED);
	if ( n->children.hash->table->size == 0 )
	  goto prune;
	goto next_choice;
      }
    } else
    { break;
    }
  }

  clearSegStack(&stack);
}


#define VMASKBITS  (sizeof(unsigned)*8)
#define VMASK_SCAN (0x1U<<(VMASKBITS-1))

static inline void
update_var_mask(trie_children_hashed *hnode, word key)
{ unsigned *maskp;

  if ( tagex(key) == TAG_VAR )
    maskp = &hnode->var_mask;
  else if ( tagex(key) == (TAG_ATTVAR|STG_STATIC) )
    maskp = &hnode->attvar_mask;
  else
    return;

  size_t vn = (size_t)(key>>LMASK_BITS); /* 1.. */
  ATOMIC_OR(maskp, vn < VMASKBITS ? 0x1U<<(vn-1) : VMASK_SCAN);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) The single node may be  in  use   with  another  thread. We have two
options:

  - Use one of the LD _active_ pointers to acquire/release access to the
    trie nodes and use safe delayed release.
  - Add the ond _single_ node to the new hash node and delete it along
    with the hash node when we clean the table.  We have opted for this
    option as it is simple and the single key is neglectable in size
    compared to the hash table anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define insert_child(trie, n, key) LDFUNC(insert_child, trie, n, key)
static trie_node *
insert_child(DECL_LD trie *trie, trie_node *n, word key)
{ for(;;)
  { trie_children children = n->children;
    trie_node *new = new_trie_node(trie, key);

    if ( !new )
      return NULL;			/* resource error */

    if ( children.any )
    { switch( children.any->type )
      { case TN_KEY:
	{ if ( children.key->key == key )
	  { destroy_node(trie, new);	/* someone else did this */
	    return children.key->child;
	  } else
	  { trie_children_hashed *hnode;

	    if ( !(hnode=alloc_from_pool(trie->alloc_pool, sizeof(*hnode))) )
	    { destroy_node(trie, new);
	      return NULL;
	    }

	    hnode->type     = TN_HASHED;
	    hnode->table    = newHTableWP(4);
	    hnode->var_mask = 0;
	    hnode->attvar_mask = 0;
	    addHTableWP(hnode->table, children.key->key, children.key->child);
	    addHTableWP(hnode->table, key, new);
	    update_var_mask(hnode, children.key->key);
	    update_var_mask(hnode, new->key);
	    new->parent = n;

	    if ( COMPARE_AND_SWAP_PTR(&n->children.hash, children.hash, hnode) )
	    { hnode->old_single = children.key;			/* See (*) */
	      return new;
	    } else
	    { hnode->old_single = NULL;
	      destroy_node(trie, new);
	      destroyHTableWP(hnode->table);
	      free_to_pool(trie->alloc_pool, hnode, sizeof(*hnode));
	      continue;
	    }
	  }
	}
	case TN_HASHED:
	{ trie_node *old = addHTableWP(children.hash->table, key, new);

	  if ( new == old )
	  { new->parent = n;
	    update_var_mask(children.hash, new->key);
	  } else
	  { destroy_node(trie, new);
	  }
	  return old;
	}
	default:
	  assert(0);
      }
    } else
    { trie_children_key *child;

      if ( !(child=alloc_from_pool(trie->alloc_pool, sizeof(*child))) )
      { destroy_node(trie, new);
	return NULL;
      }

      child->type  = TN_KEY;
      child->key   = key;
      child->child = new;

      if ( COMPARE_AND_SWAP_PTR(&n->children.key, NULL, child) )
      { child->child->parent = n;
	return child->child;
      }
      destroy_node(trie, new);
      free_to_pool(trie->alloc_pool, child, sizeof(*child));
    }
  }
}


#define follow_node(trie, n, value, add) \
	LDFUNC(follow_node, trie, n, value, add)

static trie_node *
follow_node(DECL_LD trie *trie, trie_node *n, word value, bool add)
{ trie_node *child;

  if ( (child=get_child(n, value)) )
    return child;

  if ( add )
    return insert_child(trie, n, value);
  else
    return NULL;
}


#define trie_intern_indirect(trie, w, add) \
	LDFUNC(trie_intern_indirect, trie, w, add)

static word
trie_intern_indirect(DECL_LD trie *trie, word w, bool add)
{ for(;;)
  { if ( trie->indirects )
    { return intern_indirect(trie->indirects, w, add);
    } else if ( add )
    { indirect_table *newtab = new_indirect_table();

      if ( !COMPARE_AND_SWAP_PTR(&trie->indirects, NULL, newtab) )
	destroy_indirect_table(newtab);
    } else
    { return 0;
    }
  }
}


/* If there is an error, we prune the part that we have created.
 * We should only start the prune from a new node though.  To be sure
 * we do so we first add a new node.  As this is for exception handling
 * only, the performance loss is not vital.
 */

#define prune_error(trie, node) LDFUNC(prune_error, trie, node)
static void
prune_error(DECL_LD trie *trie, trie_node *node)
{ prune_node(trie, follow_node(trie, node, TRIE_ERROR_VAL, true));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lookup `k` in `trie` and on  success   fill  `nodep`  with the leaf node
under which `k` is represented in the trie.  If `add` is `true`, add `k`
to the trie if it is not already in the true.

`vars` is either NULL or a _buffer_ In the latter case it is filled with
pointers to the variables found  in  `k`.   This  is  used by tabling to
create the `ret` term.

Return:

  - false
    Could not find term while `add` is false or exception
  - true
    Ok (found or inserted)
  - TRIE_ABSTRACTED
    Ok, but abstracted
  - TRIE_LOOKUP_CONTAINS_ATTVAR (unless O_TRIE_ATTVAR is activated)
  - TRIE_LOOKUP_CYCLIC

# Variable handling

When a fresh variable is encountered, we push its location onto `vars`
and replace it with the variable number and the TAG_VAR.  Next time we
find this variable we simply use the stored word.  Thus, f(X,X) is

    <functor><var 1><var 1>POP

In  theory, we  could do  singleton  detection and  only use  variable
numbers for singletons.  The price is  though that this needs an extra
pass.

__Attributed__ variables are represented as  below, where the <N> uses
the same  numbering as for variables.   When pushing to the  stack, we
both both  push the  location of  the attvar and  the location  of the
attributes.

    <begin_attvar N> <attributes ...> <pop> <end_attvar N>

<begin_attvar N> is (N<<LMASK_BITS)|TAG_ATTVAR|STG_STATIC
<end_attvar N>   is (N<<LMASK_BITS)|TAG_ATTVAR|STG_RESERVED
<pop>		 is TRIE_KEY_POP(popn), compensating for the push
                    for writing the attributes and restoring read/write
                    umode.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct attvar_mark
{ size_t compound_depth;
  Word   attvar;
} attvar_mark;

int /* bool|TRIE_ABSTRACTED|TRIE_LOOKUP_CONTAINS_ATTVAR|TRIE_LOOKUP_CYCLIC */
trie_lookup_abstract(DECL_LD trie *trie, trie_node *node, trie_node **nodep,
		     Word k, bool add, size_abstract *abstract,
		     TmpBuffer vars)
{ term_agenda_P agenda;
  size_t var_number = 0;
  int rc = true;
  size_t compounds = 0;		/* Nesting depth */
  tmp_buffer varb;
  size_abstract sa = {.from_depth = 1, .size = (size_t)-1};
  size_t aleft = (size_t)-1;
#if O_TRIE_ATTVAR
  tmp_buffer attvarb;
  TmpBuffer attvars = NULL;
  attvar_mark *avm = NULL;
#endif

  TRIE_STAT_INC(trie, lookups);
  if ( !node )
    node = &trie->root;
  if ( abstract )
    sa = *abstract;

  initTermAgenda_P(&agenda, 1, k);
  while( node )
  { Word p;
    word w;
    size_t popn;

    if ( !(p=nextTermAgenda_P(&agenda)) )
      break;
    if ( (popn = IS_AC_TERM_POP(p)) )
    { compounds -= popn;
      if ( compounds > 0 )
      { if ( !(node = follow_node(trie, node, TRIE_KEY_POP(popn), add)) )
	  break;
#if O_TRIE_ATTVAR
	if ( avm && avm->compound_depth == compounds )
	{ word w = (*avm->attvar)|STG_RESERVED;
	  DEBUG(MSG_TRIE_PUT_TERM,
		Sdprintf("Finished attvar at %p\n", avm->attvar));
	  (void)popBufferP(attvars, attvar_mark);
	  if ( avm > baseBuffer(attvars, attvar_mark) )
	    avm--;
	  else
	    avm = NULL;
	  if ( !(node = follow_node(trie, node, w, add)) )
	    break;
	}
#endif
	continue;
      } else
      {
#if O_TRIE_ATTVAR
	if ( avm && avm->compound_depth == compounds )
	{ word w = (*avm->attvar)|STG_RESERVED;
	  DEBUG(MSG_TRIE_PUT_TERM,
		Sdprintf("Finished attvar at %p\n", avm->attvar));
	  if ( !(node = follow_node(trie, node, TRIE_KEY_POP(popn), add)) )
	    break;
	  node = follow_node(trie, node, w, add);
	}
#endif
	break;				/* finished toplevel */
      }
    }

    if ( compounds == sa.from_depth )
      aleft = sa.size;

    w = *p;
    switch( tag(w) )
    { case TAG_VAR:
	if ( isVar(w) )
	{ word w2;

	add_var:
	  if ( var_number++ == 0 && !vars )
	  { vars = &varb;
	    initBuffer(vars);
	  }
	  addBuffer(vars, p, Word);
	  w2 = ((((word)var_number))<<LMASK_BITS)|TAG_VAR;
	  if ( tag(w) == TAG_VAR ) /* otherwise abstracted */
	    *p = w2;
	  w = w2;
	}
        node = follow_node(trie, node, w, add);
	break;
      case TAG_ATTVAR:
#if O_TRIE_ATTVAR
	if ( tagex(w) != (TAG_ATTVAR|STG_STATIC) )
	{ Word ap = valPAttVar(w);
	  if ( var_number++ == 0 && !vars )
	  { vars = &varb;
	    initBuffer(vars);
	  }
	  addBuffer(vars, p, Word);
	  addBuffer(vars, ap, Word);
	  w = ((((word)var_number))<<LMASK_BITS)|TAG_ATTVAR|STG_STATIC;
	  *p = w;
	  node = follow_node(trie, node, w, add);
	  if ( !node )
	    break;
	  if ( !attvars )
	  { attvars = &attvarb;
	    initBuffer(attvars);
	  }
	  avm = allocFromBuffer(attvars, sizeof(*avm));
	  avm->compound_depth = compounds;
	  avm->attvar = p;
	  DEBUG(MSG_TRIE_PUT_TERM,
		Sdprintf("Opened attvar at %p\n", p));
	  compounds++;
	  pushWorkAgenda_P0(&agenda, 1, ap);
	} else
	{ DEBUG(MSG_TRIE_PUT_TERM,
		Sdprintf("Shared attvar at %p\n", p));
	  node = follow_node(trie, node, w, add);
	}
#else
	rc = TRIE_LOOKUP_CONTAINS_ATTVAR;
	prune_error(trie, node);
	node = NULL;
#endif
	break;
      case TAG_COMPOUND:
      { if ( unlikely(aleft == 0) )
	{ rc = TRIE_ABSTRACTED;
	  goto add_var;
	} else
	{ Functor f = valueTerm(w);
	  size_t arity = arityFunctor(f->definition);

	  if ( aleft != (size_t)-1 )
	    aleft--;
	  if ( ++compounds == 1000 && add && !is_acyclic(p) )
	  { rc = TRIE_LOOKUP_CYCLIC;
	    prune_error(trie, node);
	    node = NULL;
	  } else
	  { node = follow_node(trie, node, f->definition, add);
	    pushWorkAgenda_P(&agenda, arity, f->arguments);
	  }
	}
	break;
      }
      default:
      { if ( !isIndirect(w) )
	{ node = follow_node(trie, node, w, add);
	} else
	{ word i = trie_intern_indirect(trie, w, add);

	  if ( i )
	    node = follow_node(trie, node, i, add);
	  else
	    node = NULL;
	}
      }
    }
  }
  clearTermAgenda_P(&agenda);

  if ( var_number )
  { Word *pp = baseBuffer(vars, Word);
    Word *ep = topBuffer(vars, Word);

    for(; pp < ep; pp++)
    { Word vp = *pp;
      if ( tag(*vp) == TAG_VAR )
      { setVar(*vp);
#if O_TRIE_ATTVAR
      } else if ( tagex(*vp) == (TAG_ATTVAR|STG_STATIC) )
      { Word ap = *++pp;
	*vp = consPtr(ap, TAG_ATTVAR|STG_GLOBAL);
#endif
      }
    }
    if ( vars == &varb )
      discardBuffer(vars);
#if O_TRIE_ATTVAR
    if ( attvars == &attvarb )
      discardBuffer(attvars);
#endif
  }

  if ( rc > 0 )
  { if ( node )
      *nodep = node;
    else
      rc = false;
  }

  DEBUG(CHK_SECURE, checkStacks(NULL));

  return rc;
}


trie *
get_trie_from_node(trie_node *node)
{ trie *trie_ptr;

  for( ; node->parent; node = node->parent )
    ;
  trie_ptr = (trie *)((char*)node - offsetof(trie, root));
  assert(trie_ptr->magic == TRIE_MAGIC || trie_ptr->magic == TRIE_CMAGIC);

  return trie_ptr;
}


bool
is_ground_trie_node(trie_node *node)
{ for( ; node->parent; node = node->parent )
  { if ( tagex(node->key) == TAG_VAR
#if O_TRIE_ATTVAR
	 || tagex(node->key) == (TAG_ATTVAR|STG_STATIC)
#endif
       )
      return false;
  }

  return true;
}


		 /*******************************
		 *    BUILD TERM FROM PATH	*
		 *******************************/

#if 0					/* debugging help */
static int
print_key(word k)
{ size_t pop;

  if ( (pop = IS_TRIE_KEY_POP(k)) )
  { Sdprintf("POP(%zd)\n", pop);
  } else
  { char buf[64];

    Sdprintf("%s\n", print_val(k, buf));
  }

  return true;
}

static int
print_keys(Word k, size_t kc)
{ size_t i;

  for(i=kc; i-->0; )
    print_key(k[i]);

  return true;
}
#endif


#define MAX_FAST 256

bool
unify_trie_term(DECL_LD trie_node *node, trie_node **parent, term_t term)
{ word fast[MAX_FAST];
  Word keys = fast;
  size_t keys_allocated = MAX_FAST;
  size_t kc = 0;
  bool rc = true;
  trie *trie_ptr;
  mark m;
  int is_secondary = ison(node, TN_SECONDARY);
						/* get the keys */
  for( ; node->parent; node = node->parent )
  { if ( is_secondary && ison(node, TN_PRIMARY) )
    { if ( parent )
	*parent = node;
      break;
    }
    if ( kc == keys_allocated )
    { keys_allocated *= 2;
      if ( keys == fast )
      { if ( (keys = malloc(sizeof(*keys)*keys_allocated)) )
	  memcpy(keys, fast, sizeof(fast));
	else
	  return PL_resource_error("memory");
      } else
      { Word newkeys;
	if ( !(newkeys=realloc(keys, sizeof(*keys)*keys_allocated)) )
	{ free(keys);
	  return PL_resource_error("memory");
	}
	keys = newkeys;
      }
    }

    keys[kc++] = node->key;
  }
  for( ; node->parent; node = node->parent )
    ;
  trie_ptr = (trie *)((char*)node - offsetof(trie, root));
  assert(trie_ptr->magic == TRIE_MAGIC);

  for(;;)
  { ukey_state ustate;
    size_t i;

    init_ukey_state(&ustate, trie_ptr, valTermRef(term), false);
  retry:
    Mark(m);
    boolex_t rcu = true;
    for(i=kc; i-- > 0; )
    { rcu = unify_key(&ustate, keys[i]);

      if ( rcu != true )
	break;
    }

    if ( rcu != true )
    { if ( rcu == false )
      { destroy_ukey_state(&ustate);
	rc = false;
	goto out;
      }
      Undo(m);
      if ( (rc=makeMoreStackSpace(rcu, ALLOW_GC)) )
      { init_ukey_state(&ustate, trie_ptr, valTermRef(term), true);
	goto retry;
      } else
	goto out;
    }

    destroy_ukey_state(&ustate);
    break;
  }

out:
  if ( keys != fast )
    free(keys);

  DEBUG(CHK_SECURE, checkStacks(NULL));
  return rc;
}


void *
map_trie_node(trie_node *n,
	      void* (*map)(trie_node *n, void *ctx), void *ctx)
{ trie_children children;
  void *rc;

next:
  children = n->children;

  if ( (rc=(*map)(n, ctx)) != NULL )
    return rc;

  if ( children.any  )
  { switch( children.any->type )
    { case TN_KEY:
      { n = children.key->child;
	goto next;
      }
      case TN_HASHED:
      { TableWP table = children.hash->table;
	TableEnum e = newTableEnumWP(table);
	table_value_t v;

	while(advanceTableEnum(e, NULL, &v))
	{ trie_node *n2 = val2ptr(v);

	  if ( (rc=map_trie_node(n2, map, ctx)) != NULL )
	  { freeTableEnum(e);
	    return rc;
	  }
	}

	freeTableEnum(e);
	break;
      }
    }
  }

  return NULL;
}


typedef struct trie_stats
{ size_t bytes;
  size_t nodes;
  size_t hashes;
  size_t values;
} trie_stats;


static void *
stat_node(trie_node *n, void *ctx)
{ trie_stats *stats = ctx;
  trie_children children = n->children;

  stats->nodes++;
  stats->bytes += sizeof(*n);
  if ( n->value && ison(n, TN_PRIMARY) )
    stats->values++;

  if ( children.any )
  { switch( children.any->type )
    { case TN_KEY:
	stats->bytes += sizeof(*children.key);
        break;
      case TN_HASHED:
	stats->bytes += sizeofTableWP(children.hash->table);
	stats->hashes++;
	break;
      default:
	assert(0);
    }
  }

  return NULL;
}


static void
stat_trie(trie *t, trie_stats *stats)
{ stats->bytes  = sizeof(*t) - sizeof(t->root);
  stats->nodes  = 0;
  stats->hashes = 0;
  stats->values = 0;

  acquire_trie(t);
  map_trie_node(&t->root, stat_node, stats);
  release_trie(t);
}




		 /*******************************
		 *	  PROLOG BINDING	*
		 *******************************/

atom_t
trie_symbol(trie *trie)
{ if ( !trie->symbol )
  { tref ref;
    int new;

    ref.trie = trie;
    trie->symbol = lookupBlob((void*)&ref, sizeof(ref),
			      &trie_blob, &new);
  }

  return trie->symbol;
}


trie *
symbol_trie(atom_t symbol)
{ void *data;
  PL_blob_t *type;

  if ( (data = PL_blob_data(symbol, NULL, &type)) && type == &trie_blob )
  { tref *ref = data;

    if ( ref->trie->magic == TRIE_MAGIC )
      return ref->trie;
  }

  return NULL;
}



#define unify_trie(t, trie) LDFUNC(unify_trie, t, trie)
static bool
unify_trie(DECL_LD term_t t, trie *trie)
{ return PL_unify_atom(t, trie->symbol);
}

bool
get_trie(term_t t, trie **tp)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &trie_blob )
  { tref *ref = data;

    if ( ref->trie->magic == TRIE_MAGIC )
    { *tp = ref->trie;
      return true;
    }

    PL_existence_error("trie", t);
  } else
    PL_type_error("trie", t);

  return false;
}


bool
get_trie_noex(term_t t, trie **tp)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &trie_blob )
  { tref *ref = data;

    *tp = ref->trie;
    return true;
  }

  return false;
}


bool
trie_error(int rc, term_t culprit)
{ switch(rc)
  { case TRIE_LOOKUP_CONTAINS_ATTVAR:
      return PL_type_error("free_of_attvar", culprit);
    case TRIE_LOOKUP_CYCLIC:
      return PL_type_error("acyclic_term", culprit);
    default:
      return false;
  }
}

bool
trie_trie_error(int rc, trie *trie)
{ GET_LD
  term_t t;

  return ( (t= PL_new_term_ref()) &&
	   PL_put_atom(t, trie->symbol) &&
	   trie_error(rc, t) );
}


static
PRED_IMPL("trie_new", 1, trie_new, 0)
{ PRED_LD
  trie *trie;

  if ( (trie = trie_create(NULL)) )
  { atom_t symbol = trie_symbol(trie);
    bool rc;

    rc = unify_trie(A1, trie);
    PL_unregister_atom(symbol);

    return rc;
  }

  return false;
}


static
PRED_IMPL("is_trie", 1, is_trie, 0)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(A1, &data, NULL, &type) && type == &trie_blob )
  { tref *ref = data;

    if ( ref->trie->magic == TRIE_MAGIC )
      return true;
  }

  return false;
}


static
PRED_IMPL("trie_destroy", 1, trie_destroy, 0)
{ trie *trie;

  if ( get_trie(A1, &trie) )
  { trie->magic = TRIE_CMAGIC;
    trie_empty(trie);

    return true;
  }

  return false;
}


#define isRecord(w) (((w)&0x3) == 0)

#define intern_value(value) LDFUNC(intern_value, value)
static word
intern_value(DECL_LD term_t value)
{ if ( value )
  { Word vp = valTermRef(value);

    DEBUG(0, assert((TAG_INTEGER&0x3) && (TAG_ATOM&0x3)));

    deRef(vp);
    if ( isAtom(*vp) || isTaggedInt(*vp) )
      return *vp;

    return ptr2word(PL_record(value));
  } else
  { return ATOM_trienode;
  }
}


static inline void
release_value(word value)
{ if ( isAtom(value) )
    PL_unregister_atom(word2atom(value));
  else if ( isRecord(value) )
    PL_erase(word2ptr(record_t, value));
}


static int
equal_value(word v1, word v2)
{ if ( v1 == v2 )
    return true;

  if ( isRecord(v1) && isRecord(v2) )
    return variantRecords(word2ptr(record_t, v1), word2ptr(record_t, v2));

  return false;
}


#define unify_value(t, value) LDFUNC(unify_value, t, value)
static int
unify_value(DECL_LD term_t t, word value)
{ if ( !isRecord(value) )
  { return PL_unify_atomic(t, value);
  } else
  { term_t t2;

    return ( (t2=PL_new_term_ref()) &&
	     PL_recorded(word2ptr(record_t, value), t2) &&
	     PL_unify(t, t2)
	   );
  }
}


bool
put_trie_value(DECL_LD term_t t, trie_node *node)
{ if ( !isRecord(node->value) )
  { *valTermRef(t) = node->value;
    return true;
  } else
  { return PL_recorded(word2ptr(record_t, node->value), t);
  }
}


bool
set_trie_value_word(trie *trie, trie_node *node, word val)
{ if ( node->value )
  { if ( !equal_value(node->value, val) )
    { word old = node->value;

      acquire_key(val);
      node->value = val;
      set(node, TN_PRIMARY);
      release_value(old);
      trie_discard_clause(trie);

      return true;
    } else
    { return false;
    }
  } else
  { acquire_key(val);
    node->value = val;
    set(node, TN_PRIMARY);
    ATOMIC_INC(&trie->value_count);
    trie_discard_clause(trie);

    return true;
  }
}

bool
set_trie_value(DECL_LD trie *trie, trie_node *node, term_t value)
{ word val = intern_value(value);

  if ( !set_trie_value_word(trie, node, val) &&
       isRecord(val) )
    PL_erase(word2ptr(record_t, val));

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Delete a node from the trie. There are   two options: (1) simply set the
value to 0 or (2), prune the branch   leading to this cell upwards until
we find another existing node.

TBD: create some sort of  lingering   mechanism  to allow for concurrent
delete and gen_trie(). More modest: link up the deleted nodes and remove
them after the references drop to 0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
trie_delete(trie *trie, trie_node *node, int prune)
{ if ( node->value )
  { if ( ison(node, TN_PRIMARY) )
      ATOMIC_DEC(&trie->value_count);

    clear(node, (TN_PRIMARY|TN_SECONDARY));
    if ( prune && trie->references == 0 )
    { prune_node(trie, node);
    } else
    { word v;

      if ( trie->release_node )
	(*trie->release_node)(trie, node);

      if ( (v=node->value) )
      { node->value = 0;
	release_value(v);
      }
    }
    trie_discard_clause(trie);
  }
}


/**
 * trie_insert(+Trie, +Key, +Value) is semidet.
 *
 * True if Key was added as a new   key  to the trie and associated with
 * Value. False if Key was already in the trie with Value
 *
 * @error permission_error if Key was associated with a different value
 */

#define trie_insert(Trie, Key, Value, nodep, update, abstract) LDFUNC(trie_insert, Trie, Key, Value, nodep, update, abstract)
static bool
trie_insert(DECL_LD term_t Trie, term_t Key, term_t Value, trie_node **nodep,
	    int update, size_abstract *abstract)
{ trie *trie;

  if ( get_trie(Trie, &trie) )
  { Word kp;
    trie_node *node;
    int rc;

    if ( isoff(trie, TRIE_ISMAP|TRIE_ISSET) )
    { if ( Value )
	set(trie, TRIE_ISMAP);
      else
	set(trie, TRIE_ISSET);
    } else
    { if ( (Value  && isoff(trie, TRIE_ISMAP)) ||
	   (!Value && isoff(trie, TRIE_ISSET)) )
      { return PL_permission_error("insert", "trie", Trie),false;
      }
    }

    kp	= valTermRef(Key);

    if ( (rc=trie_lookup_abstract(trie, NULL, &node, kp,
				  true, abstract, NULL)) == true )
    { word val = intern_value(Value);

      if ( nodep )
	*nodep = node;

      if ( node->value )
      { if ( update )
	{ if ( !equal_value(node->value, val) )
	  { word old = node->value;

	    acquire_key(val);
	    node->value = val;
	    set(node, TN_PRIMARY);
	    release_value(old);
	    trie_discard_clause(trie);
	  } else if ( isRecord(val) )
	  { PL_erase(word2ptr(record_t, val));
	  }

	  return true;
	} else
	{ if ( !equal_value(node->value, val) )
	    PL_permission_error("modify", "trie_key", Key);
	  if ( isRecord(val) )
	    PL_erase(word2ptr(record_t, val));

	  return false;
	}
      }
      acquire_key(val);
      node->value = val;
      set(node, TN_PRIMARY);
      ATOMIC_INC(&trie->value_count);
      trie_discard_clause(trie);

      return true;
    }

    return trie_error(rc, Key);
  }

  return false;
}


/**
 * trie_insert(+Trie, +Key, +Value) is semidet.
 *
 * True if Key was added as a new   key  to the trie and associated with
 * Value. False if Key was already in the trie with Value
 *
 * @error permission_error if Key was associated with a different value
 */

static
PRED_IMPL("trie_insert", 3, trie_insert, 0)
{ PRED_LD

  return trie_insert(A1, A2, A3, NULL, false, NULL);
}

/**
 * trie_insert(+Trie, +Key) is semidet.
 *
 * True if Key was added as a new key to the trie.  False if Key was
 * already in the trie.
 *
 * @error permission_error if Key was associated with a different value
 */

static
PRED_IMPL("trie_insert", 2, trie_insert, 0)
{ PRED_LD

  return trie_insert(A1, A2, 0, NULL, false, NULL);
}


/**
 * trie_insert_abstract(+Trie, +Size, +Key) is semidet.
 *
 * Insert size-abstracted version of Key
 */

static
PRED_IMPL("$trie_insert_abstract", 3, trie_insert_abstract, 0)
{ PRED_LD
  size_abstract sa = {.from_depth = 1};

  return ( PL_get_size_ex(A2, &sa.size ) &&
	   trie_insert(A1, A3, 0, NULL, false, &sa) > 0 );
}


/**
 * trie_update(+Trie, +Key, +Value) is semidet.
 *
 * Similar to trie_insert/3, but updates the associated value rather
 * then failing or raising an error.
 *
 * @error permission_error if Key was associated with a different value
 */

static
PRED_IMPL("trie_update", 3, trie_update, 0)
{ PRED_LD

  return trie_insert(A1, A2, A3, NULL, true, NULL);
}


/**
 * trie_insert(+Trie, +Term, +Value, -Handle) is semidet.
 *
 * Add Term to Trie and unify Handle with a handle to the term.
 * Fails if Term is already in Trie.
 *
 * @bug Handle is currently a pointer.  In future versions we will
 * use a dynamic array for the trie nodes and return an integer to
 * guarantee safe lookup.
 */

static
PRED_IMPL("trie_insert", 4, trie_insert, 0)
{ PRED_LD
  trie_node *node = NULL;	/* avoid warning */

  return ( trie_insert(A1, A2, A3, &node, false, NULL) &&
	   PL_unify_pointer(A4, node) );
}


static
PRED_IMPL("trie_delete", 3, trie_delete, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);

    if ( (rc=trie_lookup(trie, NULL, &node, kp, false, NULL)) == true )
    { if ( node->value )
      { if ( unify_value(A3, node->value) )
	{ trie_delete(trie, node, true);
	  return true;
	}
      }
      return false;
    }

    return trie_error(rc, A2);
  }

  return false;
}


static
PRED_IMPL("trie_lookup", 3, trie_lookup, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);

    if ( (rc=trie_lookup(trie, NULL, &node, kp, false, NULL)) == true )
    { if ( node->value )
	return unify_value(A3, node->value);
      return false;
    }

    return trie_error(rc, A2);
  }

  return false;
}


/**
 * trie_term(+Handle, -Term) is det.
 *
 * Retrieve a term for a handle returned by trie_insert/4.
 */

static
PRED_IMPL("trie_term", 2, trie_term, 0)
{ PRED_LD
  void *ptr;

  return ( PL_get_pointer_ex(A1, &ptr) &&
	   unify_trie_term(ptr, NULL, A2)
	 );
}


/**
 * trie_gen(+Trie, ?Key, -Value) is nondet.
 *
 * True when Key-Value appears in Trie.
 *
 * This needs to keep  a  list  of   choice  points  for  each node with
 * multiple children. Eventually, this is probably going to be a virtual
 * machine extension, using real choice points.
 */

static void
init_ukey_state(DECL_LD ukey_state *state, trie *trie, Word p, bool reinit)
{ state->trie = trie;
  state->ptr  = p;
  state->umode = uread;
  state->a_offset = aTop-aBase;
  if ( unlikely(reinit) )
   emptyBuffer(&state->vars, (size_t)-1);
  else
    initBuffer(&state->vars);
}

static void
destroy_ukey_state(DECL_LD ukey_state *state)
{ discardBuffer(&state->vars);
  aTop = aBase + state->a_offset;
}

typedef struct varinfo
{ Word address;			/* Address of the var/value */
#if O_TRIE_ATTVAR
  Word attributes;		/* Location of attributes */
#endif
} varinfo;

static varinfo*
find_var(ukey_state *state, size_t index) /* index is 1.. */
{ if ( index > entriesBuffer(&state->vars, varinfo) )
  { varinfo *vi = allocFromBuffer(&state->vars, sizeof(varinfo));
    memset(vi, 0, sizeof(*vi));
  }

  return &baseBuffer(&state->vars, varinfo)[index-1];
}

/**
 * Take one  step in unifying  a term with  a sequence of  trie nodes.
 * `state->ptr` is where  we are in the  term and `key` is  the key of
 * the next  trie node to  consider.  The  `state` holds the  state of
 * this state machine.
 *
 * Like the VM  unification, this engine uses an  _argument stack_ and
 * distinguishes _read_  and _write_ mode.   We are in _read_  mode as
 * long as  we are  matching an existing  structure.  If  the existing
 * structure is  a variable and the  key is a functor,  we process the
 * arguments in _write_ mode.
 *
 * Handling attributed variables is complicated.  In _write_ mode, the
 * first encounter creates the attvar and subsequent create references
 * to it.   In _read_ mode,  we cannot simply unify  as we do  not yet
 * have  the attributes.   Thus,  we push  the  term-location and  the
 * attvar to `state->attvars`  and at the end we deal  with it.  If we
 * find  an attvar  in read  mode for  the second  time, we  unify the
 * corresponding term values.
 */

static boolex_t
unify_key(DECL_LD ukey_state *state, word key)
{ Word p = state->ptr;

#define POPN(n) do {					    \
    Word __wp;						    \
    aTop -= n;						    \
    __wp = *aTop;					    \
    state->umode = ((int)(uintptr_t)__wp & uwrite);	    \
    state->ptr   = (Word)((intptr_t)__wp &~uwrite);	    \
  } while(0)

  switch(tagex(key))
  { case TAG_VAR|STG_LOCAL:			/* RESERVED_TRIE_VAL */
    { size_t popn = IS_TRIE_KEY_POP(key);

      assert(popn);
      POPN(popn);

      DEBUG(MSG_TRIE_PUT_TERM,
	    Sdprintf("U Popped(%zd) %zd, mode=%d\n",
		     popn, state->ptr-gBase, state->umode));
      return true;
    }
    case TAG_ATOM|STG_GLOBAL:			/* functor */
    { size_t arity = arityFunctor(key);

      DEBUG(MSG_TRIE_PUT_TERM,
	    Sdprintf("U Pushed %s %zd, mode=%d\n",
		     functorName(word2functor(key)), state->ptr+1-gBase, state->umode));
      pushArgumentStack((Word)((intptr_t)(p + 1)|state->umode));

      if ( state->umode == uwrite )
      { Word t;

	if ( (t=allocGlobalNoShift(arity+1)) )
	{ t[0] = key;
	  *p = consPtr(t, TAG_COMPOUND|STG_GLOBAL);
	  state->ptr = &t[1];
	  return true;
	} else
	  return GLOBAL_OVERFLOW;
      } else
      { deRef(p);

	if ( canBind(*p) )
	{ state->umode = uwrite;

	  if ( isAttVar(*p) )
	  { Word t;
	    word w;
	    size_t i;

	    if ( (t=allocGlobalNoShift(arity+1)) )
	    { if ( !hasGlobalSpace(0) )
		return overflowCode(0);
	      w = consPtr(&t[0], TAG_COMPOUND|STG_GLOBAL);
	      t[0] = key;
	      for(i=0; i<arity; i++)
		setVar(t[i+1]);
	      assignAttVar(p, &w);
	      state->ptr = &t[1];
	      return true;
	    } else
	      return GLOBAL_OVERFLOW;
	  } else
	  { Word t;
	    size_t i;

	    if ( (t=allocGlobalNoShift(arity+1)) )
	    { if ( unlikely(tTop+1 >= tMax) )
		return  TRAIL_OVERFLOW;
	      t[0] = key;
	      for(i=0; i<arity; i++)
		setVar(t[i+1]);
	      Trail(p, consPtr(t, TAG_COMPOUND|STG_GLOBAL));
	      state->ptr = &t[1];
	      return true;
	    } else
	      return GLOBAL_OVERFLOW;
	  }
	} else if ( isTerm(*p) )
	{ Functor f = valueTerm(*p);

	  if ( f->definition == key )
	  { state->ptr = &f->arguments[0];
	    return true;
	  } else
	    return false;
	} else
	{ return false;
	}
      } /*uread*/
    }
    assert(0);
    case TAG_VAR:
    { size_t index = (size_t)(key>>LMASK_BITS);
      varinfo *vi = find_var(state, index);

      DEBUG(MSG_TRIE_PUT_TERM,
	    { char b1[64]; char b2[64];
	      Sdprintf("var %zd at %s (v=%p, *v=%s)\n",
		       index,
		       print_addr(state->ptr,b1),
		       vi, print_addr(vi->address,b2));
	    });

      if ( state->umode == uwrite )
      { if ( !vi->address )
	{ setVar(*state->ptr);
	  vi->address = state->ptr;
	} else
	{ *state->ptr = makeRefG(vi->address);
	}
      } else
      { deRef(p);

	if ( !vi->address )
	{ vi->address = state->ptr;
	} else
	{ int rc;

	  if ( (rc=unify_ptrs(state->ptr, vi->address, ALLOW_RETCODE)) != true )
	    return rc;
	}
      }

      break;
    }
    assert(0);
#if O_TRIE_ATTVAR
    case TAG_ATTVAR|STG_STATIC:
    { size_t index = (size_t)(key>>LMASK_BITS);
      varinfo *vi = find_var(state, index);

      DEBUG(MSG_TRIE_PUT_TERM,
	    Sdprintf("U Attvar %zd at %p\n", index, vi->address));

      if ( !vi->address )
      { pushArgumentStack((Word)((intptr_t)(p + 1)|state->umode));
	if ( hasGlobalSpace(0) )
	{ Word gp = gTop;
	  register_attvar(gp);
	  gp[1] = consPtr(&gp[2], TAG_ATTVAR|STG_GLOBAL);
	  state->ptr = &gp[2];
	  gTop += 3;
	  vi->address = p;
	  vi->attributes = &gp[1];
	  if ( state->umode == uwrite )
	    *p = makeRefG(&gp[1]);
	  else
	    state->umode = uwrite; /* wait for end (TAG_ATTVAR|STG_RESERVED) */

	  return true;
	} else
	  return GLOBAL_OVERFLOW;
      } else
      { if ( state->umode == uwrite )
	{ *state->ptr = makeRefG(vi->attributes);
	} else
	{ boolex_t rc = unify_ptrs(vi->address, p, ALLOW_RETCODE);
	  if ( rc != true )
	    return rc;
	}
	break;
      }
    }
    case TAG_ATTVAR|STG_RESERVED:
    { if ( state->umode == uread )
      { size_t index = (size_t)(key>>LMASK_BITS);
	varinfo *vi = find_var(state, index);
	if ( hasGlobalSpace(0) )
	{ Word p2;

	  deRef2(vi->address, p2);
	  if ( !isVar(*p2) )
	  { if ( ison(state->trie, TRIE_ISTABLE) )
	    { TrailAssignment(p2);
	      *p2 = makeRefG(vi->attributes);
	    } else
	    { assignAttVar(vi->attributes, p2);
	    }
	  } else
	  { boolex_t rc = unify_ptrs(vi->attributes, vi->address, ALLOW_RETCODE);
	    if ( rc != true )
	      return rc;
	  }
	} else
	  return GLOBAL_OVERFLOW;
      }
      return true;
    }
#endif /*O_TRIE_ATTVAR*/
    assert(0);
    case STG_GLOBAL|TAG_INTEGER:		/* indirect data */
    case STG_GLOBAL|TAG_STRING:
    case STG_GLOBAL|TAG_FLOAT:
    { word w;

      w = extern_indirect_no_shift(state->trie->indirects, key);
      if ( !w )
	return GLOBAL_OVERFLOW;

      if ( state->umode == uwrite )
      { *p = w;
      } else
      { deRef(p);

	if ( canBind(*p) )
	{ if ( hasGlobalSpace(0) )
	    bindConst(p, w);
	  else
	    return overflowCode(0);
	} else if ( tag(w) == tag(*p) && isIndirect(*p) )
	{ if ( !equalIndirect(w, *p) )
	    return false;
	} else
	{ return false;
	}
      }

      break;
    }
    case TAG_ATOM:
      pushVolatileAtom(word2atom(key));
      /*FALLTHROUGH*/
    case TAG_INTEGER:
    { DEBUG(MSG_TRIE_PUT_TERM,
	    char buf[64];
	    Sdprintf("U %s\n", print_val(key, buf)));
      if ( state->umode == uwrite )
      { *p = key;
      } else
      { deRef(p);

	if ( canBind(*p) )
	{ if ( hasGlobalSpace(0) )
	    bindConst(p, key);
	  else
	    return overflowCode(0);
	} else
	{ if ( *p != key )
	    return false;
	}
      }
      break;
    }
    default:
      assert(0);
  }

  state->ptr++;

  return true;

#undef POPN
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
trie_gen/2,3 walks the trie depth first.  The path it is on is a stack of
trie_choice records, one for each node on it: the child taken and, when
more children may match, how to find the next one.  A leaf is an answer
if unify_trie_path() unifies the keys of its path with the query, and
that unification decides every answer.

So as not to walk paths that unification rejects, the walk moves a
_query cursor_ (qcursor) through the query.  The keys of a path spell a
term in pre-order: a functor, constant or variable key per node, and
POP(n) where a compound that is not a last argument ends, closing n
compound levels (see trie_lookup_abstract() and nextTermAgenda_P()).
align_key() steps over the stored key S according to the query subterm
T at the cursor:

  T \ S        functor or constant     variable   attvar   POP(n)
  nonvar       cut unless S is T's     skip T     stop     stop
	       key; a functor enters T
  variable     skip T; a functor also  skip T     stop     stop
	       skips the stored args
  frame done   stop                    stop       stop     pop frame
  skipping     count S                 count S    stop     count S

"Cut" means no path through S unifies with the query, so the walk does
not enter S.  "Stop" ends pruning below S: either S starts an attributed
variable, whose attributes the cursor does not follow, or S cannot occur
there in a trie that matches the query's shape, which nb_setarg/3 can
cause by changing the query between answers; a path cut before such a
change is not visited again.  While skipping stored arguments, `need`
counts the stored subterms still unread and `open` the compounds opened
among them and not yet closed.  When `need` reaches 0 the next key is
POP(n): it closes the `open` compounds, and if n > open also the query
frame the skip ended, as last arguments share one POP.

Each choice that can advance keeps the cursor it was made with, so
trying its next child needs no walk.  A kept cursor points into the
query's compounds, and the frames it pops to are only appended to, so it
stays valid until garbage collection or a stack shift moves the global
stack, which changes the GC count or the stack's base: a new epoch.
restore_cursor() rebuilds the cursor of a choice made in an earlier
epoch by stepping the keys of the choices beneath it again.  No kept
cursor points at the term reference of the query itself, which is on
the local stack: the cursor before the root key reads it when used.
After an answer has bound the query, the search for the next candidate
goes on only while it reads no kept pruning cursor: reaching a choice
that keeps one, it stops and leaves a choice point, and backtracking,
which undoes the answer, continues it.  So a cursor always reads the
query as the caller made it, and an answer leaves a choice point only
if a candidate or a pruning choice with children left remains.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* A query frame with arguments left: where the walk resumes when the
   compound it entered ends.  Frames are only appended while the walk
   descends, so cursors share them by index. */

typedef struct qframe
{ Word	 term;				/* next argument */
  size_t size;				/* arguments left */
  Word	 last;				/* last nonvar argument, or NULL */
  bool	 rest;				/* a nonvar left here or below */
  size_t parent;			/* frame below, as index+1; 0: none */
} qframe;

/* `term` is NULL when `size` is 0, and before the root key, when `size`
   is 1 and the argument is the query itself. */

typedef struct qcursor
{ Word	 term;				/* next argument of the query */
  size_t size;				/* arguments left in this frame */
  Word	 last;				/* last nonvar argument, or NULL */
  size_t frame;				/* frame to pop to, as index+1 */
  size_t need;				/* stored subterms left to skip */
  size_t open;				/* compounds opened while skipping */
  bool	 prune;				/* false: no longer pruning */
} qcursor;

typedef struct trie_choice
{ TableEnum  table_enum;
  TableWP    table;
  unsigned   var_mask;			/* variables left to try */
  unsigned   attvar_mask;		/* attributed ones, tried next */
  unsigned   var_index;
  unsigned   epoch;			/* epoch `cursor` belongs to */
  word	     var_tag;			/* TAG_VAR or TAG_ATTVAR|STG_STATIC */
  word       novar;
  word       key;
  trie_node *child;
  size_t     frames;			/* frames in use when it was made */
  qcursor    cursor;			/* the cursor before this node */
} trie_choice;

typedef struct
{ trie	      *trie;		/* trie we operate on */
  int	       allocated;	/* If true, the state is persistent */
  unsigned     vflags;		/* TN_PRIMARY or TN_SECONDARY */
  unsigned     epoch;		/* Kept cursors of this epoch are valid */
  int64_t      gc_seen;		/* GC count the epoch started at */
  Word	       gbase_seen;	/* Global stack base the epoch started at */
  term_t       key;		/* The query */
  bool	       resume;		/* The top choice advanced, not yet descended */
  tmp_buffer   choicepoints;	/* Stack of trie state choicepoints */
  tmp_buffer   frames;		/* qframe records the cursors share */
} trie_gen_state;

#define advance_node(ch) LDFUNC(advance_node, ch)
static inline bool advance_node(DECL_LD trie_choice *ch);

#define init_trie_state(state, trie, root, key) \
	LDFUNC(init_trie_state, state, trie, root, key)

static void
init_trie_state(DECL_LD trie_gen_state *state, trie *trie,
		const trie_node *root, term_t key)
{ state->trie = trie;
  state->allocated = false;
  state->vflags = root == &trie->root ? TN_PRIMARY : TN_SECONDARY;
  state->epoch = 1;
  state->gc_seen = LD->gc.stats.totals.collections;
  state->gbase_seen = gBase;
  state->key = key;
  state->resume = false;
  initBuffer(&state->choicepoints);
  initBuffer(&state->frames);
}


#define base_choice(state) baseBuffer(&state->choicepoints, trie_choice)
#define top_choice(state) topBuffer(&state->choicepoints, trie_choice)


static void
clear_trie_state(trie_gen_state *state)
{ trie_choice *chp = base_choice(state);
  trie_choice *top = top_choice(state);

  for(; chp < top; chp++)
  { if ( chp->table_enum )
      freeTableEnum(chp->table_enum);
  }

  discardBuffer(&state->choicepoints);
  discardBuffer(&state->frames);

  release_trie(state->trie);

  if ( state->allocated )
    freeForeignState(state, sizeof(*state));
}

foreign_t
clear_trie_gen_state(void *ctx)
{ trie_gen_state *state = ctx;

  clear_trie_state(state);
  return true;
}

/* Start a new epoch if the global stack moved since this one started,
   so the cursors kept before are rebuilt when used. */

#define check_epoch(state) LDFUNC(check_epoch, state)

static void
check_epoch(DECL_LD trie_gen_state *state)
{ int64_t gcs = LD->gc.stats.totals.collections;

  if ( gcs != state->gc_seen || gBase != state->gbase_seen )
  { state->gc_seen    = gcs;
    state->gbase_seen = gBase;
    state->epoch++;
    DEBUG(MSG_TRIE_GEN, Sdprintf("trie_gen: epoch %u\n", state->epoch));
  }
}


static void
root_cursor(qcursor *c, bool prune)
{ c->term  = NULL;
  c->size  = 1;
  c->last  = NULL;
  c->frame = 0;
  c->need  = 0;
  c->open  = 0;
  c->prune = prune;
}


static void
skip_query(qcursor *c)
{ if ( --c->size )
    c->term++;
  else
    c->term = NULL;
}


/* Drop the frames pushed after the first `count`. */

static void
drop_frames(trie_gen_state *state, size_t count)
{ if ( count < entriesBuffer(&state->frames, qframe) )
    seekBuffer(&state->frames, count, qframe);
}


#define enter_query_compound(state, c, p) \
	LDFUNC(enter_query_compound, state, c, p)

static void
enter_query_compound(DECL_LD trie_gen_state *state, qcursor *c, Word p)
{ Functor f = valueTerm(*p);
  size_t arity = arityFunctor(f->definition);
  Word a;

  if ( c->size > 1 )			/* not the last argument */
  { qframe *fr = allocFromBuffer(&state->frames, sizeof(*fr));

    if ( !fr )
      outOfCore();
    fr->term   = c->term+1;
    fr->size   = c->size-1;
    fr->last   = c->last;
    fr->rest   = ( (c->last && c->last > c->term) ||
		   (c->frame &&
		    baseBuffer(&state->frames, qframe)[c->frame-1].rest) );
    fr->parent = c->frame;
    c->frame   = entriesBuffer(&state->frames, qframe);
    DEBUG(MSG_TRIE_GEN,
	  Sdprintf("Pushed %p, size %zd\n", fr->term, fr->size));
  }
  c->term = arity ? &f->arguments[0] : NULL;
  c->size = arity;
  c->last = NULL;
  for(a = &f->arguments[arity]; a-- > &f->arguments[0]; )
  { Word d = a;

    deRef(d);
    if ( !canBind(*d) )
    { c->last = a;
      break;
    }
  }
}


static bool
pop_query_frame(trie_gen_state *state, qcursor *c)
{ if ( c->frame )
  { qframe *fr = &baseBuffer(&state->frames, qframe)[c->frame-1];

    c->term  = fr->term;
    c->size  = fr->size;
    c->last  = fr->last;
    c->frame = fr->parent;
    DEBUG(MSG_TRIE_GEN,
	  Sdprintf("Popped %p, left %zd\n", c->term, c->size));
    return true;
  }

  return false;
}


/* True if a nonvar is left in the query after the cursor.  Called only
   after the root key, as the root's own frame records no `last`. */

static bool
concrete_ahead(trie_gen_state *state, const qcursor *c)
{ return ( (c->size && c->last && c->term <= c->last) ||
	   (c->frame && baseBuffer(&state->frames, qframe)[c->frame-1].rest) );
}


typedef enum
{ Q_DONE,				/* frame has no argument left */
  Q_VAR,				/* variable, attributed or not */
  Q_KEY					/* nonvar, with its trie key */
} query_token;

#define peek_query(state, c, kp, pp) \
	LDFUNC(peek_query, state, c, kp, pp)

static query_token
peek_query(DECL_LD trie_gen_state *state, const qcursor *c,
	   word *kp, Word *pp)
{ Word p;

  if ( c->size == 0 )
    return Q_DONE;

  p = c->term ? c->term : valTermRef(state->key);
  deRef(p);
  DEBUG(CHK_SECURE, checkData(p));
  if ( canBind(*p) )
    return Q_VAR;

  if ( isTerm(*p) )
    *kp = valueTerm(*p)->definition;
  else if ( !isIndirect(*p) )
    *kp = *p;
  else if ( !(*kp = trie_intern_indirect(state->trie, *p, false)) )
    *kp = TRIE_KEY_ABSENT;		/* no stored constant equals it */
  *pp = p;

  return Q_KEY;
}


/* Step the cursor over the stored key `skey` as the table above says.
   Returns false, leaving the cursor as it was, if no path through `skey`
   can unify with the query, and true otherwise.  Once no nonvar is left
   in the query nothing can be cut, so pruning stops. */

/* A cursor that no longer prunes accepts every key without a call. */
#define align_key(state, c, skey) \
	( !(c)->prune || align_pruning_key(state, c, skey) )
#define align_pruning_key(state, c, skey) \
	LDFUNC(align_pruning_key, state, c, skey)

static bool
align_pruning_key(DECL_LD trie_gen_state *state, qcursor *c, word skey)
{ size_t popn;
  bool functor;
  word qkey = 0;
  Word p = NULL;

  if ( !skey )				/* the root is the leaf */
    return true;
  popn    = IS_TRIE_KEY_POP(skey);
  functor = (tagex(skey) == (TAG_ATOM|STG_GLOBAL));
  if ( tag(skey) == TAG_ATTVAR )
    goto stop;

  if ( c->open )			/* skipping stored arguments */
  { if ( c->need )
    { if ( popn )
      { if ( popn > c->open )
	  goto stop;
	c->open -= popn;
      } else
      { c->need--;
	if ( functor )
	{ c->need += arityFunctor(skey);
	  c->open++;
	}
      }
      return true;
    }
    if ( popn < c->open )		/* only a POP can end the skip */
      goto stop;
    popn -= c->open;
    c->open = 0;
    DEBUG(MSG_TRIE_GEN, Sdprintf("Skip ended, POP %zd left\n", popn));
    if ( popn && (c->size || !pop_query_frame(state, c)) )
      goto stop;
    goto stepped;
  }

  switch( peek_query(state, c, &qkey, &p) )
  { case Q_DONE:
      if ( popn && pop_query_frame(state, c) )
	goto stepped;
      goto stop;
    case Q_VAR:
      if ( popn )
	goto stop;
      skip_query(c);
      if ( functor )
      { c->need = arityFunctor(skey);
	c->open = 1;
      }
      goto stepped;
    case Q_KEY:
      if ( popn )
	goto stop;
      if ( tagex(skey) == TAG_VAR || skey == qkey )
      { if ( functor )
	  enter_query_compound(state, c, p);
	else
	  skip_query(c);
	goto stepped;
      }
      DEBUG(MSG_TRIE_GEN, Sdprintf("Cut %s\n", print_val(skey, NULL)));
      return false;
  }

stepped:
  if ( concrete_ahead(state, c) )
    return true;
  DEBUG(MSG_TRIE_GEN, Sdprintf("No nonvar left after %s\n",
			       print_val(skey, NULL)));
stop:
  DEBUG(MSG_TRIE_GEN,
	Sdprintf("Stop pruning at %s\n", print_val(skey, NULL)));
  c->prune = false;
  return true;
}


/* Keep in `ch`, a choice that can advance, the cursor before its node:
   restore_cursor() reads it when `ch` has advanced.  A choice with one
   child keeps none. */

static inline void
keep_cursor(trie_gen_state *state, trie_choice *ch, const qcursor *c)
{ if ( c->prune )
  { ch->epoch  = state->epoch;
    ch->frames = entriesBuffer(&state->frames, qframe);
    ch->cursor = *c;
  }
}


/* Push the choice for `node` and step the cursor over the key of the
   child it takes first.  Returns NULL if no child can match.  With the
   query bound here, a hash lookup finds the equal key and var_mask and
   attvar_mask the variable ones; else the children are enumerated.
   Time: one align_key() per child tried, all of them when enumerating. */

#define add_choice(state, c, node) \
	LDFUNC(add_choice, state, c, node)

static trie_choice *
add_choice(DECL_LD trie_gen_state *state,
	   qcursor *c, trie_node *node)
{ trie_children children = node->children;
  trie_choice *ch;

  if ( !children.any || ison(node, state->vflags) )
  { ch = allocFromBuffer(&state->choicepoints, sizeof(*ch));
    memset(ch, 0, sizeof(*ch));
    ch->child = node;
    return ch;
  }

  ch = allocFromBuffer(&state->choicepoints, sizeof(*ch));
  ch->table_enum = NULL;
  ch->table      = NULL;
  ch->cursor.prune = false;

  switch( children.any->type )
  { case TN_KEY:
      if ( !align_key(state, c, children.key->key) )
	break;
      ch->key   = children.key->key;
      ch->child = children.key->child;
      return ch;
    case TN_HASHED:
    { trie_children_hashed *hnode = children.hash;
      unsigned vmask = hnode->var_mask;
      unsigned amask = hnode->attvar_mask;
      word k = 0;
      Word p;

      if ( c->prune && !c->open &&
	   peek_query(state, c, &k, &p) == Q_KEY &&
	   !((vmask|amask) & VMASK_SCAN) )
      { if ( !(vmask|amask) )
	{ if ( !(ch->child = lookupHTableWP(hnode->table, k)) )
	    break;
	  ch->key = k;
	} else
	{ DEBUG(MSG_TRIE_GEN,
		Sdprintf("Created var choice 0x%x 0x%x\n", vmask, amask));
	  keep_cursor(state, ch, c);
	  ch->table       = hnode->table;
	  ch->var_mask    = vmask;
	  ch->attvar_mask = amask;
	  ch->var_tag     = TAG_VAR;
	  ch->var_index   = 1;
	  ch->novar       = k;
	  if ( !advance_node(ch) )
	    break;
	}
	(void)align_key(state, c, ch->key); /* k or a variable: no cut */
	return ch;
      }

      /* A root keeps its hashed children table when trie_delete/3 prunes
	 its last key (prune_node() destroys every other node that empties),
	 so the enumeration may produce no child at all. */
      keep_cursor(state, ch, c);
      ch->table_enum = newTableEnumWP(hnode->table);
      for(;;)
      { table_key_t tk;
	table_value_t tv;

	if ( !advanceTableEnum(ch->table_enum, &tk, &tv) )
	{ freeTableEnum(ch->table_enum);
	  break;
	}
	if ( align_key(state, c, tk) )
	{ ch->key   = tk;
	  ch->child = val2ptr(tv);
	  return ch;
	}
      }
      break;
    }
    default:
      assert(0);
  }

  state->choicepoints.top = (char*)ch;	/* no child can match */
  return NULL;
}


#define descent_node(state, c, ch) \
	LDFUNC(descent_node, state, c, ch)

static trie_choice *
descent_node(DECL_LD trie_gen_state *state,
	     qcursor *c, trie_choice *ch)
{ while( ch && ch->child->children.any &&
	 isoff(ch->child, state->vflags) )
  { ch = add_choice(state, c, ch->child);
  }

  return ch;
}


/* Move a choice to its next child: of an enumerated hashed node, or of a
   var choice the equal key, then each variable key var_mask names, then
   each attributed one attvar_mask names. */

static inline bool
advance_node(DECL_LD trie_choice *ch)
{ if ( ch->table_enum )
  { table_key_t k;
    table_value_t v;

    if ( advanceTableEnum(ch->table_enum, &k, &v) )
    { ch->key   = k;
      ch->child = val2ptr(v);

      return true;
    }
  } else if ( ch->table )
  { if ( ch->novar )
    { word novar = ch->novar;

      ch->novar = 0;
      if ( (ch->child=lookupHTableWP(ch->table, novar)) )
      { ch->key = novar;
	return true;
      }
    }
    for(;;)
    { for( ; ch->var_index && ch->var_index < VMASKBITS; ch->var_index++ )
      { if ( (ch->var_mask & (0x1U<<(ch->var_index-1))) )
	{ word key = ((((word)ch->var_index))<<LMASK_BITS)|ch->var_tag;

	  if ( (ch->child=lookupHTableWP(ch->table, key)) )
	  { ch->key = key;
	    ch->var_index++;
	    return true;
	  }
	}
      }
      if ( !ch->attvar_mask )
	break;
      ch->var_mask    = ch->attvar_mask;
      ch->attvar_mask = 0;
      ch->var_tag     = TAG_ATTVAR|STG_STATIC;
      ch->var_index   = 1;
    }
  }

  return false;
}


/* Set `c` to the cursor before the node of `upto`.  A choice made after
   pruning stopped kept no cursor and gets one that does not prune, of
   which nothing but `prune` is read.  A cursor kept in this epoch is
   copied, and the frames pushed after it are dropped.  Otherwise the
   cursor is rebuilt from the root by stepping the keys of the choices
   beneath `upto`, which renews the kept
   cursors of those choices too; as no choice above `upto` is left and
   none beneath it was kept in this epoch, the frames start empty.  Key
   has kept its shape since the choices were made unless nb_setarg/3
   changed it; then pruning stops where the shapes disagree, and paths
   cut before the change stay cut.
   Time: O(1) in the epoch `upto` was kept in, else d align_key() steps,
   d = upto - base_choice(state). */

#define restore_cursor(state, upto, c) \
	LDFUNC(restore_cursor, state, upto, c)

static void
restore_cursor(DECL_LD trie_gen_state *state, trie_choice *upto,
	       qcursor *c)
{ trie_choice *ch;

  if ( !upto->cursor.prune )
  { c->prune = false;
    return;
  }
  if ( upto->epoch == state->epoch )
  { drop_frames(state, upto->frames);
    *c = upto->cursor;
    return;
  }

  DEBUG(MSG_TRIE_GEN,
	Sdprintf("trie_gen: replaying %zd keys\n",
		 (size_t)(upto-base_choice(state))));
  drop_frames(state, 0);
  root_cursor(c, true);
  for(ch = base_choice(state); ; ch++)
  { ch->epoch  = state->epoch;
    ch->frames = entriesBuffer(&state->frames, qframe);
    ch->cursor = *c;
    if ( ch == upto )
      break;
    if ( !align_key(state, c, ch->key) )
      c->prune = false;
  }
}


/* Backtrack to the next candidate leaf.  A descent that a cut ends below
   the choice it advanced leaves its choices on the stack, so the walk
   resumes from the top rather than below the advanced choice, and reads
   the stack's bounds again because the descent may have reallocated it.
   With `bound`, Key is bound to an answer, so the search stops at the
   first choice it advances whose kept cursor prunes, sets state->resume
   and returns that choice; the next call, with Key unbound again, goes
   on from it.
   Time: per choice advanced, restore_cursor() and the descent from its
   child. */

#define next_choice0(state, bound) LDFUNC(next_choice0, state, bound)

static trie_choice *
next_choice0(DECL_LD trie_gen_state *state, bool bound)
{ trie_choice *btm = base_choice(state);
  trie_choice *ch  = top_choice(state)-1;
  bool advanced = state->resume;	/* ch was advanced by the last call */

  assert(!(advanced && bound));
  state->resume = false;
  while( ch >= btm )
  { if ( advanced || advance_node(ch) )
    { qcursor before;

      advanced = false;
      if ( bound && ch->cursor.prune )	/* restoring it would read Key */
      { state->resume = true;
	return ch;
      }
      restore_cursor(state, ch, &before);
      do
      { qcursor c, *cp = &before;	/* only a pruning cursor changes */

	if ( before.prune )
	{ c  = before;
	  cp = &c;
	}
	if ( align_key(state, cp, ch->key) )
	{ trie_choice *leaf;

	  if ( (leaf=descent_node(state, cp, ch)) )
	    return leaf;
	  btm = base_choice(state);
	  ch  = top_choice(state)-1;
	  goto next;
	}
      } while( advance_node(ch) );
    }

    if ( ch->table_enum )
      freeTableEnum(ch->table_enum);
    state->choicepoints.top = (char*)ch--;
  next:
    ;
  }

  return NULL;
}


#define next_choice(state, bound) LDFUNC(next_choice, state, bound)
static trie_choice *
next_choice(DECL_LD trie_gen_state *state, bool bound)
{ trie_choice *ch;

  if ( !bound )				/* else no kept cursor is read */
    check_epoch(state);
  do
  { ch = next_choice0(state, bound);
  } while ( ch && !state->resume && isoff(ch->child, state->vflags) );

  return ch;
}


static trie_node *
gen_state_leaf(trie_gen_state *state)
{ trie_choice *top = top_choice(state);

  return top[-1].child;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify term with the term represented by the keys of the choices on the
stack, the candidate's path.
Returns one of true, false or *_OVERFLOW.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define unify_trie_path(term, state) \
	LDFUNC(unify_trie_path, term, state)

static boolex_t
unify_trie_path(DECL_LD term_t term, trie_gen_state *state)
{ boolex_t rc = true;
  ukey_state ustate;
  trie_choice *ch = base_choice(state);
  trie_choice *top = top_choice(state);

  init_ukey_state(&ustate, state->trie, valTermRef(term), false);
  for( ; ch < top; ch++ )
  { if ( (rc=unify_key(&ustate, ch->key)) != true )
      break;
  }
  destroy_ukey_state(&ustate);
  DEBUG(0, if ( rc == true ) PL_check_data(term));

  return rc;
}


/* Move the contents of a buffer into a buffer at another address.  One
   that still uses its static part is copied, as its base points into
   the struct holding it; a larger one changes owner. */

static void
move_buffer(TmpBuffer to, TmpBuffer from)
{ if ( from->base == from->static_buffer )
  { size_t bytes = from->top - from->base;

    initBuffer(to);
    to->top = to->base + bytes;
    memcpy(to->base, from->base, bytes);
  } else
  { to->base = from->base;
    to->top  = from->top;
    to->max  = from->max;
  }
}


/* Enumerate the leaves below `root` whose path unifies with Key.  After
   an answer, next_choice() positions the choice stack on the next
   candidate, or stops at a pruning choice the redo goes on from; an
   answer after which it finds neither leaves no choice point.
   Time: per candidate, its path's unification and the next_choice()
   that finds the candidate after it. */

foreign_t
trie_gen_raw(trie *trie, trie_node *root, term_t Key, term_t Value,
	     term_t Data,
	     bool LDFUNCP (*unify_data)(DECL_LD term_t, trie_node*, void *ctx),
	     void *ctx, control_t PL__ctx)
{ PRED_LD
  trie_gen_state state_buf;
  trie_gen_state *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { trie_choice *ch;
      qcursor c;

      TRIE_STAT_INC(trie, gen_call);

      acquire_trie(trie);
      state = &state_buf;
      init_trie_state(state, trie, root, Key);
      root_cursor(&c, true);
      if ( (ch = add_choice(state, &c, root)) )
	ch = descent_node(state, &c, ch);
      if ( !(ch && ison(ch->child, state->vflags)) &&
	   !next_choice(state, false) )
      { clear_trie_state(state);
	return false;
      }
      break;
    }
    case FRG_REDO:
      state = CTX_PTR;
      state->key = Key;			/* this activation's reference */
      if ( state->resume && !next_choice(state, false) )
      { clear_trie_state(state);
	return false;
      }
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      clear_trie_state(state);
      return true;
    default:
      assert(0);
      return false;
  }

  Mark(fli_context->mark);
  while( !isEmptyBuffer(&state->choicepoints) )
  { trie_node *n = gen_state_leaf(state);

    if ( isoff(n, state->vflags) )	/* deleted after it was found */
    { next_choice(state, false);
      continue;
    }

    for(;;)
    { boolex_t rc;
      size_t asize = aTop - aBase; /* using the argument stack may be dubious */

      if ( (rc=unify_trie_path(Key, state)) == true )
	break;

      aTop = aBase+asize;
      Undo(fli_context->mark);
      if ( rc == false )
	goto next;

      if ( makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	continue;

      clear_trie_state(state);
      return false;				/* resource error */
    }

    DEBUG(CHK_SECURE, PL_check_data(Key));

    if ( (!Value || unify_value(Value, n->value)) &&
	 (!Data  || LDFUNCP(*unify_data)(Data, n, ctx)) )
    { if ( next_choice(state, true) )	/* Key is bound to the answer */
      { if ( !state->allocated )
	{ trie_gen_state *nstate = allocForeignState(sizeof(*state));

	  nstate->trie = state->trie;
	  nstate->vflags = state->vflags;
	  nstate->allocated = true;
	  nstate->epoch = state->epoch;
	  nstate->gc_seen = state->gc_seen;
	  nstate->gbase_seen = state->gbase_seen;
	  nstate->key = state->key;
	  nstate->resume = state->resume;
	  move_buffer(&nstate->choicepoints, &state->choicepoints);
	  move_buffer(&nstate->frames, &state->frames);

	  state = nstate;
	}
	ForeignRedoPtr(state);
      } else
      { clear_trie_state(state);
	return true;
      }
    } else
    { Undo(fli_context->mark);
    }

  next:
    next_choice(state, false);		/* Undo() unbound Key */
  }

  clear_trie_state(state);
  return false;
}


foreign_t
trie_gen(term_t Trie, term_t Root, term_t Key, term_t Value,
	 term_t Data,
	 bool LDFUNCP (*unify_data)(DECL_LD term_t, trie_node*, void *ctx),
	 void *ctx, control_t PL__ctx)
{ if ( CTX_CNTRL == FRG_FIRST_CALL )
  { trie *trie;
    trie_node *root;

    if ( get_trie(Trie, &trie) )
    { if ( Root )
      { void *ptr;

	if ( !PL_get_pointer_ex(Root, &ptr) )
	  return false;
	root = ptr;
      } else
      { root = &trie->root;
      }

      if ( root->children.any )
	return trie_gen_raw(trie, root, Key, Value, Data,
			    unify_data, ctx, PL__ctx);
    }

    return false;
  } else
  { return trie_gen_raw(NULL, NULL, Key, Value, Data,
			unify_data, ctx, PL__ctx);
  }
}

static
PRED_IMPL("trie_gen", 3, trie_gen, PL_FA_NONDETERMINISTIC)
{ return trie_gen(A1, 0, A2, A3, 0, NULL, NULL, PL__ctx);
}

static
PRED_IMPL("trie_gen", 2, trie_gen, PL_FA_NONDETERMINISTIC)
{ return trie_gen(A1, 0, A2, 0, 0, NULL, NULL, PL__ctx);
}

#define unify_node_id(t, answer, ctx) \
	LDFUNC(unify_node_id, t, answer, ctx)

static bool
unify_node_id(DECL_LD term_t t, trie_node *answer, void *ctx)
{ (void) ctx;

  return PL_unify_pointer(t, answer);
}

static
PRED_IMPL("$trie_gen_node", 3, trie_gen_node, PL_FA_NONDETERMINISTIC)
{ return trie_gen(A1, 0, A2, 0, A3, LDFUNC_REF(unify_node_id), NULL, PL__ctx);
}



static
PRED_IMPL("$trie_property", 2, trie_property, 0)
{ PRED_LD
  trie *trie;

#if O_TRIE_STATS
  static atom_t ATOM_lookup_count = 0;
  static atom_t ATOM_gen_call_count = 0;
  static atom_t ATOM_invalidated = 0;
  static atom_t ATOM_reevaluated = 0;

  if ( !ATOM_lookup_count )
  { ATOM_lookup_count   = PL_new_atom("lookup_count");
    ATOM_gen_call_count = PL_new_atom("gen_call_count");
    ATOM_invalidated    = PL_new_atom("invalidated");
    ATOM_reevaluated    = PL_new_atom("reevaluated");
  }
#endif

  if ( get_trie(A1, &trie) )
  { atom_t name; size_t arity;
    idg_node *idg;

    if ( PL_get_name_arity(A2, &name, &arity) && arity == 1 )
    { term_t arg = PL_new_term_ref();

      _PL_get_arg(1, A2, arg);

      if ( name == ATOM_node_count )
      { return PL_unify_integer(arg, trie->node_count);
      } else if ( name == ATOM_value_count )
      { return PL_unify_integer(arg, trie->value_count);
      } else if ( name == ATOM_size )
      { trie_stats stats;
	stat_trie(trie, &stats);
	if ( stats.nodes != trie->node_count )
	  Sdprintf("OOPS: trie_property/2: counted %zd nodes, admin says %zd\n",
		   (size_t)stats.nodes, (size_t)trie->node_count);
	if ( stats.values != trie->value_count )
	  Sdprintf("OOPS: trie_property/2: counted %zd values, admin says %zd\n",
		   (size_t)stats.values, (size_t)trie->value_count);
	// assert(stats.nodes  == trie->node_count);
	// assert(stats.values == trie->value_count);
	return PL_unify_int64(arg, stats.bytes);
      } else if ( name == ATOM_compiled_size )
      { atom_t dbref;
	if ( (dbref = trie->clause) )
	{ ClauseRef cref = clause_clref(dbref);
	  if ( cref )
	  { size_t sz = sizeofClause(cref->value.clause->code_size);
	    return PL_unify_int64(arg, sz);
	  }
	}
	return false;
      } else if ( name == ATOM_hashed )
      { trie_stats stats;
	stat_trie(trie, &stats);
	return PL_unify_int64(arg, stats.hashes);
#if O_TRIE_STATS
      } else if ( name == ATOM_lookup_count )
      { return PL_unify_int64(arg, trie->stats.lookups);
      } else if ( name == ATOM_gen_call_count)
      { return PL_unify_int64(arg, trie->stats.gen_call);
#ifdef O_PLMT
      } else if ( name == ATOM_wait )
      { return PL_unify_int64(arg, trie->stats.wait);
      } else if ( name == ATOM_deadlock )
      { return PL_unify_int64(arg, trie->stats.deadlock);
#endif
      } else if ( name == ATOM_invalidated && (idg=trie->data.IDG))
      { return PL_unify_int64(arg, idg->stats.invalidated);
      } else if ( name == ATOM_reevaluated && (idg=trie->data.IDG))
      { return PL_unify_int64(arg, idg->stats.reevaluated);
#endif
      } else if ( (idg=trie->data.IDG) )
      { if ( name == ATOM_idg_affected_count )
	{ return PL_unify_int64(arg, idg->affected ? idg->affected->size : 0);
	} else if ( name == ATOM_idg_dependent_count )
	{ return PL_unify_int64(arg, idg->dependent ? idg->dependent->size : 0);
	} else if ( name == ATOM_idg_size )
	{ size_t size = sizeof(*idg);

	  if ( idg->affected )  size += sizeofTablePP(idg->affected);
	  if ( idg->dependent ) size += sizeofTablePP(idg->dependent);

	  return PL_unify_int64(arg, size);
	}
      }
    }
  }

  return false;
}

#ifdef O_NESTED_TRIES

		 /*******************************
		 *	 HIERARCHICAL TRIES	*
		 *******************************/

/** trie_insert_insert(+Trie, +Index, +Value)
*/

static
PRED_IMPL("trie_insert_insert", 3, trie_insert_insert, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp, vp;
    trie_node *root, *node;
    tmp_buffer vars;
    int rc;

    initBuffer(&vars);
    kp	= valTermRef(A2);
    vp	= valTermRef(A3);

    rc = trie_lookup(trie, NULL, &root, kp, true, &vars);
    if ( rc == true )
    { rc = trie_lookup(trie, root, &node, vp, true, &vars);

      if ( rc == true )
      { set(root, TN_PRIMARY);
	root->value = ATOM_trienode;
	set(node, TN_SECONDARY);
	node->value = ATOM_trienode;
      } else
      { rc = trie_error(rc, A1);
      }
    } else
    { rc = trie_error(rc, A1);
    }

    discardBuffer(&vars);
    return rc;
  }

  return false;
}


static
PRED_IMPL("trie_lookup_gen", 3, trie_lookup_gen, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *root;
    int rc;

    kp = valTermRef(A2);
    rc = trie_lookup(trie, NULL, &root, kp, false, NULL);
    if ( rc == true )
    { return trie_gen_raw(trie, root, A3, 0, 0, NULL, NULL, PL__ctx);
    } else
    { rc = trie_error(rc, A1);
    }

    return rc;
  }

  return false;
}


static
PRED_IMPL("trie_lookup_delete", 3, trie_lookup_delete, 0)
{ PRED_LD

  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *root;
    int rc;

    kp = valTermRef(A2);
    rc = trie_lookup(trie, NULL, &root, kp, false, NULL);
    if ( rc == true )
    { Word vp = valTermRef(A3);
      trie_node *node;

      rc = trie_lookup(trie, root, &node, vp, false, NULL);
      if ( rc == true )
      { trie_delete(trie, node, true);
      } else
      { rc = trie_error(rc, A1);
      }
    } else
    { rc = trie_error(rc, A1);
    }

    return rc;
  }

  return false;
}

#endif /*O_NESTED_TRIES*/


		 /*******************************
		 *	  COMPILED TRIES	*
		 *******************************/

typedef struct trie_compile_state
{ trie	       *trie;				/* Trie we are working on */
  bool		try;				/* There are alternatives */
  bool		last_is_fail;			/* Ends in I_FAIL */
#if O_TRIE_ATTVAR
  bool		has_attvars;			/* Trie contains attvars */
#endif
  size_t	else_loc;			/* last else */
  size_t	maxvar;				/* Highest var index */
  tmp_buffer	codes;				/* Output instructions */
} trie_compile_state;

static void
init_trie_compile_state(trie_compile_state *state, trie *trie)
{ memset(state, 0, sizeof(*state));
  state->trie = trie;
  initBuffer(&state->codes);
  state->maxvar = 0;
}

static void
clean_trie_compile_state(trie_compile_state *state)
{ discardBuffer(&state->codes);
}


static void
add_vmi(trie_compile_state *state, vmi c)
{ addBuffer(&state->codes, encode(c), code);
}

static void
add_vmi_d(trie_compile_state *state, vmi c, code d)
{ addBuffer(&state->codes, encode(c), code);
  addBuffer(&state->codes, d, code);
}

static void
add_vmi_w(trie_compile_state *state, vmi c, word d)
{ addBuffer(&state->codes, encode(c), code);
  addBuffer(&state->codes, d, word);
}


static void
add_vmi_else_d(trie_compile_state *state, vmi c, code d)
{ size_t el;

  addBuffer(&state->codes, encode(c), code);
  el = entriesBuffer(&state->codes, code);
  addBuffer(&state->codes, (code)state->else_loc, code);
  state->else_loc = el;
  addBuffer(&state->codes, d, code);
}

static void
add_vmi_else_w(trie_compile_state *state, vmi c, word d)
{ size_t el;

  addBuffer(&state->codes, encode(c), code);
  el = entriesBuffer(&state->codes, code);
  addBuffer(&state->codes, (code)state->else_loc, code);
  state->else_loc = el;
  addBuffer(&state->codes, d, word);
}

static void
fixup_else(trie_compile_state *state)
{ Code base = baseBuffer(&state->codes, code);
  size_t pc = entriesBuffer(&state->codes, code);
  size_t el = state->else_loc;

  state->else_loc = base[el];
  base[el] = pc-el-1;
}

/* Add an indirect to the compiled trie.   Indirects are stored
 * using the header field, followed by the content.  I.e., the
 * same format as on the stacks, except that the trailing guard
 * is missing.  Therefore we write `wsize+1` words.
 */

static void
add_indirect(trie_compile_state *state, vmi op, word w)
{ Word p = addressIndirect(w);
  size_t wsize = wsizeofInd(*p);

  add_vmi(state, op);
  addMultipleBuffer(&state->codes, p, wsize+1, word);
}

static void
add_smallint(trie_compile_state *state, vmi c1, vmi c2, word w)
{ sword i = valInt(w);
#if CODES_PER_WORD == 1
  add_vmi_d(state, c1, (scode)i);
#else
  if ( (scode)i == i )
  { add_vmi_d(state, c1, (scode)i);
  } else
  { add_vmi(state, c2);
    addMultipleBuffer(&state->codes, &i, CODES_PER_WORD, code);
  }
#endif
}

static void
add_vmi_else_i(trie_compile_state *state, vmi c1, vmi c2, word w)
{ size_t el;
  sword i = valInt(w);
  vmi op;
#if CODES_PER_WORD == 1
  op = c1;
#else
  op =  (scode)i == i ? c1 : c2;
#endif

  addBuffer(&state->codes, encode(op), code);
  el = entriesBuffer(&state->codes, code);
  addBuffer(&state->codes, (code)state->else_loc, code);
  state->else_loc = el;
#if CODES_PER_WORD == 1
  addBuffer(&state->codes, i, scode);
#else
  if ( (scode)i == i )
    addBuffer(&state->codes, (scode)i, scode);
  else
    addMultipleBuffer(&state->codes, &i, CODES_PER_WORD, code);
#endif
}

/* If the  trie is a  map (rather than  a set), the  non-compiled trie
 * stores  the  value  as  a  record.   This  function  is  called  by
 * compile_trie_node()  on the  value extracted  from the  record.  It
 * translates the value into T_* instructions that are very similar to
 * the H_* unification instructions.
 */

#define compile_trie_value(v, state) LDFUNC(compile_trie_value, v, state)

static bool
compile_trie_value(DECL_LD Word v, trie_compile_state *state)
{ term_agenda_P agenda;
  size_t var_number = 0;
  size_t voffset = state->maxvar;
  tmp_buffer varb;
  int rc = true;
  ssize_t compounds = 0;
  Word p;
#if O_TRIE_ATTVAR
  tmp_buffer attvarb;
  TmpBuffer attvars = NULL;
  attvar_mark *avm = NULL;
#endif

  initTermAgenda_P(&agenda, 1, v);
  while( (p=nextTermAgenda_P(&agenda)) )
  { size_t popn;

    if ( (popn = IS_AC_TERM_POP(p)) )
    { compounds -= popn;

      if ( popn == 1 )
	add_vmi(state, T_POP);
      else
	add_vmi_d(state, T_POPN, (code)popn);

#if O_TRIE_ATTVAR
      if ( avm && avm->compound_depth == compounds )
      { size_t index = (*avm->attvar)>>LMASK_BITS;
	add_vmi_d(state, T_ATTVARZ, (code)index);
      }
#endif
    } else
    { word w = *p;

      switch( tag(w) )
      { case TAG_VAR:
	{ size_t index;

	  if ( isVar(w) )
	  { if ( var_number++ == 0 )
	      initBuffer(&varb);
	    index = var_number+voffset;
	    if ( index > state->maxvar )
	      state->maxvar = index;
	    addBuffer(&varb, p, Word);
	    *p = w = ((((word)index))<<LMASK_BITS)|TAG_VAR;
	  } else
	  { index = (size_t)(w>>LMASK_BITS);
	  }
	  add_vmi_d(state, T_VAR, (code)index);
	  break;
	}
	case TAG_ATTVAR:
#if O_TRIE_ATTVAR
	  if ( tagex(w) != (TAG_ATTVAR|STG_STATIC) )
	  { Word ap = valPAttVar(w);
	    if ( var_number++ == 0 )
	      initBuffer(&varb);
	    size_t index = var_number+voffset;
	    if ( index > state->maxvar )
	      state->maxvar = var_number;
	    state->has_attvars = true;

	    addBuffer(&varb, p, Word);
	    addBuffer(&varb, ap, Word);
	    w = ((((word)index))<<LMASK_BITS)|TAG_ATTVAR|STG_STATIC;
	    *p = w;
	    add_vmi_d(state, T_ATTVARA, (code)index);
	    if ( !attvars )
	    { attvars = &attvarb;
	      initBuffer(attvars);
	    }
	    avm = allocFromBuffer(attvars, sizeof(*avm));
	    avm->compound_depth = compounds;
	    avm->attvar = p;
	    DEBUG(MSG_TRIE_PUT_TERM,
		  Sdprintf("Opened attvar at %p\n", p));
	    compounds++;
	    pushWorkAgenda_P0(&agenda, 1, ap);
	  } else
	  { DEBUG(MSG_TRIE_PUT_TERM,
		  Sdprintf("Shared attvar at %p\n", p));
	    size_t index = w>>LMASK_BITS;
	    add_vmi_d(state, T_ATTVARA, (code)index);
	  }
	  break;
#else
	  rc = TRIE_LOOKUP_CONTAINS_ATTVAR;
	  goto out;
#endif
	case TAG_ATOM:			/* TBD: register */
	  add_vmi_d(state, T_ATOM, (code)w);
	  break;
	case TAG_INTEGER:
	  if ( storage(w) == STG_INLINE)
	  { add_smallint(state, T_SMALLINT, T_SMALLINTW, w);
#ifdef O_BIGNUM
	  } else
	  { add_indirect(state, T_MPZ, w);
#endif
	  }
	  break;
	case TAG_FLOAT:
	{ Code ip = (Code)valIndirectP(w);
	  add_vmi_d(state, T_FLOAT, ip[0]);
#if SIZEOF_CODE == 4
	  static_assertion(sizeof(double) == sizeof(code)*2);
	  addBuffer(&state->codes, ip[1], code);
#endif
          break;
	}
	case TAG_STRING:
	{ add_indirect(state, T_STRING, w);
	  break;
	}
	case TAG_COMPOUND:
	{ Functor f = valueTerm(w);
	  size_t arity = arityFunctor(f->definition);

	  if ( ++compounds == 1000 && !is_acyclic(p) )
	  { rc = TRIE_LOOKUP_CYCLIC;
	    goto out;
	  }
	  add_vmi_d(state, T_FUNCTOR, (code)f->definition);
	  pushWorkAgenda_P(&agenda, arity, f->arguments);
	  break;
	}
      }
    }
  }
out:
  clearTermAgenda_P(&agenda);

  if ( var_number )
  { Word *pp = baseBuffer(&varb, Word);
    Word *ep = topBuffer(&varb, Word);

    for(; pp < ep; pp++)
    { Word vp = *pp;
      if ( tag(*vp) == TAG_VAR )
      { setVar(*vp);
#if O_TRIE_ATTVAR
      } else if ( tagex(*vp) == (TAG_ATTVAR|STG_STATIC) )
      { Word ap = *++pp;
	*vp = consPtr(ap, TAG_ATTVAR|STG_GLOBAL);
#endif
      }
    }
    discardBuffer(&varb);
#if O_TRIE_ATTVAR
    if ( attvars == &attvarb )
      discardBuffer(attvars);
#endif
  }

  if ( rc != true )
  { rc = trie_error(rc, pushWordAsTermRef(v));
    popTermRef();
  }

  return rc;
}


#define compile_trie_node(n, state) LDFUNC(compile_trie_node, n, state)

static bool
compile_trie_node(DECL_LD trie_node *n, trie_compile_state *state)
{ trie_children children;
  word key;
  bool rc;

next:
  children = n->children;
  if ( n == &state->trie->root )
    goto children;
  key = n->key;

  switch(tagex(key))
  { case TAG_VAR|STG_LOCAL:			/* RESERVED_TRIE_VAL */
    { size_t popn = IS_TRIE_KEY_POP(key);

      assert(popn);
      if ( popn == 1 )
	add_vmi(state, T_POP);
      else
	add_vmi_d(state, T_POPN, (code)popn);
      break;
    }
    case TAG_ATOM|STG_GLOBAL:			/* functor */
    { if ( state->try )
	add_vmi_else_d(state, T_TRY_FUNCTOR, (code)key);
      else
	add_vmi_d(state, T_FUNCTOR, (code)key);
      break;
    }
#if O_TRIE_ATTVAR
    case TAG_ATTVAR|STG_STATIC:
    case TAG_ATTVAR|STG_RESERVED:
      state->has_attvars = true;
#endif
    /*FALLTHROUGH*/
    case TAG_VAR:
    { size_t index = (size_t)(key>>LMASK_BITS);
      vmi c;

      if ( index > state->maxvar )
	state->maxvar = index;

      if ( state->try )
      { switch(tagex(key))
	{ case TAG_VAR:                 c = T_TRY_VAR; break;
	  case TAG_ATTVAR|STG_STATIC:   c = T_TRY_ATTVARA; break;
	  case TAG_ATTVAR|STG_RESERVED:
	    if ( ison(state->trie, TRIE_ISTABLE) )
	      c = T_TRY_ATTVARZT;
	    else
	      c = T_TRY_ATTVARZ;
	    break;
	  default: assert(0);           c = 0;
	}
	add_vmi_else_d(state, c, (code)index);
      } else
      { switch(tagex(key))
	{ case TAG_VAR:                 c = T_VAR; break;
	  case TAG_ATTVAR|STG_STATIC:   c = T_ATTVARA; break;
	  case TAG_ATTVAR|STG_RESERVED:
	    if ( ison(state->trie, TRIE_ISTABLE) )
	      c = T_ATTVARZT;
	    else
	      c = T_ATTVARZ;
	    break;
	  default: assert(0);		c = 0;
	}
	add_vmi_d(state, c, (code)index);
      }
      break;
    }
    case STG_GLOBAL|TAG_INTEGER:		/* indirect data */
    case STG_GLOBAL|TAG_STRING:
    case STG_GLOBAL|TAG_FLOAT:
    { size_t index = (size_t)(key>>LMASK_BITS);
      int idx = MSB(index);
      indirect *h = &state->trie->indirects->array.blocks[idx][index];
      size_t wsize = wsizeofInd(h->header);

      switch(tag(key))
      { case TAG_INTEGER:
	  { if ( state->try )
	      add_vmi_else_w(state, T_TRY_MPZ, h->header);
	    else
	      add_vmi_w(state, T_MPZ, h->header);
	  }
	  break;
        case TAG_STRING:
	  if ( state->try )
	    add_vmi_else_w(state, T_TRY_STRING, h->header);
	  else
	    add_vmi_w(state, T_STRING, h->header);
	  break;
        case TAG_FLOAT:
	  { static_assertion(sizeof(h->data[0]) == sizeof(double));
	    if ( state->try )
	      add_vmi_else_w(state, T_TRY_FLOAT, h->data[0]);
	    else
	      add_vmi_w(state, T_FLOAT, h->data[0]);
	    goto indirect_done;
	  }
      }

      addMultipleBuffer(&state->codes, h->data, wsize, word);
    indirect_done:
      break;
    }
    case TAG_ATOM:
    { if ( state->try )
	add_vmi_else_d(state, T_TRY_ATOM, code2atom(key));
      else
	add_vmi_d(state, T_ATOM, code2atom(key));
      break;
    }
    case TAG_INTEGER:
    { if ( state->try )
	add_vmi_else_i(state, T_TRY_SMALLINT, T_TRY_SMALLINTW, key);
      else
	add_smallint(state, T_SMALLINT, T_SMALLINTW, key);
      break;
    }
    default:
      assert(0);
  }

children:
  if ( children.any && isoff(n, TN_PRIMARY|TN_SECONDARY) )
  { switch( children.any->type )
    { case TN_KEY:
      { state->try = false;
	n = children.key->child;
	goto next;
      }
      case TN_HASHED:
      { TableWP table = children.hash->table;
	TableEnum e = newTableEnumWP(table);
	table_key_t k;
	table_value_t v;

	if ( !advanceTableEnum(e, &k, &v) )
	{ freeTableEnum(e);
	  return true;				/* empty path */
	}

	for(;;)
	{ n = val2ptr(v);

	  if ( !(state->try = advanceTableEnum(e, &k, &v)) )
	  { freeTableEnum(e);
	    goto next;
	  }

	  if ( !compile_trie_node(n, state) )
	  { freeTableEnum(e);
	    return false;
	  }
	  fixup_else(state);
	}
      }
    }
  } else
  { if ( n->value )			/* what if we have none? */
    { if ( answer_is_conditional(n) )
	add_vmi_d(state, T_DELAY, ptr2code(n));

      if ( ison(state->trie, TRIE_ISMAP) )
      { add_vmi(state, T_VALUE);
	if ( !isRecord(n->value) )
	{ if ( isAtom(n->value) )
	  { add_vmi_d(state, T_ATOM, word2code(n->value));
	  } else
	  { add_smallint(state, T_SMALLINT, T_SMALLINTW, n->value);
	  }
	} else
	{ term_t t2;

	  if ( (t2=PL_new_term_ref()) &&
	       PL_recorded(word2ptr(record_t, n->value), t2) )
	  { Word p = valTermRef(t2);

	    deRef(p);
	    if ( (rc = compile_trie_value(p, state)) != true )
	      return rc;
	  } else
	  { return false;
	  }
	  PL_reset_term_refs(t2);
	}
      }
      add_vmi(state, T_CHECKWAKEUP);
      add_vmi(state, I_EXIT);
      state->last_is_fail = false;
    } else
    { add_vmi(state, I_FAIL);
      state->last_is_fail = true;
    }
  }

  DEBUG(CHK_SECURE, checkStacks(NULL));
  return true;
}


static bool
fixup_last_fail(trie_compile_state *state)
{ if ( state->last_is_fail )
    add_vmi(state, I_EXIT);	/* make sure the clause ends with I_EXIT */
  return true;
}


static bool
create_trie_clause(Definition def, Clause *cp, trie_compile_state *state)
{ size_t code_size = entriesBuffer(&state->codes, code);
  size_t size      = sizeofClause(code_size);
//size_t clsize    = size + SIZEOF_CREF_CLAUSE;
  Clause cl;

  cl = PL_malloc_atomic(size);
  memset(cl, 0, sizeof(*cl));
  cl->predicate = def;
  cl->code_size = code_size;
  cl->prolog_vars = (clsize_t)(TRIE_VAR_OFFSET + state->maxvar);
  cl->variables = cl->prolog_vars;	/* 2: pseudo arity */
  set(cl, UNIT_CLAUSE);			/* no body */
  memcpy(cl->codes, baseBuffer(&state->codes, code),
	 sizeOfBuffer(&state->codes));
  *cp = cl;

  ATOMIC_ADD(&GD->statistics.codes, cl->code_size);
  ATOMIC_INC(&GD->statistics.clauses);

  return true;
}


atom_t
compile_trie(DECL_LD Definition def, trie *trie)
{ atom_t dbref;

retry:
  if ( !(dbref = trie->clause) )
  { if ( trie->value_count == 0 )
    { dbref = ATOM_fail;
      if ( !COMPARE_AND_SWAP_ATOM(&trie->clause, 0, dbref) )
	goto retry;
    } else
    { trie_compile_state state;
      Clause cl;
      ClauseRef cref;
      fid_t fid;

      init_trie_compile_state(&state, trie);
      add_vmi(&state, def->functor->arity == 2 ? T_TRIE_GEN2 : T_TRIE_GEN3);
      if ( (fid=PL_open_foreign_frame()) )
      { if ( compile_trie_node(&trie->root, &state) &&
	     fixup_last_fail(&state) &&
	     create_trie_clause(def, &cl, &state) )
	{ cref = assertDefinition(def, cl, CL_END);
	  if ( cref )
	  { dbref = lookup_clref(cref->value.clause);
	    if ( !COMPARE_AND_SWAP_ATOM(&trie->clause, 0, dbref) )
	    { PL_unregister_atom(dbref);
	      retractClauseDefinition(def, cref->value.clause, false);
	      PL_close_foreign_frame(fid);
	      goto retry;
	    }
	  }
	} else
	{ dbref = ATOM_error;
	}
	PL_close_foreign_frame(fid);
      } else
      { dbref = ATOM_error;
      }
      assert(state.else_loc == 0);
      clean_trie_compile_state(&state);
    }
  }

  return dbref;
}


static
PRED_IMPL("$trie_compile", 2, trie_compile, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Procedure proc = (ison(trie, TRIE_ISMAP)
			     ? GD->procedures.trie_gen_compiled3
			     : GD->procedures.trie_gen_compiled2);
    atom_t clref = compile_trie(proc->definition, trie);

    return PL_unify_atomic(A2, clref);
  }

  return false;
}


static void
set_trie_clause_general_undefined(Clause clause)
{ Code PC, ep;

  PC = clause->codes;
  ep = PC + clause->code_size;

  for( ; PC < ep; PC = stepPC(PC) )
  { code c = fetchop(PC);

    switch(c)
    { case T_DELAY:
	PC[1] = ptr2code(NULL);
        break;
    }
  }
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define NDET PL_FA_NONDETERMINISTIC

BeginPredDefs(trie)
  PRED_DEF("is_trie",		    1, is_trie,		     0)
  PRED_DEF("trie_new",		    1, trie_new,	     0)
  PRED_DEF("trie_destroy",	    1, trie_destroy,	     0)
  PRED_DEF("trie_insert",	    2, trie_insert,	     0)
  PRED_DEF("trie_insert",	    3, trie_insert,	     0)
  PRED_DEF("trie_insert",	    4, trie_insert,	     0)
  PRED_DEF("$trie_insert_abstract", 3, trie_insert_abstract, 0)

  PRED_DEF("trie_update",	    3, trie_update,	     0)
  PRED_DEF("trie_lookup",	    3, trie_lookup,	     0)
  PRED_DEF("trie_delete",	    3, trie_delete,	     0)
  PRED_DEF("trie_term",		    2, trie_term,	     0)
  PRED_DEF("trie_gen",		    3, trie_gen,	     NDET)
  PRED_DEF("trie_gen",		    2, trie_gen,	     NDET)
  PRED_DEF("$trie_gen_node",	    3, trie_gen_node,	     NDET)
  PRED_DEF("$trie_property",	    2, trie_property,	     0)
#if O_NESTED_TRIES
  PRED_DEF("trie_insert_insert",    3, trie_insert_insert,   0)
  PRED_DEF("trie_lookup_gen",       3, trie_lookup_gen,      NDET)
  PRED_DEF("trie_lookup_delete",    3, trie_lookup_delete,   0)
#endif
  PRED_DEF("$trie_compile",         2, trie_compile,         0)
EndPredDefs

void
initTries(void)
{ Procedure proc;
  Definition def;

  PL_register_blob_type(&trie_blob);

  proc = PL_predicate("trie_gen_compiled", 2, "system");
  def = proc->definition;
  set(def, P_LOCKED_SUPERVISOR|P_VOLATILE);
  def->codes = SUPERVISOR(trie_gen);
  GD->procedures.trie_gen_compiled2 = proc;

  proc = PL_predicate("trie_gen_compiled", 3, "system");
  def = proc->definition;
  set(def, P_LOCKED_SUPERVISOR|P_VOLATILE);
  def->codes = SUPERVISOR(trie_gen);
  GD->procedures.trie_gen_compiled3 = proc;
}
