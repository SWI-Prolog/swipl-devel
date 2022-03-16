/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016-2021, VU University Amsterdam
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
  - Make trie_gen/3 take the known prefix into account
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define RESERVED_TRIE_VAL(n) (((word)((uintptr_t)n)<<LMASK_BITS) | \
			      TAG_VAR|STG_LOCAL)
#define TRIE_ERROR_VAL       RESERVED_TRIE_VAL(1)
#define TRIE_KEY_POP(n)      RESERVED_TRIE_VAL(10+(n))

#define IS_TRIE_KEY_POP(w)   ((tagex(w) == (TAG_VAR|STG_LOCAL) && \
			       ((w)>>LMASK_BITS) > 10) ? ((w)>>LMASK_BITS) - 10 \
						       : 0)

#define NVARS_FAST 100

/* Will eventually be shared in pl-wam.c */
typedef enum
{ uread = 0,				/* Unification in read-mode */
  uwrite				/* Unification in write mode */
} unify_mode;

typedef struct ukey_state
{ trie	       *trie;			/* Trie for indirects */
  Word		ptr;			/* current location */
  unify_mode	umode;			/* unification mode */
  size_t	max_var_seen;
  size_t	vars_allocated;		/* # variables allocated */
  Word*		vars;
  size_t        a_offset;		/* For resetting the argument stack */
  Word		var_buf[NVARS_FAST];	/* quick var buffer */
} ukey_state;

#if USE_LD_MACROS
#define	unify_key(state, key)			LDFUNC(unify_key, state, key)
#define	init_ukey_state(state, trie, p)		LDFUNC(init_ukey_state, state, trie, p)
#define	destroy_ukey_state(state)		LDFUNC(destroy_ukey_state, state)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static int	unify_key(ukey_state *state, word key);
static void	init_ukey_state(ukey_state *state, trie *trie, Word p);
static void	destroy_ukey_state(ukey_state *state);
static void	set_trie_clause_general_undefined(Clause cl);

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
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC a trie. Note that the  Prolog predicate trie_destroy/1 merely empties
the trie, leaving its destruction to the atom garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_trie_ref(atom_t aref)
{ tref *ref = PL_blob_data(aref, NULL, NULL);
  trie *t;

  if ( (t=ref->trie) )
    trie_destroy(t);			/* can be called twice */

  return TRUE;
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
  PL_BLOB_UNIQUE,
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
    PL_register_atom(key);
}

static inline void
release_key(word key)
{ if ( isAtom(key) )
    PL_unregister_atom(key);
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


void
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
  { if ( COMPARE_AND_SWAP_WORD(&trie->clause, dbref, 0) &&
	 GD->cleaning == CLN_NORMAL )		/* otherwise reclaims clause */
    { ClauseRef cref = clause_clref(dbref);	/* from two ends */

      if ( cref )
      { Clause cl = cref->value.clause;
	set_trie_clause_general_undefined(cl);	/* TBD: only if undefined */
	retractClauseDefinition(cl->predicate, cl, FALSE);
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

    clear_node(trie, &trie->root, FALSE);	/* TBD: verify not accessed */
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
	return lookupHTable(children.hash->table, (void*)key);
      default:
	assert(0);
    }
  }

  return NULL;
}


int
is_leaf_trie_node(trie_node *n)
{ trie_children children = n->children;

  if ( children.any )
  { switch( children.any->type )
    { case TN_KEY:
	return FALSE;
      case TN_HASHED:
	return children.hash->table->size == 0;
      default:
	assert(0);
    }
  }

  return TRUE;
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
	dealloc = TRUE;
	goto next;
      }
      case TN_HASHED:
      { Table table = children.hash->table;
	TableEnum e = newTableEnum(table);
	void *k, *v;
	trie_children_key *os;

	if ( (os=children.hash->old_single) )	/* see insert_child() (*) note */
	  free_to_pool(trie->alloc_pool, os, sizeof(*os));
	free_to_pool(trie->alloc_pool, children.hash, sizeof(*children.hash));

	while(advanceTableEnum(e, &k, &v))
	{ clear_node(trie, v, TRUE);
	}

	freeTableEnum(e);
	destroyHTable(table);
	break;
      }
    }
  }
}

static void
destroy_node(trie *trie, trie_node *n)
{ clear_node(trie, n, TRUE);
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
  int empty = TRUE;

  for(; empty && n->parent && false(n, TN_PRIMARY|TN_SECONDARY); n = p)
  { trie_children children;

    p = n->parent;
    children = p->children;

    if ( children.any )
    { switch( children.any->type )
      { case TN_KEY:
	  if ( COMPARE_AND_SWAP_PTR(&p->children.any, children.any, NULL) )
	    free_to_pool(trie->alloc_pool, children.key, sizeof(*children.key));
	  break;
	case TN_HASHED:
	  deleteHTable(children.hash->table, (void*)n->key);
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
	{ Table table = children.hash->table;
	  TableEnum e = newTableEnum(table);
	  void *k, *v;

	  if ( advanceTableEnum(e, &k, &v) )
	  { if ( !pushSegStack(&stack, ps, prune_state) )
	      outOfCore();
	    ps.e = e;
	    ps.n = n;

	    n = v;
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
    for(; n != root && false(n, TN_PRIMARY|TN_SECONDARY); n = p)
    { trie_children children;
      int choice = FALSE;

      p = n->parent;
      children = p->children;

      if ( children.any )
      { switch( children.any->type )
	{ case TN_KEY:
	    if ( COMPARE_AND_SWAP_PTR(&p->children.any, children.any, NULL) )
	      free_to_pool(trie->alloc_pool, children.key, sizeof(*children.key));
	    break;
	  case TN_HASHED:
	    deleteHTable(children.hash->table, (void*)n->key);
	    choice = TRUE;
	    break;
	}
      }

      destroy_node(trie, n);
      if ( choice )
	break;
    }

  next_choice:
    if ( ps.e )
    { void *k, *v;

      if ( advanceTableEnum(ps.e, &k, &v) )
      { n = v;
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
#define VMASK_SCAN (0x1<<(VMASKBITS-1))

static inline void
update_var_mask(trie_children_hashed *hnode, word key)
{ if ( tagex(key) == TAG_VAR )
  { size_t vn = (size_t)(key>>LMASK_BITS); /* 1.. */
    unsigned mask;

    if ( vn < VMASKBITS )
      mask = 0x1<<(vn-1);
    else
      mask = VMASK_SCAN;

    ATOMIC_OR(&hnode->var_mask, mask);
  }
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
	    hnode->table    = newHTable(4);
	    hnode->var_mask = 0;
	    addHTable(hnode->table, (void*)children.key->key,
				    children.key->child);
	    addHTable(hnode->table, (void*)key, (void*)new);
	    update_var_mask(hnode, children.key->key);
	    update_var_mask(hnode, new->key);
	    new->parent = n;

	    if ( COMPARE_AND_SWAP_PTR(&n->children.hash, children.hash, hnode) )
	    { hnode->old_single = children.key;			/* See (*) */
	      return new;
	    } else
	    { hnode->old_single = NULL;
	      destroy_node(trie, new);
	      destroyHTable(hnode->table);
	      free_to_pool(trie->alloc_pool, hnode, sizeof(*hnode));
	      continue;
	    }
	  }
	}
	case TN_HASHED:
	{ trie_node *old = addHTable(children.hash->table,
				     (void*)key, (void*)new);

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


#define follow_node(trie, n, value, add) LDFUNC(follow_node, trie, n, value, add)
static trie_node *
follow_node(DECL_LD trie *trie, trie_node *n, word value, int add)
{ trie_node *child;

  if ( (child=get_child(n, value)) )
    return child;

  if ( add )
    return insert_child(trie, n, value);
  else
    return NULL;
}


#define trie_intern_indirect(trie, w, add) LDFUNC(trie_intern_indirect, trie, w, add)
static word
trie_intern_indirect(DECL_LD trie *trie, word w, int add)
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
{ prune_node(trie, follow_node(trie, node, TRIE_ERROR_VAL, TRUE));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lookup `k` in `trie` and on  success   fill  `nodep`  with the leaf node
under which `k` is represented in the trie.  If `add` is `TRUE`, add `k`
to the trie if it is not already in the true.

`vars` is either NULL or a _buffer_ In the latter case it is filled with
pointers to the variables found  in  `k`.   This  is  used by tabling to
create the `ret` term.

Return:

  - FALSE
    Could not find term while `add` is FALSE or exception
  - TRUE
    Ok (found or inserted)
  - TRIE_ABSTRACTED
    Ok, but abstracted
  - TRIE_LOOKUP_CONTAINS_ATTVAR
  - TRIE_LOOKUP_CYCLIC

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
trie_lookup_abstract(DECL_LD trie *trie, trie_node *node, trie_node **nodep,
		     Word k, int add, size_abstract *abstract,
		     TmpBuffer vars)
{ term_agenda_P agenda;
  size_t var_number = 0;
  int rc = TRUE;
  size_t compounds = 0;
  tmp_buffer varb;
  size_abstract sa = {.from_depth = 1, .size = (size_t)-1};
  size_t aleft = (size_t)-1;

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
	continue;
      } else
	break;				/* finished toplevel */
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
	  if ( tag(w) == TAG_VAR )
	    *p = w2;
	  w = w2;
	}
        node = follow_node(trie, node, w, add);
	break;
      case TAG_ATTVAR:
	rc = TRIE_LOOKUP_CONTAINS_ATTVAR;

        prune_error(trie, node);
        node = NULL;
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
	setVar(*vp);
    }
    if ( vars == &varb )
      discardBuffer(vars);
  }

  if ( rc > 0 )
  { if ( node )
      *nodep = node;
    else
      rc = FALSE;
  }

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


int
is_ground_trie_node(trie_node *node)
{ for( ; node->parent; node = node->parent )
  { if ( tagex(node->key) == TAG_VAR )
      return FALSE;
  }

  return TRUE;
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

  return TRUE;
}

static int
print_keys(Word k, size_t kc)
{ size_t i;

  for(i=kc; i-->0; )
    print_key(k[i]);

  return TRUE;
}
#endif


#define MAX_FAST 256

int
unify_trie_term(DECL_LD trie_node *node, trie_node **parent, term_t term)
{ word fast[MAX_FAST];
  Word keys = fast;
  size_t keys_allocated = MAX_FAST;
  size_t kc = 0;
  int rc = TRUE;
  trie *trie_ptr;
  mark m;
  int is_secondary = true(node, TN_SECONDARY);
						/* get the keys */
  for( ; node->parent; node = node->parent )
  { if ( is_secondary && true(node, TN_PRIMARY) )
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

  retry:
    Mark(m);
    init_ukey_state(&ustate, trie_ptr, valTermRef(term));
    for(i=kc; i-- > 0; )
    { if ( (rc=unify_key(&ustate, keys[i])) != TRUE )
      { destroy_ukey_state(&ustate);
	if ( rc == FALSE )
	  goto out;
	Undo(m);
	if ( (rc=makeMoreStackSpace(rc, ALLOW_GC)) )
	  goto retry;
	else
	  goto out;
      }
    }
    destroy_ukey_state(&ustate);
    break;
  }

out:
  if ( keys != fast )
    free(keys);

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
      { Table table = children.hash->table;
	TableEnum e = newTableEnum(table);
	void *k, *v;

	while(advanceTableEnum(e, &k, &v))
	{ if ( (rc=map_trie_node(v, map, ctx)) != NULL )
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
  if ( n->value && true(n, TN_PRIMARY) )
    stats->values++;

  if ( children.any )
  { switch( children.any->type )
    { case TN_KEY:
	stats->bytes += sizeof(*children.key);
        break;
      case TN_HASHED:
	stats->bytes += sizeofTable(children.hash->table);
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
static int
unify_trie(DECL_LD term_t t, trie *trie)
{ return PL_unify_atom(t, trie->symbol);
}

int
get_trie(term_t t, trie **tp)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &trie_blob )
  { tref *ref = data;

    if ( ref->trie->magic == TRIE_MAGIC )
    { *tp = ref->trie;
      return TRUE;
    }

    PL_existence_error("trie", t);
  } else
    PL_type_error("trie", t);

  return FALSE;
}


int
get_trie_noex(term_t t, trie **tp)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &trie_blob )
  { tref *ref = data;

    *tp = ref->trie;
    return TRUE;
  }

  return FALSE;
}


int
trie_error(int rc, term_t culprit)
{ switch(rc)
  { case TRIE_LOOKUP_CONTAINS_ATTVAR:
      return PL_type_error("free_of_attvar", culprit);
    case TRIE_LOOKUP_CYCLIC:
      return PL_type_error("acyclic_term", culprit);
    default:
      return FALSE;
  }
}

int
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
    int rc;

    rc = unify_trie(A1, trie);
    PL_unregister_atom(symbol);

    return rc;
  }

  return FALSE;
}


static
PRED_IMPL("is_trie", 1, is_trie, 0)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(A1, &data, NULL, &type) && type == &trie_blob )
  { tref *ref = data;

    if ( ref->trie->magic == TRIE_MAGIC )
      return TRUE;
  }

  return FALSE;
}


static
PRED_IMPL("trie_destroy", 1, trie_destroy, 0)
{ trie *trie;

  if ( get_trie(A1, &trie) )
  { trie->magic = TRIE_CMAGIC;
    trie_empty(trie);

    return TRUE;
  }

  return FALSE;
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

    return (word)PL_record(value);
  } else
  { return ATOM_trienode;
  }
}


static inline void
release_value(word value)
{ if ( isAtom(value) )
    PL_unregister_atom(value);
  else if ( isRecord(value) )
    PL_erase((record_t)value);
}


static int
equal_value(word v1, word v2)
{ if ( v1 == v2 )
    return TRUE;

  if ( isRecord(v1) && isRecord(v2) )
    return variantRecords((record_t)v1, (record_t)v2);

  return FALSE;
}


#define unify_value(t, value) LDFUNC(unify_value, t, value)
static int
unify_value(DECL_LD term_t t, word value)
{ if ( !isRecord(value) )
  { return _PL_unify_atomic(t, value);
  } else
  { term_t t2;

    return ( (t2=PL_new_term_ref()) &&
	     PL_recorded((record_t)value, t2) &&
	     PL_unify(t, t2)
	   );
  }
}


int
put_trie_value(DECL_LD term_t t, trie_node *node)
{ if ( !isRecord(node->value) )
  { *valTermRef(t) = node->value;
    return TRUE;
  } else
  { return PL_recorded((record_t)node->value, t);
  }
}


int
set_trie_value_word(trie *trie, trie_node *node, word val)
{ if ( node->value )
  { if ( !equal_value(node->value, val) )
    { word old = node->value;

      acquire_key(val);
      node->value = val;
      set(node, TN_PRIMARY);
      release_value(old);
      trie_discard_clause(trie);

      return TRUE;
    } else
    { return FALSE;
    }
  } else
  { acquire_key(val);
    node->value = val;
    set(node, TN_PRIMARY);
    ATOMIC_INC(&trie->value_count);
    trie_discard_clause(trie);

    return TRUE;
  }
}

int
set_trie_value(DECL_LD trie *trie, trie_node *node, term_t value)
{ word val = intern_value(value);

  if ( !set_trie_value_word(trie, node, val) &&
       isRecord(val) )
    PL_erase((record_t)val);

  return TRUE;
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
  { if ( true(node, TN_PRIMARY) )
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
static int
trie_insert(DECL_LD term_t Trie, term_t Key, term_t Value, trie_node **nodep,
	    int update, size_abstract *abstract)
{ trie *trie;

  if ( get_trie(Trie, &trie) )
  { Word kp;
    trie_node *node;
    int rc;

    if ( false(trie, TRIE_ISMAP|TRIE_ISSET) )
    { if ( Value )
	set(trie, TRIE_ISMAP);
      else
	set(trie, TRIE_ISSET);
    } else
    { if ( (Value  && false(trie, TRIE_ISMAP)) ||
	   (!Value && false(trie, TRIE_ISSET)) )
      { return PL_permission_error("insert", "trie", Trie);
      }
    }

    kp	= valTermRef(Key);

    if ( (rc=trie_lookup_abstract(trie, NULL, &node, kp,
				  TRUE, abstract, NULL)) == TRUE )
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
	  { PL_erase((record_t)val);
	  }

	  return TRUE;
	} else
	{ if ( !equal_value(node->value, val) )
	    PL_permission_error("modify", "trie_key", Key);
	  if ( isRecord(val) )
	    PL_erase((record_t)val);

	  return FALSE;
	}
      }
      acquire_key(val);
      node->value = val;
      set(node, TN_PRIMARY);
      ATOMIC_INC(&trie->value_count);
      trie_discard_clause(trie);

      return TRUE;
    }

    return trie_error(rc, Key);
  }

  return FALSE;
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

  return trie_insert(A1, A2, A3, NULL, FALSE, NULL);
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

  return trie_insert(A1, A2, 0, NULL, FALSE, NULL);
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
	   trie_insert(A1, A3, 0, NULL, FALSE, &sa) > 0 );
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

  return trie_insert(A1, A2, A3, NULL, TRUE, NULL);
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
  trie_node *node;

  return ( trie_insert(A1, A2, A3, &node, FALSE, NULL) &&
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

    if ( (rc=trie_lookup(trie, NULL, &node, kp, FALSE, NULL)) == TRUE )
    { if ( node->value )
      { if ( unify_value(A3, node->value) )
	{ trie_delete(trie, node, TRUE);
	  return TRUE;
	}
      }
      return FALSE;
    }

    return trie_error(rc, A2);
  }

  return FALSE;
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

    if ( (rc=trie_lookup(trie, NULL, &node, kp, FALSE, NULL)) == TRUE )
    { if ( node->value )
	return unify_value(A3, node->value);
      return FALSE;
    }

    return trie_error(rc, A2);
  }

  return FALSE;
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
init_ukey_state(DECL_LD ukey_state *state, trie *trie, Word p)
{ state->trie = trie;
  state->ptr  = p;
  state->umode = uread;
  state->max_var_seen = 0;
  state->a_offset = aTop-aBase;
}

static void
destroy_ukey_state(DECL_LD ukey_state *state)
{ if ( state->max_var_seen && state->vars != state->var_buf )
    PL_free(state->vars);
  aTop = aBase + state->a_offset;
}

static Word*
find_var(ukey_state *state, size_t index)
{ if ( index > state->max_var_seen )
  { assert(index == state->max_var_seen+1);

    if ( !state->max_var_seen )
    { state->vars_allocated = NVARS_FAST;
      state->vars = state->var_buf;
    } else if ( index >= state->vars_allocated )
    { if ( state->vars == state->var_buf )
      { state->vars = PL_malloc(sizeof(*state->vars)*NVARS_FAST*2);
	memcpy(state->vars, state->var_buf, sizeof(*state->vars)*NVARS_FAST);
      } else
      { state->vars = PL_realloc(state->vars,
				 sizeof(*state->vars)*state->vars_allocated*2);
      }
      state->vars_allocated *= 2;
    }
    state->vars[index] = NULL;
    state->max_var_seen = index;
  }

  return &state->vars[index];
}


static int
unify_key(DECL_LD ukey_state *state, word key)
{ Word p = state->ptr;

  switch(tagex(key))
  { case TAG_VAR|STG_LOCAL:			/* RESERVED_TRIE_VAL */
    { size_t popn = IS_TRIE_KEY_POP(key);
      Word wp;

      assert(popn);
      aTop -= popn;
      wp = *aTop;
      state->umode = ((int)(uintptr_t)wp & uwrite);
      state->ptr   = (Word)((intptr_t)wp&~uwrite);

      DEBUG(MSG_TRIE_PUT_TERM,
	    Sdprintf("U Popped(%zd) %zd, mode=%d\n",
		     popn, state->ptr-gBase, state->umode));
      return TRUE;
    }
    case TAG_ATOM|STG_GLOBAL:			/* functor */
    { size_t arity = arityFunctor(key);

      DEBUG(MSG_TRIE_PUT_TERM,
	    Sdprintf("U Pushed %s %zd, mode=%d\n",
		     functorName(key), state->ptr+1-gBase, state->umode));
      pushArgumentStack((Word)((intptr_t)(p + 1)|state->umode));

      if ( state->umode == uwrite )
      { Word t;

	if ( (t=allocGlobalNoShift(arity+1)) )
	{ t[0] = key;
	  *p = consPtr(t, TAG_COMPOUND|STG_GLOBAL);
	  state->ptr = &t[1];
	  return TRUE;
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
	      return TRUE;
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
	      return TRUE;
	    } else
	      return GLOBAL_OVERFLOW;
	  }
	} else if ( isTerm(*p) )
	{ Functor f = valueTerm(*p);

	  if ( f->definition == key )
	  { state->ptr = &f->arguments[0];
	    return TRUE;
	  } else
	    return FALSE;
	} else
	{ return FALSE;
	}
      } /*uread*/
    }
    assert(0);
    case TAG_VAR:
    { size_t index = (size_t)(key>>LMASK_BITS);
      Word *v = find_var(state, index);

      DEBUG(MSG_TRIE_PUT_TERM,
	    { char b1[64]; char b2[64];
	      Sdprintf("var %zd at %s (v=%p, *v=%s)\n",
		       index,
		       print_addr(state->ptr,b1),
		       v, print_addr(*v,b2));
	    });

      if ( state->umode == uwrite )
      { if ( !*v )
	{ setVar(*state->ptr);
	  *v = state->ptr;
	} else
	{ *state->ptr = makeRefG(*v);
	}
      } else
      { deRef(p);

	if ( !*v )
	{ *v = state->ptr;
	} else
	{ int rc;

	  if ( (rc=unify_ptrs(state->ptr, *v, ALLOW_RETCODE)) != TRUE )
	    return rc;
	}
      }

      break;
    }
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
	    return FALSE;
	} else
	{ return FALSE;
	}
      }

      break;
    }
    case TAG_ATOM:
      pushVolatileAtom(key);
      /*FALLTHROUGH*/
    case TAG_INTEGER:
    { if ( state->umode == uwrite )
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
	    return FALSE;
	}
      }
      break;
    }
    default:
      assert(0);
  }

  state->ptr++;

  return TRUE;
}


typedef struct trie_choice
{ TableEnum  table_enum;
  Table      table;
  unsigned   var_mask;
  unsigned   var_index;
  word       novar;
  word       key;
  trie_node *child;
} trie_choice;

typedef struct
{ trie	      *trie;		/* trie we operate on */
  int	       allocated;	/* If TRUE, the state is persistent */
  unsigned     vflags;		/* TN_PRIMARY or TN_SECONDARY */
  tmp_buffer   choicepoints;	/* Stack of trie state choicepoints */
} trie_gen_state;

typedef struct desc_tstate
{ Word  term;
  size_t size;
} desc_tstate;

typedef struct
{ Word	       term;		/* Term we are descending */
  size_t       size;		/* Size of the current node */
  int	       compound;	/* Initialized for compound */
  int	       prune;		/* Use for pruning */
  segstack     stack;		/* Stack for argument handling */
  desc_tstate  buffer[64];	/* Quick buffer for stack */
} descent_state;

#define advance_node(ch) LDFUNC(advance_node, ch)
static int	advance_node(DECL_LD trie_choice *ch);

static void
init_trie_state(trie_gen_state *state, trie *trie, const trie_node *root)
{ state->trie = trie;
  state->allocated = FALSE;
  state->vflags = root == &trie->root ? TN_PRIMARY : TN_SECONDARY;
  initBuffer(&state->choicepoints);
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

  release_trie(state->trie);

  if ( state->allocated )
    freeForeignState(state, sizeof(*state));
}

foreign_t
clear_trie_gen_state(void *ctx)
{ trie_gen_state *state = ctx;

  clear_trie_state(state);
  return TRUE;
}

static void
clear_descent_state(descent_state *dstate)
{ if ( dstate->compound )
    clearSegStack(&dstate->stack);
}

#define get_key(state, dstate, key) LDFUNC(get_key, state, dstate, key)
static int
get_key(DECL_LD trie_gen_state *state, descent_state *dstate, word *key)
{ Word p;

  if ( dstate->size == 0 )
  { *key = TRIE_KEY_POP(0);
    return TRUE;
  }

  deRef2(dstate->term, p);
  DEBUG(CHK_SECURE, checkData(p));

  if ( canBind(*p) )
  { return FALSE;
  } else if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    desc_tstate dts;

    DEBUG(MSG_TRIE_GEN,
	  Sdprintf("get_key() for %s\n", functorName(f->definition)));

    *key = f->definition;
    if ( dstate->size > 1 )
    { if ( !dstate->compound )
      { dstate->compound = TRUE;
	initSegStack(&dstate->stack, sizeof(desc_tstate),
		     sizeof(dstate->buffer), dstate->buffer);
      }
      dts.term = dstate->term+1;
      dts.size = dstate->size-1;
      if ( !pushSegStack(&dstate->stack, dts, desc_tstate) )
	outOfCore();
      DEBUG(MSG_TRIE_GEN,
	    Sdprintf("Pushed %p, size %zd\n", dts.term, dts.size));
    }
    dstate->term = &f->arguments[0];
    dstate->size = arityFunctor(f->definition);
    return TRUE;
  } else
  { dstate->term++;
    dstate->size--;

    if ( isIndirect(*p) )
    { *key = trie_intern_indirect(state->trie, *p, FALSE);
      return *key != 0;
    } else
    { *key = *p;
      return TRUE;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Walk a step down the trie, adding  a   node  to the choice stack. If the
term we are walking is instantiated and   the trie node does not contain
variables we walk deterministically. Once we have a variable in the term
we unify against or find a variable in  the trie dstate->prune is set to
FALSE, indicating we must create a real choice.

If a known input value is matched against   a  trie choice node and this
node contains variables we create a choice   from the value and variable
mask such that  we  perform  a  couple   of  hash  lookups  rather  than
enumerating the entire table.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define add_choice(state, dstate, node) LDFUNC(add_choice, state, dstate, node)
static trie_choice *
add_choice(DECL_LD trie_gen_state *state, descent_state *dstate, trie_node *node)
{ trie_children children = node->children;
  trie_choice *ch;
  int has_key;
  word k=0;

  if ( dstate->prune )
  { DEBUG(MSG_TRIE_GEN,
	  if ( dstate->size > 0 )
	  { Word p;
	    deRef2(dstate->term, p);
	    Sdprintf("add_choice() for %s\n", print_val(*p, NULL));
	  });
    if ( !(has_key = get_key(state, dstate, &k)) )
      dstate->prune = FALSE;
  } else
    has_key = FALSE;

  if ( children.any && false(node, state->vflags) )
  { switch( children.any->type )
    { case TN_KEY:
	if ( !has_key ||
	     k == children.key->key ||
	     tagex(children.key->key) == TAG_VAR ||
	     IS_TRIE_KEY_POP(children.key->key) )
	{ word key = children.key->key;

	  if ( tagex(children.key->key) == TAG_VAR )
	    dstate->prune = FALSE;

	  ch = allocFromBuffer(&state->choicepoints, sizeof(*ch));
	  ch->key        = key;
	  ch->child      = children.key->child;
	  ch->table_enum = NULL;
	  ch->table      = NULL;

	  if ( IS_TRIE_KEY_POP(children.key->key) && dstate->compound )
	  { desc_tstate dts;
	    popSegStack(&dstate->stack, &dts, desc_tstate);
	    dstate->term = dts.term;
	    dstate->size = dts.size;
	    DEBUG(MSG_TRIE_GEN,
		  Sdprintf("Popped %p, left %zd\n", dstate->term, dstate->size));
	  }
	  break;
	} else
	{ DEBUG(MSG_TRIE_GEN, Sdprintf("Failed\n"));
	  return NULL;
	}
      case TN_HASHED:
      { void *tk, *tv;

	if ( has_key )
	{ if ( children.hash->var_mask == 0 )
	  { trie_node *child;

	    if ( (child = lookupHTable(children.hash->table, (void*)k)) )
	    { ch = allocFromBuffer(&state->choicepoints, sizeof(*ch));
	      ch->key        = k;
	      ch->child	     = child;
	      ch->table_enum = NULL;
	      ch->table      = NULL;

	      return ch;
	    } else
	      return NULL;
	  } else if ( children.hash->var_mask != VMASK_SCAN )
	  { dstate->prune = FALSE;

	    DEBUG(MSG_TRIE_GEN,
		  Sdprintf("Created var choice 0x%x\n", children.hash->var_mask));

	    ch = allocFromBuffer(&state->choicepoints, sizeof(*ch));
	    ch->table_enum = NULL;
	    ch->table      = children.hash->table;
	    ch->var_mask   = children.hash->var_mask;
	    ch->var_index  = 1;
	    ch->novar      = k;
	    if ( advance_node(ch) )
	    { return ch;
	    } else
	    { state->choicepoints.top = (char*)ch;
	      ch--;
	      return NULL;
	    }
	  }
	}
					/* general enumeration */
	dstate->prune = FALSE;
	ch = allocFromBuffer(&state->choicepoints, sizeof(*ch));
	ch->table = NULL;
	ch->table_enum = newTableEnum(children.hash->table);
	advanceTableEnum(ch->table_enum, &tk, &tv);
	ch->key   = (word)tk;
	ch->child = (trie_node*)tv;
	break;
      }
      default:
	assert(0);
        return NULL;
    }
  } else
  { ch = allocFromBuffer(&state->choicepoints, sizeof(*ch));
    memset(ch, 0, sizeof(*ch));
    ch->child = node;
  }

  return ch;
}


#define descent_node(state, dstate, ch) LDFUNC(descent_node, state, dstate, ch)
static trie_choice *
descent_node(DECL_LD trie_gen_state *state, descent_state *dstate, trie_choice *ch)
{ while( ch && ch->child->children.any &&
	 false(ch->child, state->vflags) )
  { ch = add_choice(state, dstate, ch->child);
  }

  return ch;
}


static int
advance_node(DECL_LD trie_choice *ch)
{ if ( ch->table_enum )
  { void *k, *v;

    if ( advanceTableEnum(ch->table_enum, &k, &v) )
    { ch->key   = (word)k;
      ch->child = (trie_node*)v;

      return TRUE;
    }
  } else if ( ch->table )
  { if ( ch->novar )
    { if ( (ch->child=lookupHTable(ch->table, (void*)ch->novar)) )
      { ch->key = ch->novar;
	ch->novar = 0;
	return TRUE;
      }
    }
    for( ; ch->var_index && ch->var_index < VMASKBITS; ch->var_index++ )
    { if ( (ch->var_mask & (0x1<<(ch->var_index-1))) )
      { word key = ((((word)ch->var_index))<<LMASK_BITS)|TAG_VAR;

	if ( (ch->child=lookupHTable(ch->table, (void*)key)) )
	{ ch->key = key;
	  ch->var_index++;
	  return TRUE;
	}
      }
    }
  }

  return FALSE;
}


#define next_choice0(state, dstate) LDFUNC(next_choice0, state, dstate)
static trie_choice *
next_choice0(DECL_LD trie_gen_state *state, descent_state *dstate)
{ trie_choice *btm = base_choice(state);
  trie_choice  *ch = top_choice(state)-1;

  while(ch >= btm)
  { if ( advance_node(ch) )
      return descent_node(state, dstate, ch);

    if ( ch->table_enum )
      freeTableEnum(ch->table_enum);

    state->choicepoints.top = (char*)ch;
    ch--;
  }

  return NULL;
}


#define next_choice(state) LDFUNC(next_choice, state)
static trie_choice *
next_choice(DECL_LD trie_gen_state *state)
{ trie_choice *ch;
  descent_state dstate;

  dstate.prune    = FALSE;
  dstate.compound = FALSE;

  do
  { ch = next_choice0(state, &dstate);
  } while (ch && false(ch->child, state->vflags));

  return ch;
}


static trie_node *
gen_state_leaf(trie_gen_state *state)
{ trie_choice *top = top_choice(state);

  return top[-1].child;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify term with the term represented a trie path (list of trie_choice).
Returns one of TRUE, FALSE or *_OVERFLOW.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define unify_trie_path(term, tn, gstate) LDFUNC(unify_trie_path, term, tn, gstate)
static int
unify_trie_path(DECL_LD term_t term, trie_node **tn, trie_gen_state *gstate)
{ ukey_state ustate;
  trie_choice *ch = base_choice(gstate);
  trie_choice *top = top_choice(gstate);

  init_ukey_state(&ustate, gstate->trie, valTermRef(term));
  for( ; ch < top; ch++ )
  { int rc;

    if ( (rc=unify_key(&ustate, ch->key)) != TRUE )
    { destroy_ukey_state(&ustate);
      return rc;
    }
  }

  destroy_ukey_state(&ustate);
  *tn = ch[-1].child;

  return TRUE;
}


foreign_t
trie_gen_raw(trie *trie, trie_node *root, term_t Key, term_t Value,
	     term_t Data, int LDFUNCP (*unify_data)(DECL_LD term_t, trie_node*, void *ctx),
	     void *ctx, control_t PL__ctx)
{ PRED_LD
  trie_gen_state state_buf;
  trie_gen_state *state;
  trie_node *n;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { trie_choice *ch;
      descent_state dstate;
      int rc;

      TRIE_STAT_INC(trie, gen_call);

      dstate.term     = valTermRef(Key);
      dstate.size     = 1;
      dstate.compound = FALSE;
      dstate.prune	  = TRUE;
      deRef(dstate.term);

      acquire_trie(trie);
      state = &state_buf;
      init_trie_state(state, trie, root);
      rc = ( (ch = add_choice(state, &dstate, root)) &&
	     (ch = descent_node(state, &dstate, ch)) &&
	     (true(ch->child, state->vflags) || next_choice(state)) );
      clear_descent_state(&dstate);
      if ( !rc )
      { clear_trie_state(state);
	return FALSE;
      }
      break;
    }
    case FRG_REDO:
      state = CTX_PTR;
      if ( true(gen_state_leaf(state), state->vflags) ||
	   next_choice(state) )		/* pending choice was deleted */
      { break;
      } else
      { clear_trie_state(state);
	return FALSE;
      }
    case FRG_CUTTED:
      state = CTX_PTR;
      clear_trie_state(state);
      return TRUE;
    default:
      assert(0);
      return FALSE;
  }

  Mark(fli_context->mark);
  for( ; !isEmptyBuffer(&state->choicepoints); next_choice(state) )
  { for(;;)
    { int rc;
      size_t asize = aTop - aBase; /* using the argument stack may be dubious */

      if ( (rc=unify_trie_path(Key, &n, state)) == TRUE )
	break;

      aTop = aBase+asize;
      Undo(fli_context->mark);
      if ( rc == FALSE )
	goto next;

      if ( makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	continue;

      clear_trie_state(state);
      return FALSE;				/* resource error */
    }

    DEBUG(CHK_SECURE, PL_check_data(Key));

    if ( (!Value || unify_value(Value, n->value)) &&
	 (!Data  || LDFUNCP(*unify_data)(Data, n, ctx)) )
    { if ( next_choice(state) )
      { if ( !state->allocated )
	{ trie_gen_state *nstate = allocForeignState(sizeof(*state));
	  TmpBuffer nchp = &nstate->choicepoints;
	  TmpBuffer ochp = &state->choicepoints;

	  nstate->trie = state->trie;
	  nstate->vflags = state->vflags;
	  nstate->allocated = TRUE;
	  if ( ochp->base == ochp->static_buffer )
	  { size_t bytes = ochp->top - ochp->base;
	    initBuffer(nchp);
	    nchp->top  = nchp->base + bytes;
	    memcpy(nchp->base, ochp->base, bytes);
	  } else
	  { nchp->base = ochp->base;
	    nchp->top  = ochp->top;
	    nchp->max  = ochp->max;
	  }

	  state = nstate;
	}
	ForeignRedoPtr(state);
      } else
      { clear_trie_state(state);
	return TRUE;
      }
    } else
    { Undo(fli_context->mark);
    }

next:;
  }

  clear_trie_state(state);
  return FALSE;
}


foreign_t
trie_gen(term_t Trie, term_t Root, term_t Key, term_t Value,
	 term_t Data, int LDFUNCP (*unify_data)(DECL_LD term_t, trie_node*, void *ctx),
	 void *ctx, control_t PL__ctx)
{ if ( CTX_CNTRL == FRG_FIRST_CALL )
  { trie *trie;
    trie_node *root;

    if ( get_trie(Trie, &trie) )
    { if ( Root )
      { void *ptr;

	if ( !PL_get_pointer_ex(Root, &ptr) )
	  return FALSE;
	root = ptr;
      } else
      { root = &trie->root;
      }

      if ( root->children.any )
	return trie_gen_raw(trie, root, Key, Value, Data,
			    unify_data, ctx, PL__ctx);
    }

    return FALSE;
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

#define unify_node_id(t, answer, ctx) LDFUNC(unify_node_id, t, answer, ctx)
static int
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

#ifdef O_TRIE_STATS
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
	return FALSE;
      } else if ( name == ATOM_hashed )
      { trie_stats stats;
	stat_trie(trie, &stats);
	return PL_unify_int64(arg, stats.hashes);
#ifdef O_TRIE_STATS
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

	  if ( idg->affected )  size += sizeofTable(idg->affected);
	  if ( idg->dependent ) size += sizeofTable(idg->dependent);

	  return PL_unify_int64(arg, size);
	}
      }
    }
  }

  return FALSE;
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

    rc = trie_lookup(trie, NULL, &root, kp, TRUE, &vars);
    if ( rc == TRUE )
    { rc = trie_lookup(trie, root, &node, vp, TRUE, &vars);

      if ( rc == TRUE )
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

  return FALSE;
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
    rc = trie_lookup(trie, NULL, &root, kp, FALSE, NULL);
    if ( rc == TRUE )
    { return trie_gen_raw(trie, root, A3, 0, 0, NULL, NULL, PL__ctx);
    } else
    { rc = trie_error(rc, A1);
    }

    return rc;
  }

  return FALSE;
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
    rc = trie_lookup(trie, NULL, &root, kp, FALSE, NULL);
    if ( rc == TRUE )
    { Word vp = valTermRef(A3);
      trie_node *node;

      rc = trie_lookup(trie, root, &node, vp, FALSE, NULL);
      if ( rc == TRUE )
      { trie_delete(trie, node, TRUE);
      } else
      { rc = trie_error(rc, A1);
      }
    } else
    { rc = trie_error(rc, A1);
    }

    return rc;
  }

  return FALSE;
}

#endif /*O_NESTED_TRIES*/


		 /*******************************
		 *	  COMPILED TRIES	*
		 *******************************/

typedef struct trie_compile_state
{ trie	       *trie;				/* Trie we are working on */
  int		try;				/* There are alternatives */
  int		last_is_fail;			/* Ends in I_FAIL */
  tmp_buffer	codes;				/* Output instructions */
  size_t	else_loc;			/* last else */
  size_t	maxvar;				/* Highest var index */
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
add_vmi_else_d(trie_compile_state *state, vmi c, code d)
{ size_t el;

  addBuffer(&state->codes, encode(c), code);
  el = entriesBuffer(&state->codes, code);
  addBuffer(&state->codes, (code)state->else_loc, code);
  state->else_loc = el;
  addBuffer(&state->codes, d, code);
}

static void
fixup_else(trie_compile_state *state)
{ Code base = baseBuffer(&state->codes, code);
  size_t pc = entriesBuffer(&state->codes, code);
  size_t el = state->else_loc;

  state->else_loc = base[el];
  base[el] = pc-el-1;
}


#define compile_trie_value(v, state) LDFUNC(compile_trie_value, v, state)
static int
compile_trie_value(DECL_LD Word v, trie_compile_state *state)
{ term_agenda_P agenda;
  size_t var_number = 0;
  tmp_buffer varb;
  int rc = TRUE;
  int compounds = 0;
  Word p;

  initTermAgenda_P(&agenda, 1, v);
  while( (p=nextTermAgenda_P(&agenda)) )
  { size_t popn;

    if ( (popn = IS_AC_TERM_POP(p)) )
    { if ( popn == 1 )
	add_vmi(state, T_POP);
      else
	add_vmi_d(state, T_POPN, (code)popn);
    } else
    { word w = *p;

      switch( tag(w) )
      { case TAG_VAR:
	{ size_t index;

	  if ( isVar(w) )
	  { if ( var_number++ == 0 )
	    { initBuffer(&varb);
	    }
	    addBuffer(&varb, p, Word);
	    *p = w = ((((word)var_number))<<LMASK_BITS)|TAG_VAR;
	  }
	  index = (size_t)(w>>LMASK_BITS);
	  if ( index > state->maxvar )
	    state->maxvar = index;
	  add_vmi_d(state, T_VAR, (code)index);
	  break;
	}
	case TAG_ATTVAR:
	  rc = TRIE_LOOKUP_CONTAINS_ATTVAR;
	  goto out;
	case TAG_ATOM:			/* TBD: register */
	  add_vmi_d(state, T_ATOM, (code)w);
	  break;
	case TAG_INTEGER:
	  if ( storage(w) == STG_INLINE)
	  { add_vmi_d(state, T_SMALLINT, (code)w);
	  } else
	  { size_t wsize = wsizeofIndirect(w);
	    Word ip = valIndirectP(w);

	    if ( wsize == sizeof(int64_t)/sizeof(word))
	    {
#if SIZEOF_VOIDP == 8
	      add_vmi_d(state, T_INTEGER, (code)ip[0]);
#else
	      add_vmi_d(state, T_INT64, (code)ip[0]);
	      addBuffer(&state->codes, (code)ip[1], code);
#endif
#ifdef O_GMP
	    } else
	    { add_vmi_d(state, T_MPZ, (code)ip[-1]);
	      addMultipleBuffer(&state->codes, ip, wsize, code);
#endif
	    }
	  }
	  break;
	case TAG_FLOAT:
	{ Word ip = valIndirectP(w);
	  add_vmi_d(state, T_FLOAT, (code)ip[0]);
#if SIZEOF_VOIDP == 4
	  addBuffer(&state->codes, (code)ip[1], code);
#endif
          break;
	}
	case TAG_STRING:
	{ size_t wsize = wsizeofIndirect(w);
	  Word ip = valIndirectP(w);

	  add_vmi_d(state, T_STRING, (code)ip[-1]);
	  addMultipleBuffer(&state->codes, ip, wsize, code);
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
      setVar(*vp);
    }
    discardBuffer(&varb);
  }

  return rc;
}


#define compile_trie_node(n, state) LDFUNC(compile_trie_node, n, state)
static int
compile_trie_node(DECL_LD trie_node *n, trie_compile_state *state)
{ trie_children children;
  word key;
  int rc;

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
    case TAG_VAR:
    { size_t index = (size_t)(key>>LMASK_BITS);

      if ( index > state->maxvar )
	state->maxvar = index;

      if ( state->try )
	add_vmi_else_d(state, T_TRY_VAR, (code)index);
      else
	add_vmi_d(state, T_VAR, (code)index);
      break;
    }
    case STG_GLOBAL|TAG_INTEGER:		/* indirect data */
    case STG_GLOBAL|TAG_STRING:
    case STG_GLOBAL|TAG_FLOAT:
    { size_t index = key>>LMASK_BITS;
      int idx = MSB(index);
      indirect *h = &state->trie->indirects->array.blocks[idx][index];
      size_t wsize = wsizeofInd(h->header);

      switch(tag(key))
      { case TAG_INTEGER:
#if SIZEOF_VOIDP == 8
	  if ( wsize == 1 )			/* 64-bit integer */
	  { if ( state->try )
	      add_vmi_else_d(state, T_TRY_INTEGER, (code)h->data[0]);
	    else
	      add_vmi_d(state, T_INTEGER, (code)h->data[0]);
	    goto indirect_done;
	  } else
#else
	  if ( wsize == 2 )			/* 64-bit integer */
	  { if ( state->try )
	      add_vmi_else_d(state, T_TRY_INT64, (code)h->data[0]);
	    else
	      add_vmi_d(state, T_INT64, (code)h->data[0]);
	    addBuffer(&state->codes, (code)h->data[1], code);
	    goto indirect_done;
	  } else
#endif
	  { if ( state->try )
	      add_vmi_else_d(state, T_TRY_MPZ, (code)h->header);
	    else
	      add_vmi_d(state, T_MPZ, (code)h->header);
	  }
	  break;
        case TAG_STRING:
	  if ( state->try )
	    add_vmi_else_d(state, T_TRY_STRING, (code)h->header);
	  else
	    add_vmi_d(state, T_STRING, (code)h->header);
	  break;
        case TAG_FLOAT:
	  if ( state->try )
	    add_vmi_else_d(state, T_TRY_FLOAT, (code)h->data[0]);
	  else
	    add_vmi_d(state, T_FLOAT, (code)h->data[0]);
#if SIZEOF_VOIDP == 4
	  addBuffer(&state->codes, (code)h->data[1], code);
#endif
	  goto indirect_done;
      }

      addMultipleBuffer(&state->codes, h->data, wsize, code);
    indirect_done:
      break;
    }
    case TAG_ATOM:
    { if ( state->try )
	add_vmi_else_d(state, T_TRY_ATOM, (code)key);
      else
	add_vmi_d(state, T_ATOM, (code)key);
      break;
    }
    case TAG_INTEGER:
    { if ( state->try )
	add_vmi_else_d(state, T_TRY_SMALLINT, (code)key);
      else
	add_vmi_d(state, T_SMALLINT, (code)key);
      break;
    }
    default:
      assert(0);
  }

children:
  if ( children.any && false(n, TN_PRIMARY|TN_SECONDARY) )
  { switch( children.any->type )
    { case TN_KEY:
      { state->try = FALSE;
	n = children.key->child;
	goto next;
      }
      case TN_HASHED:
      { Table table = children.hash->table;
	TableEnum e = newTableEnum(table);
	void *k, *v;

	if ( !advanceTableEnum(e, &k, &v) )
	{ freeTableEnum(e);
	  return TRUE;				/* empty path */
	}

	for(;;)
	{ n = v;

	  if ( !(state->try = advanceTableEnum(e, &k, &v)) )
	  { freeTableEnum(e);
	    goto next;
	  }

	  if ( (rc=compile_trie_node(n, state)) != TRUE )
	  { freeTableEnum(e);
	    return rc;
	  }
	  fixup_else(state);
	}
      }
    }
  } else
  { if ( n->value )			/* what if we have none? */
    { if ( answer_is_conditional(n) )
	add_vmi_d(state, T_DELAY, (code)n);

      if ( true(state->trie, TRIE_ISMAP) )
      { add_vmi(state, T_VALUE);
	if ( !isRecord(n->value) )
	{ if ( isAtom(n->value) )
	  { add_vmi_d(state, T_ATOM, (code)n->value);
	  } else
	  { add_vmi_d(state, T_SMALLINT, (code)n->value);
	  }
	} else
	{ term_t t2;

	  if ( (t2=PL_new_term_ref()) &&
	       PL_recorded((record_t)n->value, t2) )
	  { Word p = valTermRef(t2);

	    deRef(p);
	    if ( (rc = compile_trie_value(p, state)) != TRUE )
	      return rc;
	  } else
	  { return FALSE;
	  }
	  PL_reset_term_refs(t2);
	}
      }
      add_vmi(state, I_EXIT);
      state->last_is_fail = FALSE;
    } else
    { add_vmi(state, I_FAIL);
      state->last_is_fail = TRUE;
    }
  }

  return TRUE;
}


static int
fixup_last_fail(trie_compile_state *state)
{ if ( state->last_is_fail )
    add_vmi(state, I_EXIT);	/* make sure the clause ends with I_EXIT */
  return TRUE;
}


static int
create_trie_clause(Definition def, Clause *cp, trie_compile_state *state)
{ size_t code_size = entriesBuffer(&state->codes, code);
  size_t size      = sizeofClause(code_size);
  //size_t clsize    = size + SIZEOF_CREF_CLAUSE;
  Clause cl;

  cl = PL_malloc_atomic(size);
  memset(cl, 0, sizeof(*cl));
  cl->predicate = def;
  cl->code_size = code_size;
  cl->prolog_vars = TRIE_VAR_OFFSET + state->maxvar;
  cl->variables = cl->prolog_vars;	/* 2: pseudo arity */
  set(cl, UNIT_CLAUSE);			/* no body */
  memcpy(cl->codes, baseBuffer(&state->codes, code), sizeOfBuffer(&state->codes));
  *cp = cl;

  ATOMIC_ADD(&GD->statistics.codes, cl->code_size);
  ATOMIC_INC(&GD->statistics.clauses);

  return TRUE;
}


atom_t
compile_trie(DECL_LD Definition def, trie *trie)
{ atom_t dbref;

retry:
  if ( !(dbref = trie->clause) )
  { if ( trie->value_count == 0 )
    { dbref = ATOM_fail;
      if ( !COMPARE_AND_SWAP_WORD(&trie->clause, 0, dbref) )
	goto retry;
    } else
    { trie_compile_state state;
      Clause cl;
      ClauseRef cref;

      init_trie_compile_state(&state, trie);
      add_vmi(&state, def->functor->arity == 2 ? T_TRIE_GEN2 : T_TRIE_GEN3);
      if ( compile_trie_node(&trie->root, &state) &&
	   fixup_last_fail(&state) &&
	   create_trie_clause(def, &cl, &state) )
      { cref = assertDefinition(def, cl, CL_END);
	if ( cref )
	{ dbref = lookup_clref(cref->value.clause);
	  if ( !COMPARE_AND_SWAP_WORD(&trie->clause, 0, dbref) )
	  { PL_unregister_atom(dbref);
	    retractClauseDefinition(def, cref->value.clause, FALSE);
	    goto retry;
	  }
	}
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
  { Procedure proc = (true(trie, TRIE_ISMAP)
			     ? GD->procedures.trie_gen_compiled3
			     : GD->procedures.trie_gen_compiled2);
    atom_t clref = compile_trie(proc->definition, trie);

    return _PL_unify_atomic(A2, clref);
  }

  return FALSE;
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
	PC[1] = (code)NULL;
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
